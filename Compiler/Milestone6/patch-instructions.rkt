#lang racket

(require "common/register.rkt")
(provide patch-instructions)

(module+ test
  (require rackunit))

;claims t5 t6
(define t 6)

(define (newTemp)
  (if (>= t 6)
      (set! t 5)
      (set! t (add1 t)))
  (string->symbol (format "t~a" t)))

(define (resetTemp)
  (set! t 6))

(define firstClaimReg 't5)

(define (triv? t)
  (or (integer? t) (addr? t) (register? t)))

;
;(patch-binop a b c binop)->list? '((set! ...) ...)
;a,b,c:triv?
(define (patch-binop a b c binop)
  (let* ([aReg (cond
                 [(register? a) a]
                 [else (newTemp)])]
         [bReg (cond
                 [(register? b) b]
                 [(equal? a b) aReg]
                 [else (newTemp)])]
         [cReg (cond
                 [(and (equal? binop '-) (isCapability? a) (isCapability? b) (isCapability? c)) 's8]
                 [(register? c) c]
                 [(equal? b c) bReg]
                 [(equal? a c) aReg]
                 [(and (equal? binop '+) (and (integer? c) (and (< c 2048) (>= c -2048)))) c]
                 [(and (equal? binop '-) (and (integer? c) (and (< c 2048) (>= c -2048)))) c]
                 [else (newTemp)])])
    (let ([aSet (cond
                  [(register? a) '()]
                  [else `((set! ,a ,aReg))])]
          [bSet (cond
                  [(register? b) '()]
                  [(equal? a b) `((set! ,bReg ,b))]
                  [else `((set! ,bReg ,b))])]
          [cSet (cond
                  [(and (equal? binop '-) (isCapability? a) (isCapability? b) (isCapability? c)) `((set! s8 (- x0 ,c)))]
                  [(register? c) '()]
                  [(equal? b c) '()]
                  [(equal? a c) `((set! ,cReg ,c))]
                  [(and (equal? binop '+) (and (integer? c) (and (< c 2048) (>= c -2048)))) '()]
                  [(and (equal? binop '-) (and (integer? c) (and (< c 2048) (>= c -2048)))) '()]
                  [else `((set! ,cReg ,c))])]
          [binop (cond [(and (equal? binop '-) (isCapability? a) (isCapability? b) (isCapability? c)) '+]
                       [else binop])])
      (append bSet cSet `((set! ,aReg (,binop ,bReg ,cReg))) aSet))))

;
;(patch-set s)->list? '((set! ...) ...)
;s: (set! ...)
(define (patch-set s)
  (match s
    [`(set! ,a ,b) #:when (or (register? a) (register? b)) `((set! ,a ,b))]                  
    [`(set! ,a ,b) #:when (and (addr? a) (not (register? b))) (let ([reg (newTemp)])
                                                                `((set! ,reg ,b) (set! ,a ,reg)))]
    [_ #f]))

;
;(patch-with-label w)->list? '((set! ...) ...)
;w->effect
(define (patch-with-label w)
  (match w
    [`(with-label ,l ,b) (let ([sets (patch-effect b)])
                           (cons `(with-label ,l ,(car sets)) (cdr sets)))]
    [_ #f]))

;
;(patch-compare c)->list? '((set! ...) ...)
;c->effect
(define (patch-compare c)
  (match c
    [`(compare ,a (,relop ,b ,c)) (let* ([aReg (if (register? a) a (newTemp))]
                                         [bReg (cond
                                                 [(register? b) b]
                                                 [(equal? a b) aReg]
                                                 [else (newTemp)])]
                                         [cReg (cond
                                                 [(register? c) c]
                                                 [(equal? a c) aReg]
                                                 [(equal? b c) bReg]
                                                 [else (newTemp)])])
                                    (let ([aSet (if (register? a) '() `((set! ,a ,aReg)))]
                                          [bSet (cond
                                                  [(register? b) '()]
                                                  [else `((set! ,bReg ,b))])]
                                          [cSet (cond
                                                  [(register? c) '()]
                                                  [(equal? b c) '()]
                                                  [else `((set! ,cReg ,c))])])
                                      (append bSet cSet `((compare ,aReg (,relop ,bReg ,cReg))) aSet)))]
    [_ #f]))

;
;(patch-jump-if j)->list? '((set! ...) ...)
;j->effect
(define (patch-jump-if j)
  (match j
    [`(jump-if ,l (,relop ,a ,b)) (let* ([aReg (if (register? a) a (newTemp))]
                                         [bReg (cond
                                                 [(register? b) b]
                                                 [(equal? a b) aReg]
                                                 [else (newTemp)])])
                                    (let ([aSet (if (register? a) '() `((set! ,aReg ,a)))]
                                          [bSet (cond
                                                  [(register? b) '()]
                                                  [(equal? a b) '()]
                                                  [else `((set! ,bReg ,b))])])
                                      (append aSet bSet `((jump-if ,l (,relop ,aReg ,bReg))))))]
    [_ #f]))


;
;(patch-effect e)->list? '((set! ...) ...)
;e->effect
(define (patch-effect e)
  (match e
    [`(set! ,a (,binop ,b ,c)) #:when (and (triv? b) (triv? c)) (patch-binop a b c binop)]
    [`(set! ,a ,b) (patch-set e)]
    [`(with-label ,l ,b) (patch-with-label e)]
    ;[`(jump ,l) #:when (addr? l) `((set! ,(current-jump-register) ,l) (jump ,(current-jump-register)))] doe
    [`(jump ,l) `((jump ,l))]
    [`(compare ,a (,relop ,b ,c)) (patch-compare e)]
    [`(jump-if ,l (,relop ,b ,c)) (patch-jump-if e)]
    [_ #f]))


;
;(patch-instructions p) → paren-cheri-risc-v-fvars-v2?
;p : para-asm-lang-v2?
(define (patch-instructions p)
  (match p
    [`(begin ,s ...)  `(begin ,@(foldl (lambda (e sets) (append sets (patch-effect e))) '() s))]
    [_ #f]))

#;(patch-instructions '(begin
                       (with-label L.tmp.0 (set! t0 cra))
                       (set! (cfp - 8) 2)
                       (set! (cfp - 0) 1)
                       (set! cra t0)
                       (jump L.swap.1)
                       (with-label L.swap.1 (set! (cfp - 16) cra))
                       (set! t0 (cfp - 0))
                       (set! (cfp - 0) (cfp - 8))
                       (jump-if L.tmp.1 (< (cfp - 0) t0))
                       (jump L.tmp.2)
                       (with-label L.tmp.1 (set! ca0 t0))
                       (jump (cfp - 16))
                       (with-label L.tmp.2 (set! cfp (- cfp 24)))
                       (set! (cfp - 8) t0)
                       (set! (cfp - 0) (cfp - -24))
                       (set! cra L.rp-label.6)
                       (jump L.swap.1)
                       (with-label L.rp-label.6 (set! cfp (+ cfp 24)))
                       (set! t0 ca0)
                       (set! ca0 (+ t0 (cfp - 0)))
                       (jump (cfp - 16))))

(module+ test
  (define (check-patch? t1 t2 text)
    (resetTemp)
    (check-equal? t1 t2 text))
  #|
;patch-binop
  ;succes
  (check-patch? (patch-binop 'a0 'a1 'a2 '+)
                '((set! a0 (+ a1 a2)))
                "patch-binop: succes-01: all registers")
  (check-patch? (patch-binop 'a0 'a1 5 '+)
                '((set! a0 (+ a1 5)))
                "patch-binop: succes-02: 12bit integer add")
  (check-patch? (patch-binop 'a0 'a1 5 '*)
                '((set! t5 5) (set! a0 (* a1 t5)))
                "patch-binop: succes-03: 12bit integer mul")
  (check-patch? (patch-binop 'a0 'a1 5000 '+)
                '((set! t5 5000) (set! a0 (+ a1 t5)))
                "patch-binop: succes-04: 32bit integer add")
  (check-patch? (patch-binop 'fv0 'a1 'a2 '+)
                '((set! t5 (+ a1 a2)) (set! fv0 t5) )
                "patch-binop: succes-05: one memory a")
  (check-patch? (patch-binop 'a0 'fv1 'a2 '+)
                '((set! t5 fv1) (set! a0 (+ t5 a2)))
                "patch-binop: succes-06: one memory b")
  (check-patch? (patch-binop 'a0 'a1 'fv2 '+)
                '((set! t5 fv2) (set! a0 (+ a1 t5)))
                "patch-binop: succes-07: one memory c")
  (check-patch? (patch-binop 'fv0 'fv1 'fv2 '+)
                '((set! t6 fv1) (set! t5 fv2) (set! t5 (+ t6 t5)) (set! fv0 t5))
                "patch-binop: succes-08: all memory")
;patch-set
  ;succes
  (check-patch? (patch-set '(set! a0 a1)) '((set! a0 a1)) "patch-set: succes-1: reg reg")
  (check-patch? (patch-set '(set! a0 fv1)) '((set! a0 fv1)) "patch-set: succes-2: reg fvar")
  (check-patch? (patch-set '(set! a0 11)) '((set! a0 11)) "patch-set: succes-3: reg int")
  (check-patch? (patch-set '(set! fv0 a1)) '((set! fv0 a1)) "patch-set: succes-4: fvar reg")
  (check-patch? (patch-set '(set! fv0 fv1)) '((set! t5 fv1) (set! fv0 t5)) "patch-set: succes-5: fvar fvar")
  (check-patch? (patch-set '(set! fv0 11)) '((set! t5 11) (set! fv0 t5)) "patch-set: succes-6: fvar int")
;patch-compare
  ;succes
  (check-patch? (patch-compare '(compare fv0 (= fv0 fv0))) '((set! t5 fv0) (compare t5 (= t5 t5)) (set! fv0 t5)) "patch-compare: succes-1: all same fvar")
  (check-patch? (patch-compare '(compare fv0 (= fv1 fv2))) '((set! t6 fv1) (set! t5 fv2) (compare t5 (= t6 t5)) (set! fv0 t5)) "patch-compare: succes-2: all different fvar")
  (check-patch? (patch-compare '(compare fv0 (= fv0 fv1))) '((set! t5 fv0) (set! t6 fv1) (compare t5 (= t5 t6)) (set! fv0 t5)) "patch-compare: succes-3: two same fvar")
  (check-patch? (patch-compare '(compare fv0 (= fv1 fv0))) '((set! t6 fv1) (set! t5 fv0) (compare t5 (= t6 t5)) (set! fv0 t5)) "patch-compare: succes-4: two same fvar")

  (check-patch? (patch-compare '(compare t0 (= t1 t2))) '((compare t0 (= t1 t2))) "patch-compare: succes-5: all reg")
  (check-patch? (patch-compare '(compare t0 (= t1 fv0))) '((set! t5 fv0) (compare t0 (= t1 t5))) "patch-compare: succes-6: one fvar")
  (check-patch? (patch-compare '(compare fv0 (= t0 t1))) '((compare t5 (= t0 t1)) (set! fv0 t5)) "patch-compare: succes-7: one fvar")
  (check-patch? (patch-compare '(compare t0 (= fv0 t1))) '((set! t5 fv0) (compare t0 (= t5 t1))) "patch-compare: succes-8: one fvar")
;patch-jump-if
  ;succes
  (check-patch? (patch-jump-if '(jump-if foo (= fv0 fv0))) '((set! t5 fv0) (jump-if foo (= t5 t5))) "patch-jump-if: succes-1: all same fvar")
  (check-patch? (patch-jump-if '(jump-if foo (= fv0 fv1))) '((set! t5 fv0) (set! t6 fv1) (jump-if foo (= t5 t6))) "patch-jump-if: succes-2: all different fvar")
  (check-patch? (patch-jump-if '(jump-if foo (= t0 fv0))) '((set! t5 fv0) (jump-if foo (= t0 t5))) "patch-jump-if: succes-3: one fvar")
  (check-patch? (patch-jump-if '(jump-if foo (= fv0 t0))) '((set! t5 fv0) (jump-if foo (= t5 t0))) "patch-jump-if: succes-4: one fvar")
  (check-patch? (patch-jump-if '(jump-if foo (= t0 t1))) '((jump-if foo (= t0 t1))) "patch-jump-if: succes-5: all reg")
;patch-effect
  (check-patch? (patch-effect '(with-label foo (set! fv1 (+ fv2 fv0))))
                '((with-label foo (set! t6 fv2)) (set! t5 fv0) (set! t5 (+ t6 t5)) (set! fv1 t5))
                "patch-effect: succes-1: with-label")
  (check-patch? (patch-effect '(halt t0))
                '((set! a0 t0))
                "patch-effect: succes-2: halt")
  (check-patch? (patch-effect '(jump foo))
                '((jump foo))
                "patch-effect: succes-3: jump")

;patch-instructions
  ;succes
  (check-patch? (patch-instructions '(begin (set! a1 42) (halt a1)))
                '(begin (set! a1 42) (set! a0 a1) (jump "end"))
                "patch-instructions: succes-1: one instruction")
  (check-patch? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv1 42)
                    (set! fv0 fv1)
                    (halt fv0)))
                '(begin (set! t5 0) (set! fv0 t5) (set! t6 42) (set! fv1 t6) (set! t5 fv1) (set! fv0 t5) (set! a0 fv0) (jump "end"))
                "patch-instructions: succes-2: a fvar in second argument")
  (check-patch? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv1 42)
                    (set! fv0 (+ fv0 fv1))
                    (halt fv0)))
                '(begin (set! t5 0) (set! fv0 t5) (set! t6 42) (set! fv1 t6) (set! t5 fv0) (set! t6 fv1) (set! t5 (+ t5 t6)) (set! fv0 t5) (set! a0 fv0) (jump "end"))
                "patch-instructions: succes-3: fvars in binop")
  (check-patch? (patch-instructions
                 '(begin
                    (set! t1 0)
                    (set! t2 0)
                    (set! t3 42)
                    (set! t1 t2)
                    (set! t1 (+ t1 t3))
                    (halt t1)))
                '(begin (set! t1 0) (set! t2 0) (set! t3 42) (set! t1 t2) (set! t1 (+ t1 t3)) (set! a0 t1) (jump "end"))
                "patch-instructions: succes-4: multiple instructions no changes")
  (check-patch? (patch-instructions '(begin (with-label L0 (set! a1 50)) (set! a2 50) (set! a0 50) (jump L1)
                                           (with-label L1 (set! a1 50)) (set! a2 50) (set! a0 50) (jump-if L0 (= a0 a1)) (jump L2)
                                           (with-label L2 (set! a1 50)) (set! a2 50) (set! a0 50) (halt a0)))
               '(begin (with-label L0 (set! a1 50)) (set! a2 50) (set! a0 50) (jump L1)
                                           (with-label L1 (set! a1 50)) (set! a2 50) (set! a0 50) (jump-if L0 (= a0 a1)) (jump L2)
                                           (with-label L2 (set! a1 50)) (set! a2 50) (set! a0 50) (set! a0 a0) (jump "end"))
               "patch-instructions: succes-5: multiple labels")
|#
)
 
