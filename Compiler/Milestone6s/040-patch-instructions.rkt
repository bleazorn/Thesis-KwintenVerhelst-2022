#lang racket

(require "common/register.rkt")
(provide patch-instructions)

(module+ test
  (require rackunit))

;claims t5 t6
(define t -1)

(define (newTemp)
  (if (>= t (sub1 (length (current-auxiliary-registers))))
      (set! t 0)
      (set! t (add1 t)))
  (list-ref (current-auxiliary-registers) t))

(define (resetTemp)
  (set! t -1))

;
;(patch-binop a b c binop)->list? '((set! ...) ...)
;a,b,c:triv?
(define (patch-binop a b c binop)
  (let*-values ([(aReg aSet) (cond
                                [(register? a) (values a         '())]
                                [else          (let ([nTmp (newTemp)])
                                                 (values nTmp    `((set! ,a ,nTmp))))])]
                [(bReg bSet) (cond
                                [(register? b) (values b         '())]
                                [(equal? a b)  (values aReg      `((set! ,aReg ,b)))]
                                [else          (let ([nTmp (newTemp)])
                                                 (values nTmp    `((set! ,nTmp ,b))))])]
                [(cReg cSet) (cond
                                [(register? c) (values c    '())]
                                [(equal? b c)  (values bReg '())]
                                [(equal? a c)  (values aReg `((set! ,aReg ,c)))]
                                [(and (equal? binop '+) (and (integer? c) (and (< c 2048) (>= c -2048)))) (values c '())]
                                [(and (equal? binop '-) (and (integer? c) (and (< c 2048) (>= c -2048)))) (values (- 0 c) '())]
                                [else          (let ([nTmp (newTemp)])
                                                 (values nTmp `((set! ,nTmp ,c))))])])
    (let ([binop (cond [(and (equal? binop '-) (and (integer? c) (and (< c 2048) (>= c -2048)))) '+]
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
    [`(compare ,a (,relop ,b ,c)) (let*-values ([(aReg aSet) (cond
                                                               [(register? a) (values a         '())]
                                                               [else          (let ([nTmp (newTemp)])
                                                                                (values nTmp    `((set! ,a ,nTmp))))])]
                                                [(bReg bSet) (cond
                                                               [(register? b) (values b         '())]
                                                               [(equal? a b)  (values aReg      `((set! ,aReg ,b)))]
                                                               [else          (let ([nTmp (newTemp)])
                                                                                (values nTmp    `((set! ,nTmp ,b))))])]
                                                [(cReg cSet) (cond
                                                               [(register? c) (values c    '())]
                                                               [(equal? b c)  (values bReg '())]
                                                               [(equal? a c)  (values aReg `((set! ,aReg ,c)))]
                                                               [else          (let ([nTmp (newTemp)])
                                                                                (values nTmp `((set! ,nTmp ,c))))])])
                                    (append bSet cSet `((compare ,aReg (,relop ,bReg ,cReg))) aSet))]
    [_ #f]))

;
;(patch-jump-if j)->list? '((set! ...) ...)
;j->effect
(define (patch-jump-if j)
  (match j
    [`(jump-if ,l (,relop ,a ,b)) (let*-values ([(aReg aSet) (cond
                                                               [(register? a) (values a         '())]
                                                               [else          (let ([nTmp (newTemp)])
                                                                                (values nTmp    `((set! ,nTmp ,a))))])]
                                                [(bReg bSet) (cond
                                                               [(register? b) (values b         '())]
                                                               [(equal? a b)  (values aReg      '())]
                                                               [else          (let ([nTmp (newTemp)])
                                                                                (values nTmp    `((set! ,nTmp ,b))))])])
                                    (append aSet bSet `((jump-if ,l (,relop ,aReg ,bReg)))))]
    [_ #f]))


;
;(patch-effect e)->list? '(effect? ...)
;e: effect?
(define (patch-effect e)
  (match e
    [`(set! ,a (,binop ,b ,c)) #:when (or (register? b) (addr? b)) (patch-binop a b c binop)]
    [`(set! ,a ,b) (patch-set e)]
    [`(setLinear! ,a ,b) `((setLinear! ,a ,b))]
    [`(with-label ,l ,b) (patch-with-label e)]
    [`(jump ,l) `((jump ,l))]
    [`(compare ,a (,relop ,b ,c)) (patch-compare e)]
    [`(jump-if ,l (,relop ,b ,c)) (patch-jump-if e)]  
    [`(seal ,r ... ,s) `((seal ,@r ,s))]
    [`(unseal ,r ... ,s) `((unseal ,@r ,s))]
    [`(split ,a ,b ,c ,d) `((split ,a ,b ,c ,d))]
    [`(splice ,a ,b ,c ,d) `((splice ,a ,b ,c ,d))]
    [`(invoke ,a ,b) `((invoke ,a ,b))]
    [_ #f]))


;
;(patch-instructions p) → paren-risc-v-V6-cap?
;p : para-asm-lang-v6?
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
  (check-patch? (patch-binop '(cfp - 0) 'a1 'a2 '+)
                '((set! t5 (+ a1 a2)) (set! (cfp - 0) t5) )
                "patch-binop: succes-05: one memory a")
  (check-patch? (patch-binop 'a0 '(cfp - 8) 'a2 '+)
                '((set! t5 (cfp - 8)) (set! a0 (+ t5 a2)))
                "patch-binop: succes-06: one memory b")
  (check-patch? (patch-binop 'a0 'a1 '(cfp - 16) '+)
                '((set! t5 (cfp - 16)) (set! a0 (+ a1 t5)))
                "patch-binop: succes-07: one memory c")
  (check-patch? (patch-binop '(cfp - 0) '(cfp - 8) '(cfp - 16) '+)
                '((set! t6 (cfp - 8)) (set! t5 (cfp - 16)) (set! t5 (+ t6 t5)) (set! (cfp - 0) t5))
                "patch-binop: succes-08: all memory")
;patch-set
  ;succes
  (check-patch? (patch-set '(set! a0 a1)) '((set! a0 a1))                                          "patch-set: succes-01: reg reg")
  (check-patch? (patch-set '(set! a0 (cfp - 8))) '((set! a0 (cfp - 8)))                            "patch-set: succes-02: reg addr")
  (check-patch? (patch-set '(set! a0 11)) '((set! a0 11))                                          "patch-set: succes-03: reg int")
  (check-patch? (patch-set '(set! (cfp - 0) a1)) '((set! (cfp - 0) a1))                            "patch-set: succes-04: addr reg")
  (check-patch? (patch-set '(set! (cfp - 0) (cfp - 8))) '((set! t5 (cfp - 8)) (set! (cfp - 0) t5)) "patch-set: succes-05: addr addr")
  (check-patch? (patch-set '(set! (cfp - 0) 11)) '((set! t5 11) (set! (cfp - 0) t5))               "patch-set: succes-06: addr int")
;patch-compare
  ;succes
  (check-patch? (patch-compare '(compare (cfp - 0) (= (cfp - 0) (cfp - 0)))) '((set! t5 (cfp - 0)) (compare t5 (= t5 t5)) (set! (cfp - 0) t5)) "patch-compare: succes-1: all same addr")
  (check-patch? (patch-compare '(compare (cfp - 0) (= (cfp - 8) (cfp - 16)))) '((set! t6 (cfp - 8)) (set! t5 (cfp - 16)) (compare t5 (= t6 t5)) (set! (cfp - 0) t5)) "patch-compare: succes-2: all different addr")
  (check-patch? (patch-compare '(compare (cfp - 0) (= (cfp - 0) (cfp - 8)))) '((set! t5 (cfp - 0)) (set! t6 (cfp - 8)) (compare t5 (= t5 t6)) (set! (cfp - 0) t5)) "patch-compare: succes-3: two same addr")
  (check-patch? (patch-compare '(compare (cfp - 0) (= (cfp - 8) (cfp - 0)))) '((set! t6 (cfp - 8)) (set! t5 (cfp - 0)) (compare t5 (= t6 t5)) (set! (cfp - 0) t5)) "patch-compare: succes-4: two same addr")

  (check-patch? (patch-compare '(compare t0 (= t1 t2))) '((compare t0 (= t1 t2))) "patch-compare: succes-5: all reg")
  (check-patch? (patch-compare '(compare t0 (= t1 (cfp - 0)))) '((set! t5 (cfp - 0)) (compare t0 (= t1 t5))) "patch-compare: succes-6: one addr")
  (check-patch? (patch-compare '(compare (cfp - 0) (= t0 t1))) '((compare t5 (= t0 t1)) (set! (cfp - 0) t5)) "patch-compare: succes-7: one addr")
  (check-patch? (patch-compare '(compare t0 (= (cfp - 0) t1))) '((set! t5 (cfp - 0)) (compare t0 (= t5 t1))) "patch-compare: succes-8: one addr")
;patch-jump-if
  ;succes
  (check-patch? (patch-jump-if '(jump-if foo (= (cfp - 0) (cfp - 0)))) '((set! t5 (cfp - 0)) (jump-if foo (= t5 t5))) "patch-jump-if: succes-1: all same addr")
  (check-patch? (patch-jump-if '(jump-if foo (= (cfp - 0) (cfp - 8)))) '((set! t5 (cfp - 0)) (set! t6 (cfp - 8)) (jump-if foo (= t5 t6))) "patch-jump-if: succes-2: all different addr")
  (check-patch? (patch-jump-if '(jump-if foo (= t0 (cfp - 0)))) '((set! t5 (cfp - 0)) (jump-if foo (= t0 t5))) "patch-jump-if: succes-3: one addr")
  (check-patch? (patch-jump-if '(jump-if foo (= (cfp - 0) t0))) '((set! t5 (cfp - 0)) (jump-if foo (= t5 t0))) "patch-jump-if: succes-4: one addr")
  (check-patch? (patch-jump-if '(jump-if foo (= t0 t1))) '((jump-if foo (= t0 t1))) "patch-jump-if: succes-5: all reg")
;patch-effect
  (check-patch? (patch-effect '(with-label foo (set! (cfp - 8) (+ (cfp - 16) (cfp - 0)))))
                '((with-label foo (set! t6 (cfp - 16))) (set! t5 (cfp - 0)) (set! t5 (+ t6 t5)) (set! (cfp - 8) t5))
                "patch-effect: succes-01: with-label")
  (check-patch? (patch-effect '(jump foo))
                '((jump foo))
                "patch-effect: succes-03: jump")

;patch-instructions
  ;succes
  (check-patch? (patch-instructions '(begin (set! a1 42) (set! a0 a1) (jump (cfp - 0))))
                '(begin (set! a1 42) (set! a0 a1) (jump (cfp - 0)))
                "patch-instructions: succes-01: one instruction")
  (check-patch? (patch-instructions
                 '(begin
                    (set! (cfp - 0) 0)
                    (set! (cfp - 8) 42)
                    (set! (cfp - 0) (cfp - 8))
                    (set! a0 (cfp - 0))
                    (jump (cfp - 0))))
                '(begin (set! t5 0) (set! (cfp - 0) t5) (set! t6 42) (set! (cfp - 8) t6) (set! t5 (cfp - 8)) (set! (cfp - 0) t5) (set! a0 (cfp - 0)) (jump (cfp - 0)))
                "patch-instructions: succes-02: a addr in second argument")
  (check-patch? (patch-instructions
                 '(begin
                    (set! (cfp - 0) 0)
                    (set! (cfp - 8) 42)
                    (set! (cfp - 0) (+ (cfp - 0) (cfp - 8)))
                    (set! a0 (cfp - 0))
                    (jump (cfp - 0))))
                '(begin (set! t5 0) (set! (cfp - 0) t5) (set! t6 42) (set! (cfp - 8) t6) (set! t5 (cfp - 0)) (set! t6 (cfp - 8)) (set! t5 (+ t5 t6)) (set! (cfp - 0) t5) (set! a0 (cfp - 0)) (jump (cfp - 0)))
                "patch-instructions: succes-03: addrs in binop")
  (check-patch? (patch-instructions
                 '(begin
                    (set! t1 0)
                    (set! t2 0)
                    (set! t3 42)
                    (set! t1 t2)
                    (set! t1 (+ t1 t3))
                    (set! a0 t1)
                    (jump (cfp - 0))))
                '(begin (set! t1 0) (set! t2 0) (set! t3 42) (set! t1 t2) (set! t1 (+ t1 t3)) (set! a0 t1) (jump (cfp - 0)))
                "patch-instructions: succes-04: multiple instructions no changes")
  (check-patch? (patch-instructions '(begin (with-label L0 (set! a1 50)) (set! a2 50) (set! a0 50) (jump L1)
                                           (with-label L1 (set! a1 50)) (set! a2 50) (set! a0 50) (jump-if L0 (= a0 a1)) (jump L2)
                                           (with-label L2 (set! a1 50)) (set! a2 50) (set! a0 50) (set! a0 a0) (jump (cfp - 0))))
               '(begin (with-label L0 (set! a1 50)) (set! a2 50) (set! a0 50) (jump L1)
                                           (with-label L1 (set! a1 50)) (set! a2 50) (set! a0 50) (jump-if L0 (= a0 a1)) (jump L2)
                                           (with-label L2 (set! a1 50)) (set! a2 50) (set! a0 50) (set! a0 a0) (jump (cfp - 0)))
               "patch-instructions: succes-05: multiple labels")
;|#
)
 
