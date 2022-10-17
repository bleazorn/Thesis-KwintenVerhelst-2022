#lang racket

(require "common.rkt")
(provide patch-instructions)

(module+ test
  (require rackunit))

;claims t3 t4 t5

(define (select-binop binop t1 t2 tail)
  (if (and (triv? t1) (triv? t2))
      (let* ([tmp1 (if (integer? t1) (freshtmp) t1)]
             [tmp2 (if (and (equal? binop '*) (integer? t2)) (freshtmp) t2)]
             [pre1 (if (integer? t1) `((set! ,tmp1 ,t1)) '())]
             [pre2 (if (and (equal? binop '*) (integer? t2)) (append pre1 `((set! ,tmp2 ,t2))) pre1)] 
             )
        (append pre2 `((set! ,tmp1 (,binop ,tmp1 ,tmp2))) `(,(append tail `(,tmp1)))))
      #f))

(define (triv? t)
  (or (integer? t) (or (fvar? t) (isRegister? t))))

;
;(patch-triv a b c)->list? '((set! ...) ...)
;trivs:list? '(triv? ...)
(define (patch-triv trivs)
  (for/fold ([i 3] [l '()] [same '()] #:result l)
            ([t trivs])
    (if (and (or (fvar? t) (integer? t)) (not (assoc t same)))
        (values (add1 i)
                (append l `((,t ,(string->symbol (string-append "t" (~a i))))))
                (cons `(,t ,(string->symbol (string-append "t" (~a i)))) same))
        (let ([isSame (assoc t same)])
          (if isSame
              (values i
                      (append l `((,(car isSame) ,(second isSame))))
                      same)
              (values i
                      (append l `((,t ,t)))
                      same))))))

;
;(patch-binop a b c binop)
;a,b,c:triv?
(define (patch-binop a b c binop)
  (if (and (triv? a) (and (triv? b) (triv? c)))
      (let ([trivs (if (and (integer? c) (equal? '+ binop)) (patch-triv `(,a ,b)) (patch-triv `(,a ,b ,c)))])
        ;trivs) #f))#|
        (for/fold ([sets (if (and (integer? c) (equal? '+ binop))
                             `((set! ,(second (car trivs)) (,binop ,(second (second trivs)) ,c)))
                             `((set! ,(second (car trivs)) (,binop ,(second (second trivs)) ,(second (third trivs))))))]
                   [same '()] #:result sets)
                  ([t trivs])
          (if (or (equal? (car t) (second t)) (member (car t) same))
              (values sets same)
              (values (cons `(set! ,(second t) ,(car t)) sets) (cons (car t) same)))))
      #f))                                                                                   ;|#

;Longer version from patch-set to better visualize every step
;(patch-setLong s)->list? '((set! ...) ...)
;s: (set! ...)
(define (patch-setLong s)
  (match s
    [`(set! ,a ,b) #:when (and (isRegister? a) (isRegister? b)) `((set! ,a ,b))]              ;copy register
    [`(set! ,a ,b) #:when (and (isRegister? a) (integer? b)) `((set! ,a ,b))]                 ;integer in register
    [`(set! ,a ,b) #:when (and (isRegister? a) (fvar? b)) `((set! ,a ,b))]                    ;load in register
    [`(set! ,a ,b) #:when (and (fvar? a) (isRegister? b)) `((set! ,a ,b))]                    ;store register in memory
    [`(set! ,a ,b) #:when (and (fvar? a) (integer? b)) `((set! t3 ,b) (set! ,a t3))]          ;store integer in memory
    [`(set! ,a ,b) #:when (and (fvar? a) (fvar? b)) `((set! t3 ,b) (set! ,a t3))]))           ;store memory in memory

;
;(patch-set s)->list? '((set! ...) ...)
;s: (set! ...)
(define (patch-set s)
  (match s
    [`(set! ,a ,b) #:when (or (isRegister? a) (isRegister? b)) `((set! ,a ,b))]                  
    [`(set! ,a ,b) #:when (and (fvar? a) (or (integer? b) (fvar? b))) `((set! t3 ,b) (set! ,a t3))]
    [_ #f]))


;
;(patch-effect e)->list? (
;e->effect
(define (patch-effect e)
  (match e
    [`(set! ,a (,binop ,b ,c)) (patch-binop a b c binop)]
    [`(set! ,a ,b) (patch-set e)]
    [_ #f]))

;
;(patch-instructions p) → paren-x64-fvars-v2?
;p : para-asm-lang-v2?
(define (patch-instructions p)
  (match p
    [`(begin ,s ... (halt ,t))  `(begin ,@(foldl (lambda (e sets) (append sets (patch-effect e))) '() s) (set! 'a0 ,t))]
    [_ #f]))

;(patch-triv '(fv1 fv1 fv4))
;(patch-binop 'fv1 'fv2 'fv1 '*)

(module+ test
;patch-instructions
  ;succes
  (check-equal? (patch-instructions '(begin (set! a1 42) (halt a1)))
                '(begin (set! a1 42) (set! 'a0 a1))
                "patch-instructions: succes-1: one instruction")
  (check-equal? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv1 42)
                    (set! fv0 fv1)
                    (halt fv0)))
                '(begin (set! fv0 0) (set! fv1 42) `(set! t3 ,r2) `(set! ,r1 t3) (set! 'a0 fv0))
                "patch-instructions: succes-2: a fvar in second argument")
  (check-equal? (patch-instructions
                 '(begin
                    (set! t1 0)
                    (set! t2 0)
                    (set! t4 42)
                    (set! t1 t2)
                    (set! t1 (+ t1 t4))
                    (halt t1)))
                '(begin (set! t1 0) (set! t2 0) (set! t4 42) (set! t1 t2) (set! t1 (+ t1 t4)) (set! 'a0 t1))
                "patch-instructions: succes-3: multiple instructions"))
