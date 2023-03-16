#lang racket

(provide resolve-predicates)

(module+ test
  (require rackunit))

;
;(resolve-not p)->pred?
;p: pred?
(define (resolve-not p)
  (match p
    [`(= ,a ,b) `(!= ,a ,b)]
    [`(!= ,a ,b) `(= ,a ,b)]
    [`(< ,a ,b) `(>= ,a ,b)]
    [`(> ,a ,b) `(<= ,a ,b)]
    [`(<= ,a ,b) `(> ,a ,b)]
    [`(>= ,a ,b) `(< ,a ,b)]
    ['(true) '(false)]
    ['(false) '(true)]
    [`(not ,pred) (resolve-not (resolve-not pred))]
    [_ #f]))

;
;(resolve-pred p)->pred?
;p: pred?
(define (resolve-pred p)
  (match p
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) (resolve-not pred)]
    [_ #f]))

;
;(resolve-if p l1 l2)->tail?
;p: pred?
;l1: label?
;l2: label?
(define (resolve-if p l1 l2)
  (let ([pred (resolve-pred p)])
    (match pred
      [`(,relop ,a ,b) `(if (,relop ,a ,b) (jump ,l1) (jump ,l2))]
      [`(true) `(jump ,l1)]
      [`(false) `(jump ,l2)]
      [_ #f])))
    

;
;(resolve-tail t)->tail?
;t: tail?
(define (resolve-tail t)
  (match t
    [`(halt ,a) `(halt ,a)]
    [`(jump ,l) `(jump ,l)]
    [`(begin ,e ... ,tail) `(begin ,@e ,(resolve-tail tail))]
    [`(if ,p (jump ,l1) (jump ,l2)) (resolve-if p l1 l2)]
    [_ #f]))

;
;(resolve-b b)->b?
;b:b?
(define (resolve-b b)
  (match b
    [`(define ,l ,t) `(define ,l ,(resolve-tail t))]
    [_ #f]))

;
;(resolve-predicates p)->Pred-lang-V4-Block
;p:Asm-lang-V4-Block
(define (resolve-predicates p)
  (match p
    [`(module ,b ...) `(module ,@(map (lambda (def) (resolve-b def)) b))]
    [_ #f]))

(module+ test
  ;resolve-not
  ;succes
  (check-equal? (resolve-not '(= a0 a1)) '(!= a0 a1) "resolve-not: succes-01: equal")
  (check-equal? (resolve-not '(!= a0 a1)) '(= a0 a1) "resolve-not: succes-02: not equal")
  (check-equal? (resolve-not '(< a0 a1)) '(>= a0 a1) "resolve-not: succes-03: lesser then")
  (check-equal? (resolve-not '(> a0 a1)) '(<= a0 a1) "resolve-not: succes-04: greater then")
  (check-equal? (resolve-not '(<= a0 a1)) '(> a0 a1) "resolve-not: succes-05: l and e")
  (check-equal? (resolve-not '(>= a0 a1)) '(< a0 a1) "resolve-not: succes-06: g and e")

  (check-equal? (resolve-not '(true)) '(false) "resolve-not: succes-07: true")
  (check-equal? (resolve-not '(false)) '(true) "resolve-not: succes-08: false")

  (check-equal? (resolve-not '(not (true))) '(true) "resolve-not: succes-09: not")
  (check-equal? (resolve-not '(not (not (true)))) '(false) "resolve-not: succes-10: not not")
  (check-equal? (resolve-not '(not (not (not (true))))) '(true) "resolve-not: succes-11: not not not")

  ;resolve-pred
  ;succes
  (check-equal? (resolve-pred '(= a0 a1)) '(= a0 a1) "resolve-pred: succes-01: equal")
  (check-equal? (resolve-pred '(!= a0 a1)) '(!= a0 a1) "resolve-pred: succes-02: not equal")
  (check-equal? (resolve-pred '(< a0 a1)) '(< a0 a1) "resolve-pred: succes-03: lesser then")
  (check-equal? (resolve-pred '(> a0 a1)) '(> a0 a1) "resolve-pred: succes-04: greater then")
  (check-equal? (resolve-pred '(<= a0 a1)) '(<= a0 a1) "resolve-pred: succes-05: l and e")
  (check-equal? (resolve-pred '(>= a0 a1)) '(>= a0 a1) "resolve-pred: succes-06: g and e")

  (check-equal? (resolve-pred '(true)) '(true) "resolve-pred: succes-07: true")
  (check-equal? (resolve-pred '(false)) '(false) "resolve-pred: succes-08: false")

  (check-equal? (resolve-pred '(not (true))) '(false) "resolve-pred: succes-09: not")
  (check-equal? (resolve-pred '(not (not (true)))) '(true) "resolve-pred: succes-10: not not")
  (check-equal? (resolve-pred '(not (not (not (true))))) '(false) "resolve-pred: succes-11: not not not")
  
  ;resolve-if
  ;succes
  (check-equal? (resolve-if '(= a0 a1) 'L1 'L2) '(if (= a0 a1) (jump L1) (jump L2)) "resolve-if: succes-01: equal")
  (check-equal? (resolve-if '(!= a0 a1) 'L1 'L2) '(if (!= a0 a1) (jump L1) (jump L2)) "resolve-if: succes-02: not equal")
  (check-equal? (resolve-if '(< a0 a1) 'L1 'L2) '(if (< a0 a1) (jump L1) (jump L2)) "resolve-if: succes-03: lesser then")
  (check-equal? (resolve-if '(> a0 a1) 'L1 'L2) '(if (> a0 a1) (jump L1) (jump L2)) "resolve-if: succes-04: greater then")
  (check-equal? (resolve-if '(<= a0 a1) 'L1 'L2) '(if (<= a0 a1) (jump L1) (jump L2)) "resolve-if: succes-05: l and e")
  (check-equal? (resolve-if '(>= a0 a1) 'L1 'L2) '(if (>= a0 a1) (jump L1) (jump L2)) "resolve-if: succes-06: g and e")

  (check-equal? (resolve-if '(true) 'L1 'L2) '(jump L1) "resolve-if: succes-07: true")
  (check-equal? (resolve-if '(false) 'L1 'L2) '(jump L2) "resolve-if: succes-08: false")

  (check-equal? (resolve-if '(not (true)) 'L1 'L2) '(jump L2) "resolve-if: succes-09: not")
  (check-equal? (resolve-if '(not (not (true))) 'L1 'L2) '(jump L1) "resolve-if: succes-10: not not")
  (check-equal? (resolve-if '(not (not (not (true)))) 'L1 'L2) '(jump L2) "resolve-if: succes-11: not not not")

  ;resolve-tail
  ;succes
  (check-equal? (resolve-tail '(halt a0)) '(halt a0) "resolve-tail: succes-01: halt")
  (check-equal? (resolve-tail '(jump a1)) '(jump a1) "resolve-tail: succes-02: jump")
  
  (check-equal? (resolve-tail '(begin (set! a0 a1) (set! a0 (+ a1 a2)) (halt a0)))
                '(begin (set! a0 a1) (set! a0 (+ a1 a2)) (halt a0))
                "resolve-tail: succes-03: begin")
  (check-equal? (resolve-tail '(begin (set! a0 a1) (set! a0 (+ a1 a2)) (begin (set! a0 a1) (set! a0 (+ a1 a2)) (halt a0))))
                '(begin (set! a0 a1) (set! a0 (+ a1 a2)) (begin (set! a0 a1) (set! a0 (+ a1 a2)) (halt a0)))
                "resolve-tail: succes-04: begin")
  (check-equal? (resolve-tail '(begin (set! a0 a1) (set! a0 (+ a1 a2)) (if (not (not (false))) (jump L1) (jump L2))))
                '(begin (set! a0 a1) (set! a0 (+ a1 a2)) (jump L2))
                "resolve-tail: succes-05: begin")

  (check-equal? (resolve-tail '(if (not (not (true))) (jump L1) (jump L2))) '(jump L1) "resolve-tail: succes-06: if")
  (check-equal? (resolve-tail '(if (not (not (false))) (jump L1) (jump L2))) '(jump L2) "resolve-tail: succes-07: if")
  (check-equal? (resolve-tail '(if (not (not (= a0 a1))) (jump L1) (jump L2))) '(if (= a0 a1) (jump L1) (jump L2)) "resolve-tail: succes-08: if")

  ;resolve-predicates
  ;succes
  (check-equal? (resolve-predicates '(module (define L0 (if (not (not (false))) (jump L1) (jump L2))) (define L1 (jump L2)) (define L2 (halt a0))))
                '(module (define L0 (jump L2)) (define L1 (jump L2)) (define L2 (halt a0)))
                "resolve-predicates: succes-01: mult def")
  )