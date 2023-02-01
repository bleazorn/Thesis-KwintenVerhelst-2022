#lang racket

(require "langs/values-unique-lang.rkt"
         "langs/imp-mf-lang.rkt")
(provide sequentialize-let)

(module+ test
  (require rackunit))

;
;(sequentialize-alocs a)->list? '(effect ...)
;a: list? '([aloc? value?] ...)
(define (sequentialize-alocs a)
  (map (lambda (alocs) `(set! ,(first alocs) ,(sequentialize-value (second alocs)))) a))
;
;(sequentialize-pred p)->pred?
;p: pred?
;; SAND: I've rewritten the `let` cases, you can use quasiquoting with splicing when you write `append` (the @ symbol)
(define (sequentialize-pred p)
  (match p
    [`(let ,a ,pred) `(begin ,@(sequentialize-alocs a) ,(sequentialize-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(sequentialize-pred p1) ,(sequentialize-pred p2) ,(sequentialize-pred p3))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(sequentialize-pred pred))]
    [_ #f]))

;
;(sequentialize-value v)->value?
;v: value?
(define (sequentialize-value v)
  (match v
    [`(let ,a ,val) `(begin ,@(sequentialize-alocs a) ,(sequentialize-value val))]
    [`(if ,p ,v1 ,v2) `(if ,(sequentialize-pred p) ,(sequentialize-value v1) ,(sequentialize-value v2))]
    [`(call ,n ,a ...) `(call ,n ,@a)]
    [t t]))

;
;(sequentialize-tail t)->tail?
;t: tail?
(define (sequentialize-tail t)
  (match t
    [`(let ,a ,tail) `(begin ,@(sequentialize-alocs a) ,(sequentialize-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(sequentialize-pred p) ,(sequentialize-tail t1) ,(sequentialize-tail t2))]
    [`(call ,n ,a ...) `(call ,n ,@a)]
    [t t]))

;
;(sequentialize-func f)->'(define label? (lambda (aloc? ...) tail?))
;f: '(define label? (lambda (aloc? ...) tail?))
(define (sequentialize-func f)
  (match f
    [`(define ,l (lambda (,a ...) ,t)) `(define ,l (lambda ,a ,(sequentialize-tail t)))]
    [_ #f]))

;
;(sequentialize-let p) → Im-lang-V3-mf?
;p: Values-lang-V3-unique?
(define/contract (sequentialize-let p) (-> values-unique-lang? imp-mf-lang?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map sequentialize-func f) ,(sequentialize-tail t))]
    [_ "sequentialize-let failed"]))

(module+ test
;sequentialize-let
  ;succes
  (check-equal? (sequentialize-let '(module () (let ([x.1 1]) x.1)))
                '(module () (begin (set! x.1 1) x.1))
                "sequentialize-let: succes-1: one let")
  (check-equal? (sequentialize-let '(module () (+ 2 2)))
                '(module () (+ 2 2))
                "sequentialize-let: succes-2: no let")
  (check-equal? (sequentialize-let '(module () (let ([x.4 (+ 2 2)]) x.4)))
                '(module () (begin (set! x.4 (+ 2 2)) x.4))
                "sequentialize-let: succes-3: one let with operation in value")
  (check-equal? (sequentialize-let '(module () (let ([x.5 2]) (let ([y.6 2]) (+ x.5 y.6)))))
                '(module () (begin (set! x.5 2) (begin (set! y.6 2) (+ x.5 y.6))))
                "sequentialize-let: succes-4: nested let")
  (check-equal? (sequentialize-let '(module () (let ([x.7 2][y.8 3]) (let ([x.9 4]) (+ x.9 y.8)))))
                '(module () (begin (set! x.7 2) (set! y.8 3) (begin (set! x.9 4) (+ x.9 y.8))))
                "sequentialize-let: succes-5: two let then a nested let")
  (check-equal? (sequentialize-let '(module () (let ([x.7 (let ([x.10 70]) (+ x.10 x.7))]) (let ([x.9 4]) (+ x.9 y.8)))))
                '(module () (begin (set! x.7 (begin (set! x.10 70) (+ x.10 x.7))) (begin (set! x.9 4) (+ x.9 y.8))))
                "sequentialize-let: succes-6: nested let and let in value")
;sequentialize-pred
  ;succes
  (check-equal? (sequentialize-pred '(< x.1 y.1)) '(< x.1 y.1) "sequentialize-pred: succes-01: relop")
  (check-equal? (sequentialize-pred '(true)) '(true) "sequentialize-pred: succes-02: true")
  (check-equal? (sequentialize-pred '(false)) '(false) "sequentialize-pred: succes-03: false")
  (check-equal? (sequentialize-pred '(not (< x.1 y.1))) '(not (< x.1 y.1)) "sequentialize-pred: succes-04: not")

  (check-equal? (sequentialize-pred '(let ([x.7 (let ([x.10 70]) (+ x.10 x.7))]) (let ([x.9 4]) (true))))
                '(begin (set! x.7 (begin (set! x.10 70) (+ x.10 x.7))) (begin (set! x.9 4) (true)))
                "sequentialize-pred: succes-05: nested let and let in value")
  (check-equal? (sequentialize-pred '(let ([x.7 (let ([x.10 70]) (+ x.10 x.7))]) (let ([x.9 4]) (= x.9 x.9))))
                '(begin (set! x.7 (begin (set! x.10 70) (+ x.10 x.7))) (begin (set! x.9 4) (= x.9 x.9)))
                "sequentialize-pred: succes-06: nested let and let in value")
  (check-equal? (sequentialize-pred '(not (let ([x.7 (let ([x.10 70]) (+ x.10 x.7))]) (let ([x.9 4]) (false)))))
                '(not (begin (set! x.7 (begin (set! x.10 70) (+ x.10 x.7))) (begin (set! x.9 4) (false))))
                "sequentialize-pred: succes-07: nested let and let in value not")

  (check-equal? (sequentialize-pred '(if (true) (false) (true))) '(if (true) (false) (true)) "sequentialize-pred: succes-08: if")
  (check-equal? (sequentialize-pred '(if (let ([x.1 1]) (true)) (false) (true))) '(if (begin (set! x.1 1) (true)) (false) (true)) "sequentialize-pred: succes-09: if")
  (check-equal? (sequentialize-pred '(if (true) (let ([x.1 1]) (true)) (true))) '(if (true) (begin (set! x.1 1) (true)) (true)) "sequentialize-pred: succes-10: if")
  (check-equal? (sequentialize-pred '(if (true) (false) (let ([x.1 1]) (true)))) '(if (true) (false) (begin (set! x.1 1) (true))) "sequentialize-pred: succes-11: if")
;sequentialize-value
  ;succes
  (check-equal? (sequentialize-value '(if (true) x.1 y.2)) '(if (true) x.1 y.2) "sequentialize-value: succes-01: if")
  (check-equal? (sequentialize-value '(if (let ([x.1 1]) (true)) (false) (true))) '(if (begin (set! x.1 1) (true)) (false) (true)) "sequentialize-value: succes-02: if")
  (check-equal? (sequentialize-value '(if (true) (let ([x.1 1]) x.1) (true))) '(if (true) (begin (set! x.1 1) x.1) (true)) "sequentialize-value: succes-03: if")
  (check-equal? (sequentialize-value '(if (true) (false) (let ([x.1 1]) x.1))) '(if (true) (false) (begin (set! x.1 1) x.1)) "sequentialize-value: succes-04: if")
;sequentialize-tail
  ;succes
  (check-equal? (sequentialize-tail '(if (true) x.1 y.2)) '(if (true) x.1 y.2) "sequentialize-tail: succes-01: if")
  (check-equal? (sequentialize-tail '(if (let ([x.1 1]) (true)) (false) (true))) '(if (begin (set! x.1 1) (true)) (false) (true)) "sequentialize-tail: succes-02: if")
  (check-equal? (sequentialize-tail '(if (true) (let ([x.1 1]) x.1) (true))) '(if (true) (begin (set! x.1 1) x.1) (true)) "sequentialize-tail: succes-03: if")
  (check-equal? (sequentialize-tail '(if (true) (false) (let ([x.1 1]) x.1))) '(if (true) (false) (begin (set! x.1 1) x.1)) "sequentialize-tail: succes-04: if")
;sequentialize-let
  ;succes
  (check-equal? (sequentialize-let '(module () (if (true) x.1 (let ([x.1 1]) x.1)))) '(module () (if (true) x.1 (begin (set! x.1 1) x.1))) "sequentialize-let: succes-01: if let")
  (check-equal? (sequentialize-let '(module ()
                                        (define L.odd?.1
                                          (lambda (x.3)
                                            (if (= x.3 0)
                                                0
                                                (let ([y.4 (+ x.3 -1)])
                                                  (call L.even?.2 y.4)))))
                                      (define L.even?.2
                                        (lambda (x.5)
                                          (if (= x.5 0)
                                              1
                                              (let ([y.6 (+ x.5 -1)])
                                                (call L.odd?.1 y.6)))))
                                      (call L.even?.2 5)))
                '(module ()
                     (define L.odd?.1
                       (lambda (x.3)
                         (if (= x.3 0)
                             0
                             (begin (set! y.4 (+ x.3 -1))
                                    (call L.even?.2 y.4)))))
                   (define L.even?.2
                     (lambda (x.5)
                       (if (= x.5 0)
                           1
                           (begin (set! y.6 (+ x.5 -1))
                                  (call L.odd?.1 y.6)))))
                   (call L.even?.2 5))
                "sequentialize-let: succes-2: tail calls")
  )
