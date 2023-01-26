#lang racket

(require "log.rkt")
(provide normalize-bind)

(module+ test
  (require rackunit))

;; SAND: the extra parentheses were redundant here, I removed them and updated the call sites accordingly
(define (normalize-set a v)
  (match v
    [`(begin ,e ...) (normalize-effect `(set! ,a ,v))]
    [`(if ,p ,v1 ,v2) (normalize-effect `(set! ,a ,v))]
    [_ `(set! ,a ,v)]))

;
;(normalize-effect e)->effect?
;effect?
(define (normalize-effect e)
  ;(logln "effect:")
  ;(pretty-display e)
  (match e
    [`(set! ,a (begin ,e ... ,v)) `(begin ,@(map normalize-effect e) ,(normalize-set a v))] ;; SAND: rewritten with quasiquoting, also note that you can just pass the procedure as is, no need to wrap it in a lambda (change pushed through in other places too)
    [`(set! ,a (if ,p ,v1 ,v2)) `(if ,(normalize-pred p) ,(normalize-set a v1) ,(normalize-set a v2))]
    [`(set! ,a ,b) e]
    [`(begin ,e ...) `(begin ,@(map normalize-effect e))]))

;
;(normalize-pred p)->pred?
;p:pred?
(define (normalize-pred p)
  ;(logln "pred:")
  ;(pretty-display p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map normalize-effect e) ,(normalize-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(normalize-pred p1) ,(normalize-pred p2) ,(normalize-pred p3))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(normalize-pred pred))]
    [_ #f]))

;
;(normalize-value v)->value?
;v: value?
(define (normalize-value v)
  ;(logln "value:")
  ;(pretty-display v)
  (match v
    [`(begin ,e ... ,val) `(begin ,@(map normalize-effect e) ,(normalize-value val))]
    [`(if ,p ,v1 ,v2) `(if ,(normalize-pred p) ,(normalize-value v1) ,(normalize-value v2))]
    [`(call ,n ,a ...) `(call ,n ,@a)]
    [v v]))

;
;(normalize-tail t)->tail?
;t: tail?
(define (normalize-tail t)
  ;(logln "tail:")
  ;(pretty-display t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map normalize-effect e) ,(normalize-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(normalize-pred p) ,(normalize-tail t1) ,(normalize-tail t2))]
    [`(call ,n ,a ...) `(call ,n ,@a)]
    [t (normalize-value t)]))

;
;(normalize-entry e)->entry?
;e: entry?
(define (normalize-entry e)
  (normalize-tail e))

;
;(normalize-func f)->'(define label? (lambda (aloc? ...) tail?))
;f: '(define label? (lambda (aloc? ...) tail?))
(define (normalize-func f)
  (match f
    [`(define ,l (lambda (,a ...) ,t)) `(define ,l (lambda ,a ,(normalize-entry t)))]
    [_ #f]))

;Compiles Imp-lang-V3-mf? to Imp-lang-V3-cmf?, pushing set! under begin so that the right-hand-side of each set! is simple value-producing operation. This normalizes Imp-lang-V3-mf? with respect to the equations
;(normalize-bind p) → Imp-lang-V3-cmf?
;p: Imp-lang-V3-mf?
(define (normalize-bind p)
  (match p
    [`(module ,f ... ,t) `(module ,@(map normalize-func f) ,(normalize-entry t))]
    [_ "normalize-bind failed"]))


(module+ test
  ;#|
  ;normalize-effect
  ;succes
  (check-equal? (normalize-effect '(set! y.3 (begin (set! x.1 2) (set! x.2 2) (+ x.1 x.2))))
                '(begin (set! x.1 2) (set! x.2 2) (set! y.3 (+ x.1 x.2)))
                "normalize-effect: succes-01: a normalizing last value is not begin")
  (check-equal? (normalize-effect '(set! y.3 (+ x.1 x.2)))
                '(set! y.3 (+ x.1 x.2))
                "normalize-effect: succes-02: set no normalizing")
  (check-equal? (normalize-effect '(begin (set! x.1 2) (set! x.2 2)))
                '(begin (set! x.1 2) (set! x.2 2))
                "normalize-effect: succes-03: begin no normalizing")
  (check-equal? (normalize-effect '(begin (set! x.1 2) (set! x.2 2)))
                '(begin (set! x.1 2) (set! x.2 2))
                "normalize-effect: succes-04: normalizing")
  
  (check-equal? (normalize-effect '(set! y.3 (begin (set! x.1 2) (set! x.2 2) (begin (set! x.3 2) (set! x.4 2) (+ x.3 x.4)))))
                '(begin (set! x.1 2) (set! x.2 2) (begin (set! x.3 2) (set! x.4 2) (set! y.3 (+ x.3 x.4))))
                "normalize-effect: succes-05: normalizing with value a begin")
  (check-equal? (normalize-effect '(set! y.3 (begin (set! x.1 2) (set! y.4 (begin (set! x.5 2) (set! x.6 2) (+ x.5 x.6))) (begin (set! x.3 2) (set! x.4 2) (+ y.4 x.4)))))
                '(begin (set! x.1 2) (begin (set! x.5 2) (set! x.6 2) (set! y.4 (+ x.5 x.6))) (begin (set! x.3 2) (set! x.4 2) (set! y.3 (+ y.4 x.4))))
                "normalize-effect: succes-06: normalizing with value a begin")
  
  (check-equal? (normalize-effect '(set! z.3 (if (true) x.1 (+ x.1 y.2))))
                '(if (true) (set! z.3 x.1) (set! z.3 (+ x.1 y.2)))
                "normalize-effect: succes-07: normalize if")
  (check-equal? (normalize-effect '(set! z.3 (if (true) (begin (set! x.1 y.2) x.1) (if (= x.1 y.2) y.2 (+ x.1 y.2)))))
                '(if (true) (begin (set! x.1 y.2) (set! z.3 x.1)) (if (= x.1 y.2) (set! z.3 y.2) (set! z.3 (+ x.1 y.2))))
                "normalize-effect: succes-08: normalize nested if")

  ;normalize-pred
  ;succes
  (check-equal? (normalize-pred '(< x.1 y.1)) '(< x.1 y.1) "normalize-pred: succes-01: relop")
  (check-equal? (normalize-pred '(true)) '(true) "normalize-pred: succes-02: true")
  (check-equal? (normalize-pred '(false)) '(false) "normalize-pred: succes-03: false")
  (check-equal? (normalize-pred '(not (< x.1 y.1))) '(not (< x.1 y.1)) "normalize-pred: succes-04: not")

  (check-equal? (normalize-pred '(begin (set! z.3 (begin (set! x.1 y.2) x.1)) (true))) '(begin (begin (set! x.1 y.2) (set! z.3 x.1)) (true)) "normalize-pred: succes-05: begin")

  (check-equal? (normalize-pred '(if (true) (false) (not (= x.1 y.1)))) '(if (true) (false) (not (= x.1 y.1))) "normalize-pred: succes-06: if")
  ;normalize-value
  ;succes
  (check-equal? (normalize-value '(begin (set! z.3 (begin (set! x.1 y.2) x.1)) (+ x.1 y.2))) '(begin (begin (set! x.1 y.2) (set! z.3 x.1)) (+ x.1 y.2)) "normalize-value: succes-05: begin")
  (check-equal? (normalize-value '(if (true) (+ x.1 y.2) (+ x.1 y.2))) '(if (true) (+ x.1 y.2) (+ x.1 y.2)) "normalize-value: succes-06: if")
  ;normalize-tail
  ;succes
  (check-equal? (normalize-tail '(begin (set! z.3 (begin (set! x.1 y.2) x.1)) (+ x.1 y.2))) '(begin (begin (set! x.1 y.2) (set! z.3 x.1)) (+ x.1 y.2)) "normalize-tail: succes-05: begin")
  (check-equal? (normalize-tail '(if (true) (+ x.1 y.2) (+ x.1 y.2))) '(if (true) (+ x.1 y.2) (+ x.1 y.2)) "normalize-tail: succes-06: if")

  ;normalize-bind
  ;succes
  (check-equal? (normalize-bind '(module (+ 2 2)))
                '(module (+ 2 2))
                "normalize-bind: succes-1: value tail no normalizing")
  (check-equal? (normalize-bind '(module (begin (set! x.1 2) (set! x.2 2) (+ x.1 x.2))))
                '(module (begin (set! x.1 2) (set! x.2 2) (+ x.1 x.2)))
                "normalize-bind: succes-2: begin tail no normalizing")
  (check-equal? (normalize-bind '(module (begin (set! x.1 2) (set! x.2 (begin (set! x.2 3) (+ x.2 2))) (+ x.1 x.2))))
                '(module (begin (set! x.1 2) (begin (set! x.2 3) (set! x.2 (+ x.2 2))) (+ x.1 x.2)))
                "normalize-bind: succes-3: begin effect one normalizing")
  (check-equal? (normalize-bind '(module (begin (set! x.1 2) (set! x.2 (begin (set! x.2 3) (begin (set! z.3 5) (+ x.2 z.3)))) (+ x.1 x.2))))
                '(module (begin (set! x.1 2) (begin (set! x.2 3) (begin (set! z.3 5) (set! x.2 (+ x.2 z.3)))) (+ x.1 x.2)))
                "normalize-bind: succes-4: begin value in begin effect two normalizing")
  (check-equal? (normalize-bind '(module (begin (set! x.1 2) (set! x.2 (begin (set! x.2 3) (begin (set! z.3 5) (begin (set! a.4 15) (+ x.2 z.3))))) (+ x.1 x.2))))
                '(module (begin (set! x.1 2) (begin (set! x.2 3) (begin (set! z.3 5) (begin (set! a.4 15) (set! x.2 (+ x.2 z.3))))) (+ x.1 x.2)))
                "normalize-bind: succes-4: begin value in begin effect in begin effect tree normalizing")
  (check-equal? (normalize-bind '(module
                                     (if (false)
                                         (if (not (if (not (false)) (< 419 -178) (false)))
                                             (begin
                                               (set! x1.1
                                                     (begin
                                                       (set! x2.2 (* -112 -316))
                                                       (set! x3.3
                                                             (if (not (<= 8 -16))
                                                                 258
                                                                 (if (not
                                                                      (if (begin
                                                                            (set! x7.4 (+ 367 -203))
                                                                            (set! x8.5 (+ -236 -62))
                                                                            (set! x9.6 (if (false) (* -294 -146) -425))
                                                                            (set! x10.7 (begin (set! x12.8 62) (* x12.8 x12.8)))
                                                                            (set! x11.9
                                                                                  (begin
                                                                                    (set! x13.10 (+ -84 -24))
                                                                                    (set! x14.11
                                                                                          (begin
                                                                                            (set! x15.12
                                                                                                  (begin
                                                                                                    (set! x20.13 (+ 201 289))
                                                                                                    (set! x21.14 (+ -343 8))
                                                                                                    (set! x22.15 -35)
                                                                                                    (set! x23.16 -22)
                                                                                                    (+ 382 x22.15)))
                                                                                            (set! x16.17 (* -166 449))
                                                                                            (set! x17.18 (* 439 102))
                                                                                            (set! x18.19 (+ 369 461))
                                                                                            (set! x19.20 -2)
                                                                                            (* 215 430)))
                                                                                    (* -409 x13.10)))
                                                                            (<= x8.5 -62))
                                                                          (true)
                                                                          (false)))
                                                                     -306
                                                                     90)))
                                                       (set! x4.21 (+ 313 228))
                                                       (set! x5.22 (+ 3 110))
                                                       (set! x6.23 (+ 450 415))
                                                       -358))
                                               (* -242 350))
                                             (+ 189 -501))
                                         -462)))
                '(module
                     (if (false)
                         (if (not (if (not (false)) (< 419 -178) (false)))
                             (begin
                               (begin
                                 (set! x2.2 (* -112 -316))
                                 (if (not (<= 8 -16))
                                     (set! x3.3 258)
                                     (if (not
                                          (if (begin
                                                (set! x7.4 (+ 367 -203))
                                                (set! x8.5 (+ -236 -62))
                                                (if (false) (set! x9.6 (* -294 -146)) (set! x9.6 -425))
                                                (begin (set! x12.8 62) (set! x10.7 (* x12.8 x12.8)))
                                                (begin
                                                  (set! x13.10 (+ -84 -24))
                                                  (begin
                                                    (begin
                                                      (set! x20.13 (+ 201 289))
                                                      (set! x21.14 (+ -343 8))
                                                      (set! x22.15 -35)
                                                      (set! x23.16 -22)
                                                      (set! x15.12 (+ 382 x22.15)))
                                                    (set! x16.17 (* -166 449))
                                                    (set! x17.18 (* 439 102))
                                                    (set! x18.19 (+ 369 461))
                                                    (set! x19.20 -2)
                                                    (set! x14.11 (* 215 430)))
                                                  (set! x11.9 (* -409 x13.10)))
                                                (<= x8.5 -62))
                                              (true)
                                              (false)))
                                         (set! x3.3 -306)
                                         (set! x3.3 90)))
                                 (set! x4.21 (+ 313 228))
                                 (set! x5.22 (+ 3 110))
                                 (set! x6.23 (+ 450 415))
                                 (set! x1.1 -358))
                               (* -242 350))
                             (+ 189 -501))
                         -462))
                "normalize-bind: succes-5: complex program if begin")
  (check-equal? (normalize-bind '(module (define L.meth.1 (lambda (x.1) (begin (set! x.2 (begin (set! x.2 3) (begin (set! z.3 5) (begin (set! a.4 15) (+ x.2 z.3))))) (+ x.1 x.2))))
                                   (call L.meth.1 2)))
                '(module (define L.meth.1 (lambda (x.1) (begin (begin (set! x.2 3) (begin (set! z.3 5) (begin (set! a.4 15) (set! x.2 (+ x.2 z.3))))) (+ x.1 x.2))))
                   (call L.meth.1 2))
                "normalize-bind: succes-6: tail call")
  )
