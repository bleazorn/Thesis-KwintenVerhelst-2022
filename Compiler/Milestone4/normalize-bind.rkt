#lang racket

(provide normalize-bind)

(module+ test
  (require rackunit))

(define (normalize-set a v)
  (match v
    [`(begin ,e ...) `(,(normalize-effect `(set! ,a ,v)))]
    [`(if ,p ,v1 ,v2) `(,(normalize-effect `(set! ,a ,v)))]
    [_ `((set! ,a ,v))]))

;
;(normalize-effect e)->effect?
;effect?
(define (normalize-effect e)
  (match e
    [`(set! ,a (begin ,e ... ,v)) (append '(begin) (map (lambda (eff) (normalize-effect eff)) e) (normalize-set a v))]
    [`(set! ,a (if ,p ,v1 ,v2)) `(if ,p ,@(normalize-set a v1) ,@(normalize-set a v2))]
    [`(set! ,a ,b) e]
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e))]))


;
;(normalize-pred p)->pred?
;p:pred?
(define (normalize-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e) ,(normalize-pred pred))]
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
  (match v
    [`(begin ,e ... ,val) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e) ,(normalize-value val))]
    [`(if ,p ,v1 ,v2) `(if ,(normalize-pred p) ,(normalize-value v1) ,(normalize-value v2))]
    [v v]))

;
;(normalize-tail t)->tail?
;t: tail?
(define (normalize-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e) ,(normalize-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(normalize-pred p) ,(normalize-tail t1) ,(normalize-tail t2))]
    [t (normalize-value t)]))

;Compiles Imp-lang-V3-mf? to Imp-lang-V3-cmf?, pushing set! under begin so that the right-hand-side of each set! is simple value-producing operation. This normalizes Imp-lang-V3-mf? with respect to the equations
;(normalize-bind p) → Imp-lang-V3-cmf?
;p: Imp-lang-V3-mf?
(define (normalize-bind p)
  (match p
    [`(module ,tail) `(module ,(normalize-tail tail))]
    [_ #f]))


(module+ test
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
                "normalize-bind: succes-4: begin value in begin effect in begin effect tree normalizing"))
