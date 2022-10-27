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
    [`(set! ,a (if ,p ,v1 ,v2)) `(if ,p ,(normalize-set a v1) ,(normalize-set a v1))]
    [`(set! ,a ,b) e]
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e))]))


;
;(normalize-pred p)->pred?
;p:pred?
(define (normalize-pred p)
  (match p
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(normalize-pred pred))]
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e) ,(normalize-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(normalize-pred p1) ,(normalize-pred p2) ,(normalize-pred p3))]
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
                "normalize-effect: succes-1: a normalizing last value is not begin")
  (check-equal? (normalize-effect '(set! y.3 (+ x.1 x.2)))
                '(set! y.3 (+ x.1 x.2))
                "normalize-effect: succes-2: set no normalizing")
  (check-equal? (normalize-effect '(begin (set! x.1 2) (set! x.2 2)))
                '(begin (set! x.1 2) (set! x.2 2))
                "normalize-effect: succes-3: begin no normalizing")
  (check-equal? (normalize-effect '(begin (set! x.1 2) (set! x.2 2)))
                '(begin (set! x.1 2) (set! x.2 2))
                "normalize-effect: succes-4: normalizing")
  (check-equal? (normalize-effect '(set! y.3 (begin (set! x.1 2) (set! x.2 2) (begin (set! x.3 2) (set! x.4 2) (+ x.3 x.4)))))
                '(begin (set! x.1 2) (set! x.2 2) (begin (set! x.3 2) (set! x.4 2) (set! y.3 (+ x.3 x.4))))
                "normalize-effect: succes-5: normalizing with value a begin")
  (check-equal? (normalize-effect '(set! y.3 (begin (set! x.1 2) (set! y.4 (begin (set! x.5 2) (set! x.6 2) (+ x.5 x.6))) (begin (set! x.3 2) (set! x.4 2) (+ y.4 x.4)))))
                '(begin (set! x.1 2) (begin (set! x.5 2) (set! x.6 2) (set! y.4 (+ x.5 x.6))) (begin (set! x.3 2) (set! x.4 2) (set! y.3 (+ y.4 x.4))))
                "normalize-effect: succes-5: normalizing with value a begin")
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
