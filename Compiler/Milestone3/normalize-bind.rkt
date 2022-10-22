#lang racket

(provide normalize-bind)

(module+ test
  (require rackunit))

;
;(normalize-effect e)->effect?
;effect?
(define (normalize-effect e)
  (match e
    [`(set! ,a (begin ,e ... ,v)) (append '(begin) (map (lambda (eff) (normalize-effect eff)) e) (match v
                                                                                                   [`(begin ,e ...) `(,(normalize-effect `(set! ,a ,v)))]
                                                                                                   [_ `((set! ,a ,v))]))]
    [`(set! ,a ,b) e]
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e))]))

;
;(normalize-value v)->value?
;v: value?
(define (normalize-value v)
  (match v
    [`(begin ,e ... ,val) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e) ,(normalize-value val))]
    [v v]))

;
;(normalize-tail t)->tail?
;t: tail?
(define (normalize-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (normalize-effect eff)) e) ,(normalize-tail tail))]
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
                "normalize-effect: succes-1: a normalizing")
  (check-equal? (normalize-effect '(set! y.3 (+ x.1 x.2)))
                '(set! y.3 (+ x.1 x.2))
                "normalize-effect: succes-2: set no normalizing")
  (check-equal? (normalize-effect '(begin (set! x.1 2) (set! x.2 2)))
                '(begin (set! x.1 2) (set! x.2 2))
                "normalize-effect: succes-3: begin no normalizing")
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
