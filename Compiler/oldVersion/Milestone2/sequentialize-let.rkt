#lang racket

(provide sequentialize-let)

(module+ test
  (require rackunit))

;
;(sequentialize-alocs a)->list? '(effect ...)
;a: list? '([aloc? value?] ...)
(define (sequentialize-alocs a)
  (map (lambda (alocs) `(set! ,(first alocs) ,(sequentialize-value (second alocs)))) a))

;
;(sequentialize-value v)->value?
;v: value?
(define (sequentialize-value v)
  (match v
    [`(let ,a ,val) (append '(begin) (sequentialize-alocs a) `(,(sequentialize-value val)))]
    [t t]))

;
;(sequentialize-tail t)->tail?
;t: tail?
(define (sequentialize-tail t)
  (match t
    [`(let ,a ,tail) `(begin ,@(sequentialize-alocs a) ,(sequentialize-tail tail))]
    [t t]))

;Combination of sequentialize-tail en sequentialize-value
;
;
(define (sequentialize-tailval tv)
  (match tv
    [`(let ,a ,tail) `(begin ,@(sequentialize-alocs a) ,(sequentialize-tailval tail))]
    [tv tv]))

;
;(sequentialize-let p) → Im-lang-V3-mf?
;p: Values-lang-V3-unique?
(define (sequentialize-let p)
  (match p
    [`(module ,tail) `(module ,(sequentialize-tail tail))]
    [_ #f]))

(module+ test
;sequentialize-let
  ;succes
  (check-equal? (sequentialize-let '(module (let ([x.1 1]) x.1)))
                '(module (begin (set! x.1 1) x.1))
                "sequentialize-let: succes-1: one let")
  (check-equal? (sequentialize-let '(module (+ 2 2)))
                '(module (+ 2 2))
                "sequentialize-let: succes-2: no let")
  (check-equal? (sequentialize-let '(module (let ([x.4 (+ 2 2)]) x.4)))
                '(module (begin (set! x.4 (+ 2 2)) x.4))
                "sequentialize-let: succes-3: one let with operation in value")
  (check-equal? (sequentialize-let '(module (let ([x.5 2]) (let ([y.6 2]) (+ x.5 y.6)))))
                '(module (begin (set! x.5 2) (begin (set! y.6 2) (+ x.5 y.6))))
                "sequentialize-let: succes-4: nested let")
  (check-equal? (sequentialize-let '(module (let ([x.7 2][y.8 3]) (let ([x.9 4]) (+ x.9 y.8)))))
                '(module (begin (set! x.7 2) (set! y.8 3) (begin (set! x.9 4) (+ x.9 y.8))))
                "sequentialize-let: succes-5: two let then a nested let")
  (check-equal? (sequentialize-let '(module (let ([x.7 (let ([x.10 70]) (+ x.10 x.7))]) (let ([x.9 4]) (+ x.9 y.8)))))
                '(module (begin (set! x.7 (begin (set! x.10 70) (+ x.10 x.7))) (begin (set! x.9 4) (+ x.9 y.8))))
                "sequentialize-let: succes-6: nested let and let in value")
  )