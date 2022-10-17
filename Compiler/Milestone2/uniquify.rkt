#lang racket

(require "common.rkt")
(provide uniquify)

(module+ test
  (require rackunit))

;
;(uniquify-triv t loc)->triv (integer?/aloc?)
;t: triv? (integer?/name?)
;locs: list? '((name? aloc?) ...)
(define (uniquify-triv t loc)
  (if (name? t)
      (let ([na (assoc t loc)])
        (if na
            (second na)
            #f))
      (if (integer? t)
          t
          #f)))

;changes the names in the let naming to alocs and also gives an updated locs
;(uniquify-letNames n locs)->list? '('((aloc value)...) locs) car->'((aloc value)...) en cdr ->locs-updated
;n: list? '((name value) ...)
;locs: list? '((name aloc) ...)
(define (uniquify-letNames names locs)
  (for/lists (first sec #:result (list first (append sec locs)))
             ([n names])
    (let ([f (fresh (car n))])
      (values `(,f ,(second n)) `(,(car n) ,f)))))

;
;
;(uniquify-let l locs)->let?
;l->let?
;locs: list? '((name aloc) ...)
(define (uniquify-let l locs)
  (match l
    [`(let ,names ,body) (let ([newNames (uniquify-letNames names locs)])
                           `(let ,(car newNames) ,(uniquify-value body (second newNames))))]
    [_ #f]))
  
;
;(uniquify-value t locs)->value?
;v: value?
;locs: list? '((name aloc) ...)
(define (uniquify-value v locs)
  (match v
    [`(let ,a ,b) (uniquify-let v locs)]
    [`(,binop ,t1 ,t2) `(,binop ,(uniquify-triv t1 locs) ,(uniquify-triv t2 locs))]
    [t (uniquify-triv t locs)]))

;
;(uniquify-tail t locs)->tail?
;t: tail?
;locs: list? '((name aloc) ...)
(define (uniquify-tail t locs)
  (match t
    [`(let ,a ,b) (uniquify-let t locs)]
    [v (uniquify-value v locs)]))

;Compiles Values-lang-V3? to Values-lang-V3-unique? by resolving all lexical identifiers to abstract locations.
;(uniquify p) → Values-lang-V3-unique?
;p: Values-lang-V3?
(define (uniquify m)
  (match m
    [`(module ,a) `(module ,(uniquify-tail a '()))]))
          
(module+ test
  (define (check-uniquify t1 t2 text)
    (resetfresh)
    (check-equal? t1 t2 text))
;uniquify
;succes
  (check-uniquify (uniquify-letNames '((x 5) (y 2)) '())
                  '(((x.1 5) (y.2 2)) ((x x.1) (y y.2)))
                  "uniquify-letNames: succes-1: give let names without locs")
  (check-uniquify (uniquify-letNames '((x 5) (y 2)) '((x x.0)))
                  '(((x.1 5) (y.2 2)) ((x x.1) (y y.2) (x x.0)))
                  "uniquify-letNames: succes-2: give let names with locs")
;uniquify
  ;succes
  (check-uniquify (uniquify '(module (+ 2 2)))
                '(module (+ 2 2))
                "uniquify: succes-1: nothing to uniquify")
  (check-uniquify (uniquify '(module (let ([x 5]) x)))
                '(module (let ((x.1 5)) x.1))
                "uniquify: succes-2: one uniquify with integer as value")
  (check-uniquify (uniquify '(module (let ([x (+ 2 2)]) x)))
                '(module (let ((x.1 (+ 2 2))) x.1))
                "uniquify: succes-3: one uniquify with operation as value")
  (check-uniquify (uniquify '(module (let ([x 2]) (let ([x 2]) (+ x x)))))
                '(module (let ((x.1 2)) (let ((x.2 2)) (+ x.2 x.2))))
                "uniquify: succes-4: double let")
  (check-uniquify (uniquify '(module (let ([x 1] [y 2]) (+ x y))))
                  '(module (let ((x.1 1) (y.2 2)) (+ x.1 y.2)))
                  "uniquify: succes-3: let with double names")
  )
