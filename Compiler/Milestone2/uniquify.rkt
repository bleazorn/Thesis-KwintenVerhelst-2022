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
;names: list? '((name value) ...)
;locs: list? '((name aloc) ...)
(define (uniquify-letNames names locs)
  (for/lists (first sec #:result (list first (append sec locs)))
             ([n names])
    (let ([f (fresh (car n))])
      (values `[,f ,(uniquify-value (second n) locs)] `(,(car n) ,f)))))

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
    [`(let ,names ,body) (let ([newNames (uniquify-letNames names locs)])
                           `(let ,(car newNames) ,(uniquify-value body (second newNames))))]
    [`(,binop ,t1 ,t2) `(,binop ,(uniquify-triv t1 locs) ,(uniquify-triv t2 locs))]
    [t (uniquify-triv t locs)]))

;
;(uniquify-tail t locs)->tail?
;t: tail?
;locs: list? '((name aloc) ...)
(define (uniquify-tail t locs)
  (match t
    [`(let ,names ,body) (let ([newNames (uniquify-letNames names locs)])
                           `(let ,(car newNames) ,(uniquify-tail body (second newNames))))]
    [v (uniquify-value v locs)]))

;Compiles Values-lang-V3? to Values-lang-V3-unique? by resolving all lexical identifiers to abstract locations.
;(uniquify p) → Values-lang-V3-unique?
;p: Values-lang-V3?
(define (uniquify m)
  (match m
    [`(module ,a) `(module ,(uniquify-tail a '()))]))
    
         
(module+ test
  ;#|
  (define (check-uniquify t1 t2 text)
    (resetfresh)
    (check-equal? t1 t2 text))
  (define (check-uniquifyFalse t1 text)
    (resetfresh)
    (check-false t1 text))
;uniquify-triv
  ;succes
  (check-uniquify (uniquify-triv 'x '((x x.1))) 'x.1 "uniquify-triv: succes-1: symbol one in local")
  (check-uniquify (uniquify-triv 'x '((y y.2) (x x.1))) 'x.1 "uniquify-triv: succes-2: symbol one in local")
  (check-uniquify (uniquify-triv 'x '((x x.3) (y y.2) (x x.1))) 'x.3 "uniquify-triv: succes-3: symbol two in local")
  (check-uniquify (uniquify-triv 5 '((x x.3) (y y.2) (x x.1))) 5 "uniquify-triv: succes-4: integer")
  ;failure
  (check-uniquifyFalse (uniquify-triv 'x '()) "uniquify-triv: failure-1: symbol empty local")
  (check-uniquifyFalse (uniquify-triv 'x '((z z.2) (y y.1))) "uniquify-triv: failure-2: symbol not in local")
  (check-uniquifyFalse (uniquify-triv '(x) '((z z.2) (y y.1))) "uniquify-triv: failure-3: list")
  (check-uniquifyFalse (uniquify-triv "x" '((z z.2) (y y.1))) "uniquify-triv: failure-4: string")
;uniquify-letNames
  ;succes
  (check-uniquify (uniquify-letNames '() '()) '(() ()) "uniquify-letNames: succes-1: empty let and empty locals")
  (check-uniquify (uniquify-letNames '() '((y y.2) (x x.0))) '(() ((y y.2) (x x.0))) "uniquify-letNames: succes-2: empty let not empty list")
  (check-uniquify (uniquify-letNames '((x 2)) '()) '(((x.1 2)) ((x x.1))) "uniquify-letNames: succes-3: one name empty list")
  (check-uniquify (uniquify-letNames '((x 2)) '((y y.2) (x x.0))) '(((x.1 2)) ((x x.1) (y y.2) (x x.0))) "uniquify-letNames: succes-4: let and locals and shadowing")
  (check-uniquify (uniquify-letNames '((x 2) (y 3)) '((y y.1) (x x.0))) '(((x.1 2) (y.2 3)) ((x x.1) (y y.2) (y y.1) (x x.0))) "uniquify-letNames: succes-5: multiple let and locals and shadowing")
  (check-uniquify (uniquify-letNames '((x (+ y x))) '((y y.1) (x x.0))) '(((x.1 (+ y.1 x.0))) ((x x.1) (y y.1) (x x.0))) "uniquify-letNames: succes-6: name in binop in let name")
  (check-uniquify (uniquify-letNames '((x (let ((y y)) (+ y x)))) '((y y.1) (x x.0))) '(((x.1 (let ((y.2 y.1)) (+ y.2 x.0)))) ((x x.1) (y y.1) (x x.0))) "uniquify-letNames: succes-7: let in name")
  ;failure
;uniquify-value
  ;succes
  (check-uniquify (uniquify-value 1 '((x x.1))) 1 "uniquify-value: succes-01: integer")
  (check-uniquify (uniquify-value 'x '((x x.1))) 'x.1 "uniquify-value: succes-02: one symbol in local")
  (check-uniquify (uniquify-value 'x '((x x.1) (x x.0))) 'x.1 "uniquify-value: succes-03: one symbol in local with shadowing")
  
  (check-uniquify (uniquify-value '(+ 2 3) '((x x.1))) '(+ 2 3) "uniquify-value: succes-04: binop integers")
  (check-uniquify (uniquify-value '(+ y x) '((y y.2) (x x.1) (x x.0))) '(+ y.2 x.1) "uniquify-value: succes-05: binop different symbol in local with shadowing")
  (check-uniquify (uniquify-value '(+ x x) '((x x.1) (x x.0))) '(+ x.1 x.1) "uniquify-value: succes-06: binop same symbol in local with shadowing")

  (check-uniquify (uniquify-value '(let ((x 2)) (+ x x)) '((x x.0))) '(let ((x.1 2)) (+ x.1 x.1)) "uniquify-value: succes-07: let on name")
  (check-uniquify (uniquify-value '(let ((x 2) (y 3)) (+ y x)) '((x x.0))) '(let ((x.1 2) (y.2 3)) (+ y.2 x.1)) "uniquify-value: succes-08: let multiple name") 
  (check-uniquify (uniquify-value '(let ((x 2) (y 3)) (let ((x 4) (y 5)) (+ y x))) '((x x.0)))
                  '(let ((x.1 2) (y.2 3)) (let ((x.3 4) (y.4 5)) (+ y.4 x.3))) "uniquify-value: succes-09: nested let")
  (check-uniquify (uniquify-value '(let ((x x) (y 3)) (let ((x (+ x y))) (+ y x))) '((x x.0)))
                  '(let ((x.1 x.0) (y.2 3)) (let ((x.3 (+ x.1 y.2))) (+ y.2 x.3))) "uniquify-value: succes-10: nested let and has binop and triv val in let")
  (check-uniquify (uniquify-value '(let ((x x) (y 3)) (let ((x 4) (y (let ((x 7)) x))) (+ y x))) '((x x.0)))
                  '(let ((x.1 x.0) (y.2 3)) (let ((x.3 4) (y.4 (let ((x.5 7)) x.5))) (+ y.4 x.3))) "uniquify-value: succes-11: nested let and has let val in let")
  ;failure
  (check-uniquify (uniquify-value "x" '((x x.0))) #f "uniquify-value: failure-01: triv has wrong symbol")
  (check-uniquify (uniquify-value '(+ "x" 4) '((x x.0))) '(+ #f 4) "uniquify-value: failure-02: binop has wrong symbol")
  (check-uniquify (uniquify-value '(let ((x 2)) (x)) '((x x.0))) '(let ((x.1 2)) #f) "uniquify-value: failure-03: let has wrong symbol")
;uniquify-tail
  ;succes
  (check-uniquify (uniquify-tail 1 '((x x.1))) 1 "uniquify-tail: succes-01: integer")
  (check-uniquify (uniquify-tail 'x '((x x.1))) 'x.1 "uniquify-tail: succes-02: one symbol in local")
  (check-uniquify (uniquify-tail 'x '((x x.1) (x x.0))) 'x.1 "uniquify-tail: succes-03: one symbol in local with shadowing")
  
  (check-uniquify (uniquify-tail '(+ 2 3) '((x x.1))) '(+ 2 3) "uniquify-tail: succes-04: binop integers")
  (check-uniquify (uniquify-tail '(+ y x) '((y y.2) (x x.1) (x x.0))) '(+ y.2 x.1) "uniquify-tail: succes-05: binop different symbol in local with shadowing")
  (check-uniquify (uniquify-tail '(+ x x) '((x x.1) (x x.0))) '(+ x.1 x.1) "uniquify-tail: succes-06: binop same symbol in local with shadowing")

  (check-uniquify (uniquify-tail '(let ((x 2)) (+ x x)) '((x x.0))) '(let ((x.1 2)) (+ x.1 x.1)) "uniquify-tail: succes-07: let on name")
  (check-uniquify (uniquify-tail '(let ((x 2) (y 3)) (+ y x)) '((x x.0))) '(let ((x.1 2) (y.2 3)) (+ y.2 x.1)) "uniquify-tail: succes-08: let multiple name") 
  (check-uniquify (uniquify-tail '(let ((x 2) (y 3)) (let ((x 4) (y 5)) (+ y x))) '((x x.0)))
                  '(let ((x.1 2) (y.2 3)) (let ((x.3 4) (y.4 5)) (+ y.4 x.3))) "uniquify-tail: succes-09: nested let")
  (check-uniquify (uniquify-tail '(let ((x x) (y 3)) (let ((x (+ x y))) (+ y x))) '((x x.0)))
                  '(let ((x.1 x.0) (y.2 3)) (let ((x.3 (+ x.1 y.2))) (+ y.2 x.3))) "uniquify-tail: succes-10: nested let and has binop and triv val in let")
  (check-uniquify (uniquify-tail '(let ((x x) (y 3)) (let ((x 4) (y (let ((x 7)) x))) (+ y x))) '((x x.0)))
                  '(let ((x.1 x.0) (y.2 3)) (let ((x.3 4) (y.4 (let ((x.5 7)) x.5))) (+ y.4 x.3))) "uniquify-tail: succes-11: nested let and has let val in let")
  ;failure
  (check-uniquify (uniquify-tail "x" '((x x.0))) #f "uniquify-tail: failure-01: triv has wrong symbol")
  (check-uniquify (uniquify-tail '(+ "x" 4) '((x x.0))) '(+ #f 4) "uniquify-tail: failure-02: binop has wrong symbol")
  (check-uniquify (uniquify-tail '(let ((x 2)) (x)) '((x x.0))) '(let ((x.1 2)) #f) "uniquify-tail: failure-03: let has wrong symbol")
  (check-uniquify (uniquify-tail '(let ((x x)) (+ x x)) '((y y.0))) '(let ((x.1 #f)) (+ x.1 x.1)) "uniquify-tail: failure-04: name not in local")
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
  (check-uniquify (uniquify '(module  (let ((x 4) (y 3)) (let ((x 4) (y (let ((x 7)) x))) (+ y x)))))
                  '(module (let ((x.1 4) (y.2 3)) (let ((x.3 4) (y.4 (let ((x.5 7)) x.5))) (+ y.4 x.3)))) "uniquify: succes-4: nested let and has let val in let")
  ;|#
  )
