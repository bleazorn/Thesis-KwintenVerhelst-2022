#lang racket

(require "common/aloc.rkt"
         "langs/exprs-lang.rkt"
         "langs/exprs-unique-lang.rkt")
(provide uniquify)

(module+ test
  (require rackunit))

;
;(uniquify-triv t loc)->triv (integer?/aloc?)
;t: triv? (integer?/name?)
;locs: list? '((name? aloc?) ... (name label) ...)
;; SAND: can be written more concisely, you could also use `match` (<= with (? integer?), ... as pattern) or `cond` (<= syntactic sugar for nested if exprs)
(define (uniquify-triv t loc)
  (define na (assoc t loc))
  (or (and na (second na))
      (and (integer? t) t)))

;changes the names in the let naming to alocs and also gives an updated locs
;(uniquify-letNames n locs)->list? '('((aloc value)...) locs) car->'((aloc value)...) en cdr ->locs-updated
;names: list? '((name value) ...)
;locs: list? '((name aloc) ... (name label) ...)
(define (uniquify-letNames names locs)
  (for/lists (first sec #:result (list first (append sec locs)))
             ([n names])
    (let ([f (freshAloc (car n))])
      (values `[,f ,(uniquify-value (second n) locs)] `(,(car n) ,f)))))

;
;(uniquify-pred p locs)->pred?
;p: pred?
;locs: list? '((name aloc) ... (name label) ...)
(define (uniquify-pred p locs)
  (match p
    [`(let ,names ,body) (let ([new-names (uniquify-letNames names locs)])
                           `(let ,(car new-names) ,(uniquify-pred body (second new-names))))]
    [`(if ,p1 ,p2 ,p3) `(if ,(uniquify-pred p1 locs) ,(uniquify-pred p2 locs) ,(uniquify-pred p3 locs))]
    [`(,relop ,a ,b) `(,relop ,(uniquify-triv a locs) ,(uniquify-triv b locs))]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(uniquify-pred pred locs))]
    [_ (error (format "uniquify:  Failed match.\n No valid pred: ~a" p))]))
  
;
;(uniquify-value t locs)->value?
;v: value?
;locs: list? '((name aloc) ... (name label) ...)
(define (uniquify-value v locs)
  (match v
    [`(let ,names ,body) (let ([new-names (uniquify-letNames names locs)])
                           `(let ,(car new-names) ,(uniquify-value body (second new-names))))]
    [`(if ,p ,v1 ,v2) `(if ,(uniquify-pred p locs) ,(uniquify-value v1 locs) ,(uniquify-value v2 locs))]
    [`(call ,n ,vs ...) `(call ,(uniquify-triv n locs) ,@(map (lambda (vi) (uniquify-value vi locs)) vs))]
    [`(,binop ,v1 ,v2) `(,binop ,(uniquify-value v1 locs) ,(uniquify-value v2 locs))]
    [t (uniquify-triv t locs)]))
#|
;
;(uniquify-tail t locs)->tail?
;t: tail?
;locs: list? '((name aloc) ... (name label) ...)
(define (uniquify-tail t locs)
  (match t
    [`(let ,names ,body) (let ([new-names (uniquify-letNames names locs)])
                           `(let ,(car new-names) ,(uniquify-tail body (second new-names))))]
    [`(if ,p ,t1 ,t2) `(if ,(uniquify-pred p locs) ,(uniquify-tail t1 locs) ,(uniquify-tail t2 locs))]
    [`(call ,n ,a ...) `(call ,(uniquify-triv n locs) ,@(map (lambda (tr) (uniquify-triv tr locs)) a))]
    [v (uniquify-value v locs)]))
|#
;
;(funcNamesLoc names)->list? '((name aloc) ... (name label) ...)
;names: list? '(name? ...)
;locs: list? '((name aloc) ... (name label) ...)
(define (uniquify-funcNamesLoc names locs)
  (append (map (lambda (name) `(,name ,(freshAloc name))) names) locs))


;
;(funcNamesLoc names)->list? '((name aloc) ... (name label) ...)
;names: list? '(name? ...)
;locs: list? '((name aloc) ... (name label) ...)
(define (uniquify-func-para name)
  `(,name ,(freshAloc name)))


;
;(uniquify-func f)->'(define label? (lambda (aloc? ...) tail?))
;f: '(define label? (lambda (name? ...) tail?))
;locs: list? '((name aloc) ... (name label) ...)
(define (uniquify-func f locs)
  (match f
    [`(define ,l (lambda (,n ...) ,t)) (let ([func-names (append (map uniquify-func-para n) locs)])
                                         `(define ,(uniquify-triv l func-names) (lambda ,(map (lambda (name) (uniquify-triv name func-names)) n) ,(uniquify-value t func-names))))]
    [_ (error (format "uniquify: Failed match.\n No valid function: ~a" f))]))


;(uniquify-func-name f)->list? '(name label)
;f: '(define label? (lambda (name? ...) tail?))
(define (uniquify-func-label f)
  (let ([name (second f)])
    `(,name ,(freshLabel name))))

;Compiles Values-lang-V3? to Values-lang-V3-unique? by resolving all lexical identifiers to abstract locations.
;(uniquify p) → Values-lang-V3-unique?
;p: Values-lang-V3?
(define/contract (uniquify m) (-> exprs-lang? exprs-unique-lang?)
  (match m
    [`(module ,f ... ,a) (let ([func-names (map uniquify-func-label f)])
                           `(module () ,@(map (lambda (func) (uniquify-func func func-names)) f) ,(uniquify-value a func-names)))]))
         
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
;uniquify-pred
  ;succes
  (check-uniquify (uniquify-pred '(< x 5) '((x x.1))) '(< x.1 5) "uniquify-pred: succes-01: relop int change")
  (check-uniquify (uniquify-pred '(true) '((x x.1))) '(true) "uniquify-pred: succes-02: true")
  (check-uniquify (uniquify-pred '(false) '((x x.1))) '(false) "uniquify-pred: succes-03: false")
  (check-uniquify (uniquify-pred '(not (< x 5)) '((x x.1))) '(not (< x.1 5)) "uniquify-pred: succes-04: not relop")
  
  (check-uniquify (uniquify-pred '(let ((x 2)) (= x x)) '((x x.0))) '(let ((x.1 2)) (= x.1 x.1)) "uniquify-value: succes-05: let on name")
  (check-uniquify (uniquify-pred '(let ((x 2) (y 3)) (< y x)) '((x x.0))) '(let ((x.1 2) (y.2 3)) (< y.2 x.1)) "uniquify-value: succes-06: let multiple name") 
  (check-uniquify (uniquify-pred '(let ((x 2) (y 3)) (let ((x 4) (y 5)) (> y x))) '((x x.0)))
                  '(let ((x.1 2) (y.2 3)) (let ((x.3 4) (y.4 5)) (> y.4 x.3))) "uniquify-value: succes-07: nested let")
  (check-uniquify (uniquify-pred '(let ((x x) (y 3)) (let ((x (+ x y))) (!= y x))) '((x x.0)))
                  '(let ((x.1 x.0) (y.2 3)) (let ((x.3 (+ x.1 y.2))) (!= y.2 x.3))) "uniquify-value: succes-08: nested let and has relop and triv val in let")
  (check-uniquify (uniquify-pred '(let ((x x) (y 3)) (let ((x 4) (y (let ((x 7)) x))) (<= y x))) '((x x.0)))
                  '(let ((x.1 x.0) (y.2 3)) (let ((x.3 4) (y.4 (let ((x.5 7)) x.5))) (<= y.4 x.3))) "uniquify-value: succes-09: nested let and has let val in let")
  (check-uniquify (uniquify-pred '(let ((x x) (y 3)) (let ((x 4) (y (let ((x 7)) x))) (true))) '((x x.0)))
                  '(let ((x.1 x.0) (y.2 3)) (let ((x.3 4) (y.4 (let ((x.5 7)) x.5))) (true))) "uniquify-value: succes-10: nested let and has let val in let")

  (check-uniquify (uniquify-pred '(if (< x y) (= x z) (= y z)) '((x x.1) (y y.2) (z z.3))) '(if (< x.1 y.2) (= x.1 z.3) (= y.2 z.3)) "uniquify-pred: succes-11: if relop")
  (check-uniquify (uniquify-pred '(if (if (< x y) (= x z) (= y z)) (= x z) (= y z)) '((x x.1) (y y.2) (z z.3)))
                  '(if (if (< x.1 y.2) (= x.1 z.3) (= y.2 z.3)) (= x.1 z.3) (= y.2 z.3))
                  "uniquify-pred: succes-12: if relop")
  (check-uniquify (uniquify-pred '(if (< x y) (if (< x y) (= x z) (= y z)) (= y z)) '((x x.1) (y y.2) (z z.3)))
                  '(if (< x.1 y.2) (if (< x.1 y.2) (= x.1 z.3) (= y.2 z.3)) (= y.2 z.3))
                  "uniquify-pred: succes-13: if relop")
  (check-uniquify (uniquify-pred '(if (< x y) (= x z) (if (< x y) (= x z) (= y z))) '((x x.1) (y y.2) (z z.3)))
                  '(if (< x.1 y.2) (= x.1 z.3) (if (< x.1 y.2) (= x.1 z.3) (= y.2 z.3)))
                  "uniquify-pred: succes-14: if relop")

  ;failure
  (check-uniquify (uniquify-pred '(= x y) '((x x.1))) '(= x.1 #f) "uniquify-pred: failure-01: relop not change")
  (check-exn exn:fail? (thunk (uniquify-pred '(not) '((x x.1)))) "uniquify-pred: failure-02: wrong expression")

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

  (check-uniquify (uniquify-value '(if (< x y) (+ x z) (+ y z)) '((x x.1) (y y.2) (z z.3))) '(if (< x.1 y.2) (+ x.1 z.3) (+ y.2 z.3)) "uniquify-value: succes-12: if relop")
  (check-uniquify (uniquify-value '(if (if (< x y) (= x z) (= y z)) (+ x z) (+ y z)) '((x x.1) (y y.2) (z z.3)))
                  '(if (if (< x.1 y.2) (= x.1 z.3) (= y.2 z.3)) (+ x.1 z.3) (+ y.2 z.3))
                  "uniquify-value: succes-13: if relop")
  (check-uniquify (uniquify-value '(if (< x y) (if (< x y) (+ x z) (+ y z)) (+ y z)) '((x x.1) (y y.2) (z z.3)))
                  '(if (< x.1 y.2) (if (< x.1 y.2) (+ x.1 z.3) (+ y.2 z.3)) (+ y.2 z.3))
                  "uniquify-value: succes-13: if relop")
  (check-uniquify (uniquify-value '(if (< x y) (+ x z) (if (< x y) (+ x z) (+ y z))) '((x x.1) (y y.2) (z z.3)))
                  '(if (< x.1 y.2) (+ x.1 z.3) (if (< x.1 y.2) (+ x.1 z.3) (+ y.2 z.3)))
                  "uniquify-value: succes-14: if relop")
  (check-uniquify (uniquify-value `(call fun x y z) '((fun L.fun.4) (x x.1) (y y.2) (z z.3)))
                  `(call L.fun.4 x.1 y.2 z.3)
                  "uniquify-value: succes-15: call")
  ;failure
  (check-uniquify (uniquify-value "x" '((x x.0))) #f "uniquify-value: failure-01: triv has wrong symbol")
  (check-uniquify (uniquify-value '(+ "x" 4) '((x x.0))) '(+ #f 4) "uniquify-value: failure-02: binop has wrong symbol")
  (check-uniquify (uniquify-value '(let ((x 2)) (x)) '((x x.0))) '(let ((x.1 2)) #f) "uniquify-value: failure-03: let has wrong symbol")
  (check-uniquify (uniquify-value `(call fun x y z) '((x x.1) (y y.2) (z z.3)))
                  `(call #f x.1 y.2 z.3)
                  "uniquify-value: failure-04: call name")
  (check-uniquify (uniquify-value `(call fun x y z) '((fun L.fun.4) (y y.2) (z z.3)))
                  `(call L.fun.4 #f y.2 z.3)
                  "uniquify-value: failure-04: call arg 1")
  (check-uniquify (uniquify-value `(call fun x y z) '((fun L.fun.4) (x x.1) (z z.3)))
                  `(call L.fun.4 x.1 #f z.3)
                  "uniquify-value: failure-04: call arg 2")
  #|
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

  (check-uniquify (uniquify-tail '(if (< x y) (+ x z) (+ y z)) '((x x.1) (y y.2) (z z.3))) '(if (< x.1 y.2) (+ x.1 z.3) (+ y.2 z.3)) "uniquify-tail: succes-12: if relop")
  (check-uniquify (uniquify-tail '(if (if (< x y) (= x z) (= y z)) (+ x z) (+ y z)) '((x x.1) (y y.2) (z z.3)))
                  '(if (if (< x.1 y.2) (= x.1 z.3) (= y.2 z.3)) (+ x.1 z.3) (+ y.2 z.3))
                  "uniquify-tail: succes-13: if relop")
  (check-uniquify (uniquify-tail '(if (< x y) (if (< x y) (+ x z) (+ y z)) (+ y z)) '((x x.1) (y y.2) (z z.3)))
                  '(if (< x.1 y.2) (if (< x.1 y.2) (+ x.1 z.3) (+ y.2 z.3)) (+ y.2 z.3))
                  "uniquify-tail: succes-13: if relop")
  (check-uniquify (uniquify-tail '(if (< x y) (+ x z) (if (< x y) (+ x z) (+ y z))) '((x x.1) (y y.2) (z z.3)))
                  '(if (< x.1 y.2) (+ x.1 z.3) (if (< x.1 y.2) (+ x.1 z.3) (+ y.2 z.3)))
                  "uniquify-tail: succes-14: if relop")
  (check-uniquify (uniquify-tail `(call fun x y z) '((fun L.fun.4) (x x.1) (y y.2) (z z.3)))
                  `(call L.fun.4 x.1 y.2 z.3)
                  "uniquify-tail: succes-15: call")
  ;failure
  (check-uniquify (uniquify-tail "x" '((x x.0))) #f "uniquify-tail: failure-01: triv has wrong symbol")
  (check-uniquify (uniquify-tail '(+ "x" 4) '((x x.0))) '(+ #f 4) "uniquify-tail: failure-02: binop has wrong symbol")
  (check-uniquify (uniquify-tail '(let ((x 2)) (x)) '((x x.0))) '(let ((x.1 2)) #f) "uniquify-tail: failure-03: let has wrong symbol")
  (check-uniquify (uniquify-tail '(let ((x x)) (+ x x)) '((y y.0))) '(let ((x.1 #f)) (+ x.1 x.1)) "uniquify-tail: failure-04: name not in local")
    (check-uniquify (uniquify-tail `(call fun x y z) '((x x.1) (y y.2) (z z.3)))
                  `(call #f x.1 y.2 z.3)
                  "uniquify-tail: failure-05: call name")
  (check-uniquify (uniquify-tail `(call fun x y z) '((fun L.fun.4) (y y.2) (z z.3)))
                  `(call L.fun.4 #f y.2 z.3)
                  "uniquify-tail: failure-06: call arg 1")
  (check-uniquify (uniquify-tail `(call fun x y z) '((fun L.fun.4) (x x.1) (z z.3)))
                  `(call L.fun.4 x.1 #f z.3)
                  "uniquify-tail: failure-07: call arg 2")
  |#
;uniquify-func-label
  ;succes
  (check-uniquify (uniquify-func-label '(define odd?
                                         (lambda (x)
                                           (if (= x 0)
                                               0
                                               (let ([y (+ x -1)])
                                                 (call even? y))))))
                  '(odd? L.odd?.1)
                  "uniquify-func-label succes-01: name gathering")
;uniquify-func
  ;succes
  (check-uniquify (uniquify-func '(define odd?
                                    (lambda (x)
                                      (if (= x 0)
                                          0
                                          (let ([y (+ x -1)])
                                            (call even? y)))))
                                 '((even? L.even?.2) (odd? L.odd?.1)))
                  '(define L.odd?.1
                     (lambda (x.1)
                       (if (= x.1 0)
                           0
                           (let ((y.2 (+ x.1 -1)))
                             (call L.even?.2 y.2)))))
                  "uniquify-func: succes-01: name gathering")
  ;failure
  (check-exn exn:fail? (thunk (uniquify-func '(defµine odd?
                                                (lambda (x)
                                                  (if (= x 0)
                                                      0
                                                      (let ([y (+ x -1)])
                                                        (call even? y)))))
                                             '((even? L.even?.2) (odd? L.odd?.1))))
                  "uniquify-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (uniquify-func '(define
                                                (lambda (x)
                                                  (if (= x 0)
                                                      0
                                                      (let ([y (+ x -1)])
                                                        (call even? y)))))
                                             '((even? L.even?.2) (odd? L.odd?.1))))
                  "uniquify-func: failure-02: no name")
  (check-exn exn:fail? (thunk (uniquify-func '(define odd?
                                                (lambdµa (x)
                                                  (if (= x 0)
                                                      0
                                                      (let ([y (+ x -1)])
                                                        (call even? y)))))
                                             '((even? L.even?.2) (odd? L.odd?.1))))
                  "uniquify-func: failure-03:  wrong datum literal lambda")
;uniquify
  ;succes
  (check-uniquify (uniquify '(module (+ 2 2)))
                '(module () (+ 2 2))
                "uniquify: succes-1: nothing to uniquify")
  (check-uniquify (uniquify '(module (let ([x 5]) x)))
                '(module () (let ((x.1 5)) x.1))
                "uniquify: succes-2: one uniquify with integer as value")
  (check-uniquify (uniquify '(module (let ([x (+ 2 2)]) x)))
                '(module () (let ((x.1 (+ 2 2))) x.1))
                "uniquify: succes-3: one uniquify with operation as value")
  (check-uniquify (uniquify '(module (let ([x 2]) (let ([x 2]) (+ x x)))))
                '(module () (let ((x.1 2)) (let ((x.2 2)) (+ x.2 x.2))))
                "uniquify: succes-4: double let")
  (check-uniquify (uniquify '(module (let ([x 1] [y 2]) (+ x y))))
                  '(module () (let ((x.1 1) (y.2 2)) (+ x.1 y.2)))
                  "uniquify: succes-5: let with double names")
  (check-uniquify (uniquify '(module (let ((x 4) (y 3)) (let ((x 4) (y (let ((x 7)) x))) (+ y x)))))
                  '(module () (let ((x.1 4) (y.2 3)) (let ((x.3 4) (y.4 (let ((x.5 7)) x.5))) (+ y.4 x.3)))) "uniquify: succes-6: nested let and has let val in let")
  (check-uniquify (uniquify '(module
                                 (define odd?
                                   (lambda (x)
                                     (if (= x 0)
                                         0
                                         (let ([y (+ x -1)])
                                           (call even? y)))))
                               (define even?
                                 (lambda (x)
                                   (if (= x 0)
                                       1
                                       (let ([y (+ x -1)])
                                         (call odd? y)))))
                               (call even? 5)))
                  '(module ()
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
                     (call L.even?.2 5))
                  "uniquify: succes-6: tail calls")
  (check-uniquify (uniquify '(module 
                                 (define addn?
                                   (lambda (x n)
                                     (+ x n)))
                               (let ([x (+ 4 5)])
                                 (call addn? x 5))))
                  '(module ()
                       (define L.addn?.1
                         (lambda (x.2 n.3)
                           (+ x.2 n.3)))
                     (let ([x.4 (+ 4 5)])
                       (call L.addn?.1 x.4 5)))
                  "uniquify: succes-07: tail calls with int and names")
  (check-uniquify (uniquify '(module 
                                 (define addn?
                                   (lambda (x n)
                                     (+ x n)))
                               (let ([x 5])
                                 (let ([y (call addn? x 5)])
                                   (call addn? x y)))))
                  '(module ()
                       (define L.addn?.1
                         (lambda (x.2 n.3)
                           (+ x.2 n.3)))
                     (let ([x.4 5])
                       (let ([y.5 (call L.addn?.1 x.4 5)])
                         (call L.addn?.1 x.4 y.5))))
                  "uniquify: succes-08: value calls")
  ;|#
  )
