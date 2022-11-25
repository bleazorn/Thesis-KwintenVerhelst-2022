#lang racket

(provide check-values-lang)

(module+ test
  (require rackunit))

;(check-name n locs)name?
;n: name?
;locs: list? '(name? ...)
(define (check-name n locs)
  (cond [(member n locs) n]
        [(error (format "check name ~a failed" n))]))

;
;(interp-triv t locs)->integer?
;t: triv?
;locs: list? '(name? ...)
(define (check-triv t locs)
  (if (integer? t)
      t
      (check-name t locs)))

;(interp-binop binop)->binop?
;binop: binop?
(define (check-binop binop)
  (match binop
    ['+ '+]
    ['* '*]
    ['- '-]
    [_ (error (format "check binop ~a failed" binop))]))

;(interp-relop relop)->relop?
;relop: relop?
(define (check-relop relop)
  (match relop
    ['=  '=]
    ['!= '!=]
    ['<  '<]
    ['>  '>]
    ['<= '<=]
    ['>= '>=]
    [_ (error (format "check relop ~a failed" relop))]))

;
;(interp-let lets locs)->list? '(name? value?)
;lets: list? '(name? value)
;locs: list? '(name? ...)
(define (check-let l locs funcs)
  (match l
    [`(,x ,v) `(,x ,(check-value v (cons x locs) funcs))]
    [_ (error (format "check let ~a failed" l))]))

;
;(interp-value p locs)->pred?
;p: pred?
;locs: list? '(name? ...)
(define (check-predi p locs funcs)
  (match p
    [`(let (,l ...) ,pred) `(let ,(map (lambda (le) (check-let le locs funcs)) l) ,(check-predi pred (append locs (map car l)) funcs))]
    [`(if ,p1 ,p2 ,p3)     `(if ,(check-predi p1 locs funcs) ,(check-predi p2 locs funcs) ,(check-predi p3 locs funcs))]
    ['(true)               '(true)]
    ['(false)              '(false)]
    [`(not ,pred)          `(not ,(check-predi pred locs funcs))]
    [`(,relop ,t1 ,t2)     `(,(check-relop relop) ,(check-triv t1 locs) ,(check-triv t2 locs))]
    [_ (error (format "check pred ~a failed" p))]))

;
;(interp-value v locs)->value?
;v: value?
;locs: list? '(name? ...)
(define (check-value v locs funcs)
  (match v
    [`(let (,l ...) ,val) `(let ,(map (lambda (le) (check-let le locs funcs)) l) ,(check-value val (append locs (map car l)) funcs))]
    [`(if ,p ,v1 ,v2)     `(if ,(check-predi p locs funcs) ,(check-value v1 locs funcs) ,(check-value v2 locs funcs))] 
    [`(call ,x ,trivs ...) `(call ,(check-name x funcs) ,@(map (lambda (x) (check-triv x locs)) trivs))]
    [`(,binop ,t1 ,t2)    `(,(check-binop binop) ,(check-triv t1 locs) ,(check-triv t2 locs))]
    [t                     (check-triv t locs)]))


;
;(interp-tail t '())->tail?
;t: tail?
;locs: list? '(name? ...)
;funcs: list? '(name? ...)
(define (check-tail t locs funcs)
  (match t
    [`(let (,l ...) ,tail) `(let ,(map (lambda (le) (check-let le locs funcs)) l) ,(check-tail tail (append locs (map car l)) funcs))]
    [`(if ,p ,t1 ,t2)      `(if ,(check-predi p locs funcs) ,(check-tail t1 locs funcs) ,(check-tail t2 locs funcs))]
    [`(call ,x ,trivs ...) `(call ,(check-name x funcs) ,@(map (lambda (x) (check-triv x locs)) trivs))]
    [v                      (check-value v locs funcs)]))

;
;(interp-func x t locs funcs)->'(define name? (lambda (name? ...) tail?))
;f: '(define name? (lambda (name? ...) tail?))
;t: list? '(triv ...)
;locs: list? '(name? ...)
;funcs: list? '(name? ...)
(define (check-func f funcs)
  (match f
    [`(define ,l (lambda ,a ,t)) `(define ,(check-name l funcs) (lambda ,a ,(check-tail t a funcs)))]
    [_ (error (format "check functie ~a failed" (take f 2)))]))

;
;(interp-values-lang p) →integer?
;p : Values-lang-V3?
(define (check-values-lang p)
  (match p
    [`(module ,f ... ,t) (let ([funcs (map second f)])
                           `(module ,@(map (lambda (fun) (check-func fun funcs)) f) ,(check-tail t '() funcs)))]
    [_ (error "check values-lang failed in module")]))

(module+ test
  (define check-check?
    (lambda l
      (let ([leng (length l)])
        (cond [(= 3 leng) (check-equal? ((first l) (second l)) (second l) (third l))]
              [(= 4 leng) (check-equal? ((first l) (second l) (third l)) (second l) (fourth l))]
              [(= 5 leng) (check-equal? ((first l) (second l) (third l) (fourth l)) (second l) (fifth l))]))))

  (define check-error?
    (lambda l
      (let ([leng (length l)])
        (cond [(= 3 leng) (check-exn exn:fail? (thunk ((first l) (second l)) (third l)))]
              [(= 4 leng) (check-exn exn:fail? (thunk ((first l) (second l) (third l)) (fourth l)))]
              [(= 5 leng) (check-exn exn:fail? (thunk ((first l) (second l) (third l) (fourth l)) (fifth l)))]))))
;check-name
  ;succes
  (check-check? check-name 'y '(x y z) "check-name: succes-01: name exists")
  ;failure
  (check-error? check-name 'y '(x z) "check-name: failure-01: name doesn't exist")
;check-triv
  ;succes
  (check-check? check-triv 'y '(x y z) "check-triv: succes-01: name exists")
  (check-check? check-triv 5 '(x y z) "check-triv: succes-01: integer")
  ;failure
  (check-error? check-triv 'y '(x z) "check-triv: failure-01: name doesn't exist")
;check-binop
  ;succes
  (check-check? check-binop '+ "check-binop: succes-01: add")
  (check-check? check-binop '* "check-binop: succes-02: mul")
  ;failure
  (check-error? check-binop 'y "check-binop: failure-01: not a binop")
;check-relop
  ;succes
  (check-check? check-relop '= "check-relop: succes-01: =")
  (check-check? check-relop '!= "check-relop: succes-01: !=")
  (check-check? check-relop '< "check-relop: succes-01: <")
  (check-check? check-relop '> "check-relop: succes-01: >")
  (check-check? check-relop '<= "check-relop: succes-01: <=")
  (check-check? check-relop '>= "check-relop: succes-01: >=")
  ;failure
  (check-error? check-relop 'y "check-relop: failure-01: not a relop")
;check-let
  ;succes
  (check-check? check-let '(y (+ x z)) '(x y z) '() "check-let: succes-01: let")
  ;failure
  (check-error? check-let '(y (+ x z)) '(x y) '() "check-let: failure-01: value failed")
;check-predi
  ;succes
  (check-check? check-predi '(true) '(x y z) '() "check-predi: succes-01: true")
  (check-check? check-predi '(false) '(x y z) '() "check-predi: succes-02: false")
  (check-check? check-predi '(= x y) '(x y z) '() "check-predi: succes-03: relop")
  (check-check? check-predi '(not (!= y z)) '(x y z) '() "check-predi: succes-04: not")
  (check-check? check-predi '(if (< x y) (> y z) (<= x z)) '(x y z) '() "check-predi: succes-05: if")
  (check-check? check-predi '(let ([a (* x y)]) (>= a z)) '(x y z) '() "check-predi: succes-06: let")
  ;failure
  (check-error? check-predi 'y '(x y) '() "check-predi: failure-01: not a pred")
  (check-error? check-predi '(+ x y) '(x y) '() "check-predi: failure-02: not a relop")
  (check-error? check-predi '(= x y) '(x) '() "check-predi: failure-03: relop triv 1 failed")
  (check-error? check-predi '(= x y) '(y) '() "check-predi: failure-04: relop triv 2 failed")
  (check-error? check-predi '(not (+ x z)) '(x y z) '() "check-predi: failure-05: not not")

  (check-error? check-predi '(if (< x y) (> y z) (<= x z)) '(x z) '() "check-predi: failure-06: if not pred 1")
  (check-error? check-predi '(if (< x y) (> y z) (<= x y)) '(x y) '() "check-predi: failure-07: if not pred 2")
  (check-error? check-predi '(if (< x y) (> y x) (<= x z)) '(x y) '()"check-predi: failure-08: if not pred 3")

  (check-error? check-predi '(let ([a (* x y)]) (>= a z)) '(x z) '() "check-predi: failure-09: let value failed")
  (check-error? check-predi '(let ([a (* x y)]) (>= a z)) '(x y) '() "check-predi: failure-10: let pred failed")
;check-value
  ;succes
  (check-check? check-value 'y '(x y z) '() "check-value: succes-01: name exists")
  (check-check? check-value '(+ x y) '(x y z) '() "check-value: succes-02: binop")
  (check-check? check-value '(if (< x y) (+ y z) (* x z)) '(x y z) '() "check-value: succes-03: if")
  (check-check? check-value '(let ([a (+ x y)]) a) '(x y z) '() "check-value: succes-04: let")
  ;failure
  (check-error? check-value 'y '(x) '() "check-value: failure-01: not a name")
  (check-error? check-value '(= x y) '(x y) '() "check-value: failure-02: not a binop")
  (check-error? check-value '(+ x y) '(x) '() "check-value: failure-03: binop triv 1 failed")
  (check-error? check-value '(* x y) '(y) '() "check-value: failure-04: binop triv 2 failed")

  (check-error? check-value '(if (< x y) (+ y z) (* x z)) '(x z) '() "check-value: failure-06: if not pred")
  (check-error? check-value '(if (< x y) (+ y z) (* x y)) '(x y) '() "check-value: failure-07: if not val 1")
  (check-error? check-value '(if (< x y) (+ y x) (* x z)) '(x y) '() "check-value: failure-08: if not val 2")

  (check-error? check-value '(let ([a (* x y)]) (+ a z)) '(x z) '() "check-value: failure-09: let value failed")
  (check-error? check-value '(let ([a (* x y)]) (+ a z)) '(x y) '() "check-value: failure-10: let pred failed")
;check-tail
  ;succes
  (check-check? check-tail 'y '(x y z) '(fun1 fun2) "check-tail: succes-01: name exists")
  (check-check? check-tail '(+ x y) '(x y z) '(fun1 fun2) "check-tail: succes-02: binop")
  (check-check? check-tail '(if (< x y) (+ y z) (* x z)) '(x y z) '(fun1 fun2) "check-tail: succes-03: if")
  (check-check? check-tail '(let ([a (+ x y)]) a) '(x y z) '(fun1 fun2) "check-tail: succes-04: let")
  (check-check? check-tail '(call fun2 x y) '(x y z) '(fun1 fun2) "check-tail: succes-05: call")
  ;failure
  (check-error? check-tail 'y '(x) '() "check-tail: failure-01: not a name")
  (check-error? check-tail '(= x y) '(x y) '() "check-tail: failure-02: not a binop")
  (check-error? check-tail '(+ x y) '(x) '() "check-tail: failure-03: binop triv 1 failed")
  (check-error? check-tail '(* x y) '(y) '() "check-tail: failure-04: binop triv 2 failed")

  (check-error? check-tail '(if (< x y) (+ y z) (* x z)) '(x z) '() "check-tail: failure-06: if not pred")
  (check-error? check-tail '(if (< x y) (+ y z) (* x y)) '(x y) '() "check-tail: failure-07: if not val 1")
  (check-error? check-tail '(if (< x y) (+ y x) (* x z)) '(x y) '() "check-tail: failure-08: if not val 2")

  (check-error? check-tail '(let ([a (* x y)]) (+ a z)) '(x z) '() "check-tail: failure-09: let value failed")
  (check-error? check-tail '(let ([a (* x y)]) (+ a z)) '(x y) '() "check-tail: failure-10: let pred failed")

  (check-error? check-tail '(call fun2 x y) '(x y z) '(fun1) "check-tail: succes-05: call name doesn't exist")
  (check-error? check-tail '(call fun2 x y) '(x z) '(fun1 fun2) "check-tail: succes-05: arg name doesn't exist")

;check-values-lang
  ;succes
  (check-check? check-values-lang '(module (let ([x 5]
                                                 [y 6])
                                             x))
                "check-values-lang: succes-01: one let")
  (check-check? check-values-lang '(module (let ([x 5]
                                                 [y 6])
                                             (let ([y x])
                                               y)))
                "check-values-lang: succes-02: two lets")
  (check-check? check-values-lang '(module (let ()
                           5))
                "check-values-lang: succes-03: empty let")
  (check-check? check-values-lang '(module (let ([x 5]
                                                 [y 4])
                                             (if (= x y)
                                                 (+ x y)
                                                 (* x y))))
                "check-values-lang: succes-04: if")
  (check-check? check-values-lang '(module
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
                                     (call even? 5))
                "check-values-lang: succes-05: tail call") 
  ;failure
  (check-error? check-values-lang '(module
                     (let ([x 5]
                           [y x])
                       y))
             "check-values-lang: failure-01: no bound 'x in enviroment yet")
  #;(check-error? '(module
                       (let ([x 5]
                             [x 6])
                         x))
                "check-values-lang: failure-2: bound the same name twice")
  (check-error? check-values-lang '(module (let () x))
             "check-values-lang: failure-03: no bound name in enviroment")
  )