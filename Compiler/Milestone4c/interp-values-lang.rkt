#lang racket

(provide interp-values-lang)

(module+ test
  (require rackunit))


;(interp-name n locs)->integer?
;n: name?
;locs: list? '((name? integer?) ...)
(define (interp-name n locs)
  (let ([x (assoc n locs)])
    (if x
        (second x)
        n)))

;
;(interp-triv t locs)->integer?
;t: triv?
;locs: list? '((name? integer?) ...)
(define (interp-triv t locs)
  (if (integer? t)
      t
      (interp-name t locs)))

;(interp-binop binop i1 i2)->integer?
;binop: symbol ('* of '+)
;i1, i2: integer?
(define (interp-binop binop i1 i2)
  (if (and (integer? i1) (integer? i2))
      (match binop
        ['+ (+ i1 i2)]
        ['* (* i1 i2)]
        [_ #f])
  `(,binop ,i1 ,i2)))

;(interp-relop relop i1 i2)->boolean?
;binop: symbol ('* of '+)
;i1, i2: integer?
(define (interp-relop relop i1 i2)
  (if (and (integer? i1) (integer? i2))
      (match relop
        ['=  (= i1 i2)]
        ['!= (not (= i1 i2))]
        ['<  (< i1 i2)]
        ['>  (> i1 i2)]
        ['<= (<= i1 i2)]
        ['>= (>= i1 i2)]
        [_ #f])
  `(,relop ,i1 ,i2)))

;
;(interp-let lets locs)->list? '((name? integer?) ...)
;lets: list? '((name? value) ...)
;locs: list? '((name? integer?) ...)
(define (interp-let lets locs)
  (foldr (lambda (l loc) (cons `(,(first l) ,(interp-value (second l) loc)) loc)) locs lets))

;(interp-if p r1 r2)->integer?/boolean?
;p: boolean?
;r1, r2: integer?/boolean?
;locs: list? '((name? integer?) ...)
(define (interp-if p r1 r2)
  (if p
      r1
      r2))
;
;(interp-value p locs)->boolean?
;p: pred?
;locs: list? '((name? integer?) ...)
(define (interp-pred p locs)
  (match p
    [`(let (,l ...) ,pred) (let ([newLoc (interp-let l locs)])
                             (interp-pred pred newLoc))]
    [`(if ,p1 ,p2 ,p3) (interp-if (interp-pred p1 locs) (interp-pred p2 locs) (interp-pred p3 locs))]
    [`(,relop ,t1 ,t2) (interp-relop relop (interp-triv t1 locs) (interp-triv t2 locs))]
    ['(true) true]
    ['(false) false]
    [`(not ,pred) (not (interp-pred pred locs))]
    [_ #f]))

;
;(interp-value v locs)->integer?
;v: value?
;locs: list? '((name? integer?) ...)
(define (interp-value v locs)
  (match v
    [`(let (,l ...) ,val) (let ([newLoc (interp-let l locs)])
                             (interp-value val newLoc))]
    [`(if ,p ,v1 ,v2) (interp-if (interp-pred p locs) (interp-value v1 locs) (interp-value v2 locs))]
    [`(,binop ,t1 ,t2) (interp-binop binop (interp-triv t1 locs) (interp-triv t2 locs))]
    [t (interp-triv t locs)]))


;
;(interp-tail t '())->integer?
;t: tail?
;locs: list? '((name? integer?) ...)
(define (interp-tail t locs)
  (match t
    [`(let (,l ...) ,tail) (let ([newLoc (interp-let l locs)])
                             (interp-tail tail newLoc))]
    [`(if ,p ,t1 ,t2) (interp-if (interp-pred p locs) (interp-tail t1 locs) (interp-tail t2 locs))]
    [v (interp-value v locs)]))

;(generate-overflow i)->integer?
;i: integer?
(define (generate-overflow i)
  (if (>= i 0)
      (remainder i 2147483648)
      (remainder i -2147483648)))

;
;(interp-values-lang p) →integer?
;p : Values-lang-V3?
(define (interp-values-lang p)
  (match p
    [`(module ,tail) (generate-overflow (interp-tail tail '()))]
    [_ #f]))

(module+ test
;interp-triv
  ;succes
  (check-equal? (interp-triv 5 '()) 5 "interp-triv: succes-01: int")
  (check-equal? (interp-triv 'x '((x 10) (y 15) (x 20))) 10 "interp-triv: succes-02: name")
  ;failure
  (check-equal? (interp-triv 'x '()) 'x "interp-triv: failure-01: name")
;interp-binop
  ;succes
  (check-equal? (interp-binop '* 5 9) 45 "interp-binop: succes-01: mul")
  (check-equal? (interp-binop '+ 5 9) 14 "interp-binop: succes-02: add")
  ;failure
  (check-equal? (interp-binop '+ 'x 9) '(+ x 9) "interp-binop: failure-01: name")
  (check-equal? (interp-binop '- 5 9) #f "interp-binop: failure-01: not binop")
;interp-relop
  ;succes
  (check-equal? (interp-relop '= 5 5) #t "interp-relop: succes-01: '=")
  (check-equal? (interp-relop '= 5 9) #f "interp-relop: succes-02: '=")

  (check-equal? (interp-relop '!= 5 9) #t "interp-relop: succes-03: '!=")
  (check-equal? (interp-relop '!= 5 5) #f "interp-relop: succes-04: '!=")

  (check-equal? (interp-relop '< 5 9) #t "interp-relop: succes-05: '<")
  (check-equal? (interp-relop '< 9 5) #f "interp-relop: succes-06: '<")

  (check-equal? (interp-relop '> 9 5) #t "interp-relop: succes-07: '>")
  (check-equal? (interp-relop '> 5 9) #f "interp-relop: succes-08: '>")

  (check-equal? (interp-relop '<= 5 9) #t "interp-relop: succes-09: '<=")
  (check-equal? (interp-relop '<= 9 5) #f "interp-relop: succes-10: '<=")
  (check-equal? (interp-relop '<= 5 5) #t "interp-relop: succes-11: '<=")

  (check-equal? (interp-relop '>= 9 5) #t "interp-relop: succes-12: '>=")
  (check-equal? (interp-relop '>= 5 9) #f "interp-relop: succes-13: '>=")
  (check-equal? (interp-relop '>= 5 5) #t "interp-relop: succes-14: '>=")
  ;failure
  (check-equal? (interp-relop '= 'x 9) '(= x 9) "interp-relop: failure-01: name")
  (check-equal? (interp-relop '+ '5 9) #f "interp-relop: failure-02: not relop")
;interp-pred
  ;succes
  (check-equal? (interp-pred '(= x 5) '([x 5])) #t "interp-pred: succes-01: relop")
  (check-equal? (interp-pred '(true) '()) #t "interp-pred: succes-02: true")
  (check-equal? (interp-pred '(false) '()) #f "interp-pred: succes-03: false")
  (check-equal? (interp-pred '(not (true)) '([x 5])) #f "interp-pred: succes-04: not")
  (check-equal? (interp-pred '(not (not (= x 5))) '([x 5])) #t "interp-pred: succes-05: not")

  (check-equal? (interp-pred '(let ([x 10] [y 15]) (= x y)) '([x 5])) #f "interp-pred: succes-06: let")
  
  (check-equal? (interp-pred '(let ([x 10] [y 15]) (if (= x y) (false) (true))) '([x 5])) #t "interp-pred: succes-07: if")
;interp-value
  ;succes
  (check-equal? (interp-value 'x '([x 5])) 5 "interp-value succes-01: triv")
  (check-equal? (interp-value '(+ x 5) '([x 5])) 10 "interp-value succes-02: binop")
  
  (check-equal? (interp-value '(let ([x 10] [y 15]) (+ x y)) '([x 5])) 25 "interp-value succes-03: let")
  
  (check-equal? (interp-value '(let ([x 10] [y 15]) (if (= x y) 6 7)) '([x 5])) 7 "interp-value succes-04: if")
;interp-tail
  ;succes
  (check-equal? (interp-tail 5 '([x 5])) 5 "interp-tail: succes-01: triv")
  (check-equal? (interp-tail '(+ 5 x) '([x 5])) 10 "interp-tail: succes-02: binop")
  
  (check-equal? (interp-tail '(let ([x 10] [y 15]) (+ x y)) '([x 5])) 25 "interp-tail: succes-03: let")
  
  (check-equal? (interp-tail '(let ([x 10] [y 15]) (if (= x y) 6 7)) '([x 5])) 7 "interp-tail: succes-04: if")
;interp-values-lang
  ;succes
  (check-equal? (interp-values-lang
                 '(module
                      (let ([y 1] [x 2])
                        (+ y x))))
                3
                "interp-values-lang: succes-1: simple program")
  (check-equal? (interp-values-lang
                 '(module
                      (let ([x (let ([y 1] [x 2])
                                 (+ y x))])
                        (let ([y (let ([x 3]) x)])
                          (+ x y)))))
                6
                "interp-values-lang: succes-2: complex program")
  ;failure
  #;(check-equal? (interp-values-lang
                 '(module x))
                'x
                "interp-values-lang: failure-1: name without value")
  #;(check-equal? (interp-values-lang
                 '(module (+ y 3)))
                '(+ y 3)
                "interp-values-lang: failure-2: not all names have a value in binop")
  #;(check-equal? (interp-values-lang
                 '(module (let ([x 2])
                        (+ y x))))
                '(+ y 2)
                "interp-values-lang: failure-3: not all names have a value in let"))
