#lang racket

(provide interp-values-lang)

(module+ test
  (require rackunit))

(define maxsteps 100000)
(define step 0)
(define (stepping)
  (set! step (add1 step)))
(define (resetI)
  (set! step 0))

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
        ['- (- i1 i2)]
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
(define (interp-let lets locs funcs)
  (foldr (lambda (l loc) (cons `(,(first l) ,(interp-value (second l) loc funcs)) loc)) locs lets))

;
;(interp-value p locs)->boolean?
;p: pred?
;locs: list? '((name? integer?) ...)
(define (interp-pred p locs funcs)
  (match p
    [`(let (,l ...) ,pred) (let ([newLoc (interp-let l locs funcs)])
                             (interp-pred pred newLoc funcs))]
    [`(if ,p1 ,p2 ,p3) (if (interp-pred p1 locs funcs)
                           (interp-pred p2 locs funcs)
                           (interp-pred p3 locs funcs))]
    [`(,relop ,t1 ,t2) (interp-relop relop (interp-triv t1 locs) (interp-triv t2 locs))]
    ['(true) true]
    ['(false) false]
    [`(not ,pred) (not (interp-pred pred locs funcs))]
    [_ #f]))

;
;(interp-value v locs)->integer?
;v: value?
;locs: list? '((name? integer?) ...)
(define (interp-value v locs funcs)
  (if (< step maxsteps)
      (match v
        [`(let (,l ...) ,val) (let ([newLoc (interp-let l locs funcs)])
                                (interp-value val newLoc funcs))]
        [`(if ,p ,v1 ,v2) (if (interp-pred p locs funcs)
                              (interp-value v1 locs funcs)
                              (interp-value v2 locs funcs))]
        [`(call ,x ,trivs ...) (stepping) (interp-func x trivs locs funcs)]
        [`(,binop ,t1 ,t2) (interp-binop binop (interp-triv t1 locs) (interp-triv t2 locs))]
        [t (interp-triv t locs)])
      #f))


;
;(interp-tail t '())->integer?
;t: tail?
;locs: list? '((name? integer?) ...)
(define (interp-tail t locs funcs)
  (if (< step maxsteps)
      (match t
        [`(let (,l ...) ,tail) (let ([newLoc (interp-let l locs funcs)])
                                 (interp-tail tail newLoc funcs))]
        [`(if ,p ,t1 ,t2) (if (interp-pred p locs funcs)
                              (interp-tail t1 locs funcs)
                              (interp-tail t2 locs funcs))]
        [`(call ,x ,trivs ...)  (stepping) (interp-func x trivs locs funcs)]
        [v (interp-value v locs funcs)])
      #f))

;
;(interp-func x t locs funcs)->integer?
;x: label?
;t: list? '(triv ...)
;locs: list? '((name? integer?) ...)
;funcs: list? '((define name? (lambda (name? ...) tail?)) ...)
(define (interp-func x t locs funcs)
  (let ([f (findf (lambda (fun) (equal? x (second fun))) funcs)])
    (match f
      [`(define ,l (lambda ,args ,tailFunc)) (let ([newLocs (apply map list (list args (map (lambda (triv) (interp-triv triv locs)) t)))]) ;zips 2 list together
                                               (interp-tail tailFunc newLocs funcs))]
      [_ #f])))
  
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
  (resetI)
  (match p
    [`(module ,f ... ,tail) (let ([in (interp-tail tail '() f)])
                              (if (integer? in)
                                  (generate-overflow in)
                                  "Time error"))]
    [_ #f]))


(module+ test
  ;#|
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
  (check-equal? (interp-binop '- 5 9) -4 "interp-binop: succes-03: sub")
  ;failure
  (check-equal? (interp-binop '+ 'x 9) '(+ x 9) "interp-binop: failure-01: name")
  
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
  (check-equal? (interp-pred '(= x 5) '([x 5]) '()) #t "interp-pred: succes-01: relop")
  (check-equal? (interp-pred '(true) '() '()) #t "interp-pred: succes-02: true")
  (check-equal? (interp-pred '(false) '() '()) #f "interp-pred: succes-03: false")
  (check-equal? (interp-pred '(not (true)) '([x 5]) '()) #f "interp-pred: succes-04: not")
  (check-equal? (interp-pred '(not (not (= x 5))) '([x 5]) '()) #t "interp-pred: succes-05: not")

  (check-equal? (interp-pred '(let ([x 10] [y 15]) (= x y)) '([x 5]) '()) #f "interp-pred: succes-06: let")
  
  (check-equal? (interp-pred '(let ([x 10] [y 15]) (if (= x y) (false) (true))) '([x 5]) '()) #t "interp-pred: succes-07: if")
;interp-value
  ;succes
  (check-equal? (interp-value 'x '([x 5]) '()) 5 "interp-value succes-01: triv")
  (check-equal? (interp-value '(+ x 5) '([x 5]) '()) 10 "interp-value succes-02: binop")
  
  (check-equal? (interp-value '(let ([x 10] [y 15]) (+ x y)) '([x 5]) '()) 25 "interp-value succes-03: let")
  
  (check-equal? (interp-value '(let ([x 10] [y 15]) (if (= x y) 6 7)) '([x 5]) '()) 7 "interp-value succes-04: if")
;interp-tail
  ;succes
  (check-equal? (interp-tail 5 '([x 5]) '()) 5 "interp-tail: succes-01: triv")
  (check-equal? (interp-tail '(+ 5 x) '([x 5]) '()) 10 "interp-tail: succes-02: binop")
  
  (check-equal? (interp-tail '(let ([x 10] [y 15]) (+ x y)) '([x 5]) '()) 25 "interp-tail: succes-03: let")
  
  (check-equal? (interp-tail '(let ([x 10] [y 15]) (if (= x y) 6 7)) '([x 5]) '()) 7 "interp-tail: succes-04: if")
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
  (check-equal? (interp-values-lang
                 '(module
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
                0
                "interp-values-lang: succes-03: tail call")
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
                "interp-values-lang: failure-3: not all names have a value in let")
;|#
)
