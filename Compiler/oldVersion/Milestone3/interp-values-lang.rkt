#lang racket

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

;(interp-binop binop i1 i2)->integer?
;binop: symbol ('* of '+)
;i1: integer?
;i2: integer?
(define (interp-binop binop i1 i2)
  (if (and (integer? i1) (integer? i2))
      (match binop
        ['+ (+ i1 i2)]
        ['* (* i1 i2)]
        [_ #f])
  `(,binop ,i1 ,i2)))

;
;lets: list? '((name? value) ...)
;locs: list? '((name? integer?) ...)
(define (interp-let lets locs)
  (foldr (lambda (l loc) (append loc `((,(first l) ,(interp-value (second l) loc))))) locs lets))

;v: value
;locs: list? '((name? integer?) ...)
(define (interp-value v locs)
  (match v
    [v #:when (integer? v) v]
    [v #:when (symbol? v) (interp-name v locs)]
    [`(let (,l ...) ,v) (interp-value v (interp-let l locs))]
    [`(,binop ,a ,b) (interp-binop binop (interp-value a locs) (interp-value b locs))]
    [_ v]))

;
;(interp-values-lang p) → integer?
;p : Values-lang-V3?
(define (interp-values-lang p)
  (match p
    [`(module ,tail) (interp-value tail '())]
    [_ #f]))

(module+ test
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
  (check-equal? (interp-values-lang
                 '(module x))
                'x
                "interp-values-lang: failure-1: name without value")
  (check-equal? (interp-values-lang
                 '(module (+ y 3)))
                '(+ y 3)
                "interp-values-lang: failure-2: not all names have a value in binop")
  (check-equal? (interp-values-lang
                 '(module (let ([x 2])
                        (+ y x))))
                '(+ y 2)
                "interp-values-lang: failure-3: not all names have a value in let"))
