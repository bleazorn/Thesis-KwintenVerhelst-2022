#lang racket

(provide generate-values-lang)

;Get all different members in list and sort them
;(getDiff l)->list? '((any? integer?) ...)
;l: list?
(define (getDiff l)
  (sort (foldl (lambda (e n) (if (assoc e n)
                                 n
                                 (cons `(,e 0) n))) '() l) #:key car <))
;build a list of n random numbers
;(getRandoms n max)->list?
;n, max: integer?
(define (getRandoms n max)
  (build-list n (lambda (x) (random max))))

;Get the distribution of a list
;(dist lis)->list? '((any? integer?) ...)
;lis: list?
(define (dist lis)
  (foldl (lambda (e n) (let* ([a (assoc e n)]
                              [i (index-of n a)])
                         (list-set n i `(,(car a) ,(add1 (second a)))))) (getDiff lis) lis))

;(dist (getRandoms 100000 10))

(define integerBit 9)

(define n 0)

(define (newName)
  (set! step (add1 step))
  (set! n (add1 n))
  (format "x~a" n))

(define maxSteps 500)

(define step 0)

(define (resetGen)
  (set! n 0)
  (set! step 0))

(define (getRandom num axi)
  (set! step (add1 step))
  (if (< step maxSteps)
      (random num)
      (random axi)))


;
;(generate-names names)->name?
;names->list? '(name? ...)
(define (generate-names names)
  (if (null? names)
      (generate-integer)
      (let ([i (random (length names))])
        (list-ref names i))))

;
;(generate-integer)->integer?
(define (generate-integer)
  (match (random 2)
    [0 (random (expt 2 integerBit))]
    [1 (random (expt -2 integerBit) 0)]))

;
;(generate-triv names)->triv?
;names->list? '(name? ...)
(define (generate-triv names)
  (match (getRandom 5 2)
    [0 (generate-integer)]
    [1 (generate-names names)]
    [_ (generate-names names)]))

;
;(generate-relop)->relop?
(define (generate-relop)
  (match (getRandom 6 6)
    [0 '=]
    [1 '!=]
    [2 '<]
    [3 '>]
    [4 '<=]
    [5 '>=]))

;
;(generate-binop)->binop?
(define (generate-binop)
  (match (getRandom 2 2)
    [0 '+]
    [1 '*]))

;
;(generate-let)->list? list? '(name? ...) '((name? value?) ...)
;names->list? '(name? ...)
(define (generate-let names)
  (let ([newNames (build-list (add1 (getRandom 5 1)) (lambda (x) (newName)))])
    (values newNames (map (lambda (a) `[,a ,(generate-value names)]) newNames))))

;
;(generate-pred names)->pred?
;names->list? '(name? ...)
(define (generate-pred names)
  (match (getRandom 6 3)
    [0 `(,(generate-relop) ,(generate-triv names) ,(generate-triv names))]
    [1 '(true)]
    [2 '(false)]
    [3 `(not ,(generate-pred names))]
    [4 (let-values ([(newNames letName) (generate-let names)])
         `(let ,letName ,(generate-pred newNames)))]
    [5 `(if ,(generate-pred names) ,(generate-pred names) ,(generate-pred names))]
    [_ `(,(generate-relop) ,(generate-triv names) ,(generate-triv names))]))

;
;(generate-value names)->value?
;names->list? '(name? ...)
(define (generate-value names)
  (match (getRandom 4 2)
    [0 (generate-triv names)]
    [1 `(,(generate-binop) ,(generate-triv names) ,(generate-triv names))]
    [2 (let-values ([(newNames letName) (generate-let names)])
         `(let ,letName ,(generate-value newNames)))]
    [3 `(if ,(generate-pred names) ,(generate-value names) ,(generate-value names))]
    [_ (generate-triv names)]))

;
;(generate-tail names)->tail?
;names->list? '(name? ...)
(define (generate-tail names)
  (match (getRandom 3 1)
    [0 (generate-value names)]
    [1 (let-values ([(newNames letName) (generate-let names)])
         `(let ,letName ,(generate-tail newNames)))]
    [2 `(if ,(generate-pred names) ,(generate-tail names) ,(generate-tail names))]
    [_ (generate-value names)]))

;
;(generate-values-lang)->Values-lang
(define (generate-values-lang)
  (resetGen)
  `(module ,(generate-tail '())))

;(generate-values-lang)









