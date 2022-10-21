#lang racket

(module+ test
  (require rackunit))


(define i 0)

(define (freshtmp)
  (set! i (add1 i))
  (string->symbol (string-append "tmp." (~a i))))

(define (fresh v)
  (set! i (add1 i))
  (if (symbol? v)
      (string->symbol (string-append (symbol->string v) "." (~a i)))
      #f))

(define (name? v)
  (if (symbol? v)
      (let* ([s (symbol->string v)]
             [l (string->list s)]
             [j (index-of l #\.)])
        (if j
            (let* ([n (substring s (add1 j))])
                   (if (string->number n)
                       #t
                       #f))
            #f))
      #f))



(define (uniquifyChangeEleNotLet ele locs)
  (let* ([l (assoc ele locs)])
    (if l
        (list-ref l 1)
        ele)))
  
(define (uniquifyChangeEle ele locs)
  (match ele
    [`(let ,a ,b) (uniquifyLet `(let ,a ,b) locs)]
    [b (uniquifyChangeEleNotLet b locs)]))

(define (uniquifyChangeBody body locs)
  (if (list? body)
      (map (lambda (ele) (uniquifyChangeEle ele locs)) body)
      (uniquifyChangeEleNotLet body locs)))

(define (uniquifyAbstract let body locs)
  (let* ([x (first (first let))]
         [newX (fresh x)]
         [newLocs (append `((,x ,newX)) locs)])
    (append `(((,newX ,(second (first let))))) (uniquifyLet body newLocs))))
  
(define (uniquifyLet m locs)
  (match m
    [`(let ,a ,b) (append '(let) (uniquifyAbstract a b locs))]
    [b (uniquifyChangeBody b locs)]))

(define (uniquify m)
  (match m
    [`(module ,a) `(module ,(uniquifyLet a '()))]))

(uniquify '(module (+ 2 2)))

(uniquify '(module (let ([x (+ 2 2)]) x)))

(uniquify '(module (let ([x 5]) x)))

(uniquify '(module (let ([x 2]) (let ([x 2]) (+ x x)))))

        

(module+ test
  (check-equal? #t #t "first test"))
