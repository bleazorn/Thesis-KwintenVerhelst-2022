#lang racket

(define (tes1 symbol)
  (match (string->list (symbol->string symbol))
    [`(#\a ,i ...) #:when (foldl (lambda (i b) (and (char-numeric? i) b)) #t i) i]
    [_ #f]))

(define (isRegister? res)
  (let* ([s (symbol->string res)]
         [l (substring s 0 1)]
         [n (substring s 1)])
    (match `(,l ,n)
      [`("x" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 32))) res]
      [_ #f])))

(isRegister? 'x31)