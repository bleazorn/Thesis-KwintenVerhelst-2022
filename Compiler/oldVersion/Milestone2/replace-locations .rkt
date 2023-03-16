#lang racket

(module+ test
  (require rackunit))


(define replace-locations 
  #t)

(module+ test
  (check-equal? #t #t "first test"))