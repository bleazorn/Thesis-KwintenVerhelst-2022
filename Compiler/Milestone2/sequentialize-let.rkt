#lang racket

(module+ test
  (require rackunit))


(define sequentialize-let
  #t)

(module+ test
  (check-equal? #t #t "first test"))