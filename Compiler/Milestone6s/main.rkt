#lang racket

(require "compile.rkt")

(module+ test
  (require rackunit))

(when (not (vector-empty? (current-command-line-arguments)))
    (apply compile-file (vector->list (current-command-line-arguments))))

(module+ test
  (check-equal? #t #t "test"))
