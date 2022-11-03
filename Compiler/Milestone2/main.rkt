#lang racket

(require "compile.rkt")

(module+ test
  (require rackunit))

(apply compile-file (vector->list (current-command-line-arguments)))

(module+ test
  (check-equal? #t #t "test"))
