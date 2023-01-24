#lang racket

(require "config.rkt")

(provide
 (all-defined-out))

(define (logln . args)
  (when (verbose)
    (apply println args)))
