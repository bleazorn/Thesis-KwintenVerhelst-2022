#lang racket

(require "config.rkt")

(provide
 (all-defined-out))

(define (logln . args)
  (when (verbose)
    (apply println args)))

(define (pretty-log . args)
  (when (verbose)
    (apply pretty-display args)))
