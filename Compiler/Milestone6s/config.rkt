#lang racket

(provide
 (all-defined-out))

(define verbose (make-parameter #f))
(define output-file (make-parameter #f))
(define cc (make-parameter 'vanilla-riscv))
(define pass (make-parameter #f))