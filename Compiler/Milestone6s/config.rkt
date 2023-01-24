#lang racket

(provide
 (all-defined-out))

(define verbose (make-parameter #f))
(define cc (make-parameter 'vanilla-riscv))
