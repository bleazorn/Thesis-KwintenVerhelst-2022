#lang racket

(module+ test
  (require rackunit))

;Optimizes let bindings by reordering them to minimize or maximize some metric.
;(optimize-let-bindings p) → Values-lang-V3-unique?
;p: Values-lang-V3-unique?
(define (optimize-let-bindings p)
  #t)

(module+ test
  (check-equal? #t #t "first test"))
