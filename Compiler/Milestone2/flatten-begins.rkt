#lang racket

(module+ test
  (require rackunit))

;
;(flatten-begins p) → para-asm-lang-v2
;p : nested-asm-lang-v2
(define (flatten-begins p)
    (match p
      [`(begin ,s ...) (map (lambda (set) (match set
                                            [`(begin ,e ...) (flatten-begins `(begin ,e ...))]
                                            [`(set! ,a ,b) (patch-set a b)]
                                            [`(halt ,triv) (patch-halt triv)]
                                            [_ #f])) s)]
      [_ #f])))

(module+ test
  (check-equal? #t #t "first test"))