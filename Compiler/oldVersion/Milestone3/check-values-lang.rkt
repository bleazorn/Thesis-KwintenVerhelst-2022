#lang racket

(module+ test
  (require rackunit))


;Takes an arbitrary value and either returns it, if it is a valid Values-lang v3 program, or raises an error with a descriptive error message.
;(check-values-lang p) → Values-lang-V3?
;p : any/c
(define (check-values-lang p)
  #t)

(module+ test
;check-values-lang
  ;succes
  (check-equal? (check-values-lang
                 '(module
                      (let ([x 5]
                            [y 6])
                        x)))
                '(module (let ((x 5) (y 6)) x))
                "check-values-lang: succes-1: one let")
  (check-equal? (check-values-lang
                 '(module
                      (let ([x 5]
                            [y 6])
                        (let ([y x])
                          y))))
                '(module (let ((x 5) (y 6)) (let ((y x)) y)))
                "check-values-lang: succes-2: two lets")
  (check-equal? (check-values-lang
                 '(module (let () 5)))
                '(module (let () 5))
                "check-values-lang: succes-3: empty let")
  ;failure
  (check-equal? (check-values-lang
                 '(module
                      (let ([x 5]
                            [y x])
                        y)))
                error
                "check-values-lang: failure-1: no bound 'x in enviroment yet")
  (check-equal? (check-values-lang
                 '(module
                      (let ([x 5]
                            [x 6])
                        x)))
                error
                "check-values-lang: failure-2: bound the same name twice")
  (check-equal? (check-values-lang
                 '(module (let () x)))
                error
                "check-values-lang: failure-3: no bound name in enviroment"))