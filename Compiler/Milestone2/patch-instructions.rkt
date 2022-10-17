#lang racket

(require "common.rkt")

(module+ test
  (require rackunit))

;
;
;
(define (patch-set r1 r2)
  (if (fvar? r2)
      `(`(set! t3 ,r2) `(set! ,r1 t3))
      `(set! ,r1 ,r2)))
;
;
;
(define (patch-halt h)
  `(set! 'a0 ,h))

;
;(patch-instructions p) → paren-x64-fvars-v2?
;p : para-asm-lang-v2?
(define (patch-instructions p)
  (match p
    [`(begin ,s ...) (map (lambda (set) (match set
                                      [`(set! ,a ,b) (patch-set a b)]
                                      [`(halt ,triv) (patch-halt triv)]
                                      [_ #f])) s)]
    [_ #f]))

(patch-instructions '(begin (set! a1 42) (halt a1)))
(patch-instructions
   '(begin
      (set! fv0 0)
      (set! fv1 42)
      (set! fv0 fv1)
      (halt fv0)))
(patch-instructions
   '(begin
      (set! t1 0)
      (set! t2 0)
      (set! r9 42)
      (set! t1 t2)
      (set! t1 (+ t1 t4))
      (halt t1)))

(module+ test
;patch-instructions
  ;succes
  (check-equal? (patch-instructions '(begin (set! a1 42) (halt a1)))
                '((set! a1 42) (set! 'a0 a1))
                "patch-instructions: succes-1: one instruction")
  (check-equal? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv1 42)
                    (set! fv0 fv1)
                    (halt fv0)))
                '((set! fv0 0) (set! fv1 42) `(set! t3 ,r2) `(set! ,r1 t3) (set! 'a0 fv0))
                "patch-instructions: succes-2: a fvar in second argument")
  (check-equal? (patch-instructions
                 '(begin
                    (set! t1 0)
                    (set! t2 0)
                    (set! t4 42)
                    (set! t1 t2)
                    (set! t1 (+ t1 t4))
                    (halt t1)))
                '((set! t1 0) (set! t2 0) (set! t4 42) (set! t1 t2) (set! t1 (+ t1 t4)) (set! 'a0 t1))
                "patch-instructions: succes-3: multiple instructions"))