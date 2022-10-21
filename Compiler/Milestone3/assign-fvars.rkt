#lang racket

(require "common.rkt")
(provide assign-fvars)

(module+ test
  (require rackunit))

(define l -1)

(define (resetLocal)
  (set! l -1))

(define (getLocal)
  (set! l (add1 l))
  l)

;use t0, t1, t2, t3 dan pas memory
(define (freshLocal)
  (if (< l 3)
      (string->symbol (format "t~a" (getLocal)))
      (freshfvar)))
      
;
;(assign-locals locals)->assignment
;locals: locals
(define (assign-locals locals)
  `(assignment ,(map (lambda (l) (list l (freshfvar))) locals)))

;Compiles Asm-lang-V2-locals to Asm-lang-V2-assignments, by assigning each abstract location from the locals info field to a fresh frame variable.
;(assign-fvars p) → Asm-lang-V2-assignments?
;p: Asm-lang-V2-locals?
(define (assign-fvars p)
  (match p
    [`(module () ,program) p]
    [`(module ((locals (,l ...))) ,program) `(module ((locals ,l) ,(assign-locals l)) ,program)]
    [_ #f]))

(module+ test
;assign-fvars
  (define (check-assign t1 t2 text)
    (resetLocal)
    (resetfvar)
    (check-equal? t1 t2 text))
  ;succes
  (check-assign
   (assign-fvars
    '(module
         ((locals (x.1)))
       (begin
         (set! x.1 0)
         (halt x.1))))
   '(module
        ((locals (x.1))
         (assignment ((x.1 t0))))
      (begin (set! x.1 0) (halt x.1)))
   "assign-fvars: succes-1: one location")
  (check-assign
   (assign-fvars
    '(module
         ((locals (x.1 y.1 w.1)))
       (begin
         (set! x.1 0)
         (set! y.1 x.1)
         (set! w.1 (+ w.1 y.1))
         (halt w.1))))
   '(module
        ((locals (x.1 y.1 w.1)) (assignment ((x.1 t0) (y.1 t1) (w.1 t2))))
      (begin (set! x.1 0) (set! y.1 x.1) (set! w.1 (+ w.1 y.1)) (halt w.1)))
   "assign-fvars: succes-1: multiple locations still in register")
  (check-assign
   (assign-fvars
    '(module
         ((locals (x.1 y.1 w.1 a.1 b.1)))
       (begin
         (set! x.1 0)
         (set! y.1 x.1)
         (set! w.1 (+ w.1 y.1))
         (set! a.1 1)
         (set! b.1 2)
         (halt w.1))))
   '(module
        ((locals (x.1 y.1 w.1 a.1 b.1)) (assignment ((x.1 t0) (y.1 t1) (w.1 t2) (a.1 t3) (b.1 fv0))))
      (begin (set! x.1 0) (set! y.1 x.1) (set! w.1 (+ w.1 y.1)) (set! a.1 1) (set! b.1 2) (halt w.1)))
   "assign-fvars: succes-1: multiple locations one in memory")
  )