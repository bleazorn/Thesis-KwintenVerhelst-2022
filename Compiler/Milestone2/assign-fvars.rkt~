#lang racket

(require "common.rkt")
(provide assign-fvars)

(module+ test
  (require rackunit))


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
  ;succes
  (check-equal?
   (assign-fvars
    '(module
         ((locals (x.1)))
       (begin
         (set! x.1 0)
         (halt x.1))))
   '(module
        ((locals (x.1))
         (assignment ((x.1 fv0))))
      (begin (set! x.1 0) (halt x.1)))
   "assign-fvars: succes-1: one location")
  (resetfvar)
  (check-equal?
   (assign-fvars
    '(module
         ((locals (x.1 y.1 w.1)))
       (begin
         (set! x.1 0)
         (set! y.1 x.1)
         (set! w.1 (+ w.1 y.1))
         (halt w.1))))
   '(module
        ((locals (x.1 y.1 w.1)) (assignment ((x.1 fv0) (y.1 fv1) (w.1 fv2))))
      (begin (set! x.1 0) (set! y.1 x.1) (set! w.1 (+ w.1 y.1)) (halt w.1)))
   "assign-fvars: succes-1: multiple locations")
  )