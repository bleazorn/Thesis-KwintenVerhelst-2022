#lang racket

(require "replace-locations.rkt"
         "assign-fvars.rkt"
         "uncover-locals.rkt")
(provide assign-homes)
         

(module+ test
  (require rackunit))

;Compiles Asm-lang-V2 to Asm-lang-V2-nested, replacing each abstract location with a physical location.
;(assign-homes p) → Asm-lang-V2-nested?
;p: Asm-lang-V2?
(define (assign-homes p)
  (replace-locations (assign-fvars (uncover-locals p))))

(module+ test
  (check-equal? (assign-homes 'a) #t "first test"))