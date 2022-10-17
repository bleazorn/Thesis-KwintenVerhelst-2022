#lang racket

(require "uniquify.rkt"
         "sequentialize-let.rkt"
         "normalize-bind.rkt"
         "select-instructions.rkt"
         "replace-locations.rkt"
         "assign-fvars.rkt"
         "uncover-locals.rkt"
         "flatten-begins.rkt"
         "patch-instructions.rkt"
         "implement-fvars.rkt")

(define steps
  (list implement-fvars
        patch-instructions
        flatten-begins
        replace-locations
        assign-fvars
        uncover-locals
        select-instructions
        normalize-bind
        sequentialize-let
        uniquify))

 (define (createList i j)
   (if (>= i j)
       `(,j)
       (cons i (createList (add1 i) j))))
  

(define (test program)
  (for/fold ([p program])
            ([i (reverse (createList 0 9))])
    (values (let* ([fun (list-ref steps i)]
                  [res (fun p)])
             (display (format "~a:  ~a\n" fun res))
             res))))

(define (apply-function-list element)
  (map (lambda (f)
         (f element))
       steps))


(test '(module (+ 2 2)))