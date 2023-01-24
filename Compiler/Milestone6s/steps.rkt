#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
         "220-check-values-lang.rkt"
         "210-uniquify.rkt"
         "200-sequentialize-let.rkt"
         "190-normalize-bind.rkt"
         "180-impose-calling-conventions-full.rkt"
         "180-impose-calling-conventions-half.rkt"
         "175-call-convention-secure.rkt"
         "170-select-instructions.rkt"
         "160-uncover-locals.rkt"
         "150-undead-analysis.rkt"
         "140-conflict-analysis.rkt"
         "120-3-assign-registers-full.rkt"
         "120-2-assign-call-undead-variables-full.rkt"
         "120-1-allocate-frames-full.rkt"
         "121-4-assign-call-undead-variables-half.rkt"
         "121-3-allocate-frames-half.rkt"
         "121-2-assign-registers-half.rkt"
         "121-1-assign-frame-variables-half.rkt"
         "100-replace-locations.rkt"
         "090-change-frame-pointer.rkt"
         "090-secure-stack-tokens.rkt"
         "080-implement-fvars.rkt"     
         "070-expose-basic-blocks.rkt"
         "060-resolve-predicates.rkt"
         "050-flatten-program.rkt"
         "040-patch-instructions.rkt"
         "030-cap-mode-on.rkt"
         "030-cap-mode-off.rkt"
         "025-access-memory-sub-add-frame-register.rkt"
         "025-access-memory-tempory-register.rkt"
         "020-generate-cheri-risc-v.rkt"
         "020-generate-risc-v.rkt"
         "010-wrap-cheri-risc-v-run-time.rkt"
         "010-wrap-cheri-risc-v-run-time-secure.rkt"
         "010-wrap-risc-v-run-time.rkt"
         "000-wrap-cheri-risc-v-boilerplate.rkt"
         "000-wrap-risc-v-boilerplate.rkt"
         "interp-values-lang.rkt")

(provide steps
         stkTokens
         halfStack
         risc-v)

(define steps
  (make-parameter
   (list wrap-cheri-risc-v-boilerplate
         wrap-cheri-risc-v-run-time
         generate-cheri-risc-v
         access-memory-tempory-register
         cap-mode-on
         patch-instructions
         flatten-program  
         resolve-predicates
         expose-basic-blocks
         implement-fvars
         change-frame-pointer
         replace-locations
         allocate-frames-full
         assign-call-undead-variables-full
         assign-registers-full
         conflict-analysis
         undead-analysis
         uncover-locals
         select-instructions
         impose-calling-conventions-full
         normalize-bind
         sequentialize-let
         uniquify
         check-values-lang)))

;Stktokens
(define (stkTokens l)
  (let ([wI (index-where l (lambda (a) (equal? wrap-cheri-risc-v-run-time a)))]
        [gI (index-where l (lambda (a) (equal? access-memory-tempory-register a)))]
        [sI (index-where l (lambda (a) (equal? change-frame-pointer a)))]
        [iI (index-where l (lambda (a) (equal? impose-calling-conventions-full a)))])
    (let ([switchedL (list-set
                      (list-set
                       (list-set l wI wrap-cheri-risc-v-run-time-secure)
                       gI access-memory-sub-add-frame-register)
                      sI secure-stack-tokens)])
      (let ([tL (take switchedL iI)]
            [dL (drop switchedL iI)])
        (append tL
                (list call-convention-secure)
                dL)))))

;half stack
(define (halfStack l)
  (let ([iI (index-where l (lambda (a) (equal? impose-calling-conventions-full a)))])
    (let ([l (list-set l iI impose-calling-conventions-half)])
      (let ([aI (index-where l (lambda (a) (equal? allocate-frames-full a)))])
        (let ([tL (take l aI)]
              [dL (drop l (+ aI 3))])
          (append tL
                  (list assign-frame-variables-half
                        assign-registers-half
                        allocate-frames-half
                        assign-call-undead-variables-half)
                  dL))))))

;risc-v
(define (risc-v l)
  (let ([cI (index-where l (lambda (a) (equal? cap-mode-on a)))])
    (let ([dL (drop l (+ cI 1))])
      (append (list wrap-risc-v-boilerplate
                    wrap-risc-v-run-time
                    generate-risc-v
                    cap-mode-off)
              dL))))