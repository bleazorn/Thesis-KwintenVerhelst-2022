#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
         "220-check-values-lang.rkt"
         "210-uniquify.rkt"
         "205-remove-complex-opera.rkt"
         "200-sequentialize-let.rkt"
         "190-normalize-bind.rkt"
         "180-impose-calling-conventions-full.rkt"
         "180-impose-calling-conventions-half.rkt"
         "175-add-saved-registers-full.rkt"
         "175-add-saved-registers-half.rkt"
         "175-add-saved-registers-stkTokens.rkt"
         "175-add-saved-registers-cheri-linkage.rkt"
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
         "095-secure-stktokens.rkt"
         "095-secure-cheri-linkage.rkt"
         "094-clean-registers.rkt"
         "093-add-stktokens-sentry.rkt"
         "093-add-stktokens-seal.rkt"
         "092-change-return-seal.rkt"
         "091-create-got.rkt"
         "090-replace-call-got-sentry.rkt"
         "090-replace-call-got-seal.rkt"
         "090-change-frame-pointer.rkt"
         "080-implement-fvars.rkt"
         "080-implement-fvars-split.rkt"
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
         "010-wrap-cheri-risc-v-run-time-stkTokens.rkt"
         "010-wrap-cheri-risc-v-run-time-stkTokens-sentry.rkt"
         "010-wrap-cheri-risc-v-run-time-cheri-linkage-seal.rkt"
         "010-wrap-cheri-risc-v-run-time-cheri-linkage-trampoline.rkt"
         "010-wrap-risc-v-run-time.rkt"
         "000-wrap-cheri-risc-v-boilerplate.rkt"
         "000-wrap-risc-v-boilerplate.rkt"
         "interp-values-lang.rkt")

(provide steps
         stkTokens
         stkTokens-sentry
         cheri-linkage-seal
         cheri-linkage-trampoline
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
         add-saved-registers-full
         impose-calling-conventions-full
         normalize-bind
         sequentialize-let
         remove-complex-opera
         uniquify
         check-values-lang)))

;Stktokens
(define (stkTokens l)
  (let* ([wI (index-where l (curry equal? wrap-cheri-risc-v-run-time))]
         [gI (index-where l (curry equal? access-memory-tempory-register))]
         [fI (index-where l (curry equal? implement-fvars))]
         [sI (index-where l (curry equal? change-frame-pointer))]
         [cI (index-where l (curry equal? add-saved-registers-full))]
         [iI (index-where l (curry equal? impose-calling-conventions-full))]
         [switchedL (list-set
                     (list-set
                      (list-set
                       (list-set
                        (list-set l wI wrap-cheri-risc-v-run-time-stkTokens)
                        gI access-memory-sub-add-frame-register)
                       fI implement-fvars-split)
                      cI add-saved-registers-stkTokens)
                     sI (list replace-call-got-seal
                              create-got
                              change-return-seal
                              clean-registers
                              secure-stktokens
                             ))])
    (flatten switchedL)))

(define (stkTokens-sentry l)
  (let* ([wI (index-where l (curry equal? wrap-cheri-risc-v-run-time))]
         [gI (index-where l (curry equal? access-memory-tempory-register))]
         [fI (index-where l (curry equal? implement-fvars))]
         [sI (index-where l (curry equal? change-frame-pointer))]
         [cI (index-where l (curry equal? add-saved-registers-full))]
         [iI (index-where l (curry equal? impose-calling-conventions-full))]
         [switchedL (list-set
                     (list-set
                      (list-set
                       (list-set
                        (list-set l wI wrap-cheri-risc-v-run-time-stkTokens-sentry)
                        gI access-memory-sub-add-frame-register)
                       fI implement-fvars-split)
                      cI add-saved-registers-stkTokens)
                     sI (list replace-call-got-sentry
                              create-got
                              add-stktokens-sentry
                              change-return-seal
                              clean-registers
                              secure-stktokens
                             ))])
    (flatten switchedL)))

;cheri-linkage
(define (cheri-linkage-seal l)
  (let* ([wI (index-where l (curry equal? wrap-cheri-risc-v-run-time))]
         [sI (index-where l (curry equal? change-frame-pointer))]
         [cI (index-where l (curry equal? add-saved-registers-full))]
         [iI (index-where l (curry equal? impose-calling-conventions-full))]
         [switchedL (list-set
                     (list-set
                      (list-set l wI wrap-cheri-risc-v-run-time-cheri-linkage-seal)
                      cI add-saved-registers-cheri-linkage)
                      sI (list replace-call-got-sentry
                               create-got
                               change-return-seal
                               clean-registers
                               secure-cheri-linkage
                               ))])
    (flatten switchedL)))

(define (cheri-linkage-trampoline l)
  (let* ([wI (index-where l (curry equal? wrap-cheri-risc-v-run-time))]
         [sI (index-where l (curry equal? change-frame-pointer))]
         [cI (index-where l (curry equal? add-saved-registers-full))]
         [switchedL (list-set
                     (list-set
                      (list-set l wI wrap-cheri-risc-v-run-time-cheri-linkage-trampoline)
                      cI add-saved-registers-cheri-linkage)
                     sI (list replace-call-got-sentry
                              create-got
                              clean-registers
                              secure-cheri-linkage
                              ))])
         (flatten switchedL)))

;half stack
(define (halfStack l)
  (let* ([cI (index-where l (curry equal? add-saved-registers-full))]
         [iI (index-where l (curry equal? impose-calling-conventions-full))]
         [aI (index-where l (curry equal? allocate-frames-full))]
         [switchedL (list-set
                     (list-set l cI add-saved-registers-half)
                     iI impose-calling-conventions-half)]
         [tL (take switchedL aI)]
         [dL (drop switchedL (+ aI 3))])
    (append tL
            (list assign-frame-variables-half
                  assign-registers-half
                  allocate-frames-half
                  assign-call-undead-variables-half)
            dL)))

;risc-v
(define (risc-v l)
  (let* ([cI (index-where l (curry equal? cap-mode-on))]
         [dL (drop l (+ cI 1))])
    (append (list wrap-risc-v-boilerplate
                  wrap-risc-v-run-time
                  generate-risc-v
                  cap-mode-off)
            dL)))
