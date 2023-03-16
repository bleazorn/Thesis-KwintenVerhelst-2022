#lang racket

(require "common/assembly.rkt"
         "common/register.rkt"
         "common/fvar.rkt"
         "setup.rkt")

(provide generate-perm
         generate-bound
         generate-seal
         generate-unseal
         generate-sentry)

(define permissions
  '(global
    execute
    load
    store
    storeCap
    loadCap
    storeLoc
    seal
    invole
    unseal
    ASR
    cid))

(define (permissionsToInt p)
  (let ([i (index-of permissions p)])
    (if i
        (expt 2 i)
        0)))


(define (setPerms r ps)
  `(perm ,r ,ps))

(define (removePerm p rp)
  (match p
    [`(perm ,r ,ps) #:when (and (register? r) (list? ps) (member rp ps)) `(perm ,r ,(remove rp ps))]
    [_ #f]))


(define (generate-perm p)
  (match p
    [`(perm ,r ,ps) #:when (and (register? r) (list? ps)) (string-append (indent-instr (format "li t5, ~a" (foldl + (map permissionsToInt ps))))
                                                                         (indent-instr (format "CAndPerm ~a, ~a, t5" r r)))]
    [_ #f]))


(define (generate-bound b)
  (match b
    [`(bound ,r ,bas ,len) #:when (and (isCapability? r) (isCapability? bas) (or (register? len) (isNonCapRegister? len)))
                           (string-append (indent-instr (format "addi t5, ~a, 0" r))
                                          (indent-instr (format "sub t5, t5, ~a" bas))
                                          (indent-instr (format "CSetOffset ~a, ~a, ~a" r r bas))
                                          (indent-instr (format "CSetBounds ~a, ~a, ~a" r r len))
                                          (indent-instr (format "CSetOffset ~a, ~a, t5" r r)))]
    [_ #f]))



(define (createSeal i)
  (let ([addr (* i framesize)])
    (addSealAddr i addr)
    addr))

(define (generate-seal s)
  (match s
    [`(seal ,r ... ,i) #:when (andmap isCapability? r) (let ([addr (createSeal i)])
                                                         (string-append
                                                            (generate-set `(set! ct6 ((current-stack-base-pointer-register) - addr)))
                                                            (foldl (lambda (x) (string-append (indent-instr (format "CSeal ~a, ~a, ct6" x x)))) "" r)))] 
    [_ #f]))

(define (generate-unseal u)
  (match u
    [`(unseal ,r ... ,i) #:when (andmap isCapability? r) (let ([addr (getSealAddr i)])
                                                           (string-append
                                                            (generate-set `(set! ct6 ((current-stack-base-pointer-register) - addr)))
                                                            (foldl (lambda (x) (string-append (indent-instr (format "CUnseal ~a, ~a, ct6" x x)))) "" r)))]
    [_ #f]))

(define (generate-sentry s)
  (match s
    [`(sentry ,r) #:when (isCapability? r) (indent-instr (format "CSealEntry ~a, ~a" r r))]
    [_ #f]))


(define (generate-set s)
  (match s
    [`(set! (,r - ,n) ,b) #:when (isCapability? b) (string-append (indent-instr (format "CIncOffset ~a, ~a, ~a" (current-stack-register) r (- 0 n)))
                                                                  (indent-instr (format "sc.cap ~a, 0(~a)" b (current-stack-register))))]                ;store
    [`(set! (,r - ,n) ,b) #:when (isNonCapRegister? b) (string-append (indent-instr (format "CIncOffset ~a, ~a, ~a" (current-stack-register) r (- 0 n)))
                                                                      (indent-instr (format "sw.cap ~a, 0(~a)" b (current-stack-register))))]
    [`(set! ,a (,r - ,n)) #:when (isCapability? a) (string-append (indent-instr (format "CIncOffset ~a, ~a, ~a" (current-stack-register) r (- 0 n)))
                                                                  (indent-instr (format "lc.cap ~a, 0(~a)" a (current-stack-register))))]                 ;load
    [`(set! ,a (,r - ,n)) #:when (isNonCapRegister? a) (string-append (indent-instr (format "CIncOffset ~a, ~a, ~a" (current-stack-register) r (- 0 n)))
                                                                      (indent-instr (format "lw.cap ~a, 0(~a)" a (current-stack-register))))]
    [`(set! ,a ,b) #:when (and (isCapability? a) (isCapability? b))         (indent-instr (format "cmove ~a, ~a" a b))]         ;move
    [_ #f]))
    