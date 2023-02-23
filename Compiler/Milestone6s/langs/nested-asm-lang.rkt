#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[nested-asm-lang
  #:literals (info? int64? label? register? frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module set! jump return-point true false not if * + < <= = split splice seal unseal sentry invoke setLinear! set-addr!
   >= > !=)
  [p     (module info (define label tail) ... tail)]
  [info info?]
  [pred  (relop loc opand)
         (true)
         (false)
         (not pred)
         (begin effect ... pred)
         (if pred pred pred)]
  [tail  (begin effect ... tail)
         (if pred tail tail)
         (jump trg)
         (invoke reg reg)]
  [effect (set! loc triv)
          (set! loc (binop loc opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)
          (split reg reg reg int64)
          (splice reg reg reg int64)
          (seal reg ... int64)
          (unseal reg ... int64)
          (sentry reg)
          (setLinear! loc triv)
          (set-addr! loc loc)]
  [opand int64 loc]
  [triv  opand label]
  [loc   reg addr]
  [trg   label loc]
  [binop * + -]
  [relop < <= = >= > !=]
  [memory-direction + -]
  [label label?]
  [reg   register?]
  [addr  (fbp memory-direction dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [int64 int64?]
]
