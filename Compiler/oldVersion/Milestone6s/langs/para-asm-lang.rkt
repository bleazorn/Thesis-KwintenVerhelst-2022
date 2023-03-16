#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[para-asm-lang
  #:literals (info? int64? label? register? frame-base-pointer-register? dispoffset?)
  #:datum-literals (begin set! jump with-label compare jump-if * + < <= = split splice seal unseal sentry invoke setLinear! set-addr!
   >= > !=)
  [p     (begin info s ...)]
  [info info?]
  [s (set! loc triv)
     (set! loc (binop loc opand))
     (jump trg)
     (with-label label s)
     (compare loc (relop loc loc))
     (jump-if label (relop loc loc))
     (split reg reg reg int64)
     (splice reg reg reg int64)
     (seal reg ... int64)
     (unseal reg ... int64)
     (sentry reg)
     (invoke reg reg)
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
