#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[paren-cheri-risc-v
  #:literals (info? int64? int12? label? register? frame-base-pointer-register? dispoffset?)
  #:datum-literals (begin set! jump with-label compare jump-if * + < <= = split splice seal unseal sentry invoke setLinear! 
   >= > !=)
  [p     (begin info s ...)]
  [info info?]
  [s (set! addr int12)
     (set! addr trg)
     (set! reg loc)
     (set! reg triv)
     (set! reg (binop reg int16))
     (set! reg (binop reg loc))
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
     (setLinear! loc triv)]
  [opand int64 reg]
  [triv  trg int64]
  [loc   reg addr]
  [trg   label loc] ;reg]
  [binop * + -]
  [relop < <= = >= > !=]
  [memory-direction + -]
  [label label?]
  [reg   register?]
  [addr  (fbp memory-direction dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [int64 int64?]
  [int12 int12?]
]
