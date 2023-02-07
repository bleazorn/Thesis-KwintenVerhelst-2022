#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[paren-cheri-risc-v
  #:literals (info? int64? int16? label? register? frame-base-pointer-register? dispoffset?)
  #:datum-literals (begin set! jump with-label compare jump-if * + < <= =
   >= > !=)
  [p     (begin info s ...)]
  [info info?]
  [s (set! addr int16)
     (set! addr trg)
     (set! reg loc)
     (set! reg triv)
     (set! reg (binop reg int16))
     (set! reg (binop reg loc))
     (jump trg)
     (with-label label s)
     (compare loc (relop loc loc))
     (jump-if label (relop loc loc))]
  [opand int64 reg]
  [triv  trg int64]
  [loc   reg addr]
  [trg   label loc] ;reg]
  [binop * + -]
  [relop < <= = >= > !=]
  [label label?]
  [reg   register?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [int64 int64?]
  [int16 int16?]
]
