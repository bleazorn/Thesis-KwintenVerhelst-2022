#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[block-pred-lang
  #:literals (info? int64? label? register? frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module set! jump true false not if * + < <= =
   >= > !=)
  [p     (module info b ... b)]
  [info info?]
  [b     (define label tail)]
  [pred  (relop loc opand)
         (true)
         (false)
         (not pred)]
  [tail  (begin effect ... tail)
         (if pred (jump trg) (jump trg))
         (jump trg)]
  [effect (set! loc triv)
          (set! loc (binop loc opand))]
  [opand int64 loc]
  [triv  opand label]
  [loc   reg addr]
  [trg   label loc]
  [binop * + -]
  [relop < <= = >= > !=]
  [label label?]
  [reg   register?]
  [addr  (fbp - dispoffset)]
  [fbp   frame-base-pointer-register?]
  [dispoffset dispoffset?]
  [int64 int64?]
]