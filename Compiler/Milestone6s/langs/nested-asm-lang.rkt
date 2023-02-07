#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[nested-asm-lang
  #:literals (info? int64? label? register? frame-base-pointer-register? dispoffset?)
  #:datum-literals (define module set! jump return-point true false not if * + < <= =
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
         (jump trg)]
  [effect (set! loc triv)
          (set! loc (binop loc opand))
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
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
