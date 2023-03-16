#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[imp-mf-lang
  #:literals (info? name? int64? label? aloc?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
   >= > !=)
  [p     (module info (define label (lambda (aloc ...) tail)) ... tail)]
  [info info?]
  [pred  (relop opand opand)
         (true)
         (false)
         (not pred)
         (begin effect ... pred)
         (if pred pred pred)]
  [tail  value
         (begin effect ... tail)
         (if pred tail tail)
         (call triv opand ...)]
  [value triv
         (binop opand opand)
         (begin effect ... value)
         (if pred value value)
         (call triv opand ...)]
  [effect (set! aloc value)
          (begin effect ... effect)
          (if pred effect effect)]
  [opand int64 aloc]
  [triv  opand label]
  [binop * + -]
  [relop < <= = >= > !=]
  [aloc aloc?]
  [label label?]
  [int64 int64?]
]
