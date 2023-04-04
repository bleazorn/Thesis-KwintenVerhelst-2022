#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[exprs-unique-lang
  #:literals (info? int64? label? aloc?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
   >= > !=)
  [p     (module info (define label (lambda (aloc ...) tail)) ... tail)]
  [info info?]
  [pred  (relop opand opand)
         (true)
         (false)
         (not pred)
         (let ([aloc value] ...) pred)
         (if pred pred pred)]
  [tail  value
         (let ([aloc value] ...) tail)
         (if pred tail tail)
         (call triv value ...)]
  [value triv
         (binop value value)
         (let ([aloc value] ...) value)
         (if pred value value)
         (call triv value ...)]
  [opand int64 aloc]
  [triv  opand label]
  [binop * + -]
  [relop < <= = >= > !=]
  [aloc aloc?]
  [label label?]
  [int64 int64?]
]
