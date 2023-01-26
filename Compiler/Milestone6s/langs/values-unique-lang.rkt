#lang at-exp racket/base

(require
 cpsc411/compiler-lib
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[values-unique-lang
  #:literals (name? int64? label? aloc?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
   >= > !=)
  [p     (module (define label (lambda (aloc ...) tail)) ... tail)]
  [pred  (relop opand opand)
         (true)
         (false)
         (not pred)
         (let ([aloc value] ...) pred)
         (if pred pred pred)]
  [tail  value
         (let ([aloc value] ...) tail)
         (if pred tail tail)
         (call triv opand ...)]
  [value triv
         (binop opand opand)
         (let ([aloc value] ...) value)
         (if pred value value)
         (call triv opand ...)]
  [opand int64 aloc]
  [triv  opand label]
  [binop * + -]
  [relop < <= = >= > !=]
  [aloc aloc?]
  [label label?]
  [int64 int64?]
]
