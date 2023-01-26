#lang at-exp racket/base

(require
 cpsc411/compiler-lib
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[values-lang
  #:literals (name? int64?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
   >= > !=)
  [p     (module (define x (lambda (x ...) tail)) ... tail)]
  [pred  (relop triv triv)
         (true)
         (false)
         (not pred)
         (let ([x value] ...) pred)
         (if pred pred pred)]
  [tail  value
         (let ([x value] ...) tail)
         (if pred tail tail)
         (call x triv ...)]
  [value triv
         (binop triv triv)
         (let ([x value] ...) value)
         (if pred value value)
         (call x triv ...)]
  [triv  int64 x]
  [x     name?]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
]
