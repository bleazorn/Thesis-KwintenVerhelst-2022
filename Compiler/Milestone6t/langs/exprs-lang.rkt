#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[exprs-lang
  #:literals (info? name? int64?)
  #:datum-literals (define lambda module let call true false not if * + < <= =
   >= > !=)
  [p     (module (define x (lambda (x ...) value)) ... value)]
  [pred  (relop triv triv)
         (true)
         (false)
         (not pred)
         (let ([x value] ...) pred)
         (if pred pred pred)]
  [value triv
         (binop value value)
         (let ([x value] ...) value)
         (if pred value value)
         (call x value ...)]
  [triv  int64 x]
  [x     name?]
  [binop * + -]
  [relop < <= = >= > !=]
  [int64 int64?]
]
