#lang at-exp racket/base

(require
 "../common/langs-lib.rkt"
 cpsc411/langs/redex-gen)

(provide
 (all-defined-out))

@define-grammar/pred[imp-cmf-lang
  #:literals (info? int64? label? aloc? register? fvar?)
  #:datum-literals (define module set! jump-call jump-return return-point true false not if * + < <= =
   >= > !=)
  [p     (module info (define label info tail) ... tail)]
  [info info?]
  [pred  (relop opand opand)
         (true)
         (false)
         (not pred)
         (begin effect ... pred)
         (if pred pred pred)]
  [tail  (begin effect ... tail)
         (if pred tail tail)
         (jump-call trg loc ...)
         (jump-return trg loc ...)]
  [value triv
         (binop opand opand)]
  [effect (set! loc value)
          (begin effect ... effect)
          (if pred effect effect)
          (return-point label tail)]
  [opand int64 loc]
  [triv  opand label]
  [loc   rloc aloc]
  [trg   label loc]
  [binop * + -]
  [relop < <= = >= > !=]
  [aloc aloc?]
  [label label?]
  [rloc  register? fvar?]
  [int64 int64?]
]