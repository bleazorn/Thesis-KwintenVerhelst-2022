#lang racket

(require "common/aloc.rkt"
         "langs/exprs-unique-lang.rkt"
         "langs/values-unique-lang.rkt")
(provide remove-complex-opera)

(module+ test
  (require rackunit))

(define (remove-alocs a)
  (map (lambda (alocs) `(,(first alocs) ,(remove-value (second alocs)))) a))

(define (remove-call v)
  (match v
    [`(call ,l ,vs ...) (let ([tmps (map (lambda (x) (freshtmp)) vs)]
                              [new-vs (map remove-value vs)])
                          `(let ,(map list tmps new-vs)
                             (call ,l ,@tmps)))]
    [_ (error (format "remove-complex-opera*:  Failed match.\n No valid call: ~a" v))]))

;
;(remove-pred p)->pred?
;p: pred?
(define (remove-pred p)
  (match p
    [`(let ,a ,pred) `(let ,(remove-alocs a) ,(remove-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(remove-pred p1) ,(remove-pred p2) ,(remove-pred p3))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(remove-pred pred))]
    [_ (error (format "remove-complex-opera*:  Failed match.\n No valid pred: ~a" p))]))

;
;(remove-value v)->value?
;v: value?
(define (remove-value v)
  (match v
    [`(let ,a ,val) `(let ,(remove-alocs a) ,(remove-value val))]
    [`(if ,p ,v1 ,v2) `(if ,(remove-pred p) ,(remove-value v1) ,(remove-value v2))]
    [`(call ,l ,vs ...) (remove-call v)]
    [`(,binop ,v1 ,v2) (let ([tmp1 (freshtmp)]
                             [tmp2 (freshtmp)])
                        `(let ([,tmp1 ,(remove-value v1)]
                               [,tmp2 ,(remove-value v2)])
                           (,binop ,tmp1 ,tmp2)))]
    [t t]))

;
;(remove-tail t)->tail?
;t: tail?
(define (remove-tail t)
  (match t
    [`(let ,a ,tail) `(let ,(remove-alocs a) ,(remove-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(remove-pred p) ,(remove-tail t1) ,(remove-tail t2))]
    [`(call ,l ,vs ...) (remove-call t)]
    [t (remove-value t)]))

(define (remove-func f)
  (match f
    [`(define ,l (lambda (,a ...) ,t)) `(define ,l (lambda ,a ,(remove-tail t)))]
    [_ (error (format "remove-complex-opera*: Failed match.\n No valid function: ~a" f))])) 

(define/contract (remove-complex-opera p) (-> exprs-unique-lang? values-unique-lang?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map remove-func f) ,(remove-tail t))]))

(module+ test
  )