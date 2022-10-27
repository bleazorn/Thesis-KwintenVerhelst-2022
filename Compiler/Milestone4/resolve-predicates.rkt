#lang racket

(provide resolve-predicates)

(module+ test
  (require rackunit))

;
;(resolve-not p)->pred?
;p: pred?
(define (resolve-not p)
  (match p
    [`(= ,a ,b) `(!= ,a ,b)]
    [`(!= ,a ,b) `(= ,a ,b)]
    [`(< ,a ,b) `(>= ,a ,b)]
    [`(> ,a ,b) `(<= ,a ,b)]
    [`(<= ,a ,b) `(> ,a ,b)]
    [`(>= ,a ,b) `(< ,a ,b)]
    ['(true) '(false)]
    ['(false) '(true)]
    [`(not ,pred) (resolve-not (resolve-not pred))]
    [_ #f]))

;
;(resolve-pred p)->pred?
;p: pred?
(define (resolve-pred p)
  (match p
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) (resolve-not pred)]
    [_ #f]))

;
;(resolve-if p l1 l2)->tail?
;p: pred?
;l1: label?
;l2: label?
(define (resolve-if p l1 l2)
  (match p
    [`(,relop ,a ,b) `(if (,relop ,a ,b) (jump ,l1) (jump ,l2))]
    [`(true) `(jump ,l1)]
    [`(false) `(jump ,l2)]
    [_ #f]))
    

;
;(resolve-tail t)->tail?
;t: tail?
(define (resolve-tail t)
  (match t
    [`(halt ,a) `(halt ,a)]
    [`(jump ,l) `(jump ,l)]
    [`(begin ,e ... ,tail) `(begin ,e ... ,(resolve-tail tail))]
    [`(if ,p (jump ,l1) (jump ,l2)) (resolve-if (resolve-pred p) l1 l2)]
    [_ #f]))

;
;(resolve-b b)->b?
;b:b?
(define (resolve-b b)
  (match b
    [`(define ,l ,t) `(define ,l ,(resolve-tail t))]
    [_ #f]))

;
;(resolve-predicates p)->Pred-lang-V4-Block
;p:Asm-lang-V4-Block
(define (resolve-predicates p)
  (match p
    [`(module ,b ...) `(module ,(map (resolve-b) b))]
    [_ #f]))

(module+ test
  )