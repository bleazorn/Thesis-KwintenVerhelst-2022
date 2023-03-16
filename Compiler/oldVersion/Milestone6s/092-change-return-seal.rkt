#lang racket

(require "common/register.rkt"
         "common/fvar.rkt"
         "common/info.rkt"
         "langs/nested-asm-lang-jumps.rkt")
(provide change-return-seal)

;
;(change-pred p)->pred?
;p: pred?
(define (change-pred p parasize)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (change-effect eff parasize)) e) ,(change-pred pred parasize))]
    [`(if ,p1 ,p2 ,p3) `(if ,(change-pred p1 parasize) ,(change-pred p2 parasize) ,(change-pred p3 parasize))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(change-pred pred parasize))]
    [_ #f]))

;
;(change-effect e)->effect?
;e->effect?
(define (change-effect e parasize)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (change-effect eff parasize)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(change-pred p parasize) ,(change-effect e1 parasize) ,(change-effect e2 parasize))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(return-point ,l ,t) `(begin (return-point ,l ,(change-tail t parasize))
                                   (set! cfp ct6))]
    [_ #f]))

;
;(change-tail t)->tail?
;t: tail?
(define (change-tail t parasize)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (change-effect eff parasize)) e) ,(change-tail tail parasize))]
    [`(if ,p ,t1 ,t2) `(if ,(change-pred p parasize) ,(change-tail t1 parasize) ,(change-tail t2 parasize))]
    [`(jump-call ,trg) `(jump-call ,trg)]
    [`(jump-return ,trg) `(invoke ,(current-return-address-register) ,(current-frame-base-pointer-register))]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))

;
;(change-info i t)->tail?
;t: tail?
;i: info?
(define (change-info i t)
  (let ([parasize (getInfo i getParamSize)])
    (change-tail t parasize)))

;
;(change-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (change-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(change-info i t))]
    [_ #t]))


(define/contract (change-return-seal p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map change-func f) ,(change-info i t))]
    [_ "replace locations failed"]))