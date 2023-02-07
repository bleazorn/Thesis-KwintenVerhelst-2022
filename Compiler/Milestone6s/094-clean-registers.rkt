#lang racket

(require "langs/nested-asm-lang-jumps.rkt")
(provide clean-registers)

;
;(clean-pred p assign)->pred?
;p: pred?
;clean-regs-call clean-regs-return: list? '(register? ...)
(define (clean-pred p clean-regs-call clean-regs-return)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (clean-effect eff clean-regs-call clean-regs-return)) e) ,(clean-pred pred clean-regs-call clean-regs-return))]
    [`(if ,p1 ,p2 ,p3) `(if ,(clean-pred p1 clean-regs-call clean-regs-return) ,(clean-pred p2 clean-regs-call clean-regs-return) ,(clean-pred p3 clean-regs-call clean-regs-return))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(clean-pred pred clean-regs-call clean-regs-return))]
    [_ #f]))

;
;(clean-effect e assign)->effect?
;e->effect?
;clean-regs-call clean-regs-return: list? '(register? ...)
(define (clean-effect e clean-regs-call clean-regs-return)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (clean-effect eff clean-regs-call clean-regs-return)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(clean-pred p clean-regs-call clean-regs-return) ,(clean-effect e1 clean-regs-call clean-regs-return) ,(clean-effect e2 clean-regs-call clean-regs-return))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(return-point ,l ,t) `(return-point ,l ,(clean-tail t clean-regs-call clean-regs-return))]
    [_ #f]))

;
;(clean-tail t assign)->tail?
;t: tail?
;clean-regs-call clean-regs-return: list? '(register? ...)
(define (clean-tail t clean-regs-call clean-regs-return)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (clean-effect eff clean-regs-call clean-regs-return)) e) ,(clean-tail tail clean-regs-call clean-regs-return))]
    [`(if ,p ,t1 ,t2) `(if ,(clean-pred p clean-regs-call clean-regs-return) ,(clean-tail t1 clean-regs-call clean-regs-return) ,(clean-tail t2 clean-regs-call clean-regs-return))]
    [`(jump-call ,trg) `(begin ,@(map (lambda (r) `(set! ,r 0)) clean-regs-call)
                          (jump-call ,trg))]
    [`(jump-return ,trg) `(begin ,@(map (lambda (r) `(set! ,r 0)) clean-regs-return)
                          (jump-return ,trg))]
    [`(invoke ,a ,b) `(begin ,@(map (lambda (r) `(set! ,r 0)) clean-regs-return)
                          (invoke ,a ,b))]
    [_ #f]))

;
;(clean-info t i)->tail?
;t: tail?
;i: info?
(define (clean-info t i)
  (let ([clean-regs-call `(t0 t1 t2 t3 t4 t5 t6)]
        [clean-regs-return `(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6)])
    (clean-tail t clean-regs-call clean-regs-return)))
      

;
;(clean-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (clean-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(clean-info t i))]
    [_ #t]))


(define/contract (clean-registers p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map clean-func f) ,(clean-info t i))]
    [_ "replace locations failed"]))