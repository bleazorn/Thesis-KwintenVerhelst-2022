#lang racket

(require "common/info.rkt")
(require "common/fvar.rkt"
         "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt"
         "langs/nested-asm-lang-fvars.rkt")
(provide replace-call-got-sentry)


;
;(replace-label trg replace-labels)->tail?
;trg: label?
;replace-labels: list? '(label? ...)
(define (replace-label trg replace-labels)
  (let ([loc (second (assoc trg replace-labels))])
    `(begin
       (set! ,(current-invoke-jump-register) ,(newGvar loc))
       (jump ,(current-invoke-jump-register)))))

;
;(replace-pred p assign)->pred?
;p: pred?
;replace-labels: list? '(label? ...)
(define (replace-pred p replace-labels)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (replace-effect eff replace-labels)) e) ,(replace-pred pred replace-labels))]
    [`(if ,p1 ,p2 ,p3) `(if ,(replace-pred p1 replace-labels) ,(replace-pred p2 replace-labels) ,(replace-pred p3 replace-labels))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(replace-pred pred replace-labels))]
    [_ #f]))

;
;(replace-effect e assign)->effect?
;e->effect?
;replace-labels: list? '(label? ...)
(define (replace-effect e replace-labels)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (replace-effect eff replace-labels)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(replace-pred p replace-labels) ,(replace-effect e1 replace-labels) ,(replace-effect e2 replace-labels))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(set-addr! ,a ,b) `(set-addr! ,a ,b)]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(return-point ,l ,t) `(return-point ,l ,(replace-tail t replace-labels))]
    [_ #f]))

;
;(replace-tail t assign)->tail?
;t: tail?
;replace-labels: list? '(label? ...)
(define (replace-tail t replace-labels)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (replace-effect eff replace-labels)) e) ,(replace-tail tail replace-labels))]
    [`(if ,p ,t1 ,t2) `(if ,(replace-pred p replace-labels) ,(replace-tail t1 replace-labels) ,(replace-tail t2 replace-labels))]
    [`(jump-call ,trg) (replace-label trg replace-labels)]
    [`(jump-return ,trg) `(jump ,trg)]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))


;
;(replace-tail t assign)->tail?
;t: tail?
;replace-labels: list? '(label? ...)
(define (replace-tail-begin t replace-labels)
  (replace-tail t replace-labels))

;
;(replace-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (replace-func f replace-labels)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i  ,(replace-tail-begin t replace-labels))]
    [_ #t]))


(define/contract (replace-call-got-sentry p) (-> nested-asm-lang-jumps? nested-asm-lang-fvars?)
  (match p
    [`(module ,i ,f ... ,t) (let ([replace-labels (getInfo i getGOTLabels)])
                              `(module ,i ,@(map (lambda (func) (replace-func func replace-labels)) f) ,(replace-tail-begin t replace-labels)))]
    [_ "replace locations failed"]))

