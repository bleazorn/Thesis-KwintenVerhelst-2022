#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt")
(provide secure-cheri-linkage)

(module+ test
  (require rackunit))

;
;(secure-pred p)->pred?
;p: pred?
(define (secure-pred p parasize)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (secure-effect eff parasize)) e) ,(secure-pred pred parasize))]
    [`(if ,p1 ,p2 ,p3) `(if ,(secure-pred p1 parasize) ,(secure-pred p2 parasize) ,(secure-pred p3 parasize))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(secure-pred pred parasize))]
    [_ #f]))

;
;(secure-effect e)->effect?
;e->effect?
(define (secure-effect e parasize)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (secure-effect eff parasize)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(secure-pred p parasize) ,(secure-effect e1 parasize) ,(secure-effect e2 parasize))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(return-point ,l ,t) `(begin
                               (set! ,(newFvar parasize) ,(current-return-address-register))
                               (set! ,(newFvar (add1 parasize)) ,(current-frame-base-pointer-register))
                               (return-point ,l ,(secure-tail t parasize))
                               (set! ,(current-frame-base-pointer-register) ,(newFvar (add1 parasize)))
                               (set! ,(current-return-address-register) ,(newFvar parasize)))]
    [_ #f]))

;
;(secure-tail t)->tail?
;t: tail?
(define (secure-tail t parasize)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (secure-effect eff parasize)) e) ,(secure-tail tail parasize))]
    [`(if ,p ,t1 ,t2) `(if ,(secure-pred p parasize) ,(secure-tail t1 parasize) ,(secure-tail t2 parasize))]
    [`(jump-call ,trg) `(jump-call ,trg)]
    [`(jump-return ,trg) `(jump-return ,trg)]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))

;
;(secure-info i t)->tail?
;t: tail?
;i: info?
(define (secure-info i t)
  (let ([parasize (getInfo i getParamSize)])
    (secure-tail t parasize)))

;
;(secure-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (secure-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(secure-info i t))]
    [_ #t]))

(define/contract (secure-cheri-linkage p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) (let ([info (addInfo '() (setFrameSize (cons `(main ,(getInfo i getFrameSize))
                                                                         (map list
                                                                              (map second f)
                                                                              (map (lambda (x) (getInfo (third x) getFrameSize)) f)))))])
                              `(module ,info ,@(map secure-func f) ,(secure-info i t)))]
    [_ "replace locations failed"]))

(module+ test
  )