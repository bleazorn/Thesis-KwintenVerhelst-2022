#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
         "common/info.rkt"
         "langs/nested-asm-lang-fvars.rkt"
         "langs/nested-asm-lang.rkt")
(provide implement-fvars-split)

(module+ test
  (require rackunit))

;!!!DECISION: fv0 = (fp - 0) fp is frame pointer and frame size is 8

(define offset 0)
(define (resetOffSet)
  (setOffSet 0))
(define (setOffSet n)
  (set! offset n))
(define (addOffSet binop n)
  (match binop
    ['- (set! offset (- offset n))]
    ['+ (set! offset (+ offset n))]
    [_  #f]))

(define frame-stack-size 0)
(define (set-frame-stack-size n)
  (set! frame-stack-size n))
(define (reset-frame-stack-size)
  (set! frame-stack-size 0))


;if given argument is of type fvar, transforms it into (fp - dispoffset)
;(change-fvar fvar)-> symbol?
;fvar: any?
(define (change-fvar f)
  (let ([n (getFvarNumber f)])
    (cond [(and (ffvar? f) (>= n frame-stack-size)) (let ([new-n (cond [(equal? (stack-direction) '-) (- (* (add1 (- n frame-stack-size)) (framesize)) offset)]
                                                                       [(equal? (stack-direction) '+) (+ (* (- n frame-stack-size) (framesize)) offset)]
                                                                       [else n])])
                                                      `(,(current-stack-base-pointer-register) ,(stack-direction) ,new-n))]
          [(ffvar? f) (let ([new-n (cond [(equal? (stack-direction) '-) (* (add1 n) (framesize))]
                                         [(equal? (stack-direction) '+) (* n (framesize))]
                                         [else n])])
                        `(,(current-stack-base-pointer-register) ,(stack-direction) ,new-n))]
          [(fgvar? f) (let ([new-n (cond [(equal? (global-direction) '-) (* (add1 n) (framesize))]
                                     [(equal? (global-direction) '+) (* n (framesize))]
                                     [else (* n (framesize))])])
                        `(,(current-global-register) ,(global-direction) ,new-n))]
          [else f])))

;
;(implement-pred p)->pred?
;p: pred?
(define (implement-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map implement-effect e) ,(implement-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(implement-pred p1) ,(implement-pred p2) ,(implement-pred p3))]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(implement-pred pred))]
    [`(,relop ,a ,b) `(,relop ,(change-fvar a) ,(change-fvar b))]
    [_ #f]))

;
;(implement-effect e)->effect?
;e: effect?
(define (implement-effect e)
  (match e
    [`(begin ,e ...) `(begin ,@(map implement-effect e))]
    [`(if ,p ,e1 ,e2) `(if ,(implement-pred p) ,(implement-effect e1) ,(implement-effect e2))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,(change-fvar a) (,binop ,(change-fvar b) ,(change-fvar c)))]
    [`(set! ,a ,b) `(set! ,(change-fvar a) ,(change-fvar b))]
    [`(setLinear! ,a ,b) `(setLinear! ,(change-fvar a) ,(change-fvar b))]
    [`(set-addr! ,a ,b) `(set-addr! ,(change-fvar a) ,(change-fvar b))]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(return-point ,l ,t) `(return-point ,l ,(implement-tail t))]
    [_ #f]))
    

;
;(implement-tail t)->tail?
;t: tail?
(define (implement-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map implement-effect e) ,(implement-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(implement-pred p) ,(implement-tail t1) ,(implement-tail t2))]
    [`(jump ,trg) `(jump ,(change-fvar trg))]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))

;
;(implement-tail-with-info i t)->tail?
;i: info?
;t: tail?
(define (implement-tail-with-info i t)
  (set-frame-stack-size (getInfo i getFrameSize))
  (setOffSet (getInfo i getActualFrameSize))
  (implement-tail t))

  
;
;(implement-func f)->'(define label? tail?)
;f: '(define label? tail?)
(define (implement-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(implement-tail-with-info i t))]
    [_ #f]))

;
;(implement-fvars p) → Paren-cheri-risc-v-V2?
;p : Paren-cheri-risc-v-V2-fvars?
(define/contract (implement-fvars-split p) (-> nested-asm-lang-fvars? nested-asm-lang?)
  (resetOffSet)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map implement-func f) ,(implement-tail-with-info i t))]
    [_ "implement fvars failed"]))






(module+ test
  )