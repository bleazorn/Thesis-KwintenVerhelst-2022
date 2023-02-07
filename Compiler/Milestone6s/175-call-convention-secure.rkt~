#lang racket

(require "common/fvar.rkt"
         "common/info.rkt"
         "common/register.rkt")
(provide call-convention-secure)

;
;(call-pred p)->pred?
;p: pred?
(define (call-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map call-effect e) ,(call-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(call-pred p1) ,(call-pred p2) ,(call-pred p3))]
    [`(,relop ,a ,b) p]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(call-pred pred))]
    [_ #f]))


;
;(call-effect e)->effect?
;e: effect?
(define (call-effect e)
  (match e
    [`(begin ,e ...) `(begin ,@(map call-effect e))]
    [`(if ,p ,e1 ,e2) `(if ,(call-pred p) ,(call-effect e1) ,(call-effect e2))]
    [`(set! ,a ,v) e]
    [`(return-point ,l ,t) `(return-point ,l ,(call-tail t #t))]
    [_ #f]))

;
;(call-tail t return-p)->tail?
;t: tail?
;return-p: boolean?
(define (call-tail t return-p)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map call-effect e) ,(call-tail tail return-p))]
    [`(if ,p ,t1 ,t2) `(if ,(call-pred p) ,(call-tail t1 return-p) ,(call-tail t2 return-p))]
    [`(jump-call ,l ,a ...) `(jump-call ,l ,@a)]                                                   ;Call
    [`(jump-return ,l ,a ...) `(jump-return ,l ,@a)]                                               ;Return
    [_ #f]))

;
;(call-tail-info info tail)->info? tail?
;info: info?
;tail: tail?
(define (call-tail-info info tail)
  (let ([parasize (getInfo info getParamSize)]
        [newTail (call-tail tail #f)])
    (setfvar (sub1 parasize))
    (values (addInfo info (setAllocatedFvars `(,(newFvar parasize) ,(newFvar (add1 parasize)))))
            newTail)))
       

;
;(call-func f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define (call-func f)
  (match f
    [`(define ,l ,i ,t) (let-values ([(info tail) (call-tail-info i t)])
                          `(define ,l ,info ,tail))]
    [_ #f]))


;
;(call-convention-secure p)->Imp-lang-V5-cmf-proc?
;p : Imp-lang-V5-cmf-proc?
(define (call-convention-secure p)
  (match p
    [`(module ,i ,f ... ,t) (let-values ([(info tail) (call-tail-info i t)])
                              `(module ,info ,@(map call-func f) ,tail))]
    [_ #f]))



#;(call-convention-secure '(module ((new-frames ()) (paramSize 0))
                           (define L.odd?.1
                             ((new-frames ()) (paramSize 0))
                             (begin
                               (set! tmp-ra.7 cra)
                               (set! x.3 a0)
                               (if (= x.3 0)
                                   (begin (set! a0 150) (jump tmp-ra.7 cfp a0))
                                   (begin
                                     (set! y.4 (+ x.3 -1))
                                     (begin
                                       (return-point
                                        L.rpLabel.8
                                        (begin
                                          (set! a0 y.4)
                                          (set! cra L.rpLabel.8)
                                          (jump L.even?.2 cfp cra a0)))
                                       (jump tmp-ra.7 cfp a0))))))
                           (define L.even?.2
                             ((new-frames ()) (paramSize 0))
                             (begin
                               (set! tmp-ra.9 cra)
                               (set! x.5 a0)
                               (if (= x.5 0)
                                   (begin (set! a0 200) (jump tmp-ra.9 cfp a0))
                                   (begin
                                     (set! y.6 (+ x.5 -1))
                                     (begin
                                       (return-point
                                        L.rpLabel.10
                                        (begin
                                          (set! a0 y.6)
                                          (set! cra L.rpLabel.10)
                                          (jump L.odd?.1 cfp cra a0)))
                                       (jump tmp-ra.9 cfp a0))))))
                           (begin
                             (set! tmp-ra.11 cra)
                             (begin
                               (return-point
                                L.rpLabel.12
                                (begin (set! a0 5) (set! cra L.rpLabel.12) (jump L.even?.2 cfp cra a0)))
                               (jump tmp-ra.11 cfp a0)))))