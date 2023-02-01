#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt")
(provide change-frame-pointer)

(module+ test
  (require rackunit))

(define (maxFrame n)
  (* n framesize))

;
;(change-return-point p assign)->effect?
;p: pred?
;assign: list? '((aloc loc) ...)
(define (change-return-point e frames assign)
  (match e
   [`(return-point ,l ,t) (let ([sizeFrame (maxFvarNumber (filter fvar? (map second assign)))]
                                [callArgs (assoc l frames)])
                            (let ([frameSize (maxFrame (- (add1 sizeFrame) (cond [callArgs (length (second callArgs))]
                                                                                 [else 0])))]
                                  [fbp (current-frame-base-pointer-register)])
                              `(begin
                                 (set! ,fbp (- ,fbp ,frameSize))
                                 (return-point ,l ,(change-tail t frames assign))
                                 (set! ,fbp (+ ,fbp ,frameSize)))))]
    [_ #f]))

;
;(change-pred p assign)->pred?
;p: pred?
;assign: list? '((aloc loc) ...)
(define (change-pred p frames assign)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (change-effect eff frames assign)) e) ,(change-pred pred frames assign))]
    [`(if ,p1 ,p2 ,p3) `(if ,(change-pred p1 frames assign) ,(change-pred p2 frames assign) ,(change-pred p3 frames assign))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(change-pred pred frames assign))]
    [_ #f]))

;
;(change-effect e assign)->effect?
;e->effect?
;assign: list? '((aloc loc) ...)
(define (change-effect e frames assign)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (change-effect eff frames assign)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(change-pred p frames assign) ,(change-effect e1 frames assign) ,(change-effect e2 frames assign))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(return-point ,l ,t) (change-return-point e frames assign)]
    [_ #f]))

;
;(change-tail t assign)->tail?
;t: tail?
;assign: list? '((aloc loc) ...)
(define (change-tail t frames assign)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (change-effect eff frames assign)) e) ,(change-tail tail frames assign))]
    [`(if ,p ,t1 ,t2) `(if ,(change-pred p frames assign) ,(change-tail t1 frames assign) ,(change-tail t2 frames assign))]
    [`(jump-call ,trg) `(jump ,trg)]
    [`(jump-return ,trg) `(jump ,trg)]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))

;
;(change-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (change-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(change-tail t (getInfo i getNewFrames) (getInfo i getAssignment)))]
    [_ #t]))


(define (change-frame-pointer p)
  (match p
    [`(module ,i ,f ... ,pro) `(module () ,@(map change-func f) ,(change-tail pro (getInfo i getNewFrames) (getInfo i getAssignment)))]
    [_ "replace locations failed"]))

#;(change-frame-pointer '(module ((assignment ((tmp-ra.11 fv0))))
  (define L.odd?.1
    ((assignment ((x.3 t1) (tmp.13 t0) (y.4 t0) (tmp-ra.7 fv0))))
    (begin
      (set! fv0 cra)
      (set! t1 a0)
      (if (begin (set! t0 0) (= t1 t0))
        (begin (set! a0 150) (jump fv0))
        (begin
          (set! t0 (+ t1 -1))
          (begin
            (return-point
             L.rpLabel.8
             (begin (set! a0 t0) (set! cra L.rpLabel.8) (jump L.even?.2)))
            (jump fv0))))))
  (define L.even?.2
    ((assignment ((x.5 t1) (tmp.14 t0) (y.6 t0) (tmp-ra.9 fv0))))
    (begin
      (set! fv0 cra)
      (set! t1 a0)
      (if (begin (set! t0 0) (= t1 t0))
        (begin (set! a0 200) (jump fv0))
        (begin
          (set! t0 (+ t1 -1))
          (begin
            (return-point
             L.rpLabel.10
             (begin (set! a0 t0) (set! cra L.rpLabel.10) (jump L.odd?.1)))
            (jump fv0))))))
  (begin
    (set! fv0 cra)
    (begin
      (return-point
       L.rpLabel.12
       (begin (set! a0 5) (set! cra L.rpLabel.12) (jump L.even?.2)))
      (jump fv0)))))


(module+ test
  )