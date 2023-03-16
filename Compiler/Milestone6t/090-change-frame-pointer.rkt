#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt"
         "langs/nested-asm-lang-fvars.rkt")
(provide change-frame-pointer)

(module+ test
  (require rackunit))

(define (maxFrame n)
  (* n (framesize)))

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
                                  [fbp (current-stack-base-pointer-register)])
                              `(begin
                                 (set! ,fbp (,(stack-direction) ,fbp ,frameSize))
                                 (return-point ,l ,(change-tail t frames assign))
                                 (set! ,fbp (,(opposite-direction (stack-direction)) ,fbp ,frameSize)))))]
    [_ (error (format "change-frame-pointer:  Failed match.\n No valid return-point: ~a" e))]))

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
    [_ (error (format "change-frame-pointer:  Failed match.\n No valid pred: ~a" p))]))

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
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(set-addr! ,a ,b) `(set-addr! ,a ,b)]
    [_ (error (format "change-frame-pointer:  Failed match.\n No valid effect: ~a" e))]))

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
    [_ (error (format "change-frame-pointer:  Failed match.\n No valid tail: ~a" t))]))

;
;(change-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (change-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(change-tail t (getInfo i getNewFrames) (getInfo i getAssignment)))]
    [_ (error (format "change-frame-pointer:  Failed match.\n No valid function: ~a" f))]))


(define/contract (change-frame-pointer p) (-> nested-asm-lang-jumps? nested-asm-lang-fvars?)
  (match p
    [`(module ,i ,f ... ,pro) `(module () ,@(map change-func f) ,(change-tail pro (getInfo i getNewFrames) (getInfo i getAssignment)))]))


#;(parameterize ([stack-direction '+])
               (change-frame-pointer '(module ((frameSize 1)
            (assignment ((tmp-ra.7 fv0)))
            (conflicts
             ((a1 (cra cfp a0))
              (a0 (tmp-ra.7 cra a1 cfp))
              (cfp (tmp-ra.7 cra a1 a0))
              (cra (cfp a0 a1))
              (tmp-ra.7 (cfp a0))))
            (undead-out
             ((tmp-ra.7 cfp)
              (((tmp-ra.7 cfp a0)
                ((cfp a0) (cfp a0 a1) (cfp cra a0 a1) (cfp cra a0 a1)))
               (cfp a0))))
            (call-undead (tmp-ra.7))
            (locals (tmp-ra.7))
            (new-frames ())
            (paramSize 0))
  (define L.swap.1
    ((frameSize 4)
     (assignment ((z.4 fv0) (y.3 fv1) (x.2 fv2) (tmp-ra.5 fv3)))
     (conflicts
      ((cfp (tmp-ra.5 a0 z.4 cra a1 x.2 y.3))
       (a1 (cra cfp a0 x.2 tmp-ra.5))
       (a0 (tmp-ra.5 cfp cra a1 x.2))
       (cra (cfp a0 a1))
       (z.4 (tmp-ra.5 cfp))
       (y.3 (x.2 cfp tmp-ra.5))
       (x.2 (cfp a0 y.3 tmp-ra.5 a1))
       (tmp-ra.5 (cfp a0 z.4 x.2 y.3 a1))))
     (undead-out
      ((a0 a1 tmp-ra.5 cfp)
       (a1 x.2 tmp-ra.5 cfp)
       (x.2 tmp-ra.5 cfp y.3)
       ((x.2 tmp-ra.5 cfp y.3)
        ((tmp-ra.5 cfp a0) (cfp a0))
        ((((a0 tmp-ra.5 cfp)
           ((x.2 cfp a0) (cfp a0 a1) (cfp cra a0 a1) (cfp cra a0 a1)))
          (z.4 tmp-ra.5 cfp))
         ((tmp-ra.5 cfp a0) (cfp a0))))))
     (call-undead (tmp-ra.5))
     (locals (tmp-ra.5 x.2 y.3 z.4))
     (new-frames ())
     (paramSize 0))
    (begin
      (set! fv3 cra)
      (set! fv2 a0)
      (set! fv1 a1)
      (if (< fv1 fv2)
        (begin (set! a0 fv2) (jump-return fv3))
        (begin
          (begin
            (return-point
             L.rpLabel.6
             (begin
               (set! a0 fv1)
               (set! a1 fv2)
               (set! cra L.rpLabel.6)
               (jump-call L.swap.1)))
            (set! fv0 a0))
          (begin (set! a0 fv0) (jump-return fv3))))))
  (begin
    (set! fv0 cra)
    (begin
      (return-point
       L.rpLabel.8
       (begin
         (set! a0 1)
         (set! a1 2)
         (set! cra L.rpLabel.8)
         (jump-call L.swap.1)))
      (jump-return fv0))))))


(module+ test
  )