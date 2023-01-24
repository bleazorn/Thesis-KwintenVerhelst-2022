#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt")
(provide secure-stack-tokens)

(module+ test
  (require rackunit))

(define (maxFrame n)
  (* n framesize))

;
;(secure-return-point p assign)->effect?
;p: pred?
;assign: list? '((aloc loc) ...)
(define (secure-return-point e framesize parasize)
  (match e
   [`(return-point ,l ,t) (let([token (random 132057 231056)])
                            `(begin
                               (set! ,(newFvar parasize) ,(current-return-address-register))
                               (setLinear! ,(newFvar (add1 parasize)) ,(current-frame-base-pointer-register))
                               (split csp csp cfp ,framesize)
                               (return-point ,l ,(secure-tail t framesize parasize token))
                               (set! cfp ct6)
                               (splice csp csp cfp ,framesize)
                               (set! ,(current-return-address-register) ,(newFvar parasize))
                               (setLinear! ,(current-frame-base-pointer-register) ,(newFvar (add1 parasize)))))]
    [_ #f]))

;
;(secure-pred p assign)->pred?
;p: pred?
;assign: list? '((aloc loc) ...)
(define (secure-pred p framesize parasize)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (secure-effect eff framesize parasize)) e) ,(secure-pred pred framesize parasize))]
    [`(if ,p1 ,p2 ,p3) `(if ,(secure-pred p1 framesize parasize) ,(secure-pred p2 framesize parasize) ,(secure-pred p3 framesize parasize))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(secure-pred pred framesize parasize))]
    [_ #f]))

;
;(secure-effect e assign)->effect?
;e->effect?
;assign: list? '((aloc loc) ...)
(define (secure-effect e framesize parasize)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (secure-effect eff framesize parasize)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(secure-pred p framesize parasize) ,(secure-effect e1 framesize parasize) ,(secure-effect e2 framesize parasize))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(return-point ,l ,t) (secure-return-point e framesize parasize)]
    [_ #f]))

;
;(secure-tail t assign)->tail?
;t: tail?
;assign: list? '((aloc loc) ...)
(define (secure-tail t framesize parasize token)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (secure-effect eff framesize parasize)) e) ,(secure-tail tail framesize parasize token))]
    [`(if ,p ,t1 ,t2) `(if ,(secure-pred p framesize parasize) ,(secure-tail t1 framesize parasize token) ,(secure-tail t2 framesize parasize token))]
    [`(jump ,trg) (cond [(= token 0) `(jump ,trg)]
                        [else `(begin (seal cra cfp ,token) (jump ,trg))])]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))

;
;(secure-info t i)->tail?
;t: tail?
;i: info?
(define (secure-info t i)
  (let ([frameSize (cond [(> (* (getInfo i getFrameSize) (framesize)) 16384) (* (getInfo i getFrameSize) (framesize))]
                         [else 16384])]
        [parasize (getInfo i getParamSize)])
    (secure-tail t frameSize parasize 0)))
      

;
;(secure-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (secure-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l  ,(secure-info t i))]
    [_ #t]))


(define (secure-stack-tokens p)
  (match p
    [`(module ,i ,f ... ,t) `(module ,@(map secure-func f) ,(secure-info t i))]
    [_ "replace locations failed"]))


#;(secure-stack-tokens '(module ((frameSize 0)
            (assignment ((tmp-ra.11 t0)))
            (assignment ((tmp-ra.11 t0)))
            (assignment ((tmp-ra.11 t0)))
            (conflicts
             ((b ())
              (a ())
              (a0 (cra cfp))
              (sv2 (cra fv0 fv1 cfp))
              (sv1 (csp fv0 fv1 cfp))
              (sv0 (csp cra fv0 fv1))
              (fv1 (cfp fv0 tmp-ra.11 cra sv2 csp sv1 sv0))
              (fv0 (fv1 tmp-ra.11 cra sv2 csp sv1 sv0 cfp))
              (cra (cfp a0 fv0 fv1 sv2 csp sv0))
              (csp (cra fv0 fv1 cfp sv1 sv0))
              (cfp (cra fv1 a0 tmp-ra.11 sv2 csp sv1 fv0))
              (tmp-ra.11 (fv0 fv1 cfp))))
            (undead-out
             ((csp cra fv0 fv1 cfp)
              (csp cra fv0 fv1 cfp)
              (cra fv0 fv1 cfp)
              (fv0 fv1 cfp)
              (fv0 fv1 cfp)
              (((fv0 fv1) ((cfp a0) (cfp cra a0) (cfp cra a0)))
               (fv1 cfp)
               (cra cfp)
               (cra cfp))))
            (call-undead (fv0 fv1))
            (locals (tmp-ra.11))
            (paraSize -1)
            (new-frames ()))
  (define L.odd?.1
    ((frameSize 0)
     (assignment ((y.4 t0) (tmp.13 t0) (x.3 t1) (tmp-ra.7 t0)))
     (assignment ((y.4 t0) (tmp.13 t0) (x.3 t1) (tmp-ra.7 t0)))
     (assignment ((y.4 t0) (tmp.13 t0) (x.3 t1) (tmp-ra.7 t0)))
     (conflicts
      ((b ())
       (a ())
       (sv2 (cra a0 fv0 fv1 cfp))
       (sv1 (csp a0 fv0 fv1 cfp))
       (sv0 (csp cra a0 fv0 fv1))
       (fv1 (cfp fv0 y.4 x.3 a0 tmp.13 tmp-ra.7 cra sv2 csp sv1 sv0))
       (fv0 (fv1 y.4 x.3 a0 tmp.13 tmp-ra.7 cra sv2 csp sv1 sv0 cfp))
       (a0 (cra cfp fv0 fv1 tmp-ra.7 sv2 csp sv1 sv0))
       (cra (cfp a0 fv0 fv1 sv2 csp sv0))
       (csp (cra a0 fv0 fv1 cfp sv1 sv0))
       (cfp (cra fv1 a0 y.4 x.3 tmp.13 tmp-ra.7 sv2 csp sv1 fv0))
       (y.4 (cfp fv0 fv1))
       (tmp.13 (fv0 fv1 x.3 cfp))
       (x.3 (fv0 fv1 cfp tmp.13))
       (tmp-ra.7 (a0 fv0 fv1 cfp))))
     (undead-out
      ((csp cra a0 fv0 fv1 cfp)
       (csp cra a0 fv0 fv1 cfp)
       (cra a0 fv0 fv1 cfp)
       (cra a0 fv0 fv1 cfp)
       (a0 fv0 fv1 cfp)
       (fv0 fv1 x.3 cfp)
       (((tmp.13 fv0 fv1 x.3 cfp) (fv0 fv1 x.3 cfp))
        ((fv0 fv1) (fv1 cfp) (cra cfp) (cra cfp))
        ((fv0 fv1 y.4 cfp)
         (((fv0 fv1) ((cfp a0) (cfp cra a0) (cfp cra a0)))
          (fv1 cfp)
          (cra cfp)
          (cra cfp))))))
     (call-undead (fv0 fv1))
     (locals (tmp-ra.7 x.3 tmp.13 y.4))
     (paraSize -1)
     (new-frames ()))
    (begin
      (set! sv0 cfp)
      (set! sv1 cra)
      (set! sv2 csp)
      (split)
      (set! t0 cra)
      (set! t1 a0)
      (if (begin (set! t0 0) (= t1 t0))
        (begin (set! a0 150) (set! cfp fv0) (set! cra fv1) (invoke cra cfp))
        (begin
          (set! t0 (+ t1 -1))
          (begin
            (return-point
             L.rpLabel.8
             (begin (set! a0 t0) (set! cra L.rpLabel.8) (jump L.even?.2)))
            (set! cfp fv0)
            (set! cra fv1)
            (invoke cra cfp))))))
  (define L.even?.2
    ((frameSize 0)
     (assignment ((y.6 t0) (tmp.14 t0) (x.5 t1) (tmp-ra.9 t0)))
     (assignment ((y.6 t0) (tmp.14 t0) (x.5 t1) (tmp-ra.9 t0)))
     (assignment ((y.6 t0) (tmp.14 t0) (x.5 t1) (tmp-ra.9 t0)))
     (conflicts
      ((b ())
       (a ())
       (sv2 (cra a0 fv0 fv1 cfp))
       (sv1 (csp a0 fv0 fv1 cfp))
       (sv0 (csp cra a0 fv0 fv1))
       (fv1 (cfp fv0 y.6 x.5 a0 tmp.14 tmp-ra.9 cra sv2 csp sv1 sv0))
       (fv0 (fv1 y.6 x.5 a0 tmp.14 tmp-ra.9 cra sv2 csp sv1 sv0 cfp))
       (a0 (cra cfp fv0 fv1 tmp-ra.9 sv2 csp sv1 sv0))
       (cra (cfp a0 fv0 fv1 sv2 csp sv0))
       (csp (cra a0 fv0 fv1 cfp sv1 sv0))
       (cfp (cra fv1 a0 y.6 x.5 tmp.14 tmp-ra.9 sv2 csp sv1 fv0))
       (y.6 (cfp fv0 fv1))
       (tmp.14 (fv0 fv1 x.5 cfp))
       (x.5 (fv0 fv1 cfp tmp.14))
       (tmp-ra.9 (a0 fv0 fv1 cfp))))
     (undead-out
      ((csp cra a0 fv0 fv1 cfp)
       (csp cra a0 fv0 fv1 cfp)
       (cra a0 fv0 fv1 cfp)
       (cra a0 fv0 fv1 cfp)
       (a0 fv0 fv1 cfp)
       (fv0 fv1 x.5 cfp)
       (((tmp.14 fv0 fv1 x.5 cfp) (fv0 fv1 x.5 cfp))
        ((fv0 fv1) (fv1 cfp) (cra cfp) (cra cfp))
        ((fv0 fv1 y.6 cfp)
         (((fv0 fv1) ((cfp a0) (cfp cra a0) (cfp cra a0)))
          (fv1 cfp)
          (cra cfp)
          (cra cfp))))))
     (call-undead (fv0 fv1))
     (locals (tmp-ra.9 x.5 tmp.14 y.6))
     (paraSize -1)
     (new-frames ()))
    (begin
      (set! sv0 cfp)
      (set! sv1 cra)
      (set! sv2 csp)
      (split)
      (set! t0 cra)
      (set! t1 a0)
      (if (begin (set! t0 0) (= t1 t0))
        (begin (set! a0 200) (set! cfp fv0) (set! cra fv1) (invoke cra cfp))
        (begin
          (set! t0 (+ t1 -1))
          (begin
            (return-point
             L.rpLabel.10
             (begin (set! a0 t0) (set! cra L.rpLabel.10) (jump L.odd?.1)))
            (set! cfp fv0)
            (set! cra fv1)
            (invoke cra cfp))))))
  (begin
    (set! sv0 cfp)
    (set! sv1 cra)
    (set! sv2 csp)
    (set! t0 cra)
    (split)
    (begin
      (return-point
       L.rpLabel.12
       (begin (set! a0 5) (set! cra L.rpLabel.12) (jump L.even?.2)))
      (set! cfp fv0)
      (set! cra fv1)
      (invoke cra cfp)))))


(module+ test
  )