#lang racket

(require "common/info.rkt")
(require "common/fvar.rkt"
         "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt"
         "langs/nested-asm-lang-fvars.rkt")
(provide replace-call-got-seal)


;
;(replace-label trg replace-labels)->tail?
;trg: label?
;replace-labels: list? '(label? ...)
(define (replace-label trg replace-labels)
  (let ([loc (* 2 (second (assoc trg replace-labels)))])
    `(begin
       (set! ,(current-invoke-jump-register) ,(newGvar loc))
       (set! ,(current-invoke-data-register) ,(newGvar (add1 loc)))
       (invoke ,(current-invoke-jump-register) ,(current-invoke-data-register)))))

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
;(replace-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (replace-func f replace-labels)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i  (begin (set! ,(current-seal-got-call-register) ,(current-invoke-data-register))
                                               ,(replace-tail t replace-labels)))]
    [_ #t]))


(define/contract (replace-call-got-seal p) (-> nested-asm-lang-jumps? nested-asm-lang-fvars?)
  (match p
    [`(module ,i ,f ... ,t) (let ([replace-labels (getInfo i getGOTLabels)])
                              `(module ,i ,@(map (lambda (func) (replace-func func replace-labels)) f) (begin (set! ,(current-seal-got-call-register) ,(current-invoke-data-register))
                                                                                                                 ,(replace-tail t replace-labels))))]
    [_ "replace locations failed"]))


#;(replace-call-got-seal '(module ((got-labels ((L.swap.1 0)))
            (actual-frame-size 16384)
            (frameSize 3)
            (assignment ())
            (conflicts
             ((cs1 (cra))
              (cra (cfp a0 cs1 a1))
              (a1 (cra cfp a0))
              (cfp (cra a1 a0))
              (a0 (cra a1 cfp))))
            (undead-out
             (((cra cfp a0 cs1)
               ((cfp a0) (cfp a0 a1) (cfp cra a0 a1) (cfp cra a0 a1)))
              (cfp a0 cs1)))
            (call-undead ())
            (locals ())
            (allocatedFvars (fv0 fv1 fv2))
            (new-frames ())
            (paramSize 0))
  (define L.swap.1
    ((actual-frame-size 16384)
     (frameSize 3)
     (assignment ((z.4 t0) (y.3 t0) (x.2 t1)))
     (conflicts
      ((cs1 (cra a0 z.4 x.2 y.3 a1))
       (cfp (cra a0 z.4 a1 x.2 y.3))
       (cra (cfp a0 cs1 z.4 a1 x.2 y.3))
       (a1 (cra cfp a0 x.2 cs1))
       (a0 (cra cfp cs1 a1 x.2))
       (z.4 (cra cfp cs1))
       (y.3 (x.2 cfp cra cs1))
       (x.2 (cfp a0 y.3 cra cs1 a1))))
     (undead-out
      ((a1 x.2 cra cfp cs1)
       (x.2 cra cfp cs1 y.3)
       ((x.2 cra cfp cs1 y.3)
        ((cra cfp a0 cs1) (cfp a0 cs1))
        ((((a0 cra cfp cs1)
           ((x.2 cfp a0) (cfp a0 a1) (cfp cra a0 a1) (cfp cra a0 a1)))
          (z.4 cra cfp cs1))
         ((cra cfp a0 cs1) (cfp a0 cs1))))))
     (call-undead ())
     (locals (x.2 y.3 z.4))
     (allocatedFvars (fv0 fv1 fv2))
     (new-frames ())
     (paramSize 0))
    (begin
      (set! t1 a0)
      (set! t0 a1)
      (if (< t0 t1)
        (begin (set! a0 t1) (begin (set! ct6 cfp) (invoke cra ct6)))
        (begin
          (begin
            (begin
              (set! fv0 cra)
              (setLinear! fv1 cfp)
              (set! fv2 cs1)
              (begin
                (return-point
                 L.rpLabel.5
                 (begin
                   (set! a0 t0)
                   (set! a1 t1)
                   (set! cra L.rpLabel.5)
                   (begin
                     (split csp csp cfp 16384)
                     (seal cra cfp 78)
                     (jump-call L.swap.1))))
                (set! cfp ct6))
              (splice csp csp cfp 16384)
              (set! cs1 fv2)
              (setLinear! cfp fv1)
              (set! cra fv0))
            (set! t0 a0))
          (begin (set! a0 t0) (begin (set! ct6 cfp) (invoke cra ct6)))))))
  (begin
    (begin
      (set! fv0 cra)
      (setLinear! fv1 cfp)
      (set! fv2 cs1)
      (begin
        (return-point
         L.rpLabel.6
         (begin
           (set! a0 1)
           (set! a1 2)
           (set! cra L.rpLabel.6)
           (begin
             (split csp csp cfp 16384)
             (seal cra cfp 49)
             (jump-call L.swap.1))))
        (set! cfp ct6))
      (splice csp csp cfp 16384)
      (set! cs1 fv2)
      (setLinear! cfp fv1)
      (set! cra fv0))
    (begin (set! ct6 cfp) (invoke cra ct6)))))
