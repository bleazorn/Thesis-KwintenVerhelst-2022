#lang racket

(require "common/info.rkt")
(require "common/fvar.rkt"
         "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt"
         "langs/nested-asm-lang-fvars.rkt")
(provide replace-labels-to-got)


;
;(replace-label trg replace-labels)->tail?
;trg: label?
;replace-labels: list? '(label? ...)
(define (replace-label trg replace-labels)
  (let ([loc (second (assoc trg replace-labels))])
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
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))

;
;(replace-tail t assign)->tail?
;t: tail?
;replace-labels: list? '(label? ...)
(define (replace-tail-begin t replace-labels)
  `(begin (set! cs1 ct6)
          ,(replace-tail t replace-labels)))

     
;
;(replace-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (replace-func f replace-labels)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i  ,(replace-tail-begin t replace-labels))]
    [_ #t]))


(define/contract (replace-labels-to-got p) (-> nested-asm-lang-jumps? nested-asm-lang-fvars?)
  (match p
    [`(module ,i ,f ... ,t) (let ([replace-labels (getInfo i getGOTLabels)])
                              `(module ,i ,@(map (lambda (func) (replace-func func replace-labels)) f) ,(replace-tail-begin t replace-labels)))]
    [_ "replace locations failed"]))

(define/contract (re p) (-> nested-asm-lang-fvars? nested-asm-lang-fvars?)
  p)




#;(replace-labels-to-got '(module ((got-labels ((L.odd?.1 0) (L.even?.2 2))))
  (define L.odd?.1
    ()
    (begin
      (set! fv3 cra)
      (set! fv0 fv0)
      (if (begin (set! fv0 0) (= fv0 fv0))
        (begin
          (set! a0 150)
          (begin
            (set! a1 0)
            (set! a2 0)
            (set! a3 0)
            (set! a4 0)
            (set! t0 0)
            (set! t1 0)
            (set! t2 0)
            (set! t3 0)
            (set! t4 0)
            (set! t5 0)
            (set! t6 0)
            (invoke cra cfp)))
        (begin
          (set! fv0 (+ fv0 -1))
          (begin
            (begin
              (set! fv1 cra)
              (setLinear! fv2 cfp)
              (split csp csp cfp 16384)
              (return-point
               L.rpLabel.8
               (begin
                 (set! fv4 fv0)
                 (set! cra L.rpLabel.8)
                 (begin
                   (seal cra cfp 175980)
                   (begin
                     (set! t0 0)
                     (set! t1 0)
                     (set! t2 0)
                     (set! t3 0)
                     (set! t4 0)
                     (set! t5 0)
                     (set! t6 0)
                     (jump-call L.even?.2)))))
              (set! cfp ct6)
              (splice csp csp cfp 16384)
              (set! cra fv1)
              (setLinear! cfp fv2))
            (begin
              (set! a1 0)
              (set! a2 0)
              (set! a3 0)
              (set! a4 0)
              (set! t0 0)
              (set! t1 0)
              (set! t2 0)
              (set! t3 0)
              (set! t4 0)
              (set! t5 0)
              (set! t6 0)
              (invoke cra cfp)))))))
  (define L.even?.2
    ()
    (begin
      (set! fv3 cra)
      (set! fv0 fv0)
      (if (begin (set! fv0 0) (= fv0 fv0))
        (begin
          (set! a0 200)
          (begin
            (set! a1 0)
            (set! a2 0)
            (set! a3 0)
            (set! a4 0)
            (set! t0 0)
            (set! t1 0)
            (set! t2 0)
            (set! t3 0)
            (set! t4 0)
            (set! t5 0)
            (set! t6 0)
            (invoke cra cfp)))
        (begin
          (set! fv0 (+ fv0 -1))
          (begin
            (begin
              (set! fv1 cra)
              (setLinear! fv2 cfp)
              (split csp csp cfp 16384)
              (return-point
               L.rpLabel.11
               (begin
                 (set! fv4 fv0)
                 (set! cra L.rpLabel.11)
                 (begin
                   (seal cra cfp 160401)
                   (begin
                     (set! t0 0)
                     (set! t1 0)
                     (set! t2 0)
                     (set! t3 0)
                     (set! t4 0)
                     (set! t5 0)
                     (set! t6 0)
                     (jump-call L.odd?.1)))))
              (set! cfp ct6)
              (splice csp csp cfp 16384)
              (set! cra fv1)
              (setLinear! cfp fv2))
            (begin
              (set! a1 0)
              (set! a2 0)
              (set! a3 0)
              (set! a4 0)
              (set! t0 0)
              (set! t1 0)
              (set! t2 0)
              (set! t3 0)
              (set! t4 0)
              (set! t5 0)
              (set! t6 0)
              (invoke cra cfp)))))))
  (begin
    (set! fv2 cra)
    (begin
      (begin
        (set! fv0 cra)
        (setLinear! fv1 cfp)
        (split csp csp cfp 16384)
        (return-point
         L.rpLabel.14
         (begin
           (set! fv3 5)
           (set! cra L.rpLabel.14)
           (begin
             (seal cra cfp 223722)
             (begin
               (set! t0 0)
               (set! t1 0)
               (set! t2 0)
               (set! t3 0)
               (set! t4 0)
               (set! t5 0)
               (set! t6 0)
               (jump-call L.even?.2)))))
        (set! cfp ct6)
        (splice csp csp cfp 16384)
        (set! cra fv0)
        (setLinear! cfp fv1))
      (begin
        (set! a1 0)
        (set! a2 0)
        (set! a3 0)
        (set! a4 0)
        (set! t0 0)
        (set! t1 0)
        (set! t2 0)
        (set! t3 0)
        (set! t4 0)
        (set! t5 0)
        (set! t6 0)
        (invoke cra cfp))))))
