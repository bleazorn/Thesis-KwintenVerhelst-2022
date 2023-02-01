#lang racket

(require "common/info.rkt")
(require "common/fvar.rkt")
(provide replace-labels-to-got-sentry)


;
;(replace-label trg replace-labels)->tail?
;trg: label?
;replace-labels: list? '(label? ...)
(define (replace-label trg replace-labels)
  (let ([loc (second (assoc trg replace-labels))])
    `(begin
       (set! ct0 (cgp + ,(* loc (framesize))))
       (jump ct0 ct6))))

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
  `(begin  (set! cs2 pcc)
           (set! cs2 0)
           (set! cs1 (cs2 - 0))
          ,(replace-tail t replace-labels)))

;
;(replace-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (replace-func f replace-labels)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i  ,(replace-tail-begin t replace-labels))]
    [_ #t]))


(define (replace-labels-to-got-sentry p)
  (match p
    [`(module ,i ,f ... ,t) (let ([replace-labels (getInfo i getGOTLabels)])
                              `(module ,i ,@(map (lambda (func) (replace-func func replace-labels)) f) ,(replace-tail-begin t replace-labels)))]
    [_ "replace locations failed"]))

#;(replace-labels-to-got '(module
                            ((got-labels ((L.swap.1 0))))
                          (define L.swap.1
                            ()
                            (begin
                              (set! fv4 cra)
                              (set! fv0 fv0)
                              (set! fv0 fv1)
                              (if (< fv0 fv0)
                                  (begin
                                    (set! a0 fv0)
                                    (begin
                                      ((set! a0 0)
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
                                       (set! t6 0))
                                      (invoke cra cfp)))
                                  (begin
                                    (begin
                                      (begin
                                        (set! fv2 cra)
                                        (setLinear! fv3 cfp)
                                        (split csp csp cfp 16384)
                                        (return-point L.rpLabel.6
                                                      (begin
                                                        (set! fv6 fv0)
                                                        (set! fv5 fv0)
                                                        (set! cra L.rpLabel.6)
                                                        (begin
                                                          (seal cra cfp 152678)
                                                          (begin
                                                            ((set! a0 0)
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
                                                             (set! t6 0))
                                                            (jump-call L.swap.1)))))
                                        (set! cfp ct6)
                                        (splice csp csp cfp 16384)
                                        (set! cra fv2)
                                        (setLinear! cfp fv3))
                                      (set! fv0 a0))
                                    (begin
                                      (set! a0 fv0)
                                      (begin
                                        ((set! a0 0)
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
                                         (set! t6 0))
                                        (invoke cra cfp)))))))
                          (begin
                            (set! fv2 cra)
                            (begin
                              (begin
                                (set! fv0 cra)
                                (setLinear! fv1 cfp)
                                (split csp csp cfp 16384)
                                (return-point L.rpLabel.10
                                              (begin
                                                (set! fv4 2)
                                                (set! fv3 1)
                                                (set! cra L.rpLabel.10)
                                                (begin
                                                  (seal cra cfp 169232)
                                                  (begin
                                                    ((set! a0 0)
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
                                                     (set! t6 0))
                                                    (jump-call L.swap.1)))))
                                (set! cfp ct6)
                                (splice csp csp cfp 16384)
                                (set! cra fv0)
                                (setLinear! cfp fv1))
                              (begin
                                ((set! a0 0)
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
                                 (set! t6 0))
                                (invoke cra cfp))))))
