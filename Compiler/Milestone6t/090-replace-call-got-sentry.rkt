#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt"
         "langs/nested-asm-lang-fvars.rkt")
(provide replace-call-got-sentry)

(module+ test
  (require rackunit))

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
    [_ (error (format "replace-call-got-sentry:  Failed match.\n No valid pred: ~a" p))]))

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
    [`(set-addr! ,a ,b) `(set-addr! ,a ,b)]
    [_ (error (format "replace-call-got-sentry:  Failed match.\n No valid effect: ~a" e))]))

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
    [_ (error (format "replace-call-got-sentry:  Failed match.\n No valid tail: ~a" t))]))


;
;(replace-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (replace-func f replace-labels)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i  ,(replace-tail t replace-labels))]
    [_ (error (format "replace-call-got-sentry:  Failed match.\n No valid function: ~a" f))]))


(define/contract (replace-call-got-sentry p) (-> nested-asm-lang-jumps? nested-asm-lang-fvars?)
  (match p
    [`(module ,i ,f ... ,t) (let ([replace-labels (getInfo i getGOTLabels)])
                              `(module ,i ,@(map (lambda (func) (replace-func func replace-labels)) f) ,(replace-tail t replace-labels)))]))

(module+ test
  ;replace-label
  (check-equal? (replace-label 'L.even?.1 '((L.even?.1 0) (L.odd?.2 1)))
                '(begin (set! ct0 gv0) (jump ct0))
                "replace-pred: succes-01: at 0")
  (check-equal? (replace-label 'L.odd?.2 '((L.even?.1 0) (L.odd?.2 5)))
                '(begin (set! ct0 gv5) (jump ct0))
                "replace-pred: succes-01: more then zero")
  ;failure
  (check-exn exn:fail? (thunk (replace-label 'L.swap.1'((L.even?.1 0) (L.odd?.2 1)))) "replace-pred: failure-01: no label in list")
  ;replace-pred
  ;succes
  (check-equal? (replace-pred '(true) '((L.even?.1 0) (L.odd?.2 1)))
                '(true)
                "replace-pred: succes-01: true")
  (check-equal? (replace-pred '(false) '((L.even?.1 0) (L.odd?.2 1)))
                '(false)
                "replace-pred: succes-02: false")
  (check-equal? (replace-pred '(= t0 t1) '((L.even?.1 0) (L.odd?.2 1)))
                '(= t0 t1)
                "replace-pred: succes-03: relop")

  (check-equal? (replace-pred '(begin (set! t1 5) (true)) '((L.even?.1 0) (L.odd?.2 1)))
                '(begin (set! t1 5) (true))
                "replace-pred: succes-04: begin")
  (check-equal? (replace-pred '(if (= t0 t1) (true) (false)) '((L.even?.1 0) (L.odd?.2 1)))
                '(if (= t0 t1) (true) (false))
                "replace-pred: succes-05: if")
  (check-equal? (replace-pred '(not (= t1 t0)) '((L.even?.1 0) (L.odd?.2 1)))
                '(not (= t1 t0))
                "replace-pred: succes-06: not")
  ;failure
  (check-exn exn:fail? (thunk (replace-pred '+'((L.even?.1 0) (L.odd?.2 1)))) "replace-pred: failure-01: wrong pred")
  ;replace-effect
  ;succes
  (check-equal? (replace-effect '(set! t0 (+ t1 t2)) '((L.even?.1 0) (L.odd?.2 1))) 
                '(set! t0 (+ t1 t2))
                "replace-effect: succes-01: binop")
  (check-equal? (replace-effect '(set! t0 5) '((L.even?.1 0) (L.odd?.2 1)))
                '(set! t0 5)
                "replace-effect: succes-02: set")
  (check-equal? (replace-effect '(begin (set! t0 5) (set! cfp cra)) '((L.even?.1 0) (L.odd?.2 1)))
                '(begin (set! t0 5) (set! cfp cra))
                "replace-effect: succes-03: begin")
  (check-equal? (replace-effect '(if (true) (set! t0 5) (set! t0 6)) '((L.even?.1 0) (L.odd?.2 1)))
                '(if (true) (set! t0 5) (set! t0 6))
                "replace-effect: succes-04: if")
  (check-equal? (replace-effect '(return-point
                                  L.rpLabel.1
                                  (begin
                                    (set! a1 2)
                                    (set! a2 1)
                                    (set! cra L.rpLabel.1)
                                    (jump-call L.swap.1)))
                                '((L.even?.1 0) (L.odd?.2 1) (L.swap.1 2)))
                '(return-point
                  L.rpLabel.1
                  (begin
                    (set! a1 2)
                    (set! a2 1)
                    (set! cra L.rpLabel.1)
                    (begin (set! ct0 gv2) (jump ct0))))
                "replace-effect: succes-05: return point")
  (check-equal? (replace-effect '(setLinear! csp cfp) '((L.even?.1 0) (L.odd?.2 1)))
                '(setLinear! csp cfp)
                "replace-effect: succes-06: set-linear")
  (check-equal? (replace-effect '(seal cra cfp 15) '((L.even?.1 0) (L.odd?.2 1)))
                '(seal cra cfp 15)
                "replace-effect: succes-07: seal")
  (check-equal? (replace-effect '(unseal cra cfp 15) '((L.even?.1 0) (L.odd?.2 1)))
                '(unseal cra cfp 15)
                "replace-effect: succes-08: unseal")
  (check-equal? (replace-effect '(split csp csp cfp 64) '((L.even?.1 0) (L.odd?.2 1)))
                '(split csp csp cfp 64)
                "replace-effect: succes-09: split")
  (check-equal? (replace-effect '(splice csp csp cfp 64) '((L.even?.1 0) (L.odd?.2 1)))
                '(splice csp csp cfp 64)
                "replace-effect: succes-10: splice")
  (check-equal? (replace-effect '(set-addr! cra 5) '((L.even?.1 0) (L.odd?.2 1)))
                '(set-addr! cra 5)
                "replace-effect: succes-11: set-addr")
  ;failure
  (check-exn exn:fail? (thunk (replace-effect '+ '((L.even?.1 0) (L.odd?.2 1)))) "replace-effect: failure-01: wrong effect")
  ;replace-tail
  ;succes
  (check-equal? (replace-tail '(jump-call L.swap.1) '((L.even?.1 0) (L.odd?.2 1) (L.swap.1 2)))
                '(begin (set! ct0 gv2) (jump ct0))
                "replace-tail: succes-01: jump call")
  (check-equal? (replace-tail '(jump-return cra) '((L.even?.1 0) (L.odd?.2 1)))
                '(jump cra)
                "replace-tail: succes-02:jump return")
  (check-equal? (replace-tail '(begin (set! cra fv0) (jump-return cra)) '((L.even?.1 0) (L.odd?.2 1)))
                '(begin (set! cra fv0) (jump cra))
                "replace-tail: succes-03: begin")
  (check-equal? (replace-tail '(if (= t0 t1) (jump-return cra) (jump-call L.swap.1)) '((L.swap.1 0)))
                '(if (= t0 t1) (jump cra) (begin (set! ct0 gv0) (jump ct0)))
                "replace-tail: succes-04: if")
  (check-equal? (replace-tail '(invoke cra cfp) '((L.even?.1 0) (L.odd?.2 1)))
                '(invoke cra cfp)
                "replace-tail: succes-05: invoke")
  ;failure
  (check-exn exn:fail? (thunk (replace-tail '+ '((L.even?.1 0) (L.odd?.2 1)))) "replace-tail: failure-01: wrong tail")
  ;replace-func
  ;succes
  (check-equal? (replace-func '(define L.odd.1
                                 ((frameSize 8) (paramSize 0))
                                 (begin
                                   (set! cra cra)
                                   (begin
                                     (set! t0 a1)
                                     (set! t0 a2)
                                     (set! t0 a3)
                                     (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                     (jump-return cra))))
                              '((L.even?.1 0) (L.odd?.2 1) (L.swap.1 2)))
                '(define L.odd.1
                   ((frameSize 8) (paramSize 0))
                   (begin
                     (set! cra cra)
                     (begin
                       (set! t0 a1)
                       (set! t0 a2)
                       (set! t0 a3)
                       (begin (set! t0 5) (set! t0 (+ t0 t0)))
                       (jump cra))))
                "replace-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (replace-func '(defiene L.odd.1
                                               ((frameSize 8) (paramSize 0))
                                               (begin
                                                 (set! cra cra)
                                                 (begin
                                                   (set! t0 a1)
                                                   (set! t0 a2)
                                                   (set! t0 a3)
                                                   (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                   (jump-return cra))))))
             "replace-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (replace-func '(define 
                                               ((frameSize 8) (paramSize 0))
                                               (begin
                                                 (set! cra cra)
                                                 (begin
                                                   (set! t0 a1)
                                                   (set! t0 a2)
                                                   (set! t0 a3)
                                                   (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                   (jump-return cra))))))
             "replace-func: failure-02: no name")
  (check-exn exn:fail? (thunk (replace-func '(define L.odd.1
                                               (begin
                                                 (set! cra cra)
                                                 (begin
                                                   (set! t0 a1)
                                                   (set! t0 a2)
                                                   (set! t0 a3)
                                                   (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                   (jump-return cra))))))
             "replace-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (replace-func '(define L.odd.1
                                               ((frameSize 8) (paramSize 0)))))
             "replace-func: failure-04:  no tail")
  ;replace-stktokens
  )

