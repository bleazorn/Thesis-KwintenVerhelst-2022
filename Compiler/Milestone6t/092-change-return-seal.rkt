#lang racket

(require "common/register.rkt"
         "common/fvar.rkt"
         "common/info.rkt"
         "langs/nested-asm-lang-jumps.rkt")
(provide change-return-seal)

(module+ test
  (require rackunit))

;
;(change-pred p)->pred?
;p: pred?
(define (change-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map change-effect e) ,(change-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(change-pred p1) ,(change-pred p2) ,(change-pred p3))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(change-pred pred))]
    [_ (error (format "change-return-seal:  Failed match.\n No valid pred: ~a" p))]))

;
;(change-effect e)->effect?
;e->effect?
(define (change-effect e)
  (match e
    [`(begin ,e ...) `(begin ,@(map change-effect e))]
    [`(if ,p ,e1 ,e2) `(if ,(change-pred p) ,(change-effect e1) ,(change-effect e2))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(return-point ,l ,t) `(begin (return-point ,l ,(change-tail t))
                                   (set! ,(current-return-unsealed-register) ,(current-invoke-data-register)))]
    [`(set-addr! ,a ,b) `(set-addr! ,a ,b)]
    [_ (error (format "change-return-seal:  Failed match.\n No valid effect: ~a" e))]))

;
;(change-tail t)->tail?
;t: tail?
(define (change-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map change-effect e) ,(change-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(change-pred p) ,(change-tail t1) ,(change-tail t2))]
    [`(jump-call ,trg) `(jump-call ,trg)]
    [`(jump-return ,trg) `(begin (set! ,(current-invoke-data-register) ,(current-return-sealed-register))
                                 (invoke ,(current-return-address-register) ,(current-invoke-data-register)))]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ (error (format "change-return-seal:  Failed match.\n No valid tail: ~a" t))]))

;
;(change-info i t)->tail?
;t: tail?
;i: info?
(define (change-info i t)
  (change-tail t))

;
;(change-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (change-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(change-info i t))]
    [_ (error (format "change-return-seal:  Failed match.\n No valid function: ~a" f))]))


(define/contract (change-return-seal p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map change-func f) ,(change-info i t))]
    [_ "replace locations failed"]))

(module+ test
  ;change-pred
  ;succes
  (check-equal? (change-pred '(true))
                '(true)
                "change-pred: succes-01: true")
  (check-equal? (change-pred '(false))
                '(false)
                "change-pred: succes-02: false")
  (check-equal? (change-pred '(= t0 t1))
                '(= t0 t1)
                "change-pred: succes-03: relop")

  (check-equal? (change-pred '(begin (set! t1 5) (true)))
                '(begin (set! t1 5) (true))
                "change-pred: succes-04: begin")
  (check-equal? (change-pred '(if (= t0 t1) (true) (false)))
                '(if (= t0 t1) (true) (false))
                "change-pred: succes-05: if")
  (check-equal? (change-pred '(not (= t1 t0)))
                '(not (= t1 t0))
                "change-pred: succes-06: not")
  ;failure
  (check-exn exn:fail? (thunk (change-pred '+)) "change-pred: failure-01: wrong pred")
  ;change-effect
  ;succes
  (check-equal? (change-effect '(set! t0 (+ t1 t2)))
                '(set! t0 (+ t1 t2))
                "change-effect: succes-01: binop")
  (check-equal? (change-effect '(set! t0 5))
                '(set! t0 5)
                "change-effect: succes-02: set")
  (check-equal? (change-effect '(begin (set! t0 5) (set! cfp cra)))
                '(begin (set! t0 5) (set! cfp cra))
                "change-effect: succes-03: begin")
  (check-equal? (change-effect '(if (true) (set! t0 5) (set! t0 6)))
                '(if (true) (set! t0 5) (set! t0 6))
                "change-effect: succes-04: if")
  (check-equal? (change-effect '(return-point
                                 L.rpLabel.1
                                 (begin
                                   (set! a1 2)
                                   (set! a2 1)
                                   (set! cra L.rpLabel.1)
                                   (jump-call L.swap.1))))
                '(begin
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 2)
                      (set! a2 1)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1)))
                   (set! cfp ct6))
                "change-effect: succes-05: return point")
  (check-equal? (change-effect '(setLinear! csp cfp))
                '(setLinear! csp cfp)
                "change-effect: succes-06: set-linear")
  (check-equal? (change-effect '(seal cra cfp 15))
                '(seal cra cfp 15)
                "change-effect: succes-07: seal")
  (check-equal? (change-effect '(unseal cra cfp 15))
                '(unseal cra cfp 15)
                "change-effect: succes-08: unseal")
  (check-equal? (change-effect '(split csp csp cfp 64))
                '(split csp csp cfp 64)
                "change-effect: succes-09: split")
  (check-equal? (change-effect '(splice csp csp cfp 64))
                '(splice csp csp cfp 64)
                "change-effect: succes-10: splice")
  (check-equal? (change-effect '(set-addr! cra 5))
                '(set-addr! cra 5)
                "change-effect: succes-11: set-addr")
  ;failure
  (check-exn exn:fail? (thunk (change-effect '+)) "change-effect: failure-01: wrong effect")
  ;change-tail
  ;succes
  (check-equal? (change-tail '(jump-call L.swap.1))
                '(jump-call L.swap.1)
                "change-tail: succes-01: jump call")
  (check-equal? (change-tail '(jump-return cra))
                '(begin (set! ct6 cfp) (invoke cra ct6))
                "change-tail: succes-02:jump return")
  (check-equal? (change-tail '(begin (set! cra fv0) (jump-return cra)))
                '(begin (set! cra fv0) (begin (set! ct6 cfp) (invoke cra ct6)))
                "change-tail: succes-03: begin")
  (check-equal? (change-tail '(if (= t0 t1) (jump-return cra) (jump-call L.swap.1)))
                '(if (= t0 t1) (begin (set! ct6 cfp) (invoke cra ct6)) (jump-call L.swap.1))
                "change-tail: succes-04: if")
  (check-equal? (change-tail '(invoke cra cfp))
                '(invoke cra cfp)
                "change-tail: succes-05: invoke")
  ;failure
  (check-exn exn:fail? (thunk (change-tail '+)) "change-tail: failure-01: wrong tail")
  ;change-func
  ;succes
  (check-equal? (change-func '(define L.odd.1
                                ((frameSize 8) (paramSize 0))
                                (begin
                                  (set! cra cra)
                                  (begin
                                    (set! t0 a1)
                                    (set! t0 a2)
                                    (set! t0 a3)
                                    (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                    (jump-return cra)))))
                '(define L.odd.1
                   ((frameSize 8) (paramSize 0))
                   (begin
                     (set! cra cra)
                     (begin
                       (set! t0 a1)
                       (set! t0 a2)
                       (set! t0 a3)
                       (begin (set! t0 5) (set! t0 (+ t0 t0)))
                       (begin (set! ct6 cfp) (invoke cra ct6)))))
                "change-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (change-func '(defiene L.odd.1
                                              ((frameSize 8) (paramSize 0))
                                              (begin
                                                (set! cra cra)
                                                (begin
                                                  (set! t0 a1)
                                                  (set! t0 a2)
                                                  (set! t0 a3)
                                                  (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                  (jump-return cra))))))
             "change-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (change-func '(define 
                                              ((frameSize 8) (paramSize 0))
                                              (begin
                                                (set! cra cra)
                                                (begin
                                                  (set! t0 a1)
                                                  (set! t0 a2)
                                                  (set! t0 a3)
                                                  (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                  (jump-return cra))))))
             "change-func: failure-02: no name")
  (check-exn exn:fail? (thunk (change-func '(define L.odd.1
                                              (begin
                                                (set! cra cra)
                                                (begin
                                                  (set! t0 a1)
                                                  (set! t0 a2)
                                                  (set! t0 a3)
                                                  (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                  (jump-return cra))))))
             "change-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (change-func '(define L.odd.1
                                              ((frameSize 8) (paramSize 0)))))
             "change-func: failure-04:  no tail")
  ;change-stktokens
  )