#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt")
(provide secure-cheri-linkage)

(module+ test
  (require rackunit))

(define (secure-return-point e parasize)
  (match e
    [`(return-point ,l ,t) `(begin
                              (set! ,(newFvar parasize) ,(current-return-address-register))
                              (set! ,(newFvar (add1 parasize)) ,(current-return-sealed-register))
                              (return-point ,l ,(secure-tail t parasize))
                              (set! ,(current-return-sealed-register)  ,(newFvar (add1 parasize)))
                              (set! ,(current-return-address-register) ,(newFvar parasize)))]
    [_ (error (format "secure-cheri-linkage:  Failed match.\n No valid return-point: ~a" e))]))

;
;(secure-pred p)->pred?
;p: pred?
(define (secure-pred p parasize)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (secure-effect eff parasize)) e) ,(secure-pred pred parasize))]
    [`(if ,p1 ,p2 ,p3) `(if ,(secure-pred p1 parasize) ,(secure-pred p2 parasize) ,(secure-pred p3 parasize))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(secure-pred pred parasize))]
    [_ (error (format "secure-cheri-linkage:  Failed match.\n No valid pred: ~a" p))]))

;
;(secure-effect e)->effect?
;e->effect?
(define (secure-effect e parasize)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (secure-effect eff parasize)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(secure-pred p parasize) ,(secure-effect e1 parasize) ,(secure-effect e2 parasize))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(return-point ,l ,t) (secure-return-point e parasize)]
    [_ (error (format "secure-cheri-linkage:  Failed match.\n No valid effect: ~a" e))]))

;
;(secure-tail t)->tail?
;t: tail?
(define (secure-tail t parasize)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (secure-effect eff parasize)) e) ,(secure-tail tail parasize))]
    [`(if ,p ,t1 ,t2) `(if ,(secure-pred p parasize) ,(secure-tail t1 parasize) ,(secure-tail t2 parasize))]
    [`(jump-call ,trg) `(jump-call ,trg)]
    [`(jump-return ,trg) `(jump-return ,trg)]
    [_ (error (format "secure-cheri-linkage:  Failed match.\n No valid tail: ~a" t))]))

;
;(secure-info i t)->tail?
;t: tail?
;i: info?
(define (secure-info i t)
  (let ([parasize (getInfo i getParamSize)])
    (secure-tail t parasize)))

;
;(secure-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (secure-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(secure-info i t))]
    [_ (error (format "secure-cheri-linkage:  Failed match.\n No valid function: ~a" f))]))

(define/contract (secure-cheri-linkage p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) (let ([info (addInfo '() (setFrameSize (cons `(main ,(getInfo i getFrameSize))
                                                                         (map list
                                                                              (map second f)
                                                                              (map (lambda (x) (getInfo (third x) getFrameSize)) f)))))])
                              `(module ,info ,@(map secure-func f) ,(secure-info i t)))]))

(module+ test
  ;secure-return-point
  ;succes
  (check-equal? (secure-return-point '(return-point
                                       L.rpLabel.1
                                       (begin
                                         (set! a1 2)
                                         (set! a2 1)
                                         (set! cra L.rpLabel.1)
                                         (jump-call L.swap.1)))
                                     0)
                '(begin
                   (set! fv0 cra)
                   (set! fv1 cfp)
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 2)
                      (set! a2 1)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1)))
                   (set! cfp fv1)
                   (set! cra fv0))
                "secure-return-point: succes-01: simple return-point para-size 0")
  (check-equal? (secure-return-point '(return-point
                                       L.rpLabel.1
                                       (begin
                                         (set! a1 2)
                                         (set! a2 1)
                                         (set! cra L.rpLabel.1)
                                         (jump-call L.swap.1)))
                                     2)
                '(begin
                   (set! fv2 cra)
                   (set! fv3 cfp)
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 2)
                      (set! a2 1)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1)))
                   (set! cfp fv3)
                   (set! cra fv2))
                "secure-return-point: succes-02: simple return-point para-size 2")
  ;failure
  (check-exn exn:fail? (thunk (secure-return-point '+ 0)) "secure-return-point: failure-01: no return point")
  ;secure-pred
  ;succes
  (check-equal? (secure-pred '(true)
                             0)
                '(true)
                "secure-pred: succes-01: true")
  (check-equal? (secure-pred '(false)
                             0)
                '(false)
                "secure-pred: succes-02: false")
  (check-equal? (secure-pred '(= t0 t1)
                             0)
                '(= t0 t1)
                "secure-pred: succes-03: relop")

  (check-equal? (secure-pred '(begin (set! t1 5) (true))
                             0)
                '(begin (set! t1 5) (true))
                "secure-pred: succes-04: begin")
  (check-equal? (secure-pred '(if (= t0 t1) (true) (false))
                             0)
                '(if (= t0 t1) (true) (false))
                "secure-pred: succes-05: if")
  (check-equal? (secure-pred '(not (= t1 t0))
                             0)
                '(not (= t1 t0))
                "secure-pred: succes-06: not")
  ;failure
  (check-exn exn:fail? (thunk (secure-pred '+ 0)) "secure-pred: failure-01: wrong pred")
  ;secure-effect
  ;succes
  (check-equal? (secure-effect '(set! t0 (+ t1 t2))
                               0)
                '(set! t0 (+ t1 t2))
                "secure-effect: succes-01: binop")
  (check-equal? (secure-effect '(set! t0 5)
                               0)
                '(set! t0 5)
                "secure-effect: succes-02: set")
  (check-equal? (secure-effect '(begin (set! t0 5) (set! cfp cra))
                               0)
                '(begin (set! t0 5) (set! cfp cra))
                "secure-effect: succes-03: begin")
  (check-equal? (secure-effect '(if (true) (set! t0 5) (set! t0 6))
                               0)
                '(if (true) (set! t0 5) (set! t0 6))
                "secure-effect: succes-04: if")
  (check-equal? (secure-effect '(return-point
                                 L.rpLabel.1
                                 (begin
                                   (set! a1 2)
                                   (set! a2 1)
                                   (set! cra L.rpLabel.1)
                                   (jump-call L.swap.1)))
                               0)
                '(begin
                   (set! fv0 cra)
                   (set! fv1 cfp)
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 2)
                      (set! a2 1)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1)))
                   (set! cfp fv1)
                   (set! cra fv0))
                "secure-effect: succes-05: return point")
  ;failure
  (check-exn exn:fail? (thunk (secure-effect '+ 0)) "secure-effect: failure-01: wrong effect")
  ;secure-tail
  ;succes
  (check-equal? (secure-tail '(jump-call L.swap.1)
                             0)
                '(jump-call L.swap.1)
                "secure-tail: succes-01: jump call")
  (check-equal? (secure-tail '(jump-return cra)
                             0)
                '(jump-return cra)
                "secure-tail: succes-02:jump return")
  (check-equal? (secure-tail '(begin (set! cra fv0) (jump-return cra))
                             0)
                '(begin (set! cra fv0) (jump-return cra))
                "secure-tail: succes-03: begin")
  (check-equal? (secure-tail '(if (= t0 t1) (jump-return cra) (jump-call L.swap.1))
                             0)
                '(if (= t0 t1) (jump-return cra) (jump-call L.swap.1))
                "secure-tail: succes-04: if")
  ;failure
  (check-exn exn:fail? (thunk (secure-tail '+ 0)) "secure-tail: failure-01: wrong tail")
  ;secure-func
  ;succes
  (check-equal? (secure-func '(define L.odd.1
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
                       (jump-return cra))))
                "secure-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (secure-func '(defiene L.odd.1
                                              ((frameSize 8) (paramSize 0))
                                              (begin
                                                (set! cra cra)
                                                (begin
                                                  (set! t0 a1)
                                                  (set! t0 a2)
                                                  (set! t0 a3)
                                                  (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                  (jump-return cra))))))
             "secure-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (secure-func '(define 
                                              ((frameSize 8) (paramSize 0))
                                              (begin
                                                (set! cra cra)
                                                (begin
                                                  (set! t0 a1)
                                                  (set! t0 a2)
                                                  (set! t0 a3)
                                                  (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                  (jump-return cra))))))
             "secure-func: failure-02: no name")
  (check-exn exn:fail? (thunk (secure-func '(define L.odd.1
                                              (begin
                                                (set! cra cra)
                                                (begin
                                                  (set! t0 a1)
                                                  (set! t0 a2)
                                                  (set! t0 a3)
                                                  (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                  (jump-return cra))))))
             "secure-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (secure-func '(define L.odd.1
                                              ((frameSize 8) (paramSize 0)))))
             "secure-func: failure-04:  no tail")
  ;secure-stktokens
  )