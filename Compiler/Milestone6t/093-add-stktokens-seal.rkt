#lang racket

(require "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt")

(provide add-stktokens-seal)

(module+ test
  (require rackunit))

(define (add-tail t)
  `(begin (set! ,(current-seal-location-register) ,(current-invoke-data-register))
          ,t))
;
;(change-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (add-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(add-tail t))]
    [_ (error (format "add-stktokens-seal:  Failed match.\n No valid function: ~a" f))]))


(define/contract (add-stktokens-seal p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map add-func f) ,(add-tail t))]))

(module+ test
  ;add-tail
  ;succes
  (check-equal? (add-tail '(jump-call L.swap.1))
                '(begin (set! cs1 ct6) (jump-call L.swap.1))
                "add-tail: succes-01: jump call")
  (check-equal? (add-tail '(jump-return cra))
                '(begin (set! cs1 ct6) (jump-return cra))
                "add-tail: succes-02:jump return")
  (check-equal? (add-tail '(begin (set! cra fv0) (jump-return cra)))
                '(begin (set! cs1 ct6) (begin (set! cra fv0) (jump-return cra)))
                "add-tail: succes-03: begin")
  (check-equal? (add-tail '(if (= t0 t1) (jump-return cra) (jump-call L.swap.1)))
                '(begin (set! cs1 ct6) (if (= t0 t1) (jump-return cra) (jump-call L.swap.1)))
                "add-tail: succes-04: if")
  (check-equal? (add-tail '(invoke cra cfp))
                '(begin (set! cs1 ct6) (invoke cra cfp))
                "add-tail: succes-05: invoke")
  ;add-func
  ;succes
  (check-equal? (add-func '(define L.odd.1
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
                     (set! cs1 ct6)
                     (begin
                       (set! cra cra)
                       (begin
                         (set! t0 a1)
                         (set! t0 a2)
                         (set! t0 a3)
                         (begin (set! t0 5) (set! t0 (+ t0 t0)))
                         (jump-return cra)))))
                "add-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (add-func '(defiene L.odd.1
                                           ((frameSize 8) (paramSize 0))
                                           (begin
                                             (set! cra cra)
                                             (begin
                                               (set! t0 a1)
                                               (set! t0 a2)
                                               (set! t0 a3)
                                               (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                               (jump-return cra))))))
             "add-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (add-func '(define 
                                           ((frameSize 8) (paramSize 0))
                                           (begin
                                             (set! cra cra)
                                             (begin
                                               (set! t0 a1)
                                               (set! t0 a2)
                                               (set! t0 a3)
                                               (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                               (jump-return cra))))))
             "add-func: failure-02: no name")
  (check-exn exn:fail? (thunk (add-func '(define L.odd.1
                                           (begin
                                             (set! cra cra)
                                             (begin
                                               (set! t0 a1)
                                               (set! t0 a2)
                                               (set! t0 a3)
                                               (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                               (jump-return cra))))))
             "add-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (add-func '(define L.odd.1
                                           ((frameSize 8) (paramSize 0)))))
             "add-func: failure-04:  no tail")
  ;add-stktokens
  )