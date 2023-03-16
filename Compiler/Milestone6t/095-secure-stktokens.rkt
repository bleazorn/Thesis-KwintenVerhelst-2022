#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "common/assembly.rkt"
         "langs/nested-asm-lang-jumps.rkt")
(provide secure-stktokens)

(module+ test
  (require rackunit))


;
;(secure-return-point p assign)->effect?
;p: pred?
;frame-size, para-size: integer?
(define (secure-return-point e frame-size para-size)
  (match e
    [`(return-point ,l ,t) (let([token (random 0 seal-token-size)])
                             `(begin
                                (set! ,(newFvar para-size) ,(current-return-address-register))
                                (setLinear! ,(newFvar (add1 para-size)) ,(current-frame-base-pointer-register))
                                (set! ,(newFvar (add1 (add1 para-size))) ,(current-seal-location-register))
                                (return-point ,l ,(secure-tail t frame-size para-size token))
                                (splice ,(current-stack-base-pointer-register) ,(current-stack-base-pointer-register) ,(current-frame-base-pointer-register) ,frame-size)
                                (set! ,(current-seal-location-register) ,(newFvar (add1 (add1 para-size))))
                                (setLinear! ,(current-frame-base-pointer-register) ,(newFvar (add1 para-size)))
                                (set! ,(current-return-address-register) ,(newFvar para-size))))]
    [_ (error (format "secure-stktokens:  Failed match.\n No valid return-point: ~a" e))]))

;
;(secure-pred p assign)->pred?
;p: pred?
;frame-size, para-size: integer?
(define (secure-pred p frame-size para-size)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (secure-effect eff frame-size para-size)) e) ,(secure-pred pred frame-size para-size))]
    [`(if ,p1 ,p2 ,p3) `(if ,(secure-pred p1 frame-size para-size) ,(secure-pred p2 frame-size para-size) ,(secure-pred p3 frame-size para-size))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(secure-pred pred frame-size para-size))]
    [_ (error (format "secure-stktokens:  Failed match.\n No valid pred: ~a" p))]))

;
;(secure-effect e assign)->effect?
;e->effect?
;frame-size, para-size: integer?
(define (secure-effect e frame-size para-size)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (secure-effect eff frame-size para-size)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(secure-pred p frame-size para-size) ,(secure-effect e1 frame-size para-size) ,(secure-effect e2 frame-size para-size))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(return-point ,l ,t) (secure-return-point e frame-size para-size)]
    [_ (error (format "secure-stktokens:  Failed match.\n No valid effect: ~a" e))]))

;
;(secure-tail t assign)->tail?
;t: tail?
;frame-size, para-size: integer?
(define (secure-tail t frame-size para-size token)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (secure-effect eff frame-size para-size)) e) ,(secure-tail tail frame-size para-size token))]
    [`(if ,p ,t1 ,t2) `(if ,(secure-pred p frame-size para-size) ,(secure-tail t1 frame-size para-size token) ,(secure-tail t2 frame-size para-size token))]
    [`(jump-call ,trg) (let ([aux-register (car (current-auxiliary-registers))])
                         `(begin (split ,(current-stack-base-pointer-register) ,(current-stack-base-pointer-register) ,(current-frame-base-pointer-register) ,frame-size)
                                 (set! ,(current-seal-register) (,(current-seal-location-register) - ,seal-location))
                                 (seal cra cra ,(current-seal-register) ,token)
                                 (seal ,(current-frame-base-pointer-register) ,(current-frame-base-pointer-register) ,(current-seal-register) ,token)
                                 (jump-call ,trg)))]
    [`(jump-return ,trg) `(jump-return ,trg)]
    [_ (error (format "secure-stktokens:  Failed match.\n No valid tail: ~a" t))]))

;
;(secure-info t i)->tail?
;t: tail?
;i: info?
(define (secure-info t i)
  (let ([frame-size (cond [(> (* (getInfo i getFrameSize) (framesize)) 16384) (* (getInfo i getFrameSize) (framesize))]
                          [else 16384])]
        [para-size (getInfo i getParamSize)])
    (values (addInfo i (setActualFrameSize frame-size)) (secure-tail t frame-size para-size 0))))
      

;
;(secure-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (secure-func f)
  (match f
    [`(define ,l ,i ,t) (let-values ([(info tail) (secure-info t i)])
                          `(define ,l ,info ,tail))]
    [_ (error (format "secure-stktokens:  Failed match.\n No valid function: ~a" f))]))


(define/contract (secure-stktokens p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) (let-values ([(info tail) (secure-info t i)])
                              `(module ,info ,@(map secure-func f) ,tail))]))


(module+ test
  ;secure-return-point
  ;succes
  #;(check-equal? (secure-return-point '(return-point
                                       L.rpLabel.1
                                       (begin
                                         (set! a1 2)
                                         (set! a2 1)
                                         (set! cra L.rpLabel.1)
                                         (jump-call L.swap.1)))
                                     16384
                                     0)
                '(begin
                   (set! fv0 cra)
                   (setLinear! fv1 cfp)
                   (set! fv2 cs1)
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 2)
                      (set! a2 1)
                      (set! cra L.rpLabel.1)
                      (begin
                        (split cfp cfp cfp 16384)
                        (set! cs2 (cs1 - 0))
                        (seal cra cra cs2 10)
                        (seal cfp cfp cs2 10)
                        (jump-call L.swap.1))))
                   (splice cfp cfp cfp 16384)
                   (set! cs1 fv2)
                   (setLinear! cfp fv1)
                   (set! cra fv0))
                "secure-return-point: succes-01: simple return-point para-size 0")
  #;(check-equal? (secure-return-point '(return-point
                                       L.rpLabel.1
                                       (begin
                                         (set! a1 2)
                                         (set! a2 1)
                                         (set! cra L.rpLabel.1)
                                         (jump-call L.swap.1)))
                                     16384
                                     2)
                '(begin
                   (set! fv2 cra)
                   (setLinear! fv3 cfp)
                   (set! fv4 cs1)
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 2)
                      (set! a2 1)
                      (set! cra L.rpLabel.1)
                      (begin
                        (split cfp cfp cfp 16384)
                        (set! cs2 (cs1 - 0))
                        (seal cra cra cs2 55)
                        (seal cfp cfp cs2 55)
                        (jump-call L.swap.1))))
                   (splice cfp cfp cfp 16384)
                   (set! cs1 fv4)
                   (setLinear! cfp fv3)
                   (set! cra fv2))
                "secure-return-point: succes-02: simple return-point para-size 2")
  ;failure
  (check-exn exn:fail? (thunk (secure-return-point '+ 0 0)) "secure-return-point: failure-01: no return point")
  ;secure-pred
  ;succes
  (check-equal? (secure-pred '(true)
                             16384
                             0)
                '(true)
                "secure-pred: succes-01: true")
  (check-equal? (secure-pred '(false)
                             16384
                             0)
                '(false)
                "secure-pred: succes-02: false")
  (check-equal? (secure-pred '(= t0 t1)
                             16384
                             0)
                '(= t0 t1)
                "secure-pred: succes-03: relop")

  (check-equal? (secure-pred '(begin (set! t1 5) (true))
                             16384
                             0)
                '(begin (set! t1 5) (true))
                "secure-pred: succes-04: begin")
  (check-equal? (secure-pred '(if (= t0 t1) (true) (false))
                             16384
                             0)
                '(if (= t0 t1) (true) (false))
                "secure-pred: succes-05: if")
  (check-equal? (secure-pred '(not (= t1 t0))
                             16384
                             0)
                '(not (= t1 t0))
                "secure-pred: succes-06: not")
  ;failure
  (check-exn exn:fail? (thunk (secure-pred '+ 0 0)) "secure-pred: failure-01: wrong pred")
  ;secure-effect
  ;succes
  (check-equal? (secure-effect '(set! t0 (+ t1 t2))
                               16384
                               0)
                '(set! t0 (+ t1 t2))
                "secure-effect: succes-01: binop")
  (check-equal? (secure-effect '(set! t0 5)
                               16384
                               0)
                '(set! t0 5)
                "secure-effect: succes-02: set")
  (check-equal? (secure-effect '(begin (set! t0 5) (set! cfp cra))
                               16384
                               0)
                '(begin (set! t0 5) (set! cfp cra))
                "secure-effect: succes-03: begin")
  (check-equal? (secure-effect '(if (true) (set! t0 5) (set! t0 6))
                               16384
                               0)
                '(if (true) (set! t0 5) (set! t0 6))
                "secure-effect: succes-04: if")
  #;(check-equal? (secure-effect '(return-point
                                 L.rpLabel.1
                                 (begin
                                   (set! a1 2)
                                   (set! a2 1)
                                   (set! cra L.rpLabel.1)
                                   (jump-call L.swap.1)))
                               16384
                               0)
                '(begin
                   (set! fv0 cra)
                   (setLinear! fv1 cfp)
                   (set! fv2 cs1)
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 2)
                      (set! a2 1)
                      (set! cra L.rpLabel.1)
                      (begin
                        (split cfp cfp cfp 16384)
                        (set! cs2 (cs1 - 0))
                        (seal cra cra cs2 81)
                        (seal cfp cfp cs2 81)
                        (jump-call L.swap.1))))
                   (splice cfp cfp cfp 16384)
                   (set! cs1 fv2)
                   (setLinear! cfp fv1)
                   (set! cra fv0))
                "secure-effect: succes-05: return point")
  ;failure
  (check-exn exn:fail? (thunk (secure-effect '+ 0 0)) "secure-effect: failure-01: wrong effect")
  ;secure-tail
  ;succes
  (check-equal? (secure-tail '(jump-call L.swap.1)
                             16384
                             0
                             15)
                '(begin
                   (split cfp cfp cfp 16384)
                   (set! cs2 (cs1 - 0))
                   (seal cra cra cs2 15)
                   (seal cfp cfp cs2 15)
                   (jump-call L.swap.1))
                "secure-tail: succes-01: jump call")
  (check-equal? (secure-tail '(jump-return cra)
                             16384
                             0
                             15)
                '(jump-return cra)
                "secure-tail: succes-02:jump return")
  (check-equal? (secure-tail '(begin (set! cra fv0) (jump-return cra))
                             16384
                             0
                             15)
                '(begin (set! cra fv0) (jump-return cra))
                "secure-tail: succes-03: begin")
  (check-equal? (secure-tail '(if (= t0 t1) (jump-return cra) (jump-call L.swap.1))
                             16384
                             0
                             15)
                '(if (= t0 t1)
                     (jump-return cra)
                     (begin
                       (split cfp cfp cfp 16384)
                       (set! cs2 (cs1 - 0))
                       (seal cra cra cs2 15)
                       (seal cfp cfp cs2 15)
                       (jump-call L.swap.1)))
                "secure-tail: succes-04: if")
  ;failure
  (check-exn exn:fail? (thunk (secure-tail '+ 0 0 15)) "secure-tail: failure-01: wrong tail")
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
                   ((actual-frame-size 16384) (frameSize 8) (paramSize 0))
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