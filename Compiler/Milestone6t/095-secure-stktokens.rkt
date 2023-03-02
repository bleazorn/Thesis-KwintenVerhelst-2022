#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "common/assembly.rkt"
         "langs/nested-asm-lang-jumps.rkt")
(provide secure-stktokens)

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
   [`(return-point ,l ,t) (let([token (random 0 seal-token-size)])
                            `(begin
                               (set! ,(newFvar parasize) ,(current-return-address-register))
                               (setLinear! ,(newFvar (add1 parasize)) ,(current-frame-base-pointer-register))
                               (set! ,(newFvar (add1 (add1 parasize))) ,(current-seal-location-register))
                               (return-point ,l ,(secure-tail t framesize parasize token))
                               (splice csp csp cfp ,framesize)
                               (set! ,(current-seal-location-register) ,(newFvar (add1 (add1 parasize))))
                               (setLinear! ,(current-frame-base-pointer-register) ,(newFvar (add1 parasize)))
                               (set! ,(current-return-address-register) ,(newFvar parasize))))]
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
    [`(jump-call ,trg) (let ([aux-register (car (current-auxiliary-registers))])
                         `(begin (split csp csp cfp ,framesize)
                                 (set! ,(current-seal-register) (,(current-seal-location-register) - ,seal-location))
                                 (seal cra cra ,(current-seal-register) ,token)
                                 (seal cfp cfp ,(current-seal-register) ,token)
                                 (jump-call ,trg)))]
    [`(jump-return ,trg) `(jump-return ,trg)]
    [`(invoke ,a ,b) `(invoke ,a ,b)]
    [_ #f]))

(define (split-seal s)
  (match s
    [`(seal ,r ... ,i) (let ([aux-register (car (current-auxiliary-registers))])
                         `(,@(map (lambda (x) `(seal-single ,x)) r)
                           (set! ,(current-seal-register) (,(current-seal-location-register) - 0))
                           (set! ,aux-register i)
                           (set! ,(current-seal-register) (+ ,(current-seal-register) ,aux-register))))]
    [_ #f]))

;
;(secure-info t i)->tail?
;t: tail?
;i: info?
(define (secure-info t i)
  (let ([frameSize (cond [(> (* (getInfo i getFrameSize) (framesize)) 16384) (* (getInfo i getFrameSize) (framesize))]
                         [else 16384])]
        [parasize (getInfo i getParamSize)])
    (values (addInfo i (setActualFrameSize frameSize)) (secure-tail t frameSize parasize 0))))
      

;
;(secure-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (secure-func f)
  (match f
    [`(define ,l ,i ,t) (let-values ([(info tail) (secure-info t i)])
                          `(define ,l ,info ,tail))]
    [_ #t]))


(define/contract (secure-stktokens p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) (let-values ([(info tail) (secure-info t i)])
                              `(module ,info ,@(map secure-func f) ,tail))]
    [_ "replace locations failed"]))


#;(secure-stack-tokens '(module ((frameSize 4)
                                (assignment ((tmp-ra.13 fv2) (nfv.15 fv3)))
                                (conflicts
                                 ((a0 (tmp-ra.13))
                                  (cfp (tmp-ra.13 cra nfv.15))
                                  (cra (cfp nfv.15))
                                  (nfv.15 (cra cfp))
                                  (tmp-ra.13 (cfp a0))))
                                (undead-out
                                 ((tmp-ra.13 cfp)
                                  (((tmp-ra.13 cfp a0)
                                    ((cfp nfv.15) (cfp cra nfv.15) (cfp cra nfv.15)))
                                   (cfp a0))))
                                (call-undead (tmp-ra.13))
                                (locals (tmp-ra.13 nfv.15))
                                (allocatedFvars (fv0 fv1))
                                (new-frames ((L.rpLabel.14 (nfv.15))))
                                (paramSize 0))
                         (define L.odd?.1
                           ((frameSize 5)
                            (assignment ((y.4 fv0) (tmp.16 fv0) (x.3 fv0) (tmp-ra.7 fv3) (nfv.9 fv4)))
                            (conflicts
                             ((a0 (tmp-ra.7 cfp))
                              (cfp (tmp-ra.7 cra nfv.9 y.4 x.3 a0 tmp.16 fv0))
                              (fv0 (tmp-ra.7 cfp cra))
                              (cra (cfp nfv.9 fv0))
                              (nfv.9 (cra cfp))
                              (y.4 (cfp tmp-ra.7))
                              (tmp.16 (tmp-ra.7 cfp x.3))
                              (x.3 (tmp-ra.7 cfp tmp.16))
                              (tmp-ra.7 (cfp a0 y.4 x.3 tmp.16 fv0))))
                            (undead-out
                             ((fv0 tmp-ra.7 cfp)
                              (tmp-ra.7 cfp x.3)
                              (((tmp.16 tmp-ra.7 cfp x.3) (tmp-ra.7 cfp x.3))
                               ((tmp-ra.7 cfp a0) (cfp a0))
                               ((tmp-ra.7 cfp y.4)
                                (((tmp-ra.7 cfp a0) ((cfp nfv.9) (cfp cra nfv.9) (cfp cra nfv.9)))
                                 (cfp a0))))))
                            (call-undead (tmp-ra.7))
                            (locals (tmp-ra.7 x.3 tmp.16 y.4 nfv.9))
                            (allocatedFvars (fv1 fv2))
                            (new-frames ((L.rpLabel.8 (nfv.9))))
                            (paramSize 1))
                           (begin
                             (set! fv3 cra)
                             (set! fv0 fv0)
                             (if (begin (set! fv0 0) (= fv0 fv0))
                                 (begin (set! a0 150) (jump-return fv3))
                                 (begin
                                   (set! fv0 (+ fv0 -1))
                                   (begin
                                     (return-point
                                      L.rpLabel.8
                                      (begin
                                        (set! fv4 fv0)
                                        (set! cra L.rpLabel.8)
                                        (jump-call L.even?.2)))
                                     (jump-return fv3))))))
                         (define L.even?.2
                           ((frameSize 5)
                            (assignment
                             ((y.6 fv0) (tmp.17 fv0) (x.5 fv0) (tmp-ra.10 fv3) (nfv.12 fv4)))
                            (conflicts
                             ((a0 (tmp-ra.10 cfp))
                              (cfp (tmp-ra.10 cra nfv.12 y.6 x.5 a0 tmp.17 fv0))
                              (fv0 (tmp-ra.10 cfp cra))
                              (cra (cfp nfv.12 fv0))
                              (nfv.12 (cra cfp))
                              (y.6 (cfp tmp-ra.10))
                              (tmp.17 (tmp-ra.10 cfp x.5))
                              (x.5 (tmp-ra.10 cfp tmp.17))
                              (tmp-ra.10 (cfp a0 y.6 x.5 tmp.17 fv0))))
                            (undead-out
                             ((fv0 tmp-ra.10 cfp)
                              (tmp-ra.10 cfp x.5)
                              (((tmp.17 tmp-ra.10 cfp x.5) (tmp-ra.10 cfp x.5))
                               ((tmp-ra.10 cfp a0) (cfp a0))
                               ((tmp-ra.10 cfp y.6)
                                (((tmp-ra.10 cfp a0) ((cfp nfv.12) (cfp cra nfv.12) (cfp cra nfv.12)))
                                 (cfp a0))))))
                            (call-undead (tmp-ra.10))
                            (locals (tmp-ra.10 x.5 tmp.17 y.6 nfv.12))
                            (allocatedFvars (fv1 fv2))
                            (new-frames ((L.rpLabel.11 (nfv.12))))
                            (paramSize 1))
                           (begin
                             (set! fv3 cra)
                             (set! fv0 fv0)
                             (if (begin (set! fv0 0) (= fv0 fv0))
                                 (begin (set! a0 200) (jump-return fv3))
                                 (begin
                                   (set! fv0 (+ fv0 -1))
                                   (begin
                                     (return-point
                                      L.rpLabel.11
                                      (begin
                                        (set! fv4 fv0)
                                        (set! cra L.rpLabel.11)
                                        (jump-call L.odd?.1)))
                                     (jump-return fv3))))))
                         (begin
                           (set! fv2 cra)
                           (begin
                             (return-point
                              L.rpLabel.14
                              (begin (set! fv3 5) (set! cra L.rpLabel.14) (jump-call L.even?.2)))
                             (jump-return fv2)))))


(module+ test
  )