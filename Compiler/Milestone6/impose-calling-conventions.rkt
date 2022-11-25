#lang racket

(require "common/register.rkt"
         "common/fvar.rkt"
         "common/aloc.rkt"
         "common/info.rkt")
(provide impose-calling-conventions)

(module+ test
  (require rackunit))

(define maxFrame 0)

(define (resetFrame)
  (if (> (getfvar) maxFrame)
      (set! maxFrame (getfvar))
      void)
  (resetfvar))

(define curStack '())
(define (resetCurStack)
  (set! curStack '()))
(define (addCurStack l)
  (if (or (null? l) (null? (second l)))
      void
      (set! curStack (cons l curStack))))

;
;clear-registers->effect?
(define clear-registers
  '(begin (set! t0 0)
          (set! t1 0)
          (set! t2 0)
          (set! t3 0)
          (set! t4 0)
          (set! t5 0)
          (set! t6 0)))

;
;(getFrameVar args)->list? list?
;args: list?
(define (getFrameVar args)
  (resetFrame)
  (cond [(> (length args) (length (current-parameter-registers))) (values (list-tail args (length (current-parameter-registers)))
                                                                          (build-list (- (length args) (length (current-parameter-registers))) (lambda (x) (freshfvar))))]
        [else (values '() '())]))

;
;(getNewFrameVar args)->list? list?
;args: list?
(define (getNewFrameVar args)
  (cond [(> (length args) (length (current-parameter-registers))) (values (list-tail args (length (current-parameter-registers)))
                                                                          (build-list (- (length args) (length (current-parameter-registers))) (lambda (x) (freshAloc "nfv"))))]
        [else (values '() '())]))

;
;(getArgRegs args)->list? list?
;args: list?
(define (getArgRegs args)
  (cond [(> (length args) (length (current-parameter-registers))) (values (take args (length (current-parameter-registers))) (current-parameter-registers))]
        [else (values args (take (current-parameter-registers) (length args)))]))


;
;(impose-pred p)->pred?
;p: pred?
(define (impose-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map impose-effect e) ,(impose-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(impose-pred p1) ,(impose-pred p2) ,(impose-pred p3))]
    [`(not ,pred) `(not ,(impose-pred pred))]
    [pred pred]))

;
;(impose-value v)->value?/effect?
;v: value?
(define (impose-value v)
  (match v
    [`(call ,n ,a ...) (let ([rp-label (freshLabel "rpLabel")])
                         (let-values ([(regArg regPara) (getArgRegs a)]
                                      [(fraArg fraPara) (getNewFrameVar a)])
                           (addCurStack `(,rp-label ,fraPara))
                           `(return-point ,rp-label
                                          (begin ,@(map (lambda (arg par) `(set! ,par ,arg)) (append (reverse fraArg) regArg) (append (reverse fraPara) regPara))
                                                 (set! ,(current-return-address-register) ,rp-label)
                                                 (jump ,n ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@regPara ,@fraPara)))))]
    [val val]))

;
;(impose-effect e)->list? '(effect? ...)
;e: effect?
(define (impose-effect e)
  (match e
    [`(begin ,eff ...) `(begin ,@(map impose-effect eff))]
    [`(if ,p ,e1 ,e2) `(if ,(impose-pred p) ,(impose-effect e1) ,(impose-effect e2))]
    [`(set! ,a ,v) (match v
                     [`(call ,n ,args ...) `(begin ,(impose-value v) (set! ,a ,(current-return-value-register)))]
                     [_ `(set! ,a ,(impose-value v))])]
    [_ #f]))
           
;
;(impose-tail t)->tail?
;t: tail?
(define (impose-tail t tmp-ra)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map impose-effect e) ,(impose-tail tail tmp-ra))]
    [`(if ,p ,t1 ,t2) `(if ,(impose-pred p) ,(impose-tail t1 tmp-ra) ,(impose-tail t2 tmp-ra))]
    [`(call ,n ,a ...) (resetFrame)
                       (cond [(or (label? n) (aloc? n)) (let-values ([(regArg regPara) (getArgRegs a)]
                                                                     [(fraArg fraPara) (getFrameVar a)])
                                                          `(begin ,@(map (lambda (arg par) `(set! ,par ,arg)) (append (reverse fraArg) regArg) (append (reverse fraPara) regPara))
                                                                  (set! ,(current-return-address-register) ,tmp-ra)
                                                                  (jump ,n ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@regPara ,@fraPara)))]              
                             [(integer? n) n]
                             [else #f])]
    [val `(begin (set! ,(current-return-value-register) ,val)
                 (jump ,tmp-ra ,(current-frame-base-pointer-register) ,(current-return-value-register)))]))

;
;(impose-entry e)->tail? info?
;e: entry?
(define (impose-entry e tmp-ra)
  (resetCurStack)
  (resetFrame)
  (values (impose-tail e tmp-ra)
          `(,(setNewFrames curStack))))

;
;(impose-func f)->'(define label? tail?)
;f: '(define label? (lambda (aloc? ...) tail?))
(define (impose-func f)
  (match f
    [`(define ,l (lambda (,a ...) ,t)) (let ([tmp-ra (freshAloc "tmp-ra")])
                                         (let-values ([(regArg regPara) (getArgRegs a)]
                                                      [(fraArg fraPara) (getFrameVar a)]
                                                      [(entry info) (impose-entry t tmp-ra)])
                                           (println fraPara)
                                           `(define ,l ,info (begin (set! ,tmp-ra ,(current-return-address-register))
                                                                    ,@(map (lambda (arg par) `(set! ,arg ,par)) (append regArg fraArg) (append regPara fraPara))
                                                                    ,entry))))]
    [_ #f]))

;Compiles Imp-lang-V5-cmf-proc to  Imp-lang-V5-cmf by imposing calling conventions on all calls and procedure definitions. The parameter registers are defined by the list current-parameter-registers.
;(impose-calling-conventions p)->Imp-lang-V5-cmf?
;p : Imp-lang-V5-cmf-proc?
(define (impose-calling-conventions p)
  (resetCurStack)
  (match p
    [`(module ,f ... ,t) (let ([funcs (map impose-func f)]
                               [tmp-ra (freshAloc "tmp-ra")])
                           (let-values ([(entry info) (impose-entry t tmp-ra)])
                             (setfvar maxFrame)    ;sets the fvar to the maximum amount, this way lower fvas do not overwrite these
                             `(module ,info ,@funcs (begin (set! ,tmp-ra ,(current-return-address-register)) ,entry))))]
    [_ "impose-calling-conventions failed"]))

#;(impose-pred '(begin
                              (if (not (false))
                                (return-point
                                 L.rpLabel.82
                                 (begin
                                   (set! nfv.85 x3.8)
                                   (set! nfv.84 x3.8)
                                   (set! nfv.83 183)
                                   (set! ra L.rpLabel.82)
                                   (jump
                                    L.fun5.2
                                    cfp
                                    ra
                                    nfv.83
                                    nfv.84
                                    nfv.85)))
                                (set! x9.19 a0)
                                (if (false)
                                  (set! x9.19 (* x5.10 x6.11))
                                  (if (not (<= 471 x3.8))
                                    (set! x9.19 x3.8)
                                    (return-point
                                     L.rpLabel.86
                                     (begin
                                       (set! nfv.89 x3.8)
                                       (set! nfv.88 -198)
                                       (set! nfv.87 -425)
                                       (set! ra L.rpLabel.86)
                                       (jump
                                        L.fun5.2
                                        cfp
                                        ra
                                        nfv.87
                                        nfv.88
                                        nfv.89)))
                                    (set! x9.19 a0))))
                              (if (not (true))
                                (return-point
                                 L.rpLabel.90
                                 (begin
                                   (set! nfv.94 x4.9)
                                   (set! nfv.93 x5.10)
                                   (set! nfv.92 x5.10)
                                   (set! nfv.91 x5.10)
                                   (set! ra L.rpLabel.90)
                                   (jump
                                    L.fun1.5
                                    cfp
                                    ra
                                    nfv.91
                                    nfv.92
                                    nfv.93
                                    nfv.94)))
                                (set! x10.20 a0)
                                (set! x10.20 x3.8))
                              (return-point
                               L.rpLabel.95
                               (begin
                                 (set! nfv.99 x5.10)
                                 (set! nfv.98 x5.10)
                                 (set! nfv.97 x5.10)
                                 (set! nfv.96 x5.10)
                                 (set! ra L.rpLabel.95)
                                 (jump
                                  L.fun1.5
                                  cfp
                                  ra
                                  nfv.96
                                  nfv.97
                                  nfv.98
                                  nfv.99)))
                              (set! x11.21 a0)
                              (set! x12.22 (- x6.11 x6.11))
                              (begin
                                (set! x14.24 (- x5.10 x3.8))
                                (begin
                                  (return-point
                                   L.rpLabel.100
                                   (begin
                                     (set! nfv.104 -287)
                                     (set! nfv.103 x4.9)
                                     (set! nfv.102 x5.10)
                                     (set! nfv.101 x4.9)
                                     (set! ra L.rpLabel.100)
                                     (jump
                                      L.fun4.4
                                      cfp
                                      ra
                                      nfv.101
                                      nfv.102
                                      nfv.103
                                      nfv.104)))
                                  (set! x18.26 a0)
                                  (if (if (not (true)) (false) (not (false)))
                                    (return-point
                                     L.rpLabel.105
                                     (begin
                                       (set! nfv.109 x3.8)
                                       (set! nfv.108 x4.9)
                                       (set! nfv.107 x3.8)
                                       (set! nfv.106 x3.8)
                                       (set! ra L.rpLabel.105)
                                       (jump
                                        L.fun4.4
                                        cfp
                                        ra
                                        nfv.106
                                        nfv.107
                                        nfv.108
                                        nfv.109)))
                                    (set! x19.27 a0)
                                    (return-point
                                     L.rpLabel.110
                                     (begin
                                       (set! nfv.112 x3.8)
                                       (set! nfv.111 x5.10)
                                       (set! ra L.rpLabel.110)
                                       (jump L.fun3.1 cfp ra nfv.111 nfv.112)))
                                    (set! x19.27 a0))
                                  (set! x20.28 x4.9)
                                  (begin
                                    (set! x22.30 (+ x6.11 x4.9))
                                    (return-point
                                     L.rpLabel.113
                                     (begin
                                       (set! nfv.117 103)
                                       (set! nfv.116 x6.11)
                                       (set! nfv.115 -127)
                                       (set! nfv.114 x3.8)
                                       (set! ra L.rpLabel.113)
                                       (jump
                                        L.fun1.5
                                        cfp
                                        ra
                                        nfv.114
                                        nfv.115
                                        nfv.116
                                        nfv.117)))
                                    (set! x23.31 a0)
                                    (set! x24.32 x5.10)
                                    (return-point
                                     L.rpLabel.118
                                     (begin
                                       (set! nfv.121 152)
                                       (set! nfv.120 x3.8)
                                       (set! nfv.119 -302)
                                       (set! ra L.rpLabel.118)
                                       (jump
                                        L.fun2.3
                                        cfp
                                        ra
                                        nfv.119
                                        nfv.120
                                        nfv.121)))
                                    (set! x25.33 a0)
                                    (if (true)
                                      (set! x21.29 x24.32)
                                      (return-point
                                       L.rpLabel.122
                                       (begin
                                         (set! nfv.124 x24.32)
                                         (set! nfv.123 x25.33)
                                         (set! ra L.rpLabel.122)
                                         (jump
                                          L.fun3.1
                                          cfp
                                          ra
                                          nfv.123
                                          nfv.124)))
                                      (set! x21.29 a0)))
                                  (return-point
                                   L.rpLabel.125
                                   (begin
                                     (set! nfv.128 x20.28)
                                     (set! nfv.127 x18.26)
                                     (set! nfv.126 x21.29)
                                     (set! ra L.rpLabel.125)
                                     (jump
                                      L.fun2.3
                                      cfp
                                      ra
                                      nfv.126
                                      nfv.127
                                      nfv.128)))
                                  (set! x15.25 a0))
                                (return-point
                                 L.rpLabel.129
                                 (begin
                                   (set! nfv.132 x4.9)
                                   (set! nfv.131 x3.8)
                                   (set! nfv.130 x3.8)
                                   (set! ra L.rpLabel.129)
                                   (jump
                                    L.fun2.3
                                    cfp
                                    ra
                                    nfv.130
                                    nfv.131
                                    nfv.132)))
                                (set! x16.34 a0)
                                (if (<= x4.9 x5.10)
                                  (return-point
                                   L.rpLabel.133
                                   (begin
                                     (set! nfv.136 x6.11)
                                     (set! nfv.135 -278)
                                     (set! nfv.134 x4.9)
                                     (set! ra L.rpLabel.133)
                                     (jump
                                      L.fun5.2
                                      cfp
                                      ra
                                      nfv.134
                                      nfv.135
                                      nfv.136)))
                                  (set! x17.35 a0)
                                  (set! x17.35 -174))
                                (begin
                                  (if (>= x16.34 x17.35)
                                    (return-point
                                     L.rpLabel.137
                                     (begin
                                       (set! nfv.141 x14.24)
                                       (set! nfv.140 x14.24)
                                       (set! nfv.139 x17.35)
                                       (set! nfv.138 x14.24)
                                       (set! ra L.rpLabel.137)
                                       (jump
                                        L.fun1.5
                                        cfp
                                        ra
                                        nfv.138
                                        nfv.139
                                        nfv.140
                                        nfv.141)))
                                    (set! x26.36 a0)
                                    (set! x26.36 (* -295 x17.35)))
                                  (return-point
                                   L.rpLabel.142
                                   (begin
                                     (set! nfv.146 x14.24)
                                     (set! nfv.145 x17.35)
                                     (set! nfv.144 -24)
                                     (set! nfv.143 x15.25)
                                     (set! ra L.rpLabel.142)
                                     (jump
                                      L.fun4.4
                                      cfp
                                      ra
                                      nfv.143
                                      nfv.144
                                      nfv.145
                                      nfv.146)))
                                  (set! x27.37 a0)
                                  (set! x28.38 320)
                                  (set! x29.39 x16.34)
                                  (set! x30.40 x17.35)
                                  (set! x13.23 (* x30.40 -436))))
                              (true)))


(module+ test
  (define (check-impose a b m)
    (resetfresh)
    (resetfvar)
    (check-equal? (impose-calling-conventions a) b m))
  #|
;impose-calling-conventions
  ;succes
  (check-impose '(module (begin (set! x.1 2)
                                (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                                (+ x.1 y.2)))
                '(module ((new-frames ()))
                   (begin (set! tmp-ra.1 cra)
                          (begin
                            (set! x.1 2)
                            (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                            (begin (set! ca0 (+ x.1 y.2)) (jump tmp-ra.1 cfp ca0)))))
                "impose-calling-conventions: succes-01: no tail calls")
  (check-impose '(module
                     (define L.odd?.1
                       (lambda (x.3)
                         (if (= x.3 0)
                             0
                             (begin (set! y.4 (+ x.3 -1)) (call L.even?.2 y.4)))))
                   (define L.even?.2
                     (lambda (x.5)
                       (if (= x.5 0)
                           1
                           (begin (set! y.6 (+ x.5 -1)) (call L.odd?.1 y.6)))))
                   (call L.even?.2 5))
                '(module
                     ((new-frames ()))
                   (define L.odd?.1
                     ((new-frames ()))
                     (begin (set! tmp-ra.1 cra)
                            (set! x.3 a0)
                            (if (= x.3 0)
                                (begin (set! ca0 0) (jump tmp-ra.1 cfp ca0))
                                (begin (set! y.4 (+ x.3 -1)) 
                                       (begin (set! a0 y.4)
                                              (set! cra tmp-ra.1)
                                              (jump L.even?.2 cfp cra a0))))))
                   (define L.even?.2
                     ((new-frames ()))
                     (begin (set! tmp-ra.2 cra)
                            (set! x.5 a0)
                            (if (= x.5 0)
                                (begin (set! ca0 1) (jump tmp-ra.2 cfp ca0))
                                (begin (set! y.6 (+ x.5 -1)) 
                                       (begin (set! a0 y.6)
                                              (set! cra tmp-ra.2)
                                              (jump L.odd?.1 cfp cra a0))))))
                   (begin (set! tmp-ra.3 cra)
                          (begin (set! a0 5)
                                 (set! cra tmp-ra.3)
                                 (jump L.even?.2 cfp cra a0))))
                
                "sequentialize-let: succes-02: tail calls")
  (check-impose '(module (define L.test.1 (lambda (x.1 x.2 x.3) (begin (set! y.4 (+ x.1 x.2)) (+ x.3 y.4)))) (call L.test.1 1 2 3))
                '(module ((new-frames ()))
                   (define L.test.1
                     ((new-frames ()))
                     (begin (set! tmp-ra.1 cra) (set! x.1 a0) (set! x.2 a1) (set! x.3 a2)
                            (begin (set! y.4 (+ x.1 x.2))
                                   (begin
                                     (set! ca0 (+ x.3 y.4))
                                     (jump tmp-ra.1 cfp ca0)))))
                   (begin (set! tmp-ra.2 cra)
                          (begin (set! a0 1) (set! a1 2) (set! a2 3)
                                 (set! cra tmp-ra.2)
                                 (jump L.test.1 cfp cra a0 a1 a2))))
                "impose-calling-conventions: succes-03: tail calls with fvar args")
  (check-impose '(module (define L.swap.1
                           (lambda (x.1 y.2)
                             (if (< y.2 x.1)
                                 x.1
                                 (begin (set! z.3 (call L.swap.1 y.2 x.1)) z.3))))
                   (call L.swap.1 1 2))
                '(module ((new-frames ())) (define L.swap.1
                                             ((new-frames ()))
                                             (begin
                                               (set! tmp-ra.1 cra)
                                               (set! x.1 a0)
                                               (set! y.2 a1)
                                               (if (< y.2 x.1)
                                                   (begin (set! ca0 x.1) (jump tmp-ra.1 cfp ca0))
                                                   (begin
                                                     (return-point L.rp-label.2
                                                                   (begin
                                                                     (set! a0 y.2)
                                                                     (set! a1 x.1)
                                                                     (set! cra L.rp-label.2)
                                                                     (jump L.swap.1 cfp cra a0 a1)))
                                                     (set! z.3 ca0)
                                                     (begin 
                                                       (set! ca0 z.3)
                                                       (jump tmp-ra.1 cfp ca0))))))
                   (begin
                     (set! tmp-ra.3 cra)
                     (begin 
                       (set! a0 1)
                       (set! a1 2)
                       (set! cra tmp-ra.3)
                       (jump L.swap.1 cfp cra a0 a1))))
                "impose-calling-conventions: succes-04: value call")
  ;|#
  )