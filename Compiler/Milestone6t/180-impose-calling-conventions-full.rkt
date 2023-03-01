#lang racket

(require "common/register.rkt"
         "common/fvar.rkt"
         "common/aloc.rkt"
         "common/info.rkt"
         "langs/proc-imp-cmf-lang.rkt"
         "langs/imp-cmf-lang.rkt")
(provide impose-calling-conventions-full)

(module+ test
  (require rackunit))


(define curStack '())
(define (resetCurStack)
  (set! curStack '()))
(define (addCurStack l)
  (if (or (null? l) (null? (second l)))
      void
      (set! curStack (cons l curStack))))

(define parasize 0)
(define (addParaSize n)
  (set! parasize (+ parasize n)))
(define (setParaSize n)
  (set! parasize n))
(define (resetParaSize)
  (set! parasize 0))

(define (resetImpose)
  (resetCurStack)
  (resetParaSize))

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
  (resetfvar)
  (cond [(> (length args) (length (current-parameter-registers))) (let ([frameArgSize (- (length args) (length (current-parameter-registers)))])
                                                                    (addParaSize frameArgSize)
                                                                    (values (list-tail args (length (current-parameter-registers)))
                                                                            (build-list frameArgSize (lambda (x) (freshfvar)))))]
        [else (values '() '())]))

;
;(getNewFrameVar args)->list? list?
;args: list?
(define (getNewFrameVar args)
  (cond [(> (length args) (length (current-parameter-registers))) (values (list-tail args (length (current-parameter-registers)))
                                                                          (build-list (- (length args) (length (current-parameter-registers))) (lambda (x) (freshNfv))))]
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
    [`(call ,n ,a ...) (cond [(or (label? n) (aloc? n)) (let ([rp-label (freshRPLabel)])
                                                          (let-values ([(regArg regPara) (getArgRegs a)]
                                                                       [(fraArg fraPara) (getNewFrameVar a)])
                                                            (addCurStack `(,rp-label ,fraPara))
                                                            `(return-point ,rp-label
                                                                           (begin ,@(map (lambda (arg par) `(set! ,par ,arg)) (append (reverse fraArg) regArg) (append (reverse fraPara) regPara))
                                                                                  (set! ,(current-return-address-register) ,rp-label)
                                                                                  (jump-call ,n ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@regPara ,@fraPara)))))]
                             [(integer? n) n]
                             [else #f])]
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
(define (impose-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map impose-effect e) ,(impose-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(impose-pred p) ,(impose-tail t1) ,(impose-tail t2))]
    [`(call ,n ,a ...) (cond [(or (label? n) (aloc? n)) (let ([rp-label (freshRPLabel)])
                                                          (let-values ([(regArg regPara) (getArgRegs a)]
                                                                       [(fraArg fraPara) (getNewFrameVar a)])
                                                            (addCurStack `(,rp-label ,fraPara))
                                                            `(begin (return-point ,rp-label
                                                                                  (begin ,@(map (lambda (arg par) `(set! ,par ,arg)) (append (reverse fraArg) regArg) (append (reverse fraPara) regPara))
                                                                                         (set! ,(current-return-address-register) ,rp-label)
                                                                                         (jump-call ,n ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@regPara ,@fraPara)))
                                                                    (jump-return ,(current-return-address-register) ,(current-frame-base-pointer-register) ,(current-return-value-register)))))]              
                             [(integer? n) n]
                             [else #f])]
    [val `(begin (set! ,(current-return-value-register) ,val)
                 (jump-return ,(current-return-address-register) ,(current-frame-base-pointer-register) ,(current-return-value-register)))]))

;
;(impose-entry e)->tail? info?
;e: entry?
(define (impose-entry e)
  (let ([tail (impose-tail e)])
    (let ([nInfo `(,(setNewFrames curStack) ,(setParamSize parasize))])
       (values tail
               nInfo))))

;
;(impose-func f)->'(define label? tail?)
;f: '(define label? (lambda (aloc? ...) tail?))
(define (impose-func f)
  (resetImpose)
  (match f
    [`(define ,l (lambda (,a ...) ,t)) (let-values ([(regArg regPara) (getArgRegs a)]
                                                    [(fraArg fraPara) (getFrameVar a)]
                                                    [(entry info) (impose-entry t)])
                                         `(define ,l ,info (begin ,@(map (lambda (arg par) `(set! ,arg ,par)) (append regArg fraArg) (append regPara fraPara))
                                                                  ,entry)))]
    [_ #f]))

;Compiles Imp-lang-V5-cmf-proc to  Imp-lang-V5-cmf by imposing calling conventions on all calls and procedure definitions. The parameter registers are defined by the list current-parameter-registers.
;(impose-calling-conventions p)->Imp-lang-V5-cmf?
;p : Imp-lang-V5-cmf-proc?
(define/contract (impose-calling-conventions-full p) (-> proc-imp-cmf-lang? imp-cmf-lang?)
  (match p
    [`(module ,i ,f ... ,t) (let ([funcs (map impose-func f)])
                              (resetImpose)
                              (let-values ([(entry info) (impose-entry t)])
                                `(module ,info ,@funcs ,entry)))]
    [_ "impose-calling-conventions failed"]))


(module+ test
  (define (check-impose p a b m)
    (resetfresh)
    (resetfvar)
    (check-equal? (p a) b m))
 ; #|
;impose-calling-conventions
  ;succes
  (check-impose impose-calling-conventions-full
                '(module () (begin (set! x.1 2)
                                   (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                                   (+ x.1 y.2)))
                '(module ((new-frames ()) (paramSize 0))
                   (begin
                     (set! x.1 2)
                     (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                     (begin (set! a0 (+ x.1 y.2)) (jump-return cra cfp a0))))
                "impose-calling-conventions: succes-01: no tail calls")
  (check-impose impose-calling-conventions-full
                '(module ()
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
                '(module ((new-frames ()) (paramSize 0)) (define L.odd?.1
                                             ((new-frames ()) (paramSize 0))
                                             (begin
                                               (set! x.3 a0)
                                               (if (= x.3 0)
                                                   (begin (set! a0 0) (jump-return cra cfp a0))
                                                   (begin
                                                     (set! y.4 (+ x.3 -1))
                                                     (begin (return-point
                                                             L.rpLabel.1
                                                             (begin
                                                               (set! a0 y.4)
                                                               (set! cra L.rpLabel.1)
                                                               (jump-call L.even?.2 cfp cra a0)))
                                                            (jump-return cra cfp a0))))))
                   (define L.even?.2
                     ((new-frames ()) (paramSize 0))
                     (begin
                       (set! x.5 a0)
                       (if (= x.5 0)
                           (begin (set! a0 1) (jump-return cra cfp a0))
                           (begin
                             (set! y.6 (+ x.5 -1))
                             (begin (return-point
                                     L.rpLabel.2
                                     (begin
                                       (set! a0 y.6)
                                       (set! cra L.rpLabel.2)
                                       (jump-call L.odd?.1 cfp cra a0)))
                                    (jump-return cra cfp a0))))))
                   (begin (return-point
                             L.rpLabel.3
                             (begin (set! a0 5) (set! cra L.rpLabel.3) (jump-call L.even?.2 cfp cra a0)))
                     (jump-return cra cfp a0)))
                "sequentialize-let: succes-02: tail calls")
  (check-impose impose-calling-conventions-full
                '(module () (define L.test.1 (lambda (x.1 x.2 x.3) (begin (set! y.4 (+ x.1 x.2)) (+ x.3 y.4)))) (call L.test.1 1 2 3))
                '(module ((new-frames ()) (paramSize 0)) (define L.test.1
                                                           ((new-frames ()) (paramSize 0))
                                                           (begin
                                                             (set! x.1 a0)
                                                             (set! x.2 a1)
                                                             (set! x.3 a2)
                                                             (begin
                                                               (set! y.4 (+ x.1 x.2))
                                                               (begin
                                                                 (set! a0 (+ x.3 y.4))
                                                                 (jump-return cra cfp a0)))))
                   (begin
                       (return-point
                        L.rpLabel.1
                        (begin
                          (set! a0 1)
                          (set! a1 2)
                          (set! a2 3)
                          (set! cra L.rpLabel.1)
                          (jump-call L.test.1 cfp cra a0 a1 a2)))
                       (jump-return cra cfp a0)))
                "impose-calling-conventions: succes-03: tail calls with fvar args")
  (check-impose impose-calling-conventions-full
                '(module () (define L.swap.1
                              (lambda (x.1 y.2)
                                (if (< y.2 x.1)
                                    x.1
                                    (begin (set! z.3 (call L.swap.1 y.2 x.1)) z.3))))
                   (call L.swap.1 1 2))
                '(module ((new-frames ()) (paramSize 0))
                   (define L.swap.1
                     ((new-frames ()) (paramSize 0))
                     (begin
                       (set! x.1 a0)
                       (set! y.2 a1)
                       (if (< y.2 x.1)
                           (begin
                             (set! a0 x.1)
                             (jump-return cra cfp a0))
                           (begin
                             (begin
                               (return-point
                                L.rpLabel.1
                                (begin
                                  (set! a0 y.2)
                                  (set! a1 x.1)
                                  (set! cra L.rpLabel.1)
                                  (jump-call L.swap.1 cfp cra a0 a1)))
                               (set! z.3 a0))
                             (begin
                               (set! a0 z.3)
                               (jump-return cra cfp a0))))))
                   (begin
                       (return-point
                        L.rpLabel.2
                        (begin
                          (set! a0 1)
                          (set! a1 2)
                          (set! cra L.rpLabel.2)
                          (jump-call L.swap.1 cfp cra a0 a1)))
                       (jump-return cra cfp a0)))
                "impose-calling-conventions: succes-04: value call")
  ;|#
  )


