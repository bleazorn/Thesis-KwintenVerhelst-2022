#lang racket

(require "common/fvar.rkt"
         "common/info.rkt"
         "common/register.rkt"
         "common/aloc.rkt"
         "langs/imp-cmf-lang.rkt")

(provide call-convention-secure)

(module+ test
  (require rackunit))

;
;(call-pred p)->pred?
;p: pred?
(define (call-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(filter-not null? (map call-effect e)) ,(call-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(call-pred p1) ,(call-pred p2) ,(call-pred p3))]
    [`(,relop ,a ,b) p]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(call-pred pred))]
    [_ #f]))


;
;(call-effect e)->effect?
;e: effect?
(define (call-effect e)
  (match e
    [`(begin ,e ...) `(begin ,@(filter-not null? (map call-effect e)))]
    [`(if ,p ,e1 ,e2) `(if ,(call-pred p) ,(call-effect e1) ,(call-effect e2))]
    [`(set! ,a ,v) #:when (and (isTmpRa? a) (equal? v (current-return-address-register))) '()]
    [`(set! ,a ,v) e]
    [`(return-point ,l ,t) `(return-point ,l ,(call-tail t))]
    [_ #f]))

;
;(call-tail t return-p)->tail?
;t: tail?
;return-p: boolean?
(define (call-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(filter-not null? (map call-effect e)) ,(call-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(call-pred p) ,(call-tail t1) ,(call-tail t2))]
    [`(jump-call ,l ,a ...) `(jump-call ,l ,@a)]                                                   ;Call
    [`(jump-return ,l ,a ...) `(jump-return ,(current-return-address-register)
                                            ,@a
                                            ,(current-seal-location-register))]                    ;Return
    [_ #f]))

;
;(call-tail-info info tail)->info? tail?
;info: info?
;tail: tail?
(define (call-tail-info info tail)
  (let ([parasize (cond [(null? (getInfo info getParamSize)) 0]
                        [else (getInfo info getParamSize)])])
    (setfvar (sub1 parasize))
    (values (addInfo info (setAllocatedFvars `(,(newFvar parasize) ,(newFvar (add1 parasize)) ,(newFvar (add1 (add1 parasize))))))
            (call-tail tail))))
       

;
;(call-func f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define (call-func f)
  (match f
    [`(define ,l ,i ,t) (let-values ([(info tail) (call-tail-info i t)])
                          `(define ,l ,info ,tail))]
    [_ #f]))


;
;(call-convention-secure p)->Imp-lang-V5-cmf-proc?
;p : Imp-lang-V5-cmf-proc?
(define/contract (call-convention-secure p) (-> imp-cmf-lang? imp-cmf-lang?)
  (match p
    [`(module ,i ,f ... ,t) (let-values ([(info tail) (call-tail-info i t)])
                              `(module ,info ,@(map call-func f) ,tail))]
    [_ #f]))


(module+ test
;call-convention-secure
  ;succes
  (check-equal? (call-convention-secure '(module ((new-frames ()) (paramSize 0))
                                           (begin (set! tmp-ra.1 cra)
                                                  (begin
                                                    (set! x.1 2)
                                                    (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                                                    (begin (set! a0 (+ x.1 y.2)) (jump-return tmp-ra.1 cfp a0))))))
                '(module ((allocatedFvars (fv0 fv1 fv2)) (new-frames ()) (paramSize 0))
                   (begin 
                          (begin
                            (set! x.1 2)
                            (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                            (begin (set! a0 (+ x.1 y.2)) (jump-return cra cfp a0 cs1)))))
                "call-convention-secure: succes-01: no tail calls")
  (check-equal? (call-convention-secure '(module ((new-frames ()) (paramSize 0))
                                           (define L.odd?.1
                                             ((new-frames ()) (paramSize 2))
                                             (begin
                                               (set! tmp-ra.1 cra)
                                               (set! x.3 a0)
                                               (if (= x.3 0)
                                                   (begin (set! a0 0) (jump-return tmp-ra.1 cfp a0))
                                                   (begin
                                                     (set! y.4 (+ x.3 -1))
                                                     (begin (return-point
                                                             L.rpLabel.2
                                                             (begin
                                                               (set! a0 y.4)
                                                               (set! cra L.rpLabel.2)
                                                               (jump-call L.even?.2 cfp cra a0)))
                                                            (jump-return tmp-ra.1 cfp a0))))))
                                           (define L.even?.2
                                             ((new-frames ()) (paramSize 3))
                                             (begin
                                               (set! tmp-ra.3 cra)
                                               (set! x.5 a0)
                                               (if (= x.5 0)
                                                   (begin (set! a0 1) (jump-return tmp-ra.3 cfp a0))
                                                   (begin
                                                     (set! y.6 (+ x.5 -1))
                                                     (begin (return-point
                                                             L.rpLabel.4
                                                             (begin
                                                               (set! a0 y.6)
                                                               (set! cra L.rpLabel.4)
                                                               (jump-call L.odd?.1 cfp cra a0)))
                                                            (jump-return tmp-ra.3 cfp a0))))))
                                           (begin
                                             (set! tmp-ra.5 cra)
                                             (begin (return-point
                                                     L.rpLabel.6
                                                     (begin (set! a0 5) (set! cra L.rpLabel.6) (jump-call L.even?.2 cfp cra a0)))
                                                    (jump-return tmp-ra.5 cfp a0)))))
                '(module ((allocatedFvars (fv0 fv1 fv2)) (new-frames ()) (paramSize 0))
                   (define L.odd?.1
                     ((allocatedFvars (fv2 fv3 fv4)) (new-frames ()) (paramSize 2))
                     (begin
                       
                       (set! x.3 a0)
                       (if (= x.3 0)
                           (begin (set! a0 0) (jump-return cra cfp a0 cs1))
                           (begin
                             (set! y.4 (+ x.3 -1))
                             (begin (return-point
                                     L.rpLabel.2
                                     (begin
                                       (set! a0 y.4)
                                       (set! cra L.rpLabel.2)
                                       (jump-call L.even?.2 cfp cra a0)))
                                    (jump-return cra cfp a0 cs1))))))
                   (define L.even?.2
                     ((allocatedFvars (fv3 fv4 fv5)) (new-frames ()) (paramSize 3))
                     (begin
                       
                       (set! x.5 a0)
                       (if (= x.5 0)
                           (begin (set! a0 1) (jump-return cra cfp a0 cs1))
                           (begin
                             (set! y.6 (+ x.5 -1))
                             (begin (return-point
                                     L.rpLabel.4
                                     (begin
                                       (set! a0 y.6)
                                       (set! cra L.rpLabel.4)
                                       (jump-call L.odd?.1 cfp cra a0)))
                                    (jump-return cra cfp a0 cs1))))))
                   (begin
                     
                     (begin (return-point
                             L.rpLabel.6
                             (begin (set! a0 5) (set! cra L.rpLabel.6) (jump-call L.even?.2 cfp cra a0)))
                     (jump-return cra cfp a0 cs1))))
                
                "call-convention-secure: succes-02: tail calls")
  (check-equal? (call-convention-secure '(module ((new-frames ()) (paramSize 0))
                                           (define L.test.1
                                             ((new-frames ()) (paramSize 0))
                                             (begin
                                               (set! tmp-ra.1 cra)
                                               (set! x.1 a0)
                                               (set! x.2 a1)
                                               (set! x.3 a2)
                                               (begin
                                                 (set! y.4 (+ x.1 x.2))
                                                 (begin
                                                   (set! a0 (+ x.3 y.4))
                                                   (jump-return tmp-ra.1 cfp a0)))))
                                           (begin
                                             (set! tmp-ra.2 cra)
                                             (begin
                                               (return-point
                                                L.rpLabel.3
                                                (begin
                                                  (set! a0 1)
                                                  (set! a1 2)
                                                  (set! a2 3)
                                                  (set! cra L.rpLabel.3)
                                                  (jump-call L.test.1 cfp cra a0 a1 a2)))
                                               (jump-return tmp-ra.2 cfp a0)))))
                '(module ((allocatedFvars (fv0 fv1 fv2)) (new-frames ()) (paramSize 0))
                   (define L.test.1
                     ((allocatedFvars (fv0 fv1 fv2)) (new-frames ()) (paramSize 0))
                     (begin
                       
                       (set! x.1 a0)
                       (set! x.2 a1)
                       (set! x.3 a2)
                       (begin
                         (set! y.4 (+ x.1 x.2))
                         (begin
                           (set! a0 (+ x.3 y.4))
                           (jump-return cra cfp a0 cs1)))))
                   (begin
                     
                     (begin
                       (return-point
                        L.rpLabel.3
                        (begin
                          (set! a0 1)
                          (set! a1 2)
                          (set! a2 3)
                          (set! cra L.rpLabel.3)
                          (jump-call L.test.1 cfp cra a0 a1 a2)))
                       (jump-return cra cfp a0 cs1))))
                "call-convention-secure: succes-03: tail calls with fvar args")
  (check-equal? (call-convention-secure '(module ((new-frames ()) (paramSize 0))
                                           (define L.swap.1
                                             ((new-frames ()) (paramSize 0))
                                             (begin
                                               (set! tmp-ra.1 cra)
                                               (set! x.1 a0)
                                               (set! y.2 a1)
                                               (if (< y.2 x.1)
                                                   (begin
                                                     (set! a0 x.1)
                                                     (jump-return tmp-ra.1 cfp a0))
                                                   (begin
                                                     (begin
                                                       (return-point
                                                        L.rpLabel.2
                                                        (begin
                                                          (set! a0 y.2)
                                                          (set! a1 x.1)
                                                          (set! cra L.rpLabel.2)
                                                          (jump-call L.swap.1 cfp cra a0 a1)))
                                                       (set! z.3 a0))
                                                     (begin
                                                       (set! a0 z.3)
                                                       (jump-return tmp-ra.1 cfp a0))))))
                                           (begin
                                             (set! tmp-ra.3 cra)
                                             (begin
                                               (return-point
                                                L.rpLabel.4
                                                (begin
                                                  (set! a0 1)
                                                  (set! a1 2)
                                                  (set! cra L.rpLabel.4)
                                                  (jump-call L.swap.1 cfp cra a0 a1)))
                                               (jump-return tmp-ra.3 cfp a0)))))
                '(module ((allocatedFvars (fv0 fv1 fv2)) (new-frames ()) (paramSize 0))
                   (define L.swap.1
                     ((allocatedFvars (fv0 fv1 fv2)) (new-frames ()) (paramSize 0))
                     (begin
                       
                       (set! x.1 a0)
                       (set! y.2 a1)
                       (if (< y.2 x.1)
                           (begin
                             (set! a0 x.1)
                             (jump-return cra cfp a0 cs1))
                           (begin
                             (begin
                               (return-point
                                L.rpLabel.2
                                (begin
                                  (set! a0 y.2)
                                  (set! a1 x.1)
                                  (set! cra L.rpLabel.2)
                                  (jump-call L.swap.1 cfp cra a0 a1)))
                               (set! z.3 a0))
                             (begin
                               (set! a0 z.3)
                               (jump-return cra cfp a0 cs1))))))
                   (begin
                     
                     (begin
                       (return-point
                        L.rpLabel.4
                        (begin
                          (set! a0 1)
                          (set! a1 2)
                          (set! cra L.rpLabel.4)
                          (jump-call L.swap.1 cfp cra a0 a1)))
                       (jump-return cra cfp a0 cs1))))
                "call-convention-secure: succes-04: value call")
  ;|#
  )
