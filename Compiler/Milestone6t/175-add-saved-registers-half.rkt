#lang racket

(require "common/register.rkt"
         "common/aloc.rkt"
         "langs/imp-cmf-lang.rkt")

(provide add-saved-registers-half)

(module+ test
  (require rackunit))

;
;(add-tail t return-p)->tail?
;t: tail?
;return-p: boolean?
(define (add-tail t tmp-ra)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@e ,(add-tail tail tmp-ra))]
    [`(if ,p ,t1 ,t2) `(if ,p ,(add-tail t1 tmp-ra) ,(add-tail t2 tmp-ra))]
    [`(jump-call ,l ,a ...) `(begin (set! ,(current-return-address-register) ,tmp-ra)
                                    (jump-call ,l ,@a))]                             ;Call
    [`(jump-return ,l ,a ...) `(jump-return ,tmp-ra
                                            ,@a)]                                  ;Return
    [_ #f]))

;
;(add-tail-info info tail)->info? tail?
;info: info?
;tail: tail?
(define (add-tail-info info tail)
  (let ([tmp-ra (freshTmpRa)])
    (values info
            `(begin (set! ,tmp-ra ,(current-return-address-register))
                    ,(add-tail tail tmp-ra)))))
       

;
;(add-func f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define (add-func f)
  (match f
    [`(define ,l ,i ,t) (let-values ([(info tail) (add-tail-info i t)])
                          `(define ,l ,info ,tail))]
    [_ #f]))


;
;(add-convention-secure p)->Imp-lang-V5-cmf-proc?
;p : Imp-lang-V5-cmf-proc?
(define/contract (add-saved-registers-half p) (-> imp-cmf-lang? imp-cmf-lang?)
  (match p
    [`(module ,i ,f ... ,t) (let-values ([(info tail) (add-tail-info i t)])
                              `(module ,info ,@(map add-func f) ,tail))]
    [_ #f]))


(module+ test
  ;#|
 (define (check-add p a b m)
   (resetfresh)
   (check-equal? (p a) b m))
;add-saved-registers-half
  ;succes
  (check-add add-saved-registers-half '(module ((new-frames ()))
                                         (begin
                                           (set! x.1 2)
                                           (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                                           (begin (set! a0 (+ x.1 y.2)) (jump-return cra cfp a0))))
                '(module ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 cra)
                     (begin
                       (set! x.1 2)
                       (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                       (begin (set! a0 (+ x.1 y.2)) (jump-return tmp-ra.1 cfp a0)))))
                "add-saved-registers-half: succes-01: no tail calls")
  (check-add add-saved-registers-half '(module
                                           ((new-frames ()))
                                         (define L.odd?.1
                                           ((new-frames ()))
                                           (begin (set! x.3 a0)
                                                  (if (= x.3 0)
                                                      (begin (set! a0 0) (jump-return cra cfp a0))
                                                      (begin (set! y.4 (+ x.3 -1)) 
                                                             (begin (set! a0 y.4)
                                                                    (jump-call L.even?.2 cfp cra a0))))))
                                         (define L.even?.2
                                           ((new-frames ()))
                                           (begin (set! x.5 a0)
                                                  (if (= x.5 0)
                                                      (begin (set! a0 1) (jump-return cra cfp a0))
                                                      (begin (set! y.6 (+ x.5 -1)) 
                                                             (begin (set! a0 y.6)
                                                                    (jump-call L.odd?.1 cfp cra a0))))))
                                         (begin (set! a0 5)
                                                (jump-call L.even?.2 cfp cra a0)))
             '(module ((new-frames ()))
                (define L.odd?.1
                  ((new-frames ()))
                  (begin
                    (set! tmp-ra.2 cra)
                    (begin
                      (set! x.3 a0)
                      (if (= x.3 0)
                          (begin (set! a0 0) (jump-return tmp-ra.2 cfp a0))
                          (begin
                            (set! y.4 (+ x.3 -1))
                            (begin
                              (set! a0 y.4)
                              (begin
                                (set! cra tmp-ra.2)
                                (jump-call L.even?.2 cfp cra a0))))))))
                (define L.even?.2
                  ((new-frames ()))
                  (begin
                    (set! tmp-ra.3 cra)
                    (begin
                      (set! x.5 a0)
                      (if (= x.5 0)
                          (begin (set! a0 1) (jump-return tmp-ra.3 cfp a0))
                          (begin
                            (set! y.6 (+ x.5 -1))
                            (begin
                              (set! a0 y.6)
                              (begin
                                (set! cra tmp-ra.3)
                                (jump-call L.odd?.1 cfp cra a0))))))))
                (begin
                  (set! tmp-ra.1 cra)
                  (begin
                    (set! a0 5)
                    (begin (set! cra tmp-ra.1) (jump-call L.even?.2 cfp cra a0)))))
                "add-saved-registers-half: succes-02: tail calls")
  (check-add add-saved-registers-half '(module ((new-frames ()))
                                         (define L.test.1
                                           ((new-frames ()))
                                           (begin (set! x.1 a0) (set! x.2 a1) (set! x.3 a2)
                                                  (begin (set! y.4 (+ x.1 x.2))
                                                         (begin
                                                           (set! a0 (+ x.3 y.4))
                                                           (jump-return cra cfp a0)))))
                                         (begin (set! a0 1) (set! a1 2) (set! a2 3)
                                                (jump-call L.test.1 cfp cra a0 a1 a2)))
             '(module ((new-frames ()))
                (define L.test.1
                  ((new-frames ()))
                  (begin
                    (set! tmp-ra.2 cra)
                    (begin
                      (set! x.1 a0)
                      (set! x.2 a1)
                      (set! x.3 a2)
                      (begin
                        (set! y.4 (+ x.1 x.2))
                        (begin
                          (set! a0 (+ x.3 y.4))
                          (jump-return tmp-ra.2 cfp a0))))))
                (begin
                  (set! tmp-ra.1 cra)
                  (begin
                    (set! a0 1)
                    (set! a1 2)
                    (set! a2 3)
                    (begin (set! cra tmp-ra.1) (jump-call L.test.1 cfp cra a0 a1 a2)))))
             "add-saved-registers-half: succes-03: tail calls with fvar args")
  (check-add add-saved-registers-half '(module ((new-frames ())) (define L.swap.1
                                                                   ((new-frames ()))
                                                                   (begin
                                                                     (set! x.1 a0)
                                                                     (set! y.2 a1)
                                                                     (if (< y.2 x.1)
                                                                         (begin (set! a0 x.1) (jump-return cra cfp a0))
                                                                         (begin
                                                                           (begin 
                                                                             (return-point L.rpLabel.1
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
                                           (set! a0 1)
                                           (set! a1 2)
                                           (jump-call L.swap.1 cfp cra a0 a1)))
             '(module ((new-frames ()))
                (define L.swap.1
                  ((new-frames ()))
                  (begin
                    (set! tmp-ra.2 cra)
                    (begin
                      (set! x.1 a0)
                      (set! y.2 a1)
                      (if (< y.2 x.1)
                          (begin
                            (set! a0 x.1)
                            (jump-return tmp-ra.2 cfp a0))
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
                              (jump-return tmp-ra.2 cfp a0)))))))
                (begin
                  (set! tmp-ra.1 cra)
                  (begin
                    (set! a0 1)
                    (set! a1 2)
                    (begin (set! cra tmp-ra.1) (jump-call L.swap.1 cfp cra a0 a1)))))
             "add-saved-registers-half: succes-04: value call")
  ;|#
  )