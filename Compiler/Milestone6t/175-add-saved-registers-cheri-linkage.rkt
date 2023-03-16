#lang racket

(require "common/fvar.rkt"
         "common/info.rkt"
         "common/register.rkt"
         "common/aloc.rkt"
         "langs/imp-cmf-lang.rkt")

(provide add-saved-registers-cheri-linkage)

(module+ test
  (require rackunit))

;
;(add-tail t return-p)->tail?
;t: tail?
;return-p: boolean?
(define (add-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@e ,(add-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,p ,(add-tail t1) ,(add-tail t2))]
    [`(jump-call ,l ,a ...) `(jump-call ,l ,@a)]                                                   ;Call
    [`(jump-return ,l ,a ...) `(jump-return ,(current-return-address-register)
                                            ,@a)]                                                  ;Return
    [_ (error (format "add-saved-registers-cheri-linkage:  Failed match.\n No valid tail: ~a" t))]))

;
;(add-tail-info info tail)->info? tail?
;info: info?
;tail: tail?
(define (add-tail-info info tail)
  (let ([parasize (cond [(null? (getInfo info getParamSize)) 0]
                        [else (getInfo info getParamSize)])])
    (setfvar (sub1 parasize))
    (values (addInfo info (setAllocatedFvars `(,(newFvar parasize) ,(newFvar (add1 parasize)))))
            (add-tail tail))))
       

;
;(add-func f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define (add-func f)
  (match f
    [`(define ,l ,i ,t) (let-values ([(info tail) (add-tail-info i t)])
                          `(define ,l ,info ,tail))]
    [_ (error (format "add-saved-registers-cheri-linkage:  Failed match.\n No valid function ~a" f))]))


;
;(add-saved-registers-cheri-linkage p)->Imp-lang-V5-cmf-proc?
;p : Imp-lang-V5-cmf-proc?
(define/contract (add-saved-registers-cheri-linkage p) (-> imp-cmf-lang? imp-cmf-lang?)
  (match p
    [`(module ,i ,f ... ,t) (let-values ([(info tail) (add-tail-info i t)])
                              `(module ,info ,@(map add-func f) ,tail))]
    [_ #f]))


(module+ test
  (define (check-add p a b m)
    (resetfresh)
    (check-equal? (p a) b m))
  (define (check-add-2 p a1 a2 b m)
    (resetfresh)
    (check-equal? (p a1 a2) b m))
  (define (check-add-values-2 p a1 a2 b1 b2 m)
    (resetfresh)
    (let-values ([(r1 r2) (p a1 a2)])
      (check-equal? r1 b1 m)
      (check-equal? r2 b2 m)))
  ;(add-tail t)
  ;succes
  (check-add add-tail
             '(jump-return cra cfp a0)
             '(jump-return cra cfp a0)
             "add-tail: succes-01: jump-return")
  (check-add add-tail
             '(jump-call L.swap.1 cfp cra a1 a2)
             '(jump-call L.swap.1 cfp cra a1 a2)
             "add-tail: succes-02: jump-call")

  (check-add add-tail
             '(begin (set! x.1 y.2) (begin (set! a0 x.1) (jump-return cra cfp a0)))
             '(begin (set! x.1 y.2) (begin (set! a0 x.1) (jump-return cra cfp a0)))
             "add-tail: succes-03: begin")
  (check-add add-tail
             '(if (true)
                  (begin (set! a0 x.1) (jump-return cra cfp a0))
                  (begin (set! a0 y.2) (jump-return cra cfp a0)))
             '(if (true)
                  (begin (set! a0 x.1) (jump-return cra cfp a0))
                  (begin (set! a0 y.2) (jump-return cra cfp a0)))
             "add-tail: succes-04: if")
  (check-add add-tail
             '(begin
                (return-point
                 L.rpLabel.1
                 (begin
                   (set! a1 1)
                   (set! a2 2)
                   (set! cra L.rpLabel.1)
                   (jump-call L.swap.1 cfp cra a1 a2)))
                (jump-return cra cfp a0))
             '(begin
                (return-point
                 L.rpLabel.1
                 (begin
                   (set! a1 1)
                   (set! a2 2)
                   (set! cra L.rpLabel.1)
                   (jump-call L.swap.1 cfp cra a1 a2)))
                (jump-return cra cfp a0))
             "add-tail: succes-05: call")
  ;failure
  (check-exn exn:fail? (thunk (add-tail '(call x 1 2))) "add-tail: failure-01: call error")
  ;(add-tail-info e)
  ;succes
  (check-add-values-2 add-tail-info
                      '((new-frames ()) (paramSize 0)) '(begin (set! a0 x.1) (jump-return cra cfp a0)) 
                      '((allocatedFvars (fv0 fv1)) (new-frames ()) (paramSize 0)) '(begin (set! a0 x.1) (jump-return cra cfp a0)) 
                      "add-tail-info: succes-01: val")
  (check-add-values-2 add-tail-info
                      '((new-frames ()) (paramSize 0))
                      '(begin
                         (return-point
                          L.rpLabel.1
                          (begin
                            (set! a1 1)
                            (set! a2 2)
                            (set! cra L.rpLabel.1)
                            (jump-call L.swap.1 cfp cra a1 a2)))
                         (jump-return cra cfp a0))
                      '((allocatedFvars (fv0 fv1)) (new-frames ()) (paramSize 0))
                      '(begin
                         (return-point
                          L.rpLabel.1
                          (begin
                            (set! a1 1)
                            (set! a2 2)
                            (set! cra L.rpLabel.1)
                            (jump-call L.swap.1 cfp cra a1 a2)))
                         (jump-return cra cfp a0))
                      "add-tail-info: succes-02: call")

  (check-add-values-2 add-tail-info
                      '((new-frames ()) (paramSize 0)) '(begin (set! x.1 y.2) (begin (set! a0 x.1) (jump-return cra cfp a0))) 
                      '((allocatedFvars (fv0 fv1)) (new-frames ()) (paramSize 0)) '(begin (set! x.1 y.2) (begin (set! a0 x.1) (jump-return cra cfp a0))) 
                      "add-tail-info: succes-03: begin")
  (check-add-values-2 add-tail-info
                      '((new-frames ()) (paramSize 0))
                      '(if (true)
                           (begin (set! a0 x.1) (jump-return cra cfp a0))
                           (begin (set! a0 y.2) (jump-return cra cfp a0)))
                      '((allocatedFvars (fv0 fv1)) (new-frames ()) (paramSize 0))
                      '(if (true)
                           (begin (set! a0 x.1) (jump-return cra cfp a0))
                           (begin (set! a0 y.2) (jump-return cra cfp a0)))
                      "add-tail-info: succes-04: if")
  ;(add-func f)
  ;succes
  (check-add add-func
             '(define L.odd.1
                ((new-frames ()) (paramSize 0))
                (begin
                  (set! x.1 a1)
                  (set! y.2 a2)
                  (set! z.3 a3)
                  (begin
                    (return-point
                     L.rpLabel.1
                     (begin
                       (set! a1 x.1)
                       (set! a2 y.2)
                       (set! cra L.rpLabel.1)
                       (jump-call L.swap.2 cfp cra a1 a2)))
                    (jump-return cra cfp a0))))
             '(define L.odd.1
                ((allocatedFvars (fv0 fv1)) (new-frames ()) (paramSize 0))
                (begin
                  (set! x.1 a1)
                  (set! y.2 a2)
                  (set! z.3 a3)
                  (begin
                    (return-point
                     L.rpLabel.1
                     (begin
                       (set! a1 x.1)
                       (set! a2 y.2)
                       (set! cra L.rpLabel.1)
                       (jump-call L.swap.2 cfp cra a1 a2)))
                    (jump-return cra cfp a0))))
             "add-func: succes-01: tail call")
  (check-add add-func
             '(define L.odd.1
                ((new-frames ((L.rpLabel.1 (nfv.2 nfv.3)))) (paramSize 3))
                (begin
                  (set! x.1 fv0)
                  (set! y.2 fv1)
                  (set! z.3 fv2)
                  (begin
                    (begin
                      (return-point
                       L.rpLabel.1
                       (begin
                         (set! nfv.3 y.2)
                         (set! nfv.2 x.1)
                         (set! cra L.rpLabel.1)
                         (jump-call L.swap.2 cfp cra nfv.2 nfv.3)))
                      (set! x.1 a0))
                    (begin (set! a0 x.1) (jump-return cra cfp a0)))))
             '(define L.odd.1
                ((allocatedFvars (fv3 fv4))
                 (new-frames ((L.rpLabel.1 (nfv.2 nfv.3))))
                 (paramSize 3))
                (begin
                  (set! x.1 fv0)
                  (set! y.2 fv1)
                  (set! z.3 fv2)
                  (begin
                    (begin
                      (return-point
                       L.rpLabel.1
                       (begin
                         (set! nfv.3 y.2)
                         (set! nfv.2 x.1)
                         (set! cra L.rpLabel.1)
                         (jump-call L.swap.2 cfp cra nfv.2 nfv.3)))
                      (set! x.1 a0))
                    (begin (set! a0 x.1) (jump-return cra cfp a0)))))
             "add-func: succes-02: value call")
  ;failure
  (check-exn exn:fail? (thunk (add-func '(defihne L.odd.1
                                           ((new-frames ()) (paramSize 0))
                                           (begin
                                             (set! x.1 a1)
                                             (set! y.2 a2)
                                             (set! z.3 a3)
                                             (begin
                                               (return-point
                                                L.rpLabel.1
                                                (begin
                                                  (set! a1 x.1)
                                                  (set! a2 y.2)
                                                  (set! cra L.rpLabel.1)
                                                  (jump-call L.swap.2 cfp cra a1 a2)))
                                               (jump-return cra cfp a0)))))) "add-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (add-func '(define
                                           ((new-frames ()) (paramSize 0))
                                           (begin
                                             (set! x.1 a1)
                                             (set! y.2 a2)
                                             (set! z.3 a3)
                                             (begin
                                               (return-point
                                                L.rpLabel.1
                                                (begin
                                                  (set! a1 x.1)
                                                  (set! a2 y.2)
                                                  (set! cra L.rpLabel.1)
                                                  (jump-call L.swap.2 cfp cra a1 a2)))
                                               (jump-return cra cfp a0)))))) "add-func: failure-02: no name")
  (check-exn exn:fail? (thunk (add-func '(define L.odd.1
                                           (begin
                                             (set! x.1 a1)
                                             (set! y.2 a2)
                                             (set! z.3 a3)
                                             (begin
                                               (return-point
                                                L.rpLabel.1
                                                (begin
                                                  (set! a1 x.1)
                                                  (set! a2 y.2)
                                                  (set! cra L.rpLabel.1)
                                                  (jump-call L.swap.2 cfp cra a1 a2)))
                                               (jump-return cra cfp a0)))))) "add-func: failure-03: no info")
  (check-exn exn:fail? (thunk (add-func '(define L.odd.1
                                           ((new-frames ()) (paramSize 0)))))
             "add-func: failure-04: no tail")
  #|
;add-saved-registers-full
  ;succes
  (check-add add-saved-registers-full '(module ((new-frames ()) (paramSize 0))
                                                  (begin
                                                    (set! x.1 2)
                                                    (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                                                    (begin (set! a0 (+ x.1 y.2)) (jump-return cra cfp a0))))
                '(module ((new-frames ()) (paramSize 0))
                   (begin
                     (set! tmp-ra.1 cra)
                     (begin
                       (set! x.1 2)
                       (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                       (begin (set! a0 (+ x.1 y.2)) (jump-return tmp-ra.1 cfp a0)))))
                "add-saved-registers-full: succes-01: no tail calls")
  (check-add add-saved-registers-full '(module ((new-frames ()) (paramSize 0))
                                                  (define L.odd?.1
                                                    ((new-frames ()) (paramSize 2))
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
                                                    ((new-frames ()) (paramSize 3))
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
                '(module ((new-frames ()) (paramSize 0))
                   (define L.odd?.1
                     ((new-frames ()) (paramSize 2))
                     (begin
                       (set! tmp-ra.2 cra)
                       (begin
                         (set! x.3 a0)
                         (if (= x.3 0)
                             (begin (set! a0 0) (jump-return tmp-ra.2 cfp a0))
                             (begin
                               (set! y.4 (+ x.3 -1))
                               (begin (return-point
                                       L.rpLabel.1
                                       (begin
                                         (set! a0 y.4)
                                         (set! cra L.rpLabel.1)
                                         (jump-call L.even?.2 cfp cra a0)))
                                      (jump-return tmp-ra.2 cfp a0)))))))
                   (define L.even?.2
                     ((new-frames ()) (paramSize 3))
                     (begin
                       (set! tmp-ra.3 cra)
                       (begin
                         (set! x.5 a0)
                         (if (= x.5 0)
                             (begin (set! a0 1) (jump-return tmp-ra.3 cfp a0))
                             (begin
                               (set! y.6 (+ x.5 -1))
                               (begin (return-point
                                       L.rpLabel.2
                                       (begin
                                         (set! a0 y.6)
                                         (set! cra L.rpLabel.2)
                                         (jump-call L.odd?.1 cfp cra a0)))
                                      (jump-return tmp-ra.3 cfp a0)))))))
                   (begin
                     (set! tmp-ra.1 cra)
                     (begin (return-point
                             L.rpLabel.3
                             (begin (set! a0 5) (set! cra L.rpLabel.3) (jump-call L.even?.2 cfp cra a0)))
                            (jump-return tmp-ra.1 cfp a0))))
                "add-saved-registers-full: succes-02: tail calls")
  (check-add add-saved-registers-full '(module ((new-frames ()) (paramSize 0))
                                    (define L.test.1
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
                '(module ((new-frames ()) (paramSize 0))
                   (define L.test.1
                     ((new-frames ()) (paramSize 0))
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
                       (return-point
                        L.rpLabel.1
                        (begin
                          (set! a0 1)
                          (set! a1 2)
                          (set! a2 3)
                          (set! cra L.rpLabel.1)
                          (jump-call L.test.1 cfp cra a0 a1 a2)))
                       (jump-return tmp-ra.1 cfp a0))))
                "add-saved-registers-full: succes-03: tail calls with fvar args")
  (check-add add-saved-registers-full '(module ((new-frames ()) (paramSize 0))
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
                '(module ((new-frames ()) (paramSize 0))
                   (define L.swap.1
                     ((new-frames ()) (paramSize 0))
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
                       (return-point
                        L.rpLabel.2
                        (begin
                          (set! a0 1)
                          (set! a1 2)
                          (set! cra L.rpLabel.2)
                          (jump-call L.swap.1 cfp cra a0 a1)))
                       (jump-return tmp-ra.1 cfp a0))))
                "add-saved-registers-full: succes-04: value call")
  ;|#
  )
