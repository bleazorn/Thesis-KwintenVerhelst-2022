#lang racket

(require "common/fvar.rkt"
         "common/register.rkt")
(provide implement-fvars)

(module+ test
  (require rackunit))

;!!!DECISION: fv0 = (fp - 0) fp is frame pointer and frame size is 8

(define offset 0)
(define (resetOffSet)
  (addOffSet 0))
(define (addOffSet binop n)
  (match binop
    ['- (set! offset (- offset n))]
    ['+ (set! offset (+ offset n))]
    [_ #f]))

; returns (n-1)*8 with n the number of the fvar
;(fvar-number? v) -> integer?
; v: fvar?
(define (fvar-number? v)
  (if (fvar? v)
      (let* ([n (getFvarNumber v)])
        (+ (* n framesize) offset))
      #f))

;if given argument is of type fvar, transforms it into (fp - dispoffset)
;(change-fvar fvar)-> symbol?
;fvar: any?
(define (change-fvar f)
  (if (fvar? f)
      `(,(current-frame-base-pointer-register) - ,(fvar-number? f))
      f))

;
;(implement-pred p)->pred?
;p: pred?
(define (implement-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map implement-effect e) ,(implement-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(implement-pred p1) ,(implement-pred p2) ,(implement-pred p3))]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(implement-pred pred))]
    [`(,relop ,a ,b) `(,relop ,(change-fvar a) ,(change-fvar b))]
    [_ #f]))

;
;(implement-effect e)->effect?
;e: effect?
(define (implement-effect e)
  (match e
    [`(begin ,e ...) `(begin ,@(map implement-effect e))]
    [`(if ,p ,e1 ,e2) `(if ,(implement-pred p) ,(implement-effect e1) ,(implement-effect e2))]
    [`(set! ,a (,binop ,b ,c)) #:when (and (equal? a (current-frame-base-pointer-register)) (equal? b (current-frame-base-pointer-register)))
                               (addOffSet binop c) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,(change-fvar a) (,binop ,(change-fvar b) ,(change-fvar c)))]
    [`(set! ,a ,b) `(set! ,(change-fvar a) ,(change-fvar b))]
    [`(return-point ,l ,t) `(return-point ,l ,(implement-tail t))]
    [_ #f]))
    

;
;(implement-tail t)->tail?
;t: tail?
(define (implement-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map implement-effect e) ,(implement-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(implement-pred p) ,(implement-tail t1) ,(implement-tail t2))]
    [`(jump ,trg) `(jump ,(change-fvar trg))]
    [_ #f]))


  
;
;(implement-func f)->'(define label? tail?)
;f: '(define label? tail?)
(define (implement-func f)
  (match f
    [`(define ,l ,t) `(define ,l ,(implement-tail t))]
    [_ #f]))

;
;(implement-fvars p) → Paren-cheri-risc-v-V2?
;p : Paren-cheri-risc-v-V2-fvars?
(define (implement-fvars p)
  (match p
    [`(module ,f ... ,t) `(module ,@(map implement-func f) ,(implement-tail t))]
    [_ "implement fvars failed"]))

(module+ test
  (define (check-fvar? a b m)
    (check-equal? a b m))
  #|
;fvar-number?
  ;succes
  (check-equal? (fvar-number? 'fv0) 0 "fvar-number?: succes-1: first frame fv")
  (check-equal? (fvar-number? 'fv1) 8 "fvar-number?: succes-2: second frame fv")
  (check-equal? (fvar-number? 'fv2) 16 "fvar-number?: succes-3: third frame fv")
  ;failure
  (check-equal? (fvar-number? 0) #f "fvar-number?: failure-1: integer")
  (check-equal? (fvar-number? 'x) #f "fvar-number?: failure-2: random symbol")
  (check-equal? (fvar-number? 'fv) #f "fvar-number?: failure-3: no number behind fv")
  (check-equal? (fvar-number? 'fv.1) #f "fvar-number?: failure-4: char between fv and number")
;change-fvar
  ;succes
  (check-equal? (change-fvar 'fv0) '(cfp - 0) "change-fvar: succes-1: single number fv")
  (check-equal? (change-fvar 'fv2) '(cfp - 16) "change-fvar: succes-2: double number fv")
  ;failure
  (check-equal? (change-fvar 0) 0 "change-fvar: failure-1: integer")
  (check-equal? (change-fvar 'x) 'x "change-fvar: failure-2: random symbol")
;implement-set
  ;succes
  (check-equal? (implement-set '(with-label L1 (set! fv0 fv1))) '(with-label L1 (set! (cfp - 0) (cfp - 8))) "implement-set: succes-01: with-label")
;implement-fvars
  ;succes
  (check-fvar? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! t0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! sp t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(0 (begin
                   (set! a0 50)
                   (set! t0 a0)
                   (set! t0 (+ t0 t0))
                   (set! sp t0)
                   (set! sp (* sp sp))
                   (set! a1 2000)))
                "implement-fvars: succes-1: no fvars")
  (check-fvar? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! fv0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! sp t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(8 (begin
                   (set! a0 50)
                   (set! (cfp - 0) a0)
                   (set! t0 (+ t0 t0))
                   (set! sp t0)
                   (set! sp (* sp sp))
                   (set! a1 2000)))
                "implement-fvars: succes-2: one fvars")
  (check-fvar? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! fv0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! fv0 t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(8 (begin
                   (set! a0 50)
                   (set! (cfp - 0) a0)
                   (set! t0 (+ t0 t0))
                   (set! (cfp - 0) t0)
                   (set! sp (* sp sp))
                   (set! a1 2000)))
                "implement-fvars: succes-3: two same fvars")
  (check-fvar? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! fv0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! fv1 t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(16 (begin
                   (set! a0 50)
                   (set! (cfp - 0) a0)
                   (set! t0 (+ t0 t0))
                   (set! (cfp - 8) t0)
                   (set! sp (* sp sp))
                   (set! a1 2000)))
                "implement-fvars: succes-4: two different fvars")
  (check-fvar? (implement-fvars '(module (define L.swap.1
             (begin
               (set! fv2 cra)
               (set! t0 fv0)
               (set! fv0 fv1)
               (if (< fv0 t0)
                 (begin (set! ca0 t0) (jump fv2))
                 (begin
                   (begin
                     (set! cfp (- cfp 24))
                     (return-point
                      L.rp-label.6
                      (begin
                        (set! fv4 t0)
                        (set! fv3 fv0)
                        (set! cra L.rp-label.6)
                        (jump L.swap.1)))
                     (set! cfp (+ cfp 24)))
                   (set! t0 ca0)
                   (begin (set! ca0 (+ t0 fv0)) (jump fv2))))))
  (begin
    (set! t0 cra)
    (begin (set! fv1 2) (set! fv0 1) (set! cra t0) (jump L.swap.1)))))
  '(module (define L.swap.1
             (begin
               (set! (cfp - 16) cra)
               (set! t0 (cfp - 0))
               (set! (cfp - 0) (cfp - 8))
               (if (< (cfp - 0) t0)
                 (begin (set! ca0 t0) (jump (cfp - 16)))
                 (begin
                   (begin
                     (set! cfp (- cfp 24))
                     (return-point
                      L.rp-label.6
                      (begin
                        (set! (cfp - 8) t0)
                        (set! (cfp - 0) (cfp - -24))
                        (set! cra L.rp-label.6)
                        (jump L.swap.1)))
                     (set! cfp (+ cfp 24)))
                   (set! t0 ca0)
                   (begin (set! ca0 (+ t0 (cfp - 0))) (jump (cfp - 16)))))))
  (begin
    (set! t0 cra)
    (begin
      (set! (cfp - 8) 2)
      (set! (cfp - 0) 1)
      (set! cra t0)
      (jump L.swap.1))))
  "implement-fvars: succes-05: value call")
  ;|#
  )