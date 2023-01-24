#lang racket

(require "common/fvar.rkt"
         "common/register.rkt")
(provide implement-fvars)

(module+ test
  (require rackunit))

;!!!DECISION: fv0 = (fp - 0) fp is frame pointer and frame size is 8

(define offset 0)
(define (resetOffSet)
  (setOffSet 0))
(define (setOffSet n)
  (set! offset n))
(define (addOffSet binop n)
  (match binop
    ['- (set! offset (- offset n))]
    ['+ (set! offset (+ offset n))]
    [_ #f]))

; returns (n-1)*8 with n the number of the fvar
;(addr-number v) -> integer?
; v: fvar?
(define (addr-number v)
  (if (fvar? v)
      (let* ([n (add1 (getFvarNumber v))])
        (+ (* n (framesize)) offset))
      #f))

;if given argument is of type fvar, transforms it into (fp - dispoffset)
;(change-fvar fvar)-> symbol?
;fvar: any?
(define (change-fvar f)
  (if (fvar? f)
      `(,(fvarRegister) ,(stack-direction) ,(addr-number f))
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
    [`(setLinear! ,a ,b) `(setLinear! ,(change-fvar a) ,(change-fvar b))]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) (addOffSet - d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) (addOffSet + d) `(splice ,a ,b ,c ,d)]
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
    [`(invoke ,a ,b) `(invoke ,a ,b)]
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
  (resetOffSet)
  (match p
    [`(module ,f ... ,t) `(module ,@(map implement-func f) ,(implement-tail t))]
    [_ "implement fvars failed"]))

(module+ test
  (define (check-fvar? f a b o m)
    (setOffSet o)
    (check-equal? (f a) b m))
  ;#|
;addr-number
  ;succes
  (check-fvar? addr-number 'fv0 16 0 "addr-number: succes-01: first frame fv")
  (check-fvar? addr-number 'fv1 32 0 "addr-number: succes-02: second frame fv")
  (check-fvar? addr-number 'fv2 48 0 "addr-number: succes-03: third frame fv")
  
  (check-fvar? addr-number 'fv0 24 8 "addr-number: succes-04: first frame fv offset 8")
  (check-fvar? addr-number 'fv1 40 8 "addr-number: succes-05: second frame fv offset 8")
  (check-fvar? addr-number 'fv2 56 8 "addr-number: succes-06: third frame fv offset 8")
  
  (check-fvar? addr-number 'fv0 8 -8 "addr-number: succes-07: first frame fv offset -8")
  (check-fvar? addr-number 'fv1 24 -8 "addr-number: succes-08: second frame fv offset- 8")
  (check-fvar? addr-number 'fv2 40 -8 "addr-number: succes-09: third frame fv offset -8")
  ;failure
  (check-equal? (addr-number 0) #f "addr-number: failure-1: integer")
  (check-equal? (addr-number 'x) #f "addr-number: failure-2: random symbol")
  (check-equal? (addr-number 'fv) #f "addr-number: failure-3: no number behind fv")
  (check-equal? (addr-number 'fv.1) #f "addr-number: failure-4: char between fv and number")
;change-fvar
  ;succes
  (check-fvar? change-fvar 'fv0 '(cfp - 16) 0 "change-fvar: succes-1: single number fv")
  (check-fvar? change-fvar 'fv2 '(cfp - 48) 0 "change-fvar: succes-2: double number fv")

  (check-fvar? change-fvar 'fv0 '(cfp - 24) 8 "change-fvar: succes-3: single number fv offset 8")
  (check-fvar? change-fvar 'fv2 '(cfp - 56) 8 "change-fvar: succes-4: double number fv offset 8")

  (check-fvar? change-fvar 'fv0 '(cfp - 8) -8 "change-fvar: succes-5: single number fv offset -8")
  (check-fvar? change-fvar 'fv2 '(cfp - 40) -8 "change-fvar: succes-6: double number fv offset -8")
  ;failure
  (check-equal? (change-fvar 0) 0 "change-fvar: failure-1: integer")
  (check-equal? (change-fvar 'x) 'x "change-fvar: failure-2: random symbol")
;implement-set
  ;succes
  ;(check-equal? (implement-set '(with-label L1 (set! fv0 fv1))) '(with-label L1 (set! (cfp - 0) (cfp - 8))) "implement-set: succes-01: with-label")
;implement-fvars
  ;succes
  (check-fvar? implement-fvars
               '(module (define L.swap.1
                          (begin
                            (set! fv2 cra)
                            (set! t0 fv0)
                            (set! fv0 fv1)
                            (if (< fv0 t0)
                                (begin (set! ca0 t0) (jump fv2))
                                (begin
                                  (begin
                                    (set! cfp (- cfp 48))
                                    (return-point L.rp-label.6
                                                  (begin
                                                    (set! fv4 t0)
                                                    (set! fv3 fv0)
                                                    (set! cra L.rp-label.6)
                                                    (jump L.swap.1)))
                                    (set! cfp (+ cfp 48)))
                                  (set! t0 ca0)
                                  (begin (set! ca0 (+ t0 fv0)) (jump fv2))))))
                  (begin
                    (set! t0 cra)
                    (begin (set! fv1 2) (set! fv0 1) (set! cra t0) (jump L.swap.1))))
               '(module (define L.swap.1
                          (begin
                            (set! (cfp - 48) cra)
                            (set! t0 (cfp - 16))
                            (set! (cfp - 16) (cfp - 32))
                            (if (< (cfp - 16) t0)
                                (begin (set! ca0 t0) (jump (cfp - 48)))
                                (begin
                                  (begin
                                    (set! cfp (- cfp 48))
                                    (return-point L.rp-label.6
                                                  (begin
                                                    (set! (cfp - 32) t0)
                                                    (set! (cfp - 16) (cfp - -32))
                                                    (set! cra L.rp-label.6)
                                                    (jump L.swap.1)))
                                    (set! cfp (+ cfp 48)))
                                  (set! t0 ca0)
                                  (begin (set! ca0 (+ t0 (cfp - 16))) (jump (cfp - 48)))))))
                  (begin (set! t0 cra)
                         (begin (set! (cfp - 32) 2) (set! (cfp - 16) 1) (set! cra t0) (jump L.swap.1))))
               0
               "implement-fvars: succes-01: value call")
;|#
  )