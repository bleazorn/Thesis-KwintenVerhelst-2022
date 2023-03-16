#lang racket

(require "common.rkt")
(provide implement-fvars)

(module+ test
  (require rackunit))

;!!!DECISION: fv0 = (fp - 0) fp is frame pointer and frame size is 8

(define maxStackLength -8)
(define (resetMaxStack)
  (set! maxStackLength -8))
(define (setMaxStackLength n)
  (if (> n maxStackLength)
      (set! maxStackLength n)
      void))

; returns (n-1)*8 with n the number of the fvar
;(fvar-number? v) -> integer?
; v: fvar?
(define (fvar-number? v)
  (if (fvar? v)
      (let* ([s (symbol->string v)]
             [n (string->number (substring s 2))]
             [frLength (* n framesize)])
        (setMaxStackLength frLength)
        frLength)
      #f))

;if given argument is of type fvar, transforms it into (fp - dispoffset)
;(change-fvar fvar)-> symbol?
;fvar: any?
(define (change-fvar fvar)
  (if (fvar? fvar)
      `(cfp - ,(fvar-number? fvar))
      fvar))

;
;(implement-set fvar)-> symbol?
;set: any?
(define (implement-set set)
  (match set
    [`(set! ,a ,b) `(set! ,(change-fvar a) ,(change-fvar b)) ]
    [`(with-label ,a ,b) `(with-label ,a ,(implement-set b))]
    [`(jump ,a) `(jump ,a )]
    [`(compare ,a ,b) `(compare ,a ,b)]
    [`(jump-if ,a ,b) `(jump-if ,a ,b)]
    [_ #f]))

;
;(implement-fvars p) → Paren-cheri-risc-v-V2?
;p : Paren-cheri-risc-v-V2-fvars?
(define (implement-fvars p)
  (match p
    [`(begin ,s ...) (let ([beg `(begin ,@(map (lambda (set) (implement-set set)) s))])
                       `(,(+ maxStackLength framesize) ,beg))]
    [_ #f]))

(module+ test
  (define (check-fvar? a b m)
    (resetMaxStack)
    (check-equal? a b m))
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
  (resetMaxStack)
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
  )