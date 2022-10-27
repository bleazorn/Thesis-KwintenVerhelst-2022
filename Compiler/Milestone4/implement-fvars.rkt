#lang racket

(require "common.rkt")
(provide implement-fvars)

(module+ test
  (require rackunit))

;!!!DECISION: fv0 = (fp - 0) fp is frame pointer and frame size is 8

; returns (n-1)*8 with n the number of the fvar
;(fvar-number? v) -> integer?
; v: fvar?
(define (fvar-number? v)
  (if (fvar? v)
      (let* ([s (symbol->string v)]
             [n (string->number (substring s 2))])
              (* n framesize))
      #f))

;if given argument is of type fvar, transforms it into (fp - dispoffset)
;(change-fvar fvar)-> symbol?
;fvar: any?
(define (change-fvar fvar)
  (if (fvar? fvar)
      `(fp - ,(fvar-number? fvar))
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
    [`(begin ,s ...) `(begin ,@(map (lambda (set) (implement-set set)) s))]
    [_ #f]))

(module+ test
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
  (check-equal? (change-fvar 'fv0) '(fp - 0) "change-fvar: succes-1: single number fv")
  (check-equal? (change-fvar 'fv2) '(fp - 16) "change-fvar: succes-2: double number fv")
  ;failure
  (check-equal? (change-fvar 0) 0 "change-fvar: failure-1: integer")
  (check-equal? (change-fvar 'x) 'x "change-fvar: failure-2: random symbol")
;implement-fvars
  ;succes
  (check-equal? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! t0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! sp t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(begin
                   (set! a0 50)
                   (set! t0 a0)
                   (set! t0 (+ t0 t0))
                   (set! sp t0)
                   (set! sp (* sp sp))
                   (set! a1 2000))
                "implement-fvars: succes-1: no fvars")
  (check-equal? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! fv0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! sp t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(begin
                   (set! a0 50)
                   (set! (fp - 0) a0)
                   (set! t0 (+ t0 t0))
                   (set! sp t0)
                   (set! sp (* sp sp))
                   (set! a1 2000))
                "implement-fvars: succes-2: one fvars")
  (check-equal? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! fv0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! fv0 t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(begin
                   (set! a0 50)
                   (set! (fp - 0) a0)
                   (set! t0 (+ t0 t0))
                   (set! (fp - 0) t0)
                   (set! sp (* sp sp))
                   (set! a1 2000))
                "implement-fvars: succes-3: two same fvars")
  (check-equal? (implement-fvars '(begin
                                    (set! a0 50)
                                    (set! fv0 a0)
                                    (set! t0 (+ t0 t0))
                                    (set! fv1 t0)
                                    (set! sp (* sp sp))
                                    (set! a1 2000)))
                '(begin
                   (set! a0 50)
                   (set! (fp - 0) a0)
                   (set! t0 (+ t0 t0))
                   (set! (fp - 8) t0)
                   (set! sp (* sp sp))
                   (set! a1 2000))
                "implement-fvars: succes-4: two different fvars")
  )