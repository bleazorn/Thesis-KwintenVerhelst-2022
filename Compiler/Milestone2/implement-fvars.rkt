#lang racket

(require "common.rkt")

(module+ test
  (require rackunit))

; returns (n-1)*8 with n the number of the fvar
;(fvar-number? v) -> integer?
; v: fvar?
(define (fvar-number? v)
  (if (fvar? v)
      (let* ([s (symbol->string v)]
             [n (string->number (substring s 2))])
              (* (- n 1) 8))
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
    [`(set! ,fvar ,a) `(set! ,(change-fvar fvar) ,a) ]
    [_ #f]))

;
;(implement-fvars p) → paren-cheri-risc-v-v2?
;p : paren-cheri-risc-v-fvars-v2?
(define (implement-fvars p)
  (match p
    [`(begin ,s ...) (map (lambda (set) (implement-set set)) s)]
    [_ #f]))

(module+ test
;fvar-number?
  ;succes
  (check-equal? (fvar-number? 'fv1) 0 "fvar-number?: succes-1: first frame fv")
  (check-equal? (fvar-number? 'fv2) 8 "fvar-number?: succes-2: second frame fv")
  (check-equal? (fvar-number? 'fv3) 16 "fvar-number?: succes-3: third frame fv")
  ;failure
  (check-equal? (fvar-number? 0) #f "fvar-number?: failure-1: integer")
  (check-equal? (fvar-number? 'x) #f "fvar-number?: failure-2: random symbol")
  (check-equal? (fvar-number? 'fv) #f "fvar-number?: failure-3: no number behind fv")
  (check-equal? (fvar-number? 'fv.1) #f "fvar-number?: failure-4: char between fv and number")
;change-fvar
  ;succes
  (check-equal? (change-fvar 'fv1) '(fp - 0) "change-fvar: succes-1: single number fv")
  (check-equal? (change-fvar 'fv3) '(fp - 16) "change-fvar: succes-2: double number fv")
  ;failure
  (check-equal? (change-fvar 0) 0 "change-fvar: failure-1: integer")
  (check-equal? (change-fvar 'x) 'x "change-fvar: failure-2: random symbol")
  )