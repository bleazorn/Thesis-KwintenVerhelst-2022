#lang racket

(require "register.rkt")

(provide indent	
         indent-instr	
         dec->hex-string
         hex->dex
         permissions
         permissionToInt
         setPerms
         removePerm
         int64?
         int32?
         int16?)


(module+ test
  (require rackunit))

(define indent (make-string 4 #\ ))

(define (indent-instr instr)
  (format "~a~a\n" indent instr))

(define permissions
  '(global
    execute
    load
    store
    storeCap
    loadCap
    storeLoc
    seal
    invole
    unseal
    ASR
    cid))

(define (permissionToInt p)
  (let ([i (index-of permissions p)])
    (if i
        (expt 2 i)
        0)))


(define (setPerms r ps)
  `(perm ,r ,ps))

(define (removePerm p rp)
  (match p
    [`(perm ,r ,ps) #:when (and (register? r) (list? ps) (member rp ps)) `(perm ,r ,(remove rp ps))]
    [_ #f]))

;Converts a number less then 16 to its hex
;(getHexSymbol n)->string?
;n: integer?
(define (getHexSymbol n)
  (cond [(and (< n 10) (>= n 0)) (number->string n)]
        [(= n 10) "a"]
        [(= n 11) "b"]
        [(= n 12) "c"]
        [(= n 13) "d"]
        [(= n 14) "e"]
        [(= n 15) "f"]
        [else #f])) 

;converts a number to its hex
;(dec->hex-build n)->string?
;n: integer?
(define (dec->hex-build n)
  (if (< n 16)
      (getHexSymbol n)
      (string-append (dec->hex-string (quotient n 16)) (getHexSymbol (remainder n 16)))))

;converts a number to its hex, but gives an error if something went wrong in the calculation
;(dec->hex-string n)->string?
;n: integer?
(define (dec->hex-string n)
  (let ([x (dec->hex-build n)])
    (if (equal? n  (string->number (format "#x~a" x)))
        x
        (error (format "something went wrong with calculating the hexidecimal for ~a" n)))))

(define (hex->dex h)
  h)

(define (intBinBound i b)
  (let ([min (- 0 (expt 2 (sub1 b)))]
        [max (expt 2 (sub1 b))])
    (and (integer? i) (<= min i) (< i max))))

(define (int16? i)
  (intBinBound i 16))

(define (int32? i)
  (intBinBound i 32))

(define (int64? i)
  (intBinBound i 64))
  



(module+ test
;int16?
  ;succes
  (check-true (int16? 0) "int16?: succes-01: zero")
  (check-true (int16? 123) "int16?: succes-02: pos")
  (check-true (int16? -123) "int16?: succes-03: neg")
  (check-true (int16? 32767) "int16?: succes-04: max")
  (check-true (int16? -32768) "int16?: succes-05: min")
  ;failure
  (check-false (int16? 32768) "int16?: failure-01: max + 1")
  (check-false (int16? -32769) "int16?: failure-02: min + 1")
  (check-false (int16? 65536) "int16?: failure-03: double max")
  (check-false (int16? -65536) "int16?: failure-04: double min")
;int32?
  ;succes
  (check-true (int32? 0) "int32?: succes-01: zero")
  (check-true (int32? 123) "int32?: succes-02: pos")
  (check-true (int32? -123) "int32?: succes-03: neg")
  (check-true (int32? 2147483647) "int32?: succes-04: max")
  (check-true (int32? -2147483648) "int32?: succes-05: min")
  ;failure
  (check-false (int32? 2147483648) "int32?: failure-01: max + 1")
  (check-false (int32? -2147483649) "int32?: failure-02: min + 1")
  (check-false (int32? 4294967296) "int32?: failure-03: double max")
  (check-false (int32? -4294967296) "int32?: failure-04: double min")
;int64?
  ;succes
  (check-true (int64? 0) "int64?: succes-01: zero")
  (check-true (int64? 123) "int64?: succes-02: pos")
  (check-true (int64? -123) "int64?: succes-03: neg")
  (check-true (int64? 9223372036854775807) "int64?: succes-04: max")
  (check-true (int64? -9223372036854775808) "int64?: succes-05: min")
  ;failure
  (check-false (int64? 9223372036854775808) "int64?: failure-01: max + 1")
  (check-false (int64? -9223372036854775809) "int64?: failure-02: min + 1")
  (check-false (int64? 18446744073709551616) "int64?: failure-03: double max")
  (check-false (int64? -18446744073709551616) "int64?: failure-04: double min")
  )