#lang racket

(provide indent	
         indent-instr	
         dec->hex-string)

(define indent (make-string 4 #\ ))

(define (indent-instr instr)
  (format "~a~a\n" indent instr))

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

(module+ test
  (require rackunit))