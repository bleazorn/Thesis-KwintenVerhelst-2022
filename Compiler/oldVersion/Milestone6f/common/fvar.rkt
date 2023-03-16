#lang racket

(provide framesize
         fvar?
         resetfvar
         getfvar
         setfvar
         freshfvar
         newFvar
         getFvarNumber
         getFirstAvailableFvar)

(define framesize 16)

(define (fvar? v)
  (if (symbol? v)
      (let ([s (symbol->string v)])
        (if (> (string-length s) 1)
            (let ([fv (substring s 0 2)]
                  [n (string->number (substring s 2))])
              (and (equal? fv "fv") (and (integer? n) (>= n 0))))
            #f))
      #f))

(define fva -1)

(define (getfvar)
  fva)

(define (setfvar n)
  (set! fva n))

(define (resetfvar)
  (set! fva -1))

(define (freshfvar)
  (set! fva (add1 fva))
  (newFvar fva))

(define (newFvar n)
  (string->symbol (format "fv~a" n)))

(define (getFvarNumber f)
  (if (fvar? f)
      (string->number (substring (symbol->string f) 2))
      #f))

(define (getFirstAvailableFvar fs)
  (define (sortFvar f1 f2)
    (< (getFvarNumber f1) (getFvarNumber f2)))
  (let ([res (findf (lambda (f) (not (member f fs))) (build-list (length fs) newFvar))])
    (if res
        res
        (newFvar (length fs)))))

(module+ test
  (require rackunit))
