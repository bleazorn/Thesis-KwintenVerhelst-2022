#lang racket

(require "register.rkt")

(provide framesize
         stack-direction
         global-direction
         opposite-direction
         fvar?
         ffvar?
         fgvar?
         resetfvar
         getfvar
         setfvar
         freshfvar
         newFvar
         freshsvar
         newSvar
         newGvar
         getFvarNumber
         getFirstAvailableFvar
         maxFvarNumber)

(module+ test
  (require rackunit))

(define framesize
  (make-parameter 16))

(define stack-direction
  (make-parameter '-))

(define global-direction
  (make-parameter '+))

(define (opposite-direction binop)
  (match binop
    ['+ '-]
    ['- '+]))

(define (fvar? v)
  (if (symbol? v)
      (let ([s (symbol->string v)])
        (if (> (string-length s) 1)
            (let ([fv (substring s 0 2)]
                  [n (string->number (substring s 2))])
              (and (or (equal? fv "fv") (equal? fv "sv") (equal? fv "gv")) (and (integer? n) (>= n 0))))
            #f))
      #f))

(define (ffvar? v)
  (if (symbol? v)
      (let ([s (symbol->string v)])
        (if (> (string-length s) 1)
            (let ([fv (substring s 0 2)]
                  [n (string->number (substring s 2))])
              (and (equal? fv "fv") (integer? n) (>= n 0)))
            #f))
      #f))

(define (fgvar? v)
  (if (symbol? v)
      (let ([s (symbol->string v)])
        (if (> (string-length s) 1)
            (let ([fv (substring s 0 2)]
                  [n (string->number (substring s 2))])
              (and (equal? fv "gv") (integer? n) (>= n 0)))
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

(define (freshsvar)
  (set! fva (add1 fva))
  (newSvar fva))

(define (newSvar n)
  (string->symbol (format "sv~a" n)))

(define (newGvar n)
  (string->symbol (format "gv~a" n)))

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

(define (maxFvarNumber fs)
  (foldl (lambda (f n) (cond [(and (fvar? f) (< n (getFvarNumber f))) (getFvarNumber f)]
                             [else n]))
         0 fs))

(module+ test
;fvar?
  ;succes
  (check-true (fvar? 'fv0) "fvar?: succes-01: zero fv")
  (check-true (fvar? 'fv2) "fvar?: succes-02: two fv")
  ;failure
  (check-false (fvar? 0) "fvar?: failure-01: integer")
  (check-false (fvar? 'x) "fvar?: failure-02: 'x")
  (check-false (fvar? 'x.1) "fvar?: failure-03: 'x.1")
  (check-false (fvar? 'fv) "fvar?: failure-04: 'fv")
  (check-false (fvar? 'fv.1) "fvar?: failure-05: 'fv.1")
)
