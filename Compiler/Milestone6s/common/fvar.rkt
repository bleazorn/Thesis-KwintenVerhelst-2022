#lang racket

(require "register.rkt")

(provide framesize
         stack-direction
         fvar?
         resetfvar
         getfvar
         setfvar
         freshfvar
         newFvar
         freshsvar
         newSvar
         getFvarNumber
         getFirstAvailableFvar
         maxFvarNumber
         fvarRegister)

(define framesize
  (make-parameter 16))

(define stack-direction
  (make-parameter '-))

(define (fvar? v)
  (if (symbol? v)
      (let ([s (symbol->string v)])
        (if (> (string-length s) 1)
            (let ([fv (substring s 0 2)]
                  [n (string->number (substring s 2))])
              (and (or (equal? fv "fv") (equal? fv "sv")) (and (integer? n) (>= n 0))))
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

(define fvarRegister
  (make-parameter (current-frame-base-pointer-register)))

(module+ test
  (require rackunit))
