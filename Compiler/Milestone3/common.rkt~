#lang racket

(provide fvar?
         resetfvar
         freshfvar
         isRegister?
         aloc?
         resetfresh
         freshtmp
         fresh
         name?)
         
(module+ test
  (require rackunit))

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

(define (resetfvar)
  (set! fva -1))

(define (freshfvar)
  (set! fva (add1 fva))
  (string->symbol (string-append "fv" (~a fva))))

(define (info? v)
  (if (list? v)
      (foldl (lambda (e b) (and b (and (list? e) (equal? (length e) 2)))) #t v)
      #f))

(define (aloc? v)
  (if (symbol? v)
      (let* ([symv (symbol->string v)]
             [indexes (indexes-of (string->list symv) #\.)])
        (if (and (equal? (length indexes) 1) (string->number (substring (symbol->string v) (first indexes))))
            #t 
            #f))
      #f))


(define fres 0)

(define (resetfresh)
  (set! fres 0))

(define (freshtmp)
  (set! fres (add1 fres))
  (string->symbol (format "tmp.~a"fres)))

(define (fresh v)
  (set! fres (add1 fres))
  (if (symbol? v)
      (string->symbol (format "~a.~a" v fres))
      #f))

(define (name? v)
  (symbol? v))

;Returns given symbol if it is a name for a register in paren-cheri-risc-v, otherwise gives error message
;(check-reg res) -> symbol?
;res : symbol?
(define (isRegister? res)
  (if (symbol? res)
      (match res
        ['zero 'zero]
        ['ra 'ra]
        ['sp 'sp]
        ['gp 'gp]
        ['tp 'tp]
        ['fp 'fp]
        ['pc 'pc]
        [_ (let* ([s (symbol->string res)]
                  [l (substring s 0 1)]
                  [n (substring s 1)])
             (match `(,l ,n)
               [`("x" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 32))) res]
               [`("a" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 8))) res]
               [`("s" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 12))) res]
               [`("t" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 7))) res]
               [_ #f]))])
      #f))

(module+ test
;fvar?
  ;succes
  (check-true (fvar? 'fv1) "fvar?: succes-1: single number fv")
  (check-true (fvar? 'fv20) "fvar?: succes-2: double number fv")
  ;failure
  (check-false (fvar? 0) "fvar?: failure-1: integer")
  (check-false (fvar? 'x) "fvar?: failure-2: random symbol")
  (check-false (fvar? 'fv) "fvar?: failure-3: no number behind fv")
  (check-false (fvar? 'fv.1) "fvar?: failure-4: char between fv and number")
  (check-false (fvar? 'fv0) "fvar?: failure-5: number is 0, fv starts with 1")
;aloc?
  ;succes
  (check-true (aloc? 'x.1) "aloc?: succes-1: single letter single didget")
  (check-true (aloc? 'Lstart.1) "aloc?: succes-2: more letters")
  (check-true (aloc? 'Lstart.12) "aloc?: succes-3: more letters more numbers")
  ;failure
  (check-false (aloc? 0) "aloc?: failure-1: integer")
  (check-false (aloc? "x") "aloc?: failure-2: string")
  (check-false (aloc? 'x) "aloc?: failure-3: random symbol")
  (check-false (aloc? 'L.start1) "aloc?: failure-4: no number behind .")
  (check-false (aloc? 'L.start.1) "aloc?: failure-5: multiple dots")
;info?
  ;succes
  (check-true (info? '()) "info?: succes-1: empty list")
  (check-true (info? '((a 5))) "info?: succes-2: one info")
  ;failure
  (check-false (info? '((a . 5))) "info?: failure-1: pair")
  (check-false (info? "5") "info?: failure-2: not a list")
;info/c
  #|
  ;succes
  (check-equal? (info/c) '() "info/c: succes-1: single letter single didget")
  (check-equal? (info/c (locals (aloc? ...))) "info/c: succes-1: single letter single didget")
  (check-equal? (info/c 'x.1) "info/c: succes-1: single letter single didget")
  (check-equal? (info/c 'x.1) "info/c: succes-1: single letter single didget")
  (check-equal? (info/c 'x.1) "info/c: succes-1: single letter single didget")
  (check-equal? (info/c 'x.1) "info/c: succes-1: single letter single didget")
  ;failure
  (check-equal? (info/c) 0 "info/c: failure-1: integer")
  (check-equal? (info/c (locals (aloc? ...)))  '() "info/c: failure-1: integer")
  (check-equal? (info/c 0) "info/c: failure-1: integer")
  (check-equal? (info/c 0) "info/c: failure-1: integer")
  |#
  )