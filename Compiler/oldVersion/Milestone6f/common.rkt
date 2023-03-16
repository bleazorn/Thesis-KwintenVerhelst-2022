#lang racket

(provide indent
         indent-instr
         framesize
         fvar?
         resetfvar
         freshfvar
         getfvar
         setfvar
         isRegister?
         isCapability?
         isNonCapRegister?
         isAddress?
         isRegAddress?
         isCapAddress?
         aloc?
         resetfresh
         freshtmp
         freshLabel
         fresh
         name?
         isTmp?
         label?
         dec->hex-string
         fbp)

;(require cpsc411/compiler-lib)

        
(module+ test
  (require rackunit))

(define fbp 'cfp)

(define indent (make-string 4 #\ ))

(define (indent-instr instr)
  (format "~a~a\n" indent instr))

(define framesize 8)

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
  (string->symbol (string-append "fv" (~a fva))))

(define (info? v)
  (if (list? v)
      (foldl (lambda (e b) (and b (and (list? e) (equal? (length e) 2)))) #t v)
      #f))

(define (aloc? v)
  (if (symbol? v)
      (let* ([symv (symbol->string v)]
             [indexes (indexes-of (string->list symv) #\.)])
        (if (and (equal? (length indexes) 1) (string->number (substring symv (first indexes))))
            #t
            #f))
      #f))

(define (label? l)
  (if (symbol? l)
      (let* ([syml (symbol->string l)]
             [indexes (indexes-of (string->list syml) #\.)])
        (if (and (equal? (length indexes) 2) (and (equal? "L" (substring syml 0 (first indexes))) (string->number (substring syml (second indexes)))))
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
  (string->symbol (format "~a.~a" v fres)))

(define (freshLabel l)
  (set! fres (add1 fres))
  (string->symbol (format "L.~a.~a" l fres)))

(define (isTmp? t)
  (if (symbol? t)
      (let* ([symv (symbol->string t)]
             [indexes (indexes-of (string->list symv) #\.)])
        (if (and (and (equal? (length indexes) 1) (string->number (substring symv (first indexes)))) (equal? "tmp" (substring symv 0 (first indexes))))
            #t 
            #f))
      #f))

(define (name? v)
  (symbol? v))

;Returns given symbol if it is a name for a register in paren-cheri-risc-v, otherwise returns false
;(check-reg res) -> symbol?/boolean?
;res : symbol?
(define (isRegister? res)
  (or (isCapability? res) (isNonCapRegister? res)))

(define (isCapability? cap)
  (if (symbol? cap)
      (match cap
        ['cnull 'cnull]
        ['cra 'ra]
        ['csp 'sp]
        ['cgp 'gp]
        ['ctp 'tp]
        ['cfp 'fp]
        ['cpc 'pc]
        [_ (let* ([s (symbol->string cap)]
                  [l (substring s 0 1)]
                  [n (substring s 1)])
             (match `(,l ,n)
               [`("c" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 32))) cap]
               [_ (let* ([cl (substring s 0 2)]
                         [cn (substring s 2)])
                    (match `(,cl ,cn)
                      [`("ca" ,cn) #:when (and (string->number cn) (and (<= 0 (string->number cn)) (< (string->number cn) 8))) cap]
                      [`("cs" ,cn) #:when (and (string->number cn) (and (<= 0 (string->number cn)) (< (string->number cn) 12))) cap]
                      [`("ct" ,cn) #:when (and (string->number cn) (and (<= 0 (string->number cn)) (< (string->number cn) 7))) cap]
                      [_ #f]))]))])
      #f))

(define (isNonCapRegister? res)
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

;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (isAddress? a)
  (match a
    [`(,r - ,n) #:when (and (isRegister? r) (integer? n)) a]
    [_ #f]))

;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (isRegAddress? a)
  (match a
    [`(,r - ,n) #:when (and (isRegister? r) (integer? n)) a]
    [_ #f]))

;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (isCapAddress? a)
  (match a
    [`(,r - ,n) #:when (and (isCapability? r) (integer? n)) a]
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


(module+ test
;fvar?
  ;succes
  (check-true (fvar? 'fv0) "fvar?: succes-1: single number fv")
  (check-true (fvar? 'fv20) "fvar?: succes-2: double number fv")
  ;failure
  (check-false (fvar? 0) "fvar?: failure-1: integer")
  (check-false (fvar? 'x) "fvar?: failure-2: random symbol")
  (check-false (fvar? 'fv) "fvar?: failure-3: no number behind fv")
  (check-false (fvar? 'fv.1) "fvar?: failure-4: char between fv and number")
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
;isAddress?
  ;succes
  (check-equal? (isAddress? '(a0 - 5)) '(a0 - 5) "isAddress?: succes-1: an address")
  ;failure
  (check-false (isAddress? "(a0 - 5)") "isAddress?: failure-1: not a symbol")
  (check-false (isAddress? '(a10 - 5)) "isAddress?: failure-2: not a register")
  (check-false (isAddress? '(a0 - a0)) "isAddress?: failure-3: not a number")
  )
