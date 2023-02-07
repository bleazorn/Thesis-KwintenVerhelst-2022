#lang racket

(provide name?
         aloc?			
         label?			
         isTmp?
         resetfresh
         freshAloc		
         freshtmp		
         freshLabel
         freshLabelTmp
         freshNfv
         freshTmpRa
         freshRPLabel)

(module+ test
  (require rackunit))

(define (name? v)
  (symbol? v))

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

(define (isTmp? t)
  (if (symbol? t)
      (let* ([symv (symbol->string t)]
             [indexes (indexes-of (string->list symv) #\.)])
        (if (and (and (equal? (length indexes) 1) (string->number (substring symv (first indexes)))) (equal? "tmp" (substring symv 0 (first indexes))))
            #t 
            #f))
      #f))

(define fres 0)

(define (resetfresh)
  (set! fres 0))

(define (freshAloc v)
  (set! fres (add1 fres))
  (string->symbol (format "~a.~a" v fres)))

(define (freshtmp)
  (set! fres (add1 fres))
  (string->symbol (format "tmp.~a" fres)))

(define (freshLabel l)
  (set! fres (add1 fres))
  (string->symbol (format "L.~a.~a" l fres)))

(define (freshLabelTmp)
  (set! fres (add1 fres))
  (string->symbol (format "L.tmp.~a" fres)))

(define (freshNfv)
  (freshAloc "nfv"))

(define (freshTmpRa)
  (freshAloc "tmp-ra"))

(define (freshRPLabel)
  (freshLabel "rpLabel"))

(module+ test
;name?
  ;succes
  (check-true (name? 'x) "name?: succes-01: single letter")
  (check-true (name? 'even?) "name?: succes-02: multiple letters")
  (check-true (name? 'L.start.1) "name?: succes-03: 'L.start.1")
  (check-true (name? 'Lstart.1) "name?: succes-04: 'Lstart.1")
  (check-true (name? 'L.start1) "name?: succes-05: 'L.start1")
  (check-true (name? 'start.1) "name?: succes-06: 'start.1")
  ;failure
  (check-false (name? 0) "name?: failure-01: integer")
  (check-false (name? "even?") "name?: failure-02: string")
;aloc?
  ;succes
  (check-true (aloc? 'x.1) "aloc?: succes-01: x.1")
  (check-true (aloc? 'Lstart.1) "aloc?: succes-02: L.x.1")
  (check-true (aloc? 'start.1) "aloc?: succes-02: L.x.1")
  ;failure
  (check-false (aloc? 0) "aloc?: failure-01: integer")
  (check-false (aloc? "x.1") "aloc?: failure-02: string")
  (check-false (aloc? 'x) "aloc?: failure-03: 'x")
  (check-false (aloc? 'L.start.1) "aloc?: failure-04: 'L.start.1")
  (check-false (aloc? 'L.start1) "aloc?: failure-05: 'L.start1")
  (check-false (aloc? 'start) "aloc?: failure-06: 'start")
;label?
  ;succes
  (check-true (label? 'L.start.1) "label?: succes-01: zero")
  ;failure
  (check-false (label? 0) "label?: failure-01: integer")
  (check-false (label? "L.start.1") "label?: failure-02: string")
  (check-false (label? 'Lstart.1) "label?: failure-03: 'Lstart.1")
  (check-false (label? 'L.start1) "label?: failure-04: 'L.start1")
  (check-false (label? 'start.1) "label?: failure-05: 'start.1")
  )
