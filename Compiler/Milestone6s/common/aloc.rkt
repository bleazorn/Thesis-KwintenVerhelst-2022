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
  (require rackunit))
