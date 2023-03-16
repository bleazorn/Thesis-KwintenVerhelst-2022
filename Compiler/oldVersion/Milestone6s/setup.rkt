#lang racket

(require "common/assembly.rkt")

(provide initialPerms
         initialBounds
         top-stack
         resetSealAddr
         getSealAddr
         addSealAddr
         setUpExprs)

(define initialPerms
  (make-parameter '()))

(define initialBounds
  (make-parameter '()))

(define top-stack
  (make-parameter "81f00000"))

(define sealAddr '())

(define (resetSealAddr)
  (set! sealAddr '()))

(define (getSealAddr i)
  (second (assoc i sealAddr)))

(define (addSealAddr i addr)
  (set! sealAddr (cons `(,i ,addr) sealAddr)))

(define setUpExprs
  (make-parameter `(begin (perm cfp (global
                                     load
                                     store
                                     storeCap
                                     loadCap
                                     storeLoc))
                          (perm cs11 (global
                                           execute))
                          (set! t0 ,(hex->dex #x81000000))
                          (set! t1 ,(hex->dex #xf20000))
                          (bound cfp t0 t1))))

