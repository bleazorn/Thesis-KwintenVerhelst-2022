#lang racket

(require "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt")

(provide add-stktokens-sentry)

(module+ test
  (require rackunit))

(define (add-tail t)
  `(begin  (set! ,(current-seal-location-register) pcc)
           (set! t5 #x80002000)
           (set-addr! ,(current-seal-location-register) t5)
          ,t))
;
;(change-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (add-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(add-tail t))]
    [_ #t]))


(define/contract (add-stktokens-sentry p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map add-func f) ,(add-tail t))]
    [_ "replace locations failed"]))