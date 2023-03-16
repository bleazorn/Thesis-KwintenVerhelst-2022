#lang racket

(require "common/register.rkt"
         "langs/nested-asm-lang-jumps.rkt")

(provide add-stktokens-seal)

(module+ test
  (require rackunit))

(define (add-tail t)
  `(begin (set! ,(current-seal-location-register) ,(current-invoke-data-register))
          ,t))
;
;(change-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (add-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(add-tail t))]
    [_ #t]))


(define/contract (add-stktokens-seal p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map add-func f) ,(add-tail t))]
    [_ "replace locations failed"]))