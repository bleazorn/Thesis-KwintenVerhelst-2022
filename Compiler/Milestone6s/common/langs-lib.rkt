#lang racket

(require "aloc.rkt"
         "fvar.rkt"
         "info.rkt"
         "register.rkt"
         "assembly.rkt")

(provide name?
         aloc?
         label?
         fvar?
         info?
         register?
         addr?
         int64?
         int32?
         int16?
         frame-base-pointer-register?
         dispoffset?)

(define (dispoffset? n)
  (= (remainder n (framesize)) 0))
