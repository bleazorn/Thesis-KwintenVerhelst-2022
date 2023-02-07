#lang racket

(require "common/info.rkt"
         "langs/nested-asm-lang-jumps.rkt")
(provide create-got)

(define (create-info i f)
  (addInfo '() (setGOTLabels (map list (map second f) (build-list (length f) (lambda (n) (* 2 n)))))))
  


(define/contract (create-got p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(create-info i f) ,@f ,t)]
    [_ "replace locations failed"]))