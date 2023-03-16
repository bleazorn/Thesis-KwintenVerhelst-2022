#lang racket

(require "common/info.rkt"
         "common/fvar.rkt")
(provide assign-call-undead-variables)

(module+ test
  (require rackunit))

;
;(assign-recur loc conf assign)->list? '((aloc loc) ...)
;loc:list? '(aloc ...)
;conf:list? '((aloc? (...)) ...)
;assign: list? '((aloc loc) ...)
(define (assign-call-undead calls conf fs confDel)
  ;(println confDel)
  (if (null? calls)
      '()
      (let* ([i (index-of-lowest-conf confDel)]
             [c (list-ref confDel i)]
             [l (car c)]
             [fv (getFirstAvailableFvar fs)])
        (cons `(,l ,fv) (assign-call-undead (remove l calls)
                                            conf
                                            (cons fv fs)
                                            (remove-conf l confDel))))))
;
;(assign-info i)->info?
;i: info?
(define (assign-info i)
  (let ([calls  (getInfo i getCallUndead)]
        [confs  (getInfo i getConflicts)]
        [ass    (getInfo i getAssignment)])
    (let* ([nonLocConf (filter (lambda (c) (member (car c) calls)) confs)]
           [newAss (assign-call-undead calls confs (filter (lambda (a) (fvar? (second a))) ass) nonLocConf)])
      (addInfo i (setAssignment (append ass newAss))))))

;
;(assign-funcs f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define (assign-funcs f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(assign-info i) ,t)]
    [_ #f]))



;
;(assign-call-undead-variables p)->Asm-pred-lang-V6-pre-framed
;p: Asm-pred-lang-V6-conflicts
(define (assign-call-undead-variables p)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(assign-info i) ,@(map assign-funcs f) ,t)]
    [_ "assign call undead variables failed"]))


(module+ test
  ;#|                
  ;|#
  )