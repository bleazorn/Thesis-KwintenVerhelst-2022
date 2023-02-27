#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "langs/asm-pred-lang.rkt"
         "log.rkt")
(provide assign-frame-variables-half)

(module+ test
  (require rackunit))


;
;(getMaxFvar a ass conf)->fvar?
;a: aloc?
;ass: list? '((aloc loc) ...)
;conf:list? '((aloc? (...)) ...)
(define (getMaxFvar a ass conf)
  (let ([conflictedAssFvars (filter fvar? (map second (filter (lambda (as) (member (first as) (second (assoc a conf)))) ass)))]
        [conflictedFvars (filter fvar? (second (assoc a conf)))])
    ;(logln (append conflictedAssFvars conflictedFvars))
    (getFirstAvailableFvar (append conflictedAssFvars conflictedFvars))))


;
;(assign-recur loc ass conf confDel)->list? '((aloc loc) ...)
;loc:list? '(aloc ...)
;ass: list? '((aloc loc) ...)
;conf:list? '((aloc? (...)) ...)
;confDel:list? '((aloc? (...)) ...)
(define (assign-recur loc ass conf confDel)
  (if (null? loc)
      ass
      (let* ([i (index-of-lowest-conf confDel)]
             [c (list-ref confDel i)]
             [l (car c)]
             [fvar (getMaxFvar l ass conf)])
        (assign-recur (remove l loc)
                      (cons `(,l ,fvar) ass)
                      conf
                      (remove-conf l confDel)))))

;
;(assign-info i)->info?
;i: info?
(define (assign-info i)
  (let ([loc   (getInfo i getLocals)]
        [conf  (getInfo i getConflicts)]
        [ass    (getInfo i getAssignment)])
    (addInfo i (setAssignment (assign-recur loc ass conf (filter (lambda (c) (member (car c) loc)) conf))))))
             
;
;(assign-func f)->'(define label? info? tail?)   info?: '(locals assignments)
;f: '(define label? info? tail?)
(define (assign-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(assign-info i) ,t)]
    [_ #f]))

;
;(allocate-frames p)->Asm-pred-lang-V6-framed
;p: Asm-pred-lang-V6-pre-framed
(define/contract (assign-frame-variables-half p) (-> asm-pred-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,pro) `(module ,(assign-info i) ,@(map assign-func f) ,pro)]
    [_ "assign frame variables failed"]))



(module+ test
  )
