#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "langs/asm-pred-lang.rkt"
         "log.rkt")
(provide assign-call-undead-variables-full)

(module+ test
  (require rackunit))

;
;(assign-recur loc conf assign)->list? '((aloc loc) ...)
;loc:list? '(aloc ...)
;conf:list? '((aloc? (...)) ...)
;assign: list? '((aloc loc) ...)
(define (assign-call-undead calls conf fs confDel)
  ;(logln fs)
  ;(logln confDel)
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
        [ass    (getInfo i getAssignment)]
        [allFva (getInfo i getAllocatedFvars)])
    (let* ([nonLocConf (filter (lambda (c) (member (car c) calls)) confs)]
           [all-assigned-fvars (remove-duplicates (append (filter fvar? (map second ass)) (filter fvar? (map car confs)) allFva))]  ;ass: not in temp regs cons: paras start method allFva: other ways
           [newAss (assign-call-undead calls confs all-assigned-fvars nonLocConf)])
      (addInfo i (setAssignment (append ass newAss))))))

;
;(assign-funcs f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define/contract (assign-funcs f) (-> asm-pred-lang? asm-pred-lang?)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(assign-info i) ,t)]
    [_ #f]))



;
;(assign-call-undead-variables p)->Asm-pred-lang-V6-pre-framed
;p: Asm-pred-lang-V6-conflicts
(define (assign-call-undead-variables-full p)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(assign-info i) ,@(map assign-funcs f) ,t)]
    [_ "assign call undead variables failed"]))


#;(assign-funcs '(define L.fun2.2
                 ((assignment
                   ((x7.14 t0)
                    (x6.13 t0)
                    (x5.12 t0)
                    (tmp.87 t0)
                    (tmp.86 t1)
                    (x4.11 t0)
                    (x2.9 t0)
                    (x1.8 t1)
                    (tmp-ra.68 t0)
                    (x3.10 fv0)))
                  (conflicts
                   ((fv1 (a0 a1 cra))
                    (fv0 (a0 a1 cfp))
                    (cfp
                     (cra
                      a0
                      x3.10
                      tmp.86
                      tmp.87
                      x7.14
                      x6.13
                      x5.12
                      x4.11
                      a1
                      x1.8
                      x2.9
                      tmp-ra.68
                      fv0))
                    (a1 (cra cfp a0 x1.8 tmp-ra.68 fv1 fv0))
                    (a0 (cra cfp x3.10 a1 tmp-ra.68 fv1 fv0))
                    (cra
                     (cfp a0 x3.10 tmp.86 tmp.87 x7.14 x6.13 x5.12 x4.11 a1 x1.8 x2.9 fv1))
                    (tmp.87 (cra cfp tmp.86))
                    (tmp.86 (cra cfp tmp.87))
                    (x7.14 (cra cfp))
                    (x6.13 (cra cfp))
                    (x5.12 (cra cfp))
                    (x4.11 (x3.10 cra cfp))
                    (x3.10 (cra cfp x4.11 a0))
                    (x2.9 (x1.8 cra cfp))
                    (x1.8 (cra cfp x2.9 a1))
                    (tmp-ra.68 (a0 a1 cfp))))
                  (undead-out
                   ((a0 a1 cra cfp)
                    (a0 a1 cra cfp)
                    ((a0 a1 cra cfp)
                     (a1 x1.8 cra cfp)
                     (x1.8 cra cfp)
                     ((x3.10 cra cfp)
                      ((x3.10 cra cfp)
                       ((((a0 x3.10 cra cfp)
                          ((x3.10 cfp a0) (cfp a0 a1) (cfp cra a0 a1) (cfp cra a0 a1)))
                         (x3.10 cra cfp))
                        (x3.10 cra cfp)
                        (cra cfp)
                        (cra cfp)
                        (((tmp.86 cra cfp) (tmp.86 tmp.87 cra cfp) (cra cfp a0)) (cfp a0)))
                       ((cra cfp a0) (cfp a0)))))))
                  (call-undead (x3.10))
                  (locals (tmp-ra.68 x1.8 x2.9 x3.10 x4.11 x5.12 x6.13 x7.14 tmp.86 tmp.87))
                  (new-frames ())
                  (paramSize 0))
                 (begin
                   (set! fv0 cra)
                   (setLinear! fv1 cfp)
                   (begin
                     (set! tmp-ra.68 cra)
                     (set! x1.8 a0)
                     (set! x2.9 a1)
                     (begin
                       (set! x3.10 (- x1.8 x1.8))
                       (if (false)
                           (begin
                             (begin
                               (return-point
                                L.rpLabel.69
                                (begin
                                  (set! a0 x3.10)
                                  (set! a1 x3.10)
                                  (set! cra L.rpLabel.69)
                                  (jump L.fun5.3 cfp cra a0 a1)))
                               (set! x4.11 a0))
                             (set! x5.12 x3.10)
                             (set! x6.13 (+ x3.10 413))
                             (set! x7.14 329)
                             (begin
                               (begin
                                 (set! tmp.86 356)
                                 (set! tmp.87 -101)
                                 (set! a0 (* tmp.86 tmp.87)))
                               (jump cra cfp a0)))
                           (begin (set! a0 (- x3.10 x3.10)) (jump cra cfp a0))))))))


(module+ test
  ;#|                
  ;|#
  )
