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
(define (assign-funcs f) 
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(assign-info i) ,t)]
    [_ #f]))



;
;(assign-call-undead-variables p)->Asm-pred-lang-V6-pre-framed
;p: Asm-pred-lang-V6-conflicts
(define/contract (assign-call-undead-variables-full p) (-> asm-pred-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(assign-info i) ,@(map assign-funcs f) ,t)]
    [_ "assign call undead variables failed"]))


#;(assign-call-undead-variables-full '(module ((assignment ())
             (conflicts ((a0 (tmp-ra.13)) (cfp (tmp-ra.13 cra nfv.15)) (cra (cfp nfv.15)) (nfv.15 (cra cfp)) (tmp-ra.13 (cfp a0))))
             (undead-out ((tmp-ra.13 cfp) (((tmp-ra.13 cfp a0) ((cfp nfv.15) (cfp cra nfv.15) (cfp cra nfv.15))) (cfp a0))))
             (call-undead (tmp-ra.13))
             (locals (tmp-ra.13 nfv.15))
             (allocatedFvars (fv0 fv1))
             (new-frames ((L.rpLabel.14 (nfv.15))))
             (paramSize 0))
   (define L.odd?.1
     ((assignment ((y.4 fv0) (tmp.16 fv0) (x.3 fv0)))
      (conflicts
       ((a0 (tmp-ra.7 cfp))
        (cfp (tmp-ra.7 cra nfv.9 y.4 x.3 a0 tmp.16 fv0))
        (fv0 (tmp-ra.7 cfp cra))
        (cra (cfp nfv.9 fv0))
        (nfv.9 (cra cfp))
        (y.4 (cfp tmp-ra.7))
        (tmp.16 (tmp-ra.7 cfp x.3))
        (x.3 (tmp-ra.7 cfp tmp.16))
        (tmp-ra.7 (cfp a0 y.4 x.3 tmp.16 fv0))))
      (undead-out
       ((fv0 tmp-ra.7 cfp)
        (tmp-ra.7 cfp x.3)
        (((tmp.16 tmp-ra.7 cfp x.3) (tmp-ra.7 cfp x.3))
         ((tmp-ra.7 cfp a0) (cfp a0))
         ((tmp-ra.7 cfp y.4) (((tmp-ra.7 cfp a0) ((cfp nfv.9) (cfp cra nfv.9) (cfp cra nfv.9))) (cfp a0))))))
      (call-undead (tmp-ra.7))
      (locals (tmp-ra.7 x.3 tmp.16 y.4 nfv.9))
      (allocatedFvars (fv1 fv2))
      (new-frames ((L.rpLabel.8 (nfv.9))))
      (paramSize 1))
     (begin
       (set! tmp-ra.7 cra)
       (set! x.3 fv0)
       (if (begin (set! tmp.16 0) (= x.3 tmp.16))
         (begin (set! a0 150) (jump-return tmp-ra.7 cfp a0))
         (begin
           (set! y.4 (+ x.3 -1))
           (begin (return-point L.rpLabel.8 (begin (set! nfv.9 y.4) (set! cra L.rpLabel.8) (jump-call L.even?.2 cfp cra nfv.9))) (jump-return tmp-ra.7 cfp a0))))))
   (define L.even?.2
     ((assignment ((y.6 fv0) (tmp.17 fv0) (x.5 fv0)))
      (conflicts
       ((a0 (tmp-ra.10 cfp))
        (cfp (tmp-ra.10 cra nfv.12 y.6 x.5 a0 tmp.17 fv0))
        (fv0 (tmp-ra.10 cfp cra))
        (cra (cfp nfv.12 fv0))
        (nfv.12 (cra cfp))
        (y.6 (cfp tmp-ra.10))
        (tmp.17 (tmp-ra.10 cfp x.5))
        (x.5 (tmp-ra.10 cfp tmp.17))
        (tmp-ra.10 (cfp a0 y.6 x.5 tmp.17 fv0))))
      (undead-out
       ((fv0 tmp-ra.10 cfp)
        (tmp-ra.10 cfp x.5)
        (((tmp.17 tmp-ra.10 cfp x.5) (tmp-ra.10 cfp x.5))
         ((tmp-ra.10 cfp a0) (cfp a0))
         ((tmp-ra.10 cfp y.6) (((tmp-ra.10 cfp a0) ((cfp nfv.12) (cfp cra nfv.12) (cfp cra nfv.12))) (cfp a0))))))
      (call-undead (tmp-ra.10))
      (locals (tmp-ra.10 x.5 tmp.17 y.6 nfv.12))
      (allocatedFvars (fv1 fv2))
      (new-frames ((L.rpLabel.11 (nfv.12))))
      (paramSize 1))
     (begin
       (set! tmp-ra.10 cra)
       (set! x.5 fv0)
       (if (begin (set! tmp.17 0) (= x.5 tmp.17))
         (begin (set! a0 200) (jump-return tmp-ra.10 cfp a0))
         (begin
           (set! y.6 (+ x.5 -1))
           (begin (return-point L.rpLabel.11 (begin (set! nfv.12 y.6) (set! cra L.rpLabel.11) (jump-call L.odd?.1 cfp cra nfv.12))) (jump-return tmp-ra.10 cfp a0))))))
   (begin
     (set! tmp-ra.13 cra)
     (begin (return-point L.rpLabel.14 (begin (set! nfv.15 5) (set! cra L.rpLabel.14) (jump-call L.even?.2 cfp cra nfv.15))) (jump-return tmp-ra.13 cfp a0)))))


(module+ test
  ;#|                
  ;|#
  )
