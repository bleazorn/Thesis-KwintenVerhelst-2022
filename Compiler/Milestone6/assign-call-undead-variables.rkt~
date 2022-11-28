#lang racket

(require "common/info.rkt"
         "common/fvar.rkt")
(provide assign-call-undead-variables)

(module+ test
  (require rackunit))

;
;(getConfFrames x conf)->'(fvar ...)
;x: aloc?
;conf: conflicts? '((aloc (aloc ...)) ...)
(define (getConfFrames x conf)
  (let ([xConf (assoc x conf)])
    (filter fvar? (second xConf))))

;
;(getFvar x fvars)->fvar?
;i: integer?
;fvars: '(fvar? ...)
(define (getFvar i fvars)
  (if (member (newFvar i) fvars)
      (getFvar (add1 i) fvars)
      (newFvar i)))

;
;(assign-fvar x conf)->'(aloc fvar)
;x: aloc?
;conf: conflicts? '((aloc (aloc ...)) ...)
(define (assign-fvar x conf assFvar)
  (let ([xFvars (append (getConfFrames x conf) assFvar)])
    `(,x ,(getFvar 0 xFvars))))
  

;
;(assign-tail t)->assignments? '((aloc fvar) ...)
;calls: call-undead? '(loc ...)
;conf: conflicts? '((aloc (aloc ...)) ...)
(define (assign-call-undead calls confs assFvar)
  (if (null? calls)
      '()
      (let ([x (car calls)])
        (let ([assX (assign-fvar x confs assFvar)])
          (cons assX (assign-call-undead (cdr calls) (remove-conf x confs) (cons (second assX) assFvar)))))))
;
;(assign-info i)->info?
;i: info?
(define (assign-info i)
  (addInfo (removeInfo i getUndead-out) (setAssignment (assign-call-undead (getInfo i getCallUndead) (getInfo i getConflicts) '()))))

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
;assign-call-undead
  ;succes
  (check-equal? (assign-call-undead '() '((tmp-ra.10 (fv0 fv1 rbp))
                                          (rbp (r15 fv0 fv1 tmp-ra.10))
                                          (fv1 (r15 fv0 rbp tmp-ra.10))
                                          (fv0 (r15 rbp fv1 tmp-ra.10))
                                          (r15 (rbp fv0 fv1)))
                                    '())
                '()
                "assign-call-undead: succes-01: empty call-undead")
  (check-equal? (assign-call-undead '(tmp-ra.7) '((y.2 (rbp tmp-ra.7 x.1 nfv.9))
                                                  (x.1 (y.2 rbp tmp-ra.7 fv1))
                                                  (tmp-ra.7 (y.2 x.1 rbp fv1 fv0 rax z.3))
                                                  (z.3 (rbp tmp-ra.7))
                                                  (nfv.9 (r15 nfv.8 rbp y.2))
                                                  (nfv.8 (r15 rbp nfv.9))
                                                  (rbp (y.2 x.1 tmp-ra.7 rax z.3 r15 nfv.8 nfv.9))
                                                  (r15 (rbp nfv.8 nfv.9))
                                                  (rax (rbp tmp-ra.7))
                                                  (fv0 (tmp-ra.7))
                                                  (fv1 (x.1 tmp-ra.7)))
                                    '())
                '((tmp-ra.7 fv2))
                "assign-call-undead: succes-02: non empty call-undead")
  ;|#
  )