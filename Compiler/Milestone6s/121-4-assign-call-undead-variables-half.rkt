#lang racket

(require "common/info.rkt"
         "common/fvar.rkt")
(provide assign-call-undead-variables-half)

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
;conf: conflicts? '((aloc (triv? ...)) ...)
;assFvar: '(fvar? ...)
(define (assign-fvar x conf assFvar)
  (let ([xFvars (append (getConfFrames x conf) assFvar)])
    `(,x ,(getFvar 0 xFvars))))
  

;
;(assign-tail t)->assignments? '((aloc fvar) ...)
;calls: call-undead? '(loc ...)
;conf: conflicts? '((aloc (triv? ...)) ...)
(define (assign-call-undead calls confs)
  (if (null? calls)
      '()
      (let ([x (car calls)])
        (let ([assRec (assign-call-undead (cdr calls) (remove-conf x confs))])
          (let ([assX (assign-fvar x confs (map second assRec))])
            (cons assX assRec))))))
;
;(assign-info i)->info?
;i: info?
(define (assign-info i)
  (addInfo i (setAssignment (assign-call-undead (getInfo i getCallUndead) (getInfo i getConflicts)))))

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
(define (assign-call-undead-variables-half p)
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
                                          (r15 (rbp fv0 fv1))))
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
                                                  (fv1 (x.1 tmp-ra.7))))
                '((tmp-ra.7 fv2))
                "assign-call-undead: succes-02: non empty call-undead")

  (check-equal? (assign-call-undead-variables-half '(module ((conflicts
                                                         ((a0 (ra tmp-ra.9 cfp))
                                                          (cfp (ra tmp-ra.9 a0))
                                                          (ra (cfp a0))
                                                          (tmp-ra.9 (cfp a0))))
                                                        (undead-out
                                                         ((tmp-ra.9 cfp) ((tmp-ra.9 cfp a0) (cfp ra a0) (cfp ra a0))))
                                                        (call-undead ())
                                                        (locals (tmp-ra.9))
                                                        (new-frames ()))
                                                 (define L.odd?.1
                                                   ((conflicts
                                                     ((cfp (ra tmp-ra.7 a0 y.4 x.3 tmp.10))
                                                      (a0 (ra tmp-ra.7 cfp))
                                                      (ra (cfp a0))
                                                      (y.4 (tmp-ra.7 cfp))
                                                      (tmp.10 (tmp-ra.7 cfp x.3))
                                                      (x.3 (tmp-ra.7 cfp tmp.10))
                                                      (tmp-ra.7 (cfp a0 y.4 x.3 tmp.10))))
                                                    (undead-out
                                                     ((a0 tmp-ra.7 cfp)
                                                      (tmp-ra.7 cfp x.3)
                                                      (((tmp.10 tmp-ra.7 cfp x.3) (tmp-ra.7 cfp x.3))
                                                       ((tmp-ra.7 cfp a0) (cfp a0))
                                                       ((y.4 tmp-ra.7 cfp) ((tmp-ra.7 cfp a0) (cfp ra a0) (cfp ra a0))))))
                                                    (call-undead (tmp-ra.7))
                                                    (locals (tmp-ra.7 x.3 tmp.10 y.4))
                                                    (new-frames ()))
                                                   (begin
                                                     (set! tmp-ra.7 ra)
                                                     (set! x.3 a0)
                                                     (if (begin (set! tmp.10 0) (= x.3 tmp.10))
                                                         (begin (set! a0 0) (jump tmp-ra.7 cfp a0))
                                                         (begin
                                                           (set! y.4 (+ x.3 -1))
                                                           (begin
                                                             (set! a0 y.4)
                                                             (set! ra tmp-ra.7)
                                                             (jump L.even?.2 cfp ra a0))))))
                                                 (define L.even?.2
                                                   ((conflicts
                                                     ((cfp (ra tmp-ra.8 a0 y.6 x.5 tmp.11))
                                                      (a0 (ra tmp-ra.8 cfp))
                                                      (ra (cfp a0))
                                                      (y.6 (tmp-ra.8 cfp))
                                                      (tmp.11 (tmp-ra.8 cfp x.5))
                                                      (x.5 (tmp-ra.8 cfp tmp.11))
                                                      (tmp-ra.8 (cfp a0 y.6 x.5 tmp.11))))
                                                    (undead-out
                                                     ((a0 tmp-ra.8 cfp)
                                                      (tmp-ra.8 cfp x.5)
                                                      (((tmp.11 tmp-ra.8 cfp x.5) (tmp-ra.8 cfp x.5))
                                                       ((tmp-ra.8 cfp a0) (cfp a0))
                                                       ((y.6 tmp-ra.8 cfp) ((tmp-ra.8 cfp a0) (cfp ra a0) (cfp ra a0))))))
                                                    (call-undead (tmp-ra.8))
                                                    (locals (tmp-ra.8 x.5 tmp.11 y.6))
                                                    (new-frames ()))
                                                   (begin
                                                     (set! tmp-ra.8 ra)
                                                     (set! x.5 a0)
                                                     (if (begin (set! tmp.11 0) (= x.5 tmp.11))
                                                         (begin (set! a0 1) (jump tmp-ra.8 cfp a0))
                                                         (begin
                                                           (set! y.6 (+ x.5 -1))
                                                           (begin
                                                             (set! a0 y.6)
                                                             (set! ra tmp-ra.8)
                                                             (jump L.odd?.1 cfp ra a0))))))
                                                 (begin
                                                   (set! tmp-ra.9 ra)
                                                   (begin (set! a0 5) (set! ra tmp-ra.9) (jump L.even?.2 cfp ra a0)))))
                '(module ((assignment ())
                          (conflicts
                           ((a0 (ra tmp-ra.9 cfp))
                            (cfp (ra tmp-ra.9 a0))
                            (ra (cfp a0))
                            (tmp-ra.9 (cfp a0))))
                          (call-undead ())
                          (locals (tmp-ra.9))
                          (new-frames ()))
                   (define L.odd?.1
                     ((assignment ((tmp-ra.7 fv0)))
                      (conflicts
                       ((cfp (ra tmp-ra.7 a0 y.4 x.3 tmp.10))
                        (a0 (ra tmp-ra.7 cfp))
                        (ra (cfp a0))
                        (y.4 (tmp-ra.7 cfp))
                        (tmp.10 (tmp-ra.7 cfp x.3))
                        (x.3 (tmp-ra.7 cfp tmp.10))
                        (tmp-ra.7 (cfp a0 y.4 x.3 tmp.10))))
                      (call-undead (tmp-ra.7))
                      (locals (tmp-ra.7 x.3 tmp.10 y.4))
                      (new-frames ()))
                     (begin
                       (set! tmp-ra.7 ra)
                       (set! x.3 a0)
                       (if (begin (set! tmp.10 0) (= x.3 tmp.10))
                           (begin (set! a0 0) (jump tmp-ra.7 cfp a0))
                           (begin
                             (set! y.4 (+ x.3 -1))
                             (begin
                               (set! a0 y.4)
                               (set! ra tmp-ra.7)
                               (jump L.even?.2 cfp ra a0))))))
                   (define L.even?.2
                     ((assignment ((tmp-ra.8 fv0)))
                      (conflicts
                       ((cfp (ra tmp-ra.8 a0 y.6 x.5 tmp.11))
                        (a0 (ra tmp-ra.8 cfp))
                        (ra (cfp a0))
                        (y.6 (tmp-ra.8 cfp))
                        (tmp.11 (tmp-ra.8 cfp x.5))
                        (x.5 (tmp-ra.8 cfp tmp.11))
                        (tmp-ra.8 (cfp a0 y.6 x.5 tmp.11))))
                      (call-undead (tmp-ra.8))
                      (locals (tmp-ra.8 x.5 tmp.11 y.6))
                      (new-frames ()))
                     (begin
                       (set! tmp-ra.8 ra)
                       (set! x.5 a0)
                       (if (begin (set! tmp.11 0) (= x.5 tmp.11))
                           (begin (set! a0 1) (jump tmp-ra.8 cfp a0))
                           (begin
                             (set! y.6 (+ x.5 -1))
                             (begin
                               (set! a0 y.6)
                               (set! ra tmp-ra.8)
                               (jump L.odd?.1 cfp ra a0))))))
                   (begin
                     (set! tmp-ra.9 ra)
                     (begin (set! a0 5) (set! ra tmp-ra.9) (jump L.even?.2 cfp ra a0))))
                "assign-call-undead-variables: succes-01: value call")
                
  ;|#
  )