#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "langs/asm-pred-lang.rkt")
(provide allocate-frames-half)

(module+ test
  (require rackunit))

(define (maxFrame n)
  (* n (framesize)))

;
;(sortFrame a b)->boolean?
;a, b: aloc?
(define (sortFrame a b)
  (let ([as (string->number (second (string-split (symbol->string a) ".")))]
        [bs (string->number (second (string-split (symbol->string b) ".")))])
    (< as bs)))

;
;(allocate-frame startFva frame)->'((loc? fvar?) ...)
;startFva: integer?
;frames: frame?
(define (allocate-frame startFva frame)
  (setfvar startFva)
  (map (lambda (f) `(,f ,(freshfvar))) (sort frame sortFrame)))

;
;(allocate-frames startFva frames)->integer? assignment? 
;startFva: integer?
;frames: '(frame? ...)
(define (allocate-frames startFva frames)
  (for/fold ([maxFrame 0]
             [assigned '()])
            ([frame frames])
    (let ([frame-vars (second frame)])
      (values (cond [(< maxFrame (length frame-vars)) (length frame-vars)]
                    [else maxFrame])
              (append assigned (allocate-frame startFva frame-vars))))))

;
;(allocate-info i t)->tail? info?
;i: info?
;t: tail?
(define (allocate-info i)
  (let ([frames (getInfo i getNewFrames)]
        [confs  (getInfo i getConflicts)]
        [ass    (getInfo i getAssignment)]
        [allFva (getInfo i getAllocatedFvars)])
    (let* ([all-assigned-fvars (append (filter fvar? (map second ass)) (filter fvar? (map car confs)) allFva)]  ;ass: not in temp regs cons: paras start method allFva: other ways
           [startFva (maxFvarNumber all-assigned-fvars)])
      (let-values ([(maxFrame newAss) (allocate-frames startFva frames)])
        (addInfo
         (addInfo i (setFrameSize (+ (add1 startFva) maxFrame)))
         (setAssignment (append ass newAss)))))))
  
;
;(allocate-func f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define (allocate-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(allocate-info i) ,t)]
    [_ #t]))

;
;(allocate-frames p)->Asm-pred-lang-V6-framed
;p: Asm-pred-lang-V6-pre-framed
(define/contract (allocate-frames-half p) (-> asm-pred-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(allocate-info i) ,@(map allocate-func f) ,t)]
    [_ "allocate frames failed"]))

#;(allocate-frames-half '(module ((assignment ())
                                  (conflicts
                                   ((fv0 (cra tmp-ra.9 cfp))
                                    (cfp (cra tmp-ra.9 fv0))
                                    (cra (cfp fv0))
                                    (tmp-ra.9 (cfp fv0))))
                                  (undead-out
                                   ((tmp-ra.9 cfp) ((tmp-ra.9 cfp fv0) (cfp cra fv0) (cfp cra fv0))))
                                  (call-undead ())
                                  (locals (tmp-ra.9))
                                  (new-frames ()))
                        (define L.odd?.1
                          ((assignment ())
                           (conflicts
                            ((a0 (tmp-ra.7 cfp))
                             (cfp (cra tmp-ra.7 fv0 y.4 x.3 a0 tmp.10))
                             (fv0 (cra tmp-ra.7 cfp))
                             (cra (cfp fv0))
                             (y.4 (tmp-ra.7 cfp))
                             (tmp.10 (tmp-ra.7 cfp x.3))
                             (x.3 (tmp-ra.7 cfp tmp.10))
                             (tmp-ra.7 (cfp fv0 y.4 x.3 a0 tmp.10))))
                           (undead-out
                            ((fv0 tmp-ra.7 cfp)
                             (tmp-ra.7 cfp x.3)
                             (((tmp.10 tmp-ra.7 cfp x.3) (tmp-ra.7 cfp x.3))
                              ((tmp-ra.7 cfp a0) (cfp a0))
                              ((y.4 tmp-ra.7 cfp)
                               ((tmp-ra.7 cfp fv0) (cfp cra fv0) (cfp cra fv0))))))
                           (call-undead ())
                           (locals (tmp-ra.7 x.3 tmp.10 y.4))
                           (new-frames ()))
                          (begin
                            (set! tmp-ra.7 cra)
                            (set! x.3 fv0)
                            (if (begin (set! tmp.10 0) (= x.3 tmp.10))
                                (begin (set! a0 150) (jump-return tmp-ra.7 cfp a0))
                                (begin
                                  (set! y.4 (+ x.3 -1))
                                  (begin
                                    (set! fv0 y.4)
                                    (set! cra tmp-ra.7)
                                    (jump-call L.even?.2 cfp cra fv0))))))
                        (define L.even?.2
                          ((assignment ())
                           (conflicts
                            ((a0 (tmp-ra.8 cfp))
                             (cfp (cra tmp-ra.8 fv0 y.6 x.5 a0 tmp.11))
                             (fv0 (cra tmp-ra.8 cfp))
                             (cra (cfp fv0))
                             (y.6 (tmp-ra.8 cfp))
                             (tmp.11 (tmp-ra.8 cfp x.5))
                             (x.5 (tmp-ra.8 cfp tmp.11))
                             (tmp-ra.8 (cfp fv0 y.6 x.5 a0 tmp.11))))
                           (undead-out
                            ((fv0 tmp-ra.8 cfp)
                             (tmp-ra.8 cfp x.5)
                             (((tmp.11 tmp-ra.8 cfp x.5) (tmp-ra.8 cfp x.5))
                              ((tmp-ra.8 cfp a0) (cfp a0))
                              ((y.6 tmp-ra.8 cfp)
                               ((tmp-ra.8 cfp fv0) (cfp cra fv0) (cfp cra fv0))))))
                           (call-undead ())
                           (locals (tmp-ra.8 x.5 tmp.11 y.6))
                           (new-frames ()))
                          (begin
                            (set! tmp-ra.8 cra)
                            (set! x.5 fv0)
                            (if (begin (set! tmp.11 0) (= x.5 tmp.11))
                                (begin (set! a0 200) (jump-return tmp-ra.8 cfp a0))
                                (begin
                                  (set! y.6 (+ x.5 -1))
                                  (begin
                                    (set! fv0 y.6)
                                    (set! cra tmp-ra.8)
                                    (jump-call L.odd?.1 cfp cra fv0))))))
                        (begin
                          (set! tmp-ra.9 cra)
                          (begin
                            (set! fv0 5)
                            (set! cra tmp-ra.9)
                            (jump-call L.even?.2 cfp cra fv0)))))


(module+ test
  )
