#lang racket

(require "common/info.rkt"
         "common/fvar.rkt"
         "common/register.rkt")
(provide allocate-frames-full)

(module+ test
  (require rackunit))

(define (maxFrame n)
  (* n framesize))


;
;(allocate-return l t frames ass)->effect? assignment?
;l: label?
;t: tail?
;frames: new-frames?
;ass: assignment?
(define (allocate-return l t frames ass)
  (define (sortFrame a b)
    (let ([as (string->number (second (string-split (symbol->string a) ".")))]
          [bs (string->number (second (string-split (symbol->string b) ".")))])
      (< as bs)))
  ;get frame and gets the biggest fvar from the already assigned
  (let ([frame (cond [(null? frames) '()]
                     [else (second (assoc l frames))])]  
        [maxFvar (foldl (lambda (c m) (let ([cM (getFvarNumber c)])
                                        (cond [(> cM m) cM]
                                              [else m])))
                        0
                        (filter fvar? (map second ass)))])
    ;goes through the tail
      (let-values ([(newT assT) (allocate-tail t frames ass)])
        ;gets the fvars for this non-tail call and determines the framesize
        (setfvar maxFvar)
        (let ([newAss (map (lambda (f) `(,f ,(freshfvar))) (sort frame sortFrame))]
              [fbp (current-frame-base-pointer-register)]
              [frameSize (maxFrame (add1 maxFvar))]) 
        (values `(return-point ,l ,newT)
                (append newAss assT))))))
    

;
;(allocate-let effs frames ass)->'(effect? ...) assignments? '((aloc fvar) ...)
;effs: list? '(effect? ...)
;frames: ((aloc? ...) ...)
;ass:  assignments? '((aloc fvar) ...)
(define (allocate-let effs frames ass)
  (for/fold ([newEff '()]
             [assEff '()])
            ([e effs])
    (let-values ([(newE assE) (allocate-effect e frames ass)])
      (values (append newEff `(,newE)) (append assEff assE)))))

;
;(allocate-pred p frames ass)->pred? assignments? '((aloc fvar) ...)
;p: pred?
;frames: ((aloc? ...) ...)
;ass:  assignments? '((aloc fvar) ...)
(define (allocate-pred p frames ass)
  (match p
    ['(true)         (values '(true) '())]
    ['(false)        (values '(false) '())]
    [`(not ,p) (let-values ([(newP assP) (allocate-pred p frames ass)])
                 (values `(not ,newP) assP))]
    [`(begin ,e ... ,pred) (let-values ([(newE assE) (allocate-let e frames ass)]
                                        [(newP assP) (allocate-pred pred frames ass)])
                             (values `(begin ,@newE ,newP) (append assE assP)))]
    [`(if ,p1 ,p2 ,p3) (let-values ([(newP1 assP1) (allocate-pred p1 frames ass)]
                                    [(newP2 assP2) (allocate-pred p2 frames ass)]
                                    [(newP3 assP3) (allocate-pred p3 frames ass)])
                        (values `(if ,newP1 ,newP2 ,newP3) (append assP1 assP2 assP3)))]
    [`(,relop ,a ,b) (values `(,relop ,a ,b) '())]
    [_ #f]))

;
;(allocate-effect e frames ass)->effect? assignments? '((aloc fvar) ...)
;e: effect?
;frames: ((aloc? ...) ...)
;ass:  assignments? '((aloc fvar) ...)
(define (allocate-effect e frames ass)
  (match e
    [`(set! ,a ,b) (values `(set! ,a ,b) '())]
    [`(setLinear! ,a ,b) (values `(setLinear! ,a ,b) '())]
    [`(begin ,e ...) (let-values ([(newE assE) (allocate-let e frames ass)])
                             (values `(begin ,@newE) assE))]
    [`(if ,p ,e1 ,e2) (let-values ([(newP assP) (allocate-pred p frames ass)]
                                   [(newE1 assE1) (allocate-effect e1 frames ass)]
                                   [(newE2 assE2) (allocate-effect e2 frames ass)])
                        (values `(if ,newP ,newE1 ,newE2) (append assP assE1 assE2)))]
    [`(return-point ,l ,t) (allocate-return l t frames ass)]
    ['(split) (values '(split) '())]
    [_ #f]))

;
;(allocate-tail t frames ass)->tail? assignments? '((aloc fvar) ...)
;t: tail?
;frames: ((aloc? ...) ...)
;ass:  assignments? '((aloc fvar) ...)
(define (allocate-tail t frames ass)
  (match t
    [`(jump ,trg ,l ...) (values `(jump ,trg ,@l) '())]
    [`(begin ,e ... ,tail) (let-values ([(newE assE) (allocate-let e frames ass)]
                                        [(newT assT) (allocate-tail tail frames ass)])
                             (values `(begin ,@newE ,newT) (append assE assT)))]
    [`(if ,p ,t1 ,t2) (let-values ([(newP assP) (allocate-pred p frames ass)]
                                   [(newT1 assT1) (allocate-tail t1 frames ass)]
                                   [(newT2 assT2) (allocate-tail t2 frames ass)])
                        (values `(if ,newP ,newT1 ,newT2) (append assP assT1 assT2)))]
    [`(invoke ,a ,b) (values `(invoke ,a ,b) '())]
    [_ #f]))

;
;(allocate-info i t)->tail? info?
;i: info?
;t: tail?
(define (allocate-info i t)
  (let ([frames (getInfo i getNewFrames)]
        [ass    (getInfo i getAssignment)])
    (let-values ([(newT assT) (allocate-tail t frames ass)])
      (let ([sAss (map car (append ass assT))])
        (values newT (addInfo i (setAssignment (append ass assT))))))))
  
;
;(allocate-func f)->'(define label? info? tail?)
;f: '(define label? info? tail?)
(define (allocate-func f)
  (match f
    [`(define ,l ,i ,t) (let-values ([(newT infoT) (allocate-info i t)])
                          `(define ,l ,infoT ,newT))]
    [_ #t]))

;
;(allocate-frames p)->Asm-pred-lang-V6-framed
;p: Asm-pred-lang-V6-pre-framed
(define (allocate-frames-full p)
  (match p
    [`(module ,i ,f ... ,t) (let-values ([(newT infoT) (allocate-info i t)])
                              `(module ,infoT ,@(map allocate-func f) ,newT))]
    [_ "allocate frames failed"]))

(module+ test
  )
