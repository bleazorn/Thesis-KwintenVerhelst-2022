#lang racket

(require "common/register.rkt"
         "common/fvar.rkt"
         "common/aloc.rkt"
         "common/info.rkt"
         "langs/proc-imp-cmf-lang.rkt"
         "langs/imp-cmf-lang.rkt")
(provide impose-calling-conventions-full)

(module+ test
  (require rackunit))


(define new-frame '())
(define (reset-new-frame)
  (set! new-frame '()))
(define (set-new-frame l)
  (set! new-frame l))
(define (add-new-frame l)
  (if (or (null? l) (null? (second l)))
      void
      (set-new-frame (cons l new-frame))))

(define para-size 0)
(define (reset-para-size)
  (set! para-size 0))
(define (set-para-size n)
  (set! para-size n))
(define (add-para-size n)
  (set-para-size (+ para-size n)))

(define (reset-impose)
  (reset-new-frame)
  (reset-para-size))

;
;(get-frame-var args)->list? list?
;args: list?
(define (get-frame-var args)
  (resetfvar)
  (cond [(> (length args) (length (current-parameter-registers))) (let ([frameArgSize (- (length args) (length (current-parameter-registers)))])
                                                                    (add-para-size frameArgSize)
                                                                    (values (list-tail args (length (current-parameter-registers)))
                                                                            (build-list frameArgSize (lambda (x) (freshfvar)))))]
        [else (values '() '())]))

;
;(get-new-frame-var args)->list? list?
;args: list?
(define (get-new-frame-var args)
  (cond [(> (length args) (length (current-parameter-registers))) (values (list-tail args (length (current-parameter-registers)))
                                                                          (build-list (- (length args) (length (current-parameter-registers))) (lambda (x) (freshNfv))))]
        [else (values '() '())]))

;
;(get-registers args)->list? list?
;args: list?
(define (get-registers args)
  (cond [(> (length args) (length (current-parameter-registers))) (values (take args (length (current-parameter-registers))) (current-parameter-registers))]
        [else (values args (take (current-parameter-registers) (length args)))]))


;
;(impose-pred p)->pred?
;p: pred?
(define (impose-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map impose-effect e) ,(impose-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(impose-pred p1) ,(impose-pred p2) ,(impose-pred p3))]
    [`(not ,pred) `(not ,(impose-pred pred))]
    [pred pred]))

;
;(impose-value v)->value?/effect?
;v: value?
(define (impose-value v)
  (match v
    [`(call ,n ,a ...) (cond [(or (label? n) (aloc? n)) (let ([rp-label (freshRPLabel)])
                                                          (let-values ([(regArg regPara) (get-registers a)]
                                                                       [(fraArg fraPara) (get-new-frame-var a)])
                                                            (add-new-frame `(,rp-label ,fraPara))
                                                            `(return-point ,rp-label
                                                                           (begin ,@(map (lambda (arg par) `(set! ,par ,arg)) (append (reverse fraArg) regArg) (append (reverse fraPara) regPara))
                                                                                  (set! ,(current-return-address-register) ,rp-label)
                                                                                  (jump-call ,n ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@regPara ,@fraPara)))))]
                             [(integer? n) n]
                             [else (error (format "impose-calling-conventions-full:  Failed match.\n No valid value call: ~a" v))])]
    [val val]))

;
;(impose-effect e)->list? '(effect? ...)
;e: effect?
(define (impose-effect e)
  (match e
    [`(begin ,eff ...) `(begin ,@(map impose-effect eff))]
    [`(if ,p ,e1 ,e2) `(if ,(impose-pred p) ,(impose-effect e1) ,(impose-effect e2))]
    [`(set! ,a ,v) (match v
                     [`(call ,n ,args ...) `(begin ,(impose-value v) (set! ,a ,(current-return-value-register)))]
                     [_ `(set! ,a ,(impose-value v))])]
    [_ (error (format "impose-calling-conventions-full:  Failed match.\n No valid effect: ~a" e))]))
           
;
;(impose-tail t)->tail?
;t: tail?
(define (impose-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map impose-effect e) ,(impose-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(impose-pred p) ,(impose-tail t1) ,(impose-tail t2))]
    [`(call ,n ,a ...) (cond [(or (label? n) (aloc? n)) (let ([rp-label (freshRPLabel)])
                                                          (let-values ([(regArg regPara) (get-registers a)]
                                                                       [(fraArg fraPara) (get-new-frame-var a)])
                                                            (add-new-frame `(,rp-label ,fraPara))
                                                            `(begin (return-point ,rp-label
                                                                                  (begin ,@(map (lambda (arg par) `(set! ,par ,arg)) (append (reverse fraArg) regArg) (append (reverse fraPara) regPara))
                                                                                         (set! ,(current-return-address-register) ,rp-label)
                                                                                         (jump-call ,n ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@regPara ,@fraPara)))
                                                                    (jump-return ,(current-return-address-register) ,(current-frame-base-pointer-register) ,(current-return-value-register)))))]              
                             [(integer? n) n]
                             [else (error (format "impose-calling-conventions-full:  Failed match.\n No valid tail call: ~a" t))])]
    [val `(begin (set! ,(current-return-value-register) ,val)
                 (jump-return ,(current-return-address-register) ,(current-frame-base-pointer-register) ,(current-return-value-register)))]))

;
;(impose-entry e)->tail? info?
;e: entry?
(define (impose-entry e)
  (let ([tail (impose-tail e)])
    (let ([nInfo `(,(setNewFrames new-frame) ,(setParamSize para-size))])
      (values tail
              nInfo))))

;
;(impose-func f)->'(define label? tail?)
;f: '(define label? (lambda (aloc? ...) tail?))
(define (impose-func f)
  (reset-impose)
  (match f
    [`(define ,l (lambda (,a ...) ,t)) (let-values ([(regArg regPara) (get-registers a)]
                                                    [(fraArg fraPara) (get-frame-var a)]
                                                    [(entry info) (impose-entry t)])
                                         `(define ,l ,info (begin ,@(map (lambda (arg par) `(set! ,arg ,par)) (append regArg fraArg) (append regPara fraPara))
                                                                  ,entry)))]
    [_ (error (format "impose-calling-conventions-full:  Failed match.\n No valid function: ~a" f))]))

;Compiles Imp-lang-V5-cmf-proc to  Imp-lang-V5-cmf by imposing calling conventions on all calls and procedure definitions. The parameter registers are defined by the list current-parameter-registers.
;(impose-calling-conventions p)->Imp-lang-V5-cmf?
;p : Imp-lang-V5-cmf-proc?
(define/contract (impose-calling-conventions-full p) (-> proc-imp-cmf-lang? imp-cmf-lang?)
  (match p
    [`(module ,i ,f ... ,t) (let ([funcs (map impose-func f)])
                              (reset-impose)
                              (let-values ([(entry info) (impose-entry t)])
                                `(module ,info ,@funcs ,entry)))]
    [_ "impose-calling-conventions failed"]))


(module+ test
  (define (check-impose p a b m r)
    (resetfresh)
    (resetfvar)
    (reset-impose)
    (parameterize ([current-parameter-registers r])
      (check-equal? (p a) b m)))
  (define (check-impose-values-2 p a b1 b2 m r)
    (resetfresh)
    (resetfvar)
    (reset-impose)
    (parameterize ([current-parameter-registers r])
      (let-values ([(r1 r2) (p a)])
        (check-equal? r1 b1 m)
        (check-equal? r2 b2 m))))
  ;add-new-frame
  (define (check-add-new-frame l r b m)
    (set-new-frame b) 
    (add-new-frame l)
    (check-equal? new-frame r m))
  ;succes
  (check-add-new-frame '() '() '() "add-new-frame: succes-01: empty list empty new-frame")
  (check-add-new-frame '(L.rpLabel.1 ()) '() '() "add-new-frame: succes-02: no fvar")
  (check-add-new-frame '()
                       '((L.rpLabel.1 (x.1 y.2)) (L.rpLabel.2 (x.3 z.4)))
                       '((L.rpLabel.1 (x.1 y.2)) (L.rpLabel.2 (x.3 z.4)))
                       "add-new-frame: succes-03: empty list non-empty new-frame")
  (check-add-new-frame '(L.rpLabel.1 ())
                       '((L.rpLabel.1 (x.1 y.2)) (L.rpLabel.2 (x.3 z.4)))
                       '((L.rpLabel.1 (x.1 y.2)) (L.rpLabel.2 (x.3 z.4)))
                       "add-new-frame: succes-04: no fvar non-empty new-frame")
  (check-add-new-frame '(L.rpLabel.1 (x.1 y.2))
                       '((L.rpLabel.1 (x.1 y.2)))
                       '()
                       "add-new-frame: succes-05: add to empty frame")
  (check-add-new-frame '(L.rpLabel.2 (x.3 z.4))
                       '((L.rpLabel.2 (x.3 z.4)) (L.rpLabel.1 (x.1 y.2)))
                       '((L.rpLabel.1 (x.1 y.2)))
                       "add-new-frame: succes-06: add to non-empty frame")
  ;(get-frame-var args)
  ;succes
  (check-impose-values-2 get-frame-var '() '() '() "get-frame-var: succes-01: empty list" (current-parameter-registers))
  (check-impose-values-2 get-frame-var '(x.1 y.2) '() '() "get-frame-var: succes-02: less than available param " (current-parameter-registers))
  (check-impose-values-2 get-frame-var '(x.1 y.2) '(y.2) '(fv0) "get-frame-var: succes-03: more than available param" '(a1))
  (check-impose-values-2 get-frame-var '(x.1 y.2) '(x.1 y.2) '(fv0 fv1) "get-frame-var: succes-04: no available param" '())
  ;(get-new-frame-var args)
  ;succes
  (check-impose-values-2 get-new-frame-var '() '() '() "get-new-frame-var: succes-01: empty list" (current-parameter-registers))
  (check-impose-values-2 get-new-frame-var '(x.1 y.2) '() '() "get-new-frame-var: succes-02: less than available param " (current-parameter-registers))
  (check-impose-values-2 get-new-frame-var '(x.1 y.2) '(y.2) '(nfv.1) "get-new-frame-var: succes-03: more than available param" '(a1))
  (check-impose-values-2 get-new-frame-var '(x.1 y.2) '(x.1 y.2) '(nfv.1 nfv.2) "get-new-frame-var: succes-04: no available param" '())
  ;(get-registers args)
  ;succes
  (check-impose-values-2 get-registers '() '() '() "get-registers: succes-01: empty list" (current-parameter-registers))
  (check-impose-values-2 get-registers '(x.1 y.2) '(x.1 y.2) '(a1 a2) "get-registers: succes-02: less than available param" (current-parameter-registers))
  (check-impose-values-2 get-registers '(x.1 y.2) '(x.1) '(a1) "get-registers: succes-03: more than available param" '(a1))
  (check-impose-values-2 get-registers '(x.1 y.2) '() '() "get-registers: succes-04: no available param" '())
  ;(impose-pred p)
  ;succes
  (check-impose impose-pred '(true) '(true) "impose-pred: succes-01: true" (current-parameter-registers))
  (check-impose impose-pred '(false) '(false) "impose-pred: succes-02: false" (current-parameter-registers))
  (check-impose impose-pred '(< x.1 y.2) '(< x.1 y.2) "impose-pred: succes-03: relop" (current-parameter-registers))
  (check-impose impose-pred '(not (< x.1 y.2)) '(not (< x.1 y.2)) "impose-pred: succes-04: not" (current-parameter-registers))
  (check-impose impose-pred '(begin (set! x.1 y.2) (< x.1 y.2)) '(begin (set! x.1 y.2) (< x.1 y.2)) "impose-pred: succes-05: begin" (current-parameter-registers))
  (check-impose impose-pred '(if (< x.1 y.2) (true) (false)) '(if (< x.1 y.2) (true) (false)) "impose-pred: succes-06: if" (current-parameter-registers))
  ;(impose-value v)
  ;succes
  (check-impose impose-value 'x.1 'x.1 "impose-value: succes-01: triv" (current-parameter-registers))
  (check-impose impose-value '(+ x.1 y.2) '(+ x.1 y.2) "impose-value: succes-02: binop" (current-parameter-registers))
  (check-impose impose-value
                '(call L.swap.1 1 2)
                '(return-point
                  L.rpLabel.1
                  (begin
                    (set! a1 1)
                    (set! a2 2)
                    (set! cra L.rpLabel.1)
                    (jump-call L.swap.1 cfp cra a1 a2)))
                "impose-value: succes-03: call less than available param"
                (current-parameter-registers))
  (check-impose impose-value
                '(call L.swap.1 1 2)
                '(return-point
                  L.rpLabel.1
                  (begin
                    (set! nfv.2 2)
                    (set! a1 1)
                    (set! cra L.rpLabel.1)
                    (jump-call L.swap.1 cfp cra a1 nfv.2)))
                "impose-value: succes-04: call more than available param"
                '(a1))
  (check-impose impose-value
                '(call L.swap.1 1 2)
                '(return-point
                  L.rpLabel.1
                  (begin
                    (set! nfv.3 2)
                    (set! nfv.2 1)
                    (set! cra L.rpLabel.1)
                    (jump-call L.swap.1 cfp cra nfv.2 nfv.3)))
                "impose-value: succes-05: call no available param"
                '())
  
  ;failure
  (check-impose impose-value '(call 0 1 2) 0 "impose-value: failure-01: call integer" (current-parameter-registers))
  (check-exn exn:fail? (thunk (impose-value '(call x 1 2))) "impose-value: failure-02: call error")
  ;(impose-effect e)
  ;succes
  (check-impose impose-effect '(set! x.1 y.2) '(set! x.1 y.2) "impose-effect: succes-01: set triv" (current-parameter-registers))
  (check-impose impose-effect
                '(set! x.1 (call L.swap.1 1 2))
                '(begin
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 1)
                      (set! a2 2)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1 cfp cra a1 a2)))
                   (set! x.1 a0))
                "impose-effect: succes-02: set call"
                (current-parameter-registers))

  (check-impose impose-effect '(begin (set! x.1 y.2) (set! x.1 y.3)) '(begin (set! x.1 y.2) (set! x.1 y.3)) "impose-effect: succes-03: begin" (current-parameter-registers))
  (check-impose impose-effect '(if (true) (set! x.1 y.2) (set! x.1 y.3))  '(if (true) (set! x.1 y.2) (set! x.1 y.3)) "impose-effect: succes-04: if" (current-parameter-registers))
  ;failure
  (check-exn exn:fail? (thunk (impose-effect '())) "impose-effect: failure-01: no effect")
  ;(impose-tail t)
  ;succes
  (check-impose impose-tail 'x.1 '(begin (set! a0 x.1) (jump-return cra cfp a0)) "impose-tail: succes-01: val" (current-parameter-registers))
  (check-impose impose-tail
                '(call L.swap.1 1 2)
                '(begin
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! a1 1)
                      (set! a2 2)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1 cfp cra a1 a2)))
                   (jump-return cra cfp a0))
                "impose-tail: succes-02: call less than available param" (current-parameter-registers))
  (check-impose impose-tail
                '(call L.swap.1 1 2)
                '(begin
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! nfv.2 2)
                      (set! a1 1)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1 cfp cra a1 nfv.2)))
                   (jump-return cra cfp a0))
                "impose-tail: succes-03: call more than available param" '(a1))
  (check-impose impose-tail
                '(call L.swap.1 1 2)
                '(begin
                   (return-point
                    L.rpLabel.1
                    (begin
                      (set! nfv.3 2)
                      (set! nfv.2 1)
                      (set! cra L.rpLabel.1)
                      (jump-call L.swap.1 cfp cra nfv.2 nfv.3)))
                   (jump-return cra cfp a0))
                "impose-tail: succes-04: call no available param" '())

  (check-impose impose-tail '(begin (set! x.1 y.2) x.1) '(begin (set! x.1 y.2) (begin (set! a0 x.1) (jump-return cra cfp a0))) "impose-tail: succes-05: begin" (current-parameter-registers))
  (check-impose impose-tail
                '(if (true) x.1 y.2)
                '(if (true)
                     (begin (set! a0 x.1) (jump-return cra cfp a0))
                     (begin (set! a0 y.2) (jump-return cra cfp a0)))
                "impose-tail: succes-06: if" (current-parameter-registers))
  ;failure
  (check-impose impose-tail '(call 0 1 2) 0 "impose-tail: failure-01: call integer" (current-parameter-registers))
  (check-exn exn:fail? (thunk (impose-tail '(call x 1 2))) "impose-tail: failure-02: call error")
  ;(impose-entry e)
  ;succes
  (check-impose-values-2 impose-entry 'x.1 '(begin (set! a0 x.1) (jump-return cra cfp a0)) '((new-frames ()) (paramSize 0)) "impose-entry: succes-01: val" (current-parameter-registers))
  
  (check-impose-values-2 impose-entry
                         '(call L.swap.1 1 2)
                         '(begin
                            (return-point
                             L.rpLabel.1
                             (begin
                               (set! a1 1)
                               (set! a2 2)
                               (set! cra L.rpLabel.1)
                               (jump-call L.swap.1 cfp cra a1 a2)))
                            (jump-return cra cfp a0))
                         '((new-frames ()) (paramSize 0))
                         "impose-entry: succes-02: tail call less than available param" (current-parameter-registers))
  (check-impose-values-2 impose-entry
                         '(call L.swap.1 1 2)
                         '(begin
                            (return-point
                             L.rpLabel.1
                             (begin
                               (set! nfv.2 2)
                               (set! a1 1)
                               (set! cra L.rpLabel.1)
                               (jump-call L.swap.1 cfp cra a1 nfv.2)))
                            (jump-return cra cfp a0))
                         '((new-frames ((L.rpLabel.1 (nfv.2)))) (paramSize 0))
                         "impose-entry: succes-03: tail call more than available param" '(a1))
  (check-impose-values-2 impose-entry
                         '(call L.swap.1 1 2)
                         '(begin
                            (return-point
                             L.rpLabel.1
                             (begin
                               (set! nfv.3 2)
                               (set! nfv.2 1)
                               (set! cra L.rpLabel.1)
                               (jump-call L.swap.1 cfp cra nfv.2 nfv.3)))
                            (jump-return cra cfp a0))
                         '((new-frames ((L.rpLabel.1 (nfv.2 nfv.3)))) (paramSize 0))
                         "impose-entry: succes-04: tail call no available param" '())
  (check-impose-values-2 impose-entry
                         '(begin (set! x.1 (call L.swap.1 1 2)) x.1)
                         '(begin
                            (begin
                              (return-point
                               L.rpLabel.1
                               (begin
                                 (set! a1 1)
                                 (set! a2 2)
                                 (set! cra L.rpLabel.1)
                                 (jump-call L.swap.1 cfp cra a1 a2)))
                              (set! x.1 a0))
                            (begin (set! a0 x.1) (jump-return cra cfp a0)))
                         '((new-frames ()) (paramSize 0))
                         "impose-entry: succes-05: value call less than available param" (current-parameter-registers))
  (check-impose-values-2 impose-entry
                         '(begin (set! x.1 (call L.swap.1 1 2)) x.1)
                         '(begin
                            (begin
                              (return-point
                               L.rpLabel.1
                               (begin
                                 (set! nfv.2 2)
                                 (set! a1 1)
                                 (set! cra L.rpLabel.1)
                                 (jump-call L.swap.1 cfp cra a1 nfv.2)))
                              (set! x.1 a0))
                            (begin (set! a0 x.1) (jump-return cra cfp a0)))
                         '((new-frames ((L.rpLabel.1 (nfv.2)))) (paramSize 0))
                         "impose-entry: succes-06: value call more than available param" '(a1))
  (check-impose-values-2 impose-entry
                         '(begin (set! x.1 (call L.swap.1 1 2)) x.1)
                         '(begin
                            (begin
                              (return-point
                               L.rpLabel.1
                               (begin
                                 (set! nfv.3 2)
                                 (set! nfv.2 1)
                                 (set! cra L.rpLabel.1)
                                 (jump-call L.swap.1 cfp cra nfv.2 nfv.3)))
                              (set! x.1 a0))
                            (begin (set! a0 x.1) (jump-return cra cfp a0)))
                         '((new-frames ((L.rpLabel.1 (nfv.2 nfv.3)))) (paramSize 0))
                         "impose-entry: succes-07: value call no available param" '())

  (check-impose-values-2 impose-entry '(begin (set! x.1 y.2) x.1) '(begin (set! x.1 y.2) (begin (set! a0 x.1) (jump-return cra cfp a0))) '((new-frames ()) (paramSize 0)) "impose-entry: succes-08: begin" (current-parameter-registers))
  (check-impose-values-2 impose-entry
                         '(if (true) x.1 y.2)
                         '(if (true)
                              (begin (set! a0 x.1) (jump-return cra cfp a0))
                              (begin (set! a0 y.2) (jump-return cra cfp a0)))
                         '((new-frames ()) (paramSize 0))
                         "impose-entry: succes-09: if" (current-parameter-registers))
  ;failure
  (check-impose-values-2 impose-entry '(call 0 1 2) 0 '((new-frames ()) (paramSize 0)) "impose-entry: failure-01: call integer" (current-parameter-registers))
  (check-exn exn:fail? (thunk (impose-entry '(call x 1 2))) "impose-entry: failure-02: call error")
  ;(impose-func f)
  ;succes
  (check-impose impose-func
                '(define L.odd.1 (lambda (x.1 y.2 z.3) (call L.swap.2 x.1 y.2)))
                '(define L.odd.1
                   ((new-frames ()) (paramSize 0))
                   (begin
                     (set! x.1 a1)
                     (set! y.2 a2)
                     (set! z.3 a3)
                     (begin
                       (return-point
                        L.rpLabel.1
                        (begin
                          (set! a1 x.1)
                          (set! a2 y.2)
                          (set! cra L.rpLabel.1)
                          (jump-call L.swap.2 cfp cra a1 a2)))
                       (jump-return cra cfp a0))))
                "impose-func: succes-01: tail call less than available param" (current-parameter-registers))
  (check-impose impose-func
                '(define L.odd.1 (lambda (x.1 y.2 z.3) (call L.swap.2 x.1 y.2)))
                '(define L.odd.1
                   ((new-frames ((L.rpLabel.1 (nfv.2)))) (paramSize 2))
                   (begin
                     (set! x.1 a1)
                     (set! y.2 fv0)
                     (set! z.3 fv1)
                     (begin
                       (return-point
                        L.rpLabel.1
                        (begin
                          (set! nfv.2 y.2)
                          (set! a1 x.1)
                          (set! cra L.rpLabel.1)
                          (jump-call L.swap.2 cfp cra a1 nfv.2)))
                       (jump-return cra cfp a0))))
                "impose-func: succes-02: tail call more than available param" '(a1))
  (check-impose impose-func
                '(define L.odd.1 (lambda (x.1 y.2 z.3) (call L.swap.2 x.1 y.2)))
                '(define L.odd.1
                   ((new-frames ((L.rpLabel.1 (nfv.2 nfv.3)))) (paramSize 3))
                   (begin
                     (set! x.1 fv0)
                     (set! y.2 fv1)
                     (set! z.3 fv2)
                     (begin
                       (return-point
                        L.rpLabel.1
                        (begin
                          (set! nfv.3 y.2)
                          (set! nfv.2 x.1)
                          (set! cra L.rpLabel.1)
                          (jump-call L.swap.2 cfp cra nfv.2 nfv.3)))
                       (jump-return cra cfp a0))))
                "impose-func: succes-03: tail call no available param" '())
  (check-impose impose-func
                '(define L.odd.1 (lambda (x.1 y.2 z.3) (begin (set! x.1 (call L.swap.2 x.1 y.2)) x.1)))
                '(define L.odd.1
                   ((new-frames ()) (paramSize 0))
                   (begin
                     (set! x.1 a1)
                     (set! y.2 a2)
                     (set! z.3 a3)
                     (begin
                       (begin
                         (return-point
                          L.rpLabel.1
                          (begin
                            (set! a1 x.1)
                            (set! a2 y.2)
                            (set! cra L.rpLabel.1)
                            (jump-call L.swap.2 cfp cra a1 a2)))
                         (set! x.1 a0))
                       (begin (set! a0 x.1) (jump-return cra cfp a0)))))
                "impose-func: succes-04: value call less than available param" (current-parameter-registers))
  (check-impose impose-func
                '(define L.odd.1 (lambda (x.1 y.2 z.3) (begin (set! x.1 (call L.swap.2 x.1 y.2)) x.1)))
                '(define L.odd.1
                   ((new-frames ((L.rpLabel.1 (nfv.2)))) (paramSize 2))
                   (begin
                     (set! x.1 a1)
                     (set! y.2 fv0)
                     (set! z.3 fv1)
                     (begin
                       (begin
                         (return-point
                          L.rpLabel.1
                          (begin
                            (set! nfv.2 y.2)
                            (set! a1 x.1)
                            (set! cra L.rpLabel.1)
                            (jump-call L.swap.2 cfp cra a1 nfv.2)))
                         (set! x.1 a0))
                       (begin (set! a0 x.1) (jump-return cra cfp a0)))))
                "impose-func: succes-05: valuecall more than available param" '(a1))
  (check-impose impose-func
                '(define L.odd.1 (lambda (x.1 y.2 z.3) (begin (set! x.1 (call L.swap.2 x.1 y.2)) x.1)))
                '(define L.odd.1
                   ((new-frames ((L.rpLabel.1 (nfv.2 nfv.3)))) (paramSize 3))
                   (begin
                     (set! x.1 fv0)
                     (set! y.2 fv1)
                     (set! z.3 fv2)
                     (begin
                       (begin
                         (return-point
                          L.rpLabel.1
                          (begin
                            (set! nfv.3 y.2)
                            (set! nfv.2 x.1)
                            (set! cra L.rpLabel.1)
                            (jump-call L.swap.2 cfp cra nfv.2 nfv.3)))
                         (set! x.1 a0))
                       (begin (set! a0 x.1) (jump-return cra cfp a0)))))
                "impose-func: succes-06: value call no available param" '())
  ;failure
  (check-exn exn:fail? (thunk (impose-func '(dedafine L.odd.1 (lambda (x.1 y.2) (call L.swap.2 x.1 y.2))))) "impose-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (impose-func '(define (lambda (x.1 y.2) (call L.swap.2 x.1 y.2))))) "impose-func: failure-02: no name")
  (check-exn exn:fail? (thunk (impose-func '(define L.odd.1 (lamdabda (x.1 y.2) (call L.swap.2 x.1 y.2))))) "impose-func: failure-03: wrong datum literal lambda")
  #|
;impose-calling-conventions
  ;succes
  (check-impose impose-calling-conventions-half
                '(module () (begin (set! x.1 2)
                                (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                                (+ x.1 y.2)))
                '(module ((new-frames ()))
                   (begin
                     (set! x.1 2)
                     (begin (set! y.2 3) (set! y.2 (+ y.2 2)))
                     (begin (set! a0 (+ x.1 y.2)) (jump-return cra cfp a0))))
                "impose-calling-conventions: succes-01: no tail calls")
  (check-impose impose-calling-conventions-half
                '(module ()
                     (define L.odd?.1
                       (lambda (x.3)
                         (if (= x.3 0)
                             0
                             (begin (set! y.4 (+ x.3 -1)) (call L.even?.2 y.4)))))
                   (define L.even?.2
                     (lambda (x.5)
                       (if (= x.5 0)
                           1
                           (begin (set! y.6 (+ x.5 -1)) (call L.odd?.1 y.6)))))
                   (call L.even?.2 5))
                '(module
                     ((new-frames ()))
                   (define L.odd?.1
                     ((new-frames ()))
                     (begin (set! x.3 a0)
                            (if (= x.3 0)
                                (begin (set! a0 0) (jump-return cra cfp a0))
                                (begin (set! y.4 (+ x.3 -1)) 
                                       (begin (set! a0 y.4)
                                              (jump-call L.even?.2 cfp cra a0))))))
                   (define L.even?.2
                     ((new-frames ()))
                     (begin (set! x.5 a0)
                            (if (= x.5 0)
                                (begin (set! a0 1) (jump-return cra cfp a0))
                                (begin (set! y.6 (+ x.5 -1)) 
                                       (begin (set! a0 y.6)
                                              (jump-call L.odd?.1 cfp cra a0))))))
                   (begin (set! a0 5)
                          (jump-call L.even?.2 cfp cra a0)))
                
                "sequentialize-let: succes-02: tail calls")
  (check-impose impose-calling-conventions-half
                '(module () (define L.test.1 (lambda (x.1 x.2 x.3) (begin (set! y.4 (+ x.1 x.2)) (+ x.3 y.4)))) (call L.test.1 1 2 3))
                '(module ((new-frames ()))
                   (define L.test.1
                     ((new-frames ()))
                     (begin (set! x.1 a0) (set! x.2 a1) (set! x.3 a2)
                            (begin (set! y.4 (+ x.1 x.2))
                                   (begin
                                     (set! a0 (+ x.3 y.4))
                                     (jump-return cra cfp a0)))))
                   (begin (set! a0 1) (set! a1 2) (set! a2 3)
                          (jump-call L.test.1 cfp cra a0 a1 a2)))
                "impose-calling-conventions: succes-03: tail calls with fvar args")
  (check-impose impose-calling-conventions-half
                '(module () (define L.swap.1
                           (lambda (x.1 y.2)
                             (if (< y.2 x.1)
                                 x.1
                                 (begin (set! z.3 (call L.swap.1 y.2 x.1)) z.3))))
                   (call L.swap.1 1 2))
                '(module ((new-frames ())) (define L.swap.1
                                             ((new-frames ()))
                                             (begin
                                               (set! x.1 a0)
                                               (set! y.2 a1)
                                               (if (< y.2 x.1)
                                                   (begin (set! a0 x.1) (jump-return cra cfp a0))
                                                   (begin
                                                     (begin 
                                                       (return-point L.rpLabel.1
                                                                     (begin
                                                                       (set! a0 y.2)
                                                                       (set! a1 x.1)
                                                                       (set! cra L.rpLabel.1)
                                                                       (jump-call L.swap.1 cfp cra a0 a1)))
                                                     (set! z.3 a0))
                                                     (begin 
                                                       (set! a0 z.3)
                                                       (jump-return cra cfp a0))))))
                   (begin 
                     (set! a0 1)
                     (set! a1 2)
                     (jump-call L.swap.1 cfp cra a0 a1)))
                "impose-calling-conventions: succes-04: value call")
  ;|#
  )


