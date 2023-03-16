#lang racket

(require "common/register.rkt"
         "common/aloc.rkt"
         "common/fvar.rkt"
         "langs/imp-cmf-lang.rkt"
         "langs/asm-pred-lang.rkt")
(provide select-instructions)

(module+ test
  (require rackunit))

(define (loc? t)
  (or (aloc? t) (fvar? t) (register? t)))

(define (triv? t)
  (or (integer? t) (loc? t) (label? t)))

;
;(select-binop binop t1 t2 tail)->list? '((set! a b) ... tail?)
;binop: symbol?
;t1: triv?
;t2: triv?
;tail:list? '(set! a)
(define (select-binop binop t1 t2 tail)
  (if (and (triv? t1) (triv? t2))
      (let*-values ([(tmp1 pre1) (cond
                                   [(integer? t1) (let ([tmp (freshtmp)])
                                                    (values tmp `((set! ,tmp ,t1))))]
                                   [else            (values t1        '())])]
                    [(tmp2 pre2) (cond
                                   [(equal? t1 t2)                              (values tmp1       '())]
                                   [(and (integer? t2) (not (equal? binop '+))) (let ([tmp (freshtmp)])
                                                                                  (values tmp `((set! ,tmp ,t2))))]
                                   [else                                        (values t2         '())])]
                    [(tmp3 pre3) (match tail
                                   [`(set! ,a) (values a '())]           
                                   [_ (error (format "select-instructions:  Failed match.\n No valid tail for binop: ~a" tail))])])
        (if (or (or (isTmp? tmp1) (isTmp? tmp2)) (not (null? pre3)))
            (append `(begin) pre1 pre2 `((set! ,tmp3 (,binop ,tmp1 ,tmp2))) pre3)
            `(set! ,tmp3 (,binop ,tmp1 ,tmp2))))
      (error (format "select-instructions:  Failed match.\n No valid trivs: ~a ~a" t1 t2))))

;
;(select-relop t1 t2 relop)->list? '(pred? ...)
;t1: triv?
;t2: triv?
;relop: relop?
(define (select-relop t1 t2 relop)
  (if (and (triv? t1) (triv? t2))
      (let*-values ([(tmp1 pre1) (cond
                                   [(integer? t1) (let ([tmp (freshtmp)])
                                                    (values tmp `((set! ,tmp ,t1))))]
                                   [else          (values t1         '())])]
                    [(tmp2 pre2) (cond
                                   [(equal? t1 t2) (values tmp1 '())]
                                   [(integer? t2)  (let ([tmp (freshtmp)])
                                                     (values tmp `((set! ,tmp ,t2))))]
                                   [else           (values t2 '())])])
        (if (or (integer? t1) (integer? t2))
            (append `(begin) pre1 pre2 `((,relop ,tmp1 ,tmp2)))
            `(,relop ,tmp1 ,tmp2)))
      (error (format "select-instructions:  Failed match.\n No valid trivs: ~a ~a" t1 t2))))


;
;(select-value v tail)->list? '((set! a b) ... tail?)
;v: value?
;tail:list? '(set! a)
(define (select-value v tail)
  (match v
    [`(,binop ,t1 ,t2) (select-binop binop t1 t2 tail)]
    [v #:when (triv? v) (append tail `(,v))]
    [_ (error (format "select-instructions:  Failed match.\n No valid value ~a" v))]))

;
;(select-begin e)->list? '(effect? ...)
;e; list? '(effect ...)
(define (select-begin e)
  (map select-effect e))

;
;(select-pred p)->pred?
;p: pred?
(define (select-pred p)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(select-begin e) ,(select-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,(select-pred p1) ,(select-pred p2) ,(select-pred p3))]
    [`(,relop ,a ,b) (select-relop a b relop)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(select-pred pred))]                  
    [_ (error (format "select-instructions:  Failed match.\n No valid pred: ~a" p))]))

;
;(select-effect e)->effect?
;e: effect?
(define (select-effect e)
  (match e
    [`(begin ,e ...) `(begin ,@(select-begin e))]
    [`(if ,p ,e1 ,e2) `(if ,(select-pred p) ,(select-effect e1) ,(select-effect e2))]
    [`(set! ,a ,v) #:when (loc? a) (select-value v `(set! ,a))]
    [`(return-point ,l ,t) `(return-point ,l ,(select-tail t))]
    [_ (error (format "select-instructions:  Failed match.\n No valid effect: ~a" e))]))

;
;(select-tail t)->tail?
;t: tail?
(define (select-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(select-begin e) ,(select-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(select-pred p) ,(select-tail t1) ,(select-tail t2))]
    [`(jump-call ,l ,a ...) `(jump-call ,l ,@a)]
    [`(jump-return ,l ,a ...) `(jump-return ,l ,@a)]
    [_ (error (format "select-instructions:  Failed match.\n No valid tail: ~a" t))]))

;
;(select-func f)->'(define label? info? tail?)
;f: '(define label? tail?)
(define (select-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(select-tail t))]
    [_ (error (format "select-instructions:  Failed match.\n No valid function: ~a" f))]))

;Compiles Imp-lang-V3-cmf to Asm-lang-V2, selecting appropriate sequences of abstract assembly instructions to implement the operations of the source language
;(select-instructions p) → Asm-lang-V2
;p: Imp-lang-V3-cmf
(define/contract (select-instructions p) (-> imp-cmf-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,i ,@(map select-func f) ,(select-tail t))]))

(module+ test
  (define (check-select t1 t2 text)
    (resetfresh)
    (check-equal? t1 t2 text))
  ;#|
  ;select-binop
  ;succes
  (check-select (select-binop '+ 5 4 '(set! a0))
                '(begin (set! tmp.1 5) (set! a0 (+ tmp.1 4)))
                "select-binop: succes-01: add int int")
  (check-select (select-binop '* 5 4 '(set! a0))
                '(begin (set! tmp.1 5) (set! tmp.2 4) (set! a0 (* tmp.1 tmp.2)))
                "select-binop: succes-02: mul int int")
  (check-select (select-binop '+ 5 'x.1 '(set! a0))
                '(begin (set! tmp.1 5) (set! a0 (+ tmp.1 x.1)))
                "select-binop: succes-03: add int aloc")
  (check-select (select-binop '+ 'x.1 4 `(set! y.3))
                '(set! y.3 (+ x.1 4))
                "select-binop: succes-04: add aloc int")
  (check-select (select-binop '* 'x.1 4 `(set! y.3))
                '(begin (set! tmp.1 4) (set! y.3 (* x.1 tmp.1)))
                "select-binop: succes-05: mul aloc int")
  (check-select (select-binop '+ 'x.1 'x.2 `(set! y.3))
                '(set! y.3 (+ x.1 x.2))
                "select-binop: succes-06: add aloc aloc")
  (check-select (select-binop '+ 'x.1 5000 '(set! a0))
                '(set! a0 (+ x.1 5000))
                "select-binop: succes-07: add aloc int32")
  (check-select (select-binop '+ 'x.1 -5000 '(set! a0))
                '(set! a0 (+ x.1 -5000))
                "select-binop: succes-08: add aloc int32")
  (check-select (select-binop '+ 5 5000 '(set! a0))
                '(begin (set! tmp.1 5) (set! a0 (+ tmp.1 5000)))
                "select-binop: succes-09: add int int32")
  (check-select (select-binop '+ 5000 5000 '(set! a0))
                '(begin (set! tmp.1 5000) (set! a0 (+ tmp.1 tmp.1)))
                "select-binop: succes-10: add int32 int32")
  (check-select (select-binop '+ 'x.1 214748364845 '(set! y.2))
                '(set! y.2 (+ x.1 214748364845))
                "select-binop: succes-11: add aloc int>32")
  ;failure
  (check-exn exn:fail? (thunk (select-binop '+ 'x.1 52 '(halt))) "select-binop: failure-01: wrong tail")
  (check-exn exn:fail? (thunk (select-binop '+ 'symp1 52 '(set! y.2))) "select-binop: failure-02: first is no triv")
  (check-exn exn:fail? (thunk (select-binop '+ 52 'symp2 '(set! y.2))) "select-binop: failure-03: second is no triv")
  ;select-relop
  ;succes
  (check-select (select-relop 'x.1 'x.1 '=) '(= x.1 x.1) "select-relop: succes-01: both names")
  (check-select (select-relop 'x.1 5 '=) '(begin (set! tmp.1 5) (= x.1 tmp.1)) "select-relop: succes-02: one int")
  (check-select (select-relop 4 'x.1 '=) '(begin (set! tmp.1 4) (= tmp.1 x.1)) "select-relop: succes-03: one int")
  (check-select (select-relop 4 5 '=) '(begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2)) "select-relop: succes-04: both int")
  (check-select (select-relop 5 5 '=) '(begin (set! tmp.1 5) (= tmp.1 tmp.1)) "select-relop: succes-05: both int same")
  ;failure
  (check-exn exn:fail? (thunk (select-relop 'symp1 5 '=)) "select-relop: failure-01: first is no triv")
  (check-exn exn:fail? (thunk (select-relop 5 'symp1 '=)) "select-relop: failure-02: second is no triv")
  ;select-value
  ;succes
  (check-select (select-value 5 '(set! a0))
                '(set! a0 5)
                "select-value: succes-1: integer")
  (check-select (select-value 'x.1 '(set! y.3))
                '(set! y.3 x.1)
                "select-value: succes-2: aloc")
  (check-select (select-value '(* 5 4) '(set! a0))
                '(begin (set! tmp.1 5) (set! tmp.2 4) (set! a0 (* tmp.1 tmp.2)))
                "select-value: succes-3: operation")
  ;failure
  (check-exn exn:fail? (thunk (select-value 'symp1 '(set! a0))) "select-value: failure-01: first is no triv")
  ;select-pred
  ;succes
  (check-select (select-pred '(= x.1 y.2)) '(= x.1 y.2) "select-pred: succes-01: relop all names")
  (check-select (select-pred '(= 4 5)) '(begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2)) "select-pred: succes-02: relop has int")
  (check-select (select-pred '(true)) '(true) "select-pred: succes-03: true")
  (check-select (select-pred '(false)) '(false) "select-pred: succes-04: false")
  (check-select (select-pred '(not (= 4 5))) '(not (begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2))) "select-pred: succes-05: not")

  (check-select (select-pred '(begin (set! x.1 4) (set! y.1 5) (= 4 5)))
                '(begin (set! x.1 4) (set! y.1 5) (begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2))) "select-pred: succes-06: begin")
  (check-select (select-pred '(begin (set! x.1 4) (begin (set! x.1 4) (set! y.2 5) (set! x.1 (* x.1 y.2))) (= 4 5)))
                '(begin (set! x.1 4) (begin (set! x.1 4) (set! y.2 5) (set! x.1 (* x.1 y.2))) (begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2)))
                "select-pred: succes-07: nested begin")

  (check-select (select-pred '(if (= 4 5) (= 4 5) (= 4 5)))
                '(if (begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2)) (begin (set! tmp.3 4) (set! tmp.4 5) (= tmp.3 tmp.4)) (begin (set! tmp.5 4) (set! tmp.6 5) (= tmp.5 tmp.6)))
                "select-pred: succes-08: if")
  ;failure
  (check-exn exn:fail? (thunk (select-pred 'symp1)) "select-pred: failure-01: no pred")

  ;select-effect
  ;succes
  (check-select (select-effect '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5)))
                '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5))
                "select-effect: succes-1: set of effecten")
  (check-select (select-effect '(set! x.1 (+ 5 4)))
                '(begin (set! tmp.1 5) (set! x.1 (+ tmp.1 4)))
                "select-effect: succes-2: set of effecten")
  (check-select (select-effect '(begin (set! x.1 5) (set! x.1 5) (set! x.1 5)))
                '(begin (set! x.1 5) (set! x.1 5) (set! x.1 5))
                "select-effect: succes-3: set of effecten")
  (check-select (select-effect '(if (= 4 5) (set! x.1 (+ 4 5)) (set! y.2 4)))
                '(if (begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2))
                     (begin (set! tmp.3 4) (set! x.1 (+ tmp.3 5)))
                     (set! y.2 4))
                "select-effect: succes-4: if")
  ;failure
  (check-exn exn:fail? (thunk (select-effect 'symp1)) "select-effect: failure-01: no effect")

  ;select-tail
  ;succes
  (check-select (select-tail '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5) (jump-call L.foo.1)))
                '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5) (jump-call L.foo.1))
                "select-tail: succes-1: begin")
  (check-select (select-tail '(begin (set! a0 (+ 5 4)) (jump-call L.foo.1)))
                '(begin (begin (set! tmp.1 5) (set! a0 (+ tmp.1 4))) (jump-call L.foo.1))
                "select-tail: succes-2: binop")
  (check-select (select-tail '(begin (set! a0 x.1) (jump-return L.foo.1)))
                '(begin (set! a0 x.1) (jump-return L.foo.1))
                "select-tail: succes-3: triv")
  (check-select (select-tail '(if (= 4 5) (begin (set! a0 (+ 4 5)) (jump-return L.foo.1)) (begin (set! a0 4) (jump-return L.foo.1))))
                '(if (begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2))
                     (begin (begin (set! tmp.3 4) (set! a0 (+ tmp.3 5))) (jump-return L.foo.1))
                     (begin (set! a0 4) (jump-return L.foo.1)))
                "select-tail: succes-4: if")
  ;failure
  (check-exn exn:fail? (thunk (select-tail 'symp1)) "select-tail: failure-01: no tail")
  ;select-func
  ;succes
  (check-equal? (select-func '(define L.odd.1
                                ((new-frames ()) (paramSize 0))
                                (begin
                                  (set! tmp-ra.1 cra)
                                  (begin
                                    (set! x.1 a1)
                                    (set! y.2 a2)
                                    (set! z.3 a3)
                                    (set! x.1 (+ 5 5))
                                    (jump-return cra cfp)))))
                '(define L.odd.1
                   ((new-frames ()) (paramSize 0))
                   (begin
                     (set! tmp-ra.1 cra)
                     (begin
                       (set! x.1 a1)
                       (set! y.2 a2)
                       (set! z.3 a3)
                       (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                       (jump-return cra cfp))))
                "select-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (select-func '(defiene L.odd.1
                                              ((new-frames ()) (paramSize 0))
                                              (begin
                                                (set! tmp-ra.1 cra)
                                                (begin
                                                  (set! x.1 a1)
                                                  (set! y.2 a2)
                                                  (set! z.3 a3)
                                                  (set! x.1 (+ 5 5))
                                                  (jump-return cra cfp))))))
             "select-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (select-func '(define 
                                              ((new-frames ()) (paramSize 0))
                                              (begin
                                                (set! tmp-ra.1 cra)
                                                (begin
                                                  (set! x.1 a1)
                                                  (set! y.2 a2)
                                                  (set! z.3 a3)
                                                  (set! x.1 (+ 5 5))
                                                  (jump-return cra cfp))))))
             "select-func: failure-02: no name")
  (check-exn exn:fail? (thunk (select-func '(define L.odd.1
                                              (begin
                                                (set! tmp-ra.1 cra)
                                                (begin
                                                  (set! x.1 a1)
                                                  (set! y.2 a2)
                                                  (set! z.3 a3)
                                                  (set! x.1 (+ 5 5))
                                                  (jump-return cra cfp))))))
             "select-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (select-func '(define L.odd.1
                                              ((new-frames ()) (paramSize 0)))))
             "select-func: failure-04:  no tail")
  #|
;select-instructions
  ;succes
  (check-select (select-instructions '(module () (begin (set! a0 (+ 2 2)) (jump-call L.foo.1))))
                '(module () (begin (begin (set! tmp.1 2) (set! a0 (+ tmp.1 tmp.1))) (jump-call L.foo.1)))
                "select-instructions: succes-01: one operation")
  
  (check-select (select-instructions
                 '(module ()
                      (begin (set! x.1 5) (set! a0 x.1) (jump-call L.foo.1))))
                '(module () (begin (set! x.1 5) (set! a0 x.1) (jump-call L.foo.1)))
                "select-instructions: succes-02: one set")
  (check-select (select-instructions
                 '(module ()
                      (begin
                        (set! x.1 (+ 2 3))
                        (set! a0 x.1) (jump-return L.foo.1))))
                '(module () (begin (begin (set! tmp.1 2) (set! x.1 (+ tmp.1 3))) (set! a0 x.1) (jump-return L.foo.1)))
                "select-instructions: succes-03: a set with operation     !!!anders dan oplossing boek sinds b in binop kan verschillend zijn dan a")
  (check-select (select-instructions
                 '(module ()
                      (begin
                        (set! x.1 2)
                        (set! x.2 2)
                        (begin (set! a0 (+ x.1 x.2)) (jump-return L.foo.1)))))
                '(module
                     ()
                   (begin
                     (set! x.1 2)
                     (set! x.2 2)
                     (begin (set! a0 (+ x.1 x.2)) (jump-return L.foo.1))))
                "select-instructions: succes-04: multiple instructions")
  (check-select (select-instructions
                 '(module () (define L.test.1 () (begin  (set! x.2 fv0) (set! x.3 fv1) (set! x.1 ca0) (begin (set! y.4 (+ x.1 x.2)) (set! a0 (+ x.3 y.4)) (jump-return L.foo.1))))
                    (begin (set! fv0 2) (set! fv1 3) (set! ca0 1) (jump-call L.test.1 cfp ca0 fv0 fv1))))
                '(module ()
                   (define L.test.1
                     ()
                     (begin
                       (set! x.2 fv0)
                       (set! x.3 fv1)
                       (set! x.1 ca0)
                       (begin
                         (set! y.4 (+ x.1 x.2))
                         (set! a0 (+ x.3 y.4)) (jump-return L.foo.1))))
                   (begin
                     (set! fv0 2)
                     (set! fv1 3)
                     (set! ca0 1)
                     (jump-call L.test.1 cfp ca0 fv0 fv1)))
                "select-instructions: succes-05: tail Call")
  ;|#
  )
