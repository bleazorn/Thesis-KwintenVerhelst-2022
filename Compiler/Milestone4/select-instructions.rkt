#lang racket

(require "common.rkt")
(provide select-instructions)

(module+ test
  (require rackunit))


(define (triv? t)
  (or (aloc? t) (integer? t)))

;
;(select-binop binop t1 t2 tail)->list? '((set! a b) ... tail?)
;binop: symbol?
;t1: triv?
;t2: triv?
;tail:list? '(set! a) of '(halt)
(define (select-binop binop t1 t2 tail)
  (if (and (triv? t1) (triv? t2))
      (let* ([tmp1 (if (integer? t1) (freshtmp) t1)]
             [tmp2 (cond
                     [(equal? t1 t2) tmp1]
                     [(and (integer? t2) (not (equal? binop '+))) (freshtmp)]
                     [else t2])]
             [tmp3 (match tail
                     [`(set! ,a) a]           
                     [`(halt) tmp1])]
             [pre1 (if (integer? t1) `((set! ,tmp1 ,t1)) '())]
             [pre2 (cond
                     [(equal? t1 t2) '()]
                     [(and (integer? t2) (not (equal? binop '+))) `((set! ,tmp2 ,t2))]
                     [else '()])]
             [pre3 (match tail
                     [`(set! ,a) '()]           
                     [`(halt) `((halt ,tmp1))])])
        (if (or (or (isTmp? tmp1) (isTmp? tmp2)) (not (null? pre3)))
            (append `(begin) pre1 pre2 `((set! ,tmp3 (,binop ,tmp1 ,tmp2))) pre3)
            `(set! ,tmp3 (,binop ,tmp1 ,tmp2))))
        
      #f))

;
;(select-relop t1 t2 relop)->list? '(pred? ...)
;t1: triv?
;t2: triv?
;relop: relop?
(define (select-relop t1 t2 relop)
  (if (and (triv? t1) (triv? t2))
      (let* ([tmp1 (if (integer? t1) (freshtmp) t1)]
             [tmp2 (cond
                     [(equal? t1 t2) tmp1]
                     [(integer? t2) (freshtmp)]
                     [else t2])]
             [pre1 (if (integer? t1) `((set! ,tmp1 ,t1)) '())]
             [pre2 (cond
                     [(equal? t1 t2) '()]
                     [(integer? t2) `((set! ,tmp2 ,t2))]
                     [else '()])])
        (if (or (integer? t1) (integer? t2))
            (append `(begin) pre1 pre2 `((,relop ,tmp1 ,tmp2)))
            `(,relop ,tmp1 ,tmp2)))
      #f))


;
;(select-value v tail)->list? '((set! a b) ... tail?)
;v: value?
;tail:list? '(set! a) of '(halt)
(define (select-value v tail)
  (match v
    [`(,binop ,t1 ,t2) (select-binop binop t1 t2 tail)]
    [v #:when (or (integer? v) (aloc? v)) (append tail `(,v))]
    [_ #f]))

;
;(select-begin e)->list? '(effect? ...)
;e; list? '(effect ...)
(define (select-begin e)
  (map (lambda (eff) (select-effect eff)) e))

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
    [_ #f]))

;
;(select-effect e)->effect?
;e: effect?
(define (select-effect e)
  (match e
    [`(begin ,e ...) `(begin ,@(select-begin e))]
    [`(if ,p ,e1 ,e2) `(if ,(select-pred p) ,(select-effect e1) ,(select-effect e2))]
    [`(set! ,a ,v) #:when (aloc? a) (select-value v `(set! ,a))]
    [_ #f]))

;
;(select-tail t)->tail?
;t: tail?
(define (select-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(select-begin e) ,(select-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(select-pred p) ,(select-tail t1) ,(select-tail t2))]
    [v (select-value v '(halt))]))

;Compiles Imp-lang-V3-cmf to Asm-lang-V2, selecting appropriate sequences of abstract assembly instructions to implement the operations of the source language
;(select-instructions p) → Asm-lang-V2
;p: Imp-lang-V3-cmf
(define (select-instructions p)
  (match p
    [`(module ,t) `(module () ,(select-tail t))]
    [_ #f]))

(module+ test
  (define (check-select t1 t2 text)
    (resetfresh)
    (check-equal? t1 t2 text))
;select-binop
  ;succes
  (check-select (select-binop '+ 5 4 '(halt))
                '(begin (set! tmp.1 5) (set! tmp.1 (+ tmp.1 4)) (halt tmp.1))
                "select-binop: succes-01: add int int")
  (check-select (select-binop '* 5 4 '(halt))
                '(begin (set! tmp.1 5) (set! tmp.2 4) (set! tmp.1 (* tmp.1 tmp.2)) (halt tmp.1))
                "select-binop: succes-02: mul int int")
  (check-select (select-binop '+ 5 'x.1 '(halt))
                '(begin (set! tmp.1 5) (set! tmp.1 (+ tmp.1 x.1)) (halt tmp.1))
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
  (check-select (select-binop '+ 'x.1 5000 '(halt))
                '(begin (set! x.1 (+ x.1 5000)) (halt x.1))
                "select-binop: succes-07: add aloc int32")
  (check-select (select-binop '+ 'x.1 -5000 '(halt))
                '(begin (set! x.1 (+ x.1 -5000)) (halt x.1))
                "select-binop: succes-08: add aloc int32")
  (check-select (select-binop '+ 5 5000 '(halt))
                '(begin (set! tmp.1 5) (set! tmp.1 (+ tmp.1 5000)) (halt tmp.1))
                "select-binop: succes-09: add int int32")
  (check-select (select-binop '+ 5000 5000 '(halt))
                '(begin (set! tmp.1 5000) (set! tmp.1 (+ tmp.1 tmp.1)) (halt tmp.1))
                "select-binop: succes-10: add int32 int32")
  (check-select (select-binop '+ 'x.1 214748364845 '(set! y.2))
                '(set! y.2 (+ x.1 214748364845))
                "select-binop: succes-11: add aloc int>32")
;select-relop
  ;succes
  (check-select (select-relop 'x.1 'x.1 '=) '(= x.1 x.1) "select-relop: succes-01: both names")
  (check-select (select-relop 'x.1 5 '=) '(begin (set! tmp.1 5) (= x.1 tmp.1)) "select-relop: succes-02: one int")
  (check-select (select-relop 4 'x.1 '=) '(begin (set! tmp.1 4) (= tmp.1 x.1)) "select-relop: succes-03: one int")
  (check-select (select-relop 4 5 '=) '(begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2)) "select-relop: succes-04: both int")
  (check-select (select-relop 5 5 '=) '(begin (set! tmp.1 5) (= tmp.1 tmp.1)) "select-relop: succes-05: both int same")
;select-value
  ;succes
  (check-select (select-value 5 '(halt))
                '(halt 5)
                "select-value: succes-1: integer")
  (check-select (select-value 'x.1 '(set! y.3))
                '(set! y.3 x.1)
                "select-value: succes-2: aloc")
  (check-select (select-value '(* 5 4) '(halt))
                '(begin (set! tmp.1 5) (set! tmp.2 4) (set! tmp.1 (* tmp.1 tmp.2)) (halt tmp.1))
                "select-value: succes-3: operation")
  ;failure
  (check-select (select-value 'x '(halt))
                #f
                "select-value: failure-1: not an aloc")
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

;select-tail
  ;succes
   (check-select (select-tail '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5) x.1))
                 '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5) (halt x.1))
                 "select-tail: succes-1: begin")
  (check-select (select-tail '(+ 5 4))
                '(begin (set! tmp.1 5) (set! tmp.1 (+ tmp.1 4)) (halt tmp.1))
                "select-tail: succes-2: binop")
  (check-select (select-tail 'x.1)
                '(halt x.1)
                "select-tail: succes-3: triv")
  (check-select (select-tail '(if (= 4 5) (+ 4 5) 4))
                '(if (begin (set! tmp.1 4) (set! tmp.2 5) (= tmp.1 tmp.2))
                     (begin (set! tmp.3 4) (set! tmp.3 (+ tmp.3 5)) (halt tmp.3))
                     (halt 4))
                "select-tail: succes-4: if")
  
;select-instructions
  ;succes
  (check-select (select-instructions '(module (+ 2 2)))
                '(module () (begin (set! tmp.1 2) (set! tmp.1 (+ tmp.1 tmp.1)) (halt tmp.1)))
                "select-instructions: succes-1: one operation")
  
  (check-select (select-instructions
                 '(module
                      (begin (set! x.1 5) x.1)))
                '(module () (begin (set! x.1 5) (halt x.1)))
                "select-instructions: succes-2: one set")
  (check-select (select-instructions
                 '(module
                      (begin
                        (set! x.1 (+ 2 3))
                        x.1)))
                '(module () (begin (begin (set! tmp.1 2) (set! x.1 (+ tmp.1 3))) (halt x.1)))
                "select-instructions: succes-3: a set with operation     !!!anders dan oplossing boek sinds b in binop kan verschillend zijn dan a")
  (check-select (select-instructions
                 '(module
                      (begin
                        (set! x.1 2)
                        (set! x.2 2)
                        (+ x.1 x.2))))
                '(module
                     ()
                   (begin
                     (set! x.1 2)
                     (set! x.2 2)
                     (begin (set! x.1 (+ x.1 x.2)) (halt x.1))))
                "select-instructions: succes-4: multiple instructions")
  )
