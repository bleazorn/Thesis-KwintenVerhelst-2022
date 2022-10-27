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
                     [(and (integer? t2) (not (equal? binop '+))) (freshtmp)]
                     [else t2])]
             [pre1 (if (integer? t1) `((set! ,tmp1 ,t1)) '())]
             [pre2 (cond
                     [(and (integer? t2) (not (equal? binop '+))) (append pre1 `((set! ,tmp2 ,t2)))]
                     [else pre1])])
        (append pre2 `((set! ,tmp1 (,binop ,tmp1 ,tmp2))) `(,(append tail `(,tmp1)))))
      #f))

;
;(select-relop t1 t2 relop)->list? '(pred? ...)
;t1: triv?
;t2: triv?
;relop: relop?
(define (select-relop t1 t2 relop)
  (if (and (triv? t1) (triv? t2))
      (let* ([tmp1 (if (integer? t1) (freshtmp) t1)]
             [tmp2 (if (integer? t2) (freshtmp) t2)]
             [pre1 (if (integer? t1) `((set! ,tmp1 ,t1)) '())]
             [pre2 (if (integer? t2) `((set! ,tmp2 ,t2)) '())])
        (append pre1 pre2 `((,relop ,tmp1 ,tmp2))))
      #f))

;
;(select-pred p)->list? '(pred? ...)
;p: pred?
(define (select-pred p)
  (match p
    [`(,relop ,a ,b) (select-relop a b relop)]
    ['(true) '((true))]
    ['(false) '((false))]
    [`(not ,pred) `(not ,(select-pred pred))]                  ;!!!! NIET CORRECT
    [`(begin ,e ... ,pred) `(begin ,@(foldl (lambda (eff newe) (append newe (select-effect eff))) '() e) ,@(select-pred pred))]
    [`(if ,p1 ,p2 ,p3) `(if ,@(select-pred p1) ,@(select-pred p2) ,@(select-pred p3))]
    [_ #f]))

;
;(select-value v tail)->list? '((set! a b) ... tail?)
;v: value?
;tail:list? '(set! a) of '(halt)
(define (select-value v tail)
  (match v
    [`(,binop ,t1 ,t2) (select-binop binop t1 t2 tail)]
    [v #:when (or (integer? v) (aloc? v)) `(,(append tail `(,v)))]
    [_ #f]))


;
;(select-effect e)->list? '(effect? ...)
;e: effect?
(define (select-effect e)
  (match e
    [`(set! ,a ,v) #:when (aloc? a) (select-value v `(set! ,a))]
    [`(begin ,e ...) `(begin ,@(foldl (lambda (eff newe) (append newe (select-effect eff))) '() e))]
    [`(if ,p ,e1 ,e2) `(if ,@(select-pred p) ,@(select-effect e1) ,@(select-effect e2))]
    [_ #f]))

;
;(select-tail t)->tail?
;t: tail?
(define (select-tail t)
  (match t
    [`(begin ,e ... ,tail) (append (foldl (lambda (eff f) (append f (select-effect eff))) '() e) (select-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,(select-pred p) ,(select-tail t1) ,(select-tail t2))]
    [v (select-value v '(halt))]))

;Compiles Imp-lang-V3-cmf to Asm-lang-V2, selecting appropriate sequences of abstract assembly instructions to implement the operations of the source language
;(select-instructions p) → Asm-lang-V2
;p: Imp-lang-V3-cmf
(define (select-instructions p)
  (match p
    [`(module ,t) `(module () (begin ,@(select-tail t)))]
    [_ #f]))

(module+ test
  (define (check-select t1 t2 text)
    (resetfresh)
    (check-equal? t1 t2 text))
;select-binop
  ;succes
  (check-select (select-binop '+ 5 4 '(halt))
                '((set! tmp.1 5) (set! tmp.1 (+ tmp.1 4)) (halt tmp.1))
                "select-binop: succes-1: add int int")
  (check-select (select-binop '* 5 4 '(halt))
                '((set! tmp.1 5) (set! tmp.2 4) (set! tmp.1 (* tmp.1 tmp.2)) (halt tmp.1))
                "select-binop: succes-2: mul int int")
  (check-select (select-binop '+ 5 'x.1 '(halt))
                '((set! tmp.1 5) (set! tmp.1 (+ tmp.1 x.1)) (halt tmp.1))
                "select-binop: succes-3: add int aloc")
  (check-select (select-binop '+ 'x.1 4 `(set! y.3))
                '((set! x.1 (+ x.1 4)) (set! y.3 x.1))
                "select-binop: succes-4: add aloc int")
  (check-select (select-binop '* 'x.1 4 `(set! y.3))
                '((set! tmp.1 4) (set! x.1 (* x.1 tmp.1)) (set! y.3 x.1))
                "select-binop: succes-5: mul aloc int")
  (check-select (select-binop '+ 'x.1 'x.2 `(set! y.3))
                '((set! x.1 (+ x.1 x.2)) (set! y.3 x.1))
                "select-binop: succes-6: add aloc aloc")
  (check-select (select-binop '+ 'x.1 5000 '(halt))
                '((set! tmp.1 5000) (set! x.1 (+ x.1 tmp.1)) (halt x.1))
                "select-binop: succes-7: add aloc int32")
  (check-select (select-binop '+ 'x.1 -5000 '(halt))
                '((set! tmp.1 -5000) (set! x.1 (+ x.1 tmp.1)) (halt x.1))
                "select-binop: succes-8: add aloc int32")
  (check-select (select-binop '+ 5 5000 '(halt))
                '((set! tmp.1 5) (set! tmp.2 5000) (set! tmp.1 (+ tmp.1 tmp.2)) (halt tmp.1))
                "select-binop: succes-7: add int int32")
  (check-select (select-binop '+ 5000 5000 '(halt))
                '((set! tmp.1 5000) (set! tmp.2 5000) (set! tmp.1 (+ tmp.1 tmp.2)) (halt tmp.1))
                "select-binop: succes-7: add int32 int32")
  (check-select (select-binop '+ 'x.1 214748364845 '(halt))
                '((set! tmp.1 214748364845) (set! x.1 (+ x.1 tmp.1)) (halt x.1))
                "select-binop: succes-7: add aloc int>32")
;select-value
  ;succes
  (check-select (select-value 5 '(halt))
                '((halt 5))
                "select-value: succes-1: integer")
  (check-select (select-value 'x.1 '(set! y.3))
                '((set! y.3 x.1))
                "select-value: succes-2: aloc")
  (check-select (select-value '(* 5 4) '(halt))
                '((set! tmp.1 5) (set! tmp.2 4) (set! tmp.1 (* tmp.1 tmp.2)) (halt tmp.1))
                "select-value: succes-3: operation")
  ;failure
  (check-select (select-value 'x '(halt))
                #f
                "select-value: failure-1: not an aloc")

;select-effect
  ;succes
  (check-select (select-effect '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5)))
                '(begin (set! x.1 5) (set! x.2 x.1) (set! x.1 5))
                "select-effect: succes-1: set of effecten")
  (check-select (select-effect '(set! x.1 (+ 5 4)))
                '(begin (set! x.1 5) (set! x.1 5) (set! x.1 5))
                "select-effect: succes-1: set of effecten")
  (check-select (select-effect '(begin (set! x.1 5) (set! x.1 5) (set! x.1 5)))
                '(begin (set! x.1 5) (set! x.1 5) (set! x.1 5))
                "select-effect: succes-1: set of effecten")
  
;select-instructions
  ;succes
  (check-select (select-instructions '(module (+ 2 2)))
                '(module () (begin (set! tmp.1 2) (set! tmp.2 2) (set! tmp.1 (+ tmp.1 tmp.2)) (halt tmp.1)))
                "select-instructions: succes-1: one operation")
  
  (check-select (select-instructions
                 '(module
                      (begin (set! x.1 5) x.1)))
                '(module () (begin (set! x.1 5) (halt x.1)))
                "select-instructions: succes-2: one set")
  (check-select (select-instructions
                 '(module
                      (begin
                        (set! x.1 (+ 2 2))
                        x.1)))
                '(module () (begin (set! x.1 2) (set! tmp.1 2) (set! x.1 (+ x.1 tmp.1)) (halt x.1)))
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
                     (set! tmp.2 x.1)
                     (set! tmp.2 (+ tmp.2 x.2))
                     (halt tmp.2)))
                "select-instructions: succes-4: multiple instructions")
  )
