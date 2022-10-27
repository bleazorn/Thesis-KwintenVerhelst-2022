#lang racket

(provide expose-basic-blocks)

(module+ test
  (require rackunit))

(define l 1)
(define (newLabel)
  (set! l (add1 l))
  (format "L.tmp.~a" l))

;
;
;b: list? '(effect ...)
(define (expose-begin-effect b curLabel)
  (foldl (lambda (e l) (append l (expose-effect e curLabel))) '() b))

;
;(expose-effect e)->list? '(effect ...)
;e: effect?
(define (expose-effect e curLabel)
  (match e
    [`(set! ,a ,b) `((set! ,a ,b))]
    [`(begin ,e ...) (expose-begin-effect e curLabel)]
    [`(if ,pred ,e1 ,e2) (let ([l1 (newLabel)]
                               [l2 (newLabel)]
                               [l3 (newLabel)])
                           `((define l1 `(if (expose-pred pred) (jump l1) (jump l2)))
                             (define l2 ((expose-effect e1) (jump l1)
    [_ #f]))


;
;
;p: pred?
(define (expose-pred p curLabel)
  (match p
    [`(,relop ,a ,b) `((,relop ,a ,b))]
    ['(true) '((true))]
    ['(false) '((false))]
    [`(not ,pred) `((not ,@(expose-pred pred curLabel)))]
    [`(begin ,e ... ,pred) (append (expose-begin-effect e curLabel) (expose-pred pred curLabel))]
    [`(if ,pred ,pred ,pred) '()]
    [_ #f]))

;
;
;t: tail?
(define (expose-tail t curLabel)
  (match t
    [`(begin ,e ... ,t) (append (expose-begin-effect e curLabel) (expose-tail t curLabel))]
    [`(halt ,a) `((halt ,a))]
    [`(if ,pred ,t1 ,t2) (let ([l1 (newLabel)]
                               [l2 (newLabel)])
                           '((if (expose-pred pred) (jump l1) (jump l2))
                             (define l1 (expose-tail t1))
                             (define l2 (expose-tail t2))))]
    [_ #f]))

;
;(expose-basic-blocks p)->Asm-lang-V4-nested
;p:Pred-lang-V4-Block
(define (expose-basic-blocks p)
  (match p
    [`(module ,t) `(module ,(expose-tail t))]
    [_ #f]))

(module+ test
;expose-effect
  ;succes
  (check-equal? (expose-effect '(set! a0 (+ a1 a2)) "L.l.1") '((set! a0 (+ a1 a2))) "expose-effect: succes-1: set instruction")
  (check-equal? (expose-effect '(begin (set! a0 (+ a1 a2))) "L.l.1") '((set! a0 (+ a1 a2))) "expose-effect: succes-2: begin one effect")
  (check-equal? (expose-effect '(begin (set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! fv0 a0)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! fv0 a0))
                "expose-effect: succes-3: begin multiple effects")
  (check-equal? (expose-effect '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (set! fv0 a0)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (set! fv0 a0))
                "expose-effect: succes-4: nested begin")
;expose-pred
  ;succes
  (check-equal? (expose-pred '(= a0 a1) "L.l.1") '((= a0 a1)) "expose-pred: succes-1: relop instruction")
  (check-equal? (expose-pred '(true) "L.l.1") '((true)) "expose-pred: succes-2: true")
  (check-equal? (expose-pred '(false) "L.l.1") '((false)) "expose-pred: succes-3: false")
  (check-equal? (expose-pred '(not (true)) "L.l.1") '((not (true))) "expose-pred: succes-4: not instruction")
  (check-equal? (expose-pred '(begin (= a0 a1)) "L.l.1") '((= a0 a1)) "expose-pred: succes-5: begin one effect")
  (check-equal? (expose-pred '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (true)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (true))
                "expose-pred: succes-6: nested begin")
;expose-tail
  ;succes
  (check-equal? (expose-tail '(halt a0) "L.l.1") '((halt a0)) "expose-tail: succes-1: halt instruction")
  (check-equal? (expose-tail '(begin (set! a0 (+ a1 a2)) (halt a0)) "L.l.1") '((set! a0 (+ a1 a2)) (halt a0)) "expose-tail: succes-2: begin one effect")
  (check-equal? (expose-tail '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (halt a0)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (halt a0))
                "expose-tail: succes-3: nested begin")

  )