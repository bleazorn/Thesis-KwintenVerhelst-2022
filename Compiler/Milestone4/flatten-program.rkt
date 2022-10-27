#lang racket

(provide flatten-program)

(module+ test
  (require rackunit))

;
;(flatten-begin effects)->list? '((set! ...) ...)
;effects->list? '(effect? ...)
(define (flatten-begin effects)
  (foldl (lambda (eff pro) (append pro (flatten-effect eff))) '() effects))

;
;(flatten-effect e)->list? '((set! ...) ...)
;e: effect?
(define (flatten-effect e)
  (match e
    [`(begin ,e ...) (flatten-begin e)]
    [`(set! ,a ,b) `((set! ,a ,b))]
    [_ #f]))

;
;(flatten-tail t)->list? '((set! ...) ...)
;t: tail?
(define (flatten-tail t)
  (match t
    [`(begin ,e ... ,tail) `(,@(flatten-begin e) ,@(flatten-tail tail))]
    [`(halt ,triv) `((halt ,triv))]
    [`(jump ,l) `((jump ,l))]
    [`(if (,relop ,a ,b) (jump ,l1) (jump ,l2)) `((jump-if ,l1 (relop ,a ,b)) (jump ,l2))]
    [_ #f]))

;
;(flatten-begin b)->list? '((set! ...) ...)
;b:b?
(define (flatten-b b)
  (match b
    [`(define ,l ,t) (let ([tail (flatten-tail t)])
                       (cons `(with-label ,l ,(car tail)) (cdr tail)))]
    [_ #f]))

;Flatten all nested begin expressions.
;(flatten-begins p) → Paren-Cheri-Risc-V-V2-lang?
;p: Asm-lang-V2-nested?
(define (flatten-program p)
  (match p
    [`(module ,b ...) `(begin ,@(foldl (lambda (beg l) (append l (flatten-b beg))) '() b))]
    [_ #f]))

(module+ test
;flatten-tail:
  ;succes
  (check-equal? (flatten-tail '(begin (set! a0 50) (halt a0)))
                '((set! a0 50) (halt a0))
                "flatten-tail: succes-1: no nested")
  (check-equal? (flatten-tail '(begin (set! a1 50) (begin (set! a0 50) (halt a0))))
                '((set! a1 50) (set! a0 50) (halt a0))
                "flatten-tail: succes-2: tail nested")
  (check-equal? (flatten-tail '(begin (begin (set! a1 50) (set! a2 50)) (begin (set! a0 50) (halt a0))))
                '((set! a1 50) (set! a2 50) (set! a0 50) (halt a0))
                "flatten-tail: succes-3: effect and tail nested")
  (check-equal? (flatten-tail '(begin (begin (set! a1 50) (set! a2 50)) (begin (begin (set! a0 50) (halt a0)))))
                '((set! a1 50) (set! a2 50) (set! a0 50) (halt a0))
                "flatten-tail: succes-4: tail double nested")
  (check-equal? (flatten-tail '(begin (set! a4 50) (begin (set! a3 50) (begin (set! a1 50) (set! a2 50))) (set! a5 50) (begin (begin (set! a0 50) (halt a0)))))
                '((set! a4 50) (set! a3 50) (set! a1 50) (set! a2 50) (set! a5 50) (set! a0 50) (halt a0))
                "flatten-tail: succes-4: tail double nested")
 )