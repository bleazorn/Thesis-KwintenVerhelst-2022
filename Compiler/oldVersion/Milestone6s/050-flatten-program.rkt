#lang racket

(require "langs/block-asm-lang.rkt"
         "langs/para-asm-lang.rkt")
(provide flatten-program)

(module+ test
  (require rackunit))

;
;(flatten-tail t)->list? '((set! ...) ...)
;t: tail?
(define (flatten-tail t)
  (match t
    [`(begin ,e ... ,tail) `(,@e ,@(flatten-tail tail))]
    [`(jump ,l) `((jump ,l))]
    [`(invoke ,a ,b) `((invoke ,a ,b))]
    [`(if (,relop ,a ,b) (jump ,l1) (jump ,l2)) `((jump-if ,l1 (,relop ,a ,b)) (jump ,l2))]
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
(define/contract (flatten-program p) (-> block-asm-lang? para-asm-lang?)
  (match p
    [`(module ,i ,b ...) `(begin ,i ,@(foldl (lambda (beg l) (append l (flatten-b beg))) '() b))]
    [_ #f]))


(module+ test
  ;#|
;flatten-tail:
  ;succes
  (check-equal? (flatten-tail '(begin (set! a0 50) (set! a0 a0) (jump cra)))
                '((set! a0 50) (set! a0 a0) (jump cra))
                "flatten-tail: succes-1: no nested")
  (check-equal? (flatten-tail '(begin (set! a1 50) (begin (set! a0 50) (set! a0 a0) (jump cra))))
                '((set! a1 50) (set! a0 50) (set! a0 a0) (jump cra))
                "flatten-tail: succes-2: tail nested")
  (check-equal? (flatten-tail '(begin (set! a1 50) (set! a2 50) (begin (set! a0 50) (set! a0 a0) (jump cra))))
                '((set! a1 50) (set! a2 50) (set! a0 50) (set! a0 a0) (jump cra))
                "flatten-tail: succes-3: effect and tail nested")
  (check-equal? (flatten-tail '(begin (set! a1 50) (set! a2 50) (begin (begin (set! a0 50) (set! a0 a0) (jump cra)))))
                '((set! a1 50) (set! a2 50) (set! a0 50) (set! a0 a0) (jump cra))
                "flatten-tail: succes-4: tail double nested")
  (check-equal? (flatten-tail '(begin (set! a4 50) (set! a3 50) (begin (set! a1 50) (set! a2 50) (set! a5 50) (begin (begin (set! a0 50) (set! a0 a0) (jump cra))))))
                '((set! a4 50) (set! a3 50) (set! a1 50) (set! a2 50) (set! a5 50) (set! a0 50) (set! a0 a0) (jump cra))
                "flatten-tail: succes-4: tail double nested")
  (check-equal? (flatten-tail '(if (= a0 a1) (jump L.foo.1) (jump L.foo.1)))
                `((jump-if L.foo.1 (= a0 a1)) (jump L.foo.1))
                "flatten-tail: succes-5: if")
  ;flatten-program
  ;succes
  (check-equal? (flatten-program '(module () (define L.foo.0 (begin (set! a1 50) (set! a2 50) (begin (begin (set! a0 50) (jump L.foo.1)))))
                                    (define L.foo.1 (begin (set! a1 50) (set! a2 50) (begin (begin (set! a0 50) (if (= a0 a1) (jump L.foo.0) (jump L.foo.1))))))
                                    (define L.foo.1 (begin (set! a1 50) (set! a2 50) (begin (begin (set! a0 50) (set! a0 a0) (jump cra)))))))
                '(begin () (with-label L.foo.0 (set! a1 50)) (set! a2 50) (set! a0 50) (jump L.foo.1)
                        (with-label L.foo.1 (set! a1 50)) (set! a2 50) (set! a0 50) (jump-if L.foo.0 (= a0 a1)) (jump L.foo.1)
                        (with-label L.foo.1 (set! a1 50)) (set! a2 50) (set! a0 50) (set! a0 a0) (jump cra))
                "flatten-program: succes-01: mul def")
  ;|#
  )




