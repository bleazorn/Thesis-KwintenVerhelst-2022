#lang racket

(provide flatten-begins)

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
    [_ #f]))

;Flatten all nested begin expressions.
;(flatten-begins p) → Paren-Cheri-Risc-V-V2-lang?
;p: Asm-lang-V2-nested?
(define (flatten-begins p)
  (match p
    [`(begin ,e ... ,tail) `(begin ,@(flatten-begin e) ,@(flatten-tail tail))]
    [_ #f]))


(module+ test
;flatten-begins
  ;succes
  (check-equal? (flatten-begins '(begin (set! a0 50) (halt a0)))
                '(begin (set! a0 50) (halt a0))
                "flatten-begins: succes-1: no nested")
  (check-equal? (flatten-begins '(begin (set! a1 50) (begin (set! a0 50) (halt a0))))
                '(begin (set! a1 50) (set! a0 50) (halt a0))
                "flatten-begins: succes-2: tail nested")
  (check-equal? (flatten-begins '(begin (begin (set! a1 50) (set! a2 50)) (begin (set! a0 50) (halt a0))))
                '(begin (set! a1 50) (set! a2 50) (set! a0 50) (halt a0))
                "flatten-begins: succes-3: effect and tail nested")
  (check-equal? (flatten-begins '(begin (begin (set! a1 50) (set! a2 50)) (begin (begin (set! a0 50) (halt a0)))))
                '(begin (set! a1 50) (set! a2 50) (set! a0 50) (halt a0))
                "flatten-begins: succes-4: tail double nested")
  ;failure
  (check-false (flatten-begins '(begin (halt a0) (set! a0 50)))
                "flatten-begins: succes-1: no nested"))