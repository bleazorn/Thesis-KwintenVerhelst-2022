#lang racket

(require "common.rkt")
(provide uncover-locals)

(module+ test
  (require rackunit))


;
;(define (uncover-triv locals triv)->list? '(aloc? ...)
;t: triv?
;locals: list? '(aloc? ...)
(define (uncover-triv t locals)
  (if (or (not (aloc? t)) (member t locals))
      locals
      (append locals `(,t))))

;
;(uncover-begins effects locals)
;effects: list? '(effect? ...)
;locals: list? '(aloc? ...)
(define (uncover-begins effects locals)
  (for/fold ([newlocals locals])
            ([e effects])
    (values (uncover-effects e newlocals))))


;
;(uncover-effects locals program)->list? '(aloc? ...)
;e: effect?
;locals: list? '(aloc? ...)
(define (uncover-effects e locals)
  (match e
    [`(begin ,e ...) (uncover-begins e locals)]
    [`(set! ,a (,binop ,b ,c)) (uncover-triv c (uncover-triv b (uncover-triv a locals)))]
    [`(set! ,a ,b) (uncover-triv b (uncover-triv a locals))]
    [_ #f]))


;
;(uncover-tail t locals)->list? '(aloc? ...)
;t: tail?
;locals: list? '(aloc? ...)
(define (uncover-tail t locals)
  (match t
    [`(begin ,e ... ,tail) (let ([newlocals (uncover-begins e locals)]) (uncover-tail tail newlocals))]
    [`(halt ,triv) (uncover-triv triv locals)]
    [_ #f]))


;Compiles Asm-lang-V2 to Asm-lang-V2-locals, analysing which abstract locations are used in the program and decorating the program with the set of variables in an info field.
;(uncover-locals p) → Asm-lang-V2-locals?
;p: Asm-lang-V2?
(define (uncover-locals p)
  (match p
    [`(module ,loc ,pro) `(module ((locals ,(uncover-tail pro loc))) ,pro)]      ;TODO: check voor efficientie remove duplicates
    [_ #f]))

(module+ test
;uncover-triv
  ;succes
  (check-equal? (uncover-triv 'x.1 '()) '(x.1) "uncover-triv: succes-1: empty locals")
  (check-equal? (uncover-triv 'x.1 '(x.1)) '(x.1) "uncover-triv: succes-2: has alloc in locals")
  (check-equal? (uncover-triv 'y.1 '(x.1)) '(x.1 y.1) "uncover-triv: succes-3: has alloc not in locals")
  (check-equal? (uncover-triv 5 '(x.1)) '(x.1) "uncover-triv: succes-4: element is not alloc")
;uncover-locals
  ;succes
  (check-equal? (uncover-locals
                 '(module ()
                    (begin
                      (set! x.1 0)
                      (halt x.1))))
                '(module ((locals (x.1))) (begin (set! x.1 0) (halt x.1)))
                "uncover-locals: succes-1: one local")
  (check-equal? (uncover-locals
                 '(module ()
                    (begin
                      (set! x.1 0)
                      (set! y.1 x.1)
                      (set! y.1 (+ y.1 x.1))
                      (halt y.1))))
                '(module
                   ((locals (x.1 y.1)))
                   (begin (set! x.1 0) (set! y.1 x.1) (set! y.1 (+ y.1 x.1)) (halt y.1)))
                "uncover-locals: succes-2: two locals"))
