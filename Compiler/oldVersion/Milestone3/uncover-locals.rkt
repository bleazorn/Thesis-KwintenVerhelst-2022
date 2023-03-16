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
;uncover-begins
  ;succes
  (check-equal? (uncover-begins '() '()) '() "uncover-begins: succes-1: empty list and empty locals")
  (check-equal? (uncover-begins '() '(x.1)) '(x.1) "uncover-begins: succes-2: empty list and not empty locals")
  (check-equal? (uncover-begins '((set! x.1 5)) '()) '(x.1) "uncover-begins: succes-3: one element list and empty locals")
  (check-equal? (uncover-begins '((set! x.1 5)) '(y.1)) '(y.1 x.1) "uncover-begins: succes-4: one element list and not empty locals")
  (check-equal? (uncover-begins '((set! x.1 5)) '(x.1)) '(x.1) "uncover-begins: succes-5: one element list and not empty locals and already excist")
  (check-equal? (uncover-begins '((set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (set! w.4 x.1)) '()) '(y.2 z.3 x.1 w.4) "uncover-begins: succes-6: multiple elements list and not empty locals")
  (check-equal? (uncover-begins '((set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! w.4 x.1)) '()) '(y.2 z.3 x.1 w.4) "uncover-begins: succes-7: nested-begin")
  ;failure
  (check-false (uncover-begins '(set! y.2 5) '()) "uncover-begins: failure-1: wrong list")
  ;(check-false (uncover-begins '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (set! w.4 x.1)) '()) "uncover-begins: failure-4: wrong list")
;uncover-effects
  ;succes
  (check-equal? (uncover-effects '(set! x.1 4) '()) '(x.1) "uncover-begins: succes-1: integer set empty locals")
  (check-equal? (uncover-effects '(set! x.1 4) '(y.0)) '(y.0 x.1) "uncover-begins: succes-2: integer set and not empty locals")
  (check-equal? (uncover-effects '(set! x.1 z.2) '(y.0)) '(y.0 x.1 z.2) "uncover-begins: succes-3: set different alocs")
  (check-equal? (uncover-effects '(set! x.1 x.1) '(y.0)) '(y.0 x.1) "uncover-begins: succes-4: set same alocs")

  (check-equal? (uncover-effects '(set! x.1 (* z.2 w.3)) '(y.0)) '(y.0 x.1 z.2 w.3) "uncover-begins: succes-5: binop different alocs")
  (check-equal? (uncover-effects '(set! x.1 (+ x.1 5)) '(y.0)) '(y.0 x.1) "uncover-begins: succes-6: binop same aloc and integer")

  (check-equal? (uncover-effects '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (set! w.4 x.1)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4) "uncover-begins: succes-7: one begin")
  (check-equal? (uncover-effects '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! w.4 x.1)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4) "uncover-begins: succes-8: nested begins")
  ;failure
  (check-false (uncover-effects '(set! x.1 7 4) '()) "uncover-begins: failure-1: wrong input")
;uncover-tail
  ;succes
  (check-equal? (uncover-tail '(halt 4) '()) '() "uncover-tail: succes-1: integer halt empty locals")
  (check-equal? (uncover-tail '(halt x.1) '()) '(x.1) "uncover-tail: succes-2: halt aloc and empty locals")
  (check-equal? (uncover-tail '(halt 4) '(y.0 z.2)) '(y.0 z.2) "uncover-tail: succes-3: integer halt not empty locals")
  (check-equal? (uncover-tail '(halt x.1) '(y.0 z.2)) '(y.0 z.2 x.1) "uncover-tail: succes-4: halt aloc and aloc not in locals")
  (check-equal? (uncover-tail '(halt x.1) '(y.0 x.1)) '(y.0 x.1) "uncover-tail: succes-5: halt aloc and aloc in locals")

  (check-equal? (uncover-tail '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (halt x.1)) '(y.0)) '(y.0 y.2 z.3 x.1) "uncover-tail: succes-7: one begin")
  (check-equal? (uncover-tail '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (halt x.1)) '(y.0)) '(y.0 y.2 z.3 x.1) "uncover-tail: succes-8: nested begins")
  ;failure
  (check-false (uncover-effects '(set! x.1 7 4) '()) "uncover-begins: failure-1: wrong input")
  (check-false (uncover-tail '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! x.1 5)) '(y.0)) "uncover-tail: failure-2: no halt")
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
