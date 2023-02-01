#lang racket

(require "common/aloc.rkt"
         "common/info.rkt")
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
;(uncover-begins effects locals)->list? '(aloc? ...)
;effects: list? '(effect? ...)
;locals: list? '(aloc? ...)
(define (uncover-begins effects locals)
  (for/fold ([newlocals locals])
            ([e effects])
    (values (uncover-effects e newlocals))))

;
;(uncover-pred p locals)->list? '(aloc? ...)
;p: pred?
;locals: list? '(aloc? ...)
(define (uncover-pred p locals)
  (match p
    [`(begin ,e ... ,pred) (uncover-pred pred (uncover-begins e locals))]
    [`(if ,p1 ,p2 ,p3) (let* ([p1Loc (uncover-pred p1 locals)]
                              [p2Loc (uncover-pred p2 p1Loc)])
                               (uncover-pred p3 p2Loc))]
    [`(,relop ,a ,b) (uncover-triv b (uncover-triv a locals))]
    ['(true) locals]
    ['(false) locals]
    [`(not ,pred) (uncover-pred pred locals)]
    [_ #f]))


;
;(uncover-effects locals program)->list? '(aloc? ...)
;e: effect?
;locals: list? '(aloc? ...)
(define (uncover-effects e locals)
  (match e
    [`(begin ,e ...) (uncover-begins e locals)]
    [`(set! ,a (,binop ,b ,c)) (uncover-triv c (uncover-triv b (uncover-triv a locals)))]
    [`(set! ,a ,b) (uncover-triv b (uncover-triv a locals))]
    [`(setLinear! ,a ,b) (uncover-triv b (uncover-triv a locals))]
    [`(if ,p ,e1 ,e2) (let* ([pLoc (uncover-pred p locals)]
                             [eLoc (uncover-effects e1 pLoc)])
                        (uncover-effects e2 eLoc))]
    [`(return-point ,l ,t) (uncover-tail t locals)]
    ['(split) locals]
    [_ #f]))


;
;(uncover-tail t locals)->list? '(aloc? ...)
;t: tail?
;locals: list? '(aloc? ...)
(define (uncover-tail t locals)
  (match t
    [`(begin ,e ... ,tail) (let ([newlocals (uncover-begins e locals)]) (uncover-tail tail newlocals))]
    [`(if ,p ,t1 ,t2) (let* ([pLoc (uncover-pred p locals)]
                             [tLoc (uncover-tail t1 pLoc)])
                        (uncover-tail t2 tLoc))]
    [`(jump-call ,trg ,l ...) (foldl (lambda (e l) (uncover-triv e l)) locals (cons trg l))]
    [`(jump-return ,trg ,l ...) (foldl (lambda (e l) (uncover-triv e l)) locals (cons trg l))]
    [_ #f]))

;
;(uncover-info info tail)->info?
;info: info?
;tail: tail?
(define (uncover-info info tail)
  (addInfo info (setLocals (uncover-tail tail '()))))

;
;(uncover-func f)->'(define label? info? tail?)  info?: '(locals)
;f: '(define label? info? tail?)
(define (uncover-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(uncover-info i t) ,t)]
    [_ #f]))


;Compiles Asm-lang-V2 to Asm-lang-V2-locals, analysing which abstract locations are used in the program and decorating the program with the set of variables in an info field.
;(uncover-locals p) → Asm-lang-V2-locals?
;p: Asm-lang-V2?
(define (uncover-locals p)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(uncover-info i t) ,@(map uncover-func f) ,t)]  
    [_ #f]))
                
(module+ test
  ;#|
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
;uncover-pred
  ;succes
  (check-equal? (uncover-pred '(= x.1 x.1) '()) '(x.1) "uncover-pred: succes-01: relop same empty loc")
  (check-equal? (uncover-pred '(= x.1 x.1) '(x.1)) '(x.1) "uncover-pred: succes-02: relop same not empty loc")
  (check-equal? (uncover-pred '(= x.1 x.1) '(y.2)) '(y.2 x.1) "uncover-pred: succes-03: relop same not empty loc")
  (check-equal? (uncover-pred '(= x.1 y.2) '(z.3)) '(z.3 x.1 y.2) "uncover-pred: succes-04: relop not empty loc")

  (check-equal? (uncover-pred '(true) '(x.1 y.2 z.3)) '(x.1 y.2 z.3) "uncover-pred: succes-05: relop not empty loc")
  (check-equal? (uncover-pred '(false) '(x.1 y.2 z.3)) '(x.1 y.2 z.3) "uncover-pred: succes-06: relop not empty loc")
  (check-equal? (uncover-pred '(not (= x.1 y.2)) '(z.3)) '(z.3 x.1 y.2) "uncover-pred: succes-07: not")

  (check-equal? (uncover-pred '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (set! w.4 x.1) (= p.5 w.4)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4 p.5) "uncover-pred: succes-08: one begin")
  (check-equal? (uncover-pred '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! w.4 x.1) (= p.5 w.4)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4 p.5) "uncover-pred: succes-09: nested begins")
  
  (check-equal? (uncover-pred '(if (= x.1 y.2) (= z.3 y.2) (= x.1 a.4)) '(z.3)) '(z.3 x.1 y.2 a.4) "uncover-pred: succes-10: if")
;uncover-effects
  ;succes
  (check-equal? (uncover-effects '(set! x.1 4) '()) '(x.1)              "uncover-begins: succes-01: integer set empty locals")
  (check-equal? (uncover-effects '(set! x.1 4) '(y.0)) '(y.0 x.1)       "uncover-begins: succes-02: integer set and not empty locals")
  (check-equal? (uncover-effects '(set! x.1 z.2) '(y.0)) '(y.0 x.1 z.2) "uncover-begins: succes-03: set different alocs")
  (check-equal? (uncover-effects '(set! x.1 x.1) '(y.0)) '(y.0 x.1)     "uncover-begins: succes-04: set same alocs")

  (check-equal? (uncover-effects '(set! x.1 (* z.2 w.3)) '(y.0)) '(y.0 x.1 z.2 w.3) "uncover-begins: succes-05: binop different alocs")
  (check-equal? (uncover-effects '(set! x.1 (+ x.1 5)) '(y.0)) '(y.0 x.1)           "uncover-begins: succes-06: binop same aloc and integer")

  (check-equal? (uncover-effects '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (set! w.4 x.1)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4)         "uncover-begins: succes-07: one begin")
  (check-equal? (uncover-effects '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! w.4 x.1)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4) "uncover-begins: succes-08: nested begins")

  (check-equal? (uncover-effects '(if (= x.1 y.2) (set! x.1 z.2) (begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! w.4 x.1))) '(z.3)) '(z.3 x.1 y.2 z.2 w.4) "uncover-pred: succes-09: if")
  (check-equal? (uncover-effects '(return-point L.foo.1 (if (= x.1 y.2) (begin (set! a0 a.4) (jump cra)) (begin (set! y.2 5) (set! z.3 5) (set! w.5 (* y.2 z.3)) (begin (set! a0 x.1) (jump cra)))))
                                 '(z.3))
                '(z.3 x.1 y.2 a.4 w.5)
                "uncover-begins: succes-10: return")
  ;failure
  (check-false (uncover-effects '(set! x.1 7 4) '()) "uncover-begins: failure-1: wrong input")
;uncover-tail
  ;succes
  (check-equal? (uncover-tail '(begin (set! a0 x.1) (jump cra)) '()) '(x.1) "uncover-tail: succes-2: halt aloc and empty locals")
  (check-equal? (uncover-tail '(begin (set! a0 x.1) (jump cra)) '(y.0 z.2)) '(y.0 z.2 x.1) "uncover-tail: succes-4: halt aloc and aloc not in locals")
  (check-equal? (uncover-tail '(begin (set! a0 x.1) (jump cra)) '(y.0 x.1)) '(y.0 x.1) "uncover-tail: succes-5: halt aloc and aloc in locals")

  (check-equal? (uncover-tail '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (begin (set! a0 x.1) (jump cra))) '(y.0)) '(y.0 y.2 z.3 x.1) "uncover-tail: succes-7: one begin")
  (check-equal? (uncover-tail '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (begin (set! a0 x.1) (jump cra))) '(y.0)) '(y.0 y.2 z.3 x.1) "uncover-tail: succes-8: nested begins")

  (check-equal? (uncover-tail '(if (= x.1 y.2) (begin (set! a0 a.4) (jump cra)) (begin (set! y.2 5) (set! z.3 5) (set! w.5 (* y.2 z.3)) (begin (set! a0 x.1) (jump cra))))
                              '(z.3))
                '(z.3 x.1 y.2 a.4 w.5)
                "uncover-pred: succes-10: if")
  ;failure
  (check-false (uncover-tail '(set! x.1 7 4) '()) "uncover-begins: failure-1: wrong input")
  (check-false (uncover-tail '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! x.1 5)) '(y.0)) "uncover-tail: failure-2: no halt")
;uncover-locals
  ;succes
  (check-equal? (uncover-locals
                 '(module ()
                    (begin
                      (set! x.1 0)
                      (begin (set! a0 x.1) (jump cra)))))
                '(module ((locals (x.1))) (begin (set! x.1 0) (begin (set! a0 x.1) (jump cra))))
                "uncover-locals: succes-1: one local")
  (check-equal? (uncover-locals
                 '(module ()
                    (begin
                      (set! x.1 0)
                      (set! y.1 x.1)
                      (set! y.1 (+ y.1 x.1))
                      (begin (set! a0 y.1) (jump cra)))))
                '(module
                   ((locals (x.1 y.1)))
                   (begin (set! x.1 0) (set! y.1 x.1) (set! y.1 (+ y.1 x.1)) (begin (set! a0 y.1) (jump cra))))
                "uncover-locals: succes-2: two locals")
;|#
  )
