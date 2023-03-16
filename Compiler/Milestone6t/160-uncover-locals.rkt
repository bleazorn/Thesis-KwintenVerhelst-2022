#lang racket

(require "common/aloc.rkt"
         "common/info.rkt"
         "langs/asm-pred-lang.rkt")
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
  (for/fold ([new-locals locals])
            ([e effects])
    (values (uncover-effects e new-locals))))

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
    [_ (error (format "uncover-locals;  Failed match.\n No valid pred: ~a" p))]))


;
;(uncover-effects locals program)->list? '(aloc? ...)
;e: effect?
;locals: list? '(aloc? ...)
(define (uncover-effects e locals)
  (match e
    [`(begin ,e ...) (uncover-begins e locals)]
    [`(set! ,a (,binop ,b ,c)) (uncover-triv c (uncover-triv b (uncover-triv a locals)))]
    [`(set! ,a ,b) (uncover-triv b (uncover-triv a locals))]
    [`(if ,p ,e1 ,e2) (let* ([pLoc (uncover-pred p locals)]
                             [eLoc (uncover-effects e1 pLoc)])
                        (uncover-effects e2 eLoc))]
    [`(return-point ,l ,t) (uncover-tail t locals)]
    [_ (error (format "uncover-locals;  Failed match.\n No valid effect: ~a" e))]))


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
    [_ (error (format "uncover-locals;  Failed match.\n No valid tail: ~a" t))]))

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
    [_ (error (format "uncover-locals;  Failed match.\n No valid function: ~a" f))]))


;Compiles Asm-lang-V2 to Asm-lang-V2-locals, analysing which abstract locations are used in the program and decorating the program with the set of variables in an info field.
;(uncover-locals p) → Asm-lang-V2-locals?
;p: Asm-lang-V2?
(define/contract (uncover-locals p) (-> asm-pred-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(uncover-info i t) ,@(map uncover-func f) ,t)]))
                
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
  (check-exn exn:fail? (thunk (uncover-begins 'symp1 '(y.0))) "uncover-begins: failure-01: no begin")
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
  ;failure
  (check-exn exn:fail? (thunk (uncover-pred 'symp1 '(y.0))) "uncover-pred: failure-01: no pred")
  ;uncover-effects
  ;succes
  (check-equal? (uncover-effects '(set! x.1 4) '()) '(x.1)              "uncover-effects: succes-01: integer set empty locals")
  (check-equal? (uncover-effects '(set! x.1 4) '(y.0)) '(y.0 x.1)       "uncover-effects: succes-02: integer set and not empty locals")
  (check-equal? (uncover-effects '(set! x.1 z.2) '(y.0)) '(y.0 x.1 z.2) "uncover-effects: succes-03: set different alocs")
  (check-equal? (uncover-effects '(set! x.1 x.1) '(y.0)) '(y.0 x.1)     "uncover-effects: succes-04: set same alocs")

  (check-equal? (uncover-effects '(set! x.1 (* z.2 w.3)) '(y.0)) '(y.0 x.1 z.2 w.3) "uncover-effects: succes-05: binop different alocs")
  (check-equal? (uncover-effects '(set! x.1 (+ x.1 5)) '(y.0)) '(y.0 x.1)           "uncover-effects: succes-06: binop same aloc and integer")

  (check-equal? (uncover-effects '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (set! w.4 x.1)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4)         "uncover-effects: succes-07: one begin")
  (check-equal? (uncover-effects '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! w.4 x.1)) '(y.0)) '(y.0 y.2 z.3 x.1 w.4) "uncover-effects: succes-08: nested begins")

  (check-equal? (uncover-effects '(if (= x.1 y.2) (set! x.1 z.2) (begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (set! w.4 x.1))) '(z.3)) '(z.3 x.1 y.2 z.2 w.4) "uncover-effects: succes-09: if")
  (check-equal? (uncover-effects '(return-point L.foo.1 (if (= x.1 y.2) (begin (set! a0 a.4) (jump-return cra)) (begin (set! y.2 5) (set! z.3 5) (set! w.5 (* y.2 z.3)) (begin (set! a0 x.1) (jump-return cra)))))
                                 '(z.3))
                '(z.3 x.1 y.2 a.4 w.5)
                "uncover-effects: succes-10: return")
  ;failure
  (check-exn exn:fail? (thunk (uncover-effects 'symp1 '(y.0))) "uncover-effects: failure-01: no effect")
  ;uncover-tail
  ;succes
  (check-equal? (uncover-tail '(begin (set! a0 x.1) (jump-return cra)) '()) '(x.1) "uncover-tail: succes-2: halt aloc and empty locals")
  (check-equal? (uncover-tail '(begin (set! a0 x.1) (jump-call cra)) '(y.0 z.2)) '(y.0 z.2 x.1) "uncover-tail: succes-4: halt aloc and aloc not in locals")
  (check-equal? (uncover-tail '(begin (set! a0 x.1) (jump-return cra)) '(y.0 x.1)) '(y.0 x.1) "uncover-tail: succes-5: halt aloc and aloc in locals")

  (check-equal? (uncover-tail '(begin (set! y.2 5) (set! z.3 5) (set! x.1 (* y.2 z.3)) (begin (set! a0 x.1) (jump-return cra))) '(y.0)) '(y.0 y.2 z.3 x.1) "uncover-tail: succes-7: one begin")
  (check-equal? (uncover-tail '(begin (set! y.2 5) (begin (set! z.3 5) (set! x.1 (* y.2 z.3))) (begin (set! a0 x.1) (jump-return cra))) '(y.0)) '(y.0 y.2 z.3 x.1) "uncover-tail: succes-8: nested begins")

  (check-equal? (uncover-tail '(if (= x.1 y.2) (begin (set! a0 a.4) (jump-return cra)) (begin (set! y.2 5) (set! z.3 5) (set! w.5 (* y.2 z.3)) (begin (set! a0 x.1) (jump-return cra))))
                              '(z.3))
                '(z.3 x.1 y.2 a.4 w.5)
                "uncover-tail: succes-10: if")
  ;failure
  (check-exn exn:fail? (thunk (uncover-tail 'symp1 '(y.0))) "uncover-tail: failure-01: no tail")
  ;uncover-func
  ;succes
  (check-equal? (uncover-func '(define L.odd.1
                                 ((new-frames ()) (paramSize 0))
                                 (begin
                                   (set! tmp-ra.1 cra)
                                   (begin
                                     (set! x.1 a1)
                                     (set! y.2 a2)
                                     (set! z.3 a3)
                                     (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                     (jump-return cra cfp)))))
                '(define L.odd.1
                   ((locals (tmp-ra.1 x.1 y.2 z.3 tmp.1)) (new-frames ()) (paramSize 0))
                   (begin
                     (set! tmp-ra.1 cra)
                     (begin
                       (set! x.1 a1)
                       (set! y.2 a2)
                       (set! z.3 a3)
                       (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                       (jump-return cra cfp))))
                "uncover-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (uncover-func '(defdine L.odd.1
                                               ((new-frames ()) (paramSize 0))
                                               (begin
                                                 (set! tmp-ra.1 cra)
                                                 (begin
                                                   (set! x.1 a1)
                                                   (set! y.2 a2)
                                                   (set! z.3 a3)
                                                   (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                                   (jump-return cra cfp))))))
             "uncover-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (uncover-func '(define 
                                               ((new-frames ()) (paramSize 0))
                                               (begin
                                                 (set! tmp-ra.1 cra)
                                                 (begin
                                                   (set! x.1 a1)
                                                   (set! y.2 a2)
                                                   (set! z.3 a3)
                                                   (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                                   (jump-return cra cfp))))))
             "uncover-func: failure-02: no name")
  (check-exn exn:fail? (thunk (uncover-func '(define L.odd.1
                                               (begin
                                                 (set! tmp-ra.1 cra)
                                                 (begin
                                                   (set! x.1 a1)
                                                   (set! y.2 a2)
                                                   (set! z.3 a3)
                                                   (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                                   (jump-return cra cfp))))))
             "uncover-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (uncover-func '(define L.odd.1
                                               ((new-frames ()) (paramSize 0)))))
             "uncover-func: failure-04:  no tail")
  #|
;uncover-locals
  ;succes
  (check-equal? (uncover-locals
                 '(module ()
                    (begin
                      (set! x.1 0)
                      (begin (set! a0 x.1) (jump-return cra)))))
                '(module ((locals (x.1))) (begin (set! x.1 0) (begin (set! a0 x.1) (jump-return cra))))
                "uncover-locals: succes-1: one local")
  (check-equal? (uncover-locals
                 '(module ()
                    (begin
                      (set! x.1 0)
                      (set! y.1 x.1)
                      (set! y.1 (+ y.1 x.1))
                      (begin (set! a0 y.1) (jump-return cra)))))
                '(module
                   ((locals (x.1 y.1)))
                   (begin (set! x.1 0) (set! y.1 x.1) (set! y.1 (+ y.1 x.1)) (begin (set! a0 y.1) (jump-return cra))))
                "uncover-locals: succes-2: two locals")
;|#
  )
