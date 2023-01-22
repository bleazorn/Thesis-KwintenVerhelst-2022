#lang racket

(require "common/info.rkt"
         "common/aloc.rkt"
         "common/fvar.rkt"
         "common/register.rkt")
(provide conflict-analysis)

(module+ test
  (require rackunit))

#|
Approximation of Conflict
Any variable defined during a non-move instruction is in conflict with every variable (except itself) in the undead-out set associated with the instruction.

Any variable defined during a move instruction is in conflict with every variable in the undead-out set associated with the instruction, except itself and the variable referenced in the move.
|#

(define (canConflict? a)
  (or (aloc? a) (register? a) (fvar? a)))

;
;(conflict-aloc-not-each-other a notC posC undead-out conf)->conflicts?
;a: aloc?
;notC: list? '(aloc ...)
;posC: list? '(aloc ...)
;undead-out: undead-set?
(define (conflict-aloc-add-conflicted a conflicted conf)
  ;(println (format "second ~a : ~a - ~a" a conflicted conf))
  (if (canConflict? a)
      (let ([c (assoc a conf)])
        (list-set conf (index-of conf c) `(,a ,(remove-duplicates (append conflicted (second c))))))
      conf))

;
;(conflict-aloc a notC posC undead-out conf)->conflicts?
;a: aloc?
;notC: list? '(aloc ...)
;posC: list? '(aloc ...)
;undead-out: undead-set?
(define (conflict-aloc a notC posC undead-out conf)
  ;(println (format "conflict: ~a : ~a ~a - ~a - ~a" a notC posC undead-out conf))
  (if (canConflict? a)
      ;initialize every possible conflict in the conflicts list
      (let ([addConf (foldl (lambda (al co) (cond [(assoc al co) co]
                                                  [else (cons `(,al ()) co)]))
                              conf (cons a (append posC undead-out)))])
        ;get all conflicted variables
        (let ([conflicted (remove* (cons a notC) (filter canConflict? (append posC undead-out)))])
          ;add conflicted to the aloc to conf
          (let ([newConf (conflict-aloc-add-conflicted a conflicted addConf)])
            ;add for every conflicted aloc to the conf
            (foldl (lambda (p c) (conflict-aloc-add-conflicted p `(,a) c)) newConf conflicted))))
      conf))

;
;(conflict-begin b undead-outs conf)->conflicts?   '((aloc? (...)) ...)
;b:list? '(effect? ...)
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-begin b undead-outs conf)
  (for/fold ([prevConf conf]
             [nextUndead undead-outs] #:result prevConf)
            ([eff b])
    (values (conflict-effect eff (car nextUndead) prevConf) (cdr nextUndead))))

;
;(conflict-pred p undead-outs conf)->conflicts?   '((aloc? (...)) ...)
;p: pred?
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-pred p undead-outs conf)
  ;(println (format "conflict-pred: ~a -:- ~a" p undead-outs))
  (match p
    [`(begin ,e ... ,pred) (conflict-pred pred (last undead-outs) (conflict-begin e (drop-right undead-outs 1) conf))]
    [`(,relop ,a ,b) (conflict-aloc a '() '() undead-outs (conflict-aloc b '() '() undead-outs conf))]
    [`(true) conf]
    [`(false) conf]
    [`(not ,pred) (conflict-pred pred undead-outs conf)]
    [`(if ,p1 ,p2 ,p3) (conflict-pred p3 (third undead-outs)
                                         (conflict-pred p2 (second undead-outs)
                                                        (conflict-pred p1 (first undead-outs) conf)))]))

;
;(conflict-effect e undead-outs conf)->conflicts?   '((aloc? (...)) ...)
;e: effect?
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-effect e undead-outs conf)
  ;(println (format "conflict-effect: ~a -:- ~a" e undead-outs))
  (match e
    [`(begin ,eff ...) (conflict-begin eff undead-outs conf)]
    [`(set! ,a (,binop ,b ,c)) (conflict-aloc a `(,b ,c) '() undead-outs (conflict-aloc b `(,a) '() undead-outs (conflict-aloc c `(,a) '() undead-outs conf)))]
    [`(set! ,a ,b) (conflict-aloc a `(,b) '() undead-outs (conflict-aloc b `(,a) '() undead-outs conf))]
    [`(if ,p ,e1 ,e2) (conflict-effect e2 (third undead-outs)
                                         (conflict-effect e1 (second undead-outs)
                                                        (conflict-pred p (first undead-outs) conf)))]
    [`(return-point ,l ,t) (conflict-tail t (second undead-outs) conf)]
    [_ #f]))

;
;(conflict-tail t conf)->conflicts?   '((aloc? (...)) ...)
;t: tail?
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-tail t undead-outs conf)
  ;(println (format "conflict-tail: ~a -:- ~a" t undead-outs))
  (match t
    [`(begin ,e ... ,tail) (conflict-tail tail (last undead-outs) (conflict-begin e (drop-right undead-outs 1) conf))]
    [`(if ,p ,t1 ,t2) (conflict-tail t2 (third undead-outs)
                                     (conflict-tail t1 (second undead-outs)
                                                    (conflict-pred p (first undead-outs) conf)))]
    [`(jump ,trg ,l ...) (conflict-aloc trg '() '() undead-outs conf)]
    [_ #f]
    ))

;Creates initial conflicts
;(conflict-locals loc)->conflicts? '((aloc? (...)) ...)
;loc->locals?
(define (conflict-locals loc)
  (foldl (lambda (l c) (cons `(,l ()) c)) '() loc)  
  )

;
;(conflict-func f)->'(define label? info? tail?)   info?:'(locals conflicts)
;f: '(define label? info? tail?)
(define (conflict-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(addInfo i (setConflicts (conflict-tail t (getInfo i getUndead-out) (conflict-locals (getInfo i getLocals))))) ,t)]
    [_ #f]))

;Decorates a program with its conflict graph.
;(conflict-analysis p) -> Asm-lang-V2-conflicts?
;p: Asm-lang-V2-undead?
(define (conflict-analysis p)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(addInfo i (setConflicts (conflict-tail t (getInfo i getUndead-out) (conflict-locals (getInfo i getLocals))))) ,@(map conflict-func f) ,t)]
    [_ #f]))



(module+ test
  ;#|
;conflict-analysis
  ;succes
  (check-equal? (conflict-analysis '(module
                                        ((undead-out ((x.1) (() ()))) (call-undead ()) (locals (x.1)))
                                      (begin (set! x.1 42) (begin (set! a0 x.1) (jump L.foo.4)))))
                '(module
                     ((conflicts ((a0 ()) (x.1 ()))) (undead-out ((x.1) (() ()))) (call-undead ()) (locals (x.1)))
                   (begin (set! x.1 42) (begin (set! a0 x.1) (jump L.foo.4))))
                "conflict-analysis: succes-1: one instruction")
  (check-equal? (conflict-analysis '(module
                                        ((undead-out
                                          ((v.1)
                                           (v.1 w.2)
                                           (x.3 w.2)
                                           (x.3 p.1 w.2)
                                           (x.3 w.2)
                                           (y.4 x.3 w.2)
                                           (y.4 p.1 x.3 w.2)
                                           (x.3 w.2 y.4)
                                           (z.5 w.2 y.4)
                                           (y.4 z.5)
                                           (t.6 z.5)
                                           (t.6 p.1 z.5)
                                           (z.5 t.6)
                                           (z.5)
                                           (() ())))
                                         (call-undead ())
                                         (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
                                      (begin
                                        (set! v.1 1)
                                        (set! w.2 46)
                                        (set! x.3 v.1)
                                        (set! p.1 7)
                                        (set! x.3 (+ x.3 p.1))
                                        (set! y.4 x.3)
                                        (set! p.1 4)
                                        (set! y.4 (+ y.4 p.1))
                                        (set! z.5 x.3)
                                        (set! z.5 (+ z.5 w.2))
                                        (set! t.6 y.4)
                                        (set! p.1 -1)
                                        (set! t.6 (* t.6 p.1))
                                        (set! z.5 (+ z.5 t.6))
                                        (begin (set! a0 z.5) (jump L.foo.4)))))
                '(module
                     ((conflicts ((a0 ())
                                  (p.1 (z.5 t.6 x.3 w.2 y.4))
                                  (t.6 (z.5 p.1))
                                  (z.5 (t.6 p.1 y.4 w.2))
                                  (y.4 (z.5 w.2 x.3 p.1))
                                  (x.3 (w.2 y.4 p.1))
                                  (w.2 (y.4 z.5 x.3 p.1 v.1))
                                  (v.1 (w.2))))
                      (undead-out
                       ((v.1)
                        (v.1 w.2)
                        (x.3 w.2)
                        (x.3 p.1 w.2)
                        (x.3 w.2)
                        (y.4 x.3 w.2)
                        (y.4 p.1 x.3 w.2)
                        (x.3 w.2 y.4)
                        (z.5 w.2 y.4)
                        (y.4 z.5)
                        (t.6 z.5)
                        (t.6 p.1 z.5)
                        (z.5 t.6)
                        (z.5)
                        (() ())))
                      (call-undead ())
                      (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
                   (begin
                     (set! v.1 1)
                     (set! w.2 46)
                     (set! x.3 v.1)
                     (set! p.1 7)
                     (set! x.3 (+ x.3 p.1))
                     (set! y.4 x.3)
                     (set! p.1 4)
                     (set! y.4 (+ y.4 p.1))
                     (set! z.5 x.3)
                     (set! z.5 (+ z.5 w.2))
                     (set! t.6 y.4)
                     (set! p.1 -1)
                     (set! t.6 (* t.6 p.1))
                     (set! z.5 (+ z.5 t.6))
                     (begin (set! a0 z.5) (jump L.foo.4))))
                "conflict-analysis: succes-2: multiple instructions")
    (check-equal? (conflict-analysis '(module
                                          ((undead-out ((x.1) ((y.2 x.1) ((z.3) (x.1))) (() ()))) (call-undead ()) (locals (x.1)))
                                        (begin
                                          (set! x.1 42)
                                          (begin
                                            (set! y.2 x.1)
                                            (begin
                                              (set! z.3 (+ y.2 x.1))
                                              (set! x.1 z.3)))
                                          (begin (set! a0 x.1) (jump L.foo.4)))))
                '(module
                     ((conflicts ((a0 ())
                                  (z.3 ())
                                  (y.2 ())
                                  (x.1 ())))
                      (undead-out ((x.1) ((y.2 x.1) ((z.3) (x.1))) (() ()))) (call-undead ()) (locals (x.1)))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 x.1)
                       (begin
                         (set! z.3 (+ y.2 x.1))
                         (set! x.1 z.3)))
                     (begin (set! a0 x.1) (jump L.foo.4))))
                "conflict-analysis: succes-05: begin effect instruction")

  (check-equal? (conflict-analysis '(module
                                        ((undead-out ((x.1) ((y.2) (x.1)) ((z.3) (x.1)) (() ()))) (call-undead ()) (locals (x.1 y.2 z.3)))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 (+ x.1 50))
                                          (set! x.1 y.2))
                                        (begin
                                          (set! z.3 (+ x.1 x.1))
                                          (set! x.1 z.3))
                                        (begin (set! a0 x.1) (jump L.foo.4)))))
                '(module
                     ((conflicts ((a0 ())
                                  (z.3 ())
                                  (y.2 ())
                                  (x.1 ())))
                      (undead-out ((x.1) ((y.2) (x.1)) ((z.3) (x.1)) (() ()))) (call-undead ()) (locals (x.1 y.2 z.3)))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 (+ x.1 50))
                       (set! x.1 y.2))
                     (begin
                       (set! z.3 (+ x.1 x.1))
                       (set! x.1 z.3))
                     (begin (set! a0 x.1) (jump L.foo.4))))
                "conflict-analysis: succes-06: sequential begin effects")
  (check-equal? (conflict-analysis '(module
                                        ((undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1))) (() ()))) (call-undead ()) (locals (x.1 y.2 z.3 a.4 b.5)))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 (+ x.1 50))
                                          (begin
                                            (set! a.4 50)
                                            (set! b.5 60)
                                            (begin
                                              (set! a.4 (* a.4 b.5))
                                              (set! y.2 a.4))                                          
                                            (begin
                                              (set! a.4 y.2)
                                              (set! y.2 (+ b.5 b.5)))
                                            (set! x.1 y.2))
                                          (begin
                                            (set! z.3 (+ x.1 x.1))
                                            (set! x.1 z.3)))
                                        (begin (set! a0 x.1) (jump L.foo.4)))))           
                '(module
                     ((conflicts ((a0 ())
                                  (b.5 (a.4 y.2))
                                  (a.4 (b.5))
                                  (z.3 ())
                                  (y.2 (b.5))
                                  (x.1 ())))
                      (undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1))) (() ()))) (call-undead ()) (locals (x.1 y.2 z.3 a.4 b.5)))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 (+ x.1 50))
                       (begin
                         (set! a.4 50)
                         (set! b.5 60)
                         (begin
                           (set! a.4 (* a.4 b.5))
                           (set! y.2 a.4))                                          
                         (begin
                           (set! a.4 y.2)
                           (set! y.2 (+ b.5 b.5)))
                         (set! x.1 y.2))
                       (begin
                         (set! z.3 (+ x.1 x.1))
                         (set! x.1 z.3)))
                     (begin (set! a0 x.1) (jump L.foo.4))))
                "conflict-analysis: succes-07: sequential and nested begin effects")
  
    
  (check-equal? (conflict-analysis '(module
                                        ((undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1) (() ()))))) (call-undead ()) (locals (x.1 y.2 z.3 a.4 b.5)))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 (+ x.1 50))
                                          (begin
                                            (set! a.4 50)
                                            (set! b.5 60)
                                            (begin
                                              (set! a.4 (* a.4 b.5))
                                              (set! y.2 a.4))                                          
                                            (begin
                                              (set! a.4 y.2)
                                              (set! y.2 (+ b.5 b.5)))
                                            (set! x.1 y.2))
                                          (begin
                                            (set! z.3 (+ x.1 x.1))
                                            (set! x.1 z.3)
                                            (begin (set! a0 x.1) (jump L.foo.4)))))))           
                '(module
                     ((conflicts ((a0 ())
                                  (b.5 (a.4 y.2))
                                  (a.4 (b.5))
                                  (z.3 ())
                                  (y.2 (b.5))
                                  (x.1 ())))
                      (undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1) (() ()))))) (call-undead ()) (locals (x.1 y.2 z.3 a.4 b.5)))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 (+ x.1 50))
                       (begin
                         (set! a.4 50)
                         (set! b.5 60)
                         (begin
                           (set! a.4 (* a.4 b.5))
                           (set! y.2 a.4))                                          
                         (begin
                           (set! a.4 y.2)
                           (set! y.2 (+ b.5 b.5)))
                         (set! x.1 y.2))
                       (begin
                         (set! z.3 (+ x.1 x.1))
                         (set! x.1 z.3)
                         (begin (set! a0 x.1) (jump L.foo.4))))))
                "conflict-analysis: succes-08: sequential and nested begin effects with tail in nested")
  (check-equal? (conflict-analysis '(module
                                        ((undead-out ((x.1) (x.1 y.2) ((b.3 y.2) (b.3) (b.3 c.4) ((c.4) (() ()) ((c.4) (() ())))))) (call-undead ()) (locals (x.1 y.2 b.3 c.4)))
                                      (begin
                                        (set! x.1 5)
                                        (set! y.2 x.1)
                                        (begin
                                          (set! b.3 x.1)
                                          (set! b.3 (+ b.3 y.2))
                                          (set! c.4 b.3)
                                          (if (= c.4 b.3)
                                              (begin (set! a0 c.4) (jump L.foo.4))
                                              (begin
                                                (set! x.1 c.4)
                                                (begin (set! a0 c.4) (jump L.foo.4))))))))                 
                '(module
                     ((conflicts ((a0 ())
                                  (c.4 (b.3))
                                  (b.3 (c.4 y.2))
                                  (y.2 (b.3 x.1))
                                  (x.1 (y.2))))
                      (undead-out ((x.1) (x.1 y.2) ((b.3 y.2) (b.3) (b.3 c.4) ((c.4) (() ()) ((c.4) (() ())))))) (call-undead ()) (locals (x.1 y.2 b.3 c.4)))
                   (begin
                     (set! x.1 5)
                     (set! y.2 x.1)
                     (begin
                       (set! b.3 x.1)
                       (set! b.3 (+ b.3 y.2))
                       (set! c.4 b.3)
                       (if (= c.4 b.3)
                           (begin (set! a0 c.4) (jump L.foo.4))
                           (begin
                             (set! x.1 c.4)
                             (begin (set! a0 c.4) (jump L.foo.4)))))))
                "conflict-analysis: succes-09: if tail")
  (check-equal? (conflict-analysis '(module
                                        ((locals (tmp-ra.14))
                                         (undead-out
                                          ((tmp-ra.14 rbp)
                                           (tmp-ra.14 fv1 rbp)
                                           (tmp-ra.14 fv1 fv0 rbp)
                                           (fv1 fv0 r15 rbp)
                                           (fv1 fv0 r15 rbp))))
                                      (define L.swap.1
                     ((locals (nfv.16 nfv.17 z.3 tmp-ra.15 x.1 y.2))
                      (undead-out
                       ((fv0 fv1 tmp-ra.15 rbp)
                        (fv1 x.1 tmp-ra.15 rbp)
                        (y.2 x.1 tmp-ra.15 rbp)
                        ((y.2 x.1 tmp-ra.15 rbp)
                         ((tmp-ra.15 rax rbp) (rax rbp))
                         (((rax tmp-ra.15 rbp)
                           ((y.2 nfv.17 rbp)
                            (nfv.17 nfv.16 rbp)
                            (nfv.17 nfv.16 r15 rbp)
                            (nfv.17 nfv.16 r15 rbp)))
                          (z.3 tmp-ra.15 rbp)
                          (tmp-ra.15 rax rbp)
                          (rax rbp))))))
                     (begin
                       (set! tmp-ra.15 r15)
                       (set! x.1 fv0)
                       (set! y.2 fv1)
                       (if (< y.2 x.1)
                           (begin (set! rax x.1) (jump tmp-ra.15 rbp rax))
                           (begin
                             (return-point L.rp.5
                                           (begin
                                             (set! nfv.17 x.1)
                                             (set! nfv.16 y.2)
                                             (set! r15 L.rp.5)
                                             (jump L.swap.1 rbp r15 nfv.16 nfv.17)))
                             (set! z.3 rax)
                             (set! rax z.3)
                             (jump tmp-ra.15 rbp rax)))))
                                      (begin
                                        (set! tmp-ra.14 r15)
                                        (set! fv1 2)
                                        (set! fv0 1)
                                        (set! r15 tmp-ra.14)
                                        (jump L.swap.1 rbp r15 fv0 fv1))))
                '(module
                     ((conflicts
                        ((fv0 (r15 tmp-ra.14 fv1 rbp))
                         (fv1 (r15 tmp-ra.14 fv0 rbp))
                         (rbp (r15 tmp-ra.14 fv0 fv1))
                         (r15 (fv1 fv0 rbp))
                         (tmp-ra.14 (fv1 fv0 rbp))))
                      (locals (tmp-ra.14))
                      (undead-out
                       ((tmp-ra.14 rbp)
                        (tmp-ra.14 fv1 rbp)
                        (tmp-ra.14 fv1 fv0 rbp)
                        (fv1 fv0 r15 rbp)
                        (fv1 fv0 r15 rbp))))
                   (define L.swap.1
                     ((conflicts
                       ((rax (tmp-ra.15 rbp))
                        (rbp (tmp-ra.15 rax z.3 r15 nfv.16 y.2 nfv.17 x.1 fv1 fv0))
                        (fv1 (x.1 tmp-ra.15 rbp fv0 r15))
                        (fv0 (fv1 tmp-ra.15 rbp r15))
                        (r15 (nfv.17 nfv.16 rbp fv0 fv1))
                        (y.2 (nfv.17 rbp x.1 tmp-ra.15))
                        (x.1 (y.2 rbp tmp-ra.15 fv1))
                        (tmp-ra.15 (rax rbp z.3 x.1 y.2 fv1 fv0))
                        (z.3 (tmp-ra.15 rbp))
                        (nfv.17 (r15 nfv.16 y.2 rbp))
                        (nfv.16 (r15 nfv.17 rbp))))
                      (locals (nfv.16 nfv.17 z.3 tmp-ra.15 x.1 y.2))
                      (undead-out
                       ((fv0 fv1 tmp-ra.15 rbp)
                        (fv1 x.1 tmp-ra.15 rbp)
                        (y.2 x.1 tmp-ra.15 rbp)
                        ((y.2 x.1 tmp-ra.15 rbp)
                         ((tmp-ra.15 rax rbp) (rax rbp))
                         (((rax tmp-ra.15 rbp)
                           ((y.2 nfv.17 rbp)
                            (nfv.17 nfv.16 rbp)
                            (nfv.17 nfv.16 r15 rbp)
                            (nfv.17 nfv.16 r15 rbp)))
                          (z.3 tmp-ra.15 rbp)
                          (tmp-ra.15 rax rbp)
                          (rax rbp))))))
                     (begin
                       (set! tmp-ra.15 r15)
                       (set! x.1 fv0)
                       (set! y.2 fv1)
                       (if (< y.2 x.1)
                           (begin (set! rax x.1) (jump tmp-ra.15 rbp rax))
                           (begin
                             (return-point L.rp.5
                                           (begin
                                             (set! nfv.17 x.1)
                                             (set! nfv.16 y.2)
                                             (set! r15 L.rp.5)
                                             (jump L.swap.1 rbp r15 nfv.16 nfv.17)))
                             (set! z.3 rax)
                             (set! rax z.3)
                             (jump tmp-ra.15 rbp rax)))))
                   (begin
                     (set! tmp-ra.14 r15)
                     (set! fv1 2)
                     (set! fv0 1)
                     (set! r15 tmp-ra.14)
                     (jump L.swap.1 rbp r15 fv0 fv1)))
                "conflict-analysis: succes-10: return call")
  ;|#
  )
