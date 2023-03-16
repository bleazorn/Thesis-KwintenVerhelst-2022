#lang racket

(require "common.rkt")
(provide conflict-analysis)

(module+ test
  (require rackunit))

#|
Approximation of Conflict
Any variable defined during a non-move instruction is in conflict with every variable (except itself) in the undead-out set associated with the instruction.

Any variable defined during a move instruction is in conflict with every variable in the undead-out set associated with the instruction, except itself and the variable referenced in the move.
|#

;
;(conflict-aloc-not-each-other a notC posC undead-out conf)->conflicts?
;a: aloc?
;notC: list? '(aloc ...)
;posC: list? '(aloc ...)
;undead-out: undead-set?
(define (conflict-aloc-not-each-other a notC posC undead-out conf)
  ;(println (format "~a - ~a ~a - ~a - ~a" a notC posC undead-out conf))
  (if (aloc? a)
      (let ([conflicted (append posC undead-out)]
            [iAsoc (index-where conf (lambda (l) (equal? a (car l))))])
        (let ([rNotInPos (remove* (cons a notC) conflicted)]
              [alreadyconflicts (second (list-ref conf iAsoc))])
          (list-set conf iAsoc (cons a `(,(remove-duplicates (append rNotInPos alreadyconflicts)))))))
      conf))

;
;(conflict-aloc a notC posC undead-out conf)->conflicts?
;a: aloc?
;notC: list? '(aloc ...)
;posC: list? '(aloc ...)
;undead-out: undead-set?
(define (conflict-aloc a notC posC undead-out conf)
  ;(println (format "conflict: ~a : ~a - ~a" a posC undead-out))
  (if (aloc? a)
      (let ([newConf (conflict-aloc-not-each-other a notC (filter aloc? posC) undead-out conf)]
            [rNotInPos (remove* (cons a notC) (filter aloc? (append posC undead-out)))])
        (foldl (lambda (p c) (conflict-aloc-not-each-other p '() `(,a) '() c)) newConf rNotInPos))
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
    [`(,relop ,a ,b) (conflict-aloc a '() `(,b) undead-outs conf)]
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
    [`(set! ,a (,binop ,b ,c)) (conflict-aloc b '() `(,c) undead-outs
                                              (conflict-aloc a '() `(,b ,c) undead-outs conf))]
    [`(set! ,a ,b) (conflict-aloc a `(,b) '() undead-outs conf)]
    [`(if ,p ,e1 ,e2) (conflict-effect e2 (third undead-outs)
                                         (conflict-effect e1 (second undead-outs)
                                                        (conflict-pred p (first undead-outs) conf)))]
    [_ #f]))

;
;(conflict-tail t conf)->conflicts?   '((aloc? (...)) ...)
;t: tail?
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-tail t undead-outs conf)
  ;(println (format "conflict-tail: ~a -:- ~a" t undead-outs))
  (if (> (length conf) 1)
      (match t
        [`(begin ,e ... ,tail) (conflict-tail tail (last undead-outs) (conflict-begin e (drop-right undead-outs 1) conf))]
        [`(halt ,a) (conflict-aloc a '() '() undead-outs conf)]
        [`(if ,p ,t1 ,t2) (conflict-tail t2 (third undead-outs)
                                         (conflict-tail t1 (second undead-outs)
                                                        (conflict-pred p (first undead-outs) conf)))]
        [`(jump ,trg ,l ...) (conflict-aloc trg '() '() undead-outs conf)]
        [_ #f]
        )
      conf))

;
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
    [`(define ,l ((locals ,loc) (undead-out ,u)) ,t) `(define ,l ,(cons `(locals ,loc) `((conflicts ,(conflict-tail t u (conflict-locals loc))))) ,t)]
    [_ #f]))

;Decorates a program with its conflict graph.
;(conflict-analysis p) -> Asm-lang-V2-conflicts?
;p: Asm-lang-V2-undead?
(define (conflict-analysis p)
  (match p
    [`(module ((locals ,loc) (undead-out ,u)) ,f ... ,t) `(module ,(cons `(locals ,loc) `((conflicts ,(conflict-tail t u (conflict-locals loc))))) ,@(map conflict-func f) ,t)]
    [_ #f]))


;#|
(module+ test
;conflict-analysis
  ;succes
  (check-equal? (conflict-analysis '(module ((locals (x.1))
                                             (undead-out ((x.1) ())))
                                      (begin
                                        (set! x.1 42)
                                        (halt x.1))))
                '(module
                     ((locals (x.1)) (conflicts ((x.1 ()))))
                   (begin (set! x.1 42) (halt x.1)))
                "conflict-analysis: succes-1: one instruction")
  (check-equal? (conflict-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                             (undead-out
                                              ((v.1)
                                               (v.1 w.2)
                                               (w.2 x.3)
                                               (p.1 w.2 x.3)
                                               (w.2 x.3)
                                               (y.4 w.2 x.3)
                                               (p.1 y.4 w.2 x.3)
                                               (y.4 w.2 x.3)
                                               (z.5 y.4 w.2)
                                               (z.5 y.4)
                                               (t.6 z.5)
                                               (t.6 z.5 p.1)
                                               (t.6 z.5)
                                               (z.5)
                                               ())))
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
                                        (halt z.5))))
                '(module
                     ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                      (conflicts
                       ((p.1 (t.6 z.5 y.4 w.2 x.3))
                        (t.6 (z.5 p.1))
                        (z.5 (t.6 p.1 w.2 y.4))
                        (y.4 (z.5 p.1 w.2 x.3))
                        (x.3 (y.4 p.1 w.2))
                        (w.2 (z.5 y.4 p.1 x.3 v.1))
                        (v.1 (w.2)))))
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
                     (halt z.5)))
                "conflict-analysis: succes-2: multiple instructions")
  (check-equal? (conflict-analysis '(module ((locals (x.1 y.1)) (undead-out (() (x.1) ())))
                                      (begin (set! y.1 42) (set! x.1 5) (halt x.1))))
                '(module ((locals (x.1 y.1)) (conflicts ((y.1 ()) (x.1 ()))))
                   (begin (set! y.1 42) (set! x.1 5) (halt x.1)))
                "conflict-analysis: succes-03: unused variable")
  (check-equal? (conflict-analysis '(module ((locals (x.1 y.1)) (undead-out ((x.1) (x.1) ())))
                                      (begin (set! x.1 5) (set! y.1 42) (halt x.1))))
                '(module ((locals (x.1 y.1)) (conflicts ((y.1 (x.1)) (x.1 (y.1)))))
                   (begin (set! x.1 5) (set! y.1 42) (halt x.1)))
                "conflict-analysis: succes-04: unused variable")
    (check-equal? (conflict-analysis '(module ((locals (x.1)) (undead-out ((x.1) ((y.2 x.1) ((z.3) (x.1))) ())))
                                        (begin
                                          (set! x.1 42)
                                          (begin
                                            (set! y.2 x.1)
                                            (begin
                                              (set! z.3 (+ y.2 x.1))
                                              (set! x.1 z.3)))
                                          (halt x.1))))
                '(module ((locals (x.1)) (conflicts ((x.1 ()))))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 x.1)
                       (begin
                         (set! z.3 (+ y.2 x.1))
                         (set! x.1 z.3)))
                     (halt x.1)))
                "conflict-analysis: succes-05: begin effect instruction")
  (check-equal? (conflict-analysis '(module ((locals (x.1 y.2 z.3)) (undead-out ((x.1) ((y.2) (x.1)) ((z.3) (x.1)) ())))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 (+ x.1 50))
                                          (set! x.1 y.2))
                                        (begin
                                          (set! z.3 (+ x.1 x.1))
                                          (set! x.1 z.3))
                                        (halt x.1))))
                
                '(module ((locals (x.1 y.2 z.3))  (conflicts ((z.3 (x.1)) (y.2 (x.1)) (x.1 (z.3 y.2)))))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 (+ x.1 50))
                       (set! x.1 y.2))
                     (begin
                       (set! z.3 (+ x.1 x.1))
                       (set! x.1 z.3))
                     (halt x.1)))
                "conflict-analysis: succes-06: sequential begin effects")
  (check-equal? (conflict-analysis '(module ((locals (x.1 y.2 z.3 a.4 b.5)) (undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1))) ())))
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
                                        (halt x.1))))           
                '(module ((locals (x.1 y.2 z.3 a.4 b.5)) (conflicts ((b.5 (y.2 a.4)) (a.4 (b.5)) (z.3 (x.1)) (y.2 (b.5 x.1)) (x.1 (z.3 y.2)))))
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
                     (halt x.1)))
                "conflict-analysis: succes-07: sequential and nested begin effects")
  (check-equal? (conflict-analysis '(module ((locals (x.1 y.2 z.3 a.4 b.5)) (undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1) ())))))
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
                                            (halt x.1))))))           
                '(module ((locals (x.1 y.2 z.3 a.4 b.5)) (conflicts ((b.5 (y.2 a.4)) (a.4 (b.5)) (z.3 (x.1)) (y.2 (b.5 x.1)) (x.1 (z.3 y.2)))))
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
                         (halt x.1)))))
                "conflict-analysis: succes-08: sequential and nested begin effects with tail in nested")
  (check-equal? (conflict-analysis '(module ((locals (x.1 y.2 b.3 c.4)) (undead-out ((x.1) (x.1 y.2) ((b.3 y.2) (b.3) (b.3 c.4) ((c.4) () ((c.4) ()))))))
                                      (begin
                                        (set! x.1 5)
                                        (set! y.2 x.1)
                                        (begin
                                          (set! b.3 x.1)
                                          (set! b.3 (+ b.3 y.2))
                                          (set! c.4 b.3)
                                          (if (= c.4 b.3)
                                              (halt c.4)
                                              (begin
                                                (set! x.1 c.4)
                                                (halt c.4)))))))                 
                '(module ((locals (x.1 y.2 b.3 c.4)) (conflicts ((c.4 (b.3)) (b.3 (c.4 y.2)) (y.2 (b.3)) (x.1 ()))))
                   (begin
                     (set! x.1 5)
                     (set! y.2 x.1)
                     (begin
                       (set! b.3 x.1)
                       (set! b.3 (+ b.3 y.2))
                       (set! c.4 b.3)
                       (if (= c.4 b.3)
                           (halt c.4)
                           (begin
                             (set! x.1 c.4)
                             (halt c.4))))))
                "conflict-analysis: succes-09: if tail")
  )
;|#