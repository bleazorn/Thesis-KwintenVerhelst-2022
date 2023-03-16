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
  (let ([newConf (conflict-aloc-not-each-other a notC posC undead-out conf)]
        [rNotInPos (remove* (cons a notC) (append posC undead-out))])
    (foldl (lambda (p c) (conflict-aloc-not-each-other p '() `(,a) '() c)) newConf rNotInPos)))

;
;(conflict-begin b undead-outs conf)->conflicts?   '((aloc? (...)) ...)
;b:list? '(effect? ...)
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-begin b undead-outs conf)
  (for/fold ([prevConf conf]
             [nextUndead undead-outs] #:result prevConf)
            ([eff b])
    (values (conflict-effect eff nextUndead prevConf) (cdr nextUndead))))

;
;(conflict-effect e undead-outs conf)->conflicts?   '((aloc? (...)) ...)
;e: effect?
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-effect e undead-outs conf)
  (match e
    [`(begin ,eff ...) (conflict-begin eff (car undead-outs) conf)]
    [`(set! ,a (,binop ,b ,c)) (conflict-aloc a '() (filter aloc? `(,b ,c)) (car undead-outs) conf)]
    [`(set! ,a ,b) (conflict-aloc a `(,b) '() (car undead-outs) conf)]
    [_ #f]))

;
;(conflict-tail t conf)->conflicts?   '((aloc? (...)) ...)
;t: tail?
;undead-outs: undead-set-tree?
;conf:conflicts?
(define (conflict-tail t undead-outs conf)
  (if (> (length conf) 1)
      (match t
        [`(begin ,e ... ,tail) (conflict-tail tail (last undead-outs) (conflict-begin e (take undead-outs (sub1 (length undead-outs))) conf))]
        [`(halt ,a) (conflict-aloc a '() '() undead-outs conf)]
        [_ #f]
        )
      conf))

;
;(conflict-locals loc)->conflicts? '((aloc? (...)) ...)
;loc->locals?
(define (conflict-locals loc)
  (foldl (lambda (l c) (cons `(,l ()) c)) '() loc)  
  )

;Decorates a program with its conflict graph.
;(conflict-analysis p) -> Asm-lang-V2-conflicts?
;p: Asm-lang-V2-undead?
(define (conflict-analysis p)
  (match p
    [`(module ((locals ,loc) (undead-out ,u)) ,t) `(module ,(cons `(locals ,loc) `((conflicts ,(conflict-tail t u (conflict-locals loc))))) ,t)]
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
  )
;|#