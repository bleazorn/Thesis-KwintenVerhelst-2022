#lang racket

(require "common.rkt")
(provide undead-analysis)

(module+ test
  (require rackunit))

;
;(remove-undead u undead-out)->undead-set?
;u: aloc?
;undead-out: undead-set?
(define (undead-remove u undead-out)
  (if (aloc? u)
      (remove u undead-out)
      undead-out))

;
;(cons-undead u undead-out)->undead-set?
;u: aloc?
;undead-out: undead-set?
(define (undead-cons u undead-out)
  (if (and (aloc? u) (not (member u undead-out)))
      (cons u undead-out)
      undead-out))

;
;(undead-first undead-outs)->undead-out?
;undead-outs: undead-set-tree?
(define (undead-get-first undead-outs)
  (cond
    [(null? undead-outs) (values '() '())]
    [(and (not (null? (car undead-outs))) (list? (car (car undead-outs))))
     (values (car (car undead-outs)) (cons (cdr (car undead-outs)) (cdr undead-outs)))]
    [else (values (car undead-outs) (cdr undead-outs))]))


;
;(undead-begin b undead-outs)->undead-set-tree? '(() ... ,@undead-outs)
;b: list? '(effect? ...)
;undead-outs: undead-set-tree?  tail/pred '(() ()) effect '(()) 
(define (undead-begin-rec b undead-outs)
  (match b
    ['() undead-outs]
    [`(,e ,rest-e ...)
     (let ([undead-rest (undead-begin-rec rest-e undead-outs)])
       (undead-effect e undead-rest))]))

;
;(undead-begin b undead-outs undead-rest)->undead-set-tree?
;b: list? '(effect? ...)
;undead-out, undead-rest: undead-set-tree?
(define (undead-begin b undead-outs undead-rest)
  (let ([bU (undead-begin-rec b undead-outs)])
    (cons (car bU) (cons (cdr bU) undead-rest))))

;
;(undead-effect e undead-out)->undead-out?
;e->effect?
;undead-outs->undead-out?                                                                                        
(define (undead-effect e undead-outs)
  (let ([undead-out (if (null? undead-outs) '() (car undead-outs))]
        [undead-rest (if (null? undead-outs) '() (cdr undead-outs))])
    (match e
      [`(set! ,a (,binop ,b ,c)) (cons (undead-cons b (undead-cons c (undead-remove a undead-out))) undead-outs)]
      [`(set! ,a ,b) (cons (undead-cons b (undead-remove a undead-out)) undead-outs)]
      [`(begin ,e ...) (undead-begin e `(,undead-out) undead-rest)]
      [_ #f])))

;
;(undead-tail t undead-out)->undead-set-tree?
;t->tail?
;undead-outs->undead-set-tree?
(define (undead-tail t undead-outs)
  (let ([undead-out (if (null? undead-outs) '() (car undead-outs))]
        [undead-rest (if (null? undead-outs) '() (cdr undead-outs))])
    (match t
      [`(halt ,a) (cons (undead-cons a undead-out) undead-outs)]
      [`(begin ,e ... ,tail) (let* ([tU (undead-tail tail `(,undead-out))])
                               (undead-begin e tU undead-rest))]
      [_ #f])))


;Performs undeadness analysis, decorating the program with undead-set tree. Only the info field of the program is modified.
;(undead-analysis p) → Asm-lang-V2-undead?
;p:Asm-lang-V2-locals?
(define (undead-analysis p)
  (match p
    [`(module (,loc) ,t) `(module ,(cons loc `((undead-out ,(car  (cdr (undead-tail t '(()))))))) ,t)] ;ALs enkel halt instructi (x.1) ipv ((x.1))
    [_ (println "failed analysis")]))

(module+ test
;undead-remove
  ;succes
  (check-equal? (undead-remove 'x.1 '()) '() "undead-remove: succes-1: empty undead-out")
  (check-equal? (undead-remove 'x.1 '(a.1 x.1 z.2)) '(a.1 z.2) "undead-remove: succes-2: not empty undead-out")
  (check-equal? (undead-remove 5 '(a.1 x.1 z.2)) '(a.1 x.1 z.2) "undead-remove: succes-3: not aloc")
;undead-cons
  ;succes
  (check-equal? (undead-cons 'x.1 '()) '(x.1) "undead-cons: succes-1: empty undead-out")
  (check-equal? (undead-cons 'x.1 '(a.1 z.2)) '(x.1 a.1 z.2) "undead-cons: succes-2: not empty undead-out")
  (check-equal? (undead-cons 5 '(a.1 x.1 z.2)) '(a.1 x.1 z.2) "undead-cons: succes-3: not aloc")
;undead-begin-rec
  ;succes
  (check-equal? (undead-begin-rec '((set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2))) '((z.3) ()))
                '(() (x.1) (x.1 y.2) (z.3) ())
                "undead-begin-rec: succes-1: simple begin")
;undead-begin
  ;succes
  (check-equal? (undead-begin '((set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2))) '((z.3)) '(()))
                '(() ((x.1) (x.1 y.2) (z.3)) ())
                "undead-begin: succes-1: simple begin")
;undead-effect
  ;succes
  (check-equal? (undead-effect '(set! z.3 x.1) '((z.3) ())) '((x.1) (z.3) ()) "undead-effect: succes-1: set")
  (check-equal? (undead-effect '(set! z.3 (+ x.1 y.2)) '((z.3) ())) '((x.1 y.2) (z.3) ()) "undead-effect: succes-2: binop")
  (check-equal? (undead-effect '(begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2))) '((z.3) ()))
                '(() ((x.1) (x.1 y.2) (z.3)) ())
                "undead-effect: succes-3: simple begin")
;undead-tail
  ;succes
  (check-equal? (undead-tail '(halt x.1) '((z.1) ())) '((x.1 z.1) (z.1) ()) "undead-tail: succes-1: halt")
  (check-equal? (undead-tail '(halt x.1) '(())) '((x.1) ()) "undead-tail: succes-2: start halt")
  (check-equal? (undead-tail '(begin (set! x.1 42) (halt x.1)) '(())) '(() ((x.1) ())) "undead-tail: succes-3: simple begin")
  (check-equal? (undead-tail '(begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (halt z.3)) '(()))
                '(() ((x.1) (x.1 y.2) (z.3) ()))
                "undead-tail: succes-4: simple begin")
  ;#|
;undead-analysis
  ;succes
  (check-equal? (undead-analysis '(module ((locals (x.1)))
                                    (begin
                                      (set! x.1 42)
                                      (halt x.1))))
                '(module

                     ((locals (x.1)) (undead-out ((x.1) ())))
                   (begin (set! x.1 42) (halt x.1)))
                "undead-analysis: succes-01: one instruction")
  (check-equal? (undead-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
                     (halt z.5)))
                "undead-analysis: succes-02: multiple instructions")
  (check-equal? (undead-analysis '(module ((locals (x.1 y.1)))
                                    (begin
                                      (set! y.1 42)
                                      (set! x.1 5)
                                      (halt x.1))))
                '(module                     
                     ((locals (x.1 y.1)) (undead-out (() (x.1) ())))
                   (begin (set! y.1 42) (set! x.1 5) (halt x.1)))
                "undead-analysis: succes-03: unused variable")
  (check-equal? (undead-analysis '(module ((locals (x.1 y.1)))
                                    (begin
                                      (set! x.1 5)
                                      (set! y.1 42)
                                      (halt x.1))))
                '(module
                     ((locals (x.1 y.1)) (undead-out ((x.1) (x.1) ())))
                   (begin (set! x.1 5) (set! y.1 42) (halt x.1)))
                "undead-analysis: succes-04: unused variable")
    (check-equal? (undead-analysis '(module ((locals (x.1)))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 x.1)
                                          (begin
                                            (set! z.3 (+ y.2 x.1))
                                            (set! x.1 z.3)))
                                        (halt x.1))))
                '(module
                     ((locals (x.1)) (undead-out ((x.1) ((y.2 x.1) ((z.3) (x.1))) ())))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 x.1)
                       (begin
                         (set! z.3 (+ y.2 x.1))
                         (set! x.1 z.3)))
                     (halt x.1)))
                "undead-analysis: succes-05: begin effect instruction")
  (check-equal? (undead-analysis '(module ((locals (x.1 y.2 z.3)))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 (+ x.1 50))
                                          (set! x.1 y.2))
                                        (begin
                                            (set! z.3 (+ x.1 x.1))
                                            (set! x.1 z.3))
                                        (halt x.1))))
                
                '(module ((locals (x.1 y.2 z.3)) (undead-out ((x.1) ((y.2) (x.1)) ((z.3) (x.1)) ())))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 (+ x.1 50))
                                          (set! x.1 y.2))
                                        (begin
                                            (set! z.3 (+ x.1 x.1))
                                            (set! x.1 z.3))
                                        (halt x.1)))
                "undead-analysis: succes-06: sequential begin effects")
  (check-equal? (undead-analysis '(module ((locals (x.1 y.2 z.3 a.4 b.5)))
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
                '(module ((locals (x.1 y.2 z.3 a.4 b.5)) (undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1))) ())))
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
                "undead-analysis: succes-07: sequential and nested begin effects")
  (check-equal? (undead-analysis '(module ((locals (x.1 y.2 z.3 a.4 b.5)))
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
                '(module ((locals (x.1 y.2 z.3 a.4 b.5)) (undead-out ((x.1) (() ((a.4) (a.4 b.5) ((a.4 b.5) (y.2 b.5)) ((b.5) (y.2)) (x.1)) ((z.3) (x.1) ())))))
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
                "undead-analysis: succes-08: sequential and nested begin effects with tail in nested")
  ;|#
  )