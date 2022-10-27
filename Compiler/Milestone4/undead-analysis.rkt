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

(define (undead-begin-rest b undead-outs)
  ;(println (format "rest: ~a - ~a" b undead-outs))
  (match b
    ['() undead-outs]
    [`(,s ,rest-ss ...)
     ;(println (format "rest-let: ~a - ~a" rest-ss undead-outs))
     (let ([undead-outs-rest (undead-begin-rest rest-ss undead-outs)])
       (undead-effect s undead-outs-rest))]))       

;
;(undead-begin bs undead-outs)->undead-set-tree?
;bs: list? '(effect? ...)
;undead-outs: undead-set-tree?                                     
(define (undead-begin b undead-outs)
  (match (cons b undead-outs)
    [(cons '() '()) '()]
    [(cons `(,s ,rest-ss ...) `(,undead-out ,rest-undead-outs ...))
     ;(println (format "begin: ~a - ~a" rest-ss `(,undead-out)))
     (let ([undead-outs-rest (undead-begin-rest rest-ss `(,undead-out))])                    
       (append (undead-effect s `(,undead-outs-rest)) rest-undead-outs))]))                                                         ;(((x.1 y.2) ((z.3) (x.1))) ())

;
;(undead-effect e undead-out)->undead-out?
;e->effect?
;undead-outs->undead-out?                                                                                        
(define (undead-effect e undead-outs)                  
  (let ([undead-out (if (and (not (null? (car undead-outs))) (list? (car (car undead-outs))))
                         (car (car undead-outs))
                         (car undead-outs))])
    (match e
      [`(begin ,e ...) (undead-begin e undead-outs)]                                                                    
      [`(set! ,a (,binop ,b ,c)) (cons (undead-cons c (undead-cons b (undead-remove a undead-out))) undead-outs)]       
      [`(set! ,a ,b) (cons (undead-cons b (undead-remove a undead-out)) undead-outs)]
      [_ #f])))

;
;(undead-tail t undead-out)->undead-set-tree?
;t->tail?
;undead-outs->undead-set-tree?
(define (undead-tail t undead-outs)
  ;(println (format "tail: ~a - ~a" t undead-outs))
  (match t
    [`(begin ,e ... ,tail) (undead-begin-rest e (undead-tail tail undead-outs))]
    [`(halt ,a) (cons (undead-cons a (car undead-outs)) undead-outs)]
    [_ #f]))

;Performs undeadness analysis, decorating the program with undead-set tree. Only the info field of the program is modified.
;(undead-analysis p) → Asm-lang-V2-undead?
;p:Asm-lang-V2-locals?
(define (undead-analysis p)
  (match p
    [`(module (,loc) ,t) `(module ,(cons loc `((undead-out ,(cdr (undead-tail t '(())))))) ,t)]
    [_ (println "failed analysis")]))


;(undead-analysis '(module ((locals (x.1))) (begin (set! x.1 42) (begin (set! y.2 x.1) (begin (set! z.3 (+ y.2 x.1)) (set! x.1 z.3))) (halt x.1))))
;(undead-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))) (begin (set! v.1 1) (set! w.2 46) (set! x.3 v.1) (set! p.1 7) (halt z.5))))
(module+ test
 ; #|
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
;undead-effect
  ;succes
  (check-equal?  (undead-effect '(set! z.5 (+ z.5 t.6)) '((z.5))) '((t.6 z.5)(z.5)) "undead-effect: succes-1: binop operation")
  (check-equal?  (undead-effect '(set! p.1 -1) '((p.1 t.6 z.5))) '((t.6 z.5)(p.1 t.6 z.5)) "undead-effect: succes-2: assign integer")
  (check-equal?  (undead-effect '(set! t.6 y.4) '((t.6 z.5))) '((y.4 z.5)(t.6 z.5)) "undead-effect: succes-3: assign asoc")
  (check-equal?  (undead-effect '(set! y.4 x.3) '((y.4 x.3 w.2))) '((x.3 w.2)(y.4 x.3 w.2)) "undead-effect: succes-4: assign asoc and it is already an undead")
;undead-begin-rest
  ;succes
  (check-equal?  (undead-begin-rest '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '(()))
                 '((a.4 b.4) (b.4 x.1) (x.1 y.2) (z.3) ())
                 "undead-begin-rest: succes-1: multiple effects empty undead-out")
  (check-equal?  (undead-begin-rest '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '((x.1)))
                 '((a.4 b.4) (b.4 x.1) (x.1 y.2) (z.3) (x.1))
                 "undead-begin-rest: succes-2: multiple effects undead-out")
  (check-equal?  (undead-begin-rest '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '((y.2)))
                 '((a.4 b.4) (b.4 x.1) (x.1 y.2) (z.3 y.2) (y.2))
                 "undead-begin-rest: succes-3: multiple effects undead-out")
  (check-equal?  (undead-begin-rest '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '((y.2 z.3) (a.1)))
                 '((a.4 b.4) (b.4 x.1) (x.1 y.2) (y.2 z.3) (y.2 z.3) (a.1))
                 "undead-begin-rest: succes-4: multiple effects undead-out")
;undead-begin
  ;succes
  (check-equal?  (undead-begin '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '(()))
                 '((a.4 b.4) ((b.4 x.1) (x.1 y.2) (z.3) ()))
                 "undead-begin: succes-1: multiple effects empty undead-out")
  (check-equal?  (undead-begin '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '((x.1)))
                 '((a.4 b.4) ((b.4 x.1) (x.1 y.2) (z.3) (x.1)))
                 "undead-begin: succes-2: multiple effects undead-out")
  (check-equal?  (undead-begin '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '((y.2)))
                 '((a.4 b.4) ((b.4 x.1) (x.1 y.2) (z.3 y.2) (y.2)))
                 "undead-begin: succes-3: multiple effects undead-out")
  (check-equal?  (undead-begin '((set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '((y.2 z.3) (a.1) (p.8)))
                 '((a.4 b.4) ((b.4 x.1) (x.1 y.2) (y.2 z.3) (y.2 z.3)) (a.1) (p.8))
                 "undead-begin: succes-4: multiple effects undead-out")
;undead-effect
  ;succes
  (check-equal? (undead-effect '(set! x.1 y.2) '((x.1 z.3) (a.1) (p.8)))
                '((y.2 z.3) (x.1 z.3) (a.1) (p.8))
                "undead-effect: succes-01: set instruction")
  (check-equal? (undead-effect '(set! x.1 y.2) '(()))
                '((y.2) ())
                "undead-effect: succes-02: set instruction empty undead")
  (check-equal? (undead-effect '(set! x.1 y.2) '(((x.1 z.3) (a.1) (p.8)) (z.3)))
                '((y.2 z.3) ((x.1 z.3) (a.1) (p.8)) (z.3))
                "undead-effect: succes-03: set instruction begin undead out")
  
  (check-equal? (undead-effect '(set! x.1 (+ x.1 y.2)) '((x.1 z.3) (a.1) (p.8)))
                '((y.2 x.1 z.3) (x.1 z.3) (a.1) (p.8))
                "undead-effect: succes-04: binop instruction")
  (check-equal? (undead-effect '(set! x.1 (+ y.2 y.2)) '((x.1 z.3) (a.1) (p.8)))
                '((y.2 z.3) (x.1 z.3) (a.1) (p.8))
                "undead-effect: succes-05: binop instruction")
  (check-equal? (undead-effect '(set! x.1 (+ y.2 z.3)) '(()))
                '((z.3 y.2) ())
                "undead-effect: succes-06: binop instruction empty undead")
  (check-equal? (undead-effect '(set! x.1 (+ y.2 z.3)) '(((x.1 z.3) (a.1) (p.8)) (o.5)))
                '((y.2 z.3) ((x.1 z.3) (a.1) (p.8)) (o.5))
                "undead-effect: succes-07: binop instruction begin undead")

  (check-equal?  (undead-effect '(begin (set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '((y.2 z.3) (a.1) (p.8)))
                 '((a.4 b.4) ((b.4 x.1) (x.1 y.2) (y.2 z.3) (y.2 z.3)) (a.1) (p.8))
                 "undead-begin: succes-08: multiple effects undead-out")
  (check-equal?  (undead-effect '(begin (set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '(()))
                 '((a.4 b.4) ((b.4 x.1) (x.1 y.2) (z.3) ()))
                 "undead-begin: succes-09: multiple effects undead-out empty undead")



  
  (check-equal?  (undead-effect '(begin (set! x.1 a.4) (set! y.2 b.4) (set! z.3 (+ y.2 x.1)) (set! x.1 z.3)) '(((y.2 z.3) (a.1) (p.8)) (o.9)))
                 '((a.4 b.4) ((b.4 x.1) (x.1 y.2) (y.2 z.3) ((y.2 z.3) (a.1) (p.8))) (o.9))
                 "undead-begin: succes-10: multiple effects undead-out begin undead")


  
  (check-equal?  (undead-effect '(begin (set! x.1 a.4) (begin (set! y.2 b.4) (set! z.3 (+ y.2 x.1))) (set! x.1 z.3)) '((y.2 z.3) (a.1) (p.8) (o.9)))
                 '((a.4 b.4) ((b.4 x.1) ((x.1 y.2) (y.2 z.3)) (y.2 z.3)) (a.1) (p.8) (o.9))
                 "undead-begin: succes-11: nested begins")




  
  (check-equal?  (undead-effect '(begin (set! x.1 a.4) (begin (set! y.2 b.4) (begin (set! z.3 (+ y.2 x.1))) (set! x.1 z.3))) '((y.2 z.3) (a.1) (p.8) (o.9)))
                 '((a.4 b.4) ((b.4 x.1) ((x.1 y.2) ((y.2 z.3)) (y.2 z.3))) (a.1) (p.8) (o.9))
                 "undead-begin: succes-12: nested begins")

;undead-tail
  ;succes
  (check-equal?  (undead-tail '(halt z.5) '(())) '((z.5)()) "undead-tail: succes-1: halt instruction")
;undead-analysis
  ;succes
  (check-equal? (undead-analysis '(module ((locals (x.1)))
                                    (begin
                                      (set! x.1 42)
                                      (halt x.1))))
                '(module

                     ((locals (x.1)) (undead-out ((x.1) ())))
                   (begin (set! x.1 42) (halt x.1)))
                "undead-analysis: succes-1: one instruction")
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
                        (p.1 x.3 w.2)
                        (x.3 w.2)
                        (y.4 x.3 w.2)
                        (p.1 y.4 x.3 w.2)
                        (x.3 w.2 y.4)
                        (w.2 z.5 y.4)
                        (y.4 z.5)
                        (t.6 z.5)
                        (p.1 t.6 z.5)
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
                     (halt z.5)))
                "undead-analysis: succes-2: multiple instructions")
  (check-equal? (undead-analysis '(module ((locals (x.1 y.1)))
                                    (begin
                                      (set! y.1 42)
                                      (set! x.1 5)
                                      (halt x.1))))
                '(module                     
                     ((locals (x.1 y.1)) (undead-out (() (x.1) ())))
                   (begin (set! y.1 42) (set! x.1 5) (halt x.1)))
                "undead-analysis: succes-3: unused variable")
  (check-equal? (undead-analysis '(module ((locals (x.1 y.1)))
                                    (begin
                                      (set! x.1 5)
                                      (set! y.1 42)
                                      (halt x.1))))
                '(module
                     ((locals (x.1 y.1)) (undead-out ((x.1) (x.1) ())))
                   (begin (set! x.1 5) (set! y.1 42) (halt x.1)))
                "undead-analysis: succes-4: unused variable")
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
                     ((locals (x.1)) (undead-out ((x.1) ((x.1 y.2) ((z.3) (x.1))) ())))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 x.1)
                       (begin
                         (set! z.3 (+ y.2 x.1))
                         (set! x.1 z.3)))
                     (halt x.1)))
                "undead-analysis: succes-5: begin effect instruction")
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
                "undead-analysis: succes-6: sequential begin effects")
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
                "undead-analysis: succes-7: sequential and nested begin effects")
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
                "undead-analysis: succes-8: sequential and nested begin effects with tail in nested")
  

  
  ;|#
  )