#lang racket

(require "common/info.rkt"
         "common/aloc.rkt"
         "common/fvar.rkt"
         "common/register.rkt")
(provide undead-analysis)

(module+ test
  (require rackunit))

(define call '())
(define (resetCall)
  (set! call '()))
(define (addCallJump u)
  (set! call (remove-duplicates (append u call)))
  )
(define (addCallReturn rU nU)
  (set! call (remove-duplicates (append (filter (lambda (u) (and (or (aloc? u) (fvar? u)) (member u nU))) rU) call)))
  )

;
;(remove-undead u undead-out)->undead-set?
;u: aloc?
;undead-out: undead-set?
(define (undead-remove u undead-out)
  (if (or (aloc? u) (register? u) (fvar? u))
      (remove u undead-out)
      undead-out))

;
;(cons-undead u undead-out)->undead-set?
;u: aloc?
;undead-out: undead-set?
(define (undead-cons u undead-out)
  (if (and (or (aloc? u) (register? u) (fvar? u)) (not (member u undead-out)))
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
     ;(println (format "rest-let: ~a - ~a" rest-ss undead-outs))
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
;(undead-if p u1 u2)->undead-set-tree?
;p: pred?
;u1, u2: undead-set-tree? '((u) (r))
(define (undead-if p u1 u2 undead-rest)
  (let* ([newUndead (remove-duplicates (append (car u1) (car u2)))]
         [pU (undead-pred p `(,newUndead))])    ;'((p) (r))
    (cons (car pU) (cons `(,(second pU) ,(second u1) ,(second u2)) undead-rest))))
;
;(undead-pred p undead-outs)->undead-set-tree?
;p: pred?
;undead-outs: undead-set-tree? 
(define (undead-pred p undead-outs)
  (let ([undead-out (if (null? undead-outs) '() (car undead-outs))]
        [undead-rest (if (null? undead-outs) '() (cdr undead-outs))])
    (match p
      [`(begin ,e ... ,pred) (let ([pU (undead-pred pred `(,undead-out))])
                               (undead-begin e pU undead-rest))]
      [`(if ,p1 ,p2 ,p3) (let* ([u1 (undead-pred p2 `(,undead-out))]    ;'((u) (r))
                                [u2 (undead-pred p3 `(,undead-out))])
                           (undead-if p1 u1 u2 undead-rest))]
      [`(,relop ,a ,b) (cons (undead-cons a (undead-cons b undead-out)) undead-outs)]
      [`(true) (cons undead-out undead-outs)]
      [`(false) (cons undead-out undead-outs)]
      [`(not ,pred) (undead-pred pred undead-outs)]
      [_ #f])))

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
      [`(if ,p ,e1 ,e2) (let* ([u1 (undead-effect e1 `(,undead-out))]
                               [u2 (undead-effect e2 `(,undead-out))])
                          (undead-if p u1 u2 undead-rest))]
      [`(return-point ,l ,t) (let* ([uTail (undead-tail t '(()))]
                                    [nextUndead (cons l (car uTail))])
                               (addCallReturn nextUndead undead-out)
                               (cons  nextUndead (cons (list-set uTail 0 undead-out) undead-rest)))]
      [_ "effect"])))

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
      [`(if ,p ,t1 ,t2) (let* ([u1 (undead-tail t1 `(,undead-out))]    ;'((u) (r))
                               [u2 (undead-tail t2 `(,undead-out))])
                          (undead-if p u1 u2 undead-rest))]
      [`(jump ,trg ,l ...) (let ([uJump (cond [(or (aloc? trg) (fvar? trg)) (addCallJump `(,trg)) (cons trg l)]
                                              [else l])])
                             (cons uJump (cons l undead-rest)))]
      [_ "tail"])))

;
;(undead-info t)->list? '((undead-out) (call-undead))
;t: tail?
(define (undead-info i t)
  (let ([uTail (undead-tail t '(()))])
    (let ([theCall call])
      (resetCall)
      (addInfo (addInfo i (setCallUndead theCall)) (setUndead-out (car  (cdr uTail)))))))

;
;(undead-func f)->'(define label? info? tail?) info?: '(locals undead-out)
;f: '(define label? info? tail?)
(define (undead-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(undead-info i t) ,t)]
    [_ "func"]))


;Performs undeadness analysis, decorating the program with undead-set tree. Only the info field of the program is modified.
;(undead-analysis p) → Asm-lang-V2-undead?
;p:Asm-lang-V2-locals?
(define (undead-analysis p)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(undead-info i t) ,@(map undead-func f) ,t)]
    [_ (println "failed analysis")]))


(module+ test
  #|
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
;undead-pred
  ;succes
  (check-equal? (undead-pred '(= x.1 y.2) '((z.3) ())) '((x.1 y.2 z.3) (z.3) ()) "undead-pred: succes-1: relop")
  (check-equal? (undead-pred '(true) '((z.3) ())) '((z.3) (z.3) ()) "undead-pred: succes-2: true")
  (check-equal? (undead-pred '(false) '((z.3) ())) '((z.3) (z.3) ()) "undead-pred: succes-3: false")
  (check-equal? (undead-pred '(not (= x.1 y.2)) '((z.3) ())) '((x.1 y.2 z.3) (z.3) ()) "undead-pred: succes-4: not")
  (check-equal? (undead-pred '(begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (= 5 z.3)) '((z.3) ()))
                '(() ((x.1) (x.1 y.2) (z.3) (z.3)) ())
                "undead-pred: succes-5: simple begin")
  (check-equal? (undead-pred '(if (= x.1 y.2) (true) (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (= 5 z.3))) '((z.3) ()))
                '((x.1 y.2 z.3) ((z.3) (z.3) ((x.1) (x.1 y.2) (z.3) (z.3))) ())
                "undead-pred: succes-6: if")
  (check-equal? (undead-pred '(if (= x.1 y.2) (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (= 5 z.3)) (true)) '((z.3) ()))
                '((x.1 y.2 z.3) ((z.3) ((x.1) (x.1 y.2) (z.3) (z.3)) (z.3)) ())
                "undead-pred: succes-7: if change")
;undead-effect
  ;succes
  (check-equal? (undead-effect '(set! z.3 x.1) '((z.3) ())) '((x.1) (z.3) ()) "undead-effect: succes-1: set")
  (check-equal? (undead-effect '(set! z.3 (+ x.1 y.2)) '((z.3) ())) '((x.1 y.2) (z.3) ()) "undead-effect: succes-2: binop")
  (check-equal? (undead-effect '(begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2))) '((z.3) ()))
                '(() ((x.1) (x.1 y.2) (z.3)) ())
                "undead-effect: succes-3: simple begin")
  (check-equal? (undead-effect '(if (= x.1 y.2) (set! z.3 (+ x.1 y.2)) (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)))) '((z.3) ()))
                '((x.1 y.2) ((x.1 y.2) (z.3) ((x.1) (x.1 y.2) (z.3))) ())
                "undead-effect: succes-4: if")
    (check-equal? (undead-effect '(if (= x.1 y.2) (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2))) (set! z.3 (+ x.1 y.2))) '((z.3) ()))
                '((x.1 y.2) ((x.1 y.2) ((x.1) (x.1 y.2) (z.3)) (z.3)) ())
                "undead-effect: succes-5: if change")
;undead-tail
  ;succes
  (check-equal? (undead-tail '(halt x.1) '((z.1) ())) '((x.1 z.1) (z.1) ()) "undead-tail: succes-1: halt")
  (check-equal? (undead-tail '(halt x.1) '(())) '((x.1) ()) "undead-tail: succes-2: start halt")
  (check-equal? (undead-tail '(begin (set! x.1 42) (halt x.1)) '(())) '(() ((x.1) ())) "undead-tail: succes-3: simple begin")
  (check-equal? (undead-tail '(begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (halt z.3)) '(()))
                '(() ((x.1) (x.1 y.2) (z.3) ()))
                "undead-tail: succes-4: simple begin")
  (check-equal? (undead-tail '(if (= x.1 y.2) (halt a.4) (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (halt z.3))) '((z.3) ()))
                '((x.1 y.2 a.4 z.3) ((a.4 z.3) (z.3) ((x.1) (x.1 y.2) (z.3) (z.3))) ())
                "undead-effect: succes-5: if")
  (check-equal? (undead-tail '(if (= x.1 y.2) (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (halt z.3)) (halt a.4)) '((z.3) ()))
                '((x.1 y.2 a.4 z.3) ((a.4 z.3) ((x.1) (x.1 y.2) (z.3) (z.3)) (z.3)) ())
                "undead-effect: succes-6: if change")

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
  (check-equal? (undead-analysis '(module ((locals (x.1 y.2 b.3 c.4)))
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
                '(module ((locals (x.1 y.2 b.3 c.4)) (undead-out ((x.1) (x.1 y.2) ((b.3 y.2) (b.3) (b.3 c.4) ((c.4) () ((c.4) ()))))))
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
                "undead-analysis: succes-09: if tail")
  

  
  ;|#
  )