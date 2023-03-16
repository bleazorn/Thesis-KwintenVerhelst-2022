#lang racket

(require "common/info.rkt"
         "common/aloc.rkt"
         "common/fvar.rkt"
         "common/register.rkt"
         "langs/asm-pred-lang.rkt"
         "log.rkt")
(provide undead-analysis)

(module+ test
  (require rackunit))

(define call '())
(define (resetCall)
  (set! call '()))
(define (addCallJump u)
  (set! call (remove-duplicates (append (filter (lambda (u) (and (or (aloc? u) (fvar? u)))) u) call)))
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
     ;(logln (format "rest-let: ~a - ~a" rest-ss undead-outs))
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
    ;(logln (format "pred: ~a" p))
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
      [_ (error (format "undead-analysis:  Failed match.\n No valid pred: ~a" p))])))

;
;(undead-effect e undead-out)->undead-out?
;e->effect?
;undead-outs->undead-out?                                                                                        
(define (undead-effect e undead-outs)
  (let ([undead-out (if (null? undead-outs) '() (car undead-outs))]
        [undead-rest (if (null? undead-outs) '() (cdr undead-outs))])
    ;(logln (format "effect: ~a - ~a" e undead-out))
    (match e
      [`(set! ,a (,binop ,b ,c)) (cons (undead-cons b (undead-cons c (undead-remove a undead-out))) undead-outs)]
      [`(set! ,a ,b) (cons (undead-cons b (undead-remove a undead-out)) undead-outs)]
      [`(setLinear! ,a ,b) (cons (undead-cons b (undead-remove a undead-out)) undead-outs)]
      [`(begin ,e ...) (undead-begin e `(,undead-out) undead-rest)]
      [`(if ,p ,e1 ,e2) (let* ([u1 (undead-effect e1 `(,undead-out))]
                               [u2 (undead-effect e2 `(,undead-out))])
                          (undead-if p u1 u2 undead-rest))]
      [`(return-point ,l ,t) (let* ([uTail (undead-tail t '(()))]
                                    [nextUndead (undead-remove (current-return-value-register) (remove-duplicates (append undead-out (car uTail))))])
                               (addCallReturn nextUndead undead-out)
                               (cons nextUndead (cons (list-set uTail 0 undead-out) undead-rest)))]
      ['(split) (cons undead-out undead-outs)]
      [_ (error (format "undead-analysis:  Failed match.\n No valid effect: ~a" e))])))

;
;(undead-tail t undead-out)->undead-set-tree?
;t->tail?
;undead-outs->undead-set-tree?
(define (undead-tail t undead-outs)
  (let ([undead-out (if (null? undead-outs) '() (car undead-outs))]
        [undead-rest (if (null? undead-outs) '() (cdr undead-outs))])
    ;(logln (format "tail: ~a - ~a" t undead-out))
    (match t
      [`(begin ,e ... ,tail) (let* ([tU (undead-tail tail `(,undead-out))])
                               (undead-begin e tU undead-rest))]
      [`(if ,p ,t1 ,t2) (let* ([u1 (undead-tail t1 `(,undead-out))]    ;'((u) (r))
                               [u2 (undead-tail t2 `(,undead-out))])
                          (undead-if p u1 u2 undead-rest))]
      [`(jump-call ,trg ,l ...) (let ([uJump (cond [(or (aloc? trg) (fvar? trg) (register? trg)) (addCallJump `(,trg)) (cons trg l)]
                                                   [else l])])
                                  (cons uJump (cons l undead-rest)))]
      [`(jump-return ,trg ,l ...) (let ([uJump (cond [(or (aloc? trg) (fvar? trg) (register? trg)) (cons trg l)]
                                                     [else l])])
                                    (cons uJump (cons l undead-rest)))]
      [_ (error (format "undead-analysis:  Failed match.\n No valid tail: ~a" t))])))

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
    [_ (error (format "undead-analysis:  Failed match.\n No valid function: ~a" f))]))


;Performs undeadness analysis, decorating the program with undead-set tree. Only the info field of the program is modified.
;(undead-analysis p) → Asm-lang-V2-undead?
;p:Asm-lang-V2-locals?
(define/contract (undead-analysis p) (-> asm-pred-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,t) `(module ,(undead-info i t) ,@(map undead-func f) ,t)]))


(module+ test
  (define (check-undead? a b m)
    (resetCall)
    (check-equal? a b m))
  ;#|
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
  ;failure
  (check-exn exn:fail? (thunk (undead-pred 'symp1 '((z.3) ()))) "undead-pred: failure-01: no pred")
  ;undead-effect
  ;succes
  (check-undead? (undead-effect '(set! z.3 x.1) '((z.3) ())) '((x.1) (z.3) ()) "undead-effect: succes-01: set")
  (check-undead? (undead-effect '(set! z.3 (+ x.1 y.2)) '((z.3) ())) '((x.1 y.2) (z.3) ()) "undead-effect: succes-02: binop")
  (check-undead? (undead-effect '(begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2))) '((z.3) ()))
                 '(() ((x.1) (x.1 y.2) (z.3)) ())
                 "undead-effect: succes-03: simple begin")
  (check-undead? (undead-effect '(if (= x.1 y.2) (set! z.3 (+ x.1 y.2)) (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)))) '((z.3) ()))
                 '((x.1 y.2) ((x.1 y.2) (z.3) ((x.1) (x.1 y.2) (z.3))) ())
                 "undead-effect: succes-04: if")
  (check-undead? (undead-effect '(if (= x.1 y.2)
                                     (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)))
                                     (set! z.3 (+ x.1 y.2)))
                                '((z.3) ()))
                 '((x.1 y.2)
                   ((x.1 y.2)
                    ((x.1) (x.1 y.2) (z.3))
                    (z.3))
                   ())
                 "undead-effect: succes-05: if change")
  (check-undead? (undead-effect '(return-point
                                  L.rp.6
                                  (begin
                                    (set! nfv.16 new-n.10)
                                    (set! cra L.rp.6)
                                    (jump-return L.fact.4 cfp cra nfv.16)))
                                '((z.3) ()))
                 '((z.3 new-n.10 cfp)
                   ((z.3) ((cfp nfv.16) (cfp cra nfv.16) (cfp cra nfv.16)))
                   ())
                 "undead-effect: succes-06: return-point")
  ;failure
  (check-exn exn:fail? (thunk (undead-effect 'symp1 '((z.3) ()))) "undead-effect: failure-01: no effect")
  ;undead-tail
  ;succes
  (check-undead? (undead-tail '(begin (set! a0 x.1) (jump-return cra)) '((z.1) ())) '((x.1 cra) ((cra) ()) ()) "undead-tail: succes-01: halt")
  (check-undead? (undead-tail '(begin (set! a0 x.1) (jump-call L.foo.4)) '(())) '((x.1) (() ())) "undead-tail: succes-02: start halt")
  (check-undead? (undead-tail '(begin (set! x.1 42) (begin (set! a0 x.1) (jump-call L.foo.4))) '(())) '(() ((x.1) (() ()))) "undead-tail: succes-03: simple begin")
  (check-undead? (undead-tail '(begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (begin (set! a0 z.3) (jump-call L.foo.4))) '(()))
                 '(() ((x.1) (x.1 y.2) (z.3) (() ())))
                 "undead-tail: succes-04: simple begin")
  (check-undead? (undead-tail '(if (= x.1 y.2)
                                   (begin (set! a0 z.3) (jump-return cra))
                                   (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (begin (set! a0 z.3) (jump-return cra))))
                              '((z.3) ()))
                 '((x.1 y.2 z.3 cra)
                   ((z.3 cra)
                    ((cra) ())
                    ((x.1 cra) (x.1 y.2 cra) (z.3 cra) ((cra) ())))
                   ())
                 "undead-tail: succes-05: if")
  
  (check-undead? (undead-tail '(if (= x.1 y.2)
                                   (begin (set! x.1 5) (set! y.2 6) (set! z.3 (+ x.1 y.2)) (begin (set! a0 z.3) (jump-return cra)))
                                   (begin (set! a0 z.3) (jump-return cra)))
                              '((z.3) ()))
                 '((x.1 y.2 cra z.3)
                   ((cra z.3)
                    ((x.1 cra) (x.1 y.2 cra) (z.3 cra) ((cra) ()))
                    ((cra) ()))
                   ())
                 "undead-effect: succes-06: if change")
  (check-undead? (undead-tail '(jump-call L.foo.4) '((z.1) ())) '(() () ()) "undead-tail: succes-07: jump-call")
  (check-undead? (undead-tail '(jump-return cra) '((z.1) ())) '((cra) () ()) "undead-tail: succes-08: jump-return")
  ;failure
  (check-exn exn:fail? (thunk (undead-tail 'symp1 '((z.3) ()))) "undead-tail: failure-01: no tail")
  ;undead-func
  ;succes
  (check-equal? (undead-func '(define L.odd.1
                                ((locals (tmp-ra.1 x.1 y.2 z.3 tmp.1)) (new-frames ()) (paramSize 0))
                                (begin
                                  (set! tmp-ra.1 cra)
                                  (begin
                                    (set! x.1 a1)
                                    (set! y.2 a2)
                                    (set! z.3 a3)
                                    (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                    (jump-return cra cfp)))))
                '(define L.odd.1
                   ((undead-out
                     ((a1 a2 a3 cra cfp)
                      ((a2 a3 cra cfp)
                       (a3 cra cfp)
                       (cra cfp)
                       ((tmp.1 cra cfp) (cra cfp))
                       (cfp))))
                    (call-undead ())
                    (locals (tmp-ra.1 x.1 y.2 z.3 tmp.1))
                    (new-frames ())
                    (paramSize 0))
                   (begin
                     (set! tmp-ra.1 cra)
                     (begin
                       (set! x.1 a1)
                       (set! y.2 a2)
                       (set! z.3 a3)
                       (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                       (jump-return cra cfp))))
                "undead-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (undead-func '(defdine L.odd.1
                                              ((locals (tmp-ra.1 x.1 y.2 z.3 tmp.1)) (new-frames ()) (paramSize 0))
                                              (begin
                                                (set! tmp-ra.1 cra)
                                                (begin
                                                  (set! x.1 a1)
                                                  (set! y.2 a2)
                                                  (set! z.3 a3)
                                                  (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                                  (jump-return cra cfp))))))
             "undead-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (undead-func '(define
                                              ((locals (tmp-ra.1 x.1 y.2 z.3 tmp.1)) (new-frames ()) (paramSize 0))
                                              (begin
                                                (set! tmp-ra.1 cra)
                                                (begin
                                                  (set! x.1 a1)
                                                  (set! y.2 a2)
                                                  (set! z.3 a3)
                                                  (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                                  (jump-return cra cfp))))))
             "undead-func: failure-02: no name")
  (check-exn exn:fail? (thunk (undead-func '(define L.odd.1
                                              (begin
                                                (set! tmp-ra.1 cra)
                                                (begin
                                                  (set! x.1 a1)
                                                  (set! y.2 a2)
                                                  (set! z.3 a3)
                                                  (begin (set! tmp.1 5) (set! x.1 (+ tmp.1 tmp.1)))
                                                  (jump-return cra cfp))))))
             "undead-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (undead-func '(define L.odd.1
                                              ((locals (tmp-ra.1 x.1 y.2 z.3 tmp.1)) (new-frames ()) (paramSize 0)))))
             "undead-func: failure-04:  no tail")
  #|
;undead-analysis
  ;succes
  (check-undead? (undead-analysis '(module ((locals (x.1)))
                                    (begin
                                      (set! x.1 42)
                                      (begin (set! a0 x.1) (jump-call L.foo.4)))))
                '(module
                     ((undead-out ((x.1) (() ()))) (call-undead ()) (locals (x.1)))
                   (begin (set! x.1 42) (begin (set! a0 x.1) (jump-call L.foo.4))))
                "undead-analysis: succes-01: one instruction")
  (check-undead? (undead-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
                                      (begin (set! a0 z.5) (jump-call L.foo.4)))))
                '(module
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
                     (begin (set! a0 z.5) (jump-call L.foo.4))))
                "undead-analysis: succes-02: multiple instructions")
  (check-undead? (undead-analysis '(module ((locals (x.1 y.1)))
                                    (begin
                                      (set! y.1 42)
                                      (set! x.1 5)
                                      (begin (set! a0 x.1) (jump-call L.foo.4)))))
                '(module
                     ((undead-out (() (x.1) (() ()))) (call-undead ()) (locals (x.1 y.1)))
                   (begin (set! y.1 42) (set! x.1 5) (begin (set! a0 x.1) (jump-call L.foo.4))))
                "undead-analysis: succes-03: unused variable")
  (check-undead? (undead-analysis '(module ((locals (x.1 y.1)))
                                    (begin
                                      (set! x.1 5)
                                      (set! y.1 42)
                                      (begin (set! a0 x.1) (jump-return L.foo.4)))))
                '(module
                     ((undead-out ((x.1) (x.1) (() ()))) (call-undead ()) (locals (x.1 y.1)))
                   (begin (set! x.1 5) (set! y.1 42) (begin (set! a0 x.1) (jump-return L.foo.4))))
                "undead-analysis: succes-04: unused variable")
    (check-undead? (undead-analysis '(module ((locals (x.1)))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 x.1)
                                          (begin
                                            (set! z.3 (+ y.2 x.1))
                                            (set! x.1 z.3)))
                                        (begin (set! a0 x.1) (jump-return L.foo.4)))))
                '(module
                     ((undead-out ((x.1) ((y.2 x.1) ((z.3) (x.1))) (() ()))) (call-undead ()) (locals (x.1)))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 x.1)
                       (begin
                         (set! z.3 (+ y.2 x.1))
                         (set! x.1 z.3)))
                     (begin (set! a0 x.1) (jump-return L.foo.4))))
                "undead-analysis: succes-05: begin effect instruction")
  (check-undead? (undead-analysis '(module ((locals (x.1 y.2 z.3)))
                                      (begin
                                        (set! x.1 42)
                                        (begin
                                          (set! y.2 (+ x.1 50))
                                          (set! x.1 y.2))
                                        (begin
                                            (set! z.3 (+ x.1 x.1))
                                            (set! x.1 z.3))
                                        (begin (set! a0 x.1) (jump-return L.foo.4)))))
                
                '(module
                     ((undead-out ((x.1) ((y.2) (x.1)) ((z.3) (x.1)) (() ()))) (call-undead ()) (locals (x.1 y.2 z.3)))
                   (begin
                     (set! x.1 42)
                     (begin
                       (set! y.2 (+ x.1 50))
                       (set! x.1 y.2))
                     (begin
                       (set! z.3 (+ x.1 x.1))
                       (set! x.1 z.3))
                     (begin (set! a0 x.1) (jump-return L.foo.4))))
                "undead-analysis: succes-06: sequential begin effects")
  (check-undead? (undead-analysis '(module ((locals (x.1 y.2 z.3 a.4 b.5)))
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
                                      (begin (set! a0 x.1) (jump-return L.foo.4)))))           
                '(module
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
                     (begin (set! a0 x.1) (jump-return L.foo.4))))
                "undead-analysis: succes-07: sequential and nested begin effects")
  (check-undead? (undead-analysis '(module ((locals (x.1 y.2 z.3 a.4 b.5)))
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
                                          (begin (set! a0 x.1) (jump-return L.foo.4)))))))           
                '(module
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
                         (begin (set! a0 x.1) (jump-return L.foo.4))))))
                "undead-analysis: succes-08: sequential and nested begin effects with tail in nested")
  (check-undead? (undead-analysis '(module
                                       ((locals (x.1 y.2 b.3 c.4)))
                                    (begin
                                      (set! x.1 5)
                                      (set! y.2 x.1)
                                      (begin
                                        (set! b.3 x.1)
                                        (set! b.3 (+ b.3 y.2))
                                        (set! c.4 b.3)
                                        (if (= c.4 b.3)
                                            (begin (set! a0 c.4) (jump-return L.foo.4))
                                            (begin
                                              (set! x.1 c.4)
                                              (begin (set! a0 c.4) (jump-return L.foo.4))))))))                 
                '(module
                     ((undead-out ((x.1) (x.1 y.2) ((b.3 y.2) (b.3) (b.3 c.4) ((c.4) (() ()) ((c.4) (() ())))))) (call-undead ()) (locals (x.1 y.2 b.3 c.4)))
                   (begin
                     (set! x.1 5)
                     (set! y.2 x.1)
                     (begin
                       (set! b.3 x.1)
                       (set! b.3 (+ b.3 y.2))
                       (set! c.4 b.3)
                       (if (= c.4 b.3)
                           (begin (set! a0 c.4) (jump-return L.foo.4))
                           (begin
                             (set! x.1 c.4)
                             (begin (set! a0 c.4) (jump-return L.foo.4)))))))
                "undead-analysis: succes-09: if tail")
  (check-undead? (undead-analysis '(module
                                       ((new-frames ())
                                        (locals (tmp-ra.2)))
                                     (define L.swap.1
                                       ((new-frames (()))
                                        (locals (z.3 tmp-ra.1 x.1 y.2)))
                                       (begin
                                         (set! tmp-ra.1 cra)
                                         (set! x.1 rdi)
                                         (set! y.2 rsi)
                                         (if (< y.2 x.1)
                                             (begin (set! a0 x.1) (jump-return tmp-ra.1 cfp a0))
                                             (begin
                                               (return-point L.rp.1
                                                             (begin
                                                               (set! rsi x.1)
                                                               (set! rdi y.2)
                                                               (set! cra L.rp.1)
                                                               (jump-call L.swap.1 cfp cra rdi rsi)))
                                               (set! z.3 a0)
                                               (set! a0 z.3)
                                               (jump-return tmp-ra.1 cfp a0)))))
                                     (begin
                                       (set! tmp-ra.2 cra)
                                       (set! rsi 2)
                                       (set! rdi 1)
                                       (set! cra tmp-ra.2)
                                       (jump-call L.swap.1 cfp cra rdi rsi))))                 
                 '(module
                      ((undead-out
                        ((tmp-ra.2 cfp)
                         (tmp-ra.2 cfp rsi)
                         (tmp-ra.2 cfp rdi rsi)
                         (cfp cra rdi rsi)
                         (cfp cra rdi rsi)))
                       (call-undead ())
                       (new-frames ())
                       (locals (tmp-ra.2)))
                    (define L.swap.1
                      ((undead-out
                        ((rdi rsi tmp-ra.1 cfp)
                         (rsi x.1 tmp-ra.1 cfp)
                         (x.1 tmp-ra.1 cfp y.2)
                         ((x.1 tmp-ra.1 cfp y.2)
                          ((tmp-ra.1 cfp a0) (cfp a0))
                          (((a0 tmp-ra.1 cfp)
                            ((y.2 cfp rsi) (cfp rdi rsi) (cfp cra rdi rsi) (cfp cra rdi rsi)))
                           (z.3 tmp-ra.1 cfp)
                           (tmp-ra.1 cfp a0)
                           (cfp a0)))))
                       (call-undead (tmp-ra.1))
                       (new-frames (()))
                       (locals (z.3 tmp-ra.1 x.1 y.2)))
                      (begin
                        (set! tmp-ra.1 cra)
                        (set! x.1 rdi)
                        (set! y.2 rsi)
                        (if (< y.2 x.1)
                            (begin (set! a0 x.1) (jump-return tmp-ra.1 cfp a0))
                            (begin
                              (return-point L.rp.1
                                            (begin
                                              (set! rsi x.1)
                                              (set! rdi y.2)
                                              (set! cra L.rp.1)
                                              (jump-call L.swap.1 cfp cra rdi rsi)))
                              (set! z.3 a0)
                              (set! a0 z.3)
                              (jump-return tmp-ra.1 cfp a0)))))
                    (begin
                      (set! tmp-ra.2 cra)
                      (set! rsi 2)
                      (set! rdi 1)
                      (set! cra tmp-ra.2)
                      (jump-call L.swap.1 cfp cra rdi rsi)))
                 "undead-analysis: succes-10: return call")
  (check-undead? (undead-analysis '(module
                                       ((new-frames ())
                                        (locals (tmp-ra.6)))
                                     (define L.swap.1
                                       ((new-frames ((nfv.4 nfv.5)))
                                        (locals (nfv.4 nfv.5 z.3 tmp-ra.3 x.1 y.2)))
                                       (begin
                                         (set! tmp-ra.3 cra)
                                         (set! x.1 fv0)
                                         (set! y.2 fv1)
                                         (if (< y.2 x.1)
                                             (begin (set! a0 x.1) (jump-return tmp-ra.3 cfp a0))
                                             (begin
                                               (return-point L.rp.2
                                                             (begin
                                                               (set! nfv.5 x.1)
                                                               (set! nfv.4 y.2)
                                                               (set! cra L.rp.2)
                                                               (jump-call L.swap.1 cfp cra nfv.4 nfv.5)))
                                               (set! z.3 a0)
                                               (set! a0 z.3)
                                               (jump-return tmp-ra.3 cfp a0)))))
                                     (begin
                                       (set! tmp-ra.6 cra)
                                       (set! fv1 2)
                                       (set! fv0 1)
                                       (set! cra tmp-ra.6)
                                       (jump-call L.swap.1 cfp cra fv0 fv1))))                 
                 '(module
                      ((undead-out
                        ((tmp-ra.6 cfp)
                         (tmp-ra.6 cfp fv1)
                         (tmp-ra.6 cfp fv0 fv1)
                         (cfp cra fv0 fv1)
                         (cfp cra fv0 fv1)))
                       (call-undead ())
                       (new-frames ())
                       (locals (tmp-ra.6)))
                    (define L.swap.1
                      ((undead-out
                        ((fv0 fv1 tmp-ra.3 cfp)
                         (fv1 x.1 tmp-ra.3 cfp)
                         (x.1 tmp-ra.3 cfp y.2)
                         ((x.1 tmp-ra.3 cfp y.2)
                          ((tmp-ra.3 cfp a0) (cfp a0))
                          (((a0 tmp-ra.3 cfp)
                            ((y.2 cfp nfv.5)
                             (cfp nfv.4 nfv.5)
                             (cfp cra nfv.4 nfv.5)
                             (cfp cra nfv.4 nfv.5)))
                           (z.3 tmp-ra.3 cfp)
                           (tmp-ra.3 cfp a0)
                           (cfp a0)))))
                       (call-undead (tmp-ra.3))
                       (new-frames ((nfv.4 nfv.5)))
                       (locals (nfv.4 nfv.5 z.3 tmp-ra.3 x.1 y.2)))
                      (begin
                        (set! tmp-ra.3 cra)
                        (set! x.1 fv0)
                        (set! y.2 fv1)
                        (if (< y.2 x.1)
                            (begin (set! a0 x.1) (jump-return tmp-ra.3 cfp a0))
                            (begin
                              (return-point L.rp.2
                                            (begin
                                              (set! nfv.5 x.1)
                                              (set! nfv.4 y.2)
                                              (set! cra L.rp.2)
                                              (jump-call L.swap.1 cfp cra nfv.4 nfv.5)))
                              (set! z.3 a0)
                              (set! a0 z.3)
                              (jump-return tmp-ra.3 cfp a0)))))
                    (begin
                      (set! tmp-ra.6 cra)
                      (set! fv1 2)
                      (set! fv0 1)
                      (set! cra tmp-ra.6)
                      (jump-call L.swap.1 cfp cra fv0 fv1)))
                 "undead-analysis: succes-11: return call")
  (check-undead? (undead-analysis '(module
                                       ((new-frames ())
                                        (locals (ra.12)))
                                     (define L.fact.4
                                       ((new-frames ((nfv.16)))
                                        (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17)))
                                       (begin
                                         (set! x.9 fv0)
                                         (set! ra.13 cra)
                                         (if (= x.9 0)
                                             (begin (set! a0 1) (jump-return ra.13 cfp a0))
                                             (begin
                                               (set! tmp.14 -1)
                                               (set! tmp.15 x.9)
                                               (set! tmp.15 (+ tmp.15 tmp.14))
                                               (set! new-n.10 tmp.15)
                                               (return-point L.rp.6
                                                             (begin
                                                               (set! nfv.16 new-n.10)
                                                               (set! cra L.rp.6)
                                                               (jump-call L.fact.4 cfp cra nfv.16)))
                                               (set! factn-1.11 a0)
                                               (set! tmp.17 x.9)
                                               (set! tmp.17 (* tmp.17 factn-1.11))
                                               (set! a0 tmp.17)
                                               (jump-return ra.13 cfp a0)))))
                                     (begin
                                       (set! ra.12 cra)
                                       (set! fv0 5)
                                       (set! cra ra.12)
                                       (jump-call L.fact.4 cfp cra fv0))))                 
                 '(module
                      ((undead-out
                        ((ra.12 cfp) (ra.12 cfp fv0) (cfp cra fv0) (cfp cra fv0)))
                       (call-undead ())
                       (new-frames ())
                       (locals (ra.12)))
                    (define L.fact.4
                      ((undead-out
                        ((cra cfp x.9)
                         (ra.13 cfp x.9)
                         ((ra.13 cfp x.9)
                          ((ra.13 cfp a0) (cfp a0))
                          ((tmp.14 x.9 ra.13 cfp)
                           (tmp.15 tmp.14 x.9 ra.13 cfp)
                           (tmp.15 x.9 ra.13 cfp)
                           (x.9 ra.13 cfp new-n.10)
                           ((a0 x.9 ra.13 cfp) ((cfp nfv.16) (cfp cra nfv.16) (cfp cra nfv.16)))
                           (x.9 factn-1.11 ra.13 cfp)
                           (tmp.17 factn-1.11 ra.13 cfp)
                           (tmp.17 ra.13 cfp)
                           (ra.13 cfp a0)
                           (cfp a0)))))
                       (call-undead (x.9 ra.13))
                       (new-frames ((nfv.16)))
                       (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17)))
                      (begin
                        (set! x.9 fv0)
                        (set! ra.13 cra)
                        (if (= x.9 0)
                            (begin (set! a0 1) (jump-return ra.13 cfp a0))
                            (begin
                              (set! tmp.14 -1)
                              (set! tmp.15 x.9)
                              (set! tmp.15 (+ tmp.15 tmp.14))
                              (set! new-n.10 tmp.15)
                              (return-point L.rp.6
                                            (begin
                                              (set! nfv.16 new-n.10)
                                              (set! cra L.rp.6)
                                              (jump-call L.fact.4 cfp cra nfv.16)))
                              (set! factn-1.11 a0)
                              (set! tmp.17 x.9)
                              (set! tmp.17 (* tmp.17 factn-1.11))
                              (set! a0 tmp.17)
                              (jump-return ra.13 cfp a0)))))
                    (begin
                      (set! ra.12 cra)
                      (set! fv0 5)
                      (set! cra ra.12)
                      (jump-call L.fact.4 cfp cra fv0)))
                 "undead-analysis: succes-12: return call")
  ;|#
  )
