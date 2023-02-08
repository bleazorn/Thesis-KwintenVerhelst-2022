#lang racket

(require "common/info.rkt"
         "common/register.rkt"
         "common/fvar.rkt"
         "common/aloc.rkt"
         "langs/asm-pred-lang.rkt"
         "log.rkt")
(provide assign-registers-half)

(module+ test
  (require rackunit))

(define nonAssFrameVar "delFrameVar")

(define (isNonAssFrameVar? v)
  (let ([vs (symbol->string v)])
    (cond [(>= (string-length vs) (string-length nonAssFrameVar)) (equal? nonAssFrameVar (substring (symbol->string v) 0 (string-length nonAssFrameVar)))]
          [else #f])))

(define (newNonAssFrameVar)
  (freshAloc nonAssFrameVar))

(define spilled '())

(define (getSpilled)
  spilled)

(define (addSpilled l)
  (set! spilled (cons l spilled)))

(define (resetSpilled)
  (set! spilled '()))


;
;(getRegFromAssign con assign)->list? '(loc ...)
;con: list? '(aloc ...)
;assign: list? '((aloc loc) ...)
(define (getRegFromAssign con assign)
  (foldl (lambda (c regs) (let ([l (assoc c assign)])
                            (cond [l (cons (second l) regs)]
                                  [else regs])))
         '() con))      

;
;(assign-recur loc conf assign)->list? '((aloc loc) ...)
;loc:list? '(aloc ...)
;conf:list? '((aloc? (...)) ...)
;assign: list? '((aloc loc) ...)
(define (assign-recur loc conf assign confDel)
  ;(logln confDel)
  (cond [(null? loc) assign]
        [else (let* ([i (index-of-lowest-conf confDel)]
                     [l (car (list-ref confDel i))])
                (let ([available (remove* (getRegFromAssign (second (assoc l conf)) assign) (current-assignable-registers))])
                  (cond [(null? available) (addSpilled l)(assign-recur (remove l loc)
                                                                       conf
                                                                       assign
                                                               (remove-conf l confDel))]
                        [else (assign-recur (remove l loc)
                                            conf
                                            (cons `(,l ,(car available)) assign)
                                            (remove-conf l confDel))])))]))

;
;(assign-info i)->info?
;i: info?
(define (assign-info i)
  (resetSpilled)
  (let ([loc   (getInfo i getLocals)]
        [conf  (getInfo i getConflicts)]
        [ass    (getInfo i getAssignment)])
    (let* ([nonLocConf (filter (lambda (c) (member (car c) loc)) conf)]
           [newAss (assign-recur loc conf ass nonLocConf)])
      (addInfo (addInfo i (setLocals (getSpilled)))
               (setAssignment newAss)))))

;
;(assign-func f)->'(define label? info? tail?)   info?: '(locals assignments)
;f: '(define label? info? tail?)
(define (assign-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,(assign-info i) ,t)]
    [_ #f]))
    

;Performs graph-colouring register allocation. The pass attempts to fit each of the abstract location declared in the locals set into a register, and if one cannot be found, assigns it a frame variable instead.
;(assign-registers p) → Asm-lang-V2-assignments?
;p: Asm-lang-V2-conflicts?
(define/contract (assign-registers-half p) (-> asm-pred-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,pro) `(module ,(assign-info i) ,@(map assign-func f) ,pro)]
    [_ #f]))

  
(define testConf '((p.1 (z.5 t.6 y.4 x.3 w.2 v.1))
                   (t.6 (p.1 z.5 v.1))
                   (z.5 (p.1 t.6 w.2 y.4 v.1))
                   (y.4 (z.5 x.3 p.1 w.2))
                   (x.3 (y.4 p.1 w.2))
                   (w.2 (z.5 y.4 p.1 x.3 v.1))
                   (v.1 (w.2 t.6 p.1 z.5))))
;(assign-recur '(v.1 w.2 x.3 y.4 z.5 t.6 p.1) testConf '() testConf)
  
(module+ test
;assign-registers
  ;succes
  (check-equal? (assign-registers-half '(module ((locals (x.1))
                                            (conflicts ((x.1 ())))
                                            (assignment ()))
                                     (begin
                                       (set! x.1 42)
                                       (set! x.1 x.1)
                                       (jump L.foo.4))))
                '(module
                     ((locals ()) (conflicts ((x.1 ())))  (assignment ((x.1 t0))))
                   (begin (set! x.1 42) (set! x.1 x.1) (jump L.foo.4)))
                "assign-registers: succes-1 one instruction")
  (check-equal? (assign-registers-half '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                            (conflicts
                                             ((x.3 (z.5 p.1 y.4 v.1 w.2))
                                              (w.2 (z.5 p.1 y.4 v.1 x.3))
                                              (v.1 (w.2 x.3))
                                              (y.4 (t.6 z.5 p.1 w.2 x.3))
                                              (p.1 (t.6 z.5 y.4 w.2 x.3))
                                              (z.5 (t.6 p.1 y.4 w.2 x.3))
                                              (t.6 (z.5 p.1 y.4))))
                                            (assignment ()))
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
                                       (set! z.5 z.5)
                                       (jump L.foo.4))))
                '(module
                     ((locals (z.5))
                      (conflicts
                       ((x.3 (z.5 p.1 y.4 v.1 w.2))
                        (w.2 (z.5 p.1 y.4 v.1 x.3))
                        (v.1 (w.2 x.3))
                        (y.4 (t.6 z.5 p.1 w.2 x.3))
                        (p.1 (t.6 z.5 y.4 w.2 x.3))
                        (z.5 (t.6 p.1 y.4 w.2 x.3))
                        (t.6 (z.5 p.1 y.4))))
                      (assignment
                       ((p.1 t4) (y.4 t3) (w.2 t2) (x.3 t1) (t.6 t0) (v.1 t0)))
                      )
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
                     (set! z.5 z.5)
                     (jump L.foo.4)))
                "assign-registers: succes-2: multiple instructions")
  )
;|#
