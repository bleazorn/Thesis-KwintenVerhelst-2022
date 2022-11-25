#lang racket

(require "common/info.rkt"
         "common/register.rkt"
         "common/fvar.rkt"
         "common/aloc.rkt")
(provide assign-registers)

(module+ test
  (require rackunit))

(define nonAssFrameVar "delFrameVar")

(define (isNonAssFrameVar? v)
  (let ([vs (symbol->string v)])
    (cond [(>= (string-length vs) (string-length nonAssFrameVar)) (equal? nonAssFrameVar (substring (symbol->string v) 0 (string-length nonAssFrameVar)))]
          [else #f])))

(define (newNonAssFrameVar)
  (freshAloc nonAssFrameVar))


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
;(getARegister conflictedRegisters)->loc
;conflictedRegisters: list? '(loc ...)
(define (getARegister conflictedRegisters)
  (let ([available (remove* conflictedRegisters '(t0))]);(current-assignable-registers))])
    (if (null? available)
        (newNonAssFrameVar)                           
        (car available))))        

;
;(assign-recur loc conf assign)->list? '((aloc loc) ...)
;loc:list? '(aloc ...)
;conf:list? '((aloc? (...)) ...)
;assign: list? '((aloc loc) ...)
(define (assign-recur loc conf assign confDel)
  ;(println confDel)
  (if (null? loc)
      assign
      (let* ([i (index-of-lowest-conf confDel)]
             [c (list-ref confDel i)]
             [l (car c)]
             [reg (getARegister (getRegFromAssign (second (assoc l conf)) assign))])
        (assign-recur (remove l loc)
                      conf
                      (cons `(,l ,reg) assign)
                      (remove-conf l confDel)))))

(define (deleteSpiled ass)
  (filter-not (lambda (a) (isNonAssFrameVar? (second a))) ass))

;
;(assign-info i)->info?
;i: info?
(define (assign-info i)
  (let ([loc   (getInfo i getLocals)]
        [conf  (getInfo i getConflicts)]
        [ass    (getInfo i getAssignment)])
    (let* ([nonLocConf (filter (lambda (c) (member (car c) loc)) conf)]
           [newAss (assign-recur loc conf ass nonLocConf)]
           [dAss (deleteSpiled newAss)])
      (addInfo (addInfo (addInfo '() (setLocals (remove* (map car dAss) loc)))
                        (setConflicts conf))
               (setAssignment dAss)))))

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
(define (assign-registers p)
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
  
#|
(module+ test
;assign-registers
  ;succes
  (check-equal? (assign-registers '(module ((locals (x.1))
                                            (conflicts ((x.1 ()))))
                                     (begin
                                       (set! x.1 42)
                                       (halt x.1))))
                '(module
                     ((locals (x.1)) (assignment ((x.1 t0))))
                   (begin (set! x.1 42) (halt x.1)))
                "assign-registers: succes-1 one instruction")
  (check-equal? (assign-registers '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                            (conflicts
                                             ((x.3 (z.5 p.1 y.4 v.1 w.2))
                                              (w.2 (z.5 p.1 y.4 v.1 x.3))
                                              (v.1 (w.2 x.3))
                                              (y.4 (t.6 z.5 p.1 w.2 x.3))
                                              (p.1 (t.6 z.5 y.4 w.2 x.3))
                                              (z.5 (t.6 p.1 y.4 w.2 x.3))
                                              (t.6 (z.5 p.1 y.4)))))
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
                      (assignment
                       ((v.1 t0) (t.6 t0) (x.3 t1) (w.2 t2) (y.4 t3) (p.1 t4) (z.5 fv0))))
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
                "assign-registers: succes-2: multiple instructions")
  )
;|#
