#lang racket

(require "common/info.rkt"
         "common/register.rkt"
         "common/fvar.rkt"
         "common/aloc.rkt"
         "langs/asm-pred-lang.rkt"
         "log.rkt")
(provide assign-registers-full)

(module+ test
  (require rackunit))

;
;(get-conflicted-assignments con assign)->list? '(loc ...)
;con: list? '(aloc ...)
;assign: list? '((aloc loc) ...)
(define (get-conflicted-assignments con assign)
  (map second (filter (lambda (a) (member (car a) con)) assign)))

;
;(get-assignment conflictedRegisters)->loc
;conflictedRegisters: list? '(loc ...)
(define (get-assignment conflicted-assignments)
  (let ([conflicted-registers (filter register? conflicted-assignments)]
        [conflicted-fvars (filter fvar? conflicted-assignments)])
    (let ([available (remove* conflicted-registers (current-assignable-registers))])
      (if (null? available)
          (getFirstAvailableFvar conflicted-fvars)
          (car available)))))

;
;(assign-recur loc conf assign)->list? '((aloc loc) ...)
;loc:list? '(aloc ...)
;conf:list? '((aloc? (...)) ...)
;assign: list? '((aloc loc) ...)
(define (assign-recur loc conf assign confDel already-assigned-fvars)
  ;(logln confDel)
  (if (null? loc)
      '()
      (let* ([i (index-of-lowest-conf confDel)]
             [c (list-ref confDel i)]
             [l (car c)]
             [conflicted-assignments (append (get-conflicted-assignments (second (assoc l conf)) assign) already-assigned-fvars)]
             [new-loc (get-assignment conflicted-assignments)])
        (cons `(,l ,new-loc) (assign-recur (remove l loc)
                                       conf
                                       (cons `(,l ,new-loc) assign)
                                       (remove-conf l confDel)
                                       already-assigned-fvars)))))

;
;(assign-info i)->info?
;i: info?
(define (assign-info i)
  (let ([frames (getInfo i getNewFrames)]
        [locs   (getInfo i getLocals)]
        [calls  (getInfo i getCallUndead)]
        [confs  (getInfo i getConflicts)]
        [allFva (getInfo i getAllocatedFvars)])
    ;remove new-frames and call-undead locs from getting assigned
    (let* ([frameLocs (foldl (lambda (f ls) (append ls (second f))) '() frames)]
           [assLocs (remove* (append frameLocs calls) locs)]                                        ;framelocs: allocated frames   calls: call-undead-variable
           [all-assigned-fvars (remove-duplicates (append (filter fvar? (map car confs)) allFva))])
      (let* ([nonLocConf (filter (lambda (c) (member (car c) assLocs)) confs)]
             [newAss (assign-recur assLocs confs '() nonLocConf all-assigned-fvars)])
        (addInfo i (setAssignment newAss))))))

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
(define/contract (assign-registers-full p) (-> asm-pred-lang? asm-pred-lang?)
  (match p
    [`(module ,i ,f ... ,pro) `(module ,(assign-info i) ,@(map assign-func f) ,pro)]
    [_ #f]))
  
(module+ test
;assign-registers
  ;succes
  (check-equal? (assign-registers-full '(module ((locals (x.1))
                                            (conflicts ((x.1 ())))
                                            (new-frames ())
                                            (call-undead ())
                                            (allocatedFvars ()))
                                     (begin
                                       (set! x.1 42)
                                       (set! x.1 x.1)
                                       (jump-return L.foo.4))))
                '(module
                     ((assignment ((x.1 t0))) (locals (x.1)) (conflicts ((x.1 ()))) (new-frames ()) (call-undead ()) (allocatedFvars ()))
                   (begin (set! x.1 42) (set! x.1 x.1) (jump-return L.foo.4)))
                "assign-registers: succes-1 one instruction")
  (check-equal? (assign-registers-full '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                            (conflicts
                                             ((x.3 (z.5 p.1 y.4 v.1 w.2))
                                              (w.2 (z.5 p.1 y.4 v.1 x.3))
                                              (v.1 (w.2 x.3))
                                              (y.4 (t.6 z.5 p.1 w.2 x.3))
                                              (p.1 (t.6 z.5 y.4 w.2 x.3))
                                              (z.5 (t.6 p.1 y.4 w.2 x.3))
                                              (t.6 (z.5 p.1 y.4))))
                                             (new-frames ())
                                             (call-undead ())
                                             (allocatedFvars ()))
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
                                       (jump-call L.foo.4))))
                '(module
                     ((assignment
                       ((v.1 t0) (t.6 t0) (x.3 t1) (w.2 t2) (y.4 t3) (p.1 t4) (z.5 fv0)))
                      (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                      (conflicts
                       ((x.3 (z.5 p.1 y.4 v.1 w.2))
                        (w.2 (z.5 p.1 y.4 v.1 x.3))
                        (v.1 (w.2 x.3))
                        (y.4 (t.6 z.5 p.1 w.2 x.3))
                        (p.1 (t.6 z.5 y.4 w.2 x.3))
                        (z.5 (t.6 p.1 y.4 w.2 x.3))
                        (t.6 (z.5 p.1 y.4))))
                      (new-frames ())
                      (call-undead ())
                      (allocatedFvars ()))
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
                     (jump-call L.foo.4)))
                "assign-registers: succes-2: multiple instructions")
  )
;|#