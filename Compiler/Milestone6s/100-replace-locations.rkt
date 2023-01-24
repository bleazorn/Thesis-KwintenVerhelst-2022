#lang racket

(require "common/info.rkt"
         "common/aloc.rkt")
(provide replace-locations)

(module+ test
  (require rackunit))

;
;(replace-triv t assign)
;t: triv?
;assign: list? '((aloc loc) ...)
(define (replace-triv t assign)
  (let ([l (assoc t assign)])
    (cond [l (second l)]
          [else t])))

;
;(replace-pred p assign)->pred?
;p: pred?
;assign: list? '((aloc loc) ...)
(define (replace-pred p assign)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (replace-effect eff assign)) e) ,(replace-pred pred assign))]
    [`(if ,p1 ,p2 ,p3) `(if ,(replace-pred p1 assign) ,(replace-pred p2 assign) ,(replace-pred p3 assign))]
    [`(,relop ,a ,b) `(,relop ,(replace-triv a assign) ,(replace-triv b assign))]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(replace-pred pred assign))]
    [_ #f]))

;
;(replace-effect e assign)->effect?
;e->effect?
;assign: list? '((aloc loc) ...)
(define (replace-effect e assign)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (replace-effect eff assign)) e))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,(replace-triv a assign) (,binop ,(replace-triv b assign) ,(replace-triv c assign)))]
    [`(set! ,a ,b) `(set! ,(replace-triv a assign) ,(replace-triv b assign))]
    [`(setLinear! ,a ,b) `(setLinear! ,(replace-triv a assign) ,(replace-triv b assign))]
    [`(if ,p ,e1 ,e2) `(if ,(replace-pred p assign) ,(replace-effect e1 assign) ,(replace-effect e2 assign))]
    [`(return-point ,l ,t) `(return-point ,(replace-triv l assign) ,(replace-tail t assign))]
    ['(split) '(split)]
    [_ #f]))

;
;(replace-tail t assign)->tail?
;t: tail?
;assign: list? '((aloc loc) ...)
(define (replace-tail t assign)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (replace-effect eff assign)) e) ,(replace-tail tail assign))]
    [`(if ,p ,t1 ,t2) `(if ,(replace-pred p assign) ,(replace-tail t1 assign) ,(replace-tail t2 assign))]
    [`(jump ,trg ,l ...) `(jump ,(replace-triv trg assign))]
    [`(invoke ,a ,b) `(invoke ,(replace-triv a assign) ,(replace-triv b assign))]
    [_ #f]))

;
;(replace-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (replace-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(replace-tail t (getInfo i getAssignment)))]
    [_ #f]))

;Compiles Asm-lang-V2-assignments to Asm-lang-V2-nested, replaced each abstract location with its assigned physical location from the assignment info field.
;(replace-locations p) → Asm-lang-V2-nested?
;p: Asm-lang-V2-assignments?
(define (replace-locations p)
  (match p
    [`(module ,i ,f ... ,pro) `(module ,i ,@(map replace-func f) ,(replace-tail pro (getInfo i getAssignment)))]
    [_ "replace locations failed"]))

(module+ test
  ;#|
;replace-triv
  ;succes
  (check-equal? (replace-triv 'x.1 '((x.1 a0))) 'a0 "replace-triv: succes-1: aloc")
  (check-equal? (replace-triv 5 '((x.1 a0))) 5 "replace-triv: succes-1: integer")
  ;failure
  (check-equal? (replace-triv 'x.1 '((y.1 a0))) 'x.1 "replace-triv: failure-1: aloc not assigned")
  ;(check-equal? (replace-triv 'x.1 '(x.1 a0)) error "replace-triv: failure-2: assign wrong")
;replace-pred
  ;succes
  (check-equal? (replace-pred '(= x.1 y.2) '((x.1 a0) (y.2 a1))) '(= a0 a1) "replace-pred: succes-01: relop")
  (check-equal? (replace-pred '(true) '((x.1 a0) (y.2 a1))) '(true) "replace-pred: succes-02: true")
  (check-equal? (replace-pred '(false) '((x.1 a0) (y.2 a1))) '(false) "replace-pred: succes-03: false")
  (check-equal? (replace-pred '(not (= x.1 y.2)) '((x.1 a0) (y.2 a1))) '(not (= a0 a1)) "replace-pred: succes-04: not")

  (check-equal? (replace-pred '(begin (set! x.1 x.1) (set! y.2 5) (set! x.1 (+ x.1 y.2)) (= x.1 y.2)) '((x.1 a0) (y.2 a1)))
                '(begin (set! a0 a0) (set! a1 5) (set! a0 (+ a0 a1)) (= a0 a1))
                "replace-pred: succes-05: begin")

  (check-equal? (replace-pred '(if (= x.1 y.2) (= x.1 y.2) (= y.2 x.1)) '((x.1 a0) (y.2 a1)))
                '(if (= a0 a1) (= a0 a1) (= a1 a0))
                "replace-pred: succes-06: if")
;replace-effect
  ;succes
  (check-equal? (replace-effect '(set! x.1 y.2) '((x.1 a0) (y.2 a1))) '(set! a0 a1) "replace-effect: succes-1: set")
  (check-equal? (replace-effect '(set! x.1 x.1) '((x.1 a0) (y.2 a1))) '(set! a0 a0) "replace-effect: succes-2: set same trivs")

  (check-equal? (replace-effect '(set! x.1 (+ y.2 z.3)) '((x.1 a0) (y.2 a1) (z.3 a2))) '(set! a0 (+ a1 a2)) "replace-effect: succes-3: binop")
  (check-equal? (replace-effect '(set! x.1 (+ x.1 5)) '((x.1 a0) (y.2 a1))) '(set! a0 (+ a0 5)) "replace-effect: succes-4: binop same trivs")

  (check-equal? (replace-effect '(begin (set! x.1 x.1) (set! y.2 5) (set! x.1 (+ x.1 y.2))) '((x.1 a0) (y.2 a1))) '(begin (set! a0 a0) (set! a1 5) (set! a0 (+ a0 a1))) "replace-effect: succes-5: begin")

  (check-equal? (replace-effect '(if (= x.1 y.2) (set! x.1 y.2) (set! y.2 x.1)) '((x.1 a0) (y.2 a1)))
                '(if (= a0 a1) (set! a0 a1) (set! a1 a0))
                "replace-effect: succes-06: if")
  
;replace-tail
  ;succes
  (check-equal? (replace-tail '(begin (set! x.1 x.1) (set! y.2 5) (set! x.1 (+ x.1 y.2)) (jump x.1)) '((x.1 a0) (y.2 a1)))
                '(begin (set! a0 a0) (set! a1 5) (set! a0 (+ a0 a1)) (jump a0))
                "replace-tail: succes-2: begin")

  (check-equal? (replace-tail '(if (= x.1 y.2) (jump x.1) (jump y.2)) '((x.1 a0) (y.2 a1)))
                '(if (= a0 a1) (jump a0) (jump a1))
                "replace-tail: succes-03: if")

  
  ;failure


  ;replace-locations
  ;succes
  (check-equal? (replace-locations
                 '(module ((locals (x.1)) (assignment ((x.1 rax))))
                    (begin
                      (set! x.1 0)
                      (jump x.1))))
                '(module ((locals (x.1)) (assignment ((x.1 rax))))
                   (begin
                     (set! rax 0)
                     (jump rax)))
                "replace-locations: succes-1: one location")
  (check-equal? (replace-locations
                 '(module ((locals (x.1 y.1 w.1))
                           (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                    (begin
                      (set! x.1 0)
                      (set! y.1 x.1)
                      (set! w.1 (+ w.1 y.1))
                      (jump w.1))))
                '(module ((locals (x.1 y.1 w.1))
                          (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                   (begin
                     (set! rax 0)
                     (set! rbx rax)
                     (set! r9 (+ r9 rbx))
                     (jump r9)))
                "replace-locations: succes-2: multiple locations")
  ;|#
  )
