#lang racket

(require "langs/nested-asm-lang-jumps.rkt")

(provide clean-registers)

(module+ test
  (require rackunit))

;
;(clean-pred p assign)->pred?
;p: pred?
;clean-regs-call clean-regs-return: list? '(register? ...)
(define (clean-pred p clean-regs-call clean-regs-return)
  (match p
    [`(begin ,e ... ,pred) `(begin ,@(map (lambda (eff) (clean-effect eff clean-regs-call clean-regs-return)) e) ,(clean-pred pred clean-regs-call clean-regs-return))]
    [`(if ,p1 ,p2 ,p3) `(if ,(clean-pred p1 clean-regs-call clean-regs-return) ,(clean-pred p2 clean-regs-call clean-regs-return) ,(clean-pred p3 clean-regs-call clean-regs-return))]
    [`(,relop ,a ,b) `(,relop ,a ,b)]
    ['(true) '(true)]
    ['(false) '(false)]
    [`(not ,pred) `(not ,(clean-pred pred clean-regs-call clean-regs-return))]
    [_ (error (format "clean-registers:  Failed match.\n No valid pred: ~a" p))]))

;
;(clean-effect e assign)->effect?
;e->effect?
;clean-regs-call clean-regs-return: list? '(register? ...)
(define (clean-effect e clean-regs-call clean-regs-return)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (clean-effect eff clean-regs-call clean-regs-return)) e))]
    [`(if ,p ,e1 ,e2) `(if ,(clean-pred p clean-regs-call clean-regs-return) ,(clean-effect e1 clean-regs-call clean-regs-return) ,(clean-effect e2 clean-regs-call clean-regs-return))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,a (,binop ,b ,c))]
    [`(set! ,a ,b) `(set! ,a ,b)]
    [`(setLinear! ,a ,b) `(setLinear! ,a ,b)]
    [`(seal ,r ... ,s) `(seal ,@r ,s)]
    [`(unseal ,r ... ,s) `(unseal ,@r ,s)]
    [`(split ,a ,b ,c ,d) `(split ,a ,b ,c ,d)]
    [`(splice ,a ,b ,c ,d) `(splice ,a ,b ,c ,d)]
    [`(return-point ,l ,t) `(return-point ,l ,(clean-tail t clean-regs-call clean-regs-return))]
    [_ (error (format "clean-registers:  Failed match.\n No valid effect: ~a" e))]))

;
;(clean-tail t assign)->tail?
;t: tail?
;clean-regs-call clean-regs-return: list? '(register? ...)
(define (clean-tail t clean-regs-call clean-regs-return)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (clean-effect eff clean-regs-call clean-regs-return)) e) ,(clean-tail tail clean-regs-call clean-regs-return))]
    [`(if ,p ,t1 ,t2) `(if ,(clean-pred p clean-regs-call clean-regs-return) ,(clean-tail t1 clean-regs-call clean-regs-return) ,(clean-tail t2 clean-regs-call clean-regs-return))]
    [`(jump-call ,trg) `(begin ,@(map (lambda (r) `(set! ,r 0)) clean-regs-call)
                               (jump-call ,trg))]
    [`(jump-return ,trg) `(begin ,@(map (lambda (r) `(set! ,r 0)) clean-regs-return)
                                 (jump-return ,trg))]
    [`(invoke ,a ,b) `(begin ,@(map (lambda (r) `(set! ,r 0)) clean-regs-return)
                             (invoke ,a ,b))]
    [_ (error (format "clean-registers:  Failed match.\n No valid tail: ~a" t))]))

;
;(clean-info t i)->tail?
;t: tail?
;i: info?
(define (clean-info t i)
  (let ([clean-regs-call '(t0 t1 t2 t3 t4 t5 t6)]
        [clean-regs-return '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6)])
    (clean-tail t clean-regs-call clean-regs-return)))
      

;
;(clean-func f)->'(define label? tail?)
;f: '(define label? info? tail?)
(define (clean-func f)
  (match f
    [`(define ,l ,i ,t) `(define ,l ,i ,(clean-info t i))]
    [_ (error (format "clean-registers:  Failed match.\n No valid function: ~a" f))]))


(define/contract (clean-registers p) (-> nested-asm-lang-jumps? nested-asm-lang-jumps?)
  (match p
    [`(module ,i ,f ... ,t) p]; `(module ,i ,@(map clean-func f) ,(clean-info t i))]
    ))

(module+ test
  ;clean-pred
  ;succes
  (check-equal? (clean-pred '(true)
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(true)
                "clean-pred: succes-01: true")
  (check-equal? (clean-pred '(false)
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(false)
                "clean-pred: succes-02: false")
  (check-equal? (clean-pred '(= t0 t1)
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(= t0 t1)
                "clean-pred: succes-03: relop")

  (check-equal? (clean-pred '(begin (set! t1 5) (true))
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(begin (set! t1 5) (true))
                "clean-pred: succes-04: begin")
  (check-equal? (clean-pred '(if (= t0 t1) (true) (false))
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(if (= t0 t1) (true) (false))
                "clean-pred: succes-05: if")
  (check-equal? (clean-pred '(not (= t1 t0))
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(not (= t1 t0))
                "clean-pred: succes-06: not")
  ;failure
  (check-exn exn:fail? (thunk (clean-pred '+ 
                                          '(t0 t1 t2 t3 t4 t5 t6)
                                          '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6)))
             "clean-pred: failure-01: wrong pred")
  ;clean-effect
  ;succes
  (check-equal? (clean-effect '(set! t0 (+ t1 t2))
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(set! t0 (+ t1 t2))
                "clean-effect: succes-01: binop")
  (check-equal? (clean-effect '(set! t0 5)
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(set! t0 5)
                "clean-effect: succes-02: set")
  (check-equal? (clean-effect '(begin (set! t0 5) (set! cfp cra))
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(begin (set! t0 5) (set! cfp cra))
                "clean-effect: succes-03: begin")
  (check-equal? (clean-effect '(if (true) (set! t0 5) (set! t0 6))
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(if (true) (set! t0 5) (set! t0 6))
                "clean-effect: succes-04: if")
  (check-equal? (clean-effect '(return-point
                                L.rpLabel.1
                                (begin
                                  (set! a1 2)
                                  (set! a2 1)
                                  (set! cra L.rpLabel.1)
                                  (jump-call L.swap.1)))
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(return-point
                  L.rpLabel.1
                  (begin
                    (set! a1 2)
                    (set! a2 1)
                    (set! cra L.rpLabel.1)
                    (begin
                      (set! t0 0)
                      (set! t1 0)
                      (set! t2 0)
                      (set! t3 0)
                      (set! t4 0)
                      (set! t5 0)
                      (set! t6 0)
                      (jump-call L.swap.1))))
                "clean-effect: succes-05: return point")
  (check-equal? (clean-effect '(setLinear! cra cfp)
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(setLinear! cra cfp)
                "clean-effect: succes-06: setLinear!")
  (check-equal? (clean-effect '(seal cra cfp 15)
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(seal cra cfp 15)
                "clean-effect: succes-07: seal")
  (check-equal? (clean-effect '(unseal cra cfp 15)
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(unseal cra cfp 15)
                "clean-effect: succes-08: unseal")
  (check-equal? (clean-effect '(split csp csp cfp 64)
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(split csp csp cfp 64)
                "clean-effect: succes-09: split")
  (check-equal? (clean-effect '(splice csp csp cfp 64)
                              '(t0 t1 t2 t3 t4 t5 t6)
                              '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(splice csp csp cfp 64)
                "clean-effect: succes-10: splice")
  ;failure
  (check-exn exn:fail? (thunk (clean-effect '+
                                            '(t0 t1 t2 t3 t4 t5 t6)
                                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6)))
             "clean-effect: failure-01: wrong effect")
  ;clean-tail
  ;succes
  (check-equal? (clean-tail '(jump-call L.swap.1)
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(begin
                   (set! t0 0)
                   (set! t1 0)
                   (set! t2 0)
                   (set! t3 0)
                   (set! t4 0)
                   (set! t5 0)
                   (set! t6 0)
                   (jump-call L.swap.1))
                "clean-tail: succes-01: jump call")
  (check-equal? (clean-tail '(jump-return cra)
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(begin
                   (set! a1 0)
                   (set! a2 0)
                   (set! a3 0)
                   (set! a4 0)
                   (set! t0 0)
                   (set! t1 0)
                   (set! t2 0)
                   (set! t3 0)
                   (set! t4 0)
                   (set! t5 0)
                   (set! t6 0)
                   (jump-return cra))
                "clean-tail: succes-02:jump return")
  (check-equal? (clean-tail '(begin (set! cra fv0) (jump-return cra))
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(begin
                   (set! cra fv0)
                   (begin
                     (set! a1 0)
                     (set! a2 0)
                     (set! a3 0)
                     (set! a4 0)
                     (set! t0 0)
                     (set! t1 0)
                     (set! t2 0)
                     (set! t3 0)
                     (set! t4 0)
                     (set! t5 0)
                     (set! t6 0)
                     (jump-return cra)))
                "clean-tail: succes-03: begin")
  (check-equal? (clean-tail '(if (= t0 t1) (jump-return cra) (jump-call L.swap.1))
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(if (= t0 t1)
                     (begin
                       (set! a1 0)
                       (set! a2 0)
                       (set! a3 0)
                       (set! a4 0)
                       (set! t0 0)
                       (set! t1 0)
                       (set! t2 0)
                       (set! t3 0)
                       (set! t4 0)
                       (set! t5 0)
                       (set! t6 0)
                       (jump-return cra))
                     (begin
                       (set! t0 0)
                       (set! t1 0)
                       (set! t2 0)
                       (set! t3 0)
                       (set! t4 0)
                       (set! t5 0)
                       (set! t6 0)
                       (jump-call L.swap.1)))
                "clean-tail: succes-04: if")
  (check-equal? (clean-tail '(invoke cra cfp)
                            '(t0 t1 t2 t3 t4 t5 t6)
                            '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6))
                '(begin
                   (set! a1 0)
                   (set! a2 0)
                   (set! a3 0)
                   (set! a4 0)
                   (set! t0 0)
                   (set! t1 0)
                   (set! t2 0)
                   (set! t3 0)
                   (set! t4 0)
                   (set! t5 0)
                   (set! t6 0)
                   (invoke cra cfp))
                "clean-tail: succes-05: invoke")
  ;failure
  (check-exn exn:fail? (thunk (clean-tail '+
                                          '(t0 t1 t2 t3 t4 t5 t6)
                                          '(a1 a2 a3 a4 t0 t1 t2 t3 t4 t5 t6)))
             "clean-tail: failure-01: wrong tail")
  ;clean-func
  ;succes
  (check-equal? (clean-func '(define L.odd.1
                               ((frameSize 8) (paramSize 0))
                               (begin
                                 (set! cra cra)
                                 (begin
                                   (set! t0 a1)
                                   (set! t0 a2)
                                   (set! t0 a3)
                                   (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                   (jump-return cra)))))
                '(define L.odd.1
                   ((frameSize 8) (paramSize 0))
                   (begin
                     (set! cra cra)
                     (begin
                       (set! t0 a1)
                       (set! t0 a2)
                       (set! t0 a3)
                       (begin (set! t0 5) (set! t0 (+ t0 t0)))
                       (begin
                         (set! a1 0)
                         (set! a2 0)
                         (set! a3 0)
                         (set! a4 0)
                         (set! t0 0)
                         (set! t1 0)
                         (set! t2 0)
                         (set! t3 0)
                         (set! t4 0)
                         (set! t5 0)
                         (set! t6 0)
                         (jump-return cra)))))
                "clean-func: succes-01: simple function")
  ;failure
  (check-exn exn:fail? (thunk (clean-func '(defiene L.odd.1
                                             ((frameSize 8) (paramSize 0))
                                             (begin
                                               (set! cra cra)
                                               (begin
                                                 (set! t0 a1)
                                                 (set! t0 a2)
                                                 (set! t0 a3)
                                                 (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                 (jump-return cra))))))
             "clean-func: failure-01: wrong datum literal define")
  (check-exn exn:fail? (thunk (clean-func '(define 
                                             ((frameSize 8) (paramSize 0))
                                             (begin
                                               (set! cra cra)
                                               (begin
                                                 (set! t0 a1)
                                                 (set! t0 a2)
                                                 (set! t0 a3)
                                                 (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                 (jump-return cra))))))
             "clean-func: failure-02: no name")
  (check-exn exn:fail? (thunk (clean-func '(define L.odd.1
                                             (begin
                                               (set! cra cra)
                                               (begin
                                                 (set! t0 a1)
                                                 (set! t0 a2)
                                                 (set! t0 a3)
                                                 (begin (set! t0 5) (set! t0 (+ t0 t0)))
                                                 (jump-return cra))))))
             "clean-func: failure-03:  no info")
  (check-exn exn:fail? (thunk (clean-func '(define L.odd.1
                                             ((frameSize 8) (paramSize 0)))))
             "clean-func: failure-04:  no tail")
  ;clean-stktokens
  )