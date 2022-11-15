#lang racket

(require "common.rkt")
(provide impose-calling-conventions)

(module+ test
  (require rackunit))

(define argument-registers
  '(a0 a1))
  ;'(ca0 ca1 ca2 ca3 ca4 ca5 ca6 ca7))

(define (takeReg n)
  (set! argument-registers (take '(a0 a1 a2 a3 a4 a5 a6 a7) n)))

(define maxFrame 0)

(define (resetFrame)
  (if (> (getfvar) maxFrame)
      (set! maxFrame (getfvar))
      void)
  (resetfvar))

;
;(impose-args args)->list? '(aloc? ...)
;args: list? '(opand? ...)
(define (impose-args args isCall?)
  (define i -1)
  (define (impose-setFvar a)
    (cond [isCall? `(set! ,(freshfvar) ,a)]
          [else    `(set! ,a ,(freshfvar))]))
  (define (impose-setReg a)
    (set! i (add1 i))
    (cond [isCall? `(set! ,(list-ref argument-registers i) ,a)]
          [else    `(set! ,a ,(list-ref argument-registers i))]))
  ;Reset fvar, version only works with frame and deletes everything on the stack before hand
  (resetFrame)
  (let ([ta (cond [(> (length args) (length argument-registers)) (list-tail args (length argument-registers))]
                  [else '()])]
        [ca (cond [(> (length args) (length argument-registers)) (take args (length argument-registers))]
                  [else args])])
    (append (map impose-setFvar ta)
            (map impose-setReg ca))))

;
;(impose-args args)->list? '((set! aloc? opand?) ...)
;args: list? '(opand? ...)
(define (impose-argsCall args)
  (define (getFvar a)
    (freshfvar))
  ;Reset fvar, version only works with frame and deletes everything on the stack before hand
  (resetFrame)
  (let ([ta (cond [(> (length args) (length argument-registers)) (list-tail args (length argument-registers))]
                  [else '()])]
        [ca (cond [(< (length args) (length argument-registers)) (take argument-registers (length args))]
                  [else argument-registers])])
    (append ca
            (map (lambda (x) (freshfvar)) ta))))
           
;
;(impose-tail t)->tail?
;t: tail?
(define (impose-tail t)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@e ,(impose-tail tail))]
    [`(if ,p ,t1 ,t2) `(if ,p ,(impose-tail t1) ,(impose-tail t2))]
    [`(call ,n ,a ...) (cond [(or (label? n) (aloc? n)) `(begin ,@(impose-args a #t) (jump ,n ,fbp ,@(impose-argsCall a)))]              
                             [(integer? n) n]
                             [else #f])]
    [t t]))

;
;(impose-func f)->'(define label? tail?)
;f: '(define label? (lambda (aloc? ...) tail?))
(define (impose-func f)
  (match f
    [`(define ,l (lambda (,a ...) ,t)) `(define ,l (begin ,@(impose-args a #f) ,(impose-tail t)))]
    [_ #f]))

;Compiles Imp-lang-V5-cmf-proc to  Imp-lang-V5-cmf by imposing calling conventions on all calls and procedure definitions. The parameter registers are defined by the list current-parameter-registers.
;(impose-calling-conventions p)->Imp-lang-V5-cmf?
;p : Imp-lang-V5-cmf-proc?
(define (impose-calling-conventions p)
  (match p
    [`(module ,f ... ,t) (let ([res `(module ,@(map impose-func f) ,(impose-tail t))])
                           (setfvar maxFrame)    ;sets the fvar to the maximum amount, this way lower fvas do not overwrite these
                           res)]
    [_ "impose-calling-conventions failed"]))

(module+ test
  (define (check-impose a b m n)
    (takeReg n)
    (check-equal? (impose-calling-conventions a) b m))
  ;#|
;impose-calling-conventions
  ;succes
  (check-equal? (impose-calling-conventions '(module (begin (set! x.1 2) (begin (set! x.2 3) (set! x.2 (+ x.2 2))) (+ x.1 x.2))))
                '(module (begin (set! x.1 2) (begin (set! x.2 3) (set! x.2 (+ x.2 2))) (+ x.1 x.2)))
                "impose-calling-conventions: succes-01: no tail calls")
  (check-equal? (impose-calling-conventions '(module
                                                 (define L.odd?.1
                                                   (lambda (x.3)
                                                     (if (= x.3 0)
                                                         0
                                                         (begin (set! y.4 (+ x.3 -1)) (call L.even?.2 y.4)))))
                                               (define L.even?.2
                                                 (lambda (x.5)
                                                   (if (= x.5 0)
                                                       1
                                                       (begin (set! y.6 (+ x.5 -1)) (call L.odd?.1 y.6)))))
                                               (call L.even?.2 5)))
                '(module
                     (define L.odd?.1
                       (begin (set! x.3 a0)
                              (if (= x.3 0)
                                  0
                                  (begin (set! y.4 (+ x.3 -1)) 
                                         (begin (set! a0 y.4) (jump L.even?.2 cfp a0))))))
                   (define L.even?.2
                     (begin
                       (set! x.5 a0)
                       (if (= x.5 0)
                           1
                           (begin (set! y.6 (+ x.5 -1)) 
                                  (begin (set! a0 y.6) (jump L.odd?.1 cfp a0))))))
                   (begin (set! a0 5) (jump L.even?.2 cfp a0)))
                "sequentialize-let: succes-02: tail calls")
  (check-impose '(module (define L.test.1 (lambda (x.1 x.2 x.3) (begin (set! y.4 (+ x.1 x.2)) (+ x.3 y.4)))) (call L.test.1 1 2 3))
                '(module (define L.test.1 (begin  (set! x.2 fv0) (set! x.3 fv1) (set! x.1 a0) (begin (set! y.4 (+ x.1 x.2)) (+ x.3 y.4))))
                   (begin (set! fv0 2) (set! fv1 3) (set! a0 1) (jump L.test.1 cfp a0 fv0 fv1)))
                "impose-calling-conventions: succes-03: tail calls with fvar args"
                1)
  ;|#
  )