#lang racket

(provide expose-basic-blocks)

(module+ test
  (require rackunit))

(define l 0)
(define (newLabel)
  (set! l (add1 l))
  (format "L.tmp.~a" l))
(define (resetLabel)
  (set! l 0))


;
;(expose-begin e curLabel)->(list? list?) '(effect? ...) '(define ...)
;e: list? '(effect? ...)
;restP: tail?
;restD: list? '(define...)
(define (expose-begin e restP restD)
  (match e
    ['() (values restP restD)]
    [`((if ,p ,e1 ,e2) ,restE ...) (let ([l1 (newLabel)]
                                         [l2 (newLabel)]
                                         [l3 (newLabel)])
                                     ;(println (format "begin-if ~a - ~a ~a ~a" e l1 l2 l3))
                                     ;(println (format "begin-if1 ~a - ~a" (append e1 `((jump ,l3))) e1))
                                     ;(println (format "begin-if2 ~a - ~a" (append e2 `((jump ,l3))) e2))
                                     (let-values ([(pP pD) (expose-if p l1 l2)]
                                                  [(e1P e1D) (expose-begin (cons e1 `((jump ,l3))) '() '())]
                                                  [(e2P e2D) (expose-begin (cons e2 `((jump ,l3))) '() '())]
                                                  [(rP rD) (expose-begin restE restP restD)])
                                       ;(println (format "begin-if ~a - ~a ~a ~a" e l1 l2 l3))
                                       ;(println (format "begin-if1 ~a - ~a" e1 e1D))
                                       (values `(,pP)
                                               (append `((define ,l1 (begin ,@e1P))
                                                         (define ,l2 (begin ,@e2P))
                                                         (define ,l3 (begin ,@rP)))
                                                       pD e1D e2D rD))))]
    [`((begin ,e ...) ,restE ...) (let ([l1 (newLabel)])
                                    (let-values ([(bP bD) (expose-begin (append e `((jump ,l1))) '() '())]
                                                 [(rP rD) (expose-begin restE restP restD)])
                                      (values bP
                                              (append `((define ,l1 (begin ,@rP)))
                                                      bD rD))))]
    [`(,e ,restE ...) (let-values ([(eP eD) (expose-effect e)]
                                 [(rP rD) (expose-begin restE restP restD)])
                        ;(println (format "begin-effects ~a - ~a" eP rP))
                        (values (append eP rP) (append eD rD)))]))
;
;(expose-ifBegin p l1 l2)->tail?/pred?/effect?
;p: tail?/pred?/effect?
;l1, l2: label?
(define (expose-if p l1 l2)
  ;(println (format "if1: ~a" p))
  (let-values ([(pP pD) (expose-pred p l1 l2)])
    ;(println (format "if2: ~a" pP))
    (values  (match pP
               [`(if ,pr ,n1 ,n2) `(if ,pr ,n1 ,n2)]
               [`(begin ,e ... ,pred) `(begin ,@e (if ,pred (jump ,l1) (jump ,l2)))]
               [a `(if ,pP (jump ,l1) (jump ,l2))])
             pD)))
;
;(expose-effect e)->list? '(effect ...)
;e: list? '(effect? ...)
(define (expose-effect e)
  ;(println (format "effect ~a" e))
  (match e
    [`(set! ,a ,b) (values `((set! ,a ,b)) '())]
    [`(begin ,eff ...) (let-values ([(bP bD) (expose-begin eff '() '())])
                       (println (format "effect-begin ~a" eff))
                       (values bP bD))]
    [`(if ,pred ,e1 ,e2) (let ([l1 (newLabel)]
                               [l2 (newLabel)])
                           (println "!!!!!!!!!!!!am i here")
                           (let-values ([(pP pD) (expose-if pred l1 l2)]
                                        [(e1P e1D) (expose-effect e1)]
                                        [(e2P e2D) (expose-effect e2)])
                             (values `(,pP)
                                     (append `((define ,l1 ,@e1P)
                                               (define ,l2 ,@e2P))
                                             e1D e2D pD))))]
    [`(jump ,l) (values `((jump ,l)) '())]
    [_ #f]))



;
;(expose-pred p curLabel)->list?
;p: pred?
(define (expose-pred p curL1 curL2)
  (match p
    [`(,relop ,a ,b) (values `(,relop ,a ,b) '())]
    ['(true) (values '(true) '())]
    ['(false) (values '(false) '())]
    [`(not ,pred) (let-values ([(pP pD) (expose-pred pred curL1 curL2)])
                    (values `(not ,pP) pD))]
    [`(begin ,e ... ,pred) (let*-values ([(pP pD) (expose-pred pred curL1 curL2)]
                                         [(bP bD) (expose-begin e `(,pP) pD)])
                             (values `(begin ,@bP) bD))]
    [`(if ,p1 ,p2 ,p3) (let ([l1 (newLabel)]
                             [l2 (newLabel)])
                         (let-values ([(p1P p1D) (expose-if p1 l1 l2)]
                                      [(p2P p2D) (expose-if p2 curL1 curL2)]
                                      [(p3P p3D) (expose-if p3 curL1 curL2)])
                           (values p1P
                                   (append `((define ,l1 ,p2P)
                                             (define ,l2 ,p3P))
                                           p1D p2D p3D))))]
    [_ #f]))

;
;(expose-tail t curLabel)->(tail? list?) (curDefine  '(define ...))
;t: tail?
(define (expose-tail t)
  (match t
    [`(begin ,e ... ,t) (let*-values ([(tP tD) (expose-tail t)]
                                      [(bP bD) (expose-begin e `(,tP) tD)])
                          (values `(begin ,@bP) bD))]
    [`(halt ,a) (values `(halt ,a) '())]
    [`(if ,pred ,t1 ,t2) (let ([l1 (newLabel)]
                               [l2 (newLabel)])
                           (let-values ([(pP pD) (expose-if pred l1 l2)]
                                        [(t1P t1D) (expose-tail t1)]
                                        [(t2P t2D) (expose-tail t2)])
                             (values pP
                                     (append `((define ,l1 ,t1P)
                                               (define ,l2 ,t2P))
                                             t1D t2D pD))))]
    [_ #f]))

;
;(expose-basic-blocks p)->Asm-lang-V4-nested
;p:Pred-lang-V4-Block
(define (expose-basic-blocks p)
  ;(println "begin")
  ;(println p)
  ;(println "start")
  (match p
    [`(module ,t) (let ([l (newLabel)])
                    (let-values ([(tP tD) (expose-tail t)])
                      `(module ,@(cons `(define ,l ,tP) tD))))]
    [_ "blocks failed"]))

#|
(expose-basic-blocks '(module (begin (set! a b) (set! c d) (if (< p o) (begin (set! e f) (begin (if (if (= z w) (true) (false)) (set! q p) (set! g h)) (set! m l)) (set! k l)) (set! i u)) (halt a0))))

;|#

(module+ test
  (define (check-expose-func f a o m)
    (define-values (cp def) (f a))
    (resetLabel)
    (check-equal? cp (car o) m)
    (check-equal? def (second o) m))
  (define (check-expose p c m)
    (resetLabel)
    (check-equal? p c m))
                        
  #|
;expose-effect
  ;succes
  (check-equal? (expose-effect '(set! a0 (+ a1 a2)) "L.l.1") '((set! a0 (+ a1 a2))) "expose-effect: succes-1: set instruction")
  (check-equal? (expose-effect '(begin (set! a0 (+ a1 a2))) "L.l.1") '((set! a0 (+ a1 a2))) "expose-effect: succes-2: begin one effect")
  (check-equal? (expose-effect '(begin (set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! fv0 a0)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! fv0 a0))
                "expose-effect: succes-3: begin multiple effects")
  (check-equal? (expose-effect '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (set! fv0 a0)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (set! fv0 a0))
                "expose-effect: succes-4: nested begin")
;expose-pred
  ;succes
  (check-equal? (expose-pred '(= a0 a1) "L.l.1") '((= a0 a1)) "expose-pred: succes-1: relop instruction")
  (check-equal? (expose-pred '(true) "L.l.1") '((true)) "expose-pred: succes-2: true")
  (check-equal? (expose-pred '(false) "L.l.1") '((false)) "expose-pred: succes-3: false")
  (check-equal? (expose-pred '(not (true)) "L.l.1") '((not (true))) "expose-pred: succes-4: not instruction")
  (check-equal? (expose-pred '(begin (= a0 a1)) "L.l.1") '((= a0 a1)) "expose-pred: succes-5: begin one effect")
  (check-equal? (expose-pred '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (true)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (true))
                "expose-pred: succes-6: nested begin")
;expose-tail
  ;succes
  (check-equal? (expose-tail '(halt a0) "L.l.1") '((halt a0)) "expose-tail: succes-1: halt instruction")
  (check-equal? (expose-tail '(begin (set! a0 (+ a1 a2)) (halt a0)) "L.l.1") '((set! a0 (+ a1 a2)) (halt a0)) "expose-tail: succes-2: begin one effect")
  (check-equal? (expose-tail '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (halt a0)) "L.l.1")
                '((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (halt a0))
                "expose-tail: succes-3: nested begin")
  (check-expose-func expose-tail '(halt a0) '((halt a0) ()) "expose-tail: succes-1: halt instruction")
  (check-expose-func expose-tail '(if (true) (halt a0) (halt a1))
                     '((if (true) (jump "L.tmp.1") (jump "L.tmp.2"))
                       ((define "L.tmp.1" (halt a0)) (define "L.tmp.2" (halt a1))))
                   "expose-tail: succes-2: if instruction met halt")
;|#  
;expose-basic-blocks
  ;succes
  (check-expose (expose-basic-blocks '(module (halt a0)))
                '(module (define "L.tmp.1" (halt a0)))
                "expose-basic-blocks: succes-01: halt tail")
  (check-expose (expose-basic-blocks '(module (if (< a0 a1) (halt a0) (halt a1))))
                '(module (define "L.tmp.1" (if (< a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.2" (halt a0))
                   (define "L.tmp.3" (halt a1)))
                "expose-basic-blocks: succes-02: if tail")
  (check-expose (expose-basic-blocks '(module (if (< a0 a1) (halt a0) (if (= a0 a1) (halt a1) (halt a2)))))
                '(module (define "L.tmp.1" (if (< a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.2" (halt a0))
                   (define "L.tmp.3" (if (= a0 a1) (jump "L.tmp.4") (jump "L.tmp.5")))
                   (define "L.tmp.4" (halt a1))
                   (define "L.tmp.5" (halt a2)))
                "expose-basic-blocks: succes-03: nested if tail")
  (check-expose (expose-basic-blocks '(module (if (if (< a0 a1) (= a0 a1) (> a0 a1)) (halt a0) (halt a1))))
                '(module (define "L.tmp.1" (if (< a0 a1) (jump "L.tmp.4") (jump "L.tmp.5")))
                   (define "L.tmp.2" (halt a0))           
                   (define "L.tmp.3" (halt a1))
                   (define "L.tmp.4" (if (= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.5" (if (> a0 a1) (jump "L.tmp.2") (jump "L.tmp.3"))))
                "expose-basic-blocks: succes-04: nested if tail")
  (check-expose (expose-basic-blocks '(module (if (if (< a0 a1) (= a0 a1) (if (< a0 a1) (= a0 a1) (if (< a0 a1) (= a0 a1) (> a0 a1)))) (halt a0) (halt a1))))
                '(module (define "L.tmp.1" (if (< a0 a1) (jump "L.tmp.4") (jump "L.tmp.5"))) (define "L.tmp.2" (halt a0))
                   (define "L.tmp.3" (halt a1))
                   (define "L.tmp.4" (if (= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.5" (if (< a0 a1) (jump "L.tmp.6") (jump "L.tmp.7")))
                   (define "L.tmp.6" (if (= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.7" (if (< a0 a1) (jump "L.tmp.8") (jump "L.tmp.9")))
                   (define "L.tmp.8" (if (= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.9" (if (> a0 a1) (jump "L.tmp.2") (jump "L.tmp.3"))))
                "expose-basic-blocks: succes-05: multiple nested if pred if tail")
  (check-expose (expose-basic-blocks '(module (if (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (>= a2 a3) (if (< a0 a1) (> a0 a1) (if (<= a0 a1) (!= a0 a1) (>= a0 a1)))) (halt a0) (halt a1))))
                '(module (define "L.tmp.1" (if (< a0 a1) (jump "L.tmp.6") (jump "L.tmp.7")))
                   (define "L.tmp.2" (halt a0))
                   (define "L.tmp.3" (halt a1))
                   (define "L.tmp.4" (if (>= a2 a3) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.5" (if (< a0 a1) (jump "L.tmp.8") (jump "L.tmp.9")))
                   (define "L.tmp.6" (if (= a0 a1) (jump "L.tmp.4") (jump "L.tmp.5")))
                   (define "L.tmp.7" (if (= a1 a0) (jump "L.tmp.4") (jump "L.tmp.5")))
                   (define "L.tmp.8" (if (> a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.9" (if (<= a0 a1) (jump "L.tmp.10") (jump "L.tmp.11")))
                   (define "L.tmp.10" (if (!= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.11" (if (>= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3"))))
                "expose-basic-blocks: succes-06: multiple nested if tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a0 b0) (set! a0 b0) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (halt a0) (halt a1)))))
                '(module (define "L.tmp.1" (begin (set! a0 b0) (set! a0 b0) (if (< a0 a1) (jump "L.tmp.4") (jump "L.tmp.5")))) (define "L.tmp.2" (halt a0))
                   (define "L.tmp.3" (halt a1))
                   (define "L.tmp.4" (if (= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.5" (if (= a1 a0) (jump "L.tmp.2") (jump "L.tmp.3"))))
                "expose-basic-blocks: succes-07: begin tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a2 b2) (if (> a0 a1) (set! a0 b0) (set! a1 b1)) (set! a2 b2) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (halt a0) (halt a1)))))
                '(module (define "L.tmp.1" (begin (set! a2 b2) (if (> a0 a1) (jump "L.tmp.6") (jump "L.tmp.7")))) (define "L.tmp.6" (begin (set! a0 b0) (jump "L.tmp.8")))
                   (define "L.tmp.7" (begin (set! a1 b1) (jump "L.tmp.8")))
                   (define "L.tmp.8" (begin (set! a2 b2) (if (< a0 a1) (jump "L.tmp.4") (jump "L.tmp.5"))))
                   (define "L.tmp.2" (halt a0))
                   (define "L.tmp.3" (halt a1))
                   (define "L.tmp.4" (if (= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.5" (if (= a1 a0) (jump "L.tmp.2") (jump "L.tmp.3"))))
                "expose-basic-blocks: succes-08: begin tail if effect")
  (check-expose (expose-basic-blocks '(module (begin (set! a2 b2) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (set! a0 b0) (set! a1 b1)) (set! a2 b2) (halt a0))))
                '(module (define "L.tmp.1" (begin (set! a2 b2) (if (< a0 a1) (jump "L.tmp.5") (jump "L.tmp.6"))))
                   (define "L.tmp.2" (begin (set! a0 b0) (jump "L.tmp.4")))
                   (define "L.tmp.3" (begin (set! a1 b1) (jump "L.tmp.4")))
                   (define "L.tmp.4" (begin (set! a2 b2) (halt a0)))
                   (define "L.tmp.5" (if (= a0 a1) (jump "L.tmp.2") (jump "L.tmp.3")))
                   (define "L.tmp.6" (if (= a1 a0) (jump "L.tmp.2") (jump "L.tmp.3"))))
                "expose-basic-blocks: succes-09: begin tail if effect with if pred")
  (check-expose (expose-basic-blocks '(module (if (begin (set! a c) (set! c a) (< a0 a1)) (halt a0) (halt a1))))
                '(module (define "L.tmp.1" (begin (set! a c) (set! c a) (if (< a0 a1) (jump "L.tmp.2") (jump "L.tmp.3"))))
                   (define "L.tmp.2" (halt a0))
                   (define "L.tmp.3" (halt a1)))
                "expose-basic-blocks: succes-10: begin pred in if tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a b) (set! c d) (begin (set! e f) (set! k l)) (halt a0))))
                '(module (define "L.tmp.1" (begin (set! a b) (set! c d) (set! e f) (set! k l) (jump "L.tmp.2")))
                   (define "L.tmp.2" (begin (halt a0))))
                 "expose-basic-blocks: succes-11: begin effect in begin tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a b) (set! c d) (begin (set! e f) (begin (set! l f) (set! m l)) (set! k l)) (halt a0))))
                '(module (define "L.tmp.1" (begin (set! a b) (set! c d) (set! e f) (set! l f) (set! m l) (jump "L.tmp.3")))
                   (define "L.tmp.2" (begin (halt a0)))
                   (define "L.tmp.3" (begin (set! k l) (jump "L.tmp.2"))))
                "expose-basic-blocks: succes-12: nested begin effect in begin tail")
  ;|#
  )