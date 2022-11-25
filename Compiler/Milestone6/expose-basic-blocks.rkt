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
;
;def: define? '(define label? tail?)
;t; list? '(effect? ... tail?)
(define (insert-define def t)
  (match def
    [`(define ,l ,tail) `(define ,l ,(append tail `(,t)))]
    [_ #f]))

;
;(expose-if-effect e l)-> '(list? list?) '(effect ...) '((define label? tail?) ...)
;e: effect?
;l: label?
(define (expose-if-effect e l)
  ;(println "if-effect")
  ;(pretty-display e)
  ;(println l)
  (match e
    [`(begin ,e ...) (expose-begin-effect (append e `((jump ,l))))]
    [`(if ,p ,e1 ,e2) (let ([l1 (newLabel)]
                            [l2 (newLabel)])
                        ;(println (format "~a - ~a" l1 l2))
                        (let-values ([(pP pD) (expose-if-pred p l1 l2)]
                                     [(e1P e1D) (expose-if-effect e1 l)]
                                     [(e2P e2D) (expose-if-effect e2 l)])
                          (values `(,pP)
                                  (append `((define ,l1 ,(cons 'begin `(,@e1P)));,(cons 'begin (cons e1P `((jump ,l)))))
                                            (define ,l2 ,(cons 'begin `(,@e2P))));,(cons 'begin (cons e2P `((jump ,l))))))
                                          pD e1D e2D))))]
    [`(set! ,a ,b) (values (cons e `((jump ,l))) '())]
    [`(return-point ,l ,t) (error "return point should not be in if, it should be in an begin")]
    [_ #f]))

;
;(expose-begin-split e)-> '(list? list?) '(effect ...) '((define label? tail?) ...)
;e: list? '(effect? ...)
(define (expose-begin-split e)
  ;(println "begin-if")
  ;(pretty-display e)
  (match e
    ['() (values '() '())]
    [`((jump ,l)) (values `((jump ,l)) '())]
    [`(,eff ,rest-eff ...) (match eff
                             [`(set! ,a ,b) (let-values ([(rP rD) (expose-begin-split rest-eff)])
                                              (values (cons eff rP) rD))]
                             [`(if ,p ,e1 ,e2) (let ([l1 (newLabel)]
                                                     [l2 (newLabel)]
                                                     [l3 (newLabel)])
                                                 ;(println (format "~a - ~a - ~a" l1 l2 l3))
                                                 (let-values ([(pP pD) (expose-if-pred p l1 l2)]
                                                              [(e1P e1D) (expose-if-effect e1 l3)]
                                                              [(e2P e2D) (expose-if-effect e2 l3)]
                                                              [(rP rD) (expose-begin-split rest-eff)])
                                                   (values `(,pP)
                                                           (append `((define ,l1 ,(cons 'begin e1P))
                                                                     (define ,l2 ,(cons 'begin e2P)))
                                                                   pD e1D e2D
                                                                   `((define ,l3 ,(cons 'begin rP)))
                                                                   rD))))]
                             [`(return-point ,l ,t) (let-values ([(tP tD) (expose-tail t)]
                                                                 [(rP rD) (expose-begin-split rest-eff)])
                                                      (values `(,tP)
                                                              (append `((define ,l ,(cons 'begin rP)))
                                                                      tD rD)))]
                             [_ #f])]))

;
;(expose-flatten-begin e)->list? '(effect ...)
;e: list? '(effect ...)
(define (expose-begin-flatten e)
  (for/fold ([res '()])
            ([effs e])
    (values (append res (match effs
                          [`(begin ,eff ...) (expose-begin-flatten eff)]
                          [a `(,effs)]
                          [_ #f])))))

;
;(expose-begin e)-> '(list? list?) '(effect ...) '((define label? tail?) ...)
;e: list? '(effect? ...)
(define (expose-begin-effect e)
  ;(println "effect:")
  ;(pretty-display e)
  (let ([eff (expose-begin-flatten e)])
    (expose-begin-split eff)))

;
;(expose-pred p curLabel)->(tail? list?) '(e/p/t ....) (curDefine  '(define ...))
;p: pred
(define (expose-if-pred p curL1 curL2)
  ;(println (format "pred: ~a - ~a" curL1 curL2))
  ;(pretty-display p)
  (match p
    [`(if ,p1 ,p2 ,p3) (let ([l1 (newLabel)]
                             [l2 (newLabel)])
                         ;(println (format "~a - ~a" l1 l2))
                         (let-values ([(p1P p1D) (expose-if-pred p1 l1 l2)]
                                      [(p2P p2D) (expose-if-pred p2 curL1 curL2)]
                                      [(p3P p3D) (expose-if-pred p3 curL1 curL2)])
                           (values p1P
                                   (append `((define ,l1 ,p2P)
                                             (define ,l2 ,p3P))
                                           p1D p2D p3D))))]
    [`(begin ,e ... ,pred) (let-values ([(eP eD) (expose-begin-effect e)]
                                        [(pP pD) (expose-if-pred pred curL1 curL2)])
                             (if (null? eD)
                                 (values (cons 'begin (append eP `(,pP))) pD)
                                 (values (cons 'begin eP) (append (drop-right eD 1) `(,(insert-define (last eD) pP)) pD))))]
    [`(,relop ,a ,b) (values `(if (,relop ,a ,b) (jump ,curL1) (jump ,curL2)) '())]
    ['(true) (values `(jump ,curL1) '())]
    ['(false) (values `(jump ,curL2) '())]
    [`(not ,pred) (match pred
                    [`(begin ,e ... ,pred) (expose-if-pred `(begin ,@e (not ,pred)) curL1 curL2)]
                    [`(if ,p1 ,p2 ,p3) (expose-if-pred `(if ,p1 (not ,p2) (not ,p3)) curL1 curL2)]
                    [`(not ,p1) (expose-if-pred p1 curL1 curL2)]
                    [a (values `(if (not ,pred) (jump ,curL1) (jump ,curL2)) '())])]
    [_ #f]))
    

;
;(expose-tail t curLabel)->(tail? list?) (curDefine  '(define ...))
;t: tail?
(define (expose-tail t)
  (match t
    [`(begin ,e ... ,tail) (let-values ([(eP eD) (expose-begin-effect e)]
                                        [(tP tD) (expose-tail tail)])
                             (if (null? eD)
                                 (values (cons 'begin (append eP `(,tP))) tD)
                                 (values (cons 'begin eP) (append (drop-right eD 1) `(,(insert-define (last eD) tP)) tD))))]
    [`(if ,pred ,t1 ,t2) (let ([l1 (newLabel)]
                               [l2 (newLabel)])
                           (let-values ([(pP pD) (expose-if-pred pred l1 l2)]
                                        [(t1P t1D) (expose-tail t1)]
                                        [(t2P t2D) (expose-tail t2)])
                             (values pP
                                     (append `((define ,l1 ,t1P)
                                               (define ,l2 ,t2P))
                                             t1D t2D pD))))]
    [`(jump ,l) (values `(jump ,l) '())]
    [_ #f]))

;
;(expose-func f)->list? '((define label? tail?) ...)
;f: '(define label? tail?)
(define (expose-func f)
  (match f
    [`(define ,l ,t)  (let-values ([(tP tD) (expose-tail t)])
                        `((define ,l ,tP) ,@tD))]
    [_ #f]))


;
;(expose-basic-blocks p)->Asm-lang-V4-nested
;p:Pred-lang-V4-Block
(define (expose-basic-blocks p)
  ;(println "begin")
  ;(println p)
  ;(println "start")
  (match p
    [`(module ,f ... ,t) (let-values ([(tP tD) (expose-tail t)])
                           `(module (define "L.tmp.0" ,tP) ,@tD ,@(foldl (lambda (func des) (append (expose-func func) des)) '() f)))]
    [_ "blocks failed"]))


#;(expose-basic-blocks '(module (define L.odd?.1
                                (begin (set! t1 ca0)
                                       (if (begin (set! t0 0) (= t1 t0))
                                           (halt 0)
                                           (begin (set! t0 (+ t1 -1))
                                                  (begin (set! ca0 t0) (jump L.even?.2))))))
                        (define L.even?.2
                          (begin (set! t1 ca0)
                                 (if (begin (set! t0 0) (= t1 t0))
                                     (halt 1)
                                     (begin (set! t0 (+ t1 -1))
                                            (begin (set! ca0 t0) (jump L.odd?.1))))))
                        (begin (set! ca0 5) (jump L.even?.2))))


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
  (check-expose-func expose-effect '(set! a0 (+ a1 a2)) '(((set! a0 (+ a1 a2))) ()) "expose-effect: succes-1: set instruction")
  
  (check-expose-func expose-effect '(begin (set! a0 (+ a1 a2))) '(((set! a0 (+ a1 a2))) ()) "expose-effect: succes-2: begin one effect")
  (check-expose-func expose-effect '(begin (set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! fv0 a0))
                '(((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! fv0 a0)) ())
                "expose-effect: succes-3: begin multiple effects")
  (check-expose-func expose-effect '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (set! fv0 a0))
                '(((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (jump "L.tmp.1")) ((define "L.tmp.1" (begin (set! fv0 a0)))))
                "expose-effect: succes-4: nested begin")

  (check-expose-func expose-effect '(if (true) (set! a0 (+ a1 a2)) (set! a0 a1))
                     '(((if (true) (jump "L.tmp.1") (jump "L.tmp.2")))
                       ((define "L.tmp.1" (set! a0 (+ a1 a2))) (define "L.tmp.2" (set! a0 a1))))
                     "expose-effect: succes-05: if")

;expose-pred
  ;succes
  (check-expose-pred expose-pred '(= a0 a1) "L1" "L2" '((= a0 a1) ()) "expose-pred: succes-1: relop instruction")
  (check-expose-pred expose-pred '(true) "L1" "L2" '((true) ()) "expose-pred: succes-2: true")
  (check-expose-pred expose-pred '(false) "L1" "L2" '((false) ()) "expose-pred: succes-3: false")
  (check-expose-pred expose-pred '(not (true)) "L1" "L2" '((not (true)) ()) "expose-pred: succes-4: not instruction")
  (check-expose-pred expose-pred '(begin (= a0 a1)) "L1" "L2" '(((= a0 a1)) ()) "expose-pred: succes-5: begin one effect")
  (check-expose-pred expose-pred '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (true))  "L1" "L2"
                '(((set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (jump "L.tmp.1")) ((define "L.tmp.1" (begin (true)))))
                "expose-pred: succes-6: nested begin")
 |#
  #|
;expose-tail
  ;succes
  (check-expose-func expose-tail '(halt a0) '((halt a0) ()) "expose-tail: succes-.1: halt instruction")
  
  (check-expose-func expose-tail '(begin (set! a0 (+ a1 a2)) (halt a0)) '((begin (set! a0 (+ a1 a2)) (halt a0)) ()) "expose-tail: succes-.2: begin one effect")
  (check-expose-func expose-tail '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0)) (halt a0))
                     '((begin (set! a0 fv0) (set! a1 fv1) (set! a0 (+ a1 a0)) (set! a2 a0) (halt a0))
                       ())
                     "expose-tail: succes-.3: nested begin")
  (check-expose-func expose-tail '(begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0) (halt a0)))
                     '((begin (set! a0 fv0) (set! a1 fv1) (begin (set! a0 (+ a1 a0)) (set! a2 a0) (halt a0)))
                       ())
                     "expose-tail: succes-04: nested begin")
  
  (check-expose-func expose-tail '(if (true) (halt a0) (halt a1))
                     '((jump "L.tmp.1")
                       ((define "L.tmp.1" (halt a0)) (define "L.tmp.2" (halt a1))))
                     "expose-tail: succes-05: if single pred")
  (check-expose-func expose-tail '(if (if (= a0 a1) (true) (false)) (halt a0) (halt a1))
                     '((if (= a0 a1) (jump "L.tmp.3") (jump "L.tmp.4"))
                       ((define "L.tmp.1" (halt a0))
                         (define "L.tmp.2" (halt a1))
                         (define "L.tmp.3" (jump "L.tmp.1"))
                         (define "L.tmp.4" (jump "L.tmp.2"))))
                     "expose-tail: succes-06: if if pred")
  (check-expose-func expose-tail '(if (begin (set! a0 a1) (true)) (halt a0) (halt a1))
                     '((begin (set! a0 a1) (jump "L.tmp.1"))
                       ((define "L.tmp.1" (halt a0)) (define "L.tmp.2" (halt a1))))
                     "expose-tail: succes-07: if begin pred")
;expose-basic-blocks
  ;succes
  (check-expose (expose-basic-blocks '(module (halt a0)))
                '(module (define "L.tmp.0" (halt a0)))
                "expose-basic-blocks: succes-01: halt tail")
  (check-expose (expose-basic-blocks '(module (if (< a0 a1) (halt a0) (halt a1))))
                '(module (define "L.tmp.0" (if (< a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.1" (halt a0))
                   (define "L.tmp.2" (halt a1)))
                "expose-basic-blocks: succes-02: if tail")
  (check-expose (expose-basic-blocks '(module (if (< a0 a1) (halt a0) (if (= a0 a1) (halt a1) (halt a2)))))
                '(module (define "L.tmp.0" (if (< a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.1" (halt a0))
                   (define "L.tmp.2" (if (= a0 a1) (jump "L.tmp.3") (jump "L.tmp.4")))
                   (define "L.tmp.3" (halt a1))
                   (define "L.tmp.4" (halt a2)))
                "expose-basic-blocks: succes-03: nested if tail")
  (check-expose (expose-basic-blocks '(module (if (if (< a0 a1) (= a0 a1) (> a0 a1)) (halt a0) (halt a1))))
                '(module (define "L.tmp.0" (if (< a0 a1) (jump "L.tmp.3") (jump "L.tmp.4")))
                   (define "L.tmp.1" (halt a0))           
                   (define "L.tmp.2" (halt a1))
                   (define "L.tmp.3" (if (= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.4" (if (> a0 a1) (jump "L.tmp.1") (jump "L.tmp.2"))))
                "expose-basic-blocks: succes-04: nested if tail")
  (check-expose (expose-basic-blocks '(module (if (if (< a0 a1) (= a0 a1) (if (< a0 a1) (= a0 a1) (if (< a0 a1) (= a0 a1) (> a0 a1)))) (halt a0) (halt a1))))
                '(module (define "L.tmp.0" (if (< a0 a1) (jump "L.tmp.3") (jump "L.tmp.4")))
                   (define "L.tmp.1" (halt a0))
                   (define "L.tmp.2" (halt a1))
                   (define "L.tmp.3" (if (= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.4" (if (< a0 a1) (jump "L.tmp.5") (jump "L.tmp.6")))
                   (define "L.tmp.5" (if (= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.6" (if (< a0 a1) (jump "L.tmp.7") (jump "L.tmp.8")))
                   (define "L.tmp.7" (if (= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.8" (if (> a0 a1) (jump "L.tmp.1") (jump "L.tmp.2"))))
                "expose-basic-blocks: succes-05: multiple nested if pred if tail")
  (check-expose (expose-basic-blocks '(module (if (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (>= a2 a3) (if (< a0 a1) (> a0 a1) (if (<= a0 a1) (!= a0 a1) (>= a0 a1)))) (halt a0) (halt a1))))
                '(module (define "L.tmp.0" (if (< a0 a1) (jump "L.tmp.5") (jump "L.tmp.6")))
                   (define "L.tmp.1" (halt a0))
                   (define "L.tmp.2" (halt a1))
                   (define "L.tmp.3" (if (>= a2 a3) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.4" (if (< a0 a1) (jump "L.tmp.7") (jump "L.tmp.8")))
                   (define "L.tmp.5" (if (= a0 a1) (jump "L.tmp.3") (jump "L.tmp.4")))
                   (define "L.tmp.6" (if (= a1 a0) (jump "L.tmp.3") (jump "L.tmp.4")))
                   (define "L.tmp.7" (if (> a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.8" (if (<= a0 a1) (jump "L.tmp.9") (jump "L.tmp.10")))
                   (define "L.tmp.9" (if (!= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.10" (if (>= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2"))))
                "expose-basic-blocks: succes-06: multiple nested if tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a0 b0) (set! a0 b0) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (halt a0) (halt a1)))))
                '(module (define "L.tmp.0" (begin (set! a0 b0) (set! a0 b0) (if (< a0 a1) (jump "L.tmp.3") (jump "L.tmp.4"))))
                   (define "L.tmp.1" (halt a0))
                   (define "L.tmp.2" (halt a1))
                   (define "L.tmp.3" (if (= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.4" (if (= a1 a0) (jump "L.tmp.1") (jump "L.tmp.2"))))
                "expose-basic-blocks: succes-07: begin tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a2 b2) (if (> a0 a1) (set! a0 b0) (set! a1 b1)) (set! a2 b2) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (halt a0) (halt a1)))))
                '(module (define "L.tmp.0" (begin (set! a2 b2) (if (> a0 a1) (jump "L.tmp.1") (jump "L.tmp.2"))))
                   (define "L.tmp.1" (begin (set! a0 b0) (jump "L.tmp.3")))
                   (define "L.tmp.2" (begin (set! a1 b1) (jump "L.tmp.3")))
                   (define "L.tmp.3" (begin (set! a2 b2) (if (< a0 a1) (jump "L.tmp.6") (jump "L.tmp.7"))))
                   (define "L.tmp.4" (halt a0))
                   (define "L.tmp.5" (halt a1))
                   (define "L.tmp.6" (if (= a0 a1) (jump "L.tmp.4") (jump "L.tmp.5")))
                   (define "L.tmp.7" (if (= a1 a0) (jump "L.tmp.4") (jump "L.tmp.5"))))
                "expose-basic-blocks: succes-08: begin tail if effect")
  (check-expose (expose-basic-blocks '(module (begin (set! a2 b2) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (set! a0 b0) (set! a1 b1)) (set! a2 b2) (halt a0))))
                '(module (define "L.tmp.0" (begin (set! a2 b2) (if (< a0 a1) (jump "L.tmp.4") (jump "L.tmp.5"))))
                   (define "L.tmp.1" (begin (set! a0 b0) (jump "L.tmp.3")))
                   (define "L.tmp.2" (begin (set! a1 b1) (jump "L.tmp.3")))
                   (define "L.tmp.4" (if (= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.5" (if (= a1 a0) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.3" (begin (set! a2 b2) (halt a0))))
                "expose-basic-blocks: succes-09: begin tail if effect with if pred")
  (check-expose (expose-basic-blocks '(module (if (begin (set! a c) (set! c a) (< a0 a1)) (halt a0) (halt a1))))
                '(module (define "L.tmp.0" (begin (set! a c) (set! c a) (if (< a0 a1) (jump "L.tmp.1") (jump "L.tmp.2"))))
                   (define "L.tmp.1" (halt a0))
                   (define "L.tmp.2" (halt a1)))
                "expose-basic-blocks: succes-10: begin pred in if tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a b) (set! c d) (begin (set! e f) (set! k l)) (halt a0))))
                '(module (define "L.tmp.0" (begin (set! a b) (set! c d) (set! e f) (set! k l) (halt a0))))
                 "expose-basic-blocks: succes-11: begin effect in begin tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a b) (set! c d) (begin (set! e f) (begin (set! l f) (set! m l)) (set! k l)) (halt a0))))
                '(module (define "L.tmp.0" (begin (set! a b) (set! c d) (set! e f) (set! l f) (set! m l) (set! k l) (halt a0))))
                "expose-basic-blocks: succes-12: nested begin effect in begin tail")
  (check-expose (expose-basic-blocks '(module (begin (set! a2 b2) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (set! a0 b0) (set! a1 b1)) (set! a2 b2) (if (if (< a0 a1) (= a0 a1) (= a1 a0)) (set! a0 b0) (set! a1 b1)) (set! a9 r9) (halt a0))))
                '(module (define "L.tmp.0" (begin (set! a2 b2) (if (< a0 a1) (jump "L.tmp.4") (jump "L.tmp.5"))))
                   (define "L.tmp.1" (begin (set! a0 b0) (jump "L.tmp.3")))
                   (define "L.tmp.2" (begin (set! a1 b1) (jump "L.tmp.3")))
                   (define "L.tmp.4" (if (= a0 a1) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.5" (if (= a1 a0) (jump "L.tmp.1") (jump "L.tmp.2")))
                   (define "L.tmp.3" (begin (set! a2 b2) (if (< a0 a1) (jump "L.tmp.9") (jump "L.tmp.10"))))
                   (define "L.tmp.6" (begin (set! a0 b0) (jump "L.tmp.8")))
                   (define "L.tmp.7" (begin (set! a1 b1) (jump "L.tmp.8")))
                   (define "L.tmp.9" (if (= a0 a1) (jump "L.tmp.6") (jump "L.tmp.7")))
                   (define "L.tmp.10" (if (= a1 a0) (jump "L.tmp.6") (jump "L.tmp.7")))
                   (define "L.tmp.8" (begin (set! a9 r9) (halt a0))))
                "expose-basic-blocks: succes-13: multiple if effects")
  (check-expose (expose-basic-blocks '(module
                            (if (false)
                                (if (not
                                     (if (not (false))
                                         (begin (set! t1 419) (set! t0 -178) (< t1 t0))
                                         (false)))
                                    (begin
                                      (begin
                                        (begin (set! t2 -112) (set! t1 -316) (set! t0 (* t2 t1)))
                                        (if (not (begin (set! t1 8) (set! t0 -16) (<= t1 t0)))
                                            (set! t0 258)
                                            (if (not
                                                 (if (begin
                                                       (begin (set! t1 367) (set! t0 (+ t1 -203)))
                                                       (begin (set! t0 -236) (set! t4 (+ t0 -62)))
                                                       (if (false)
                                                           (begin
                                                             (set! t2 -294)
                                                             (set! t1 -146)
                                                             (set! t0 (* t2 t1)))
                                                           (set! t0 -425))
                                                       (begin (set! t1 62) (set! t0 (* t1 t1)))
                                                       (begin
                                                         (begin (set! t0 -84) (set! t3 (+ t0 -24)))
                                                         (begin
                                                           (begin
                                                             (begin (set! t1 201) (set! t0 (+ t1 289)))
                                                             (begin (set! t1 -343) (set! t0 (+ t1 8)))
                                                             (set! t2 -35)
                                                             (set! t0 -22)
                                                             (begin (set! t1 382) (set! t0 (+ t1 t2))))
                                                           (begin
                                                             (set! t2 -166)
                                                             (set! t1 449)
                                                             (set! t0 (* t2 t1)))
                                                           (begin
                                                             (set! t2 439)
                                                             (set! t1 102)
                                                             (set! t0 (* t2 t1)))
                                                           (begin (set! t1 369) (set! t0 (+ t1 461)))
                                                           (set! t0 -2)
                                                           (begin
                                                             (set! t2 215)
                                                             (set! t1 430)
                                                             (set! t0 (* t2 t1))))
                                                         (begin (set! t1 -409) (set! t0 (* t1 t3))))
                                                       (begin (set! t0 -62) (<= t4 t0)))
                                                     (true)
                                                     (false)))
                                                (set! t0 -306)
                                                (set! t0 90)))
                                        (begin (set! t1 313) (set! t0 (+ t1 228)))
                                        (begin (set! t1 3) (set! t0 (+ t1 110)))
                                        (begin (set! t1 450) (set! t0 (+ t1 415)))
                                        (set! t0 -358))
                                      (begin (set! t1 -242) (set! t0 350) (set! t1 (* t1 t0)) (halt t1)))
                                    (begin (set! t0 189) (set! t0 (+ t0 -501)) (halt t0)))
                                (halt -462))))
                '(module (define "L.tmp.0" (jump "L.tmp.2")) (define "L.tmp.1" (if (not (false)) (jump "L.tmp.5") (jump "L.tmp.6")))
                   (define "L.tmp.2" (halt -462))
                   (define "L.tmp.3" (begin (set! t2 -112) (set! t1 -316) (set! t0 (* t2 t1)) (begin (set! t1 8) (set! t0 -16) (if (not (<= t1 t0)) (jump "L.tmp.7") (jump "L.tmp.8")))))
                   (define "L.tmp.4" (begin (set! t0 189) (set! t0 (+ t0 -501)) (halt t0)))
                   (define "L.tmp.7" (begin (set! t0 258) (jump "L.tmp.9")))
                   (define "L.tmp.8" (begin (begin (set! t1 367) (set! t0 (+ t1 -203)) (set! t0 -236) (set! t4 (+ t0 -62)) (jump "L.tmp.15"))))
                   (define "L.tmp.10" (begin (set! t0 -306) (jump "L.tmp.9")))
                   (define "L.tmp.11" (begin (set! t0 90) (jump "L.tmp.9")))
                   (define "L.tmp.12" (if (not (true)) (jump "L.tmp.10") (jump "L.tmp.11")))
                   (define "L.tmp.13" (if (not (false)) (jump "L.tmp.10") (jump "L.tmp.11")))
                   (define "L.tmp.14" (begin (set! t2 -294) (set! t1 -146) (set! t0 (* t2 t1)) (jump "L.tmp.16")))
                   (define "L.tmp.15" (begin (set! t0 -425) (jump "L.tmp.16")))
                   (define "L.tmp.16"
                     (begin
                       (set! t1 62)
                       (set! t0 (* t1 t1))
                       (set! t0 -84)
                       (set! t3 (+ t0 -24))
                       (set! t1 201)
                       (set! t0 (+ t1 289))
                       (set! t1 -343)
                       (set! t0 (+ t1 8))
                       (set! t2 -35)
                       (set! t0 -22)
                       (set! t1 382)
                       (set! t0 (+ t1 t2))
                       (set! t2 -166)
                       (set! t1 449)
                       (set! t0 (* t2 t1))
                       (set! t2 439)
                       (set! t1 102)
                       (set! t0 (* t2 t1))
                       (set! t1 369)
                       (set! t0 (+ t1 461))
                       (set! t0 -2)
                       (set! t2 215)
                       (set! t1 430)
                       (set! t0 (* t2 t1))
                       (set! t1 -409)
                       (set! t0 (* t1 t3))
                       (begin (set! t0 -62) (if (<= t4 t0) (jump "L.tmp.12") (jump "L.tmp.13")))))
                   (define "L.tmp.9"
                     (begin
                       (set! t1 313)
                       (set! t0 (+ t1 228))
                       (set! t1 3)
                       (set! t0 (+ t1 110))
                       (set! t1 450)
                       (set! t0 (+ t1 415))
                       (set! t0 -358)
                       (begin (set! t1 -242) (set! t0 350) (set! t1 (* t1 t0)) (halt t1))))
                   (define "L.tmp.5" (begin (set! t1 419) (set! t0 -178) (if (not (< t1 t0)) (jump "L.tmp.3") (jump "L.tmp.4"))))
                   (define "L.tmp.6" (if (not (false)) (jump "L.tmp.3") (jump "L.tmp.4"))))
                "expose-basic-blocks: succes-14: complex if effects")
  ;|#
  )


