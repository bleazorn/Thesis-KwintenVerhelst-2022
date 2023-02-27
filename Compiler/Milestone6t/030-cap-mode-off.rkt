#lang racket

(require "common/register.rkt"
         "common/aloc.rkt"
         "langs/paren-cheri-risc-v.rkt")

(provide cap-mode-off)


;
;(cap-binop e)->effect?
;e: effect?
(define (cap-binop e)
  (match e
    [`(set! ,a (+ ,b ,c)) `(set! ,(makeReg a) (+ ,(makeReg b) ,c))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,(makeReg a) (,binop ,(makeReg b) ,(makeReg c)))]
    [_ e]))

;
;(cap-set e)->effect?
;e: effect?
(define (cap-set e)
  (match e
    [`(set! (,a ,binop ,n) ,b)                                                 `(set! (,(makeReg a) ,binop ,n) ,(makeReg b))]
    [`(set! ,a (,b ,binop ,n))                                                 `(set! ,(makeReg a) (,(makeReg b) ,binop ,n))]
    [`(set! ,r ,a) #:when (label? a)                                      `(set! ,(makeReg r) ,a)]
    [`(set! ,r ,a) #:when (and (integer? a) (or (> a 2048) (<= a -2048))) `(set! ,(makeReg r) ,a)]
    [`(set! ,a ,b) `(set! ,(makeReg a) ,(makeReg b))]
    [_ e]))

;
;(cap-jump e)->effect?
;e: effect?
(define (cap-jump e)
  (match e
    [`(jump ,r) #:when (register? r) `(jump ,(makeReg r))]
    [`(jump (,r ,binop ,n)) #:when (register? r) `(jump (,(makeReg r) ,binop ,n))]
    [_ e]))    

;
;(patch-effect e)->effect?
;e: effect?
(define (cap-effect e)
  (match e
    [`(set! ,a (,binop ,b ,c)) #:when (register? b) (cap-binop e)]
    [`(set! ,a ,b)                (cap-set e)]
    [`(with-label ,l ,eff)        `(with-label ,l ,(cap-effect eff))]
    [`(jump ,l)                   (cap-jump e)]
    [`(compare ,a (,relop ,b ,c)) `(compare ,(makeReg a) (,relop ,(makeReg b) ,(makeReg c)))]
    [`(jump-if ,l (,relop ,b ,c)) `(jump-if ,l (,relop ,(makeReg b) ,(makeReg c)))]
    [_ #f]))

;
;(set-cap-mode p)->paren-cheri-risc-v-V6?
;p: paren-risc-v-V6-cap?
(define/contract (cap-mode-off p) (-> paren-cheri-risc-v? paren-cheri-risc-v?)
  (match p
    [`(begin ,i ,s ...)  `(begin ,i ,@(map cap-effect s))]
    [_ "cap mode on failed"]))


(module+ test
  (require rackunit)
#|
;cap-binop
  ;succes
  (check-equal? (cap-binop '(set! a0 (+ a0 a0))) '(set! a0 (+ a0 a0)) "cap-binop: succes-01: add reg reg reg")
  (check-equal? (cap-binop '(set! a0 (+ a0 ca0))) '(set! a0 (+ a0 a0)) "cap-binop: succes-02: add reg reg cap")
  (check-equal? (cap-binop '(set! a0 (+ ca0 a0))) '(set! a0 (+ a0 a0)) "cap-binop: succes-03: add reg cap reg")
  (check-equal? (cap-binop '(set! ca0 (+ a0 a0))) '(set! a0 (+ a0 a0)) "cap-binop: succes-04: add cap reg reg")
  (check-equal? (cap-binop '(set! ca0 (+ ca0 a0))) '(set! ca0 (+ ca0 a0)) "cap-binop: succes-05: add cap cap reg")
  (check-equal? (cap-binop '(set! ca0 (+ ca0 ca0))) '(set! ca0 (+ ca0 a0)) "cap-binop: succes-06: add cap cap cap")

  (check-equal? (cap-binop '(set! a0 (- a0 a0))) '(set! a0 (- a0 a0)) "cap-binop: succes-07: sub reg reg reg")
  (check-equal? (cap-binop '(set! a0 (- a0 ca0))) '(set! a0 (- a0 a0)) "cap-binop: succes-08: sub reg reg cap")
  (check-equal? (cap-binop '(set! a0 (- ca0 a0))) '(set! a0 (- a0 a0)) "cap-binop: succes-09: sub reg cap reg")
  (check-equal? (cap-binop '(set! ca0 (- a0 a0))) '(set! a0 (- a0 a0)) "cap-binop: succes-10: sub cap reg reg")
  (check-equal? (cap-binop '(set! ca0 (- ca0 ca0))) '(set! a0 (- a0 a0)) "cap-binop: succes-11: sub cap cap cap")

  (check-equal? (cap-binop '(set! a0 (* a0 a0))) '(set! a0 (* a0 a0)) "cap-binop: succes-12: mul reg reg reg")
  (check-equal? (cap-binop '(set! a0 (* a0 ca0))) '(set! a0 (* a0 a0)) "cap-binop: succes-13: mul reg reg cap")
  (check-equal? (cap-binop '(set! a0 (* ca0 a0))) '(set! a0 (* a0 a0)) "cap-binop: succes-14: mul reg cap reg")
  (check-equal? (cap-binop '(set! ca0 (* a0 a0))) '(set! a0 (* a0 a0)) "cap-binop: succes-15: mul cap reg reg")
  (check-equal? (cap-binop '(set! ca0 (* ca0 ca0))) '(set! a0 (* a0 a0)) "cap-binop: succes-16: mul cap cap cap")

  (check-equal? (cap-binop '(set! a0 (+ a0 50))) '(set! a0 (+ a0 50)) "cap-binop: succes-17: add reg reg int")
  (check-equal? (cap-binop '(set! ca0 (+ a0 50))) '(set! a0 (+ a0 50)) "cap-binop: succes-18: add cap reg int")
  (check-equal? (cap-binop '(set! a0 (+ ca0 50))) '(set! a0 (+ a0 50)) "cap-binop: succes-19: add reg cap int")
  (check-equal? (cap-binop '(set! ca0 (+ ca0 50))) '(set! ca0 (+ ca0 50)) "cap-binop: succes-20: add cap cap int")
;cap-set
  ;succes
  (check-equal? (cap-set '(set! (cfp - 8) a0)) '(set! (cfp - 8) a0) "cap-set: succes-01: cap addr reg")
  (check-equal? (cap-set '(set! (cfp - 8) ca0)) '(set! (cfp - 8) a0) "cap-set: succes-21: cap addr cap")
  (check-equal? (cap-set '(set! (fp - 8) a0)) '(set! (cfp - 8) a0) "cap-set: succes-03: reg addr reg")
  (check-equal? (cap-set '(set! (fp - 8) ca0)) '(set! (cfp - 8) a0) "cap-set: succes-04: reg addr cap")

  (check-equal? (cap-set '(set! a0 (cfp - 8))) '(set! a0 (cfp - 8)) "cap-set: succes-05: cap addr reg")
  (check-equal? (cap-set '(set! ca0 (cfp - 8))) '(set! a0 (cfp - 8)) "cap-set: succes-06: cap addr cap")
  (check-equal? (cap-set '(set! a0 (fp - 8))) '(set! a0 (cfp - 8)) "cap-set: succes-07: reg addr reg")
  (check-equal? (cap-set '(set! a0 (fp - 8))) '(set! a0 (cfp - 8)) "cap-set: succes-08: reg addr cap")

  (check-equal? (cap-set '(set! ca0 L.foo.1)) '(set! ca0 L.foo.1) "cap-set: succes-09: cap label")
  (check-equal? (cap-set '(set! a0 L.foo.1)) '(set! ca0 L.foo.1) "cap-set: succes-10: reg label")
  
  (check-equal? (cap-set '(set! a0 50)) '(set! a0 50) "cap-set: succes-11: reg in12")
  (check-equal? (cap-set '(set! ca0 50)) '(set! ca0 50) "cap-set: succes-12: cap in12")
  (check-equal? (cap-set '(set! a0 50000)) '(set! a0 50000) "cap-set: succes-13: reg in32")
  (check-equal? (cap-set '(set! ca0 50000)) '(set! a0 50000) "cap-set: succes-14: cap in32")

  (check-equal? (cap-set '(set! a0 a0)) '(set! a0 a0) "cap-set: succes-15: reg reg")
  (check-equal? (cap-set '(set! a0 ca0)) '(set! a0 a0) "cap-set: succes-16: reg cap")
  (check-equal? (cap-set '(set! ca0 a0)) '(set! a0 a0) "cap-set: succes-17: cap reg")
  (check-equal? (cap-set '(set! ca0 ca0)) '(set! ca0 ca0) "cap-set: succes-18: cap cap")
;cap-jump
  ;succes
  (check-equal? (cap-jump '(jump a0)) '(jump ca0) "cap-set: succes-01: jump reg")
  (check-equal? (cap-jump '(jump ca0)) '(jump ca0) "cap-set: succes-02: jump cap")
  
  (check-equal? (cap-jump '(jump (fp - 8))) '(jump (cfp - 8)) "cap-set: succes-03: jump reg addr")
  (check-equal? (cap-jump '(jump (cfp - 8))) '(jump (cfp - 8)) "cap-set: succes-04: jump cap addr")

  (check-equal? (cap-jump '(jump L.foo.1)) '(jump L.foo.1) "cap-set: succes-05: jump label")
;cap-effect
  ;succes
  (check-equal? (cap-effect '(set! ca0 (+ a0 ca0))) '(set! a0 (+ a0 a0)) "cap-effect: succes-01: add cap reg cap")
  (check-equal? (cap-effect '(set! ca0 a0)) '(set! a0 a0) "cap-effect: succes-02: cap reg")

  (check-equal? (cap-effect '(with-label L.foo.1 (set! ca0 a0))) '(with-label L.foo.1 (set! a0 a0)) "cap-effect: succes-03: with reg reg")
  (check-equal? (cap-effect '(jump a0)) '(jump ca0) "cap-effect: succes-04: jump reg")

  (check-equal? (cap-effect '(compare a0 (= a0 a0))) '(compare a0 (= a0 a0)) "cap-effect: succes-05: comp reg reg reg")
  (check-equal? (cap-effect '(compare ca0 (!= a0 a0))) '(compare a0 (!= a0 a0)) "cap-effect: succes-06: comp cap reg reg")
  (check-equal? (cap-effect '(compare a0 (< ca0 a0))) '(compare a0 (< a0 a0)) "cap-effect: succes-07: comp reg cap reg")
  (check-equal? (cap-effect '(compare a0 (> a0 ca0))) '(compare a0 (> a0 a0)) "cap-effect: succes-08: comp reg reg cap")
  (check-equal? (cap-effect '(compare ca0 (<= ca0 ca0))) '(compare a0 (<= a0 a0)) "cap-effect: succes-09: comp cap cap cap")
  (check-equal? (cap-effect '(compare a0 (>= ca0 ca0))) '(compare a0 (>= a0 a0)) "cap-effect: succes-10: comp reg cap cap")

  (check-equal? (cap-effect '(jump-if L.foo.1 (= a0 a0))) '(jump-if L.foo.1 (= a0 a0)) "cap-effect: succes-11: jump-if reg reg")
  (check-equal? (cap-effect '(jump-if L.foo.1 (!= ca0 a0))) '(jump-if L.foo.1 (!= a0 a0)) "cap-effect: succes-12: jump-if cap reg")
  (check-equal? (cap-effect '(jump-if L.foo.1 (< a0 ca0))) '(jump-if L.foo.1 (< a0 a0)) "cap-effect: succes-13: jump-if reg cap")
  (check-equal? (cap-effect '(jump-if L.foo.1 (> ca0 ca0))) '(jump-if L.foo.1 (> a0 a0)) "cap-effect: succes-14: jump-if cap cap")
;set-cap-mode
  ;succes
  (check-equal? (cap-mode-on '(begin (set! a0 a0) (set! a0 (* ca0 a0)) (set! a0 (+ a0 a0)) (set! ca0 a0)))
                '(begin (set! a0 a0) (set! a0 (* a0 a0)) (set! a0 (+ a0 a0)) (set! a0 a0))
                "set-cap-mode: succes-01: simple program")

;|#
  )