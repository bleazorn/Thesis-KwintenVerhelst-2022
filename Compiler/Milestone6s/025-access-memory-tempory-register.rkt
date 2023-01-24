#lang racket

(require "common/fvar.rkt")
(provide access-memory-tempory-register)

(define (access-address-binop binop)
  (match binop
    ['- -]
    ['+ +]
    [_ #f]))

(define (access-tempory-set r binop n)
  `(set! ct10 (+ ,r ,(access-address-binop binop) 0 n)))

;
;(access-set s)->list? '(set? ...)
;s: set?
(define (access-set s)
  (match s
    [`(set! (,r ,binop ,n) ,b) #:when (access-address-binop binop) `(,(access-tempory-set r binop n)
                                                                     (set! (ct10 - 0) ,b))]
    [`(set! ,a (,r ,binop ,n)) #:when (access-address-binop binop) `(,(access-tempory-set r binop n)
                                                                     (set! ,a (ct10 - 0)))]
    [_ `(,s)]))

;
;(access-set-linear s)->list? '(set? ...)
;s: set?
(define (access-set-linear s)
  (match s
    [`(setLinear! (,r ,binop ,n) ,b) #:when (access-address-binop binop) `(,(access-tempory-set r binop n)
                                                                           (setLinear! (ct10 - 0) ,b))]
    [`(setLinear! ,a (,r ,binop ,n)) #:when (access-address-binop binop) `(,(access-tempory-set r binop n)
                                                                           (setLinear! ,a (ct10 - 0)))]
    [_ `(,s)]))

;
;(access-jump s)->list? '(set? ...)
;s: set?
(define (access-jump s)
  (match s
    [`(jump (,r ,binop ,n)) #:when (access-address-binop binop) `(,(access-tempory-set r binop n)
                                                                  (set! ct5 (ct10 - 0))
                                                                  (jump ct5))]
    [_ `(,s)]))

;
;(access-with-label s)->list? '(set? ...)
;s: set?
(define (access-with-label s)
  (match s
    [`(with-label ,l ,a) (let ([sets (access-sets a)])
                           (cons `(with-label ,l ,(car sets)) (cdr sets)))]
    [_ #f]))

;
;(generate-seting s) -> list? '(set? ...)
; s: set?
(define (access-sets s)
  (match s 
    [`(set! ,a ,b) (access-set s)]                                 ;set
    [`(setLinear! ,a ,b) (access-set-linear s)]                    ;setLinear
    [`(with-label ,l ,a) (access-with-label s)]                    ;set label 
    [`(jump ,l) (access-jump s)]                                   ;unconditional jump
    [`(compare ,a (,relop ,b ,c)) `((compare ,a (,relop ,b ,c)))]  ;compare
    [`(jump-if ,l (,relop ,b ,c)) `((jump-if ,l (,relop ,b ,c)))]  ;conditional jump
    [`(perm ,r ,ps) `((perm ,r ,ps))]                              ;perms
    [`(bound ,r ,bas ,len) `((bound ,r ,bas ,len))]                ;bounds
    [`(seal ,r ... ,t) `((seal ,@r ,t))]                           ;seal
    [`(unseal ,r ... ,t) `((unseal ,@r ,t))]                       ;unseal
    [`(split ,a ,b ,c ,d) `((split ,a ,b ,c ,d))]                  ;split
    [`(splice ,a ,b ,c ,d) `((splice ,a ,b ,c ,d))]                ;splice
    [`(sentry ,r) `((sentry ,r))]                                  ;sentry
    [`(invoke ,a ,b) `((invoke ,a ,b))]                            ;invoke
    [_ #f]))

;Generates paren-cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-cheri-risc-v p) -> string?/boolean?
; p: any?
(define (access-memory-tempory-register p)
    (match p
    [`(begin ,s ...) `(begin ,@(foldl (lambda (set n) (append n (access-sets set))) '() s))]
    [_ #f]))