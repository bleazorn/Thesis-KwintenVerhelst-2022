#lang racket

(require "common/fvar.rkt"
         "langs/paren-cheri-risc-v.rkt"
         "log.rkt")
(provide access-memory-sub-add-frame-register)

(define (access-address-binop binop)
  (match binop
    ['- -]
    ['+ +]
    [_ #f]))

(define (access-before binop n)
  ((access-address-binop binop) 0 n))

(define (access-after binop n)
  ((access-address-binop binop) 0 (- 0 n)))

(define (access-before-set r binop n)
  `(set! ,r (+ ,r ,(access-before binop n))))

(define (access-after-set r binop n)
  `(set! ,r (+ ,r ,(access-after binop n))))

   
;
;(access-set s)->list? '(set? ...)
;s: set?
(define (access-set s)
  (logln s)
  (match s
    [`(set! (,r ,binop ,n) ,b) #:when (access-address-binop binop) `(,(access-before-set r binop n)
                                                                     (set! (,r - ,0) ,b)
                                                                     ,(access-after-set r binop n))]
    [`(set! ,a (,r ,binop ,n)) #:when (access-address-binop binop) `(,(access-before-set r binop n)
                                                                     (set! ,a (,r - ,0))
                                                                     ,(access-after-set r binop n))]
    [_ `(,s)]))

;
;(access-set-linear s)->list? '(set? ...)
;s: set?
(define (access-set-linear s)
  (match s
    [`(setLinear! (,r ,binop ,n) ,b) #:when (access-address-binop binop) `(,(access-before-set r binop n)
                                                                           (setLinear! (,r - ,0) ,b)
                                                                           ,(access-after-set r binop n))]
    [`(setLinear! ,a (,r ,binop ,n)) #:when (access-address-binop binop) `(,(access-before-set r binop n)
                                                                           (setLinear! ,a (,r - ,0))
                                                                           ,(access-after-set r binop n))]
    [_ `(,s)]))

;
;(access-jump s)->list? '(set? ...)
;s: set?
(define (access-jump s)
  (match s
    [`(jump (,r ,binop ,n)) #:when (access-address-binop binop) `(,(access-before-set r binop n)
                                                                  `(set! ct5 (,r - ,0))
                                                                  ,(access-after-set r binop n)
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
(define/contract (access-memory-sub-add-frame-register p) (-> paren-cheri-risc-v? paren-cheri-risc-v?)
    (match p
    [`(begin ,i ,s ...) `(begin ,i ,@(foldl (lambda (set n) (append n (access-sets set))) '() s))]
    [_ #f]))
