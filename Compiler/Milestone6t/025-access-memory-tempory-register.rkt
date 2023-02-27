#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
         "common/assembly.rkt"
         "langs/paren-cheri-risc-v.rkt")
(provide access-memory-tempory-register)

(define (access-tempory-set r binop n)
  (cond [(int12? n) `((set! ,(current-stack-register) (,binop ,r ,n)))]
        [else (let ([temp-reg (car (current-auxiliary-registers))])
                `((set! ,temp-reg ,n)
                  (set! ,(current-stack-register) (,binop ,r ,temp-reg))))]))

;
;(access-set s)->list? '(set? ...)
;s: set?
(define (access-set s)
  (match s
    [`(set! (,r ,binop ,n) ,b) #:when (addr-binop? binop) `(,@(access-tempory-set r binop n)
                                                            (set! (,(current-stack-register) - 0) ,b))]
    [`(set! ,a (,r ,binop ,n)) #:when (addr-binop? binop) `(,@(access-tempory-set r binop n)
                                                            (set! ,a (,(current-stack-register) - 0)))]
    [_ `(,s)]))

;
;(access-set-linear s)->list? '(set? ...)
;s: set?
(define (access-set-linear s)
  (match s
    [`(setLinear! (,r ,binop ,n) ,b) #:when (addr-binop? binop) `(,@(access-tempory-set r binop n)
                                                                  (setLinear! (,(current-stack-register) - 0) ,b))]
    [`(setLinear! ,a (,r ,binop ,n)) #:when (addr-binop? binop) `(,@(access-tempory-set r binop n)
                                                                  (setLinear! ,a (,(current-stack-register) - 0)))]
    [_ `(,s)]))

;
;(access-jump s)->list? '(set? ...)
;s: set?
(define (access-jump s)
  (match s
    [`(jump (,r ,binop ,n)) #:when (addr-binop? binop) `(,@(access-tempory-set r binop n)
                                                         (set! ,(current-jump-register) (,(current-stack-register) - 0))
                                                         (jump ,(current-jump-register)))]
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
    [`(set-addr! ,a ,b) `((set-addr! ,a ,b))]
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
(define/contract (access-memory-tempory-register p) (-> paren-cheri-risc-v? paren-cheri-risc-v?)
    (match p
    [`(begin ,i ,s ...) `(begin ,i ,@(foldl (lambda (set n) (append n (access-sets set))) '() s))]
    [_ #f]))
