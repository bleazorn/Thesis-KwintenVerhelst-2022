#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
         "common/assembly.rkt"
         "langs/paren-cheri-risc-v.rkt"
         "log.rkt")
(provide access-memory-sub-add-frame-register)


(define (opposite-binop binop)
  (match binop
    ['+ '-]
    ['- '+]
    [_ #f]))


(define (access-set-before-after set-reg set r binop n)
  (cond [(int12? n) `((set! ,r (,binop ,r ,n))
                      ,set
                      (set! ,r (,(opposite-binop binop) ,r ,n)))]
        [else  (let ([temp-reg (car (remove (makeReg set-reg) (current-auxiliary-registers)))])
                 `((set! ,temp-reg ,n)
                   (set! ,r (,binop ,r ,temp-reg))
                   ,set
                   (set! ,r (,(opposite-binop binop) ,r ,temp-reg))))]))

   
;
;(access-set s)->list? '(set? ...)
;s: set?
(define (access-set s)
  (logln s)
  (match s
    [`(set! (,r ,binop ,n) ,b) #:when (addr-binop? binop) (access-set-before-after b `(set! (,r - ,0) ,b) r binop n)]
    [`(set! ,a (,r ,binop ,n)) #:when (addr-binop? binop) (access-set-before-after a `(set! ,a (,r - ,0)) r binop n)]
    [_ `(,s)]))

;
;(access-set-linear s)->list? '(set? ...)
;s: set?
(define (access-set-linear s)
  (match s
    [`(setLinear! (,r ,binop ,n) ,b) #:when (addr-binop? binop) (access-set-before-after b `(setLinear! (,r - ,0) ,b) r binop n)]
    [`(setLinear! ,a (,r ,binop ,n)) #:when (addr-binop? binop) (access-set-before-after a `(setLinear! ,a (,r - ,0)) r binop n)]
    [_ `(,s)]))

;
;(access-jump s)->list? '(set? ...)
;s: set?
(define (access-jump s)
  (match s
    [`(jump (,r ,binop ,n)) #:when (addr-binop? binop) `(,@(access-set-before-after (current-jump-register) `(set! ,(current-jump-register) (,r - ,0)) r binop n)
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
