#lang racket

(provide current-register-set
         current-stack-base-pointer-register
         current-frame-base-pointer-register
         frame-base-pointer-register?
         current-auxiliary-registers
         current-return-value-register
         current-return-value-register
         current-assignable-registers
         current-parameter-registers
         current-return-address-register
         current-jump-register
         current-stack-register
         current-global-register
         current-invoke-jump-register
         current-invoke-data-register
         current-seal-register
         current-seal-location-register
         current-seal-got-call-register
         register?
         isNonCapRegister?
         isCapability?
         special-register?
         makeReg
         makeCap
         addr-binop?
         addr?
         isRegAddress?
         isCapAddress?)

(module+ test
  (require rackunit))


(define (current-capReg-set)
  `(cnull cra csp cgp ctp cfp
          ,@(build-list 8 (lambda (a) (string->symbol (format "ca~a" a))))
          ,@(build-list 12 (lambda (a) (string->symbol (format "cs~a" a))))
          ,@(build-list 7 (lambda (a) (string->symbol (format "ct~a" a))))
          ,@(build-list 32 (lambda (a) (string->symbol (format "c~a" a))))))

(define (current-intReg-set)
  `(zero ra sp gp tp fp
          ,@(build-list 8 (lambda (a) (string->symbol (format "a~a" a))))
          ,@(build-list 12 (lambda (a) (string->symbol (format "s~a" a))))
          ,@(build-list 7 (lambda (a) (string->symbol (format "t~a" a))))
          ,@(build-list 32 (lambda (a) (string->symbol (format "x~a" a))))))

(define current-register-set
  (make-parameter (append (current-capReg-set) (current-intReg-set))))

(define current-frame-base-pointer-register
  (make-parameter 'cfp))

(define (frame-base-pointer-register? v)
  ;(or (equal? v (current-stack-base-pointer-register)) (equal? v (current-global-register)) (equal? v 'fp) (equal? v (current-stack-register))))
  (register? v))

(define current-stack-base-pointer-register
  (make-parameter 'cfp))

(define current-auxiliary-registers
  (make-parameter '(t5 t6)))

(define current-return-value-register
  (make-parameter 'a0))

(define current-assignable-registers
  (make-parameter (build-list 5 (lambda (a) (string->symbol (format "t~a" a))))))

(define current-parameter-registers
  (make-parameter (build-list 7 (lambda (a) (string->symbol (format "a~a" (add1 a)))))))

(define current-return-address-register
  (make-parameter 'cra))

(define current-jump-register
  (make-parameter 'cs11))

(define current-stack-register
  (make-parameter 'cs10))

(define current-global-register
  (make-parameter 'cgp))

(define current-invoke-jump-register
  (make-parameter 'ct0))
  
(define current-invoke-data-register
  (make-parameter 'ct6))

(define current-seal-register
  (make-parameter 'cs2))

(define current-seal-location-register
  (make-parameter 'cs1))

(define current-seal-got-call-register
  (make-parameter 'cs1))

(define special-registers
  (make-parameter '(pcc)))


;Returns given symbol if it is a name for a register in paren-cheri-risc-v, otherwise returns false
;(check-reg res) -> symbol?/boolean?
;res : symbol?
(define (register? res)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (or (isCapability? res) (isNonCapRegister? res) (x86Register? res))))

(define (isNonCapRegister? res)
  (if (symbol? res)
      (match res
        ['zero 'zero]
        ['ra 'ra]
        ['sp 'sp]
        ['gp 'gp]
        ['tp 'tp]
        ['fp 'fp]
        ['pc 'pc]
        [_ (let* ([s (symbol->string res)]
                  [l (substring s 0 1)]
                  [n (substring s 1)])
             (match `(,l ,n)
               [`("x" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 32))) res]
               [`("a" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 8))) res]
               [`("s" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 12))) res]
               [`("t" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 7))) res]
               [_ #f]))])
      #f))

(define (isCapability? cap)
  (if (symbol? cap)
      (match cap
        ['cnull 'cnull]
        ['cra 'cra]
        ['csp 'csp]
        ['cgp 'cgp]
        ['ctp 'ctp]
        ['cfp 'cfp]
        ['pcc 'pcc]
        [_ (let* ([s (symbol->string cap)]
                  [l (substring s 0 1)]
                  [n (substring s 1)])
             (match `(,l ,n)
               [`("c" ,n) #:when (and (string->number n) (and (<= 0 (string->number n)) (< (string->number n) 32))) cap]
               [_ (let* ([cl (substring s 0 2)]
                         [cn (substring s 2)])
                    (match `(,cl ,cn)
                      [`("ca" ,cn) #:when (and (string->number cn) (and (<= 0 (string->number cn)) (< (string->number cn) 8))) cap]
                      [`("cs" ,cn) #:when (and (string->number cn) (and (<= 0 (string->number cn)) (< (string->number cn) 12))) cap]
                      [`("ct" ,cn) #:when (and (string->number cn) (and (<= 0 (string->number cn)) (< (string->number cn) 7))) cap]
                      [_ #f]))]))])
      #f))

(define (x86Register? reg)
  (member reg '(r9 r15 rax rbx rbp rdi rsi)))

(define (special-register? reg)
  (member (makeCap reg) (special-registers)))

(define (makeCap reg)
  (if (isNonCapRegister? reg)
      (match (symbol->string reg)
        ["zero" 'cnull]
        ["pc" 'pcc]
        [x #:when (equal? (substring x 0 1) "x") (string->symbol (format "c~a" (substring x 1)))]
        [x (string->symbol (string-append "c" x))])
      reg))

(define (makeReg cap)
  (if (isCapability? cap)
      (match (symbol->string cap)
        ["cnull" 'zero]
        ["pcc" 'pc]
        [c #:when (and (equal? (substring c 0 1) "c") (string->number (substring c 1 2))) (string->symbol (format "x~a" (substring c 1)))]
        [c (string->symbol (substring c 1))])
      cap))

(define (addr-binop? binop)
  (match binop
    ['+ #t]
    ['- #t]
    [_ #f]))


;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (addr? a)
  (match a
    [`(,r ,binop ,n) #:when (and (register? r) (integer? n) (addr-binop? binop)) a]
    [_ #f]))

;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (isRegAddress? a)
  (match a
    [`(,r ,binop ,n) #:when (and (isNonCapRegister? r) (integer? n) (addr-binop? binop)) a]
    [_ #f]))

;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (isCapAddress? a)
  (match a
    [`(,r ,binop ,n) #:when (and (isCapability? r) (integer? n) (addr-binop? binop)) a]
    [_ #f]))




