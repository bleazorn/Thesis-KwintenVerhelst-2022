#lang racket

(provide current-register-set
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
         register?
         isNonCapRegister?
         isCapability?
         makeReg
         makeCap
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

(define (current-register-set)
  (append (current-capReg-set) (current-intReg-set)))

(define (current-frame-base-pointer-register)
  'cfp)

(define (frame-base-pointer-register? v)
  (equal? v (current-frame-base-pointer-register)))

(define (current-auxiliary-registers)
  '(t5 t6))

(define (current-return-value-register)
  'a0)

(define (current-assignable-registers)
  (build-list 5 (lambda (a) (string->symbol (format "t~a" a)))))

(define (current-parameter-registers)
  '());(build-list 8 (lambda (a) (string->symbol (format "a~a" a)))))

(define (current-return-address-register)
  'ra)

(define (current-jump-register)
  'cs11)

(define (current-stack-register)
  'cs10)

;Returns given symbol if it is a name for a register in paren-cheri-risc-v, otherwise returns false
;(check-reg res) -> symbol?/boolean?
;res : symbol?
(define (register? res)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (or (isCapability? res) (isNonCapRegister? res))))

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


;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (addr? a)
  (match a
    [`(,r - ,n) #:when (and (register? r) (integer? n)) a]
    [_ #f]))

;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (isRegAddress? a)
  (match a
    [`(,r - ,n) #:when (and (isNonCapRegister? r) (integer? n)) a]
    [_ #f]))

;Returns given symbol if it is an address, otherwise returns false. address in stile (isRegister? - integer?)
;(isAddress? a)->symbol?/boolean?
;a:any/c
(define (isCapAddress? a)
  (match a
    [`(,r - ,n) #:when (and (isCapability? r) (integer? n)) a]
    [_ #f]))


