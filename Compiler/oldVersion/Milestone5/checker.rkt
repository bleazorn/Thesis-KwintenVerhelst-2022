#lang racket

(provide check-paren-cheri-risc-v)

(module+ test
  (require rackunit))

;Returns given symbol if it is a name for a register in paren-cheri-risc-v, otherwise gives error message
;(check-reg res) -> symbol?
;res : symbol?
(define (check-reg res)
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
               [_ (error 'check-reg "~a ~a" res "is not a register for paren-cheri-risc-v")]))])
      (raise-argument-error 'check-reg "symbol?" res)))

;Returns given argument if it is a triv in paren-cheri-risc-v, otherwise gives error message
;(check-triv t) -> integer?/symbol?
;t : any?
(define (check-triv t)
  (match t
    [i #:when (and (integer? i) (and (< i 2048) (>= i -2048))) i]
    [r #:when (check-reg r) r]
    [_ (error 'check-binop "~a ~a" t "is not a triv for paren-cheri-risc-v")]))

;Returns given instruction set if it is an operation in paren-cheri-risc-v, otherwise gives error message
;(check-binop b) -> any?
;b : any?
(define (check-binop b)
  (match b
    [`(set! ,a (+ ,b ,c)) `(set! ,(check-reg a) (+ ,(check-reg b) ,(check-triv c)))]
    [`(set! ,a (* ,b ,c)) `(set! ,(check-reg a) (* ,(check-reg b) ,(check-reg c)))]
    [_ (error 'check-binop "~a ~a" b "is not a binop for paren-cheri-risc-v")]))

;Returns given argument if it is an instruction set in paren-cheri-risc-v, otherwise gives error message
;(check-set s) -> any?
;s : any?
(define (check-set s)
  (match s
    [`(set! ,a (,binop ,b ,c)) (check-binop s)]
    [`(set! ,a ,b) `(set! ,(check-reg a) ,(check-triv b))]
    [_ (error 'check-binop "~a ~a" s "is not an instruction set for paren-cheri-risc-v")]))

(define (check-paren-cheri-risc-v-syntax p)
  (match p
    [`(begin ,s ...) `(begin ,@(map (lambda (s) (check-set s)) s))]
    [_ (error "wrong syntax")]))

(define (check-paren-cheri-risc-v-init p)
  #t)

(define (check-paren-cheri-risc-v p)
  (check-paren-cheri-risc-v-syntax p))


(module+ test
  (define (check-equal-catch-error e1 e2 text)
    (with-handlers ([exn:fail (λ (e) (displayln "got an error"))])
      (check-equal? e1 e2 text)))
    
;check-reg
  ;succes
  (check-equal? (check-reg 'a0) 'a0 "check-reg: succes-1: bestaand register")
  
  ;failure
  ;(check-equal? (check-reg 'v0) error "check-reg: failure-1: onbestaand register")
  ;(check-equal? (check-reg "a0") error "check-reg: failure-2: geen symbool")
;check-triv
  ;succes
  (check-equal? (check-triv 'a0) 'a0 "check-triv: succes-1: bestaand register")
  (check-equal? (check-triv 5) 5 "check-triv: succes-2: integer 12bit")
  
  ;failure
  ;(check-equal? (check-triv 2048) error "check-triv: failure-1: te groot integer")
  ;(check-equal? (check-reg "5") error "check-triv: failure-2: geen triv")
;check-binop
  ;succes
  (check-equal? (check-binop '(set! a0 (+ a0 50))) '(set! a0 (+ a0 50)) "check-binop: succes-1: addition met integer")
  (check-equal? (check-binop '(set! a0 (+ a0 a0))) '(set! a0 (+ a0 a0)) "check-binop: succes-2: addition met register")
  (check-equal? (check-binop '(set! a0 (* a0 a0))) '(set! a0 (* a0 a0)) "check-binop: succes-3: multiplier met register")
#|
  ;failure
  (check-equal? (check-binop '(set! a0 (* a0 50))) error "check-binop: failure-1: multiplier met integer")
  
  (check-equal? (check-binop '(set! a0 (+ 50 50))) error "check-binop: failure-2: addition met verkeerde triv")
  (check-equal? (check-binop '(set! a0 (+ 50 a0))) error "check-binop: failure-3: addition met verkeerde triv")
  (check-equal? (check-binop '(set! a0 (* 50 a0))) error "check-binop: failure-4: multiplier met verkeerde triv")

  (check-equal? (check-binop '(set! 50 (+ a0 50))) error "check-binop: failure-5: addition met verkeerde triv")
  (check-equal? (check-binop '(set! 50 (+ a0 a0))) error "check-binop: failure-6: addition met verkeerde triv")
  (check-equal? (check-binop '(set! 50 (* a0 a0))) error "check-binop: failure-7: multiplier met verkeerde triv")

  (check-equal? (check-binop '(set! a0 (- a0 50))) error "check-binop: failure-8: niet bestaande operation")
|#
;check-set
  ;succes
  (check-equal? (check-set '(set! a0 50)) '(set! a0 50) "check-set: succes-1: voeg integer toe")
  (check-equal? (check-set '(set! a0 a0)) '(set! a0 a0) "check-set: succes-2: copy register")
  (check-equal? (check-set '(set! a0 (+ a0 50))) '(set! a0 (+ a0 50)) "check-set: succes-3:  addition met integer")
  (check-equal? (check-set '(set! a0 (+ a0 a0))) '(set! a0 (+ a0 a0)) "check-set: succes-4: addition met register")
  (check-equal? (check-set '(set! a0 (* a0 a0))) '(set! a0 (* a0 a0)) "check-set: succes-5: multiplier met register")
  
  ;failure
  ;(check-equal? (check-set '(set! 50 50)) error "check-set: failure-1: copy met verkeerde triv")
  ;(check-equal? (check-set '(set! 50 a0)) error "check-set: failure-2: copy met verkeerde triv")
;check-paren-cheri-risc-v
  (check-equal? (check-paren-cheri-risc-v '(begin (set! a0 (+ a0 450)))) '(begin (set! a0 (+ a0 450))) "check-paren-cheri-risc-v: succes-1: een enkele instructie")
  (check-equal? (check-paren-cheri-risc-v-syntax
                 '(begin
                    (set! a0 50)
                    (set! t0 a0)
                    (set! t0 (+ t0 t0))
                    (set! sp t0)
                    (set! sp (* sp sp))
                    (set! a1 2000)))
                '(begin
                   (set! a0 50)
                   (set! t0 a0)
                   (set! t0 (+ t0 t0))
                   (set! sp t0)
                   (set! sp (* sp sp))
                   (set! a1 2000))
                "check-paren-cheri-risc-v: succes-2: meerdere instructies")
  #|
  ;failure
  (check-equal? (check-paren-cheri-risc-v '(set! a0 (+ a0 450))) error "check-paren-cheri-risc-v: failure-1: vergeet initialisatie")
  (check-equal? (check-paren-cheri-risc-v-syntax
                 '(begin
                    (set! a0 50)
                    (set! t0 a0)
                    (set! t0 (- t0 t0))
                    (set! sp t0)
                    (set! sp (* sp sp))
                    (set! a1 2000)))
                error
                "check-paren-cheri-risc-v: failure-2: meerdere instructies met 1 slechte instructie")
|#
  )
