#lang racket

(require "common.rkt")
(provide generate-cheri-risc-v)

(module+ test
  (require rackunit))

#|
move register      addi rd, rs, 0    (register => register)
insert integer     addi rd, zero, +imm (integer=>register) (12bit (signed))
addition reg       add rd, rs1, rs2  (register + register => register)
addition int       addi rd, rs, +imm (integer + register => register) (12bit (signed))
multiplication     mul  rd, rs1, rs2 (register * register => register)
|#

;If given argument t is an integer or a cheri-risc-v register it returns it in a string, otherwise it returns false
;(generate-triv t) -> string?/boolean?
; t: any?
(define (generate-triv t)
  (match t
    [i #:when (integer? i) (~a i)]
    [r #:when (symbol? r) (~a r)]
    [_ #f]))

;Generates the addition cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-add add) -> string?/boolean?
; t: any?
(define (generate-add add)
  (match add
    [`(set! ,a (+ ,b ,c)) #:when (integer? c) (string-append "addi " (generate-triv a) ", " (generate-triv b) ", " (generate-triv c))]
    [`(set! ,a (+ ,b ,c)) (string-append "add " (generate-triv a) ", " (generate-triv b) ", " (generate-triv c))]
    [_ #f]))

;Generates the addition or multiplication cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-binop bin) -> string?/boolean?
; bin: any?
(define (generate-binop bin)
 (match bin
   [`(set! ,a (* ,b ,c)) (string-append "mul " (generate-triv a) ", " (generate-triv b) ", " (generate-triv c))]
   [`(set! ,a (+ ,b ,c)) (generate-add bin)]
   [_ #f]))

;Generates the addition/multiplication or copy cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-set s) -> string?/boolean?
; s: any?
(define (generate-set s)
  (match s
    [`(set! ,a (,binop ,b ,c)) (generate-binop s)]
    [`(set! ,a ,b) #:when (integer? b) (string-append "addi " (generate-triv a) ", x0, " (generate-triv b))]
    [`(set! ,a ,b) (string-append "addi " (generate-triv a) ", " (generate-triv b) ", 0")]
    [_ #f]))

;Generates paren-cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-cheri-risc-v p) -> string?/boolean?
; p: any?
(define (generate-cheri-risc-v p)
    (match p
    [`(begin ,s ...) (foldl (lambda (s string) (string-append string indent (generate-set s) "\n")) "" s)]
    [_ #f]))

(module+ test
;generate-triv
  ;succes
  (check-equal? (generate-triv 'a0) "a0" "generate-triv: succes-1: bestaand register")
  (check-equal? (generate-triv 5) "5" "generate-triv: succes-2: integer 12bit")
  
  ;failure
  (check-equal? (generate-triv "5") #f "generate-triv: failure-2: geen triv")
;generate-add
  ;succes
  (check-equal? (generate-add '(set! a0 (+ a0 50))) "addi a0, a0, 50" "generate-add: succes-1: addition met integer")
  (check-equal? (generate-add '(set! a0 (+ a0 a0))) "add a0, a0, a0" "generate-add: succes-2: addition met register")

  ;failure
  (check-equal? (generate-add '(set! a0 (* a0 50))) #f "generate-add: failure-1: verkeerde operatie")
  
  (check-equal? (generate-add '(set! a0 (+ 50 50))) "addi a0, 50, 50" "generate-add: failure-2: addition met verkeerde triv - checker faalde")
  (check-equal? (generate-add '(set! a0 (+ 50 a0))) "add a0, 50, a0" "generate-add: failure-3: addition met verkeerde triv - checker faalde")

  (check-equal? (generate-add '(set! 50 (+ a0 50))) "addi 50, a0, 50" "generate-add: failure-4: addition met verkeerde triv - checker faalde")
  (check-equal? (generate-add '(set! 50 (+ a0 a0))) "add 50, a0, a0" "generate-add: failure-5: addition met verkeerde triv - checker faalde")

;generate-binop
  ;succes
  (check-equal? (generate-binop '(set! a0 (+ a0 50))) "addi a0, a0, 50" "generate-binop: succes-1: addition met integer")
  (check-equal? (generate-binop '(set! a0 (+ a0 a0))) "add a0, a0, a0" "generate-binop: succes-2: addition met register")
  (check-equal? (generate-binop '(set! a0 (* a0 a0))) "mul a0, a0, a0" "generate-binop: succes-3: multiplier met register")

  ;failure
  (check-equal? (generate-binop '(set! a0 (* a0 50))) "mul a0, a0, 50" "generate-binop: failure-1: multiplier met integer - checker faalde")
  (check-equal? (generate-binop '(set! a0 (* 50 a0))) "mul a0, 50, a0" "generate-binop: failure-4: multiplier met verkeerde triv - checker faalde")
  (check-equal? (generate-binop '(set! 50 (* a0 a0))) "mul 50, a0, a0" "generate-binop: failure-7: multiplier met verkeerde triv - checker faalde")

  (check-equal? (generate-binop '(set! a0 (- a0 50))) #f "generate-binop: failure-8: niet bestaande operation")

;generate-set
  ;succes
  (check-equal? (generate-set '(set! a0 50)) "addi a0, zero, 50" "generate-set: succes-1: voeg integer toe")
  (check-equal? (generate-set '(set! a0 t0)) "addi a0, t0, 0" "generate-set: succes-2: copy register")
  (check-equal? (generate-set '(set! a0 (+ t0 50))) "addi a0, t0, 50" "generate-set: succes-3:  addition met integer")
  (check-equal? (generate-set '(set! a0 (+ t0 t1))) "add a0, t0, t1" "generate-set: succes-4: addition met register")
  (check-equal? (generate-set '(set! a0 (* t0 t1))) "mul a0, t0, t1" "generate-set: succes-5: multiplier met register")
  
  ;failure
  (check-equal? (generate-set '(set! 50 50)) "addi 50, zero, 50" "generate-set: failure-1: copy met verkeerde triv - checker faalde")
  (check-equal? (generate-set '(set! 50 a0)) "addi 50, a0, 0" "generate-set: failure-2: copy met verkeerde triv - checker faalde")
  (check-equal? (generate-set '(st! a0 t0)) #f "generate-set: failure-3: verkeerd symbool")
  (check-equal? (generate-set '(st! a0)) #f "generate-set: failure-4: te weinig argumenten")
  (check-equal? (generate-set 'a) #f "generate-set: failure-5: verkeerd argument")
;generate-cheri-risc-v
  (check-equal? (generate-cheri-risc-v '(begin (set! a0 (+ t0 450)))) "    addi a0, t0, 450\n" "generate-cheri-risc-v: succes-1: een enkele instructie")
  (check-equal? (generate-cheri-risc-v
                 '(begin
                    (set! a0 50)
                    (set! t0 a0)
                    (set! t0 (+ t0 t0))
                    (set! sp t0)
                    (set! sp (* sp sp))
                    (set! a1 2000)))
                "    addi a0, zero, 50\n    addi t0, a0, 0\n    add t0, t0, t0\n    addi sp, t0, 0\n    mul sp, sp, sp\n    addi a1, zero, 2000\n"
                "generate-cheri-risc-v: succes-2: meerdere instructies")
  
;failure 
  (check-equal? (generate-cheri-risc-v '(set! a0 (+ a0 450))) #f "generate-cheri-risc-v: failure-1: vergeet initialisatie")
 #|
  (check-equal? (generate-cheri-risc-v
                 '(begin
                    (set! a0 50)
                    (set! t0 a0)
                    (set! t0 (- t0 t0))
                    (set! sp t0)
                    (set! sp (* sp sp))
                    (set! a1 2000)))
                error
                "generate-cheri-risc-v: failure-2: meerdere instructies met 1 slechte instructie")
|#
  )
