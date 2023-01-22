#lang racket

(require "common/register.rkt"
         "common/aloc.rkt"
         "common/assembly.rkt")
(provide generate-risc-v)

(module+ test
  (require rackunit))

#|
move register      addi rd, rs, 0    (register => register)
insert integer     addi rd, zero, +imm (integer=>register) (12bit (signed))
addition reg       add rd, rs1, rs2  (register + register => register)
addition int       addi rd, rs, +imm (integer + register => register) (12bit (signed))
multiplication     mul  rd, rs1, rs2 (register * register => register)
load in memory     lw rd, addr       (register => addr)  (addr: ((reg - int) == -int(reg)))
store in memory    sw rd, addr       (addr => register)

jal reg, offset			jump naar offset (20bits) (reg=pc+4)
beq reg, reg, label		jump =
bne reg, reg, label		jump !=
blt reg, reg, label		jump <
bgt reg, reg, label		jump >
ble reg, reg, label		jump <=
bge reg, reg, label		jump >=

slt reg, reg, reg		compares < and result in first reg 0 false | 1 true
slti reg reg, int12		compares < with 12bit int
sltu reg, reg, reg		compares < unsigned
sltiu reg, reg, int12		compares < unsigned 12 bit integer
|#

(define (generate-addr a)
  (match a
    [`(,r - ,n) (format "-~a(~a)" n r)]
    [a a]))

(define (int12? n)
  (and (integer? n) (and (< n 2048) (>= n -2048))))

;Generates the addition or multiplication cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-binop bin) -> string?/boolean?
; bin: any?
(define (generate-binop bin)
 (match bin
   [`(set! ,a (+ ,b ,c)) #:when (int12? c) (indent-instr (format "addi ~a, ~a, ~a" a b c))]       ;reg reg int
   [`(set! ,a (+ ,b ,c)) (indent-instr (format "add ~a, ~a, ~a" a b c))]                          ;reg reg reg
   [`(set! ,a (- ,b ,c)) (indent-instr (format "sub ~a, ~a, ~a" a b c))]                          ;reg reg reg
   [`(set! ,a (* ,b ,c)) (indent-instr (format "mul ~a, ~a, ~a" a b c))]                          ;mul
   [_ #f]))

;
;(generate-set s)->string?/boolean?
;s: set?
(define (generate-set s)
  (match s
    [`(set! (,r - ,n) ,b) (indent-instr (format "sw ~a, ~a(~a)" b n r))]                                        ;store
    [`(set! ,a (,r - ,n)) (indent-instr (format "lw ~a, ~a(~a)" a n r))]                                        ;load
    [`(set! ,a (,binop ,b ,c)) (generate-binop s)]                                                              ;binop
    [`(set! ,a ,b) #:when (int12? b) (indent-instr (format "addi ~a, zero, ~a" a b))]                           ;add int12
    [`(set! ,a ,b) #:when (and (integer? b) (not (int12? b))) (indent-instr (format "li ~a, ~a" a b))]          ;add int32
    [`(set! ,a ,b) #:when (label? b) (indent-instr (format "la ~a, ~a" a b))]                                   ;add label
    [`(set! ,a ,b) #:when (and (register? a) (register? b)) (indent-instr (format "addi ~a, ~a, 0" a b))]       ;move
    [_ #f]))

;
;(generate-jump j)->string?/boolean?
;j: set?
(define (generate-jump j)
  (match j
    [`(jump ,l) #:when (register? l) (indent-instr (format "jr ~a" l))]            ;jump register
    [`(jump (,r - ,n)) (string-append (indent-instr (format "lwu s9, ~a(~a)" n r)) ;jump addr
                                      (indent-instr (format "jr s9")))]
    [`(jump ,l) #:when (label? l) (indent-instr (format "j ~a" l))]                ;jump label
    [_ #f]))

;
;(generate-relop r) -> string?/boolean?
;r: any?
(define (generate-relop r)
  (match r
    [`(compare ,a (= ,b ,c))  (string-append (indent-instr (format "sub ~a, ~a, ~a" a b c)) (indent-instr (format "sltiu ~a, ~a, 1" a a)))]
    [`(compare ,a (!= ,b ,c)) (string-append (indent-instr (format "sub ~a, ~a, ~a" a b c)) (indent-instr (format "sltu ~a, x0, ~a" a a)))]
    [`(compare ,a (< ,b ,c))  (indent-instr (format "slt ~a, ~a, ~a" a b c))]
    [`(compare ,a (> ,b ,c))  (indent-instr (format "slt ~a, ~a, ~a" a c b))]
    [`(compare ,a (<= ,b ,c)) (string-append (indent-instr (format "sub ~a, ~a, ~a" a b c)) (indent-instr (format "slti ~a, ~a, 1" a a)))]
    [`(compare ,a (>= ,b ,c)) (string-append (indent-instr (format "sub ~a, ~a, ~a" a c b)) (indent-instr (format "slti ~a, ~a, 1" a a)))]
    [_ #f]))

;
;(generate-jump-if j) -> string?/boolean?
;j: any?
(define (generate-jump-if j)
  (match j
    [`(jump-if ,l (= ,b ,c))  (indent-instr (format "beq ~a, ~a, ~a" b c l))]
    [`(jump-if ,l (!= ,b ,c)) (indent-instr (format "bne ~a, ~a, ~a" b c l))]
    [`(jump-if ,l (< ,b ,c))  (indent-instr (format "blt ~a, ~a, ~a" b c l))]
    [`(jump-if ,l (> ,b ,c))  (indent-instr (format "bgt ~a, ~a, ~a" b c l))]
    [`(jump-if ,l (<= ,b ,c)) (indent-instr (format "ble ~a, ~a, ~a" b c l))]
    [`(jump-if ,l (>= ,b ,c)) (indent-instr (format "bge ~a, ~a, ~a" b c l))]
    [_ #f]))

;Generates the addition/multiplication or copy cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-seting s) -> string?/boolean?
; s: any?
(define (generate-sets s)
  (match s 
    [`(set! ,a ,b) (generate-set s)]                             ;set
    [`(with-label ,l ,a) (format "~a:\n~a" l (generate-sets a))] ;set label
    [`(jump ,l) (generate-jump s)]                               ;unconditional jump
    [`(compare ,a (,relop ,b ,c)) (generate-relop s)]            ;compare
    [`(jump-if ,l (,relop ,b ,c)) (generate-jump-if s)]          ;conditional jump  
    [_ #f]))

;Generates paren-cheri-risc-v code in string if argument matches. Otherwise false.
;(generate-cheri-risc-v p) -> string?/boolean?
; p: any?
(define (generate-risc-v p)
    (match p
    [`(begin ,s ...) (foldl (lambda (s string) (format "~a~a" string (generate-sets s))) "" s)]
    [_ #f]))

(module+ test
  #|
;generate-binop
  ;succes
  (check-equal? (generate-binop '(set! a0 (+ a0 50))) "    addi a0, a0, 50\n" "generate-binop: succes-01: addition met 12bit int")
  (check-equal? (generate-binop '(set! a0 (+ a0 a0))) "    add a0, a0, a0\n"  "generate-binop: succes-02: addition met register")
  (check-equal? (generate-binop '(set! a0 (* a0 a0))) "    mul a0, a0, a0\n"  "generate-binop: succes-03: multiplier met register")
  (check-equal? (generate-binop '(set! a0 (- a0 a0))) "    sub a0, a0, a0\n"  "generate-binop: succes-04: substraction met register")

  (check-equal? (generate-binop '(set! ca0 (+ ca0 50))) "    CIncOffsetImm ca0, ca0, 50\n" "generate-binop: succes-05: add cap int12")
  (check-equal? (generate-binop '(set! ca0 (+ ca0 a0))) "    CIncOffset ca0, ca0, a0\n"    "generate-binop: succes-06: add cap cap")

  ;failure
  (check-equal? (generate-binop '(set! a0 (* a0 50)))    "    mul a0, a0, 50\n"    "generate-binop: failure-1: multiplier met integer - checker faalde")
  (check-equal? (generate-binop '(set! a0 (* 50 a0)))    "    mul a0, 50, a0\n"    "generate-binop: failure-4: multiplier met verkeerde triv - checker faalde")
  (check-equal? (generate-binop '(set! 50 (* a0 a0)))    "    mul 50, a0, a0\n"    "generate-binop: failure-7: multiplier met verkeerde triv - checker faalde")
  (check-equal? (generate-binop '(set! a0 (+ a0 50000))) "    add a0, a0, 50000\n" "generate-binop: failure-4: addition met 32bit int")

;generate-set
  ;succes
  (check-equal? (generate-set '(set! a0 50))        "    addi a0, zero, 50\n" "generate-set: succes-01: voeg integer toe")
  (check-equal? (generate-set '(set! a0 t0))        "    addi a0, t0, 0\n"    "generate-set: succes-02: copy register")
  (check-equal? (generate-set '(set! a0 (+ t0 50))) "    addi a0, t0, 50\n"   "generate-set: succes-03:  addition met integer")
  (check-equal? (generate-set '(set! a0 (+ t0 t1))) "    add a0, t0, t1\n"    "generate-set: succes-04: addition met register")
  (check-equal? (generate-set '(set! a0 (* t0 t1))) "    mul a0, t0, t1\n"    "generate-set: succes-05: multiplier met register")
  (check-equal? (generate-set '(set! a0 50000))     "    li a0, 50000\n"      "generate-set: succes-06: voeg 32bit int toe")
  (check-equal? (generate-set '(set! a0 (cfp - 0))) "    CIncOffset cs10, cfp, 0\n    lw.cap a0, 0(cs10)\n" "generate-set: succes-07: load from memory")
  (check-equal? (generate-set '(set! (cfp - 0) a0)) "    CIncOffset cs10, cfp, 0\n    sw.cap a0, 0(cs10)\n" "generate-set: succes-08: store in memory")

  (check-equal? (generate-set '(set! ca0 50))         "    CIncOffsetImm ca0, cnull, 50\n" "generate-set: succes-09: add int")
  (check-equal? (generate-set '(set! ca0 ct0))        "    cmove ca0, ct0\n"               "generate-set: succes-10: copy cap")
  (check-equal? (generate-set '(set! ca0 (+ ct0 50))) "    CIncOffsetImm ca0, ct0, 50\n"   "generate-set: succes-11: add cap int12")
  (check-equal? (generate-set '(set! ca0 (+ ct0 a0))) "    CIncOffset ca0, ct0, a0\n"      "generate-set: succes-12: add cap cap")
  (check-equal? (generate-set '(set! a0 L.foo.1))     "    cllc a0, L.foo.1\n"             "generate-set: succes-13: label")
  ;failure
;generate-jump
  ;succes
  (check-equal? (generate-jump '(jump ca0))       "    cjr ca0\n"                          "generate-jump: succes-01: jump cap")
  (check-equal? (generate-jump '(jump (cfp - 8))) "    CIncOffsetImm cs10, cfp, -8\n    lwu.cap s9, 0(cs10)\n    csetoffset cs11, cs11, s9\n    cjr cs11\n"
                                                                                           "generate-jump: succes-02: jump addr")
  (check-equal? (generate-jump '(jump L.foo.1))   "    cllc cs11, L.foo.1\n    cjr cs11\n" "generate-jump: succes-03: jump label")
  ;failure
  

;generate-relop
  ;succes
  (check-equal? (generate-relop '(compare a0 (= t0 t1)))  "    sub a0, t0, t1\n    sltiu a0, a0, 1\n" "generate-relop: succes-1: =")
  (check-equal? (generate-relop '(compare a0 (!= t0 t1))) "    sub a0, t0, t1\n    sltu a0, x0, a0\n" "generate-relop: succes-2: !=")
  (check-equal? (generate-relop '(compare a0 (< t0 t1)))  "    slt a0, t0, t1\n" "generate-relop: succes-3: <")
  (check-equal? (generate-relop '(compare a0 (> t0 t1)))  "    slt a0, t1, t0\n" "generate-relop: succes-4: >")
  (check-equal? (generate-relop '(compare a0 (<= t0 t1))) "    sub a0, t0, t1\n    slti a0, a0, 1\n" "generate-relop: succes-5: <=")
  (check-equal? (generate-relop '(compare a0 (>= t0 t1))) "    sub a0, t1, t0\n    slti a0, a0, 1\n" "generate-relop: succes-6: >=")
;generate-jump-if
  ;succes
  (check-equal? (generate-jump-if '(jump-if foo (= t0 t1)))  "    beq t0, t1, foo\n" "generate-jump-if: succes-1: =")
  (check-equal? (generate-jump-if '(jump-if foo (!= t0 t1))) "    bne t0, t1, foo\n" "generate-jump-if: succes-2: !=")
  (check-equal? (generate-jump-if '(jump-if foo (< t0 t1)))  "    blt t0, t1, foo\n" "generate-jump-if: succes-3: <")
  (check-equal? (generate-jump-if '(jump-if foo (> t0 t1)))  "    bgt t0, t1, foo\n" "generate-jump-if: succes-4: >")
  (check-equal? (generate-jump-if '(jump-if foo (<= t0 t1))) "    ble t0, t1, foo\n" "generate-jump-if: succes-5: <=")
  (check-equal? (generate-jump-if '(jump-if foo (>= t0 t1))) "    bge t0, t1, foo\n" "generate-jump-if: succes-6: >=")
;generate-sets
  ;succes
  (check-equal? (generate-sets '(with-label foo (set! a0 (+ t0 50)))) "foo:\n    addi a0, t0, 50\n"               "generate-sets: succes-01: set label")
  (check-equal? (generate-sets '(jump L.foo.5))                       "    cllc cs11, L.foo.5\n    cjr cs11\n"    "generate-sets: succes-02: jump")
  (check-equal? (generate-sets '(compare a0 (= t0 t1)))               "    sub a0, t0, t1\n    sltiu a0, a0, 1\n" "generate-sets: succes-03: compare")
  (check-equal? (generate-sets '(jump-if foo (= t0 t1)))              "    beq t0, t1, foo\n"                     "generate-sets: succes-04: jump-if")
  
  ;failure
  (check-equal? (generate-sets '(set! 50 50)) #f  "generate-sets: failure-1: copy met verkeerde triv - checker faalde")
  (check-equal? (generate-sets '(set! 50 a0)) #f  "generate-sets: failure-2: copy met verkeerde triv - checker faalde")
  (check-equal? (generate-sets '(st! a0 t0)) #f   "generate-sets: failure-3: verkeerd symbool")
  (check-equal? (generate-sets '(st! a0)) #f      "generate-sets: failure-4: te weinig argumenten")
  (check-equal? (generate-sets 'a) #f             "generate-sets: failure-5: verkeerd argument")
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
;|#
  ;|#
  )
