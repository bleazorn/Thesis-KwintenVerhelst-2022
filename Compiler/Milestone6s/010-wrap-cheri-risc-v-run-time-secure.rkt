#lang racket

(require "common/assembly.rkt"
         "setup.rkt"
         "020-generate-cheri-risc-v.rkt")

(provide wrap-cheri-risc-v-run-time-secure)

(module+ test
  (require rackunit))

(define (createSeals)
  "")

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-cheri-risc-v-run-time-secure p)
  (pretty-display p)
  (match p
    [s #:when (string? s) (string-append "main:\n"
                                         (indent-instr (format "auipcc csp, %pcrel_hi(0x~a)" 81000000))
                                         (indent-instr (format "cmove cs2, csp"))
                                         "//csp init\n"
                                         (indent-instr (format "li t5, 381"))
                                         (indent-instr (format "CAndPerm csp, csp, t5"))
                                         (indent-instr (format "li t0, 15859712"))
                                         (indent-instr (format "CSetBounds csp, csp, t0"))
                                         (indent-instr (format "CMakelinear csp, csp"))
                                         "//PCC init\n"
                                         (indent-instr (format "cllc ct0, L.pcJump"))
                                         (indent-instr (format "li t5, 259"))
                                         (indent-instr (format "CAndPerm ct0, ct0, t5"))
                                         (indent-instr (format " cjalr ct0, ct0"))
                                         "L.pcJump:\n"
                                         "//Call Setup\n"
                                         (indent-instr (format "li t0, 16384"))
                                         (indent-instr (format "CSplitCap cfp, csp, t0"))
                                         "//Seal Setup\n"
                                         (indent-instr (format "li t5, 641"))
                                         (indent-instr (format "CAndPerm cs2, cs2, t5"))
                                         (indent-instr (format "li t5, 132056"))
                                         (indent-instr (format "csetoffset cs2, cs2, t5"))
                                         (indent-instr (format "CSeal cra, cra, cs2"))
                                         (indent-instr (format "CSeal cfp, cfp, cs2"))
                                         s)]
    [_ "Run-time wrap failed"]))

(define test "L.tmp.0:
    CIncOffset cs10, cfp, -16
    sc.cap cra, 0(cs10)
    CIncOffsetImm cfp, cfp, -16
    addi a0, zero, 5
    cllc cra, L.rpLabel.12
    cllc cs11, L.even?.2
    cjr cs11
L.rpLabel.12:
    CIncOffsetImm cfp, cfp, 16
    CIncOffsetImm cs10, cfp, -16
    lwu.cap s9, 0(cs10)
    csetoffset cs11, cs11, s9
    cjr cs11
L.even?.2:
    CIncOffset cs10, cfp, -16
    sc.cap cra, 0(cs10)
    addi t1, a0, 0
    addi t0, zero, 0
    beq t1, t0, L.tmp.17
    cllc cs11, L.tmp.18
    cjr cs11
L.tmp.17:
    addi a0, zero, 200
    CIncOffsetImm cs10, cfp, -16
    lwu.cap s9, 0(cs10)
    csetoffset cs11, cs11, s9
    cjr cs11
L.tmp.18:
    addi t0, t1, -1
    CIncOffsetImm cfp, cfp, -16
    addi a0, t0, 0
    cllc cra, L.rpLabel.10
    cllc cs11, L.odd?.1
    cjr cs11
L.rpLabel.10:
    CIncOffsetImm cfp, cfp, 16
    CIncOffsetImm cs10, cfp, -16
    lwu.cap s9, 0(cs10)
    csetoffset cs11, cs11, s9
    cjr cs11
L.odd?.1:
    CIncOffset cs10, cfp, -16
    sc.cap cra, 0(cs10)
    addi t1, a0, 0
    addi t0, zero, 0
    beq t1, t0, L.tmp.15
    cllc cs11, L.tmp.16
    cjr cs11
L.tmp.15:
    addi a0, zero, 150
    CIncOffsetImm cs10, cfp, -16
    lwu.cap s9, 0(cs10)
    csetoffset cs11, cs11, s9
    cjr cs11
L.tmp.16:
    addi t0, t1, -1
    CIncOffsetImm cfp, cfp, -16
    addi a0, t0, 0
    cllc cra, L.rpLabel.8
    cllc cs11, L.even?.2
    cjr cs11
L.rpLabel.8:
    CIncOffsetImm cfp, cfp, 16
    CIncOffsetImm cs10, cfp, -16
    lwu.cap s9, 0(cs10)
    csetoffset cs11, cs11, s9
    cjr cs11")

;(wrap-cheri-risc-v-run-time-secure test)




(module+ test
  (check-equal? #t #t "first test"))
