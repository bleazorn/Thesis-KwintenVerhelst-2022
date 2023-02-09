#lang racket

(require "common/assembly.rkt"
         "common/fvar.rkt"
         "020-generate-cheri-risc-v.rkt")
(provide wrap-cheri-risc-v-run-time)

(module+ test
  (require rackunit))

(define (createSeals)
  "")

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-cheri-risc-v-run-time p)
  (pretty-display p)
  (match p
    [`(begin ,i ,s) #:when (string? s) (string-append "main:" "\n"
                                                      (indent-instr (format "auipcc cfp, %pcrel_hi(0x~a)" 81000000))
                                                      (indent-instr (format "cmove cs11, cfp"))
                                                      "//csp init\n"
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "CAndPerm cfp, cfp, t5"))
                                                      (indent-instr (format "li t0, 0xf00000"))
                                                      (indent-instr (format "CSetBounds cfp, cfp, t0"))
                                                      (cond [(equal? '- (stack-direction)) (indent-instr (format "addi t0, t0, -~a" (framesize)))
                                                                                           (indent-instr (format "CSetOffset cfp, cfp, t0"))]
                                                            [else ""])
                                                      "//PCC init\n"
                                                      (indent-instr (format "cllc ct0, L.pcJump"))
                                                      (indent-instr (format "li t5, 259"))
                                                      (indent-instr (format "CAndPerm ct0, ct0, t5"))
                                                      (indent-instr (format "cjalr ct0, ct0"))
                                                      "L.pcJump:\n"
                                                      s
                                                      "error:\n"
                                                      (indent-instr "addi a0, x0, 321")
                                                      (indent-instr "cret"))]
    [_ "Run-time wrap failed"]))


(module+ test
  (check-equal? #t #t "first test"))

#;(wrap-cheri-risc-v-run-time '(begin ()      ".global L.tmp.0
                                    L.tmp.0:
                                    addi s6, s6, 1
                                    CIncOffsetImm cs10, cfp, -16
                                    sc.cap cra, 0(cs10)
                                    CIncOffsetImm cfp, cfp, -16
                                    addi a0, zero, 5
                                    cllc cra, L.rpLabel.12
                                    cllc cs11, L.even?.2
                                    cjr cs11
                                    .global L.rpLabel.12
                                    L.rpLabel.12:
                                    addi s6, s6, 2
                                    CIncOffsetImm cfp, cfp, 16
                                    CIncOffsetImm cs10, cfp, -16
                                    lc.cap ct5, 0(cs10)
                                    cjr ct5
                                    .global L.even?.2
                                    L.even?.2:
                                    addi s6, s6, 3
                                    CIncOffsetImm cs10, cfp, -16
                                    sc.cap cra, 0(cs10)
                                    addi t1, a0, 0
                                    addi t0, zero, 0
                                    beq t1, t0, L.tmp.17
                                    cllc cs11, L.tmp.18
                                    cjr cs11
                                    .global L.tmp.17
                                    L.tmp.17:
                                    addi s6, s6, 4
                                    addi a0, zero, 200
                                    CIncOffsetImm cs10, cfp, -16
                                    lc.cap ct5, 0(cs10)
                                    cjr ct5
                                    .global L.tmp.18
                                    L.tmp.18:
                                    addi s6, s6, 5
                                    addi t0, t1, -1
                                    CIncOffsetImm cfp, cfp, -16
                                    addi a0, t0, 0
                                    cllc cra, L.rpLabel.10
                                    cllc cs11, L.odd?.1
                                    cjr cs11
                                    .global L.rpLabel.10
                                    L.rpLabel.10:
                                    addi s6, s6, 6
                                    CIncOffsetImm cfp, cfp, 16
                                    CIncOffsetImm cs10, cfp, -16
                                    lc.cap ct5, 0(cs10)
                                    cjr ct5
                                    .global L.odd?.1
                                    L.odd?.1:
                                    addi s6, s6, 7
                                    CIncOffsetImm cs10, cfp, -16
                                    sc.cap cra, 0(cs10)
                                    addi t1, a0, 0
                                    addi t0, zero, 0
                                    beq t1, t0, L.tmp.15
                                    cllc cs11, L.tmp.16
                                    cjr cs11
                                    .global L.tmp.15
                                    L.tmp.15:
                                    addi s6, s6, 8
                                    addi a0, zero, 150
                                    CIncOffsetImm cs10, cfp, -16
                                    lc.cap ct5, 0(cs10)
                                    cjr ct5
                                    .global L.tmp.16
                                    L.tmp.16:
                                    addi s6, s6, 9
                                    addi t0, t1, -1
                                    CIncOffsetImm cfp, cfp, -16
                                    addi a0, t0, 0
                                    cllc cra, L.rpLabel.8
                                    cllc cs11, L.even?.2
                                    cjr cs11
                                    .global L.rpLabel.8
                                    L.rpLabel.8:
                                    addi s6, s6, 10
                                    CIncOffsetImm cfp, cfp, 16
                                    CIncOffsetImm cs10, cfp, -16
                                    lc.cap ct5, 0(cs10)
                                    cjr ct5"
                                    ))
