#lang racket

(require "common.rkt")
(provide wrap-cheri-risc-v-run-time)

(module+ test
  (require rackunit))

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-cheri-risc-v-run-time p)
  (pretty-display p)
  (match p
    [s #:when (string? s) (string-append "main:" "\n"
                                         (indent-instr "cspecialr cfp, pcc")
                                         (indent-instr "auipcc cfp, %pcrel_hi(0x81f00000)")
                                         (indent-instr "CIncOffset cs10, cfp, 8")
                                         (indent-instr "sw.cap ra, 0(cs10)")
                                         (indent-instr "cllc cra, end")
                                         ;(indent-instr (format "CIncOffsetImm csp, cfp, -~a" info))
                                         ;(indent-instr (format "CSetBoundsImm cfp, csp, ~a" info))
                                         s
                                         "end:\n"
                                         (indent-instr "CIncOffset cs10, cfp, 8")
                                         (indent-instr "lwu.cap s9, 0(cs10)")
                                         (indent-instr "csetoffset cs11, cs11, s9")
                                         (indent-instr "cjr cs11")
                                         (indent-instr "cret"))]
    [_ "Run-time wrap failed"]))


(module+ test
  (check-equal? #t #t "first test"))
