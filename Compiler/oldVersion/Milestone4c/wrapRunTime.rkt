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
    [`(,info . ,s)  (string-append "main:" "\n"
                                 (indent-instr "cspecialr cfp, pcc")
                                 (indent-instr "auipcc ct0, %pcrel_hi(0x81f00000)")
                                 (indent-instr "CSetAddr cfp, cfp, t0")
                                 (indent-instr (format "CSetBoundsImm cfp, cfp, ~a" info))
                                 (indent-instr "CIncOffsetImm csp, cfp, 0")
                                 s
                                 "end:\n"
                                 (indent-instr "cret"))]
    [_ "Run-time wrap failed"]))


(module+ test
  (check-equal? #t #t "first test"))
