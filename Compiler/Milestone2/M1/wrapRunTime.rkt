#lang racket

(require "common.rkt")
(provide wrap-cheri-risc-v-run-time)

(module+ test
  (require rackunit))

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-cheri-risc-v-run-time p)
  (string-append (indent-instr "auipc fp, %pcrel_hi(0x81f00000)")
                 (indent-instr "addi sp, fp, 0")
                 p
                 (indent-instr "ret")))


(module+ test
  (check-equal? #t #t "first test"))
