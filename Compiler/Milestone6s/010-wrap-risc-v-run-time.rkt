#lang racket

(require "common/assembly.rkt")
(provide wrap-risc-v-run-time)

(module+ test
  (require rackunit))

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-risc-v-run-time p)
  (pretty-display p)
  (match p
    [s #:when (string? s) (string-append "main:" "\n"
                                         (indent-instr "auipc fp, %pcrel_hi(0x81f00000)")
                                         s
                                         "end:\n"
                                         (indent-instr "ret"))]
    [_ "Run-time wrap failed"]))


(module+ test
  (check-equal? #t #t "first test"))
