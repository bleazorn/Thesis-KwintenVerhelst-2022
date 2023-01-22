#lang racket

(require "common/assembly.rkt"
         "setup.rkt"
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
    [s #:when (string? s) (string-append "main:" "\n"
                                         (indent-instr (format "auipcc cfp, %pcrel_hi(0x~a)" (top-stack)))
                                         (indent-instr (format "cmove cs11, cfp"))
                                         (createSeals)
                                         (generate-cheri-risc-v (setUpExprs))       
                                         s
                                         "error:\n"
                                         (indent-instr "addi a0, x0, 321")
                                         (indent-instr "cret"))]
    [_ "Run-time wrap failed"]))


(module+ test
  (check-equal? #t #t "first test"))
