#lang racket

(require "common.rkt")
(provide wrap-cheri-risc-v-boilerplate)

(module+ test
  (require rackunit))

;wraps the boilerplate for a cheri-risc-v around given code
;(wrap-cheri-risc-v-boilerplate p) -> string?
;p: string?
(define (wrap-cheri-risc-v-boilerplate p)
  (string-append (indent-instr ".p2align 6")        
                 (indent-instr ".global main")
                 (indent-instr ".option nocapmode")
                 "main:" "\n"
                 p))

(module+ test
  (check-equal? #t #t "first test"))
