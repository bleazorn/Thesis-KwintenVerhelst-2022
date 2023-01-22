#lang racket

(require "common/assembly.rkt")
(provide wrap-risc-v-boilerplate)

(module+ test
  (require rackunit))

;wraps the boilerplate for a cheri-risc-v around given code
;(wrap-cheri-risc-v-boilerplate p) -> string?
;p: string?
(define (wrap-risc-v-boilerplate p)
  (string-append (indent-instr ".p2align 6")        
                 (indent-instr ".global main")
                 (indent-instr ".option nocapmode")
                 p))

(module+ test
  (check-equal? #t #t "first test"))
