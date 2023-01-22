#lang racket

(require "common/assembly.rkt")
(provide wrap-cheri-risc-v-boilerplate)

(module+ test
  (require rackunit))

;wraps the boilerplate for a cheri-risc-v around given code
;(wrap-cheri-risc-v-boilerplate p) -> string?
;p: string?
(define (wrap-cheri-risc-v-boilerplate p)
  (string-append "#include \"encoding.h\"\n\n"
                 (indent-instr ".p2align 6")        
                 (indent-instr ".global main")
                 (indent-instr ".option capmode")
                 p))

(module+ test
  (check-equal? #t #t "first test"))
