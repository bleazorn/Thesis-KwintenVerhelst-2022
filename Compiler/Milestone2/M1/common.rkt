#lang racket

(provide indent
         indent-instr)

(define indent (make-string 4 #\ ))

(define (indent-instr instr)
  (format "~a~a\n" indent instr))


