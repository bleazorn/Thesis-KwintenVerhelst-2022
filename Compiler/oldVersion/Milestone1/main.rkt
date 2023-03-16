#lang racket

(require "checker.rkt"
         "compiler.rkt"
         "wrapRunTime.rkt"
         "wrapBoilerplate.rkt")

;appends two programs
(define (p-append p1 p2)
  (match `(,p1 ,p2)
    [`((begin ,s1 ...) (begin ,s2 ...)) `(begin ,@s1 ,@s2)]))

;creates a string from given code
;(paren-cheri-risc-v-compiler p) -> string?
(define (paren-cheri-risc-v-compiler p)
  (wrap-cheri-risc-v-boilerplate
   (wrap-cheri-risc-v-run-time
    (generate-cheri-risc-v
     (check-paren-cheri-risc-v p)))))

;write a given string to a given file
;(write-string-to-file file string) -> any
;file: path-string?
;string: string?
(define (write-string-to-file file string)
  (cond [(file-exists? file) (delete-file file)])
  (with-output-to-file file
    (lambda () (printf string))))

;compile a given program and write to given file
;(write-program-to-file file p) -> any
;file: path-string?
;p:
(define (write-program-to-file file p)
  (write-string-to-file file (paren-cheri-risc-v-compiler p)))

;compile a given program and write to the file "test.S"
;(write-program p) -> any
;p:
(define (write-program p)
  (write-program-to-file "test.S" p))

(define p1 (build-path (current-directory) "src" "racket"))

(define test-program
  '(begin
     (set! t0 250)
     (set! a0 t0)
     (set! a0 (+ a0 50))
     (set! a0 (+ a0 -300))
     ))

(write-program test-program)
  
