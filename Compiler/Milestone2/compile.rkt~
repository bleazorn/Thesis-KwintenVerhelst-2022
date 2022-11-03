#lang racket

(require "uniquify.rkt"
         "sequentialize-let.rkt"
         "normalize-bind.rkt"
         "select-instructions.rkt"
         "replace-locations.rkt"
         "assign-fvars.rkt"
         "uncover-locals.rkt"
         "flatten-begins.rkt"
         "patch-instructions.rkt"
         "implement-fvars.rkt"
         "generate-cheri-risc-v.rkt"
         "wrapRunTime.rkt"
         "wrapBoilerplate.rkt")

(provide compile-file
         compile-program)

(module+ test
  (require rackunit))

(define steps
  (list wrap-cheri-risc-v-boilerplate
        wrap-cheri-risc-v-run-time
        generate-cheri-risc-v
        implement-fvars
        patch-instructions
        flatten-begins
        replace-locations
        assign-fvars
        uncover-locals
        select-instructions
        normalize-bind
        sequentialize-let
        uniquify))

 (define (createList i j)
   (if (>= i j)
       `(,j)
       (cons i (createList (add1 i) j))))
  

(define (compileStepsDis start end program)
  (pretty-display 
   (for/fold ([p program])
             ([i (reverse (createList start end))])
     (values (let* ([fun (list-ref steps i)]
                    [res (fun p)])
               (display (format "~a:  ~a\n" fun res))
               res)))))

(define (compileSteps start end program)
   (for/fold ([p program])
             ([i (reverse (createList start end))])
     (values (let* ([fun (list-ref steps i)]
                    [res (fun p)])
               res))))

(define (compile program)
  (println "Compiling Program")
  (println program)
  (compileSteps 0 12 program))

(define (test program)
  (compileStepsDis 2 12 program))

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
  (write-string-to-file file (compile p)))

;compile a given program and write to the file "test.S"
;(write-program p) -> void
;p:
(define (write-program p)
  (write-program-to-file "test.S" p))


(define (read-program-from-file file)
  (if (file-exists? file)
      (file->value file)
      (println (format "File ~a does not exist" file))))

(define (compile-program p)
  (println p)
  (write-program p))

(define (compile-file file)
  (write-program (read-program-from-file file)))

;######################################################################

(define testProgram
  '(module (+ 50 0))
  )
(module+ test
  (check-equal? #t #t "test"))
;(test testProgram)
;(write-program testProgram)
;(compile-file "test.txt")
