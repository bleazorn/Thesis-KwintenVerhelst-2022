#lang racket

(require "common.rkt"
         "check-values-lang.rkt"
         "uniquify.rkt"
         "sequentialize-let.rkt"
         "normalize-bind.rkt"
         "impose-calling-conventions.rkt"
         "select-instructions.rkt"
         "uncover-locals.rkt"
         "undead-analysis.rkt"
         "conflict-analysis.rkt"
         "assign-call-undead-variables.rkt"
         "allocate-frames.rkt"
         "assign-registers.rkt"
         "assign-frame-variables.rkt"
         "replace-locations.rkt"       
         "expose-basic-blocks.rkt"
         "resolve-predicates.rkt"
         "flatten-program.rkt"
         "patch-instructions.rkt"
         "implement-fvars.rkt"
         "generate-cheri-risc-v.rkt"
         "wrapRunTime.rkt"
         "wrapBoilerplate.rkt"
         "interp-values-lang.rkt")
;(require racket/system)

(provide compile-file
         compile-program)

(module+ test
  (require rackunit))

(define steps
  (list wrap-cheri-risc-v-boilerplate
        wrap-cheri-risc-v-run-time
        generate-cheri-risc-v
        patch-instructions
        flatten-program  
        resolve-predicates
        expose-basic-blocks
        implement-fvars
        replace-locations
        assign-frame-variables
        assign-registers
        allocate-frames
        assign-call-undead-variables
        conflict-analysis
        undead-analysis
        uncover-locals
        select-instructions
        impose-calling-conventions
        normalize-bind
        sequentialize-let
        uniquify
        check-values-lang))

;; SAND: createList already exists, it's called inclusive-range

(define (compileStepsDis start end program)
  (resetfvar)
  (for/fold ([p program])
             ([i (reverse (inclusive-range start end))])
     
     (values (let ([fun (list-ref steps i)])
               (println (format "~a:" fun))
               (let ([res (fun p)])
                 (pretty-display (cond [(list? res) (let-values ([(tak dro) (split-at res 1)])
                                                      (append tak '(()) dro))]
                                       [else res]))
                 (display (format "\n\n"))
                 res)))))

(define (compileSteps start end program)
  (resetfvar)
  (for/fold ([p program])
            ([i (reverse (inclusive-range start end))])
    (values (let* ([fun (list-ref steps i)]
                   [res (fun p)])
              res))))

(define (compile program)
  (println "Compiling Program")
  (println program)
  (compileSteps 0 (sub1 (length steps)) program))

(define (test program)
  (compileStepsDis 2 (sub1 (length steps)) program))

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
(define (write-program p file)
  (write-program-to-file file p))


(define (read-program-from-file file)
  (if (file-exists? file)
      (file->value file)
      (println (format "File ~a does not exist" file))))

(define (compile-program p)
  (write-program p "Test.S"))

(define (compile-file file)
  (write-program (read-program-from-file file) (format "~a.S" (car (string-split file ".")))))

;######################################################################

(define simpleProgram '(module (define fun2 (lambda (x1 x2 x1 x1 x1) (call fun5 x2))) 
                         (define fun5 (lambda (x1) (call fun1 x1))) 
                         (define fun1 (lambda (x1) (call fun3 x1 175 x1 x1))) 
                         (define fun3 (lambda (x1 x2 x3 x4) x4)) (call fun2 445 500 100 200 300)))
(define testProgram
  '(module
      (define swap
        (lambda (x y)
          (if (< y x)
              x
              (let ([z (call swap y x)])
                (+ x y)))))
      (call swap 50 200))
  )

(module+ test
  (check-equal? #t #t "test"))

;(test simpleProgram)
;(compileStepsDis 2 (sub1 (length steps)) testProgram)
;(interp-values-lang testProgram)
;(write-program testProgram "ap.S")
;(compile-file "ap.txt")
;(test simpleProgram)
