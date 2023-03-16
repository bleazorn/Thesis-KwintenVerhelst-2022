#lang racket

(require "uniquify.rkt"
         "sequentialize-let.rkt"
         "normalize-bind.rkt"
         "select-instructions.rkt"
         "replace-locations.rkt"
         "assign-registers.rkt"
         "conflict-analysis.rkt"
         "undead-analysis.rkt"
         "uncover-locals.rkt"
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
        implement-fvars
        patch-instructions
        flatten-program  
        resolve-predicates
        expose-basic-blocks
        replace-locations
        assign-registers
        conflict-analysis
        undead-analysis
        uncover-locals
        select-instructions
        normalize-bind
        sequentialize-let
        uniquify))

;; SAND: createList already exists, it's called inclusive-range

(define (compileStepsDis start end program)
  (pretty-display 
   (for/fold ([p program])
             ([i (reverse (inclusive-range start end))])
     
     (values (let ([fun (list-ref steps i)])
               (println (format "~a:" fun))
               (let ([res (fun p)])
                 (pretty-display res)
                 (display (format "\n\n"))
                 res))))))

(define (compileSteps start end program)
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

(define simpleProgram '(module 50))
(define testProgram
  '(module
       (if (true)
           (if (true) 135 (* 265 -504))
           (if (not (false))
               -103
               (if (true)
                   (let ((x1
                          (if (false)
                              (let ((x3
                                     (let ((x5 -360) (x6 -206) (x7 162))
                                       (if (if (if (let ((x8
                                                          (let ((x12 x6)
                                                                (x13 (* x6 x7))
                                                                (x14 (+ 43 453))
                                                                (x15 (* x5 x7)))
                                                            (if (false)
                                                                (* -247 -89)
                                                                (let ((x16
                                                                       (if (if (if (true)
                                                                                   (let ((x19
                                                                                          (if (true)
                                                                                              (+
                                                                                               x14
                                                                                               x12)
                                                                                              (let ((x23
                                                                                                     -416)
                                                                                                    (x24
                                                                                                     (+
                                                                                                      -69
                                                                                                      x13))
                                                                                                    (x25
                                                                                                     (*
                                                                                                      -476
                                                                                                      x13))
                                                                                                    (x26
                                                                                                     (*
                                                                                                      x12
                                                                                                      x15)))
                                                                                                (*
                                                                                                 x23
                                                                                                 -420))))
                                                                                         (x20
                                                                                          357)
                                                                                         (x21
                                                                                          (+
                                                                                           x14
                                                                                           -89))
                                                                                         (x22
                                                                                          x12))
                                                                                     (>=
                                                                                      -63
                                                                                      x21))
                                                                                   (false))
                                                                               (false)
                                                                               (true))
                                                                           (+ 143 x15)
                                                                           150))
                                                                      (x17 (+ x15 297))
                                                                      (x18 (+ 362 x15)))
                                                                  (* x16 25)))))
                                                         (x9 x6)
                                                         (x10 -40)
                                                         (x11 (+ x5 x7)))
                                                     (true))
                                                   (= x5 394)
                                                   (true))
                                               (true)
                                               (<= x5 -129))
                                           x7
                                           (+ 89 x6))))
                                    (x4 22))
                                x3)
                              (+ -169 11)))
                         (x2 (* 246 -155)))
                     (+ -331 -380))
                   -427))))
  )
(module+ test
  (check-equal? #t #t "test"))

;(interp-values-lang testProgram)
;(test testProgram)
;(write-program testProgram "ap.S")
;(compile-file "ap.txt")
;(test simpleProgram)
