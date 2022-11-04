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
         "wrapBoilerplate.rkt")
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

(define (createList i j)
  (if (>= i j)
      `(,j)
      (cons i (createList (add1 i) j))))
  

(define (compileStepsDis start end program)
  (pretty-display 
   (for/fold ([p program])
             ([i (reverse (createList start end))])
     
     (values (let ([fun (list-ref steps i)])
               (println (format "~a:" fun))
               (let ([res (fun p)])
                 (pretty-display res)
                 (display (format "\n\n"))
                 res))))))

(define (compileSteps start end program)
  (for/fold ([p program])
            ([i (reverse (createList start end))])
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
     (if (!= -129 329)
         (if (if (= -121 -258)
                 (not (not (if (true) (true) (true))))
                 (not
                  (if (not (false))
                      (false)
                      (if (<= -438 -341) (not (!= -40 -53)) (false)))))
             (if (false)
                 (let ((x1 (if (true) -253 (* -232 277)))
                       (x2 40)
                       (x3
                        (let ((x5
                               (if (if (not
                                        (let ((x8 (+ 266 290))
                                              (x9
                                               (let ((x10 217))
                                                 (let ((x11 x10) (x12 (+ x10 -271)))
                                                   x12))))
                                          (if (true)
                                              (if (if (false)
                                                      (let ((x13
                                                             (let ((x16 (+ x9 x8))
                                                                   (x17 (+ 175 x8))
                                                                   (x18 x8))
                                                               x17))
                                                            (x14 (* x9 250))
                                                            (x15 (* 463 x8)))
                                                        (= x13 -317))
                                                      (false))
                                                  (false)
                                                  (true))
                                              (true))))
                                       (false)
                                       (true))
                                   (+ -317 497)
                                   (+ -485 458)))
                              (x6 -166)
                              (x7 (+ 126 -121)))
                          (+ x7 x7)))
                       (x4 (+ 224 361)))
                   -308)
                 (* -396 496))
             -247)
         (* 59 239)))
  )
(module+ test
  (check-equal? #t #t "test"))


;(test testProgram)
;(write-program testProgram "ap.S")
;(compile-file "ap.txt")
;(test simpleProgram)
