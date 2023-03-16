#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
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
         "implement-fvars.rkt"     
         "expose-basic-blocks.rkt"
         "resolve-predicates.rkt"
         "flatten-program.rkt"
         "patch-instructions.rkt"
         "cap-mode-on.rkt"
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
        cap-mode-on
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

(define simpleProgram '(module
                      (define odd?
                        (lambda (x)
                          (if (= x 0)
                              0
                              (let ([y (+ x -1)])
                                (call even? y)))))
                    (define even?
                      (lambda (x)
                        (if (= x 0)
                            1
                            (let ([y (+ x -1)])
                              (call odd? y)))))
                    (call even? 5)))

(define infProgram '(module (define fun1
                              (lambda (x1 x2)
                                (let ([x3 x2]
                                      [x4 (call fun2 x1 x1)])
                                  (call fun2 x3 x2))))
                      (define fun2
                        (lambda (x1 x2)
                          (call fun3 x2)))
                      (define fun3
                        (lambda (x1)
                          (+ x1 15)))
                      (call fun1 -50 200)))
(define testProgram
  '(module (define fun2 (lambda (x1 x2) (let ((x3 (if (if (not (let ((x4 (if (if (false) (not (true)) (let ((x7 (call fun3 -38 x1 x1)) (x8 x1) (x9 (+ x2 x1)) (x10 (* -259 -103))) (false))) x1 287)) (x5 454) (x6 (- x1 x2))) (false))) (= x2 272) (true)) -65 x1))) (- x3 -224)))) (define fun3 (lambda (x1 x2 x3) (if (false) (if (false) (if (true) (let ((x4 (let ((x9 (call fun2 x3 x3)) (x10 (let ((x14 (if (let ((x15 (let ((x18 (let ((x21 (* x3 -217)) (x22 (call fun2 90 x1)) (x23 (+ x2 x1))) -116)) (x19 (if (if (true) (<= x1 x3) (>= x1 x1)) (let ((x24 (let ((x27 (if (let ((x30 (let ((x35 (call fun1 x1 x3))) (if (not (let ((x5 (- x35 x35)) (x6 x35) (x7 (let ((x8 (+ x35 x35)) (x9 (let ((x12 (+ x35 x35)) (x13 (call fun4 x35 x35)) (x14 (- 360 x35)) (x15 x35)) (let ((x16 (* x13 -68))) (call fun1 x16 x16)))) (x10 (- x35 x35)) (x11 (+ x35 -308))) (call fun2 x9 x8)))) (if (not (>= x6 -37)) (false) (false)))) 244 (call fun1 x35 x35)))) (x31 (let ((x17 (call fun3 x3 x2 x2)) (x18 (call fun4 x1 x1))) (if (let ((x19 (+ x18 x17)) (x20 (if (true) (call fun4 x17 x17) (call fun4 -342 x17)))) (if (let ((x21 (call fun3 x19 x20 x20)) (x22 (* x20 208))) (false)) (true) (< x20 -189))) (call fun5 x17 -337 x17 -380) x18))) (x32 (call fun3 131 x1 x1)) (x33 (call fun3 x2 x3 -152)) (x34 (let ((x23 x2)) -426))) (if (true) (false) (false))) (let ((x24 (+ x3 415)) (x25 x1) (x26 x3) (x27 x3) (x28 (+ x2 -235))) (+ 365 -65)) x1)) (x28 (+ 8 387)) (x29 (- x3 x3))) (* 125 x28))) (x25 x2) (x26 (- x2 x3))) (* -302 x26)) 28)) (x20 (* 448 -493))) (* -107 509))) (x16 x2) (x17 (* -118 -413))) (true)) -356 169))) (+ 253 x14))) (x11 (- x2 185)) (x12 (* 196 x2)) (x13 x1)) (- -406 x10))) (x5 478) (x6 (- 411 373)) (x7 (+ x1 x3)) (x8 324)) (* x8 x5)) (* x2 x3)) (+ x1 x1)) -167))) (define fun1 (lambda (x1 x2) (if (let ((x3 (if (if (let ((x4 (call fun4 x2 x1)) (x5 (let ((x3 x1) (x4 (call fun1 x1 x2)) (x5 (call fun4 x2 x2)) (x6 (if (false) (if (not (< -208 x2)) (let ((x7 (call fun3 x2 x2 291))) (call fun1 x7 x7)) (* x1 -194)) (if (true) (+ x1 x1) (call fun1 289 x2))))) (call fun1 x4 x6)))) (let ((x8 x5) (x9 (let ((x11 (call fun3 x5 x4 x5)) (x12 (call fun1 x5 x4)) (x13 (+ x5 x4)) (x14 (call fun3 x5 x5 -130))) (- x11 x11))) (x10 (call fun5 x5 x4 x4 x4))) (true))) (> x1 x2) (true)) (call fun2 x2 x2) (call fun5 x1 x1 x1 x2)))) (< x3 x3)) (call fun3 x1 x1 x2) (call fun5 x2 x2 x2 x1)))) (define fun5 (lambda (x1 x2 x3 x4) (call fun3 -143 106 x3))) (define fun4 (lambda (x1 x2) (call fun3 x2 319 x2))) (call fun2 -471 403))
  )

(module+ test
  (check-equal? #t #t "test"))

#;(parameterize ()
                 ;[current-parameter-registers '()]
                 ;[current-assignable-registers '()]) 
  (append (current-parameter-registers) (current-assignable-registers))
  (test testProgram))
(compileStepsDis 2 (sub1 (length steps)) simpleProgram)
(interp-values-lang simpleProgram)
;(write-program testProgram "ap.S")
;(compile-file "ap.txt")
;(test simpleProgram)
