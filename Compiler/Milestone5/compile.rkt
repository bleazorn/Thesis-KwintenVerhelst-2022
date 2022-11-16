#lang racket

(require "check-values-lang.rkt"
         "uniquify.rkt"
         "sequentialize-let.rkt"
         "normalize-bind.rkt"
         "impose-calling-conventions.rkt"
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
        impose-calling-conventions
        normalize-bind
        sequentialize-let
        uniquify
        check-values-lang))

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
  '(module (define "fun4" (lambda ("x1" "x2")
                            (if (not (if (> "x2" -96)
                                         (false)
                                         (let (("x2" (if (if (= "x2" "x1")
                                                             (not (if (false)
                                                                      (let (("x4" (let (("x8" (+ "x1" 462))
                                                                                        ("x9" (if (true)
                                                                                                  (let (("x10" "x1"))
                                                                                                    (let (("x11" "x10")
                                                                                                          ("x12" (if (not (false)) (let (("x13" (* "x10" "x10")) ("x14" (let (("x17" "x10") ("x18" (if (let (("x21" "x10") ("x22" (let (("x24" -503)) 320)) ("x23" (+ "x10" "x10"))) (false)) (+ 297 "x10") (* "x10" "x10"))) ("x19" (if (not (<= "x10" -148)) (let (("x25" (* "x10" "x10"))) "x25") (* 33 "x10"))) ("x20" (if (< "x10" "x10") (+ "x10" "x10") (let (("x26" (if (if (not (false)) (if (let (("x30" 286) ("x31" (+ "x10" "x10")) ("x32" -244) ("x33" (if (true) (* "x10" 57) (+ "x10" 338))) ("x34" (* -428 "x10"))) (let (("x35" (if (let (("x38" (if (false) "x31" (let (("x41" "x33") ("x42" "x30")) (if (false) (let (("x43" (* -353 392)) ("x44" (* "x41" "x42")) ("x45" (* 416 "x41")) ("x46" (+ 311 "x41"))) "x46") 148)))) ("x39" (* -399 -252)) ("x40" (+ "x31" "x31"))) (true)) "x34" 459)) ("x36" 316) ("x37" 12)) (<= "x35" 316))) (< -118 "x10") (true)) (false)) (+ -29 "x10") (* 17 "x10"))) ("x27" (* "x10" -410)) ("x28" (* "x10" 311)) ("x29" "x10")) (* "x29" -107))))) (* "x19" "x20"))) ("x15" (* -497 "x10")) ("x16" -11)) (* 101 "x16")) 372))) (+ 163 423))) "x2"))) 481)) ("x5" (* "x2" -161)) ("x6" (+ "x1" "x2")) ("x7" (+ "x1" "x2"))) (>= 462 65)) (false))) (false)) (* "x2" -179) (* "x1" -339))) ("x3" (+ "x1" "x2"))) (>= "x3" "x2")))) (let (("x3" (+ "x1" 179))) (call "fun4" -8 "x3")) (call "fun3" "x2"))))
     (define "fun3" (lambda ("x1") (call "fun4" "x1" "x1")))
     (if (false) (call "fun4" -334 -298) 510))
  )
(module+ test
  (check-equal? #t #t "test"))

;(interp-values-lang testProgram)
(test testProgram)
;(write-program testProgram "ap.S")
;(compile-file "ap.txt")
;(test simpleProgram)
