#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
         "config.rkt"
         "log.rkt"
         "steps.rkt"
         "interp-values-lang.rkt")
;(require racket/system)

(provide compile-file
         compile-program)

(module+ test
  (require rackunit))


(define (compileStepsDis start end program)
  (resetfvar)
  (logln (steps))
  (for/fold ([p program])
             ([i (reverse (inclusive-range start end))])
     (values (let ([fun (list-ref (steps) i)])
               (logln (format "~a:" fun))
               (pretty-display (format "~a:" fun))
               (let ([res (fun p)])
                 (cond [(list? res) (pretty-display (cons (car res) (cons '() (cdr res))))]
                       [else (display res)])
                 (display (format "\n\n"))
                 res)))))

(define (compileSteps start end program)
  (resetfvar)
  (for/fold ([p program])
            ([i (reverse (inclusive-range start end))])
    (values (let* ([fun (list-ref (steps) i)]
                   [res (fun p)])
              res))))

(define (compile program)
  (logln "Compiling Program")
  (logln program)
  (compileSteps 0 (sub1 (length (steps))) program))

(define (test program)
  (compileStepsDis 2 (sub1 (length (steps))) program))

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
(define (write-program-to-file p)
  (let ([transformer (if (pass)
                         (compose pretty-format compile)
                         compile)])
    (if (output-file)
        (write-string-to-file (output-file) (transformer p))
        (printf (transformer p)))))

;compile a given program and write to the file "test.S"
;(write-program p) -> void
;p:
(define (write-program p)
  (write-program-to-file p))

(define (read-program-from-file file)
  (if (file-exists? file)
      (file->value file)
      (println (format "File ~a does not exist" file))))

(define (compile-program p)
  (write-program p "Test.S"))

(define (setup-cc steps)
  (match (cc)
    ['vanilla-cheri-riscv steps]
    ['stktokens                    (stkTokens steps)]
    ['stkTokens-sentry             (stkTokens-sentry steps)]
    ['cheri-linkage-seal           (cheri-linkage-seal steps)]
    ['cheri-linkage-trampoline     (cheri-linkage-trampoline steps)]
    ['risc-v        (risc-v steps)]
    ['tail-calls    (halfStack steps)]
    [unknown        (error "unsupported calling convention: " unknown)]))

(define (setup-passes steps)
  (if (not (pass))
      steps
      (let ([s (memf (compose (curry equal? (pass)) object-name) steps)])
        (if s
            s
            (error "no such pass exists: " (pass))))))

(define setup-steps (compose setup-passes setup-cc))

(define (setup-sealed-register)
  (match (cc)
    ['cheri-linkage-seal       'csp]
    ['cheri-linkage-trampoline 'csp]
    [_                         'cfp]))

(define (setup-stack-register)
  (match (cc)
    ['stktokens        'csp]
    ['stkTokens-sentry 'csp]
    [_                 'cfp]))

(define (setup-stack-direction)
  (match (cc)
    ['stktokens        '+]
    ['stkTokens-sentry '+]
    [_                 '-]))

(define (setup-seal-call-register)
  (match (cc)
    ['stktokens        'cs1]
    ['stkTokens-sentry 'cs1]
    [_                 'cs1]))
    

(define (compile-file file)
  (println (format "test: ~a" (output-file)))
  (parameterize ([steps                               (setup-steps (steps))]
                 [current-stack-base-pointer-register (setup-stack-register)]
                 [current-return-sealed-register      (setup-sealed-register)]
                 [current-seal-got-call-register      (setup-seal-call-register)]
                 [stack-direction                     (setup-stack-direction)]
                 ;[current-parameter-registers '()]
                 ;[current-assignable-registers '()]
                 )
    ;(pretty-display (format "~a: ~a ~a ~a" (cc) (current-stack-base-pointer-register) (current-frame-base-pointer-register) (stack-direction)))
    (write-program (read-program-from-file file))))

;######################################################################


(define simpleProgram '(module
                           (define odd?
                             (lambda (x)
                               (if (= x 0)
                                   150
                                   (let ([y (+ x -1)])
                                     (call even? y)))))
                         (define even?
                           (lambda (x)
                             (if (= x 0)
                                 200
                                 (let ([y (+ x -1)])
                                   (call odd? y)))))
                         (call even? 5)))

(define testProgram '(module
                         (define fun2
                           (lambda (x1 x2 x3)
                             (call fun1 x3 -279 x1)))
                       (define fun1
                         (lambda (x1 x2 x3)
                           x2))
                       (let ((x1 (call fun2 -351 139 -148)))
                         488)))
(define swapProgram
  '(module
       (define swap
         (lambda (x y)
           (if (< y x)
               x
               (let ([z (call swap y x)])
                 z))))
     (call swap 1 2))
  )

(define bigProgram
  '(module (define fun4 (lambda (x1) (if (let ((x2 (call fun5 x1 x1 x1 x1))) (< x2 x2)) (call fun1 x1) (let ((x5 (if (= 231 x1) (call fun2 x1) (let ((x9 (let ((x10 (let ((x15 (call fun4 x1)) (x16 (if (if (true) (if (false) (!= x1 x1) (not (if (= x1 x1) (let ((x18 213)) (if (let ((x19 (call fun5 x18 295 x18 x18)) (x20 (* x18 x18)) (x21 (call fun1 x18)) (x22 (call fun3 x18)) (x23 (call fun5 x18 x18 6 x18))) (let ((x2 x19) (x3 (call fun1 x20)) (x4 (* 295 x20)) (x5 x19)) (< x5 x5))) (false) (< x18 x18))) (if (false) (= x1 x1) (true))))) (let ((x6 (let ((x9 (call fun2 x1))) (if (let ((x10 (- x9 x9)) (x11 x9) (x12 (let ((x13 x9) (x14 (let ((x15 x9) (x16 (if (!= x9 x9) (let ((x18 (call fun4 x9)) (x19 (call fun5 x9 x9 -491 x9)) (x20 (call fun1 x9)) (x21 (if (!= x9 x9) (if (<= x9 x9) (- -28 -156) (call fun3 x9)) (call fun1 406))) (x22 (call fun2 x9))) (* x18 477)) (* x9 x9))) (x17 (call fun2 -81))) (- x15 x15)))) (let ((x23 (call fun4 x13)) (x24 (call fun3 x13))) (call fun5 x24 314 x23 x24))))) (if (let ((x25 (- x11 x12)) (x26 (let ((x28 x10) (x29 (if (let ((x32 (if (true) (call fun3 x10) (let ((x35 (call fun5 216 x11 x10 x11)) (x36 (let ((x39 442) (x40 (call fun4 x10)) (x41 (let ((x43 (* x10 x12)) (x44 x10)) (* -169 x43))) (x42 (- 443 -168))) (* 305 505))) (x37 x10) (x38 (- x12 x11))) (* 290 x36)))) (x33 (* x12 x12)) (x34 (+ x12 x11))) (true)) x10 x10)) (x30 x10) (x31 -209)) (+ 429 x28))) (x27 (- 296 -49))) (false)) (< x12 61) (= 69 x12))) x9 (- -311 x9)))) (x7 23) (x8 (- 427 x1))) (false))) (* x1 x1) (+ -43 x1))) (x17 (+ x1 x1))) (* -278 x16))) (x11 (+ x1 x1)) (x12 (- x1 40)) (x13 x1) (x14 (* -272 x1))) (+ x13 x14)))) (- x9 -266)))) (x6 x1) (x7 -507) (x8 (* x1 x1))) x5)))) (define fun3 (lambda (x1) (call fun5 x1 x1 x1 -326))) (define fun5 (lambda (x1 x2 x3 x4) (call fun5 x3 -504 -142 x4))) (define fun1 (lambda (x1) (call fun2 x1))) (define fun2 (lambda (x1) x1)) (let ((x1 (+ 73 -159)) (x2 (+ -198 449)) (x3 (call fun1 298)) (x4 (call fun1 -328)) (x5 (if (false) (- -103 -143) (if (not (true)) (let ((x2 (let ((x4 -270) (x5 (call fun4 -26)) (x6 (- 143 41)) (x7 (+ -305 -174))) (* x6 276))) (x3 -26)) x3) 398)))) (- x1 x4)))
  )

(define aProgram
  '(module
      (define fact
        (lambda (n)
          (if (= n 0)
              1
              (let ([x (- n 1)])
                (let ([new (call fact x)])
                  (* n new))))))
        (call fact 5)))

(module+ test
  (check-equal? #t #t "test"))

;#|
(parameterize (;[cc (cc)]
               ;[cc 'vanilla-cheri-riscv]
               ;[cc 'stktokens]
               ;[cc 'stkTokens-sentry]
               [cc 'cheri-linkage-seal]
               ;[cc 'cheri-linkage-trampoline]
               ;[cc 'risc-v]
               ;[cc 'tail-calls]
               )
  (parameterize ([steps                               (setup-steps (steps))]
                 [current-stack-base-pointer-register (setup-stack-register)]
                 [current-return-sealed-register      (setup-sealed-register)]
                 [current-seal-got-call-register      (setup-seal-call-register)]
                 [stack-direction                     (setup-stack-direction)]
                 ;[current-parameter-registers '()]
                 ;[current-assignable-registers '()]
                 )
    (println (steps))
    (compileStepsDis 2 (sub1 (length (steps))) swapProgram)))
;|#
;(compileStepsDis 2 (sub1 (length (steps))) simpleProgram)
;(interp-values-lang aProgram)
;(write-program testProgram "ap.S")
#|
(parameterize (;)
               [steps (halfStack (risc-v (steps)))])
               ;[current-parameter-registers '()]
               ;[current-assignable-registers '()]) 
  (compile-file "ap.txt"))
;|#
;(compile-file "ap.txt")
;(test simpleProgram)
