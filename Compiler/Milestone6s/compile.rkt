#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
         "config.rkt"
         "steps.rkt"
         "interp-values-lang.rkt")
;(require racket/system)

(provide compile-file
         compile-program)

(module+ test
  (require rackunit))


;; SAND: createList already exists, it's called inclusive-range

(define (compileStepsDis start end program)
  (resetfvar)
  (println (steps))
  (for/fold ([p program])
             ([i (reverse (inclusive-range start end))])
     (values (let ([fun (list-ref (steps) i)])
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
    (values (let* ([fun (list-ref (steps) i)]
                   [res (fun p)])
              res))))

(define (compile program)
  (println "Compiling Program")
  (println program)
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

(define (setup-cc steps)
  (match (cc)
    ['vanilla-riscv steps]
    ['stktokens     (stkTokens steps)]
    [unknown        (error "unsupported calling convention: " unknown)]))

(define (compile-file file)
  (parameterize ([steps (setup-cc (steps))]
                 [fvarRegister 'csp]
                 [stack-direction '+])
                 ;[current-parameter-registers '()]
                 ;[current-assignable-registers '()]) 
    (write-program (read-program-from-file file) (format "~a.S" (car (string-split file "."))))))

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
  '(module (define fun1 (lambda (x1) (call fun2 -385 375))) (define fun2 (lambda (x1 x2) (if (let ((x3 (* x1 x1)) (x4 506) (x5 (- -510 x2))) (<= x3 x5)) (call fun5 -293 209 x1) (* -79 182)))) (define fun5 (lambda (x1 x2 x3) (call fun3 x3 x3))) (define fun3 (lambda (x1 x2) (let ((x3 x1) (x4 (if (false) (call fun4 x1 x1 x2) x1)) (x5 (call fun4 x1 x1 x1)) (x6 (if (true) (let ((x4 (let ((x5 (+ x2 x1)) (x6 (if (false) (* x1 x1) (let ((x10 (if (true) (let ((x14 (if (not (if (true) (< x2 x1) (false))) (call fun4 x1 x1 -110) (call fun2 174 x1)))) (let ((x15 (call fun1 x14)) (x16 (let ((x17 (let ((x21 (call fun2 111 x14)) (x22 (call fun2 x14 x14))) (call fun1 x22))) (x18 (if (not (= x14 x14)) (- x14 43) (if (= x14 x14) (* x14 x14) (let ((x23 (if (true) (call fun3 29 x14) (call fun4 -426 x14 x14))) (x24 (let ((x27 (let ((x31 (call fun1 x14)) (x32 (call fun5 x14 x14 x14))) (call fun5 -326 x32 -132))) (x28 x14) (x29 (call fun4 x14 x14 446)) (x30 (let ((x33 x14) (x34 (if (let ((x37 x14)) (if (true) (not (true)) (let ((x38 x37) (x39 (call fun2 x37 x37)) (x40 (call fun5 x37 -69 x37))) (> x40 x38)))) (let ((x41 x14)) (if (false) (if (false) (call fun4 338 -268 x41) (- x41 308)) (- -70 x41))) (* x14 x14))) (x35 (+ x14 -383)) (x36 -81)) (* x36 -44)))) (- x30 x27))) (x25 (- x14 x14)) (x26 x14)) 376)))) (x19 (+ -138 x14)) (x20 x14)) (+ x20 -6)))) x15)) (- -279 -195))) (x11 x2) (x12 x1) (x13 -326)) 414))) (x7 (* x1 428)) (x8 x1) (x9 (- x1 -292))) x9))) -19) x2)) (x7 438)) (- x5 -7)))) (define fun4 (lambda (x1 x2 x3) (call fun1 x2))) (if (if (true) (let ((x1 (call fun1 -446)) (x2 -351) (x3 -99) (x4 -392)) (false)) (< -286 76)) (* -364 382) -139))
  )

(module+ test
  (check-equal? #t #t "test"))

;#|
(parameterize ([steps (stkTokens (steps))]
               [fvarRegister 'csp]
               [stack-direction '+]
               ;[steps (halfStack (risc-v (steps)))])
               [current-parameter-registers '()]
               [current-assignable-registers '()]) 
  (compileStepsDis 2 (sub1 (length (steps))) swapProgram))
;|#
;(compileStepsDis 2 (sub1 (length (steps))) simpleProgram)
;(interp-values-lang bigProgram)
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
