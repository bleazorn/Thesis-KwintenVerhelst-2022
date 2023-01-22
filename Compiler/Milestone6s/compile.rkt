#lang racket

(require "common/fvar.rkt"
         "common/register.rkt"
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

(define (compile-file file)
  (parameterize ([steps (stkTokens (steps))]
                 [fvarRegister 'csp])
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
  '(module (define fun1 (lambda (x1 x2 x3) (let ((x4 (+ x3 x3)) (x5 (call fun4 -134 x1 x3)) (x6 (- x3 x2))) (call fun5 x5)))) (define fun4 (lambda (x1 x2 x3) (call fun3 x3 x3 x2))) (define fun3 (lambda (x1 x2 x3) (call fun2 x1))) (define fun2 (lambda (x1) (call fun5 x1))) (define fun5 (lambda (x1) x1)) (if (let ((x1 (if (true) (- -448 443) (if (false) (if (true) (- -141 -126) (call fun3 -228 -239 339)) (call fun1 429 -234 -66)))) (x2 (if (<= 267 -403) (* -341 -265) (call fun4 -423 294 47))) (x3 (call fun1 -329 -176 -445)) (x4 (call fun1 -469 496 -28)) (x5 (let ((x4 (call fun2 308))) (if (let ((x5 (call fun4 x4 x4 x4)) (x6 (- 235 x4)) (x7 (- 432 x4))) (let ((x8 (let ((x13 (* x5 x7))) (call fun3 156 x13 x13))) (x9 (call fun1 x6 x5 -74)) (x10 (let ((x14 (if (true) (- x5 x5) x6)) (x15 (call fun5 x5)) (x16 (+ x5 x6))) (- 197 x14))) (x11 (call fun5 x7)) (x12 (call fun2 443))) (true))) (call fun3 x4 x4 x4) (call fun3 267 x4 x4))))) (let ((x17 (call fun3 x2 x3 x4)) (x18 (call fun4 x1 x2 x4)) (x19 (let ((x20 (call fun4 x3 x5 x1)) (x21 (let ((x25 x4) (x26 (call fun3 x3 x4 -464)) (x27 (call fun3 x4 x3 x3))) (call fun4 -475 x25 x25))) (x22 (call fun2 x2)) (x23 (call fun2 x5)) (x24 (if (true) (call fun2 x5) (* x2 x4)))) (if (>= x21 x24) (let ((x28 (let ((x29 (if (not (> x24 x21)) (call fun2 x22) (if (let ((x30 -8)) (!= 347 179)) (+ x21 x21) 304)))) (+ x29 x29)))) (+ 511 x28)) 394)))) (!= x19 -257))) (- -226 443) 313))
  )

(module+ test
  (check-equal? #t #t "test"))

;#|
(parameterize ([steps (stkTokens (steps))]
               [fvarRegister 'csp])
               ;[steps (halfStack (risc-v (steps)))])
               ;[current-parameter-registers '()]
               ;[current-assignable-registers '()]) 
  (compileStepsDis 2 (sub1 (length (steps))) testProgram))
;|#
;(compileStepsDis 2 (sub1 (length (steps))) simpleProgram)
;(interp-values-lang testProgram)
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
