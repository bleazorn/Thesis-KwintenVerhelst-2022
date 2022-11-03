#lang racket

(require racket/system)
(module+ test
  (require rackunit))

(define (check-program-file program result)
  (define-values (sp out in err)
    (subprocess #f #f #f "/bin/bash" "./tester.sh" program (number->string result)))
  (define output (port->string out))
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  (subprocess-wait sp)
  output
  )

(define (check-program-p program result)
  (write-string-to-file "tmpTest" program)
  (define output (check-program-file "tmpTest" result))
  (delete-file "tmpTest")
  (pretty-display (format "~a\nResult: ~a" program output)) 
  (second (string-split output "\n")))

(define (write-string-to-file file program)
  (cond [(file-exists? file) (delete-file file)])
  (with-output-to-file file
    (lambda () (printf (~a program)))))

(define (check-program program result)
  (if (and (string? program) (file-exists? program))
      (check-program-file program result)
      (check-program-p program result)))

(module+ test
  (define (check-Program program result text)
    (check-equal? (check-program program result) "Test Succeed" text))
  (define (check-Program-failed program result text)
    (check-equal? (check-program program result) "Test Failed" text))
  
;Milestone 2 en 3
  ;succes
  ; #|
  (check-Program '(module 50) 50 "Program: succes-1: value integer")
  (check-Program '(module -50) -50 "Program: succes-2: value negative integer")
  (check-Program '(module (+ 50 1)) 51 "Program: succes-3: value binop")
  (check-Program '(module (let ([x 10]) (+ 50 x))) 60 "Program: succes-4: let one var")
  (check-Program '(module (let ([x 10] [y 5]) (+ y x))) 15 "Program: succes-5: let two var")
  (check-Program '(module (let ([x (+ 2 3)]) (+ 50 x))) 55 "Program: succes-6: binop in let")
  (check-Program '(module (let ([x (+ 2 3)]) (let ([y (* 6 10)]) (+ y x)))) 65 "Program: succes-7: nested let")
  (check-Program '(module (let ([a 1] [b 2]) (let ([c (+ a b)]) (let ([x (+ a b)] [y (+ a c)] [l 5]) (+ c x))))) 6 "Program: succes-7: complex program")
  (check-Program '(module (let ([a 1] [b 2]) (let ([c (+ a b)]) (let ([x (+ a b)] [y (+ a c)] [l 5]) (let ([z (+ a b)] [d (+ y x)] [e (+ c l)]) (+ c e)))))) 11 "Program: succes-9: put things in memory")
  (check-Program '(module (let ([x -10] [y -5]) (+ y x))) -15 "Program: succes-10: neg vars")
  (check-Program '(module (let ([x -10] [y (let ([z 10] [b 5]) (+ z b))]) (+ y x))) 5 "Program: succes-11: let value")
  
  ;failure
  (check-Program-failed '(module (let ([a 1] [b 2] [c 3] [x 4] [y 5] [z 6]) (* (+ (* a b) (+ x y)) (* c z)))) 29 "Program: succes-1: binop can only have trivs")

  ;|#
;Milestone 4
  ;#|
  (check-Program '(module (if (true) 17 18)) 17 "Program: succes-12: if true")
  (check-Program '(module (if (false) 17 18)) 18 "Program: succes-13: if false")
  (check-Program '(module (let ([x 50] [y 40]) (if (= x y) (+ x y) (* x y)))) 2000 "Program: succes-14: if compare")  
  (check-Program '(module (let ([x 50] [y 40]) (if (not (= x y)) (+ x y) (* x y)))) 90 "Program: succes-15: if not")
  
  (check-Program '(module (let ([x 50] [y 40]) (if (not (if (= x y) (< x y) (> x y))) (+ x y) (* x y)))) 2000 "Program: succes-16: if not")
  (check-Program '(module (let ([x 50] [y 40]) (if (let ([z 5]) (< z y)) (+ x y) (* x y)))) 90 "Program: succes-17: if let")
  (check-Program '(module (let ([x 50] [y 50]) (let ([z (if (= x y) (let ([k 40]) k) (* x y))]) z))) 40 "Program: succes-18: value if")
  ;|#
  )