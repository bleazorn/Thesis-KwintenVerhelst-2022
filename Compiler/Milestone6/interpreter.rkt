#lang racket

(module+ test
  (require rackunit))

;stack := (list (cons reg integer) ...)
(define (get-stack reg stack)
  (let ([r (assoc reg stack)])
    (if r
        (cdr r)
        #f)))

;set := (cons reg integer)
;throws defined error if not in stack !!change to user defined
(define (set-stack set stack)
  (if set
    (let ([i (index-where stack (lambda (s) (equal? (car set) (car s))))])
      (if i
          (list-set stack i set)
          (append stack `(,set))))
    #f))

(define (interp-triv triv stack)
  (match triv
    [i #:when (integer? i) i]
    [r (get-stack r stack)]))

(define (interp-mov reg triv stack)
  (set-stack (cons reg (interp-triv triv stack)) stack))

(define (interp-binop reg binop stack)
  (match binop
    [`(* ,a ,b) (interp-mov reg (* (interp-triv a stack) (interp-triv b stack)) stack)]
    [`(+ ,a ,b) (interp-mov reg (+ (interp-triv a stack) (interp-triv b stack)) stack)]
    [_ (print binop)]))


(define (interp-set set stack)
  (match set
    [`(set! ,a (,binop ,b ,c)) (interp-binop a `(,binop ,b ,c) stack)]
    [`(set! ,a ,b) (interp-mov a b stack)]
    [_ (print "at set")]))

(define (find-ret stack)
  (get-stack 'rax stack))
  

(define (interp-instrs instrs stack)
  (let ([s (foldl (lambda (i s) (interp-set i s)) stack instrs)])
    (find-ret s)))

(define (interp-paren-cheri-risc-v p)
    (match p
    [`(begin ,s ...) (interp-instrs s '())]
    [_ (error "wrong syntax")]))


(module+ test
  (check-equal? #t #t "first test"))

(define testStack (list (cons 'rax 15) (cons 'rbx 16)))
 
testStack
(interp-set '(set! rax (+ rax 42)) testStack)


(interp-paren-cheri-risc-v
   '(begin
      (set! rax 0)
      (set! rax (+ rax 42))))
