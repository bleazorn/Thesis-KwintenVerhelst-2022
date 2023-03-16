#lang racket

(provide generate-values-lang)
(require "interp-values-lang.rkt")

;Get all different members in list and sort them
;(getDiff l)->list? '((any? integer?) ...)
;l: list?
(define (getDiff l)
  (sort (foldl (lambda (e n) (if (assoc e n)
                                 n
                                 (cons `(,e 0) n))) '() l) #:key car <))
;build a list of n random numbers
;(getRandoms n max)->list?
;n, max: integer?
(define (getRandoms n max)
  (build-list n (lambda (x) (random max))))

;Get the distribution of a list
;(dist lis)->list? '((any? integer?) ...)
;lis: list?
(define (dist lis)
  (foldl (lambda (e n) (let* ([a (assoc e n)]
                              [i (index-of n a)])
                         (list-set n i `(,(car a) ,(add1 (second a)))))) (getDiff lis) lis))

;(dist (getRandoms 100000 10))

(define integerBit 9)

(define n 0)

(define (newName)
  (set! step (add1 step))
  (set! n (add1 n))
  (format "x~a" n))

(define step 0)

(define (genFuncs)
  (build-list 5  (lambda (x) `(,(format "fun~a" (add1 x)) ,(random 1 5) #f))))

(define funcs (genFuncs))

(define (resetGen)
  (set! n 0)
  (set! step 0))

(define (resetAll)
  (set! n 0)
  (set! step 0)
  (set! funcs (genFuncs)))

(define maxSteps 200)

(define (getRandom num axi)
  (set! step (add1 step))
  (if (< step maxSteps)
      (random num)
      (random axi)))

;
;(generate-names names)->name?
;names->list? '(name? ...)
(define (generate-names names)
  (if (null? names)
      (generate-integer)
      (let ([i (random (length names))])
        (list-ref names i))))

;
;(generate-integer)->integer?
(define (generate-integer)
  (match (random 2)
    [0 (random (expt 2 integerBit))]
    [1 (random (expt -2 integerBit) 0)]))

;
;(generate-triv names)->triv?
;names->list? '(name? ...)
(define (generate-triv names)
  (match (getRandom 5 2)
    [0 (generate-integer)]
    [1 (generate-names names)]
    [_ (generate-names names)]))

;
;(generate-relop)->relop?
(define (generate-relop)
  (match (getRandom 6 6)
    [0 '=]
    [1 '!=]
    [2 '<]
    [3 '>]
    [4 '<=]
    [5 '>=]))

;
;(generate-binop)->binop?
(define (generate-binop)
  (match (getRandom 2 2)
    [0 '+]
    [1 '*]))

;
;(generate-let)->list? list? '(name? ...) '((name? value?) ...)
;names->list? '(name? ...)
(define (generate-let names)
  (let ([newNames (build-list (add1 (getRandom 5 1)) (lambda (x) (newName)))])
    (values newNames (map (lambda (a) `[,a ,(generate-value names)]) newNames))))

;
;(generate-pred names)->pred?
;names->list? '(name? ...)
(define (generate-pred names)
  (match (getRandom 6 3)
    [0 `(,(generate-relop) ,(generate-triv names) ,(generate-triv names))]
    [1 '(true)]
    [2 '(false)]
    [3 `(not ,(generate-pred names))]
    [4 (let-values ([(newNames letName) (generate-let names)])
         `(let ,letName ,(generate-pred newNames)))]
    [5 `(if ,(generate-pred names) ,(generate-pred names) ,(generate-pred names))]
    [_ `(,(generate-relop) ,(generate-triv names) ,(generate-triv names))]))

;
;(generate-value names)->value?
;names->list? '(name? ...)
(define (generate-value names)
  (match (getRandom 4 2)
    [0 (generate-triv names)]
    [1 `(,(generate-binop) ,(generate-triv names) ,(generate-triv names))]
    [2 (let-values ([(newNames letName) (generate-let names)])
         `(let ,letName ,(generate-value newNames)))]
    [3 `(if ,(generate-pred names) ,(generate-value names) ,(generate-value names))]
    [_ (generate-triv names)]))


;
;(createFunc funcs)->'(define name? (lambda (name? ...) tail?))
;i: integer?
;funcs: '((name? integer? boolean?) ...)
(define (createFunc i)
  (resetGen)
  (let* ([fun (list-ref funcs i)]
         [args (build-list (second fun) (lambda (x) (newName)))])
    (set! funcs (list-set funcs i (list-set fun 2 #t)))
    (let-values ([(tail tDefines) (generate-tail args)])
      (cons `(define ,(first fun) (lambda ,args ,tail)) tDefines))))


;
;(generate-func funcs)->'(call name? name? ...) '((define name? (lambda (name? ...) tail?)) ...)
;names->list? '(name? ...)
;funcs: '((name? integer? boolean?) ...)
(define (generate-func names)
  (let* ([i (random (length funcs))]
         [fun (list-ref funcs i)])
    (values `(call ,(first fun) ,@(build-list (second fun) (lambda (x) (generate-triv names)))) (cond [(third fun) '()]
                                                                                                      [else (createFunc i)]))))

;
;(generate-tail names)->tail? '((define name? (lambda (name? ...) tail?)) ...)
;names->list? '(name? ...)
;funcs: '((name? integer? boolean?) ...)
(define (generate-tail names)
  (match (getRandom 5 1)
    [0 (values (generate-value names) '())]
    [1 (let*-values ([(newNames letName) (generate-let names)]
                     [(newTail newFuncs) (generate-tail newNames)])
         (values `(let ,letName ,newTail)
                 newFuncs))]
    [2 (let-values ([(newTail1 newFuncs1) (generate-tail names)]
                    [(newTail2 newFuncs2) (generate-tail names)])
         (values `(if ,(generate-pred names) ,newTail1 ,newTail2)
                 (append newFuncs1 newFuncs2)))]
    [3 (generate-func names)]
    [4 (generate-func names)]
    [_ (generate-value names)]))


;
;(generate-program n)->Values-lang
;n: integer?
(define (generate-program)
  (let-values ([(newTail newFuncs) (generate-tail '())])
    `(module ,@newFuncs ,newTail)))

;
;(generate-values-lang)->Values-lang/string?
;n: integer?
(define (generate-values-lang)
  (resetAll)
  (let ([p (generate-program)])
    (if (integer? (interp-values-lang p))
        p
       "Generation Failed")))


(define (randomTest n)
  (for ([i (build-list n values)])
    (generate-values-lang)))

;(randomTest 10)