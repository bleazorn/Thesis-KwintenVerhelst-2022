#lang racket

(require "common.rkt")
(provide replace-locations)

(module+ test
  (require rackunit))

;
;(replace-triv t assign)
;t: triv?
;assign: list? '((aloc loc) ...)
(define (replace-triv t assign)
  (let ([l (assoc t assign)])
    (if (and l (aloc? (car l)))
        (second l)
        t)))

;
;(replace-effect e assign)->effect?
;e->effect?
;assign: list? '((aloc loc) ...)
(define (replace-effect e assign)
  (match e
    [`(begin ,e ...) `(begin ,@(map (lambda (eff) (replace-effect eff assign)) e))]
    [`(set! ,a (,binop ,b ,c)) `(set! ,(replace-triv a assign) (,binop ,(replace-triv b assign) ,(replace-triv c assign)))]
    [`(set! ,a ,b) `(set! ,(replace-triv a assign) ,(replace-triv b assign))] 
    [_ #f]))

;
;(replace-tail t assign)->tail?
;t: tail?
;assign: list? '((aloc loc) ...)
(define (replace-tail t assign)
  (match t
    [`(begin ,e ... ,tail) `(begin ,@(map (lambda (eff) (replace-effect eff assign)) e) ,(replace-tail tail assign))]
    [`(halt ,t) `(halt ,(replace-triv t assign))]
    [_ #f]))

;Compiles Asm-lang-V2-assignments to Asm-lang-V2-nested, replaced each abstract location with its assigned physical location from the assignment info field.
;(replace-locations p) → Asm-lang-V2-nested?
;p: Asm-lang-V2-assignments?
(define (replace-locations p)
  (match p
    [`(module () ,pro) p]
    [`(module ((locals ,loc) (assignment ,ass)) ,pro) (replace-tail pro ass)]
    [`(module ((locals ,loc) (conflicts ,conf) (assignment ,ass)) ,pro) (replace-tail pro ass)]
    [_ #f]))

(module+ test
;replace-locations
  ;succes
  (check-equal? (replace-locations
                 '(module ((locals (x.1)) (assignment ((x.1 rax))))
                    (begin
                      (set! x.1 0)
                      (halt x.1))))
                '(begin
                   (set! rax 0)
                   (halt rax))
                "replace-locations: succes-1: one location")
  (check-equal? (replace-locations
                 '(module ((locals (x.1 y.1 w.1))
                           (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                    (begin
                      (set! x.1 0)
                      (set! y.1 x.1)
                      (set! w.1 (+ w.1 y.1))
                      (halt w.1))))
                '(begin
                   (set! rax 0)
                   (set! rbx rax)
                   (set! r9 (+ r9 rbx))
                   (halt r9))
                "replace-locations: succes-2: multiple locations")
  )
