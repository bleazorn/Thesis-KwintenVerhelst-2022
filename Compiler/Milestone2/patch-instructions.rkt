#lang racket

(require "common.rkt")
(provide patch-instructions)

(module+ test
  (require rackunit))

;claims t4 t5 t6
(define t 6)

(define (newTemp)
  (if (>= t 6)
      (set! t 5)
      (set! t (add1 t)))
  (string->symbol (format "t~a" t)))

(define (resetTemp)
  (set! t 6))

(define firstClaimReg 't5)

(define (triv? t)
  (or (integer? t) (or (fvar? t) (isRegister? t))))

;##########################################################################################################
#|
;
;
(define (select-binop binop t1 t2 tail)
  (if (and (triv? t1) (triv? t2))
      (let* ([tmp1 (if (integer? t1) (freshtmp) t1)]
             [tmp2 (if (and (equal? binop '*) (integer? t2)) (freshtmp) t2)]
             [pre1 (if (integer? t1) `((set! ,tmp1 ,t1)) '())]
             [pre2 (if (and (equal? binop '*) (integer? t2)) (append pre1 `((set! ,tmp2 ,t2))) pre1)] 
             )
        (append pre2 `((set! ,tmp1 (,binop ,tmp1 ,tmp2))) `(,(append tail `(,tmp1)))))
      #f))


;
;(patch-triv a b c)->list? '((set! ...) ...)
;trivs:list? '(triv? ...)
(define (patch-triv trivs)
  (for/fold ([i 4] [l '()] [same '()] #:result l)
            ([t trivs])
    (if (and (or (fvar? t) (integer? t)) (not (assoc t same)))
        (values (add1 i)
                (append l `((,t ,(string->symbol (string-append "t" (~a i))))))
                (cons `(,t ,(string->symbol (string-append "t" (~a i)))) same))
        (let ([isSame (assoc t same)])
          (if isSame
              (values i
                      (append l `((,(car isSame) ,(second isSame))))
                      same)
              (values i
                      (append l `((,t ,t)))
                      same))))))

;
;(patch-binop a b c binop)
;a,b,c:triv?
(define (patch-binop2 a b c binop)
  (if (and (triv? a) (and (triv? b) (triv? c)))
      (let ([trivs (if (and (integer? c) (equal? '+ binop)) (patch-triv `(,a ,b)) (patch-triv `(,a ,b ,c)))])
        (for/fold ([sets (if (and (integer? c) (equal? '+ binop))
                             `((set! ,(second (car trivs)) (,binop ,(second (second trivs)) ,c)))
                             `((set! ,(second (car trivs)) (,binop ,(second (second trivs)) ,(second (third trivs))))))]
                   [same '()] #:result sets)
                  ([t trivs])
          (if (or (equal? (car t) (second t)) (member (car t) same))
              (values sets same)
              (values (cons `(set! ,(second t) ,(car t)) sets) (cons (car t) same)))))
      #f))
;|#

;
;(patch-binop a b c binop)->list? '((set! ...) ...)
;a,b,c:triv?
(define (patch-binop a b c binop)
  (let* ([aReg (if (isRegister? a) a (newTemp))]
         [bReg (cond
                 [(isRegister? b) b]
                 [(equal? a b) aReg]
                 [else (newTemp)])]
         [cReg (cond
                 [(isRegister? c) c]
                 [(equal? b c) bReg]
                 [else (newTemp)])])
    (let ([aSet (if (isRegister? a) '() `((set! ,a ,aReg)))]
          [bSet (cond
                 [(isRegister? b) '()]
                 [else `((set! ,bReg ,b))])]
          [cSet (cond
                 [(isRegister? c) '()]
                 [(equal? b c) '()]
                 [else `((set! ,cReg ,c))])])
      (append bSet cSet `((set! ,aReg (,binop ,bReg ,cReg))) aSet))))
    

;Longer version from patch-set to better visualize every step
;(patch-setLong s)->list? '((set! ...) ...)
;s: (set! ...)
(define (patch-setLong s)
  (match s
    [`(set! ,a ,b) #:when (and (isRegister? a) (isRegister? b)) `((set! ,a ,b))]              ;copy register
    [`(set! ,a ,b) #:when (and (isRegister? a) (integer? b)) `((set! ,a ,b))]                 ;integer in register
    [`(set! ,a ,b) #:when (and (isRegister? a) (fvar? b)) `((set! ,a ,b))]                    ;load in register
    [`(set! ,a ,b) #:when (and (fvar? a) (isRegister? b)) `((set! ,a ,b))]                    ;store register in memory
    [`(set! ,a ,b) #:when (and (fvar? a) (integer? b)) `((set! ,firstClaimReg ,b) (set! ,a ,firstClaimReg))]          ;store integer in memory
    [`(set! ,a ,b) #:when (and (fvar? a) (fvar? b)) `((set! ,firstClaimReg ,b) (set! ,a ,firstClaimReg))]))           ;store memory in memory

;
;(patch-set s)->list? '((set! ...) ...)
;s: (set! ...)
(define (patch-set s)
  (match s
    [`(set! ,a ,b) #:when (or (isRegister? a) (isRegister? b)) `((set! ,a ,b))]                  
    [`(set! ,a ,b) #:when (and (fvar? a) (not (isRegister? b))) (let ([reg (newTemp)])
                                                                  `((set! ,reg ,b) (set! ,a ,reg)))]
    [_ #f]))


;
;(patch-effect e)->list? '((set! ...) ...)
;e->effect
(define (patch-effect e)
  (match e
    [`(set! ,a (,binop ,b ,c)) (patch-binop a b c binop)]
    [`(set! ,a ,b) (patch-set e)]
    [_ #f]))

;
;(patch-instructions p) → paren-x64-fvars-v2?
;p : para-asm-lang-v2?
(define (patch-instructions p)
  (match p
    [`(begin ,s ... (halt ,t))  `(begin ,@(foldl (lambda (e sets) (append sets (patch-effect e))) '() s) (set! a0 ,t))]
    [_ #f]))

;###############################################################################################################################################################

#|
;Returns reg is triv is activated otherwise returns triv
;(patch-triv t act)->triv?
;t: triv?
;act: list?   '(((triv reg)...) (triv))
(define (patch-triv t act)
  (let ([reg (assoc t (car act))])
    (if reg
        (second reg)
        t)))
        
;checks if trivs exist in register en need to be put in a register, if so create a set instruction
;(patch-triv trivs act)->list? '((set! ...) ...)
;trivs:list? '(triv?)
;act: list?   '(((triv reg)...) (triv))
(define (patch-trivs trivs act)
  (for/fold ([inst '()])
            ([t trivs])
    (let ([reg (assoc t (car act))]
          [justActivated (member t (second act))])
      (if justActivated
          (append inst `((set! ,(second reg) ,t)))
          inst))))


;
;(patch-binop a b c binop)->list? '((set! ...) ...)
;a,b,c:triv?
;binop:symbol?   '+/'*
;act: list?   '(((triv reg)...) (triv))
(define (patch-binop a b c binop act)
  (append `(,@(patch-trivs `(,a ,b ,c) act)) `((set! ,(patch-triv a act) (,binop ,(patch-triv b act) ,(patch-triv c act))))))
          
      
      
  
;
;(patch-set s)->list? '((set! ...) ...)
;s: (set! ...)
;act: list?   '(((triv reg)...) (triv))
(define (patch-set a b act)
  (append `(,@(patch-trivs `(,b ,a) act)) `((set! ,(patch-triv a act) ,(patch-triv b act)))))

;
;(patch-halt a act)->list? '((set! ...) ...)
;a->triv
;act: list?   '(((triv reg)...) (triv))
(define (patch-halt a act)
  (append `(,@(patch-trivs `(,a) act)) `((set! a0 ,(patch-triv a act)))))


;
;(patch-effect e)->list? '((set! ...) ...)
;e->effect
;act: list?   '(((triv reg)...) (triv))
(define (patch-effect e act)
  (match e
    [`(set! ,a (,binop ,b ,c)) (patch-binop a b c binop act)]
    [`(set! ,a ,b) (patch-set a b act)]
    [`(halt ,a) (patch-halt a act)]
    [_ #f]))
  
;#####################################################################################

;
;(patch-act-fvar t newT act)->list?   '(((triv reg)...) (triv))
;t: triv?
;newT: triv?
;act: list?   '(((triv reg)...) (triv))
(define (patch-act-fvar t newT act)
  (let ([active (car act)]
        [jActive (second act)])
    (if (assoc t active)
        act                                                           ;it already activated for a while, so nothing changes
        (append `(,(cons `(,t ,newT) active)) `(,jActive)))))          ;add it to active and put it  in just activated


;Checks if triv is in a register, otherwise creates a register to add it into
;(patch-act t newT act)->list?   '(((triv reg)...) (triv))
;t: triv?
;newT: triv?
;act: list?   '(((triv reg)...) (triv))
(define (patch-act t newT act)
  (let ([active (car act)]
        [jActive (second act)])
    (if (assoc t active)
        (if (member t jActive)
            (append `(,active) `(,(remove t jActive)))                     ;delete the entry from just activated
            act)                                                           ;it already activated for a while, so nothing changes
        (append `(,(cons `(,t ,newT) active)) `(,(cons t jActive))))))          ;add it to active and put it  in just activated
        
;
;(patch-binop-act a b c binop act)->list?   '(((triv reg)...) (triv))
;a b c: triv?
;binop: symbol? '+/'*
;act: list?   '(((triv reg)...) (triv))
(define (patch-binop-act a b c binop act)
  (let* ([actA (if (isRegister? a) act (patch-act a (newTemp) act))]
         [actB (if (isRegister? b) actA (patch-act b (newTemp) actA))])
    (if (or (isRegister? c)
            (and (integer? c) (equal? binop '+)))
        actB
        (patch-act c (newTemp) actB))))

;
;(patch-set-act a b act)->list?   '(((triv reg)...) (triv))
;a b c: triv?
;binop: symbol? '+/'*
;act: list?   '(((triv reg)...) (triv))
(define (patch-set-act a b act)
  (if (or (isRegister? a) (isRegister? a))
      act
      (let ([newT (newTemp)])
        (patch-act a newT (patch-act b newT act)))))

;
;(patch-effect-ada e ada)->list?   '(((triv reg)...) (triv))
;e: effect?/tail?
;act: list?   '(((triv reg)...) (triv))
(define (patch-effect-act e act)
  (match e
    [`(set! ,a (,binop ,b ,c))  (patch-binop-act a b c binop act)]
    [`(set! ,a ,b) (patch-set-act a b act)]
    [`(halt ,a) act]                                                               
    [_ #f]))

;
;(patch-begin e t)->list? '((set! ...) ...)
;e->list? '(effect? ...)
;t: triv
(define (patch-begin e)
  ;(println e)
  (for/fold ([act '(() ())]                ;actives '(((triv reg)...) (triv))
             [inst '()] #:result inst)                   ;'((set! ...)...)                         
            ([eff e])
    (let ([curAct (patch-effect-act eff act)])
      ;(println eff)
      ;(println curAct)
      ;(println inst)
      (values curAct (append inst (patch-effect eff curAct))))))


;
;(patch-instructions p) → paren-x64-fvars-v2?
;p : para-asm-lang-v2?
(define (patch-instructions p)
  ;(println "begin")
  (match p
    [`(begin ,s ...)  `(begin ,@(patch-begin s))]
    [_ #f]))

;(patch-instructions '(begin (set! t0 1) (set! t1 2) (set! t2 3) (set! t3 4) (set! fv0 5) (set! fv0 (+ fv0 fv0)) (halt fv0)))
;|#

(module+ test
  (define (check-patch? t1 t2 text)
    (resetTemp)
    (check-equal? t1 t2 text))
  #|
;patch-triv
  (check-patch? (patch-triv 'a0 '(((fv0 a0)(fv1 t0)) (fv0 fv1)))
                'a0
                "patch-triv: succes-1: not activated")
  (check-patch? (patch-triv 'fv0 '(((fv0 a0)(fv1 t0)) (fv0 fv1)))
                'a0
                "patch-triv: succes-2: activated")
;patch-trivs
  ;succes
  (check-patch? (patch-trivs '(a0) '(() ()))
                '()
                "patch-trivs: succes-1: just a register")
  (check-patch? (patch-trivs '(fv0) '(((fv0 a0)) ()))
                '()
                "patch-trivs: succes-2: already in register memory")
  (check-patch? (patch-trivs '(fv0) '(((fv0 a0)) (fv0)))
                '((set! a0 fv0))
                "patch-trivs: succes-3: not in register memory")
  (check-patch? (patch-trivs '(fv0 fv0) '(((fv0 a0)) ()))
                '()
                "patch-trivs: succes-4: two same memories already in a register")
  (check-patch? (patch-trivs '(fv0 a0) '(((fv0 a0)) (fv0)))
                '((set! a0 fv0))
                "patch-trivs: succes-5: one register one memory not in register")
  (check-patch? (patch-trivs '(fv0 fv1) '(((fv0 a0)(fv1 t0)) (fv0)))
                '((set! a0 fv0))
                "patch-trivs: succes-6: two memories on already in register")
  (check-patch? (patch-trivs '(fv0 fv1) '(((fv0 a0)(fv1 t0)) (fv0 fv1)))
                '((set! a0 fv0) (set! t0 fv1))
                "patch-trivs: succes-7: two memories not in register")
;patch-binop
  ;succes
  (check-patch? (patch-binop 'a0 'a1 'a2 '+ '(((fv0 t4)(fv1 t5)) (fv0 fv1)))
                '((set! a0 (+ a1 a2)))
                "patch-binop: succes-1: all registers")
  (check-patch? (patch-binop 'a0 'fv1 'a2 '+ '(((fv0 t4)(fv1 t5)) (fv0)))
                '((set! a0 (+ t5 a2)))
                "patch-binop: succes-2: one memory already in a register")
  (check-patch? (patch-binop 'a0 'fv1 'a2 '+ '(((fv0 t4)(fv1 t5)) (fv0 fv1)))
                '((set! t5 fv1) (set! a0 (+ t5 a2)))
                "patch-binop: succes-3: one memory not in a register")
  ;|#
;patch-instructions
  ;succes
  (check-patch? (patch-instructions '(begin (set! a1 42) (halt a1)))
                '(begin (set! a1 42) (set! a0 a1))
                "patch-instructions: succes-1: one instruction")
  (check-patch? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv1 42)
                    (set! fv0 fv1)
                    (halt fv0)))
                '(begin (set! t5 0) (set! fv0 t5) (set! t6 42) (set! fv1 t6) (set! t5 fv1) (set! fv0 t5) (set! a0 fv0))
                "patch-instructions: succes-2: a fvar in second argument")
  (check-patch? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv1 42)
                    (set! fv0 (+ fv0 fv1))
                    (halt fv0)))
                '(begin (set! t5 0) (set! fv0 t5) (set! t6 42) (set! fv1 t6) (set! t5 fv0) (set! t6 fv1) (set! t5 (+ t5 t6)) (set! fv0 t5) (set! a0 fv0))
                "patch-instructions: succes-3: fvars in binop")
  (check-patch? (patch-instructions
                 '(begin
                    (set! t1 0)
                    (set! t2 0)
                    (set! t3 42)
                    (set! t1 t2)
                    (set! t1 (+ t1 t3))
                    (halt t1)))
                '(begin (set! t1 0) (set! t2 0) (set! t3 42) (set! t1 t2) (set! t1 (+ t1 t3)) (set! a0 t1))
                "patch-instructions: succes-4: multiple instructions no changes"))
