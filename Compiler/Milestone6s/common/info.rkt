#lang racket

;#|
(provide info?
         getInfo
         addInfo
         removeInfo
         getNewFrames
         getLocals
         getCallUndead
         getUndead-out
         getConflicts
         getAssignment
         getParamSize
         getFrameSize
         getAllocatedFvars
         getGOTLabels
         setNewFrames
         setLocals
         setCallUndead
         setUndead-out
         setConflicts
         setAssignment
         setParamSize
         setFrameSize
         setAllocatedFvars
         setGOTLabels
         remove-conf
         index-of-lowest-conf
         isInfo?)

(define (info? i)
  (cond [(list? i) (foldl (lambda (j b) (or b (isInfo? j))) #t i)]
        [else #f]))

(define (isInfo? a)
  (or (getNewFrames a) (getLocals a) (getCallUndead a) (getUndead-out a) (getConflicts a) (getAssignment a) (getParamSize a) (getFrameSize a) (getAllocatedFvars a) (getGOTLabels a)))

(define (getInfo i p)
  (let ([inf (filter p i)])
    (cond [(and (list? inf) (not (null? inf)) (list? (car inf)) (= (length (car inf)) 2)) (second (car inf))]
          [else '()])))

(define (addInfo i a)
  (cond [(and (isInfo? a) (info? i)) (let ([index (index-where i (lambda (x) (equal? (car x) (car a))))])
                                       (cond [index (list-set i index a)]
                                             [else (cons a i)]))]
        [else #f]))

(define (removeInfo i p)
  (filter-not p i))

(define (getNewFrames f)
  (match f
    [`(new-frames ,a) a]
    [_ #f]))

(define (getLocals l)
  (match l
    [`(locals ,a) a]
    [_ #f]))

(define (getCallUndead c)
  (match c
    [`(call-undead ,a) a]
    [_ #f]))

(define (getUndead-out u)
  (match u
    [`(undead-out ,a) a]
    [_ #f]))

(define (getConflicts c)
  (match c
    [`(conflicts ,a) a]
    [_ #f]))

(define (getAssignment a)
  (match a
    [`(assignment ,a) a]
    [_ #f]))

(define (getParamSize s)
  (match s
    [`(paramSize ,s) s]
    [_ #f]))

(define (getFrameSize f)
  (match f
    [`(frameSize ,f) f]
    [_ #f]))

(define (getAllocatedFvars a)
  (match a
    [`(allocatedFvars ,a) a]
    [_ #f]))

(define (getGOTLabels a)
  (match a
    [`(got-labels ,a) a]
    [_ #f]))

(define (setNewFrames f)
  `(new-frames ,f))

(define (setLocals l)
  `(locals ,l))

(define (setCallUndead c)
  `(call-undead ,c))

(define (setUndead-out u)
  `(undead-out ,u))

(define (setConflicts c)
  `(conflicts ,c))

(define (setAssignment a)
  `(assignment ,a))

(define (setParamSize s)
  `(paramSize ,s))

(define (setFrameSize f)
  `(frameSize ,f))

(define (setGOTLabels f)
  `(got-labels ,f))

(define (setAllocatedFvars f)
  `(allocatedFvars ,f))
;
;(remove-conf l conf)-> conflicts? '((aloc (aloc ...)) ...) 
;l: aloc?
;conf: conflicts? '((aloc (aloc ...)) ...) 
(define (remove-conf l conf)
  (map (lambda (c) (append `(,(car c)) `(,(remove l (second c))))) (remove (assoc l conf) conf)))


;Returns the index of the location with the least conflicts
;(index-of-lowest-conf conf)->integer?
;conf:conflicts?
(define (index-of-lowest-conf conf)
  (for/fold ([indexLowest 0]
             [curIndex 0]
             [lowestLength (length (second (car conf)))] #:result indexLowest)
            ([c conf])
    (let ([curLength (length (second c))])
      (values (if (> lowestLength curLength) curIndex indexLowest) (add1 curIndex) (if (> lowestLength curLength) curLength lowestLength)))))

(define testInfo '((assignment ((tmp-ra.11 t0)))
                   (assignment ((tmp-ra.11 t0)))
                   (conflicts
                    ((b ())
                     (a ())
                     (a0 (cra cfp))
                     (sv2 (cra fv0 fv1 cfp))
                     (sv1 (csp fv0 fv1 cfp))
                     (sv0 (csp cra fv0 fv1))
                     (fv1 (cfp fv0 tmp-ra.11 cra sv2 csp sv1 sv0))
                     (fv0 (fv1 tmp-ra.11 cra sv2 csp sv1 sv0 cfp))
                     (cra (cfp a0 fv0 fv1 sv2 csp sv0))
                     (csp (cra fv0 fv1 cfp sv1 sv0))
                     (cfp (cra fv1 a0 tmp-ra.11 sv2 csp sv1 fv0))
                     (tmp-ra.11 (fv0 fv1 cfp))))
                   (undead-out
                    ((csp cra fv0 fv1 cfp)
                     (csp cra fv0 fv1 cfp)
                     (cra fv0 fv1 cfp)
                     (fv0 fv1 cfp)
                     (fv0 fv1 cfp)
                     (((fv0 fv1) ((cfp a0) (cfp cra a0) (cfp cra a0)))
                      (fv1 cfp)
                      (cra cfp)
                      (cra cfp))))
                   (call-undead (fv0 fv1))
                   (locals (tmp-ra.11))
                   (paramSize -1)
                   (new-frames ())))
;|#

#|
(provide info%)

(define info% (class object%
                (define new-frames  '())
                (define locals      '())
                (define call-undead '())
                (define undead-out  '())
                (define conflicts   '())
                (define assignment  '())

                (super-new)
                
                (define/public (getNewFrames)
                  new-frames)

                (define/public (getLocals)
                  locals)
                
                (define/public (getCallUndead)
                  call-undead)
  
                (define/public (getUndead-out)
                  undead-out)
                
                (define/public (getConflicts)
                  conflicts)
                
                (define/public (getAssignment)
                  assignment)
                
                (define/public (setNewFrames f)
                  (set! new-frames f))
                
                (define/public (setLocals l)
                  (set! locals l))
                
                (define/public (setCallUndead c)
                  (set! call-undead c))
                
                (define/public (setUndead-out u)
                  (set! undead-out u))

                (define/public (setConflicts c)
                  (set! conflicts c))
                
                (define/public (setAssignment a)
                  (set! assignment a))))
;|#