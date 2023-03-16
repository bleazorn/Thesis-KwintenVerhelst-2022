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
         setNewFrames
         setLocals
         setCallUndead
         setUndead-out
         setConflicts
         setAssignment
         remove-conf
         index-of-lowest-conf)

(define (info? i)
  (cond [(list? i) (foldl (lambda (j b) (or b (isInfo? j))) #t i)]
        [else #f]))

(define (isInfo? a)
  (or (getNewFrames a) (getLocals a) (getCallUndead a) (getUndead-out a) (getConflicts a) (getAssignment a)))

(define (getInfo i p)
  (let ([inf (filter p i)])
    (cond [(and (list? inf) (list? (car inf)) (list? (second (car inf))) (= (length (car inf)) 2)) (second (car inf))]
          [else #f])))

(define (addInfo i a)
  (cond [(and (isInfo? a) (info? i)) (cons a i)]
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