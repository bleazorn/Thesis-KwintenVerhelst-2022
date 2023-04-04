#lang racket

;normal
'(module ()
   (define L.swap.1
     ()
     (begin
       (set! fv0 cra)
       (begin
         (set! t2 a1)    ;x
         (set! t0 a2)    ;y
         (if (< t0 t2)
             (begin (set! a0 t2) (jump fv0))   ;x
             (begin
               (begin
                 (set! t1 t0)  ;y
                 (set! t0 t2)  ;x
                 (begin
                   (begin
                     (set! cfp (- cfp 16))
                     (return-point L.rpLabel.9 (begin (set! a1 t1) (set! a2 t0) (set! cra L.rpLabel.9) (jump L.swap.1)))
                     (set! cfp (+ cfp 16)))
                   (set! t0 a0)))              
               (begin (set! a0 t0) (jump fv0)))))))
   (begin
     (set! fv0 cra)
     (begin
       (set! t0 1)
       (set! t1 2)
       (begin
         (begin
           (set! cfp (- cfp 16))
           (return-point L.rpLabel.10 (begin (set! a1 t0) (set! a2 t1) (set! cra L.rpLabel.10) (jump L.swap.1)))
           (set! cfp (+ cfp 16)))
         (jump fv0)))))
;stktoken-seal
(module ()
  (define L.swap.1   ;sealed cfp - sealed cra - csp - cgp - cs1 in ct6
    ()
    (begin
      (set! cs1 ct6)
      (begin
        (set! t2 a1)     ;x
        (set! t0 a2)     ;y
        (if (< t0 t2)
            (begin (set! a0 t2) (begin (set! ct6 cfp) (invoke cra ct6)))
            (begin
              (begin
                (set! t1 t0)   ;y
                (set! t0 t2)   ;x
                (begin
                  (begin
                    (set! fv0 cra)
                    (setLinear! fv1 cfp)
                    (set! fv2 cs1)
                    (begin
                      (return-point
                       L.rpLabel.9
                       (begin
                         (set! a1 t1)
                         (set! a2 t0)
                         (set! cra L.rpLabel.9)
                         (begin
                           (split csp csp cfp 16384)
                           (set! cs2 (cs1 - 0))
                           (seal cra cra cs2 28)
                           (seal cfp cfp cs2 28)
                           (begin
                             (set! ct0 gv0)
                             (set! ct6 gv1)
                             (invoke ct0 ct6)))))
                      (set! cfp ct6))
                    (splice csp csp cfp 16384)
                    (set! cs1 fv2)
                    (setLinear! cfp fv1)
                    (set! cra fv0))
                  (set! t0 a0)))
              (begin (set! a0 t0) (begin (set! ct6 cfp) (invoke cra ct6))))))))
  (begin
    (set! cs1 ct6)
    (begin
      (set! t0 1)
      (set! t1 2)
      (begin
        (begin
          (set! fv0 cra)
          (setLinear! fv1 cfp)
          (set! fv2 cs1)
          (begin
            (return-point
             L.rpLabel.10
             (begin
               (set! a1 t0)
               (set! a2 t1)
               (set! cra L.rpLabel.10)
               (begin
                 (split csp csp cfp 16384)
                 (set! cs2 (cs1 - 0))
                 (seal cra cra cs2 71)
                 (seal cfp cfp cs2 71)
                 (begin (set! ct0 gv0) (set! ct6 gv1) (invoke ct0 ct6)))))
            (set! cfp ct6))
          (splice csp csp cfp 16384)
          (set! cs1 fv2)
          (setLinear! cfp fv1)
          (set! cra fv0))
        (begin (set! ct6 cfp) (invoke cra ct6))))))
;stktokens-sentry
(module ()
  (define L.swap.1
    ()
    (begin
      (set! cs1 pcc)
      (set! t5 2147491840)
      (set-addr! cs1 t5)
      (begin
        (set! t2 a1)
        (set! t0 a2)
        (if (< t0 t2)
          (begin (set! a0 t2) (begin (set! ct6 cfp) (invoke cra ct6)))
          (begin
            (begin
              (set! t1 t0)
              (set! t0 t2)
              (begin
                (begin
                  (set! fv0 cra)
                  (setLinear! fv1 cfp)
                  (set! fv2 cs1)
                  (begin
                    (return-point
                     L.rpLabel.9
                     (begin
                       (set! a1 t1)
                       (set! a2 t0)
                       (set! cra L.rpLabel.9)
                       (begin
                         (split csp csp cfp 16384)
                         (set! cs2 (cs1 - 0))
                         (seal cra cra cs2 0)
                         (seal cfp cfp cs2 0)
                         (begin (set! ct0 gv0) (jump ct0)))))
                    (set! cfp ct6))
                  (splice csp csp cfp 16384)
                  (set! cs1 fv2)
                  (setLinear! cfp fv1)
                  (set! cra fv0))
                (set! t0 a0)))
            (begin (set! a0 t0) (begin (set! ct6 cfp) (invoke cra ct6))))))))
  (begin
    (set! cs1 pcc)
    (set! t5 2147491840)
    (set-addr! cs1 t5)
    (begin
      (set! t0 1)
      (set! t1 2)
      (begin
        (begin
          (set! fv0 cra)
          (setLinear! fv1 cfp)
          (set! fv2 cs1)
          (begin
            (return-point
             L.rpLabel.10
             (begin
               (set! a1 t0)
               (set! a2 t1)
               (set! cra L.rpLabel.10)
               (begin
                 (split csp csp cfp 16384)
                 (set! cs2 (cs1 - 0))
                 (seal cra cra cs2 90)
                 (seal cfp cfp cs2 90)
                 (begin (set! ct0 gv0) (jump ct0)))))
            (set! cfp ct6))
          (splice csp csp cfp 16384)
          (set! cs1 fv2)
          (setLinear! cfp fv1)
          (set! cra fv0))
        (begin (set! ct6 cfp) (invoke cra ct6))))))
;cheri-linkage-seal
(module ()
  (define L.swap.1
    ()
    (begin
      (set! t2 a1)
      (set! t0 a2)
      (if (< t0 t2)
          (begin (set! a0 t2) (begin (set! ct6 csp) (invoke cra ct6)))
          (begin
            (begin
              (set! t1 t0)
              (set! t0 t2)
              (begin
                (begin
                  (set! fv0 cra)
                  (set! fv1 csp)
                  (begin
                    (return-point
                     L.rpLabel.9
                     (begin
                       (set! a1 t1)
                       (set! a2 t0)
                       (set! cra L.rpLabel.9)
                       (begin (set! ct0 gv0) (jump ct0))))
                    (set! csp ct6))
                  (set! csp fv1)
                  (set! cra fv0))
                (set! t0 a0)))
            (begin (set! a0 t0) (begin (set! ct6 csp) (invoke cra ct6)))))))
  (begin
    (set! t0 1)
    (set! t1 2)
    (begin
      (begin
        (set! fv0 cra)
        (set! fv1 csp)
        (begin
          (return-point
           L.rpLabel.10
           (begin
             (set! a1 t0)
             (set! a2 t1)
             (set! cra L.rpLabel.10)
             (begin (set! ct0 gv0) (jump ct0))))
          (set! csp ct6))
        (set! csp fv1)
        (set! cra fv0))
      (begin (set! ct6 csp) (invoke cra ct6)))))
