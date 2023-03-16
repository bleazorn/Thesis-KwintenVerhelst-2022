#lang racket

(require "common/assembly.rkt"
         "common/fvar.rkt"
         "common/info.rkt"
         "setup.rkt"
         "log.rkt"
         "020-generate-cheri-risc-v.rkt")

(provide wrap-cheri-risc-v-run-time-stkTokens)

(module+ test
  (require rackunit))

(define (createSeals)
  "")

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-cheri-risc-v-run-time-stkTokens p)
  (pretty-log p)
  (match p
    [`(begin ,i ,s) #:when (string? s) (string-append "main:\n"
                                                      (indent-instr (format "auipcc csp, %pcrel_hi(0x~a)" 81000000))
                                                      (indent-instr (format "cmove cs2, csp"))
                                                      (indent-instr (format "cmove cgp, csp"))
                                                      (indent-instr (format "cmove cs11, csp"))
                                                      "//csp init\n"
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "li t6, 0xf00000"))
                                                      (indent-instr (format "CAndPerm csp, csp, t5"))
                                                      (indent-instr (format "CSetBounds csp, csp, t6"))
                                                      (indent-instr (format "CMakelinear csp, csp"))
                                                      "//PCC init\n"
                                                      (indent-instr (format "cllc ct0, L.pcJump"))
                                                      (indent-instr (format "li t5, 259"))
                                                      (indent-instr (format "CAndPerm ct0, ct0, t5"))
                                                      (indent-instr (format "cjr ct0"))
                                                      "L.pcJump:\n"
                                                      "//Call Setup\n"
                                                      (indent-instr (format "li t6, 16384"))
                                                      (indent-instr (format "CSplitCap cfp, csp, t6"))
                                                      "//Seal Setup\n"
                                                      (indent-instr (format "li t5, 641"))
                                                      (indent-instr (format "CAndPerm cs2, cs2, t5"))
                                                      "//Seal Split\n"
                                                      (indent-instr (format "li t4, 132056"))
                                                      (indent-instr (format "CSetAddr cs2, cs2, t4"))
                                                      (indent-instr (format "CSeal cra, cra, cs2"))
                                                      (indent-instr (format "CSeal cfp, cfp, cs2"))

                                                      "//Seal creations\n"
                                                      (seal-setup 134056 100 80002000)

                                                      "//GOT Setup\n"
                                                      (indent-instr (format "li t4, 0x82000000"))
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "li t6, 0x100"))
                                                      (indent-instr (format "CSetAddr cgp, cgp, t4"))
                                                      (indent-instr (format "CAndPerm cgp, cgp, t5"))
                                                      (indent-instr (format "CSetBounds cgp, cgp, t6"))

                                                      "//GOT Pointer creations\n"
                                                      (initialize-got (getInfo i getGOTLabels))
                                                      
                                                      "//Clean Registers\n"
                                                      ;TODO: Hacky change ct6
                                                      (indent-instr (format "cmove ct6, cs11"))
                                                      (indent-instr (format "li t4, 0x80002000"))
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "li t0, 0x100"))
                                                      (indent-instr (format "CSetAddr ct6, ct6, t4"))
                                                      (indent-instr (format "CAndPerm ct6, ct6, t5"))
                                                      (indent-instr (format "CSetBounds ct6, ct6, t0"))
                                                      
                                                      (indent-instr (format "addi t0, x0, 0"))
                                                      (indent-instr (format "addi t1, x0, 0"))
                                                      (indent-instr (format "addi t4, x0, 0"))
                                                      (indent-instr (format "addi t5, x0, 0"))
                                                      ;(indent-instr (format "addi t6, x0, 0"))
                                                      (indent-instr (format "addi s1, x0, 0"))
                                                      (indent-instr (format "addi s2, x0, 0"))
                                                      (indent-instr (format "addi s11, x0, 0"))
                                                      
                                                      
                                                      s)]
    [_ "Run-time wrap failed"]))

(define (seal-setup start end loc)
  (string-append
   (indent-instr (format "CMove cs1, cs2"))
   (indent-instr (format "li t4, ~a" start))
   (indent-instr (format "li t6, ~a" end))
   (indent-instr (format "CSetAddr cs1, cs1, t4"))
   (indent-instr (format "csetbounds cs1, cs1, t6"))
   (indent-instr (format "li t4, 0x~a" loc))
   (indent-instr (format "CSetAddr cs11, cs11, t4"))
   (indent-instr (format "sc.cap cs1, 0(cs11)"))))


(define (initialize-got got-labels)
  (if (null? got-labels)
      ""
      (let* ([got-label (car got-labels)]
             [name (car got-label)]
             [loc (second got-label)])
        (string-append
         (format "//got ~a \n" name)
         (initialize-got-label name loc)
         (initialize-got (cdr got-labels))))))
      
  

(define (initialize-got-label name loc)
  (string-append 
   (indent-instr (format "cmove ct0, cs11"))
   (indent-instr (format "cmove ct1, cs11"))
   "//pc\n"
   (indent-instr (format "li t4, 0x80002100"))
   (indent-instr (format "li t5, 259"))
   (indent-instr (format "li t6, 0x7f00"))
   (indent-instr (format "CSetAddr ct0, ct0, t4"))
   (indent-instr (format "CAndPerm ct0, ct0, t5"))
   (indent-instr (format "CSetBounds ct0, ct0, t6"))
   (indent-instr (format "cllc ct4, ~a" name))
   (indent-instr (format "CSetAddr ct0, ct0, t4"))
   "//sp\n"
   (indent-instr (format "li t4, 0x80002000"))
   (indent-instr (format "li t5, 381"))
   (indent-instr (format "li t6, 0x100"))
   (indent-instr (format "CSetAddr ct1, ct1, t4"))
   (indent-instr (format "CAndPerm ct1, ct1, t5"))
   (indent-instr (format "CSetBounds ct1, ct1, t6"))
   "//seal\n"
   (indent-instr (format "li t5, 132059"))
   (indent-instr (format "CSetAddr cs2, cs2, t5"))
   (indent-instr (format "CSeal ct0, ct0, cs2"))
   (indent-instr (format "CSeal ct1, ct1, cs2"))
   "//store\n"
   (indent-instr (format "cincoffsetimm cgp, cgp, ~a" (* (* 2 loc) (framesize))))
   (indent-instr (format "sc.cap ct0, 0(cgp)"))
   (indent-instr (format "cincoffsetimm cgp, cgp, ~a" (framesize)))
   (indent-instr (format "sc.cap ct1, 0(cgp)"))
   (indent-instr (format "cincoffsetimm cgp, cgp, -~a" (* (add1 (* 2 loc)) (framesize))))))

(module+ test
  (check-equal? #t #t "first test"))
