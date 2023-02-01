#lang racket

(require "common/assembly.rkt"
         "common/fvar.rkt"
         "common/info.rkt"
         "setup.rkt"
         "log.rkt"
         "020-generate-cheri-risc-v.rkt")

(provide wrap-cheri-risc-v-run-time-secure)

(module+ test
  (require rackunit))

(define (createSeals)
  "")

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-cheri-risc-v-run-time-secure p)
  (pretty-log p)
  (match p
    [`(begin ,i ,s) #:when (string? s) (string-append "main:\n"
                                                      (indent-instr (format "auipcc csp, %pcrel_hi(0x~a)" 81000000))
                                                      (indent-instr (format "cmove cs2, csp"))
                                                      (indent-instr (format "cmove cgp, csp"))
                                                      "//csp init\n"
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "CAndPerm csp, csp, t5"))
                                                      (indent-instr (format "li t0, 15859712"))
                                                      (indent-instr (format "CSetBounds csp, csp, t0"))
                                                      (indent-instr (format "CMakelinear csp, csp"))
                                                      "//PCC init\n"
                                                      (indent-instr (format "cllc ct0, L.pcJump"))
                                                      (indent-instr (format "li t5, 259"))
                                                      (indent-instr (format "CAndPerm ct0, ct0, t5"))
                                                      (indent-instr (format " cjalr ct0, ct0"))
                                                      "L.pcJump:\n"
                                                      "//Call Setup\n"
                                                      (indent-instr (format "li t0, 16384"))
                                                      (indent-instr (format "CSplitCap cfp, csp, t0"))
                                                      "//Seal Setup\n"
                                                      (indent-instr (format "li t5, 641"))
                                                      (indent-instr (format "CAndPerm cs2, cs2, t5"))

                                                      (seal-setup 134056 100 0)
                                                          
                                                      (indent-instr (format "li t5, 132056"))
                                                      (indent-instr (format "csetoffset cs2, cs2, t5"))
                                                      
                                                      (indent-instr (format "CSeal cra, cra, cs2"))
                                                      (indent-instr (format "CSeal cfp, cfp, cs2"))

                                                      "//GOT Setup\n"
                                                      (indent-instr (format "li t0, 0x82000000"))
                                                      (indent-instr (format "csetoffset cgp, cgp, t0"))
                                                      (initialize-got (getInfo i getGOTLabels))
                                                      (indent-instr (format "li t4, 381"))
                                                      (indent-instr (format "li t6, 0x100"))
                                                      (indent-instr (format "CAndPerm cgp, cgp, t4"))
                                                      (indent-instr (format "csetbounds cgp, cgp, t6"))
                                                      s)]
    [_ "Run-time wrap failed"]))

(define (seal-setup start end loc)
  (string-append
   (indent-instr (format "cmove cs1, cs2"))
   (indent-instr (format "li t5, ~a" start))
   (indent-instr (format "li t6, ~a" end))
   (indent-instr (format "csetoffset cs1, cs1, t5"))
   (indent-instr (format "csetbounds cs1, cs1, t6"))
   (indent-instr (format "li t5, 0x80002000"))
   (indent-instr (format "csetaddr cgp, cgp, t5"))
   (indent-instr (format "cincoffsetimm cgp, cgp, ~a" (* loc (framesize))))
   (indent-instr (format "sc.cap cs1, 0(cgp)"))
   (indent-instr (format "cincoffsetimm cgp, cgp, -~a" (* loc (framesize))))))


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
   (indent-instr (format "cmove ct0, cgp"))
   (indent-instr (format "cmove ct1, cgp"))
   "//pc\n"
   (indent-instr (format "li t4, 383"))
   (indent-instr (format "li t5, 0x80002000"))
   (indent-instr (format "li t6, 0x1000"))
   (indent-instr (format "csetoffset ct0, ct0, t5"))
   (indent-instr (format "CAndPerm ct0, ct0, t4"))
   (indent-instr (format "CSetBounds ct0, ct0, t6"))
   (indent-instr (format "cllc ct5, ~a" name))
   (indent-instr (format "csetaddr ct0, ct0, t5"))
   "//seal\n"
   (indent-instr (format "li t5, 132059"))
   (indent-instr (format "csetoffset cs2, cs2, t5"))
   (indent-instr (format "cseal ct0, ct0, cs2"))
   "//store\n"
   (indent-instr (format "cincoffsetimm cgp, cgp, ~a" (* loc (framesize))))
   (indent-instr (format "sc.cap ct0, 0(cgp)"))
   (indent-instr (format "cincoffsetimm cgp, cgp, -~a" (* loc (framesize))))))

(module+ test
  (check-equal? #t #t "first test"))
