#lang racket

(require "common/assembly.rkt"
         "common/fvar.rkt"
         "common/info.rkt"
         "setup.rkt"
         "log.rkt"
         "020-generate-cheri-risc-v.rkt")

(provide wrap-cheri-risc-v-run-time-cheri-linkage-trampoline)

(module+ test
  (require rackunit))

(define trampoline-size 7)

;wraps the run-time for a cheri-risc-v around given code
;(wrap-cheri-risc-v-run-time p) -> string?
;p: string?
(define (wrap-cheri-risc-v-run-time-cheri-linkage-trampoline p)
  (pretty-log p)
  (match p
    [`(begin ,i ,s) #:when (string? s) (string-append "main:\n"
                                                      (indent-instr (format "auipcc cfp, %pcrel_hi(0x~a)" 81000000))
                                                      (indent-instr (format "cmove cs2, cfp"))
                                                      (indent-instr (format "cmove cgp, cfp"))
                                                      (indent-instr (format "cmove cs11, cfp"))
                                                      "//cfp init\n"
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "cllc ct0, main.size"))
                                                      (indent-instr (format "lw.cap t6, 0(ct0)"))
                                                      (indent-instr (format "CAndPerm cfp, cfp, t5"))
                                                      (indent-instr (format "CSetBounds cfp, cfp, t6"))
                                                      (cond [(equal? (stack-direction) '-) (indent-instr (format "CSetOffset cfp, cfp, t6"))]
                                                            [else ""])
                                                      "//PCC init\n"
                                                      (indent-instr (format "cllc ct0, L.pcJump"))
                                                      (indent-instr (format "li t5, 259"))
                                                      (indent-instr (format "CAndPerm ct0, ct0, t5"))
                                                      (indent-instr (format "cjr ct0"))
                                                      "L.pcJump:\n"
                                                      "//GOT Setup\n"
                                                      (indent-instr (format "li t4, 0x82000000"))
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "li t6, 0x100"))
                                                      (indent-instr (format "CSetAddr cgp, cgp, t4"))
                                                      (indent-instr (format "CAndPerm cgp, cgp, t5"))
                                                      (indent-instr (format "CSetBounds cgp, cgp, t6"))
                                                      
                                                      (initialize-got (getInfo i getGOTLabels))

                                                      "//Clean Registers\n"
                                                      (indent-instr (format "addi t0, x0, 0"))
                                                      (indent-instr (format "addi t1, x0, 0"))
                                                      (indent-instr (format "addi t4, x0, 0"))
                                                      (indent-instr (format "addi t5, x0, 0"))
                                                      (indent-instr (format "addi t6, x0, 0"))
                                                      (indent-instr (format "addi s1, x0, 0"))
                                                      (indent-instr (format "addi s2, x0, 0"))
                                                      (indent-instr (format "addi s11, x0, 0"))
                                                      
                                                      
                                                      s

                                                      (indent-instr (format ".section .data\n"))
                                                      (intitialize-framesizes (getInfo i getFrameSize))
                                                      (indent-instr (format "tram.loc: .int 0\n"))
                                                      
                                                      (indent-instr (format ".section .text.plt\n"))
                                                      "plt:\n"
                                                      "//Gets cfp, cra from call\n"
                                                      "//Gets cs10, t6 from call plt\n"
                                                      (indent-instr (format "CSpecialr cs2, pcc"))		
                                                      (indent-instr (format "CMove csp, cfp"))			
                                                      (indent-instr (format "CMove cfp, cs2"))
                                                      (indent-instr (format "CMove cs1, cs2"))
                                                      "//Init new cfp\n"
                                                      (indent-instr (format "CGetBase t0, csp"))
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "sub t0, t0, t6"))
                                                      (indent-instr (format "CSetAddr cfp, cfp, t0"))
                                                      (indent-instr (format "CAndPerm cfp, cfp, t5"))
                                                      (indent-instr (format "CSetBounds cfp, cfp, t6"))
                                                      (cond [(equal? (stack-direction) '-) (indent-instr (format "CSetOffset cfp, cfp, t6"))]
                                                            [else ""])
                                                      
                                                      "tramp.set:\n"
                                                      "//Install trampoline\n"
                                                      (indent-instr (format "li t4, 0x82001000"))
                                                      (indent-instr (format "CSetAddr cs1, cs1, t4"))
                                                      (indent-instr (format "cllc ct2, tram.loc"))
                                                      (indent-instr (format "lw.cap t3, 0(ct2)"))
                                                      (indent-instr (format "cincoffset cs1, cs1, t3"))
                                                      "//trampoline size\n"
                                                      (indent-instr (format "cincoffset ct5, cs1, 0x~a0" (+ 2 trampoline-size)))
                                                      "//Store csp and cra\n"
                                                      (indent-instr (format "sc.cap cra, 0(cs1)"))
                                                      (indent-instr (format "cincoffset cs1, cs1, 0x10"))
                                                      (indent-instr (format "sc.cap csp, 0(cs1)"))
                                                      (indent-instr (format "cincoffset cs1, cs1, 0x10"))
                                                      "//Store trampoline\n"
                                                      (indent-instr (format "cllc ct6, tramp"))
                                                      "tramp.begin:\n"
                                                      (indent-instr (format "bge s1, t5, tramp.end"))
                                                      (indent-instr (format "lc.cap ct1, 0(ct6)"))
                                                      (indent-instr (format "sc.cap ct1, 0(cs1)"))
                                                      (indent-instr (format "cincoffset ct6, ct6, 0x10"))
                                                      (indent-instr (format "cincoffset cs1, cs1, 0x10"))
                                                      (indent-instr (format "cllc ct0, tramp.begin"))
                                                      (indent-instr (format "cjr ct0"))
                                                      "tramp.end:\n"
                                                      "//Store trampoline location\n"
                                                      (indent-instr (format "addi t3, t3, 0x~a0" (+ 2 trampoline-size)))
                                                      (indent-instr (format "sw.cap t3, 0(ct2)"))
                                                      "//Create cra\n"
                                                      (indent-instr (format "cincoffset cs1, cs1, -0x~a0" trampoline-size))
                                                      (indent-instr (format "csealentry cra, cs1"))
                                                      "//Clear registers\n"
                                                      (indent-instr (format "addi t0, x0, 0"))
                                                      (indent-instr (format "addi t1, x0, 0"))
                                                      (indent-instr (format "addi t4, x0, 0"))
                                                      (indent-instr (format "addi t5, x0, 0"))
                                                      (indent-instr (format "addi t6, x0, 0"))
                                                      (indent-instr (format "addi s1, x0, 0"))
                                                      (indent-instr (format "addi s2, x0, 0"))
                                                      (indent-instr (format "addi sp, x0, 0"))
                                                      "//Jump to call\n"
                                                      (indent-instr (format "Cjr cs10"))

                                                      (create-plt (getInfo i getGOTLabels))

                                                      (indent-instr (format ".p2align 6"))
                                                      "//Trampoline code\n"
                                                      "tramp:\n"
                                                      (indent-instr (format "cspecialr csp, pcc"))
                                                      (indent-instr (format "cincoffset csp, csp, -0x20"))
                                                      (indent-instr (format "lc.cap cra, 0(csp)"))
                                                      (indent-instr (format "cincoffset csp, csp, 0x10"))
                                                      (indent-instr (format "lc.cap cfp, 0(csp)"))
                                                      (indent-instr (format "addi sp, x0, 0"))
                                                      (indent-instr (format "cjr cra"))
                                                      )]
    [_ "Run-time wrap failed"]))


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
   (indent-instr (format "CMove ct0, cs11"))
   "//pc\n"
   (indent-instr (format "cllc ct4, ~a.plt" name))
   (indent-instr (format "CSetAddr ct0, ct0, t4"))
   "//Seal\n"
   (indent-instr (format "CSealEntry ct0, ct0"))
   "//store\n"
   (indent-instr (format "cincoffsetimm cgp, cgp, ~a" (* loc (framesize))))
   (indent-instr (format "sc.cap ct0, 0(cgp)"))
   (indent-instr (format "cincoffsetimm cgp, cgp, -~a" (* loc (framesize))))))

(define (intitialize-framesizes framesizes)
  (if (null? framesizes)
      ""
      (let ([name (car (car framesizes))]
            [size (second (car framesizes))])
        (string-append
         (indent-instr (format "~a.size: .int ~a\n" name (* size (framesize))))
         (intitialize-framesizes (cdr framesizes))))))


(define (create-plt labels)
  (if (null? labels)
      ""
      (string-append
       (create-plt-label (car labels))
       (create-plt (cdr labels)))))

(define (create-plt-label label)
  (let ([name (car label)])
    (string-append
     (format "~a.plt:\n" name)
     "//PC Setup\n"
     (indent-instr (format "Cllc cs10, ~a" name))
     (indent-instr (format "li t5, 259"))
     (indent-instr (format "CAndPerm cs10, cs10, t5"))
     "//Name frame size\n"
     (indent-instr (format "cllc cs11, ~a.size" name))
     (indent-instr (format "lw.cap t6, 0(cs11)"))
     "//Jump to generalize plt\n"
     (indent-instr (format "Cllc cs11, plt"))
     (indent-instr (format "Cjr cs11")))))

(module+ test
  (check-equal? #t #t "first test"))
