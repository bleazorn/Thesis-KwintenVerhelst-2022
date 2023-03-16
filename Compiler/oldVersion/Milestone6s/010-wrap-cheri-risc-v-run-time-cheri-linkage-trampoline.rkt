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

(define (createSeals)
  "")

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
                                                      (indent-instr (format "li t6, 0xf00000"))
                                                      (indent-instr (format "CAndPerm cfp, cfp, t5"))
                                                      (indent-instr (format "CSetBounds cfp, cfp, t6"))
                                                      (cond [(equal? (stack-direction) '-) (string-append (indent-instr (format "addi t6, t6, -~a" (framesize)))
                                                                                                          (indent-instr (format "CSetOffset cfp, cfp, t6")))]
                                                            [else ""])
                                                      "//PCC init\n"
                                                      (indent-instr (format "cllc ct0, L.pcJump"))
                                                      (indent-instr (format "li t5, 259"))
                                                      (indent-instr (format "CAndPerm ct0, ct0, t5"))
                                                      (indent-instr (format "cjr ct0"))
                                                      "L.pcJump:\n"
                                                      "//GOT Setup\n"
                                                      (indent-instr (format "li t4, 0x82000000"))
                                                      (indent-instr (format "li t4, 381"))
                                                      (indent-instr (format "li t6, 0x100"))
                                                      (indent-instr (format "CSetAddr cgp, cgp, t4"))
                                                      (indent-instr (format "CAndPerm cgp, cgp, t5"))
                                                      (indent-instr (format "CSetBounds cgp, cgp, t6"))
                                                      
                                                      (initialize-got (getInfo i getGOTLabels))
                                                      s

                                                      (indent-instr (format ".section .data"))
                                                      (intitialize-framesizes (getInfo i getFrameSize))

                                                      (indent-instr (format ".section .text.plt"))
                                                      "plt:\n"
                                                      (indent-instr (format "CSpecialr cs2, pcc"))		
                                                      (indent-instr (format "CMove csp, cfp"))			
                                                      (indent-instr (format "CMove cfp, cs2"))				
                                                      "//Init new cfp\n"
                                                      (indent-instr (format "CGetBase t0, csp"))
                                                      (indent-instr (format "li t5, 381"))
                                                      (indent-instr (format "sub t0, t0, t6"))
                                                      (indent-instr (format "CSetAddr cfp, cfp, t0"))
                                                      (indent-instr (format "CAndPerm cfp, cfp, t5"))
                                                      (indent-instr (format "CSetBounds cfp, cfp, t6"))
                                                      "//Seal\n"
                                                      (indent-instr (format "li t0, 134057"))
                                                      (indent-instr (format "CSetAddr cs2, cs2, t0"))
                                                      (indent-instr (format "CSeal cra, cra, cs2"))
                                                      (indent-instr (format "CSeal csp, csp, cs2"))
                                                      "//Jump to call\n"
                                                      (indent-instr (format "Cjr cs10"))

                                                      (create-plt (getInfo i getGOTLabels))
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
         (format "~a.size: .int ~a" name size)
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
     (format "~a.plt:" name)
     "//PC Setup\n"
     (indent-instr (format "Cllc cs10, ~a" name))
     (indent-instr (format "li t5, 259"))
     (indent-instr (format "CAndPerm cs10, cs10, t5"))
     "//Name frame size\n"
     (indent-instr (format "li t6, ~a.size" name))
     "//Jump to generalize plt\n"
     (indent-instr (format "Cllc cs11, plt"))
     (indent-instr (format "Cjr cs11")))))

(module+ test
  (check-equal? #t #t "first test"))
