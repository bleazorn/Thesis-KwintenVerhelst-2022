#lang racket

(require racket/cmdline)
(require "config.rkt"
         "compile.rkt")

;; TODO: dirty, should exclude main from testing
(when (not (vector-empty? (current-command-line-arguments)))
  (let ([file-to-compile
         (command-line
          #:program "compile"
          #:usage-help """
Nanopass compiler targeting CHERI-RISC-V.
Default calling convention is vanilla riscv.
"""
          #:once-each
          [("-v" "--verbose") "Compile with verbose messages"
                              (verbose #t)]
          [("-o" "--output-file") out
                                  "Output file for assembly code"
                                  (output-file out)]
          #:once-any
          [("-s" "--stktokens") "Compile with the StkStokens Calling Convention"
                                (cc 'stktokens)]
          #:args (filename)
          filename)])
    (compile-file file-to-compile)))

(module+ test
  (require rackunit)
  (check-equal? #t #t "test"))
