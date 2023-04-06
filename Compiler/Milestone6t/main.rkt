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
          [("-p" "--pass") p
                           "Compile up to the given pass"
                           (pass (string->symbol p))]
          #:once-any
          [("-s" "--stktokens") "Compile with the StkStokens Calling Convention"
                                (cc 'stktokens)]
          [("-e" "--stktokenssentry") "Compile with the StkStokens Calling Convention"
                                (cc 'stkTokens-sentry)]
          [("-c" "--cherilinkage") "Compile with the cheri-linkage Calling Convention"
                                (cc 'cheri-linkage-seal)]
          [("-t" "--cherilinkage") "Compile with the cheri-linkage Calling Convention"
                                (cc 'cheri-linkage-trampoline)]
          [("-r" "--risc-v") "Compile with normal risc-v"
                                (cc 'risc-v)]
          #:args (filename)
          filename)])
    (compile-file file-to-compile)))

(module+ test
  (require rackunit)
  (check-equal? #t #t "test"))
