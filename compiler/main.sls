;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

(library (loko compiler main)
  (export
    compiler-passes assemble-text-file
    (rename (record->sexpr compiler-code->sexpr)))
  (import
    (loko arch asm)
    (loko config)
    (loko compiler recordize)
    (loko compiler let)
    (loko compiler letrec)
    (loko compiler cp0)
    (loko compiler values)
    (loko compiler quasiquote)
    (loko compiler mutation)
    (loko compiler freevar)
    (loko compiler loops)
    (loko compiler infer)
    (loko compiler closure)
    (loko compiler global)
    (loko compiler optimize)
    (only (psyntax compat) pretty-print)
    (rnrs))

(define (assemble-text-file filename codes primlocs locs-ht locs-rev verbose codegen-options)
  (define (print . x)
    (when verbose (for-each display x) (newline)))
  (define (generate-assembler codes)
    ;; Returns text and data.
    (print "Generating code...")
    #;
    (for-each (lambda (x)
                (pretty-print (labels-top-level-name x))
                (unless (equal? (labels-top-level-name x) '(loko runtime unicode))
                  (pretty-print (record->sexpr x)))
                (newline))
              codes)
    (let-values ([(text data) (code-generator (config-target-cpu) codes primlocs
                                              'make-init-code codegen-options)])
      (print "Optimizing code...")
      (cond ((instruction-analyzer (config-target-cpu))
             => (lambda (a) (pass-optimize text data a
                                           (target-convention (config-target-cpu)))))
            (else
             ;; No instruction analyzer? No optimizations for you.
             (values text data)))))
  (let* ((codes (pass-prune-globals codes locs-ht locs-rev))
         (codes (pass-direct-calls codes locs-ht)))
    (let*-values ([(text data) (generate-assembler codes)]
                  [(text data) (generate-tables (config-target-cpu) (config-target-kernel)
                                                text data)])
      (print "Assembling code...")
      ;; (for-each (lambda (x) (write x) (newline)) text) (newline)
      (call-with-port (open-file-output-port filename (file-options no-fail))
        (lambda (p)
          (let ((asm (assembler-library (config-target-cpu)
                                        (config-target-kernel)
                                        text data)))
            ;; (for-each (lambda (x) (write x) (newline)) asm)
            (let-values ([(binary symbols)
                          (assemble (config-target-cpu) asm)])
              (display filename)
              (newline)
              (put-bytevector p binary)
              #;
              (let-values (((syms addrs) (hashtable-entries symbols)))
                (display "Symbol table:\n")
                (vector-for-each
                 (lambda (label addr)
                   (print (number->string addr 16) " - " label))
                 syms addrs)
                (newline))))))
      (when verbose
        (display "Done.\n")))))

;; The input to compiler-passes comes from psyntax. The output
;; eventually goes to a code generator (and normally gets there via
;; assemble-text-file).
(define (compiler-passes name code)
  (define (progress step)
    ;; (display (list step) (current-error-port))
    #f)
  (let* ((c (pass-recordize name code))
         (_ (progress 'let))
         (c (pass-let c))
         (_ (progress 'letrec-prepass))
         (c (pass-letrec-prepass c))
         (_ (progress 'letrec))
         (c (pass-letrec c))
         (_ (progress 'cp0))
         (c (pass-cp0 c))
         (_ (progress 'mutation))
         (c (pass-mutation c))
         (_ (progress 'quasiquote))
         (c (pass-quasiquote c))
         (_ (progress 'values))
         (c (pass-values c))
         (_ (progress 'let))
         (c (pass-let c))
         (_ (progress 'freevar))
         (c (pass-freevar c))
         (_ (progress 'loops))
         (c (pass-loops c))
         (_ (progress 'infer))
         (c (pass-infer c))
         (_ (progress 'closure))
         (c (pass-closure name c)))
    c)))
