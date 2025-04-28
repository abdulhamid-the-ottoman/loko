;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Compilation with static linking

(library (loko compiler static)
  (export
    compile-program)
  (import
    (rnrs (6))
    (srfi :98 os-environment-variables)
    (loko runtime utils)
    (psyntax library-manager)
    (only (psyntax compat) parameterize)
    (loko compiler expander)
    (loko compiler main)
    (loko config)
    (only (loko compiler cp0) cp0-effort-limit))

(define (get-scheme-libraries target-cpu target-kernel options)
  (define kernels
    (case target-kernel
      [(pc) '(pc)]
      [(pc+linux) '(pc linux)]
      [(pc+netbsd) '(pc netbsd)]
      [(polyglot) '(pc linux netbsd)]
      [(linux) '(linux)]
      [(netbsd) '(netbsd)]
      [else '()]))
  (if (memq 'freestanding options)
      '()
      `("loko/runtime/parameters"
        "loko/config"
        "loko/runtime/booleans"
        "loko/runtime/fixnums"
        "loko/runtime/flonums"
        "loko/runtime/chars"
        "loko/runtime/pairs"
        "loko/runtime/bytevectors"
        "loko/runtime/arithmetic"
        "loko/runtime/strings"
        "loko/runtime/unicode"
        "loko/runtime/vectors"
        "loko/runtime/utils"
        "loko/runtime/equal"     ;eqv? -> memv -> case
        "loko/runtime/context"
        "loko/runtime/records"
        "loko/runtime/conditions"
        "laesare/reader"
        "loko/runtime/control"
        "loko/runtime/hashtables"
        "loko/runtime/symbols"
        "loko/runtime/enums"
        "loko/match"
        "loko/runtime/init"
        "loko/runtime/time"
        "pfds/heaps"
        "loko/queues"
        "loko/runtime/logging"
        "loko/runtime/fibers"
        "loko/runtime/io-printer"
        "loko/runtime/io-tc"
        "loko/runtime/io"
        "loko/runtime/pretty"
        "loko/runtime/reader"
        "loko/runtime/sorting"
        ,@(if (memq 'eval options)
              '("psyntax/compat"
                "psyntax/internal"
                "psyntax/config"
                "psyntax/library-manager"
                "psyntax/builders"
                "psyntax/expander"  ;assert starts working: assertion-error
                "loko/compiler/utils"
                "loko/compiler/recordize"
                "loko/compiler/let"
                "loko/compiler/letrec"
                "loko/compiler/cp0"
                "loko/compiler/quasiquote"
                "loko/compiler/mutation"
                "loko/compiler/values"
                "loko/compiler/freevar"
                "loko/compiler/loops"
                "loko/compiler/infer"
                "loko/compiler/closure"
                "loko/compiler/global"
                "loko/compiler/optimize"
                "loko/compiler/expander"
                "loko/runtime/repl"
                "loko/arch/asm")
              '())
        "struct/pack"
        ,@(if (memq 'main options)
              '("loko/runtime/main")
              '())
        "loko/runtime/mmap"
        "loko/runtime/unsafe"
        "loko/runtime/buddy"
        "loko/runtime/scheduler"
        ;; Target-specific stuff. Only code in (loko arch ...) should
        ;; import these.
        ,@(if (or (memq 'linux kernels) (memq 'netbsd kernels))
              '("loko/runtime/elf"
                "srfi/%3a198/private")
              '())
        ,@(case target-cpu
            ((amd64)
             `("machine-code/disassembler/x86-opcodes"
               ;; FIXME: use instead of (loko arch amd64 disassembler)
               ;; "machine-code/disassembler/private"
               ;; "machine-code/disassembler/x86"
               "loko/arch/amd64/disassembler"
               "loko/arch/amd64/config"
               "loko/arch/amd64/memory"
               "loko/arch/amd64/traps"
               "loko/arch/amd64/tagging"
               ,@(if (memq 'eval options)
                     '("loko/arch/amd64/objects"
                       "loko/arch/amd64/codegen"
                       "loko/arch/amd64/analyzer")
                     '())
               "loko/arch/amd64/processes"
               "loko/arch/amd64/playground"
               "loko/arch/amd64/prototyping"
               "loko/arch/amd64/registers"
               ,@(if (or (memq 'pc kernels)
                         (memq 'eval options))
                     '("machine-code/assembler/x86-misc"
                       "machine-code/assembler/x86-operands"
                       ;;FIXME: runs some code on startup
                       "machine-code/assembler/x86")
                     '())
               "machine-code/format/elf"
               ,@(if (memq 'eval options)
                     '("machine-code/assembler/elf"
                       "loko/arch/amd64/tables"
                       "loko/arch/amd64/lib-gc"
                       "loko/arch/amd64/lib-printer"
                       "loko/arch/amd64/lib-stacks"
                       "loko/arch/amd64/lib-traps"
                       "loko/arch/amd64/lib-valgrind"
                       "loko/arch/amd64/lib")
                     '())
               ,@(if (memq 'pc kernels)
                     `("loko/arch/amd64/pc-ustate"
                       "loko/arch/amd64/pc-segments"
                       ,@(if (memq 'eval options)
                             '("loko/arch/amd64/pc-interrupts"
                               "loko/arch/amd64/pc-paging"
                               "loko/arch/amd64/pc-syscalls"
                               "loko/arch/amd64/pc-start")
                             '())
                       "loko/arch/amd64/pc-usermode"
                       "loko/arch/amd64/pc-ap-boot"
                       "loko/drivers/pci"
                       "loko/drivers/acpi/eisa-id"
                       "loko/drivers/acpi/resources"
                       "loko/drivers/acpi/tables"
                       "loko/drivers/acpi/aml"
                       "loko/drivers/acpi/platform"
                       "loko/drivers/acpi/interrupts"
                       "loko/arch/amd64/pc-acpi"
                       "loko/dlists"
                       "loko/drivers/early/debugcon"
                       "loko/drivers/early/ns8250"
                       "loko/drivers/early/vga"
                       "loko/arch/amd64/pc-init")
                     '())
               ,@(if (and (memq 'eval options)
                          (or (memq 'linux kernels) (memq 'netbsd kernels)))
                     '("loko/arch/amd64/elf-start")
                     '())
               "loko/arch/amd64/common-init"
               ,@(if (memq 'linux kernels)
                     `("loko/arch/amd64/linux-numbers"
                       "loko/arch/amd64/linux-syscalls"
                       ,@(if (memq 'eval options)
                             '("loko/arch/amd64/linux-start")
                             '())
                       "loko/arch/amd64/linux-init")
                     '())
               ,@(if (memq 'netbsd kernels)
                     `("loko/arch/amd64/netbsd-numbers"
                       "loko/arch/amd64/netbsd-syscalls"
                       ,@(if (memq 'eval options)
                             '("loko/arch/amd64/netbsd-start")
                             '())
                       "loko/arch/amd64/netbsd-init")
                     '())
               ,@(if (memq 'eval options)
                     ;; These native and cross compiler targets will
                     ;; be supported. They should also be in
                     ;; compile-loko.sps if you want to build Loko.
                     `(,@(if (memq 'pc kernels) '("loko/arch/amd64/pc-asm") '())
                       ,@(if (memq 'linux kernels) '("loko/arch/amd64/linux-asm") '())
                       ,@(if (memq 'netbsd kernels) '("loko/arch/amd64/netbsd-asm") '())
                       ,@(if (eq? target-kernel 'pc+linux) '("loko/arch/amd64/pc-and-linux-asm") '())
                       ,@(if (eq? target-kernel 'polyglot) '("loko/arch/amd64/polyglot-asm") '()))
                     '())
               ,@(if (memq 'pc kernels)
                     '("loko/u8rings"
                       "loko/drivers/uart/ns8250"
                       "loko/arch/amd64/pc-process")
                     '())
               ,@(if (memq 'linux kernels)
                     '("loko/arch/amd64/linux-process")
                     '())
               ,@(if (memq 'netbsd kernels)
                     '("loko/arch/amd64/netbsd-process")
                     '())))
            (else '()))
        ,@(if (memq 'eval options)
              '("loko/runtime/eval"
                "loko/compiler/main"
                "loko/compiler/static")
              '())
        "loko/runtime/random"
        "loko/runtime/start"
        ,@(if (memq 'eval options)
              '("loko/runtime/start-libman")
              '()))))

;; Get filenames from a subset of the libraries above here.
(define (get-scheme-library-files library-dir target-cpu target-kernel options)
  (map (lambda (fn)
         (let ((base (string-append library-dir "/" fn)))
           (if (file-exists? (string-append base ".loko.sls"))
               (string-append base ".loko.sls")
               (string-append base ".sls"))))
       (get-scheme-libraries target-cpu target-kernel options)))

(define (compile-program out-fn sps-fn options)
  (define loko-source-path (or (get-environment-variable "LOKO_SOURCE")
                               (config-source-path)))
  (define library-files
    (get-scheme-library-files loko-source-path
                              (config-target-cpu)
                              (config-target-kernel)
                              options))
  (define verbose (memq 'verbose options))
  (parameterize ([include-path (list loko-source-path)])
    (let-values ([(name* core* locs)
                  (expand-files library-files sps-fn
                                (and (memq 'use-primlocs options)
                                     (if (memq 'bootstrap options)
                                         #t
                                         'hashtable))
                                (memq 'freestanding options)
                                verbose)])
      (let ((locs-ht (make-eq-hashtable))
            (locs-rev (make-eq-hashtable))
            (use-primlocs (memq 'use-primlocs options)))
        (for-each (lambda (primloc)
                    (hashtable-set! locs-ht (car primloc) (cdr primloc))
                    ;; If primlocs is not included in the program then
                    ;; there is no way to get access to it at runtime,
                    ;; so bindings that only appear there don't need
                    ;; to be included in the program.
                    (when use-primlocs
                      (hashtable-set! locs-rev (cdr primloc) (car primloc))))
                  locs)
        (let* ((primlocs (lambda (x)
                           (cond ((hashtable-ref locs-ht x #f))
                                 ((memq x '(eval environment syntax->datum))
                                  (error 'compile-loko "Eval needs -feval"))
                                 (else
                                  (error 'compile-loko "No location for primitive" x)))))
               (codes
                (map-in-order (lambda (name core)
                                (when verbose
                                  (display name) (newline))
                                (let ((code (compiler-passes name core)))
                                  ;; (pretty-print (compiler-code->sexpr code))
                                  code))
                              name* core*))
               (codegen-options (filter pair? options)))
          (assemble-text-file out-fn codes primlocs locs-ht locs-rev verbose
                              codegen-options)))))))
