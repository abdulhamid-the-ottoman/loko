;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Loko runtime for the ELF ABI on amd64

(library (loko arch amd64 elf-start)
  (export
    image-header image-footer
    text text-start
    data)
  (import
    (only (loko arch amd64 memory)
          STACK-0-START STACK-0-SIZE HEAP-0-START HEAP-0-SIZE PAGE-SIZE)
    (loko arch amd64 registers)
    (loko arch amd64 objects)
    (loko runtime context)
    (loko config)
    (rnrs)
    (rnrs r5rs)
    (machine-code assembler elf)
    (machine-code format elf))

;;; ELF

;; Linux Standard Base
(define PT_GNU_EH_FRAME #x6474e550)
(define PT_GNU_STACK #x6474e551)
(define PT_GNU_RELRO #x6474e552)

(define elf:start '#(elf start))
(define elf:segments '#(elf segments))
(define elf:sections '#(elf sections))

(define (support-netbsd?)
  (case (config-target-kernel)
    ((netbsd pc+netbsd polyglot) #t)
    (else #f)))

(define segments
  `(text data bss stack0 heap0
         gnu-stack
         ,@(if (support-netbsd?) '(netbsd-note) '())))

(define sections-promise
  (delay
   (make-string-table
    `("" ".text" ".data" ".bss"
      ".shstrtab" ".note.ABI-tag" ".strtab" ".symtab"
      ".debug_gdb_scripts"
      ,@(if (support-netbsd?) '(".note.netbsd.ident") '())))))

(define-syntax sections (identifier-syntax (force sections-promise)))

(define (elf64-header entry)
  ;; This header must be at the very start of the image.
  `((%label ,elf:start)
    ,@(elf-64-assembler (make-elf-image
                         #f ELFCLASS64 ELFDATA2LSB ELFOSABI-SYSV 0
                         ET-EXEC EM-X86-64 EV-CURRENT
                         entry
                         (if (null? segments) 0 `(- ,elf:segments ,elf:start))
                         (if (string-table-empty? sections) 0 `(- ,elf:sections ,elf:start))
                         0 #f #f (length segments) #f (string-table-size sections)
                         (or (string-table-list-index sections ".shstrtab")
                             SHN-UNDEF)))))

(define (image-header)
  (elf64-header 'elf-entry))

(define (mangle str)
  ;; gdb can't handle all the symbols that Loko uses
  (call-with-string-output-port
    (lambda (p)
      (string-for-each (lambda (c)
                         (case c
                           ((#\-) (display #\_ p))
                           ((#\:) (display "_c_" p))
                           ((#\/) (display "_s_" p))
                           ((#\!) (display "_ex" p))
                           ((#\?) (display "_p" p))
                           ((#\*) (display "_star" p))
                           (else (put-char p c))))
                       str))))

(define (image-footer)
  (define sym-labels (make-eq-hashtable))
  (define :shstrtab '#(elf shstrtab))
  (define :shstrtab-end '#(elf shstrtab-end))
  (define :strtab '#(elf strtab))
  (define :strtab-end '#(elf strtab-end))
  (define :symtab '#(elf symtab))
  (define :symtab-end '#(elf symtab-end))
  (define :debug_gdb_scripts '#(elf debug_gdb_scripts))
  (define :debug_gdb_scripts-end '#(elf debug_gdb_scripts_end))
  (define :abi '#(elf abi))
  (define :abi-end '#(elf abi-end))
  (define :netbsd_ident '#(elf netbsd_ident))
  (define :netbsd_ident-end '#(elf netbsd_ident-end))
  ;; The ELF header contains pointers to everything here
  `((%call ,(lambda (assemble! port ip symbols bss _end)
              ;; Rewind to before the BSS was inserted
              (- (- _end bss)))
           bss bss-end)
    (%section elf)
    (%label ,elf:segments)
    ;; Text, data and bss as separate segments.
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-X)
                                          0 elf:start elf:start
                                          `(- text-end ,elf:start)
                                          `(- text-end ,elf:start)
                                          (expt 2 12)))
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R)
                                          `(- data ,elf:start) 'data 'data
                                          `(- data-end data)
                                          `(- data-end data)
                                          (expt 2 12)))
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-W)
                                          0 'bss 'bss
                                          0 `(- bss-end bss)
                                          (expt 2 12)))
    ;; Stack and heap for thread area #0.
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-W)
                                          0 (fx+ STACK-0-START PAGE-SIZE)
                                          (fx+ STACK-0-START PAGE-SIZE)
                                          0 (fx- STACK-0-SIZE PAGE-SIZE) (expt 2 12)))
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-W)
                                          0 HEAP-0-START HEAP-0-START
                                          0 HEAP-0-SIZE (expt 2 12)))
    ;; The initial stack should not be executable. The lack of PF-X
    ;; here also makes Linux turn off READ_IMPLIES_EXEC, which affects
    ;; the segments above.
    ,@(elf-64-assembler (make-elf-segment PT_GNU_STACK (bitwise-ior PF-R PF-W)
                                          0 0 0 0 0 16))
    ,@(if (support-netbsd?)
          (elf-64-assembler (make-elf-segment PT-NOTE (bitwise-ior PF-R)
                                              `(- ,:netbsd_ident ,elf:start)
                                              :netbsd_ident :netbsd_ident
                                              `(- ,:netbsd_ident-end ,:netbsd_ident)
                                              `(- ,:netbsd_ident-end ,:netbsd_ident)
                                              4))
          '())

    ;; section headers
    (%label ,elf:sections)
    ,@(elf-64-assembler (make-elf-section 0 SHT-NULL 0 0 0 0 0 0 0 0))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".text")
                         SHT-PROGBITS
                         (bitwise-ior SHF-EXECINSTR SHF-ALLOC)
                         'text `(- text ,elf:start) '(- text-end text)
                         0 0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".data")
                         SHT-PROGBITS (bitwise-ior SHF-ALLOC)
                         'data
                         `(- data ,elf:start) '(- data-end data)
                         0 0 (expt 2 12) #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".bss")
                         SHT-NOBITS (bitwise-ior SHF-WRITE SHF-ALLOC)
                         'bss
                         0 '(- bss-end bss)
                         0 0 (expt 2 12) #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".shstrtab")
                         SHT-STRTAB 0 0
                         `(- ,:shstrtab ,elf:start)
                         `(- ,:shstrtab-end ,:shstrtab)
                         0 0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".note.ABI-tag")
                         SHT-NOTE 0 0
                         `(- ,:abi ,elf:start)
                         `(- ,:abi-end ,:abi)
                         0 0 8 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".strtab")
                         SHT-STRTAB 0 0
                         `(- ,:strtab ,elf:start)
                         `(- ,:strtab-end ,:strtab)
                         0 0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".symtab")
                         SHT-SYMTAB 0 0
                         `(- ,:symtab ,elf:start)
                         `(- ,:symtab-end ,:symtab)
                         (string-table-list-index sections ".strtab")
                         0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".debug_gdb_scripts")
                         SHT-PROGBITS 0 0
                         `(- ,:debug_gdb_scripts ,elf:start)
                         `(- ,:debug_gdb_scripts-end ,:debug_gdb_scripts)
                         0 0 0 #f))
    ,@(if (support-netbsd?)
          (elf-64-assembler (make-elf-section
                             (string-table-byte-index sections ".note.netbsd.ident")
                             SHT-PROGBITS 0 :netbsd_ident
                             `(- ,:netbsd_ident ,elf:start)
                             `(- ,:netbsd_ident-end ,:netbsd_ident)
                             0 0 4 #f))
          '())

    ;; Section name table.
    (%label ,:shstrtab)
    (%vu8 ,(string-table-bytes sections))
    (%label ,:shstrtab-end)

    ;; ABI tag. This should be using u64's according to "ELF-64
    ;; object file format", but "Linux Standard Base" disagrees.
    (%align 4 0)
    (%label ,:abi)
    (%u32 ,(bytevector-length (string->utf8 "GNU\x0;")))
    (%u32 ,(* 4 (length '(0 3 17 0))))
    (%u32 1)                            ;GNU/Linux
    (%align 4 0)
    (%vu8 ,(string->utf8 "GNU\x0;"))
    (%align 4 0)
    (%u32 ,@'(0 3 17 0))                ;needs at least 3.17
    (%label ,:abi-end)

    ;; NetBSD sysident.o
    ,@(if (support-netbsd?)
          `((%align 4 0)
            (%label ,:netbsd_ident)
            ;; Name size, description size, tag
            (%u32 7 4 1)
            (%vu8 ,(string->utf8 "NetBSD\x0;\x0;"))
            ;; NetBSD 9.0
            (%u32 900000000)
            (%label ,:netbsd_ident-end))
          '())

    ;; The strings for the symbol table
    (%label ,:strtab)
    (%utf8z "")
    (%call ,(lambda (assemble! port ip symbols)
              ;; TODO: this prints some strings that will not be used
              (for-each
               (lambda (sym)
                 (let ((name (car sym)))
                   (when (symbol? name)
                     (cond ((hashtable-ref sym-labels name #f) =>
                            (lambda (label)
                              (assemble! `(%label ,label))))
                           (else
                            (let ((label `#(strtab ,name)))
                              (hashtable-set! sym-labels name label)
                              (assemble! `(%label ,label)))))
                     (assemble! `(%utf8z ,(mangle (symbol->string name)))))))
               symbols)
              0))
    (%label ,:strtab-end)

    ;; The very symbol table itself
    ;;(%align 8 0)
    (%label ,:symtab)
    ,@(elf-64-assembler (make-elf-symbol 0 0 0 0 0 0 0))
    (%call ,(lambda (assemble! port ip symbols)
              (define :TEXT (string-table-list-index sections ".text"))
              (define :DATA (string-table-list-index sections ".data"))
              (define :BSS (string-table-list-index sections ".bss"))
              (define (elf-symbol sym str-label)
                (let* ((start (car sym))
                       (end (cadr sym))
                       (section (caddr sym))
                       (misc (cdddr sym)))
                  (if (or (not section)
                          (eq? start 'text))
                      '()
                      (elf-64-assembler
                       (make-elf-symbol `(- ,str-label ,:strtab)
                                        STB-GLOBAL
                                        (case section
                                          ((text) STT-FUNC)
                                          ((bss data) STT-OBJECT)
                                          (else STT-NOTYPE))
                                        0
                                        (case section
                                          ((text) :TEXT)
                                          ((bss) :BSS)
                                          ((data) :DATA)
                                          ;; SHN-ABS for anything?
                                          (else SHN-UNDEF))
                                        start `(- ,end ,start))))))
              (for-each (lambda (symbol)
                          (when (symbol? (car symbol))
                            (for-each assemble!
                                      (elf-symbol
                                       symbol
                                       (hashtable-ref sym-labels (car symbol) 0)))))
                        symbols)
              0))
    (%label ,:symtab-end)

    ;; Debug scripts for GDB
    (%label ,:debug_gdb_scripts)
    (%u8 1)                           ;Python
    (%utf8z ,(string-append (config-gdb-auto-load-path)
                            "/loko-amd64-" (loko-version) "-gdb.py"))
    (%label ,:debug_gdb_scripts-end)))

;;; .text

;; This is at the very start of the binary image. Its job is to make
;; an ELF image, and before entering scheme-start, set up the basic
;; environment.
(define (text-start)
  (define afl-map-size 65536)
  `((%align 8)
    (%mode 64)
    ;;;
    ;;; ELF entry point.
    ;;;
    (%label elf-entry)
    ;; Enable alignment checking.
    (pushfq)
    (or (mem32+ rsp) ,(RFLAGS AC))
    (popfq)

    ;; On the stack now: argc, argv, envp, auxv (AT_* from
    ;; /usr/include/elf.h), padding, argument strings, environment
    ;; strings, end marker
    (mov rax rsp)
    (sal rax 3)
    (mov (mem64+ boot-loader-data) rax)

    ,@(case (config-target-kernel)
        ((linux pc+linux)
         `((jmp linux-start)))
        ((netbsd pc+netbsd)
         `((jmp netbsd-start)))
        ((polyglot)
         `((test rbx rbx)
           (jnz netbsd-start)
           (jmp linux-start)))
        (else
         (error 'elf-start "Unsupported target kernel" (config-target-kernel))))

    ;;; afl shared memory area (remapped later). Make it twice as
    ;;; large, so that the fork server has somewhere to write to
    ;;; which is not the same area as the instrumented children.
    (%comm afl-map ,(* 2 afl-map-size) 4096)
    (%comm afl-location 8 8)))

(define (text)
  '())

;;; .data

(define (data)
  `()))
