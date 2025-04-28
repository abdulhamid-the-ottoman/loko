;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; ELF binary format loader

(library (loko kernel binfmt-elf)
  (export
    binfmt-elf-load)
  (import
    (rnrs (6))
    (machine-code format elf)
    (loko system logging)
    (loko system unsafe)
    (only (loko) machine-type)
    (only (loko system $host) dma-allocate dma-free
          $make-ustate
          make-usermode-page-table
          page-table-map!)
    ;; These should hopefully be the same on other archs
    (only (loko arch amd64 linux-numbers)
          AT_ENTRY
          AT_PHDR AT_PHENT AT_PHNUM))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*) (log/x DEBUG x*))
(define (hex x) (number->string x 16))

;; Both command-line and environment are lists of bytevectors. The
;; auxv is a list of conses mapping a machine word to either a machine
;; word or a bytevector. The result should be padded to a multiple of
;; 16 bytes to get the stack pointer properly aligned.

(define (initial-process-stack stack-top command-line environment auxiliary)
  (define pieces '())
  (define len 0)
  (define (push bv)
    (set! pieces (cons bv pieces))
    (set! len (fx+ len (bytevector-length bv)))
    (fx- stack-top len))
  (define (push-word n)
    (let ((bv (make-bytevector 8)))
      (bytevector-u64-native-set! bv 0 n)
      (push bv)))
  (define word-size 8)
  (define stack-align 16)
  (assert (fx>? (fixnum-width) 32))     ;assumes a 64-bit arch
  ;; Arguments
  (let* ((arg* (map (lambda (arg)
                      (push #vu8(0))
                      (push arg))
                    command-line))
         (env* (map (lambda (env)
                      (push #vu8(0))
                      (push env))
                    environment))
         (auxv (map (lambda (aux)
                      ;; The value can be a bytevector. This
                      ;; automatically adds NUL-termination and makes
                      ;; a location for the bytevector.
                      (if (bytevector? (cdr aux))
                          (cons (car aux) (begin (push #vu8(0)) (push (cdr aux))))
                          aux))
                    auxiliary)))
    ;; The data up until now was NUL-terminated strings and stuff, but
    ;; now start the pointers. First align to the word size.
    (push (make-bytevector (fx- (fxand (fx+ len (fx- word-size 1)) (fx- word-size)) len) 0))
    ;; Take the total so far and count the storage for all the
    ;; pointers, the two NULLs and argc. Align to stack-align.
    (let ((total (fx+ len (fx* word-size (+ 4 (length command-line)
                                            (length environment)
                                            (length auxiliary))))))
      (push (make-bytevector (fx- (fxand (fx+ total (fx- stack-align 1)) (fx- stack-align)) total) 0)))
    ;; auxv
    (push-word 0)
    (for-each (lambda (x)
                (push-word (cdr x))
                (push-word (car x)))
              (reverse auxv))
    ;; envp
    (push-word 0)
    (for-each push-word (reverse env*))
    ;; argv
    (push-word 0)
    (for-each push-word (reverse arg*))
    ;; argc
    (push-word (length arg*)))
  ;; Convert to a bytevector. This just goes directly into RAM, so
  ;; there's some wasteful copying.
  (call-with-bytevector-output-port
    (lambda (p)
      (for-each (lambda (bv) (put-bytevector p bv)) pieces))))

;; Load an ELF executable into a new usermode process state.
(define (binfmt-elf-load filename command-line environment auxv)
  ;; TODO: Using only 2M pages would be better.
  ;; TODO: Respect the alignment requirement on segments.
  (define (get-4K-zero-page)
    (dma-allocate 4096 (fxnot 4095)))
  (define page-size 4096)
  ;; Map an ELF segment into the given page table.
  (define (map-segment! p pt seg)
    (cond
      ((eqv? (elf-segment-type seg) PT-LOAD)
       (log/debug seg)
       (log/debug "VADDR: " (hex (elf-segment-vaddr seg))
                  " PADDR: " (hex (elf-segment-paddr seg))
                  " Offset: " (hex (elf-segment-offset seg))
                  " Filesz: " (hex (elf-segment-filesz seg))
                  " Memsz: " (hex (elf-segment-memsz seg))
                  " Align: " (hex (elf-segment-align seg)))
       (let* ((vstart (fxand (elf-segment-vaddr seg) (fxnot (fx- page-size 1))))
              (vend (fxand (fx+ (fx+ (elf-segment-vaddr seg)
                                     (elf-segment-memsz seg))
                                (fx- page-size 1))
                           (fxnot (fx- page-size 1))))
              (page-offset (fx- (elf-segment-vaddr seg) vstart))
              (rwx (fxand (elf-segment-flags seg) (fxior PF-R PF-W PF-X))))
         (log/debug "vstart: " (hex vstart) " vend: " (hex vend))
         (set-port-position! p (fx- (elf-segment-offset seg)
                                    page-offset))
         (do ((temp (make-bytevector page-size))
              (vaddr vstart (fx+ vaddr page-size))
              (remaining (fx+ page-offset (elf-segment-filesz seg))
                         (fx- remaining page-size)))
             ((fx=? vaddr vend))
           (let ((&paddr (dma-allocate page-size (fxnot (fx- page-size 1)))))
             (page-table-map! pt vaddr &paddr rwx #t get-4K-zero-page)
             (when (fx>? remaining 0)
               (let ((n (get-bytevector-n! p temp 0 (fxmin remaining page-size))))
                 #;
                 (log/debug "Copy " n " bytes to #x" (hex &paddr)
                            " vaddr #x" (hex vaddr))
                 (unless (eof-object? n)
                   (do ((i 0 (fx+ i 1)))
                       ((fx=? i n))
                     (put-mem-u8 (fx+ &paddr i) (bytevector-u8-ref temp i))))))))))))
  (call-with-port (open-file-input-port filename)
    (lambda (p)
      (unless (is-elf-image? p)
        (assertion-violation 'binfmt-elf-load
                             "Wrong file format" filename))
      (let ((elf (open-elf-image p)))
        (unless (fx=? (elf-image-type elf) ET-EXEC)
          (assertion-violation 'binfmt-elf
                               "Expected an executable ELF image" filename))
        (unless (fx=? (elf-image-machine elf)
                      (case (vector-ref (machine-type) 0)
                        ((amd64) EM-X86-64)
                        (else -1)))
          (assertion-violation 'binfmt-elf-load
                               "Wrong architecture" filename))
        (let ((pt (make-usermode-page-table)))
          ;; Map the segments
          (for-each (lambda (seg)
                      (map-segment! p pt seg))
                    (elf-image-segments elf))
          ;; Add things we know about the ELF segments.
          (let* ((auxv (cons* (cons AT_ENTRY (elf-image-entry elf))
                              (cons AT_PHENT (elf-image-phentsize elf))
                              (cons AT_PHNUM (elf-image-phnum elf))
                              auxv))
                 (phdr-addr (exists (lambda (seg)
                                      ;; Find the virtual address
                                      ;; phoff. Maybe a bit naïve.
                                      (let ((phoff (elf-image-phoff elf))
                                            (segoff (elf-segment-offset seg))
                                            (segsz (elf-segment-filesz seg)))
                                        (if (< segoff phoff (+ segoff segsz))
                                            (+ (elf-segment-vaddr seg) phoff)
                                            #f)))
                                    (elf-image-segments elf)))
                 (auxv (if phdr-addr
                           (cons (cons AT_PHDR phdr-addr) auxv)
                           auxv)))
            (let* ((stack-top (case (vector-ref (machine-type) 0)
                                ((amd64) #x800000000000)
                                (else (error #f "Stack top unknown"))))
                   (stack (initial-process-stack stack-top command-line environment auxv))
                   (sp (fx- stack-top (bytevector-length stack)))
                   (stack-size-rounded (fxand (fx+ (bytevector-length stack) (fx- page-size 1))
                                              (fxnot (fx- page-size 1))))
                   (stack-start-virt (fx- stack-top stack-size-rounded))
                   ;; XXX: This is a consecutive allocation. It doesn't
                   ;; really need to be, but it simplifies the copying.
                   (stack-phys (dma-allocate stack-size-rounded (fxnot (fx- page-size 1)))))
              ;; Map the stack into the process
              (log/debug "Stack virtual: " (hex stack-start-virt)
                         " physical: " (hex stack-phys)
                         " size: " (hex stack-size-rounded))
              (do ((offset 0 (fx+ offset page-size))
                   (to-be-freed? #t #f))
                  ((fx=? offset stack-size-rounded))
                (page-table-map! pt (fx+ stack-start-virt offset)
                                 (fx+ stack-phys offset)
                                 (fxior PF-R PF-W) to-be-freed? get-4K-zero-page))
              ;; Copy the stack
              (let ((&stack (fx- (fx+ stack-phys stack-size-rounded) (bytevector-length stack))))
                (do ((i 0 (fx+ i 8)))
                    ((fx=? i (bytevector-length stack)))
                  (put-mem-u32 (fx+ &stack i) (bytevector-u32-native-ref stack i))
                  (put-mem-u32 (fx+ &stack (fx+ i 4)) (bytevector-u32-native-ref stack (fx+ i 4)))))
              ;; Initial program counter
              (let ((entry (elf-image-entry elf)))
                (log/debug "ELF entry: " (hex entry)  " stack: " (hex sp))
                (values pt ($make-ustate pt entry sp)))))))))))
