;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; ACPI support

;; PCs use ACPI for many things, including: power management,
;; interrupt routing, and much more information. Much more.

(library (loko arch amd64 pc-acpi)
  (export
    pc-acpi-find-rsdp-structure
    pc-acpi-enumerate-tables
    pc-acpi-open-table

    FADT-signature
    DSDT-signature)
  (import
    (rnrs (6))
    (loko system unsafe)
    (struct pack))

(define offsetof-rsdp-Revision 15)
(define offsetof-rsdp-RsdtAddress 16)
(define offsetof-rsdp-Length 20)
(define offsetof-rsdp-XsdtAddress 24)
(define offsetof-sdt-Signature 0)
(define offsetof-sdt-Length 4)
(define sizeof-sdt 36)

(define FADT-signature "FACP")
(define DSDT-signature "DSDT")

(define fxasl fxarithmetic-shift-left)

;; Search parts of the first 1MB of memory for the Root System
;; Description Pointer (RSDP) structure. TODO: get this from
;; multiboot2.
(define (pc-acpi-find-rsdp-structure)
  (define RSD_ (fxior (fxasl (char->integer #\R) 0)
                      (fxasl (char->integer #\S) 8)
                      (fxasl (char->integer #\D) 16)
                      (fxasl (char->integer #\space) 24)))
  (define PTR_ (fxior (fxasl (char->integer #\P) 0)
                      (fxasl (char->integer #\T) 8)
                      (fxasl (char->integer #\R) 16)
                      (fxasl (char->integer #\space) 24)))
  (define (verify-rsdp-checksum &addr)
    (do ((i 0 (fx+ i 1))
         (sum 0 (fxand #xff (fx+ sum (get-mem-u8 (fx+ &addr i))))))
        ((fx=? i 20)
         ;; TODO: there's also an extended checksum
         (eqv? sum 0))))
  (define (find-rsdp &addr len)
    (let lp ((i 0) (&addr &addr))
      (cond ((fx=? i len)
             #f)
            ((and (eqv? RSD_ (get-mem-u32 &addr))
                  (eqv? PTR_ (get-mem-u32 (fx+ &addr 4)))
                  (verify-rsdp-checksum &addr))
             &addr)
            (else
             (lp (fx+ i 16) (fx+ &addr 16))))))
  ;; XXX: assumes that the memory map contained all ACPI data and that
  ;; it has already been identity-mapped.
  (let* ((ebda-seg (get-mem-u16 (+ (fxasl #x40 4) #x0E)))
         (&ebda (fxasl ebda-seg 4)))
    (or (find-rsdp &ebda 1024)
        (find-rsdp #xE0000 #x20000))))

;; Get the 32-bit value from an unaligned memory address, little
;; endian encoding.
(define (get-mem-u32leu &addr)
  (fxior (fxasl (get-mem-u8 &addr) 0)
         (fxasl (get-mem-u8 (fx+ &addr 1)) 8)
         (fxasl (get-mem-u8 (fx+ &addr 2)) 16)
         (fxasl (get-mem-u8 (fx+ &addr 3)) 24)))

(define (acpi-sdt-length &addr)
  (get-mem-u32leu (fx+ &addr offsetof-sdt-Length)))

;; Check that the given table looks valid.
(define (acpi-validate-table &addr)
  (do ((len (acpi-sdt-length &addr))
       (i 0 (fx+ i 1))
       (sum 0 (fxand #xff (fx+ sum (get-mem-u8 (fx+ &addr i))))))
      ((fx=? i len)
       (eqv? sum 0))))

;; Open a binary input port connected to the table data.
(define (open-memory-acpi-table-port &addr)
  (define pos 0)
  (define (read! bv start count)
    (let ((count (fxmin count (fx- (acpi-sdt-length &addr) pos))))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i count)
           (set! pos (fx+ pos i))
           count)
        (bytevector-u8-set! bv (fx+ start i) (get-mem-u8 (fx+ &addr (fx+ pos i)))))))
  (define (port-position)
    pos)
  (define (set-port-position! new-pos)
    (set! pos (fxmin new-pos (acpi-sdt-length &addr))))
  (define id
    (string-append "ACPI "
                   (utf8->string
                    (pack "<L" (get-mem-u32leu (fx+ &addr offsetof-sdt-Signature))))
                   " @ #x"
                   (number->string &addr 16)))
  (make-custom-binary-input-port id read! port-position set-port-position! #f))

;; Get a list of ACPI tables. The tables are not checksum-validated.
;; Returns a list of (<signature> . <thunk>), where signature is the
;; table signature as a string and the thunk opens a port that
;; contains the bytes of the structure.
(define (pc-acpi-enumerate-tables &rsdp)
  (define (acpi-search-rsdt &addr entry-size)
    (and
      (acpi-validate-table &addr)
      (let ((p (open-memory-acpi-table-port &addr)))
        (set-port-position! p sizeof-sdt)
        (let lp ()
          (if (port-eof? p)
              '()
              (let* ((&table (if (eqv? entry-size 8) (get-unpack p "=Q") (get-unpack p "=L")))
                     (tablep (open-memory-acpi-table-port &table))
                     (signature (utf8->string (get-bytevector-n tablep 4))))
                (cons (cons signature (lambda () (open-memory-acpi-table-port &table)))
                      (cond ((equal? signature FADT-signature)
                             ;; The DSDT is squirreled away behind the
                             ;; FADT. Use the 64-bit pointer if it
                             ;; exists; otherwise the 32-bit pointer.
                             (let ((bv (begin
                                         (set-port-position! tablep 40)
                                         (get-bytevector-n tablep (format-size "=L"))))
                                   (x-bv (begin
                                           (set-port-position! tablep 140)
                                           (get-bytevector-n tablep (format-size "=Q")))))
                               (cond ((and (bytevector? x-bv)
                                           (eqv? (bytevector-length x-bv) (format-size "=Q")))
                                      (let ((&x_dsdt (unpack "=Q" x-bv)))
                                        (cons (cons DSDT-signature
                                                    (lambda ()
                                                      (open-memory-acpi-table-port &x_dsdt)))
                                              (lp))))
                                     ((and (bytevector? bv)
                                           (eqv? (bytevector-length bv) (format-size "=L")))
                                      (let ((&dsdt (unpack "=L" bv)))
                                        (cons (cons DSDT-signature
                                                    (lambda ()
                                                      (open-memory-acpi-table-port &dsdt)))
                                              (lp))))
                                     (else (lp)))))
                            (else (lp))))))))))
  (let ((&rsdt (get-mem-u32 (fx+ &rsdp offsetof-rsdp-RsdtAddress)))
        (&xsdt (if (fx>=? (get-mem-u8 (fx+ &rsdp offsetof-rsdp-Revision)) 2)
                   (get-mem-s61 (fx+ &rsdp offsetof-rsdp-XsdtAddress))
                   0)))
    (append (or (and (not (eqv? 0 &xsdt))
                     (acpi-validate-table &xsdt)
                     (acpi-search-rsdt &xsdt 8))
                '())
            (or (and (not (eqv? 0 &rsdt))
                     (acpi-validate-table &rsdt)
                     (acpi-search-rsdt &rsdt 4))
                '()))))

;; Return a port that can be used to read the table.
(define (pc-acpi-open-table tables signature)
  (cond ((assoc signature tables) =>
         (lambda (x)
           (let ((open (cdr x)))
             (open))))
        (else #f))))
