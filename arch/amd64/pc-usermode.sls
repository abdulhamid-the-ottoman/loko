;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; Usermode process support

(library (loko arch amd64 pc-usermode)
  (export
    $make-ustate
    $free-ustate
    make-usermode-page-table
    page-table-map!
    page-table-lookup
    page-table-free!)
  (import
    (rnrs)
    (loko system unsafe)
    (only (loko system $host) dma-allocate dma-free)
    (only (loko system $primitives) $linker-address)
    (loko arch amd64 pc-ustate)
    (prefix (loko arch amd64 pc-segments) pc-segments:)
    (loko arch amd64 registers))

#;
(define (print . x) (for-each display x) (newline))

(define-syntax print
  (lambda (x)
    (syntax-case x ()
      [(_ . x) #'(values)])))

(define-record-type page-table
  (sealed #t)
  (fields &pt (mutable valid?)))

(define (make-usermode-page-table)
  (let ((&pt (dma-allocate 4096 (fxnot 4095)))
        (&pml4 ($linker-address 'pml4)))
    ;; Copy the entry for the last part of the address space. This is
    ;; used for supervisor mode (supervisor-addr). Zero out the U/S
    ;; bit for good measure.
    (put-mem-u64 (fx+ &pt (- 4096 8))
                 (bitwise-and (get-mem-u64 (fx+ &pml4 (- 4096 8)))
                              (bitwise-not (expt 2 page-U/S))))
    (make-page-table &pt #t)))

;; XXX: the {put,get}-mem-s61 procedures can't be used fully
;; correctly because the NX flag is bit 63.
(define (get-mem-u64 a)
  (bitwise-ior (get-mem-u32 a)
               (bitwise-arithmetic-shift-left (get-mem-u32 (fx+ a 4)) 32)))

(define (put-mem-u64 a v)
  (put-mem-u32 a (bitwise-and v #xffffffff))
  (put-mem-u32 (fx+ a 4) (bitwise-arithmetic-shift-right v 32)))

(define (get-pml4t ptrec)
  (fxand (page-table-&pt ptrec)
         (fxnot (- (expt 2 12) 2))))

;; Flags available in all levels of the page table hierarchy
(define page-P   0)                 ;present
(define page-R/W 1)                 ;read/write
(define page-U/S 2)                 ;user/supervisor
(define page-PWT 4)                 ;page-level writethrough(*)
(define page-PCD 4)                 ;page-level cache disable(*)
(define page-A   5)                 ;accessed
(define page-NX  63)                ;no-execute

;; Also available in all levels, but specific to Loko
(define page-TBF 9)                 ;to be freed on page table free

;; Flags available only in the lowest level of the page table
(define page-D       6)             ;dirty
(define pte-PAT      7)             ;page-attribute table(*)
(define pte-G        8)             ;global
(define pde/pdpe-PAT 12)            ;page-attribute table(*)

;; Flags available only at the PDE level, and the PDPE level if
;; support for 1GB pages is available.
(define page-PS 7)       ;page size

;; Bit field offsets in virtual addresses
(define PHYSICAL-SIGN 52)           ;XXX:
(define VIRTUAL-SIGN 48)            ;XXX:
(define VIRTUAL-PML4 39)
(define VIRTUAL-PDP 30)
(define VIRTUAL-PD 21)
(define VIRTUAL-PT 12)

;; Helpers.

(define (align-down x n)
  (fxand x (fxnot (fx- n 1))))

(define (align-up x n)
  (fxand (fx+ x (fx- n 1))
         (fxnot (fx- n 1))))

(define (entry-present? x) (fxbit-set? x page-P))

(define (entry-writable? x) (fxbit-set? x page-R/W))

(define (entry-user? x) (fxbit-set? x page-U/S))

(define (entry-to-be-freed? x) (fxbit-set? x page-TBF))

;; Uses the second u32 of the entry
(define (entry^-no-execute? x) (fxbit-set? x (fx- page-NX 32)))

(define (pml4e-address x)
  (fxand x (fxand (fxnot (- (expt 2 12) 1))
                  (- (expt 2 52) 1))))

(define (pdpe-1G? x) (fxbit-set? x page-PS))

(define (pdpe-1G-address x)
  (fxand x (fxand (fxnot (- (expt 2 30) 1))
                  (- (expt 2 52) 1))))

(define pdpe-address pml4e-address)

(define pde-2M? pdpe-1G?)

(define pde-4K-address pml4e-address)

(define (pde-2M-address x)
  (fxand x (fxand (fxnot (- (expt 2 20) 1))
                  (- (expt 2 52) 1))))

(define (pde-2M-pat x)
  (fxior (if (bitwise-bit-set? x pde/pdpe-PAT) #b100 0)
         (if (bitwise-bit-set? x page-PCD) #b10 0)
         (if (bitwise-bit-set? x page-PWT) #b1 0)))

(define pte-address pml4e-address)

(define (pte-pat x)
  (fxior (if (bitwise-bit-set? x pte-PAT) #b100 0)
         (if (bitwise-bit-set? x page-PCD) #b10 0)
         (if (bitwise-bit-set? x page-PWT) #b1 0)))

(define (pflags e . fs)
  (let lp ((fs fs))
    (if (null? fs)
        '()
        (let ((bit (car fs)) (name (cadr fs)) (fs (cddr fs)))
          (if (bitwise-bit-set? e bit)
              (cons name (lp fs))
              (lp fs))))))

(define (pte-flags e)
  (cons (list 'PAT (pte-pat e))
        (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
                page-D 'D pte-G 'G page-NX 'NX)))

(define (pde-2M-flags e)
  (cons (list 'PAT (pde-2M-pat e))
        (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
                page-D 'D pte-G 'G page-NX 'NX)))

(define (pde-4K-flags e)
  (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
          page-NX 'NX))

(define pdpe-flags pde-4K-flags)

(define pml4e-flags pde-4K-flags)

(define (vaddr-pml4 addr)
  (fxand (fxarithmetic-shift-right addr VIRTUAL-PML4)
         (- (expt 2 (- 64 VIRTUAL-SIGN)) 1)))

(define (vaddr-pdp addr)
  (fxbit-field addr VIRTUAL-PDP VIRTUAL-PML4))

(define (vaddr-pd addr)
  (fxbit-field addr VIRTUAL-PD VIRTUAL-PDP))

(define (vaddr-pt addr)
  (fxbit-field addr VIRTUAL-PT VIRTUAL-PD))

(define (print-page-table ptrec)
  (define (table-for-each proc table-base)
    ;; Run proc on each of the entries in a descriptor table.
    (do ((i 0 (fx+ i 8)))
        ((fx=? i 4096))
      (let ((e (get-mem-s61 (fx+ table-base i))))
        (when (entry-present? e)
          (proc e (fxarithmetic-shift-right i 3))))))
  (define (print-pml4e e base)
    (print " PML4E table base #x" (fmt-addr base) " entry at #x"
           (fmt-addr (pml4e-address e)) #\tab (pml4e-flags e))
    (table-for-each
     (lambda (pdpe i)
       (print-pdpe pdpe (fxior base (fxarithmetic-shift-left i VIRTUAL-PDP))))
     (pml4e-address e)))
  (define (print-pdpe e base)
    (cond ((pdpe-1G? e)
           (print "  PDPE 1G page #x" (fmt-addr base) " => #x"
                  (fmt-addr (pdpe-1G-address e))))
          (else
           (print "  PDPE table base #x" (fmt-addr base)
                  " entry at #x" (fmt-addr (pdpe-address e))
                  #\tab (pdpe-flags e))
           (table-for-each
            (lambda (pde i)
              (print-pde pde (fxior base (fxarithmetic-shift-left i VIRTUAL-PD))))
            (pdpe-address e))
           (print))))
  (define (print-pde e base)
    (cond ((pde-2M? e)
           (print "   PDE 2M page #x" (fmt-addr base) " => #x"
                    (fmt-addr (pde-2M-address e)) #\tab (pde-2M-flags e)))
          (else
           (print "   PDE 4K table base #x" (fmt-addr base) " entry at #x"
                  (fmt-addr (pde-4K-address e)) #\tab (pde-4K-flags e))
           (table-for-each
            (lambda (pte i)
              (print-pte pte (fxior base (fxarithmetic-shift-left i VIRTUAL-PT))))
            (pde-4K-address e))
           (print))))
  (define (fmt-addr x) (and x (number->string x 16)))
  (define (print-pte e base)
    (print "    PTE page #x" (fmt-addr base) " => #x"
           (fmt-addr (pte-address e)) #\tab (pte-flags e)))
  (define pt (page-table-&pt ptrec))
  (print "Full page table follows.")
  (print "PML4: #x" (fmt-addr pt))
  (print "Page-Map Level-4 Table:")
  (table-for-each
   (lambda (pml4e i)
     ;; TODO: sign-extension
     (print-pml4e pml4e (fxarithmetic-shift-left i VIRTUAL-PML4)))
   (get-pml4t ptrec)))

;; FIXME: There is essentially another copy of this procedure in pc-init.

;; Create a virtual memory mapping from vaddr to paddr. Only uses 4K pages.
;; The rwx argument matches the ELF segment flags.
(define (page-table-map! ptrec vaddr paddr rwx to-be-freed? get-4K-zero-page)
  ;; XXX: can ELF have unreadable pages?
  (define higher-attributes
    (fxior (expt 2 page-P)
           (expt 2 page-R/W)
           (expt 2 page-U/S)
           (expt 2 page-TBF)))
  (define page-attributes
    (bitwise-ior (bitwise-ior
                  (expt 2 page-P)
                  (expt 2 page-U/S))
                 (if to-be-freed?
                     (expt 2 page-TBF)
                     0)
                 (if (fxbit-set? rwx 1)
                     (expt 2 page-R/W)
                     (if (not (fxbit-set? rwx 0))
                         (expt 2 page-NX)
                         0))))
  (assert (fx=? vaddr (align-down vaddr 4096)))
  (assert (fx=? paddr (align-down paddr 4096)))
  (assert (page-table-valid? ptrec))
  (let ((pml4 (vaddr-pml4 vaddr))
        (rest (fxand vaddr (- (expt 2 VIRTUAL-SIGN) 1))))
    (let ((pdp (vaddr-pdp rest)) (pd (vaddr-pd rest)) (pt (vaddr-pt rest)))
      ;; Populate the table.
      (print "Populate: " (list pml4 pdp pd pt (number->string page-attributes 16)))
      (let ((&pml4e (fx+ (get-pml4t ptrec)
                         (fx* pml4 8))))
        ;; Add a PML4E
        (when (not (entry-present? (get-mem-u32 &pml4e)))
          (print "Adding a page to PML4T")
          (put-mem-u64 &pml4e (bitwise-ior (get-4K-zero-page)
                                           higher-attributes)))
        (let ((&pdpe (fx+ (pml4e-address (get-mem-s61 &pml4e))
                          (fx* pdp 8))))
          ;; Add a PDPE
          (when (not (entry-present? (get-mem-u32 &pdpe)))
            (print "Adding a page to PDPT")
            (put-mem-u64 &pdpe (bitwise-ior (get-4K-zero-page)
                                            higher-attributes)))
          (let ((pdpe (get-mem-s61 &pdpe)))
            (unless (pdpe-1G? pdpe)
              (let ((&pde (fx+ (pdpe-address pdpe)
                               (fx* pd 8))))
                ;; Add a PDE
                (when (not (entry-present? (get-mem-u32 &pde)))
                  (print "Adding a page to PDT")
                  (put-mem-u64 &pde (bitwise-ior (get-4K-zero-page)
                                                 higher-attributes)))
                (let ((pde (get-mem-s61 &pde)))
                  (unless (pde-2M? pde)
                    (let ((&pte (fx+ (pde-4K-address pde)
                                     (fx* pt 8))))
                      ;; Add a PTE
                      (when (not (entry-present? (get-mem-u32 &pte)))
                        (print "Adding a PTE")
                        (put-mem-u64 &pte (bitwise-ior paddr page-attributes))))))))))))))

;; Look up a virtual address in a page table, checking that usermode
;; has permissions along the way. Returns the physical address and the
;; number of bytes valid at the address.
(define (page-table-lookup ptrec vaddr write?)
  (define test (fxior (expt 2 page-P)
                      (expt 2 page-U/S)
                      (if write? (expt 2 page-R/W) 0)))
  (assert (page-table-valid? ptrec))
  (let ((pml4 (vaddr-pml4 vaddr))
        (rest (fxand vaddr (- (expt 2 VIRTUAL-SIGN) 1))))
    (let ((pdp (vaddr-pdp rest)) (pd (vaddr-pd rest)) (pt (vaddr-pt rest)))
      (let ((e (get-mem-s61 (fx+ (get-pml4t ptrec)
                                 (fx* pml4 8)))))
        (if (not (fx=? test (fxand e test)))
            (values #f #f)
            (let ((e (get-mem-s61 (fx+ (pml4e-address e)
                                       (fx* pdp 8)))))
              (if (not (fx=? test (fxand e test)))
                  (values #f #f)
                  (if (pdpe-1G? e)
                      (let ((offset (fxbit-field vaddr 0 VIRTUAL-PDP)))
                        (values (fx+ (pdpe-1G-address e) offset)
                                (fx- (expt 2 VIRTUAL-PDP) offset)))
                      (let ((e (get-mem-s61 (fx+ (pdpe-address e)
                                                 (fx* pd 8)))))
                        (if (not (fx=? test (fxand e test)))
                            (values #f #f)
                            (if (pde-2M? e)
                                (let ((offset (fxbit-field vaddr 0 VIRTUAL-PD)))
                                  (values (fx+ (pde-2M-address e) offset)
                                          (fx- (expt 2 VIRTUAL-PD) offset)))
                                (let ((e (get-mem-s61 (fx+ (pde-4K-address e)
                                                           (fx* pt 8)))))
                                  (if (not (fx=? test (fxand e test)))
                                      (values #f #f)
                                      (let ((offset (fxbit-field vaddr 0 VIRTUAL-PT)))
                                        (values (fx+ (pte-address e) offset)
                                                (fx- (expt 2 VIRTUAL-PT) offset))))))))))))))))

;; Free all user-accessible pages in the page table and the page table
;; itself.
(define (page-table-free! ptrec)
  (define (free addr)
    (print "Freeing " (fmt-addr addr))
    (dma-free addr))

  (define (table-for-each proc table-base)
    ;; Run proc on each of the present user-accessible entries in a
    ;; descriptor table, which are marked to be freed.
    (define test (fxior (expt 2 page-P)
                        (expt 2 page-U/S)
                        (expt 2 page-TBF)))
    (do ((i 0 (fx+ i 8)))
        ((fx=? i 4096))
      (let ((e (get-mem-s61 (fx+ table-base i))))
        (when (fx=? test (fxand e test))
          (proc e (fxarithmetic-shift-right i 3)))))
    (free table-base))

  (define (free-pml4e e base)
    (print " PML4E table base #x" (fmt-addr base) " entry at #x"
           (fmt-addr (pml4e-address e)) #\tab (pml4e-flags e))
    (table-for-each
     (lambda (pdpe i)
       (free-pdpe pdpe (fxior base (fxarithmetic-shift-left i VIRTUAL-PDP))))
     (pml4e-address e)))

  (define (free-pdpe e base)
    (cond ((pdpe-1G? e)
           (print "  PDPE 1G page #x" (fmt-addr base) " => #x"
                  (fmt-addr (pdpe-1G-address e)))
           (free (pdpe-1G-address e)))
          (else
           (print "  PDPE table base #x" (fmt-addr base)
                  " entry at #x" (fmt-addr (pdpe-address e))
                  #\tab (pdpe-flags e))
           (table-for-each
            (lambda (pde i)
              (free-pde pde (fxior base (fxarithmetic-shift-left i VIRTUAL-PD))))
            (pdpe-address e))
           (print))))

  (define (free-pde e base)
    (cond ((pde-2M? e)
           (print "   PDE 2M page #x" (fmt-addr base) " => #x"
                  (fmt-addr (pde-2M-address e)) #\tab (pde-2M-flags e))
           (free (pde-2M-address e)))
          (else
           (print "   PDE 4K table base #x" (fmt-addr base) " entry at #x"
                  (fmt-addr (pde-4K-address e)) #\tab (pde-4K-flags e))
           (table-for-each
            (lambda (pte i)
              (free-pte pte (fxior base (fxarithmetic-shift-left i VIRTUAL-PT))))
            (pde-4K-address e))
           (print))))

  (define (fmt-addr x)
    (and x (number->string x 16)))

  (define (free-pte e base)
    (print "    PTE page #x" (fmt-addr base) " => #x"
           (fmt-addr (pte-address e)) #\tab (pte-flags e))
    (free (pte-address e)))

  (assert (page-table-valid? ptrec))
  (page-table-valid?-set! ptrec #f)
  (table-for-each
   (lambda (pml4e i)
     (free-pml4e pml4e (fxarithmetic-shift-left i VIRTUAL-PML4)))
   (get-pml4t ptrec)))

(define ($make-ustate ptrec entry rsp)
  (let ((&ustate (dma-allocate USTATE-SIZE USTATE-ALIGN))
        (cs pc-segments:code-PL3)
        (ss pc-segments:data-PL3)
        (rflags (RFLAGS IF))
        (rax 0)
        (rbx 0)
        (rcx 0)
        (rdx 0)
        (rsi 0)
        (rdi 0)
        (rbp 0)
        (r8 0)
        (r9 0)
        (r10 0)
        (r11 0)
        (r12 0)
        (r13 0)
        (r14 0)
        (r15 0))
    (put-mem-s61 (fx+ &ustate USTATE:CR3) (page-table-&pt ptrec))

    ;; Initialize the FXSAVE area
    (put-mem-u16 (fx+ &ustate (fx+ USTATE:FXSAVE #x00)) #x037F)
    (put-mem-u32 (fx+ &ustate (fx+ USTATE:FXSAVE #x18))
                 (MXCSR PM UM OM ZM DM IM))

    ;; Create a stack for syscall-usermode-run
    (put-mem-s61 (fx+ &ustate USTATE:SS) ss)
    (put-mem-s61 (fx+ &ustate USTATE:RSP) rsp)
    (put-mem-s61 (fx+ &ustate USTATE:RFLAGS) rflags)
    (put-mem-s61 (fx+ &ustate USTATE:CS) cs)
    (put-mem-s61 (fx+ &ustate USTATE:RIP) entry)
    (put-mem-s61 (fx+ &ustate USTATE:RAX) rax)
    (put-mem-s61 (fx+ &ustate USTATE:RBX) rbx)
    (put-mem-s61 (fx+ &ustate USTATE:RCX) rcx)
    (put-mem-s61 (fx+ &ustate USTATE:RDX) rdx)
    (put-mem-s61 (fx+ &ustate USTATE:RSI) rsi)
    (put-mem-s61 (fx+ &ustate USTATE:RDI) rdi)
    (put-mem-s61 (fx+ &ustate USTATE:RBP) rbp)
    (put-mem-s61 (fx+ &ustate USTATE:R8) r8)
    (put-mem-s61 (fx+ &ustate USTATE:R9) r9)
    (put-mem-s61 (fx+ &ustate USTATE:R10) r10)
    (put-mem-s61 (fx+ &ustate USTATE:R11) r11)
    (put-mem-s61 (fx+ &ustate USTATE:R12) r12)
    (put-mem-s61 (fx+ &ustate USTATE:R13) r13)
    (put-mem-s61 (fx+ &ustate USTATE:R14) r14)
    (put-mem-s61 (fx+ &ustate USTATE:R15) r15)

    ;; FIXME: wrap in a suitable record type
    &ustate))

(define ($free-ustate ustate)
  (dma-free ustate))

)
