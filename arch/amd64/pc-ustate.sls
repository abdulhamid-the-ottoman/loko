;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; Usermode process state

(library (loko arch amd64 pc-ustate)
  (export
    USTATE-SIZE
    USTATE-ALIGN
    USTATE:CR3
    USTATE:CR2
    USTATE:FAULT-NUMBER
    USTATE:FAULT-CODE
    USTATE:FSBASE
    USTATE:GSBASE
    USTATE:SS
    USTATE:RSP
    USTATE:RFLAGS
    USTATE:CS
    USTATE:RIP
    USTATE:RAX
    USTATE:RBX
    USTATE:RCX
    USTATE:RDX
    USTATE:RSI
    USTATE:RDI
    USTATE:RBP
    USTATE:R8
    USTATE:R9
    USTATE:R10
    USTATE:R11
    USTATE:R12
    USTATE:R13
    USTATE:R14
    USTATE:R15
    USTATE-IRET-TOP
    USTATE-IRET-BOTTOM
    USTATE-SAVE-AREA
    USTATE:FXSAVE
    USTATE-GP-REGS
    )
  (import
    (rnrs (6)))

(define-syntax define-const
  (syntax-rules ()
    ((_ name v)
     (define-syntax name (identifier-syntax v)))))

(define-const USTATE-SIZE         4096)
(define-const USTATE-ALIGN        (fxnot (fx- 16 1)))
(define-const USTATE:CR3          (* 8 0))
(define-const USTATE:CR2          (* 8 1))
(define-const USTATE:FAULT-NUMBER (* 8 2))
(define-const USTATE:FAULT-CODE   (* 8 3))
(define-const USTATE:FSBASE       (* 8 4))
(define-const USTATE:GSBASE       (* 8 5))

;; The iretq data (one u64 for each, from lower address to higher
;; address): RIP, CS, RFLAGS, RSP, SS. Followed by GP regs that are
;; saved/restored when entering/leaving usermode.
(define-const USTATE-IRET-TOP (* 8 64)) ;top of stack for iretq data
(define-const USTATE:SS      (- USTATE-IRET-TOP (* 8 1)))
(define-const USTATE:RSP     (- USTATE-IRET-TOP (* 8 2)))
(define-const USTATE:RFLAGS  (- USTATE-IRET-TOP (* 8 3)))
(define-const USTATE:CS      (- USTATE-IRET-TOP (* 8 4)))
(define-const USTATE:RIP     (- USTATE-IRET-TOP (* 8 5)))

(define-const USTATE:RAX     (- USTATE-IRET-TOP (* 8 6)))
(define-const USTATE:RBX     (- USTATE-IRET-TOP (* 8 7)))
(define-const USTATE:RCX     (- USTATE-IRET-TOP (* 8 8)))
(define-const USTATE:RDX     (- USTATE-IRET-TOP (* 8 9)))
(define-const USTATE:RSI     (- USTATE-IRET-TOP (* 8 10)))
(define-const USTATE:RDI     (- USTATE-IRET-TOP (* 8 11)))
(define-const USTATE:RBP     (- USTATE-IRET-TOP (* 8 12)))
(define-const USTATE:R8      (- USTATE-IRET-TOP (* 8 13)))
(define-const USTATE:R9      (- USTATE-IRET-TOP (* 8 14)))
(define-const USTATE:R10     (- USTATE-IRET-TOP (* 8 15)))
(define-const USTATE:R11     (- USTATE-IRET-TOP (* 8 16)))
(define-const USTATE:R12     (- USTATE-IRET-TOP (* 8 17)))
(define-const USTATE:R13     (- USTATE-IRET-TOP (* 8 18)))
(define-const USTATE:R14     (- USTATE-IRET-TOP (* 8 19)))
(define-const USTATE:R15     (- USTATE-IRET-TOP (* 8 20)))
(define-const USTATE-IRET-BOTTOM USTATE:R15) ;bottom of stack for iretq+GP data

(define-const USTATE-SAVE-AREA (* 8 64)) ;16 quadwords

;; The GP registers that are not popped by iretq. Same order as above.
(define-const USTATE-GP-REGS
  '(rax rbx rcx rdx rsi rdi rbp r8 r9 r10 r11 r12 r13 r14 r15))

(define-const USTATE:FXSAVE  (* 8 128))  ;512-byte area, 16 byte alignment

)
