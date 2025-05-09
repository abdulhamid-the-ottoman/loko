;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Interrupt descriptors and entry points

;; Note: The stack should always be 16-byte aligned in interrupt
;; handlers. The CPU takes care of that itself when there is no
;; automatic stack switch.

;; TODO: #MC indicates that there is no reliable way to restart the program!

(library (loko arch amd64 pc-interrupts)
  (export text data)
  (import
    (rename (rnrs) (bitwise-arithmetic-shift ash))
    (loko arch amd64 config)
    (loko arch amd64 registers)
    (loko arch amd64 objects)
    (loko runtime context)
    (prefix (loko arch amd64 pc-segments) pc-segments:))

;; These are programmed into the PICs and designate offsets into the
;; IDT. Must be aligned to 8.
;; (define PIC1-offset 32)
;; (define PIC2-offset (+ PIC1-offset 8))
;; (define APIC-offset (+ PIC2-offset 8))

;; (define irq-APIC-timer (+ APIC-offset 0))
;; (define irq-APIC-spurious (+ APIC-offset 1))
;; (define irq-APIC-lint0 (+ APIC-offset 2))
;; (define irq-APIC-lint1 (+ APIC-offset 3))
;; (define irq-APIC-error (+ APIC-offset 4))
;; (define irq-APIC-perfcnt (+ APIC-offset 5))
;; (define irq-APIC-thermal (+ APIC-offset 6))

;; Descriptor types
(define interrupt-gate #b1110)          ;disables interrupts
(define trap-gate #b1111)               ;does not disable interrupts

(define (sysdesc dpl type offset selector ist)
  ;; Encodes a 64-bit system descriptor. Interrupt and trap gates
  ;; only right now. The DPL controls which privilege levels can
  ;; manually trigger an interrupt. Set dpl=3 to allow the int NN
  ;; instruction for CPL=3.
  `(bitwise-ior (bitwise-bit-field ,offset 0 16)
                (ash (bitwise-bit-field ,offset 16 64) ,(+ 32 16))
                ,(ash selector 16)      ;cs
                ,(ash ist 32)           ;Interrupt Stack Table
                ,(ash type (+ 32 8))
                ,(ash dpl (+ 32 13))    ;Descriptor Privilege Level
                ,(ash 1 (+ 32 15))))    ;Segment Present

(define (make-default-idt)
  ;; Code segment selector to use for the IDT (this is an index into
  ;; the GDT). Because HLT only runs in CPL=0, this cs must be used
  ;; for everything that can interrupt HLT.
  (define cs pc-segments:code-PL0-non-conforming)
  ;; Code segment selector for exceptions that run in CPL=3. XXX: the
  ;; interrupt handler for these will overwrite the Scheme stack, but
  ;; that might be OK, just as long as enough debugging info is
  ;; available elsewhere. Anything here with IST=0 must accept that
  ;; the Scheme stack is gone.
  (define cs-cpl3 pc-segments:code-PL3)
  (define idt (make-vector 256 0))
  ;; 0-31: Exceptions. The * suffix means an error code is pushed
  ;; on the stack.
  (vector-set! idt 0 (sysdesc 0 trap-gate 'fault-DE cs-cpl3 0))
  (vector-set! idt 1 (sysdesc 0 interrupt-gate 'trap/fault-DB cs 7))
  (vector-set! idt 2 (sysdesc 0 interrupt-gate 'int-NMI cs 1))
  (vector-set! idt 3 (sysdesc 0 interrupt-gate 'trap-BP cs 7))
  ;;4 #OF INTO (impossible)
  ;;5 #BR BOUND (impossible)
  (vector-set! idt 6 (sysdesc 0 trap-gate 'fault-UD cs-cpl3 0))
  (vector-set! idt 7 (sysdesc 0 interrupt-gate 'fault-NM cs 7))
  (vector-set! idt 8 (sysdesc 0 interrupt-gate 'abort-DF* cs 7))
  ;;9 Not used anymore
  (vector-set! idt 10 (sysdesc 0 interrupt-gate 'fault-TS* cs 7))
  (vector-set! idt 11 (sysdesc 0 interrupt-gate 'fault-NP* cs 7))
  (vector-set! idt 12 (sysdesc 0 trap-gate 'fault-SS* cs-cpl3 0))
  (vector-set! idt 13 (sysdesc 0 trap-gate 'fault-GP* cs-cpl3 0))
  (vector-set! idt 14 (sysdesc 0 interrupt-gate 'fault-PF* cs 2))
  ;;15 Intel reserved
  (vector-set! idt 16 (sysdesc 0 interrupt-gate 'fault-MF cs 7))
  (vector-set! idt 17 (sysdesc 0 trap-gate 'fault-AC* cs-cpl3 0))
  (vector-set! idt 18 (sysdesc 0 interrupt-gate 'abort-MC cs 7))
  (vector-set! idt 19 (sysdesc 0 interrupt-gate 'fault-XM cs 7)) ;aka #XF?
  ;;30 Security Exception #SX
  ;;31 Reserved
  ;; Fill in every entry so there is never a #GP due to a missing
  ;; IDT entry. N.B.! This can not handle pushed error codes. It
  ;; is not possible to know if new vectors in the 0-31 range will
  ;; have error codes.
  (let lp ((i 0))
    (unless (fx=? i (vector-length idt))
      (cond
        ((and (eqv? (vector-ref idt i) 0) (fx>=? i 32))
         (let ((name (string->symbol
                      (string-append "int-" (number->string i 16)))))
           (vector-set! idt i (sysdesc 0 interrupt-gate name cs 6))
           (lp (fx+ i 1))))
        (else
         (lp (fx+ i 1))))))
  (vector->list idt))

(define (make-usermode-idt)
  (define (sysdesc* dpl type offset selector ist)
    (sysdesc dpl type `(bitwise-ior ,offset ,pc-segments:supervisor-addr)
             selector ist))
  (define cs pc-segments:code-PL0-non-conforming)
  (define idt (make-vector 256 0))
  (define ist 7)
  ;; 0-31: Exceptions. The * suffix means an error code is pushed
  ;; on the stack. Only need special handlers for those that store
  ;; extra error information (e.g. an error code or CR2).
  ;; TODO: Let usermode handle e.g #AC by itself. Let usermode invoke some
  ;; of these directly, int3?
  (vector-set! idt 2 (sysdesc* 0 interrupt-gate 'int-NMI cs 1)) ;try to ignore NMI
  (vector-set! idt 8 (sysdesc* 0 interrupt-gate 'abort-DF* cs 7)) ;double faults are fatal
  (vector-set! idt 10 (sysdesc* 0 interrupt-gate 'fault-usermode-TS* cs ist))
  (vector-set! idt 11 (sysdesc* 0 interrupt-gate 'fault-usermode-NP* cs ist))
  (vector-set! idt 12 (sysdesc* 0 interrupt-gate 'fault-usermode-SS* cs ist))
  (vector-set! idt 13 (sysdesc* 0 interrupt-gate 'fault-usermode-GP* cs ist))
  (vector-set! idt 14 (sysdesc* 0 interrupt-gate 'fault-usermode-PF* cs ist))
  (vector-set! idt 17 (sysdesc* 0 interrupt-gate 'fault-usermode-AC* cs ist))
  ;; Everything else calls a generic handler.
  (do ((i 0 (fx+ i 1)))
      ((fx=? i (vector-length idt)))
    (when (eqv? (vector-ref idt i) 0)
      (let ((name (string->symbol
                   (string-append "int-usermode-" (number->string i 16)))))
        (vector-set! idt i (sysdesc* 0 interrupt-gate name cs ist)))))
  (vector->list idt))

;; Makes code for interrupt handlers that push the interrupt number
;; and then call a generic handler.
(define (make-generic-handlers prefix target)
  (let lp ((i 0))
    (if (fx=? i 256)
        '()
        (let ((name (string->symbol (string-append prefix (number->string i 16)))))
          `((%label ,name)
            (push ,(immediate i))
            (jmp ,target)
            ,@(lp (fx+ i 1)))))))

(define (text)
  (define (pvec i) (+ (* 8 i) (- (tag 'vector))))
  (define all-registers '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

  `(
;;; Serious exceptions

    ,@(letrec ((gen (lambda (label code)
                      `((%label ,label)
                        (mov al ,code) (out #x80 al)
                        (hlt)
                        (jmp ,label)))))
        `(,@(gen 'fault-TS* #xf0)     ;Invalid TSS
          ,@(gen 'fault-NP* #xf1)     ;Segment Not Present
          ,@(gen 'trap/fault-DB #xf3) ;Debug Exception
          ,@(gen 'fault-MF #xf4)      ;x87 FPU Floating-Point Error
          ,@(gen 'fault-XM #xf5)      ;SSE exception
          ,@(gen 'trap-BP #xf6)       ;Breakpoint
          ,@(gen 'abort-MC #xf7)      ;Machine Check
          ,@(gen 'fault-NM #xf8)      ;Device not available
          ,@(gen 'abort-DF* #xf9)     ;Double Fault
          ))

    ;; (%label trap/fault-DB)            ;Debug Exception
    ;; (push 0)                          ;code
    ;; (push 3)                          ;vector
    ;; (jmp gdb-attach)

    ;; (%label trap-BP)                  ;Breakpoint
    ;; (push 0)                          ;code
    ;; (push 1)                          ;vector
    ;; (jmp gdb-attach)

;;; Page fault
    (%label fault-PF*)                ;Page Fault
    ;; RSP layout: code RIP CS RFLAGS RSP SS. PF executes on IST 2
    ;; to allow dynamic extension of stack space.

    ;; Check if this is a reserved bit violation or if the CPU was
    ;; executing in a data or non-present page.
    (test (mem32+ rsp) #b11000)
    (jnz PF-serious-error)
    ;; TODO: handle non-present pages for the growable stacks

    ;; Return to Scheme. XXX: should pass CR2
    ;;(mov rbx (mem+ rsp #x08))         ;rbx=saved rip

    ;; RCX->RIP, R11->RFLAGS, IA32_STAR[63:48]+16->CS,
    ;; IA32_STAR[63:48]+8->SS.
    ;;(mov rcx PF-raise)
    (mov ecx invalid-address)
    (mov r11 (mem+ rsp #x18))         ;saved rflags
    (and r11d ,(bitwise-ior (RFLAGS IF)))
    (or r11d ,(bitwise-ior (RFLAGS-IOPL 3)
                           (RFLAGS AC)))
    (mov rsp (mem+ rsp #x20))         ;switch stack
    (sysretq)                         ;return to CPL=3

    (%label PF-serious-error)
    ;; The operating system has fallen and it can't get up.
    ;; FIXME: system-wide panic. or maybe something less dramatic.
    (mov al #xf8) (out #x80 al)
    (%label PF-stop)
    (hlt)
    (jmp PF-stop)

;;; Stack-Segment Fault
    (%label fault-SS*)
    ;; As far as I can tell, this can only be the result of
    ;; referencing a non-canonical pointer with RBP as base
    ;; register.
    (mov rax (mem64+ rsp 8))
    (mov rsp (mem64+ rsp ,(* 8 4)))
    (push rax)
    (jmp noncanonical-address)

;;; General protection
    (%label fault-GP*)
    ;; Non-canonical address violation is the only one we're
    ;; interested in here. Anything else is likely a system failure.
    ;; FIXME: check what happened... check CPL of the pushed CS?
    (mov rax (mem64+ rsp 8))
    (mov rsp (mem64+ rsp ,(* 8 4)))
    (push rax)
    (jmp noncanonical-address)

;;; Alignment check
    ;; CPL=3, interrupts enabled.
    (%label fault-AC*)
    (mov rax (mem64+ rsp 8))
    (mov rsp (mem64+ rsp ,(* 8 4)))
    (push rax)
    (jmp alignment-check)

;;; Divide error
    ;; CPL=3, interrupts enabled.
    (%label fault-DE)
    (mov rax (mem64+ rsp))
    (mov rsp (mem64+ rsp ,(* 8 3)))
    (push rax)
    (jmp divide-error)

;;; Undefined opcode
    ;; CPL=3, interrupts enabled.
    (%label fault-UD)
    (mov rax (mem64+ rsp))
    (mov rsp (mem64+ rsp ,(* 8 3)))
    (push rax)
    (jmp undefined-opcode)

;;; IRQs and interrupts with no other handler

    (%label int-NMI)                  ;Non-Maskable Interrupt
    (iretq)                           ;iret enables NMI again

    ;; All other interrupts go here. CPL=0, interrupts disabled,
    ;; interrupt stack table 6.

    ;; It would be nice if this could be CPL=3, but unfortunately
    ;; that seems to raise #DF since HLT runs at CPL=0. Using AMD's
    ;; MWAIT this could be done at CPL=3.

    ;; RSP layout: interrupt-number RIP CS RFLAGS RSP SS

    ,@(make-generic-handlers "int-" 'int-generic)

    (%align 16)
    (%label int-generic)
    ,@(let ((iret '#(pc iret)))
        `((pop (mem64+ ,%cpu ,(pvec CPU-VECTOR:LAST-INTERRUPT-VECTOR))) ;save vector number
          (cmp (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-SP)) 0)
          (je ,iret)                    ;scheduler running, don't preempt
          (test (mem64+ rsp 16) ,(RFLAGS-IOPL 3))
          (jz ,iret)                    ;CPL=0, don't preempt

          ;; Preempt the current process.
          (push rbp)
          (mov rbp rsp)      ;remember rsp
          ;; RBP layout: RBP RIP CS RFLAGS RSP SS
          (mov rsp (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SAVE-AREA)))
          (add rsp ,PROCESS-SAVE-SIZE)

          ;; Push the IRETQ return frame, which is consumed by
          ;; interrupts:resume (and nothing else).
          (push (mem64+ rbp ,(* 8 5))) ;SS
          (push (mem64+ rbp ,(* 8 4))) ;RSP
          (push (mem64+ rbp ,(* 8 3))) ;RFLAGS
          (push (mem64+ rbp ,(* 8 2))) ;CS
          (push (mem64+ rbp ,(* 8 1))) ;RIP
          (push ,%pcb)                 ;save process-vector
          (sub rsp ,(* 16 3))          ;make space for movdqa
          (movdqa (mem128+ rsp ,(* 16 0)) xmm0) ;movdqa needs 16-byte alignment
          (movdqa (mem128+ rsp ,(* 16 1)) xmm1)
          (movdqa (mem128+ rsp ,(* 16 2)) xmm2)
          (sub rsp 8)
          (stmxcsr (mem32+ rsp))
          ,@(map (lambda (reg)
                   `(push ,(if (eq? reg 'rbp)
                               '(mem64+ rbp) ;fixup RBP
                               reg)))
                 all-registers)
          (push interrupts:resume)
          ;; (mov al ,(char->integer #\p))
          ;; (out #xe9 al)

          ;; RCX->RIP, R11->RFLAGS, IA32_STAR[63:48]+16->CS,
          ;; IA32_STAR[63:48]+8->SS.
          (mov rdi (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-SP)))
          (mov ecx switch-stack)
          (mov r11d ,(bitwise-ior (RFLAGS-IOPL 3)
                                  (RFLAGS AC)))
          (sar rdi ,(shift 'fixnum))
          (mov rax ,(immediate 'preempted))
          (sysretq)

          ;; It was decided against preempting. This ought to mean
          ;; that the scheduler is running and that it has issued a
          ;; HLT instruction (see syscall-hlt). The instruction
          ;; following the HLT is a CLI, but another interrupt can
          ;; happen immediately after IRETQ and before CLI if RFLAGS
          ;; is not modified here. TODO: use sysretq instead?
          (%label ,iret)
          ;; (push rax)
          ;; (mov al ,(char->integer #\h))
          ;; (out #xe9 al)
          ;; (pop rax)
          (and (mem64+ rsp 16) ,(fxnot (RFLAGS IF))) ;CLI
          (iretq)))

    ;; This is the reverse of the preempting in int-generic. It runs
    ;; in CPL=3 and is returned into from switch-stack.
    (%align 16)
    (%label interrupts:resume)
    ,@(map (lambda (reg) `(pop ,reg))
           (reverse all-registers))
    (ldmxcsr (mem32+ rsp))
    (add rsp 8)
    (movdqa xmm2 (mem128+ rsp ,(* 16 2)))
    (movdqa xmm1 (mem128+ rsp ,(* 16 1)))
    (movdqa xmm0 (mem128+ rsp ,(* 16 0)))
    (add rsp ,(* 16 3))
    (pop ,%pcb)                         ;restore process-vector
    (iretq)

;;; Usermode

    ;; Interrupt handlers for usermode. All interrupts in usermode
    ;; cause a return to the scheduler. CPL=0, interrupts are
    ;; disabled, the stack is on IST7. All other registers are from
    ;; the usermode process.

    (%label fault-usermode-PF*)
    (swapgs)
    (sal (mem64+ rsp) ,(shift 'fixnum))
    (pop (mem64+ gs ,(pvec CPU-VECTOR:LAST-INTERRUPT-CODE)))
    (mov (mem64+ gs ,(pvec CPU-VECTOR:LAST-INTERRUPT-VECTOR)) ,(immediate 14))
    (push rax)
    (mov rax cr2)
    (sal rax ,(shift 'fixnum))
    (mov (mem64+ gs ,(pvec CPU-VECTOR:LAST-FAULTING-ADDRESS)) rax)
    (pop rax)
    (jmp syscall-usermode-resume)

    ;; Generic handlers for faults with an error code
    ,@(apply append
             (map (lambda (vector-number label)
                    `((%label ,label)
                      (swapgs)
                      (sal (mem64+ rsp) ,(shift 'fixnum))
                      (pop (mem64+ gs ,(pvec CPU-VECTOR:LAST-INTERRUPT-CODE)))
                      (mov (mem64+ gs ,(pvec CPU-VECTOR:LAST-INTERRUPT-VECTOR))
                           ,(immediate vector-number))
                      (jmp syscall-usermode-resume)))
                  '(10 11 12 13 17)
                  '(fault-usermode-TS*
                    fault-usermode-NP*
                    fault-usermode-SS*
                    fault-usermode-GP*
                    fault-usermode-AC*)))

    ,@(make-generic-handlers "int-usermode-" 'int-usermode-generic)

    (%align 16)
    (%label int-usermode-generic)
    (swapgs)
    (pop (mem64+ gs ,(pvec CPU-VECTOR:LAST-INTERRUPT-VECTOR))) ;save vector number
    (jmp syscall-usermode-resume)))

(define (data)
  `(;; Default interrupt descriptor tables (for Scheme code)

    (%align 8 0)
    (%label idtr)
    (%u16 (- idt-end idt 1))
    (%u64 idt)

    (%align 8 0)
    (%label idt)
    (%u128 ,@(make-default-idt))
    (%label idt-end)

    ;; Interrupt descriptor table for when in usermode

    (%label idtr-usermode)
    (%u16 (- idt-usermode-end idt-usermode 1))
    (%u64 (bitwise-ior ,pc-segments:supervisor-addr idt-usermode))

    (%align 8 0)
    (%label idt-usermode)
    (%u128 ,@(make-usermode-idt))
    (%label idt-usermode-end))))
