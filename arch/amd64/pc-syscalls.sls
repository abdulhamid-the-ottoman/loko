;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2021 G. Weinholt
#!r6rs

;;; System call interface using SYSCALL/SYSRET

;; These syscalls are only present to work around the fact that #AC
;; only works at CPL=3 and some instructions only work at CPL=0.

;; The return value should be in RAX and should fit in a fixnum, but
;; it should not actually be a fixnum. This is to make it compatible
;; with the Linux syscall interface which obviously does not return
;; fixnums.

;; Code that runs at CPL=0 with a usermode page table should use GS
;; instead of FS to locate the per-CPU data structure. It is set to
;; the same value as FS, but with an offset that makes the pages
;; accessible only to code running at CPL=0.

(library (loko arch amd64 pc-syscalls)
  (export
    text data
    text-initialize-syscalls
    ;; syscall numbers
    NR-hlt NR-rdmsr NR-wrmsr NR-usermode-run)
  (import
    (rnrs)
    (prefix (loko arch amd64 pc-segments) pc-segments:)
    (loko runtime context)
    (loko arch amd64 config)
    (loko arch amd64 objects)
    (loko arch amd64 registers)
    (loko arch amd64 pc-ustate))

;; The IA32_STAR register works like this:
;; On SYSRET: IA32_STAR[63:48]+16->CS(sel), IA32_STAR[63:48]+8->SS(sel).
;; On SYSCALL: IA32_STAR[47:32]->CS(sel), IA32_STAR[47:32]+8->SS(sel).
;; If the selectors are wrong then things will seem to work, until
;; they are reloaded.

;; Loko syscall numbers
(define NR-hlt -1)
(define NR-rdmsr -2)
(define NR-wrmsr -3)
(define NR-usermode-run -4)

(define (text-initialize-syscalls)
  `((mov ecx ,(MSR IA32_EFER))
    (rdmsr)
    (or eax ,(EFER SCE))
    (wrmsr)                     ;enable SYSCALL/SYSRET
    ;; Negated RFLAGS mask. Bits to clear.
    (mov ecx ,(MSR IA32_FMASK))
    (rdmsr)
    (mov eax ,(RFLAGS IF))
    (xor edx edx)
    (wrmsr)
    ;; Target RIP
    (mov ecx ,(MSR IA32_LSTAR))
    (mov eax syscall-vector)
    (mov edx (>> ,pc-segments:supervisor-addr 32))
    (wrmsr)
    ;; Segment selectors
    (mov ecx ,(MSR IA32_STAR))
    ;; (rdmsr)
    (xor eax eax)
    (mov edx ,(fxior (fxarithmetic-shift-left (- pc-segments:data-PL3 8) 16)
                     pc-segments:code-PL0))
    (wrmsr)))

(define (text)
  (define (pvec i) (+ (* 8 i) (- (tag 'vector))))
  (define syscall-error '#(syscall error))
  (define syscall-usermode-resume-2 '#(syscall umres2))
  `((%align 8)
    (%label syscall-vector)
    ;; Preserve RCX and R11 in order to do SYSRET.
    ;; RAX is used to pass in a syscall number.
    ;; RAX is also the return value.
    ;; Arguments passed in RDI RSI RDX R10 R8 R9.
    ;; There are no fixnums here, it's all integers.
    ;; Do NOT clobber the stack, it's a Scheme stack!

    (swapgs)
    (cmp (mem64+ gs ,(pvec CPU-VECTOR:USTATE)) 0)
    (jne syscall-usermode-vector)       ;usermode?

    (cmp rax ,NR-hlt)
    (je syscall-hlt)
    (cmp rax ,NR-rdmsr)
    (je syscall-rdmsr)
    (cmp rax ,NR-wrmsr)
    (je syscall-wrmsr)
    (cmp rax ,NR-usermode-run)
    (je syscall-usermode-run)

    ;; RCX->RIP, R11->RFLAGS, IA32_STAR[63:48]+16->CS,
    ;; IA32_STAR[63:48]+8->SS.

    ;; Returns to 64-bit CPL=3 code.
    (%label ,syscall-error)
    (xor eax eax)
    (dec rax)
    (swapgs)
    (sysretq)

    (%label syscall-usermode-vector)
    ;; This is a syscall from a usermode process; save the state and
    ;; return to the scheduler. CPL=0, interrupts are disabled, the
    ;; stack belongs to usermode, the usermode page table is active.
    ;; Arguments are in rax rdi rsi rdx r10 r8 r9. Saved rflags are in
    ;; r11, saved rip is in rcx.
    (mov (mem64+ gs ,(pvec CPU-VECTOR:LAST-INTERRUPT-VECTOR)) ,(immediate -1))
    (mov (mem64+ gs ,(pvec CPU-VECTOR:SCRATCH)) rsp)

    ;; Find the ustate and update the data for syscall-usermode-run.
    (mov rsp (mem64+ gs ,(pvec CPU-VECTOR:USTATE)))
    (sar rsp ,(shift 'fixnum))
    (mov (mem64+ rsp ,USTATE:RBP) rbp)
    (lea rbp (mem64+ rsp ,USTATE:RBP))  ;hacky
    (add rsp ,USTATE-IRET-TOP)

    ;; Save data for iretq
    (push ,pc-segments:data-PL3)                 ;SS
    (push (mem64+ gs ,(pvec CPU-VECTOR:SCRATCH))) ;RSP
    (push r11)                                   ;RFLAGS
    (push ,pc-segments:code-PL3)                 ;CS
    (push rcx)                                   ;RIP
    (mov (mem64+ gs ,(pvec CPU-VECTOR:SCRATCH)) 0)

    ;; Save the process state and switch back to the scheduler. For
    ;; reasons, RBP should point to RBP.
    (jmp ,syscall-usermode-resume-2)

;;; Enable interrupts and wait for an interrupt
    ;; should possibly be monitor/mwait instead
    (%align 8)
    (%label syscall-hlt)                ;caller has RFLAGS.IF=0
    (sti)                               ;briefly set RFLAGS.IF=1
    (hlt)                               ;int-generic runs after this
    (cli)                               ;set RFLAGS.IF=0 again
    (xor eax eax)
    (swapgs)
    (sysretq)

;;; Read an MSR
    (%align 8)
    (%label syscall-rdmsr)
    (mov r10 rcx)
    (mov ecx edi)
    (rdmsr)
    (mov (mem32+ rsi) eax)
    (mov (mem32+ rsi 4) edx)
    (xor eax eax)
    (mov rcx r10)
    (swapgs)
    (sysretq)

;;; Write an MSR
    (%align 8)
    (%label syscall-wrmsr)
    (mov r10 rcx)
    (mov ecx edi)
    (mov eax esi)
    ;;(mov edx edx)
    (wrmsr)
    (xor eax eax)
    (mov rcx r10)
    (swapgs)
    (sysretq)

;;; Run (or resume) a usermode process
    ;; Jumps to usermode. Will return from there via a syscall or an
    ;; interrupt (or a trap) that jumps to syscall-usermode-resume.
    (%align 8)
    (%label syscall-usermode-run)       ; rdi = ustate

    ;; Check that we're running from the scheduler and save the
    ;; scheduler's stack pointer.
    ;; (cmp (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-SP)) 0)
    ;; (jne ,syscall-error)
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-SP)) rsp)
    (sal (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-SP)) ,(shift 'fixnum))

    ;; Save all other registers not clobbered by the syscall. XXX:
    ;; Each CPU needs one of these areas so it doesn't really need to
    ;; live in the ustate.
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 0))) rbx)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 1))) rdx)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 2))) rsi)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 3))) rbp)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 4))) r8)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 5))) r9)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 6))) r10)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 7))) r12)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 8))) r13)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 9))) r14)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 10))) r15)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 11))) rcx)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 12))) r11)
    (mov (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 13))) rdi)

    (mov rsi ,pc-segments:supervisor-addr)
    (or rdi rsi)
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:USTATE)) rdi)
    (sal (mem64+ ,%cpu ,(pvec CPU-VECTOR:USTATE)) ,(shift 'fixnum))
    (lidt (mem+ idtr-usermode))
    (lea rsp (mem64+ rdi ,USTATE-IRET-BOTTOM))
    (mov rax (mem64+ rdi ,USTATE:CR3))
    (mov cr3 rax)                       ;switch to the usermode process page table
    (or rsp rsi)

    ;; Restore the user's FSBASE & GSBASE
    (mov rax (mem64+ rdi ,USTATE:FSBASE))
    (mov rdx rax)
    (sar rdx 32)
    (mov ecx ,(MSR IA32_FS_BASE))
    (wrmsr)
    (mov rax (mem64+ rdi ,USTATE:GSBASE))
    (mov rdx rax)
    (sar rdx 32)
    (mov ecx ,(MSR IA32_KERNEL_GS_BASE)) ;this becomes the user GS base
    (wrmsr)

    ;; Restore the FPU state
    (fxrstor (mem+ rdi ,USTATE:FXSAVE))

    ;; Restore GP registers
    ,@(map (lambda (reg) `(pop ,reg)) (reverse USTATE-GP-REGS))
    (swapgs)
    (iretq)                  ;comes back to -resume via interrupt or syscall

    (%align 8)
    (%label syscall-usermode-resume)
    ;; Usermode was preempted; jump back to the scheduler. CPL=0,
    ;; interrupts are disabled, GS and the stack belong to us. CR3 is
    ;; for usermode and all other registers are also from usermode.
    (push rbp)
    (mov rbp rsp)

    ;; RBP layout: RBP RIP CS RFLAGS RSP SS
    ;; Find the ustate and update the data for syscall-usermode-run.
    (mov rsp (mem64+ gs ,(pvec CPU-VECTOR:USTATE)))
    (sar rsp ,(shift 'fixnum))
    (add rsp ,USTATE-IRET-TOP)

    ;; Save the iretq data
    (push (mem64+ rbp ,(* 8 5))) ;SS
    (push (mem64+ rbp ,(* 8 4))) ;RSP
    (push (mem64+ rbp ,(* 8 3))) ;RFLAGS
    (push (mem64+ rbp ,(* 8 2))) ;CS
    (push (mem64+ rbp ,(* 8 1))) ;RIP

    ;; Usermode was preempted or yielded. Save the usermode register state.
    (%label ,syscall-usermode-resume-2)
    ,@(map (lambda (reg)
             `(push ,(if (eq? reg 'rbp) '(mem64+ rbp) reg)))
           USTATE-GP-REGS)

    (mov rax pml4)
    (mov cr3 rax)               ;restore the normal page table
    (lidt (mem+ idtr))          ;restore the normal interrupt handlers

    ;; No longer running from usermode
    (mov rdi (mem64+ gs ,(pvec CPU-VECTOR:USTATE)))
    (sar rdi ,(shift 'fixnum))
    (mov (mem64+ gs ,(pvec CPU-VECTOR:USTATE)) 0)

    ;; Save the usermode FSBASE and GSBASE
    (mov ecx ,(MSR IA32_FS_BASE))
    (rdmsr)
    (sal rdx 32)
    (or rax rdx)
    (mov (mem64+ rdi ,USTATE:FSBASE) rax)
    (mov ecx ,(MSR IA32_KERNEL_GS_BASE)) ;actually user GS base
    (rdmsr)
    (sal rdx 32)
    (or rax rdx)
    (mov (mem64+ rdi ,USTATE:GSBASE) rax)

    ;; Save the usermode FPU state and reset it
    (fxsave (mem+ rdi ,USTATE:FXSAVE))
    (fninit)
    (ldmxcsr (mem32+ mxcsr-default))

    ;; Restore FS for the scheduler.
    (mov ecx ,(MSR IA32_GS_BASE))
    (rdmsr)
    (mov ecx ,(MSR IA32_FS_BASE))
    (and edx ,(bitwise-and #xFFFFFFFF
                           (bitwise-not
                            (bitwise-bit-field pc-segments:supervisor-addr 32 64))))
    (wrmsr)

    ;; Return to the scheduler. Restore the stack and the GP registers.
    (mov rsp (mem64+ gs ,(pvec CPU-VECTOR:SCHEDULER-SP)))
    (sar rsp ,(shift 'fixnum))
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-SP)) 0)
    (xor rax rax)                       ;returned from sys_urun

    (mov rbx (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 0))))
    (mov rdx (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 1))))
    (mov rsi (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 2))))
    (mov rbp (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 3))))
    (mov r8 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 4))))
    (mov r9 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 5))))
    (mov r10 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 6))))
    (mov r12 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 7))))
    (mov r13 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 8))))
    (mov r14 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 9))))
    (mov r15 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 10))))
    (mov rcx (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 11)))) ;rip
    (mov r11 (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 12)))) ;rflags
    (mov rdi (mem64+ rdi ,(+ USTATE-SAVE-AREA (* 8 13))))

    (swapgs)
    (sysretq)))

(define (data)
  '()))
