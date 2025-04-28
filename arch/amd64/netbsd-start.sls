;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Loko assembler runtime for NetBSD/amd64

(library (loko arch amd64 netbsd-start)
  (export
    text data)
  (import
    (loko arch amd64 config)
    (loko arch amd64 netbsd-numbers)
    (loko arch amd64 registers)
    (loko arch amd64 objects)
    (loko runtime context)
    (prefix (loko arch amd64 lib) lib:)
    (loko config)
    (rnrs))

(define (asm-syscall syscall-number . args)
  (let lp ((reg64* '(rdi rsi rdx r10 r8 r9))
           (reg32* '(edi esi edx r10d r8d r9d))
           (arg* args)
           (code `((mov eax ,syscall-number)
                   (syscall))))
    (cond ((null? arg*)
           code)
          ((null? reg64*)
           (error 'syscall "Too many arguments" syscall-number args))
          (else
           (let ((arg (car arg*)))
             (lp (cdr reg64*)
                 (cdr reg32*)
                 (cdr arg*)
                 (cons (cond
                         ;; XXX: The peephole optimizer doesn't touch this code.
                         ((eqv? arg 0)
                          `(xor ,(car reg32*) ,(car reg32*)))
                         ((and (fixnum? arg) (fx<=? arg #xffffffff))
                          `(mov ,(car reg32*) ,arg))
                         (else
                          `(mov ,(car reg64*) ,arg)))
                       code)))))))

;;; .text

(define (text)
  (define (pvec i) (+ (* 8 i) (- (tag 'vector))))
  `(
    ;;;
    ;;; NetBSD-specific initialization
    ;;;
    (%label netbsd-start)
    ;; #AC is already enabled and boot-loader-data has been set.
    (mov (mem64+ *debug-put-u8) netbsd:debug-put-u8)
    (mov (mem64+ *panic) netbsd:panic)
    (mov (mem64+ boot-loader-type) ,(immediate 'netbsd))

    ;; NetBSD/amd64 9.0 leaves some registers at non-zero values
    (xor ebx ebx)
    (xor r8l r8l)
    (xor r9l r9l)
    (xor r10l r10l)
    (xor r11l r11l)
    (xor r12l r12l)
    (xor r13l r13l)
    (xor r14l r14l)
    (xor r15l r15l)

    ;; Setup FS
    ,@(lib:text-allocate-per-cpu-vector 'netbsd:panic)
    (push rax)
    ,@(asm-syscall SYS_sysarch X86_64_SET_FSBASE 'rsp)

    ;; Alternate signal stack, so the kernel doesn't smash our stack
    ,@(lib:text-allocate-per-cpu-stack)
    (sal rax ,(shift 'fixnum))
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:ALTSIGSTK-BASE)) rax)
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:ALTSIGSTK-SIZE)) ,(immediate lib:per-cpu-stack-size))

    (jmp scheme-init)

    ;;;
    ;;; Returns from a signal handler (given in sigaction). This
    ;;; becomes the return address in signal handler stack frames.
    ;;;
    (%align 8)
    (%label __sigtramp_siginfo_2)
    ;; This can only be done when ONSTACK has been used, because
    ;; otherwise the Scheme stack has been smashed and returning is
    ;; meaningless.
    ,@(asm-syscall SYS_setcontext 'r15)
    (jmp netbsd:panic)

    ;;;
    ;;; Scheme code makes this the signal handler for everything
    ;;; that is a runtime error (type errors mainly).
    ;;;
    (%align 8)
    (%label netbsd:signal-handler)
    ;; The arguments here are as follows: int signo, siginfo_t *,
    ;; ucontext_t *. They are passed in rdi, rsi and rdx.
    ;; This needs to restore the original rsp because otherwise there
    ;; will be junk on the stack which the GC will choke on.
    (mov r15 (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_R15))))
    (mov r14 (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_R14))))
    (mov r13 (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_R13))))
    (mov rbx (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_RBX))))
    (mov rbp (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_RBP))))
    (mov eax (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_TRAPNO))))
    (mov rcx (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_RIP))))
    (mov rsp (mem+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_RSP))))
    (push (mem64+ rdx ,(+ offsetof-ucontext_t-uc_mcontext offsetof-mcontext_t-__gregs (* 8 _REG_RFLAGS))))
    (popfq)
    (push rcx)                        ;RIP
    ,@(map (lambda (r) `(xor ,r ,r))  ;who knows what the kernel put here
           '(ecx edx esi edi r8d r9d r10d r11d r12d))
    (and eax ,(bitwise-not TC_FLAGMASK))
    (cmp eax ,T_DIVIDE)               ;#DE
    (je divide-error)
    (cmp eax ,T_STKFLT)               ;#SS
    (je noncanonical-address)
    (cmp eax ,T_PROTFLT)              ;#GP
    (je noncanonical-address)
    ;; TODO: #PF has several types of causes
    (cmp eax ,T_PAGEFLT)              ;#PF
    (je invalid-address)
    (cmp eax ,T_ALIGNFLT)             ;#AC
    (je alignment-check)
    (cmp eax ,T_TRCTRAP)              ;#DB
    (je debug-exception)
    (cmp eax ,T_BPTFLT)               ;#BP
    (je breakpoint)
    (cmp eax ,T_PRIVINFLT)            ;#UD
    (je undefined-opcode)
    ;; Some more generic error invoker is needed.
    (jmp (mem64+ *panic))

    ;;;
    ;;; The preemption timer has fired. It's time for the current
    ;;; process to scurry away and let the scheduler run. SIGURG is
    ;;; masked when the scheduler runs.
    ;;;
    (%align 8)
    (%label netbsd:preempt)
    (jmp netbsd:panic)                  ;TODO: not ported yet
    ,@(let ((preempt (vector 'preempt))
            (exit (vector 'exit)))
        `((mov rbp (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-SP)))
          (test rbp rbp)
          (jz ,exit)                  ;is the scheduler already running?
          (mov eax ,(immediate #f))
          (mov edx ,(immediate #t))
          (cmpxchg (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-RUNNING?)) rdx)
          (je ,preempt)                 ;not already yielding?
          (%label ,exit)
          (ret)
          (%label ,preempt)
          ;; Save the alternate stack in the process's save area. It
          ;; is important that the alternate stack + 8 can't be
          ;; bigger than the save area. DF is clear (AMD64 ABI).
          (mov rbx ,%pcb)
          (mov rdi (mem64+ rbx ,(pvec PROCESS-VECTOR:SAVE-AREA)))
          (mov rcx (mem64+ ,%cpu ,(pvec CPU-VECTOR:ALTSIGSTK-BASE))) ;alternate stack
          (add rcx (mem64+ ,%cpu ,(pvec CPU-VECTOR:ALTSIGSTK-SIZE)))
          (sar rcx ,(shift 'fixnum))
          (sub rcx rsp)               ;rcx = bytes to save
          (mov rsi rsp)               ;copy destination
          (shr rcx 3)                 ;rcx = quads to save
          ;; Build a stack frame for resuming the process
          (mov eax netbsd:resume)
          (stos (mem64+ rdi) rax)     ;push return address for scheduler
          (mov rax rbx)
          (stos (mem64+ rdi) rax)     ;save process vector
          (mov rax rcx)
          (stos (mem64+ rdi) rax)     ;push size of the state
          (mov rax rsp)
          (stos (mem64+ rdi) rax)     ;this stack pointer
          ;; Copy the state that Linux saved
          (rep.movs (mem64+ rdi) (mem64+ rsi))
          ;; Switch back to the scheduler.
          (mov rdi rbp)               ;scheduler's rsp
          (sar rdi ,(shift 'fixnum))
          (mov rsp (mem64+ rbx ,(pvec PROCESS-VECTOR:SAVE-AREA))) ;rsp for resume
          (mov rax ,(immediate 'preempted)) ;TODO: must be unforgeable
          (jmp switch-stack)))

    ;;;
    ;;; The reverse of netbsd:preempt. Returned into from switch-stack.
    ;;; Note: SIGURG must be masked.
    ;;;
    (%align 8)
    (%label netbsd:resume)
    (jmp netbsd:panic)                  ;TODO: not ported yet
    (pop ,%pcb)
    (pop rcx)                         ;quads to restore
    (pop rdi)                         ;copy destination
    (mov rsi rsp)                     ;copy source
    ;; Seems that the next instruction disables SIGURG delivery
    (mov rsp rdi)                     ;restore alternate stack pointer
    (rep.movs (mem64+ rdi) (mem64+ rsi)) ;restore sigreturn data
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-RUNNING?)) ,(immediate #f)) ;not currently yielding
    (ret)

    ;;; Debug output
    (%align 8)
    (%label netbsd:debug-put-u8 netbsd:panic)
    (push rdi)                        ;rdi = octet
    ,@(asm-syscall SYS_write STDERR_FILENO 'rsp 1)
    (pop rdi)
    (ret)

    ;;; Panic
    (%align 8)
    (%label netbsd:panic)
    (push rdi)
    ,@(asm-syscall SYS_write STDERR_FILENO 'panic-msg0
                   '(- panic-msg0-end panic-msg0 1))
    (pop rdi)
    (call debug-display)
    ,@(asm-syscall SYS_write STDERR_FILENO 'panic-msg1
                   '(- panic-msg1-end panic-msg1 1))
    ,@(asm-syscall SYS_exit EX_SOFTWARE)
    (jmp netbsd:panic)))

;;; .data

(define (data)
  `((%utf8z "This Scheme program runs on NetBSD/amd64"))))
