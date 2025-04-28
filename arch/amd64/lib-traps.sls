;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Trap/error handling

(library (loko arch amd64 lib-traps)
  (export
    lib-traps:text lib-traps:data)
  (import
    (loko arch amd64 config)
    (loko arch amd64 objects)
    (loko runtime context)
    (rnrs (6)))

;; TODO: these handlers should detect double faults, i.e.
;; detect when an error handler causes an error in itself

;; TODO: these handlers should detect when there's an error during GC

(define (lib-traps:text)
  (define (pvec i) (+ (* 8 i) (- (tag 'vector))))
  (define (invoke-trap category)
    `((mov rax ,(immediate category))
      (jmp invoke-trap)))
  (define %arg-reg* '(rdi rsi rdx rcx r8 r9))
  (define unwind-done '#(trap done))

  ;; Invoked by fault handlers
  `((%align 8)
    (%label invoke-trap alignment-check)
    ;; The program gets here if it tries to do fx+ on bad
    ;; types, or take the cdr of a fixnum, or call a
    ;; non-procedure object, etc. There are a few cases:
    ;; 1. The trap is taken during the normal execution
    ;;    of a procedure.
    ;; 1a. If this is the scheduler process, then the current
    ;;     stack frame must be unwound.
    ;; 1b. If this is a regular process then ask the scheduler
    ;;     to clean up the current stack frame.
    ;; 2. If the trapping instruction is an indirect jump
    ;;    then the current stack frame is empty (tail calls).
    ;; XXX: all live registers should've been saved for inspection.

    ;; On entry to this routine the RIP of the trapping instruction
    ;; can be found at (mem64+ rsp).

    ;; 200AD5: 41FF67FD                       (jmp (mem64+ r15 #x-3))
    (mov rdx (mem64+ rsp))        ;rdx is rip of trapped instruction
    (movzx ecx (mem8+ rdx))
    (shl ecx 16)
    (mov ch (mem8+ rdx 1))
    (mov cl (mem8+ rdx 2))
    (sal rdx ,(shift 'fixnum))
    (cmp ecx #x41FF67)
    (je ,unwind-done)                 ;skip unwind if indirect jmp

    ;; Unwind the caller's stack frame. The table is a vector
    ;; with entries of the form ...,low,high,locals,... The
    ;; job is to find the entry where low≤rdx≤high. Must not
    ;; clobber rdx. TODO: it would be excellent if this was
    ;; not needed.
    ,@(let ((next-entry '#(trap next))
            (check-entry '#(trap check)))
        `((mov rsi bootstrap-unwind-table)
          (mov rcx (mem64+ rsi))
          (add rsi 8)
          (%label ,check-entry)
          (test ecx ecx)
          (jz ,unwind-done)           ;not in the table
          (cmp rdx (mem64+ rsi))
          (jnae ,next-entry)
          (cmp rdx (mem64+ rsi 8))
          (jnbe ,next-entry)
          (mov rsi (mem64+ rsi 16))      ;found the entry!
          (lea rsp (mem+ rsp rsi 8))     ;unwind
          (jmp ,unwind-done)
          (%label ,next-entry)
          (add rsi ,(* 8 3))
          (sub rcx ,(* 8 3))
          (jmp ,check-entry)))

    (%label ,unwind-done)
    ;; ((error-invoker) category closure rip irritants)
    (mov ,(list-ref %arg-reg* 0) rax) ;category symbol
    (mov ,(list-ref %arg-reg* 1) r15)
    (mov ,(list-ref %arg-reg* 2) rdx)
    (mov ,(list-ref %arg-reg* 3) ,(immediate '()))
    (mov r15 (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ERROR-INVOKER)))
    (mov eax ,(immediate -4))                   ;argcount
    (jmp (mem64+ r15 ,(fx- (tag 'procedure))))

    (%align 8)
    (%label alignment-check invalid-address)
    ;; This label is called when an #AC has been generated. It is
    ;; called from the user program. Returning will return to the
    ;; program, and probably generate another #AC. A non-local
    ;; return must be used to recover, or the thread must die.
    ,@(invoke-trap 'alignment)

    (%align 8)
    (%label invalid-address access-violation)
    ;; This is called on #PF for references to missing pages. It is
    ;; also called when executing in non-executable pages (which
    ;; shouldn't happen). Can't return here either.
    ,@(invoke-trap 'page-fault)

    (%align 8)
    (%label access-violation noncanonical-address)
    ;; This is called on #PF when writing to read-only pages.
    ,@(invoke-trap 'accvio)

    (%align 8)
    (%label noncanonical-address divide-error)
    ;; This is called on #GP when using a noncanonical address. On
    ;; Linux this can also be called when doing a syscall wrong.
    ,@(invoke-trap 'noncanonical)

    (%align 8)
    (%label divide-error debug-exception)
    ;; Called on division by zero (and some other things?).
    ,@(invoke-trap 'divide)

    (%align 8)
    (%label debug-exception breakpoint)
    ;; Currently unused
    ,@(invoke-trap 'debug)

    (%align 8)
    (%label breakpoint undefined-opcode)
    ;; Currently unused
    ,@(invoke-trap 'breakpoint)

    (%align 8)
    (%label undefined-opcode heap-overflow)
    ;; Various explicitly coded assertion and error checks
    ;; (using the the ud2 instruction).
    ,@(invoke-trap 'undefined)

    ;;; Called explicitly
    (%align 8)
    (%label heap-overflow bad-mv-let)
    ;; This is called when the current program will not fit in
    ;; the stop and copy heap.
    (mov ,(list-ref %arg-reg* 3) rdi)   ;allocation size
    (mov rax ,(immediate 'memory))
    (mov ,(list-ref %arg-reg* 0) rax) ;category
    (mov ,(list-ref %arg-reg* 1) r15) ;closure
    (mov rax (mem64+ rsp))
    (sal rax ,(shift 'fixnum))
    (mov ,(list-ref %arg-reg* 2) rax) ;RIP
    (mov r15 (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ERROR-INVOKER)))
    (mov eax ,(immediate -4))                   ;argcount
    (jmp (mem64+ r15 ,(fx- (tag 'procedure))))

    (%align 8)
    (%label bad-mv-let formals-mismatch)
    ;; This is called from inside a procedure immediately after
    ;; learning that the wrong number of arguments was returned into
    ;; an mv-let.

    ;; ((error-invoker) category closure rip irritants)
    (mov rax ,(immediate 'mv-let))
    (mov ,(list-ref %arg-reg* 0) rax) ;category
    (mov ,(list-ref %arg-reg* 1) r15) ;closure
    (mov rax (mem64+ rsp))
    (sal rax ,(shift 'fixnum))
    (mov ,(list-ref %arg-reg* 2) rax) ;RIP
    (mov ,(list-ref %arg-reg* 3) ,(immediate #f))
    (mov r15 (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ERROR-INVOKER)))
    (mov eax ,(immediate -4))                   ;argcount
    (jmp (mem64+ r15 ,(fx- (tag 'procedure))))

    (%align 8)
    (%label formals-mismatch)
    ;; This is called when none of the cases in a case-lambda
    ;; match the number of arguments that were passed. There
    ;; is not yet any local stack frame.
    (xor r10d r10d)               ;no fixed arguments
    (cdqe)
    (add rsp rax)                 ;rax negative!
    (call consargs)               ;all args to r12
    (sub rsp rax)
    ;; ((error-invoker) category closure rip irritants)
    (mov rax ,(immediate 'formals))
    (mov ,(list-ref %arg-reg* 0) rax) ;category
    (mov ,(list-ref %arg-reg* 1) r15) ;closure
    (mov rax (mem64+ rsp))
    (sal rax ,(shift 'fixnum))
    (mov ,(list-ref %arg-reg* 2) rax) ;RIP
    (mov ,(list-ref %arg-reg* 3) r12) ;consed arguments
    (mov r15 (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ERROR-INVOKER)))
    (mov eax ,(immediate -4))                   ;argcount
    (jmp (mem64+ r15 ,(fx- (tag 'procedure))))))

(define (lib-traps:data)
  '()))
