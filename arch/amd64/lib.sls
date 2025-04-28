;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Boot loader-independent amd64 assembler runtime.

(library (loko arch amd64 lib)
  (export
    assemble
    text-start text text-end
    text-allocate-per-cpu-vector
    text-allocate-per-cpu-stack
    per-cpu-stack-size
    data-start data data-end)
  (import
    (only (loko arch amd64 memory)
          STACK-0-START STACK-0-SIZE
          HEAP-0-START HEAP-0-SIZE
          HEAP-MARGIN)
    (only (loko arch amd64 registers) RFLAGS MXCSR MXCSR-RC)
    (loko arch amd64 lib-gc)
    (loko arch amd64 lib-printer)
    (loko arch amd64 lib-stacks)
    (loko arch amd64 lib-traps)
    (loko arch amd64 lib-valgrind)
    (loko arch amd64 objects)
    (loko arch amd64 config)
    (loko runtime context)
    (loko config)
    (rnrs (6))
    (only (machine-code assembler x86) assemble)) ;reexported

(define NR-hlt -1)                      ;XXX: just here temporarily

(define (text-start)
  `((%section text)
    (%label text)
    (%mode 64)))

;; This stack is used to handle interrupts/signals that can happen
;; any time but should not ruin the process currently running. XXX:
;; At least 2048 on Linux, 8192 on NetBSD
(define per-cpu-stack-size 8192)      ;at least MINSIGSTKSZ
(define per-cpu-vector-size 128)      ;positive multiple of 64

(define (text-allocate-per-cpu-vector error-label)
  ;; This code is used in the bootloader-specific startup routine.
  ;; It returns a value in rax which should be loaded into the fs
  ;; segment base. The error-label is jumped to if the maximum
  ;; number of supported CPU's has been exceeded.
  `((mov ebx ,(immediate 1))
    (lock.xadd (mem+ cpu-counter) ebx) ;ebx = cpu number
    (cmp rbx ,(immediate (config-max-cpus)))
    (jnb ,error-label)                ;too many CPUs?
    (imul eax ebx ,(/ per-cpu-vector-size (immediate 1)))
    (add rax per-cpu-vector)
    (mov (mem64+ rax ,(immediate CPU-VECTOR:CPU-NUMBER)) rbx)
    (or rax ,(tag 'vector))))

(define (text-allocate-per-cpu-stack)
  ;; This code is used in the bootloader-specific startup routine.
  ;; It returns a stack pointer base in rax. It must be used after
  ;; text-allocate-per-cpu-vector.
  `((mov eax ,per-cpu-stack-size)
    (lock.xadd (mem64+ per-cpu-stack-offset) rax)
    (add rax per-cpu-stacks)))

(define (text)
  (define (pvec i) (+ (* 8 i) (- (tag 'vector))))

  `((%comm *debug-put-u8 16 16)        ;routine
    (%comm *panic 16 16)               ;routine
    (%comm boot-loader-type 16 16)     ;immsym
    (%comm boot-loader-data 16 16)     ;fixnum
    (%comm cpu-counter 8 8)

    ;; Per-CPU data. This is aligned to cache line size (assumed to
    ;; be 64) and each CPU owns a cache line. At start time the fs
    ;; segment base points to the start of a 64-bit area in this
    ;; vector.
    (%comm per-cpu-vector ,(* (config-max-cpus) per-cpu-vector-size) 64)
    ;; Stacks for e.g. timer interrupt/signal. XXX: not used by pc-*.
    (%comm per-cpu-stacks ,(* (config-max-cpus) per-cpu-stack-size) 16)
    (%comm per-cpu-stack-offset 8 8)

    ;; Should be removed when the runtime doesn't try to use them
    ;; under Linux.
    (%align 8)
    (%label $tmp-cli)
    (pushfq)
    (and (mem64+ rsp) ,(fxnot (RFLAGS IF)))
    (popfq)
    (ret)
    (%label $tmp-sti)
    (pushfq)
    (or (mem64+ rsp) ,(RFLAGS IF))
    (popfq)
    (ret)

    ;;;
    ;;; Allocates lists for optional arguments
    ;;;
    (%align 8)
    (%label consargs values)
    ,@(let ((gc '#(consargs gc))
            (gc-now '#(consargs gc-now))
            (gc-done '#(consargs gc-done))
            (cons-regs '#(consargs regs))
            (loop '#(consargs loop))
            (exit '#(consargs exit))
            (reg-label* (map (lambda (reg) `#(consargs ,reg)) %arg-reg*))
            (clear-label* (map (lambda (reg) `#(consargs-gc ,reg)) %arg-reg*)))
        ;; This is the code that implements rest argument lists.
        ;; Input:     rax      Number of arguments passed (negative)
        ;;            r10      Number of fixed arguments (positive) that
        ;;                       will not be consed
        ;; Output:    r12      Newly allocated list
        ;; Variable:  rbx      Pointer to the next item from the stack
        ;;            r10      Scratch register
        ;;            r11      Arguments left to cons up
        ;;            r12      The list being constructed
        ;; Preserved: rax rdi rsi rdx rcx r8 r9 r15 rbx rbp
        `((lea r11 (mem+ rax r10))    ;=-argcount+fixedargs
          (add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)) r11)
          (add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)) r11)
          (js ,gc)                    ;get more heap

          ;; There is now space for the list.
          (%label ,gc-done)
          (push rbx)                  ;get another temporary register
          (neg r11)                   ;=remaining conses
          (mov r12d ,(immediate '()))
          (lea rbx (mem+ rsp 8 8 ,(immediate (length %arg-reg*))))
          (push r10)                  ;TODO: this is silly
          (test r11 r11)
          (jz ,exit)

          ;; Cons arguments from the stack.
          (%label ,loop)
          (mov r10 (mem64+ rsp))      ;fixed
          (add r10 r11)               ;fixed+remaining
          (cmp r10 ,(immediate (length %arg-reg*)))
          (jle ,cons-regs)            ;nothing left on the stack?
          (mov r10 (mem64+ rbx))
          (mov r13 (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
          (mov (mem64+ r13 ,(car-offset)) r10)
          (mov (mem64+ r13 ,(cdr-offset)) r12)
          (lea r12 (mem+ r13 ,(tag 'pair))) ;set! r12 (cons r10 r12)
          (add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) 16)
          (add rbx 8)
          (sub r11 8)
          (jnz ,loop)
          (jmp ,exit)

          ;; And now cons the register-passed arguments. Some kind
          ;; of Duff's device. KISS for now. This code finds which
          ;; register to start the consing from.
          (%label ,cons-regs)
          (test r11 r11)
          (jz ,exit)
          (mov r10 (mem64+ rsp))
          (add r10 r11)               ;fixed+remaining
          ,@(let lp ((label* reg-label*) (i 1))
              (if (null? label*)
                  '()
                  `((cmp r10 ,(immediate i))
                    (je ,(car label*))
                    ,@(lp (cdr label*) (fx+ i 1)))))
          (jmp ,exit)
          ;; The generated code conses up to "r11" arguments from
          ;; the registers.
          ,@(let lp ((reg* (reverse %arg-reg*))
                     (label* (reverse reg-label*)))
              (if (null? reg*)
                  '()
                  `((%label ,(car label*))
                    (mov r10 (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
                    (mov (mem64+ r10 ,(car-offset)) ,(car reg*))
                    (mov (mem64+ r10 ,(cdr-offset)) r12)
                    (lea r12 (mem+ r10 ,(tag 'pair)))
                    (add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) 16)
                    ,@(if (null? (cdr reg*))
                          '()
                          `((sub r11 8)
                            (jz ,exit)
                            ,@(lp (cdr reg*) (cdr label*)))))))

          (%label ,exit)
          (pop r10)
          (pop rbx)
          (ret)

          ;; Prepare to call the GC. First clear the dead argument
          ;; registers.
          (%label ,gc)
          ,@(let lp ((label* clear-label*) (i 0))
              (if (null? label*)
                  '()
                  `((cmp rax ,(immediate (fx- i))) ;rax negative
                    (je ,(car label*))
                    ,@(lp (cdr label*) (fx+ i 1)))))
          (jmp ,gc-now)
          ,@(let lp ((reg* %arg-reg*) (label* clear-label*))
              (if (null? reg*)
                  '()
                  `((%label ,(car label*))
                    (xor ,(car reg*) ,(car reg*))
                    ,@(if (null? (cdr reg*))
                          '()
                          (lp (cdr reg*) (cdr label*))))))

          (%label ,gc-now)
          ,@(let ((saved `(rax r10 r11 ,@%arg-reg*))
                  (clean-loop '#(consargs clean loop))
                  (clean-done '#(consargs clean done)))
              `(,@(map (lambda (reg) `(push ,reg)) saved)
                ;; Clean up our caller's stack frame. They made room
                ;; for as many arguments were passed, but some
                ;; arguments are passed in registers, so the stack
                ;; slots where those values would've gone need to be
                ;; cleaned up. (They could've just made room for as
                ;; many slots as are needed, but that's more
                ;; complicated code in a place that's inlined a lot).
                (lea rdi (mem+ rsp 8 ,(* 8 (length saved)))) ;ptr to first slot to clean up
                (mov rcx rax)
                (neg rcx)               ;rcx=argcount
                (cmp rcx ,(immediate (length %arg-reg*)))
                (mov edx ,(immediate (length %arg-reg*)))
                (cmova rcx rdx)         ;rcx=min(argcount,len(argreg))
                (test rcx rcx)
                (jz ,clean-done)
                (xor edx edx)
                (%label ,clean-loop)
                (mov (mem64+ rdi) rdx)
                (add rdi 8)
                (sub rcx 8)
                (jne ,clean-loop)       ;loop until rcx<0
                (%label ,clean-done)

                ;; Here rdi will point to the return address in the
                ;; caller's frame, not the return address of the
                ;; caller. This is because everything in the caller's
                ;; frame currently is live.
                (lea rdi (mem+ rsp 8 8 ,(* 8 (length saved)))) ;adjusted in stop-and-copy
                (sub rdi rax)
                (call stop-and-copy)
                ,@(map (lambda (reg) `(pop ,reg)) (reverse saved))))
          (jmp ,gc-done)))

    ;;;
    ;;; Unpacks a list of values to multiple return values
    ;;;
    (%align 8)
    (%label values scheme-init)
    ;; * rdi is a list of values (six or more)
    (mov r11 rdi)
    (mov ,(list-ref %ret-reg* 0) (mem64+ r11 ,(fx- (car-offset) (tag 'pair)))) ;car
    (mov r11                     (mem64+ r11 ,(fx- (cdr-offset) (tag 'pair)))) ;cdr
    (mov ,(list-ref %ret-reg* 1) (mem64+ r11 ,(fx- (car-offset) (tag 'pair))))
    (mov r11                     (mem64+ r11 ,(fx- (cdr-offset) (tag 'pair))))
    (mov ,(list-ref %ret-reg* 2) (mem64+ r11 ,(fx- (car-offset) (tag 'pair))))
    (mov r11                     (mem64+ r11 ,(fx- (cdr-offset) (tag 'pair))))
    (mov ,(list-ref %ret-reg* 3) (mem64+ r11 ,(fx- (car-offset) (tag 'pair))))
    (mov r11                     (mem64+ r11 ,(fx- (cdr-offset) (tag 'pair))))
    (mov ,(list-ref %ret-reg* 4) (mem64+ r11 ,(fx- (car-offset) (tag 'pair))))
    (mov r11                     (mem64+ r11 ,(fx- (cdr-offset) (tag 'pair))))
    (mov ,(list-ref %ret-reg* 5) (mem64+ r11 ,(fx- (car-offset) (tag 'pair))))
    (mov r11                     (mem64+ r11 ,(fx- (cdr-offset) (tag 'pair))))
    (mov r10 ,(immediate -6))
    ,@(let ((loop '#(values loop))
            (done '#(values done)))
        ;; Place any additional values on the stack
        `((%label ,loop)
          (cmp r11d ,(immediate '()))
          (je ,done)
          (mov rax (mem64+ r11 ,(fx- (car-offset) (tag 'pair)))) ;car
          (mov r11 (mem64+ r11 ,(fx- (cdr-offset) (tag 'pair)))) ;cdr
          (mov (mem64+ rsp r10 ,(immediate 6) -8) rax)
          (sub r10 ,(immediate 1))
          (jmp ,loop)
          (%label ,done)))
    (mov rax ,(car %ret-reg*))
    (stc)
    (ret)

    ;;;
    ;;; Setup the process environment and start Scheme.
    ;;;
    (%align 8)
    (%label scheme-init process-init)
    ;; Jumped to by linux:start, netbsd:start or multiboot:start.

    ;; At this point heap and stack area 0 are mapped into VM. Switch
    ;; to area #0.

    ,@(let ((stop '#(scheme-init stop))
            (cont '#(scheme-init cont)))
        ;; TODO: Only CPU 0 has a stack and heap...
        `((mov r10 (mem64+ ,%cpu ,(- (tag 'vector)) ,(* 8 CPU-VECTOR:CPU-NUMBER)))
          (test r10 r10)
          (jz ,cont)
          (%label ,stop)
          (mov rax ,NR-hlt)
          (syscall)
          (jmp ,stop)
          (%label ,cont)))

    (mov rsp ,(+ STACK-0-START STACK-0-SIZE (- PROCESS-SAVE-SIZE)))

    ;; Initialize and allocate the process-vector on the heap.
    (mov ,%pcb ,(+ HEAP-0-START (tag 'vector)))
    (mov rax initial-pcb-length)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:LENGTH)) rax)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ERROR-INVOKER)) (+ *panic ,(tag 'procedure)))
    (mov rdx ,HEAP-0-START)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) rdx)
    (mov rcx ,(- (div HEAP-0-SIZE 2) HEAP-MARGIN))
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)) rcx)
    (add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) rax)
    (sub (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)) rax)

    ;; Initialize the per-cpu-vector. CPU-VECTOR:CPU-NUMBER is
    ;; already set.
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:LENGTH)) ,(immediate (/ (- per-cpu-vector-size 8) 8)))
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:LAST-INTERRUPT-VECTOR)) ,(immediate #f))
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-RUNNING?)) ,(immediate #t)) ;scheduler running

    ;; Set memory management parameters.
    (mov rdx ,HEAP-0-START)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-CURRENT-HEAP)) rdx)
    (mov ecx ,(div HEAP-0-SIZE 2))
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)) rcx)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)) rcx)
    (add rdx rcx)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-OTHER-HEAP)) rdx)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:STACK-TOP)) rsp)
    (mov edx ,(- (- STACK-0-SIZE PROCESS-SAVE-SIZE) 4096))
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:STACK-SIZE)) rdx)

    ;; Start with zeroed registers
    ,@(map (lambda (r) `(xor ,r ,r))
           '(eax ecx edx ebx ebp esi edi r8d r9d r10d r11d r12d r13d #;r14d r15d))
    (ldmxcsr (mem32+ mxcsr-default))
    ,@(map (lambda (r) `(xorpd ,r ,r))
           '(xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15))
    ;; (vzeroall)
    (call scheme-start)
    (mov rdi ,(immediate 'start))
    (jmp (mem64+ *panic))

    ;;;
    ;;; The entry point for newly created processes.
    ;;;
    (%align 8)
    (%label process-init)
    ;; This routine is called from a scheduler process that is
    ;; starting up a new process. RSP is already set. The stack is
    ;; created by $process-start.
    (mov rax ,(immediate 'scheme))
    (mov (mem64+ boot-loader-type) rax)
    (mov (mem64+ boot-loader-data) 0)
    (pop ,%pcb)
    ,@(map (lambda (r) `(xor ,r ,r))
           '(eax ecx edx ebx ebp esi edi r8d r9d r10d r11d r12d r13d #;r14d r15d))
    (ldmxcsr (mem32+ mxcsr-default))
    ,@(map (lambda (r) `(xorpd ,r ,r))
           '(xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15))
    ;; (vzeroall)
    (popfq)                           ;also enables interrupts
    (mov (mem64+ ,%cpu ,(pvec CPU-VECTOR:SCHEDULER-RUNNING?)) ,(immediate #f)) ;not currently yielding
    (ret)

;;; Other libraries

    ,@(lib-gc:text)
    ,@(lib-printer:text)
    ,@(lib-stacks:text)
    ,@(lib-traps:text)
    ,@(lib-valgrind:text)))

(define (text-end)
  `((%label text-end)))

(define (data-start)
  `((%align 4096 0)
    (%label data)
    (%section data)))

(define (data)
  `(,@(lib-gc:data)
    ,@(lib-printer:data)
    ,@(lib-stacks:data)
    ,@(lib-traps:data)
    ,@(lib-valgrind:data)
    ;; Floating point stuff
    (%label c_231)                 ;Hackers Delight, table 17-2, c_231
    (%u32 #x4B400000)                   ;2²³+2²²
    (%label c_23)
    (%u32 #x4B000000)                   ;2²³
    (%label mxcsr-default)
    (%u32 ,(MXCSR PM UM OM ZM DM IM))
    (%label mxcsr-round)                ;round to nearest (even)
    (%u32 ,(fxior (MXCSR PM UM OM ZM DM IM) (MXCSR-RC #b00)))
    (%label mxcsr-floor)                ;round down, toward -∞
    (%u32 ,(fxior (MXCSR PM UM OM ZM DM IM) (MXCSR-RC #b01)))
    (%label mxcsr-ceiling)             ;round up, toward +∞
    (%u32 ,(fxior (MXCSR PM UM OM ZM DM IM) (MXCSR-RC #b10)))
    (%label mxcsr-truncate)             ;round toward zero, truncate
    (%u32 ,(fxior (MXCSR PM UM OM ZM DM IM) (MXCSR-RC #b11)))
    ;; Panic message
    (%label panic-msg0)
    (%utf8z "\x1b;[0;1;31mLoko Scheme panic: \x1b;[33m")
    (%label panic-msg0-end)
    (%label panic-msg1)
    (%utf8z "\x1b;[0m\r\n")
    (%label panic-msg1-end)))

(define (data-end)
  `((%label data-end)
    ;; Put uninitialized data here
    (%align 4096 0)
    (%label bss)
    (%section bss)
    (%align 4 0)
    (%label bss-end))))
