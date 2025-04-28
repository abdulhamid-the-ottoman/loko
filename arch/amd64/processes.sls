;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Process control

(library (loko arch amd64 processes)
  (export
    $process-start)
  (import
    (rnrs (6))
    (loko arch amd64 memory)
    (loko arch amd64 tagging)
    (loko runtime context)
    (only (loko) machine-type)
    (loko system unsafe)
    (loko system $primitives)
    (except (loko system $host) $process-start)
    (srfi :98 os-environment-variables))

(define ($process-start area)
  ;; TODO: most of this should be replaced with a real virtual
  ;; memory address allocator.
  (define K 1024)
  (define M (* 1024 K))
  (define G (* 1024 M))
  (define T (* 1024 G))
  (define MARK-AND-SWEEP-TOP (* 1 T))
  (define STACK-SIZE (* 128 M))
  (define HUGE-PAGE (* 2 M))
  (define HEAP-SIZE
    (let ((heap-size
           (cond ((get-environment-variable "LOKO_HEAP") =>
                  (lambda (megs)
                    (* M (string->number megs))))
                 ((eq? (vector-ref (machine-type) 1) 'pc)
                  (* 256 M))
                 (else
                  (* 1024 M)))))
      ;; If it's a larger allocation then align it to a multiple of HUGE-PAGE.
      (if (> heap-size HUGE-PAGE)
          (fxand (fx+ heap-size (fx- HUGE-PAGE 1)) (fx- HUGE-PAGE))
          heap-size)))
  (define VIRTUAL-ADDRESS-TOP (expt 2 (- 48 1)))
  (define (stack-area n)
    (assert (and (fixnum? n) (not (fxnegative? n))))
    (let ((start (fx+ MARK-AND-SWEEP-TOP (fx* n (fx+ HEAP-SIZE STACK-SIZE)))))
      (when (fx>? (fx+ start (fx+ HEAP-SIZE STACK-SIZE)) VIRTUAL-ADDRESS-TOP)
        (error 'stack-area "There is no stack area with this number" n))
      start))
  (define (heap-area n)
    (assert (and (fixnum? n) (not (fxnegative? n))))
    (let ((start (fx+ (fx+ MARK-AND-SWEEP-TOP STACK-SIZE) (fx* n (fx+ HEAP-SIZE STACK-SIZE)))))
      (when (fx>? (fx+ start (fx+ HEAP-SIZE STACK-SIZE)) VIRTUAL-ADDRESS-TOP)
        (error 'heap-area "There is no heap area with this number" n))
      start))
  (define PROCESS-SAVE-SIZE 4096)
  (assert (fx>=? area 1))
  (let ((start-current-heap (heap-area area))
        (size-current-heap (fxdiv HEAP-SIZE 2))
        (start-other-heap (fx+ (heap-area area) (fxdiv HEAP-SIZE 2)))
        (size-other-heap (fxdiv HEAP-SIZE 2))
        (stack-bottom (fx+ (stack-area area) 4096))
        (stack-top (fx+ (stack-area area) (fx- STACK-SIZE PROCESS-SAVE-SIZE)))
        (stack-size (fx- (fx- STACK-SIZE PROCESS-SAVE-SIZE) 4096)))
    ($mmap stack-bottom (+ stack-size PROCESS-SAVE-SIZE) 'stack)
    ($mmap (heap-area area) HEAP-SIZE 'heap)
    (let ((rflags (bitwise-ior (expt 2 18)  ;Alignment check
                               (expt 2 9))) ;Interrupt flag
          (alloc start-current-heap)
          (heap-rem (fx- size-current-heap HEAP-MARGIN))
          (start ($linker-address 'scheme-start))
          (rip ($linker-address 'process-init)))

      ;; Allocate a process vector.
      (let* ((veclen ($pcb-ref PROCESS-VECTOR:LENGTH))
             (pcb (fx+ alloc (tag 'vector)))
             (veclen^ (fxand (fx+ (* 8 veclen) 15) -16))
             (alloc (fx+ alloc veclen^))
             (heap-remaining (fx- heap-rem veclen^))
             ;; FIXME: should probably be process-panic?
             (error-invoker (fx+ ($linker-address '*panic) (tag 'procedure))))
        (unless (fx>? heap-remaining HEAP-MARGIN)
          (error '$process-start "The heap is too small" heap-remaining))
        (letrec ([put (lambda (pcb idx val)
                        (put-mem-s61 (fx+ pcb (+ (* 8 idx) (- (tag 'vector)))) val))])
          (put pcb PROCESS-VECTOR:LENGTH (fxarithmetic-shift-left veclen (shift 'fixnum)))
          (put pcb PROCESS-VECTOR:ERROR-INVOKER error-invoker)
          (put pcb PROCESS-VECTOR:START-CURRENT-HEAP start-current-heap)
          (put pcb PROCESS-VECTOR:SIZE-CURRENT-HEAP size-current-heap)
          (put pcb PROCESS-VECTOR:START-OTHER-HEAP start-other-heap)
          (put pcb PROCESS-VECTOR:SIZE-OTHER-HEAP size-other-heap)
          (put pcb PROCESS-VECTOR:STACK-TOP stack-top)
          (put pcb PROCESS-VECTOR:STACK-SIZE stack-size)
          (put pcb PROCESS-VECTOR:HEAP-REMAINING heap-remaining) ;FIXME: what about the margin?
          (put pcb PROCESS-VECTOR:ALLOCATION-POINTER alloc))

        ;; Fill in the stack frame that will be popped by process-init.
        (let* ((sp (fx+ (stack-area area) (fx- STACK-SIZE PROCESS-SAVE-SIZE)))
               ;; The routine called to start up the Scheme code
               (sp (fx- sp 8)) (_ (put-mem-s61 sp start))
               ;; Must have RFLAGS to enable #AC and interrupts
               ;; (relevant if running directly on the hardware).
               (sp (fx- sp 8)) (_ (put-mem-s61 sp rflags))
               ;; Process vector (pcb)
               (sp (fx- sp 8)) (_ (put-mem-s61 sp pcb))
               ;; Process entry point
               (sp (fx- sp 8)) (_ (put-mem-s61 sp rip)))
          sp))))))
