;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2021 G. Weinholt
#!r6rs

;;; Execution context

(library (loko runtime context)
  (export
    CPU-VECTOR-SIZE
    CPU-VECTOR:LENGTH
    CPU-VECTOR:SCHEDULER-SP
    CPU-VECTOR:ALTSIGSTK-BASE
    CPU-VECTOR:ALTSIGSTK-SIZE
    CPU-VECTOR:LAST-INTERRUPT-VECTOR
    CPU-VECTOR:BOOT-LOADER-TYPE
    CPU-VECTOR:BOOT-LOADER-DATA
    CPU-VECTOR:SCHEDULER-RUNNING?
    CPU-VECTOR:CPU-NUMBER
    CPU-VECTOR:USTATE
    CPU-VECTOR:LAST-INTERRUPT-CODE
    CPU-VECTOR:LAST-FAULTING-ADDRESS
    CPU-VECTOR:SCRATCH
    PROCESS-VECTOR-OFFSET
    PROCESS-VECTOR:LENGTH
    PROCESS-VECTOR:ERROR-INVOKER
    PROCESS-VECTOR:START-CURRENT-HEAP
    PROCESS-VECTOR:SIZE-CURRENT-HEAP
    PROCESS-VECTOR:START-OTHER-HEAP
    PROCESS-VECTOR:SIZE-OTHER-HEAP
    PROCESS-VECTOR:STACK-TOP
    PROCESS-VECTOR:SAVE-AREA
    PROCESS-VECTOR:STACK-SIZE
    PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT
    PROCESS-VECTOR:HEAP-MARGIN-LIFTED?
    PROCESS-VECTOR:HEAP-REMAINING
    PROCESS-VECTOR:ALLOCATION-POINTER
    PROCESS-SAVE-SIZE)
  (import
    (rnrs (6)))

(define-syntax define-const
  (syntax-rules ()
    ((_ name v)
     (define-syntax name
       (identifier-syntax v)))))

(define-const CPU-VECTOR-SIZE 128)      ;n * 64

;; FIXME: get shorter names for these.

;; Indexes into the per-cpu vector (offset from fs base).
;; For use with $processor-data-ref.
(define-const CPU-VECTOR:LENGTH                  0)
(define-const CPU-VECTOR:SCHEDULER-SP            1)
(define-const CPU-VECTOR:ALTSIGSTK-BASE          2)
(define-const CPU-VECTOR:ALTSIGSTK-SIZE          3)
(define-const CPU-VECTOR:LAST-INTERRUPT-VECTOR   4)
(define-const CPU-VECTOR:BOOT-LOADER-TYPE        5)
(define-const CPU-VECTOR:BOOT-LOADER-DATA        6)
(define-const CPU-VECTOR:SCHEDULER-RUNNING?      7)
(define-const CPU-VECTOR:CPU-NUMBER              8)
(define-const CPU-VECTOR:USTATE                  9)
(define-const CPU-VECTOR:LAST-INTERRUPT-CODE    10)
(define-const CPU-VECTOR:LAST-FAULTING-ADDRESS  11)
(define-const CPU-VECTOR:SCRATCH                12)

;; Indexes into the process vector.
;; For use with $pcb-ref.
(define-const PROCESS-VECTOR-OFFSET 16) ;offset for top-level environment
(define-const PROCESS-VECTOR:LENGTH                     0)
(define-const PROCESS-VECTOR:HEAP-REMAINING             1)
(define-const PROCESS-VECTOR:ALLOCATION-POINTER         2)
(define-const PROCESS-VECTOR:START-CURRENT-HEAP         3)
(define-const PROCESS-VECTOR:SIZE-CURRENT-HEAP          4)
(define-const PROCESS-VECTOR:START-OTHER-HEAP           5)
(define-const PROCESS-VECTOR:SIZE-OTHER-HEAP            6)
(define-const PROCESS-VECTOR:STACK-TOP                  7)
(define-const PROCESS-VECTOR:STACK-SIZE                 8)
(define-const PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT   9)
(define-const PROCESS-VECTOR:HEAP-MARGIN-LIFTED?       10)
(define-const PROCESS-VECTOR:ERROR-INVOKER             11)
;; The save area begins right after the stack.
(define-const PROCESS-VECTOR:SAVE-AREA PROCESS-VECTOR:STACK-TOP)

;; The amount of space reserved for saving the state of the process
;; when it is preempted. Stored above the top of the stack. On Linux
;; this should be larger than per-cpu-stack-size. On the PC this
;; should fit the FXSAVE/XSAVE image.
(define-const PROCESS-SAVE-SIZE 4096))
