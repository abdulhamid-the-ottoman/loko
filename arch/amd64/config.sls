;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Configuration for the AMD64 architecture

(library (loko arch amd64 config)
  (export
    %cpu
    %pcb
    %closure
    %arg-reg*
    %ret-reg*
    %arg-count
    %value-count
    %value-count/64

    use-sse4.1
    use-popcnt)
  (import
    (rnrs))

(define-syntax define-const
  (syntax-rules ()
    ((_ name v)
     (define-syntax name
       (identifier-syntax v)))))

(define-const %cpu 'fs)                 ;Per-CPU vector

;; This should be a base register that can be used to access the
;; process vector.
(define-const %pcb 'r14)                ;Process Control Block

(define-const %closure 'r15)

(define-const %arg-reg* '(rdi rsi rdx rcx r8 r9))

(define-const %ret-reg* %arg-reg*)

;; The number of arguments passed to a procedure, negated.
(define-const %arg-count 'eax)

;; The number of values returned from a procedure. Only valid if the
;; carry flag is set.
(define-const %value-count 'r10d)

(define-const %value-count/64 'r10)

(define-const use-sse4.1 #f)

(define-const use-popcnt #f)

;; This assumption is made here and there
(assert (equal? %arg-reg* %ret-reg*))

)
