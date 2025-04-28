;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019 G. Weinholt
#!r6rs

;;; Valgrind client requests for when running under Valgrind.

;; If not under Valgrind then these calls are NOPs. Valgrind believes
;; in the red zone, which Loko does not care about, so it needs
;; patching to disable those checks. In memcheck/mc_main.c the
;; helperc_MAKE_STACK_UNINIT_* procedures should be disabled.

(library (loko arch amd64 lib-valgrind)
  (export
    lib-valgrind:text lib-valgrind:data)
  (import
    (loko arch amd64 objects)
    (rnrs))

(define (lib-valgrind:text)
;;;
;;; Issues a Valgrind client request
;;;
  `((%label $valgrind)
    ;; Function signature.
    ;; Input:   rdi    fixnum pointer to request data
    ;; Output:  rax    fixnum result
    ;;
    ;; Valgrind magic sequence.
    ;; Input:   rax    pointer to the request
    ;; Output:  rdx    result
    (mov rax rdi)
    (sar rax ,(shift 'fixnum))
    ;; This code is here because the optimizer would remove the magic
    ;; sequence.
    (xor edx edx)
    (rol rdi 3)
    (rol rdi 13)
    (rol rdi 61)
    (rol rdi 51)
    (xchg rbx rbx)                      ;magic happens
    (mov rax rdx)
    (shl rax ,(shift 'fixnum))
    (ret)))

(define (lib-valgrind:data)
  `()))
