;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; SRFI-198 Foreign Interface Error Handling

;; Private to Loko.

(library (srfi :198 private)
  (export
    make-syscall-error*
    syscall-errno-condition?
    condition-syscall-function
    condition-syscall-errno
    condition-syscall-symbol
    condition-syscall-message)
  (import
    (rnrs conditions))

(define-condition-type &syscall-errno &error
   make-syscall-error* syscall-errno-condition?
   (function condition-syscall-function)
   (errno condition-syscall-errno)
   (symbol condition-syscall-symbol)
   (message condition-syscall-message)))
