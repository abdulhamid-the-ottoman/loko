;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

;;; POSIX subprocess support

;; Based on ProcessesCowan.md.

(library (pre-srfi processes)
  (export
    make-pipe
    make-textual-pipe
    make-process

    pid->proc

    process?
    synthetic-process?

    process-child-id
    process-child-group
    process-child-session
    process-terminated?
    process-stopped?
    process-exit-code
    process-stop-signal
    process-terminate-signal

    process-wait
    process-wait-any
    process-wait-group

    process-terminate
    process-send-signal
    process-send-group-signal

    process-fork
    process-exec

    process-exception?
    process-exception-errno
    process-exception-message

    ;; XXX: not in the document
    get-process-id)
  (import
    (rnrs (6))
    (srfi :198 private)
    (pre-srfi processes compat)))
