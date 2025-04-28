;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Linux amd64 compatibility for the processes library

;; TODO: define-library and cond-expand would be a good way to handle
;; the different archs and OSes

(library (pre-srfi processes compat)
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
    (rnrs)

    (loko arch amd64 linux-syscalls)
    (loko arch amd64 linux-numbers)

    (only (loko system unsafe) bytevector-address)
    (only (loko) port-file-descriptor collections include/resolve)
    (loko match)
    (loko system fibers)

    (srfi :98)
    (srfi :170)
    (srfi :198 private)

    (only (loko system $host) enable-signal acknowledge-signal wait-signal-operation))

(include/resolve ("pre-srfi" "processes") "processlib.scm")

(spawn-fiber sigchld-fiber))
