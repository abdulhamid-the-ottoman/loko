;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019 G. Weinholt
#!r6rs

;;; Code to start a Scheme process

(library (loko runtime start)
  (export)
  (import
    (only (rnrs (6)) call/cc lambda exit quote)
    (only (loko runtime fibers) run-fibers)
    (only (loko runtime init) init))

;; Call the init code from pc-init, linux-init or process-init.
(init 'pre-fibers)

;; Ensure that everything from here on (the rest of the libraries and
;; the top-level) runs with a fiber scheduler.
(call/cc
  (lambda (k)
    (run-fibers k)
    (exit 0)))

;; Do initialization that requires fibers.
(init 'post-fibers))
