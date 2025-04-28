;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme process-context)
  (export
    command-line emergency-exit (rename (r7rs-exit exit))
    get-environment-variable
    get-environment-variables)
  (import
    (rnrs)
    (srfi :98 os-environment-variables)
    (only (loko system r7rs) emergency-exit))

(define (translate-status status)
  (case status
    ((#t) 0)
    ((#f) 1)
    (else status)))

(define r7rs-exit
  (case-lambda
    (()
     (exit))
    ((status)
     (exit (translate-status status))))))
