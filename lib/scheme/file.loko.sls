;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme file)
  (export
    call-with-input-file call-with-output-file delete-file file-exists?
    open-binary-input-file open-binary-output-file
    open-input-file open-output-file with-input-from-file
    with-output-to-file)
  (import
    (rnrs))

(define (open-binary-input-file file)
  (open-file-input-port file))

(define (open-binary-output-file file)
  (open-file-output-port file)))
