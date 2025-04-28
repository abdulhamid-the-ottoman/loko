;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme read)
  (export
    read)
  (import
    (except (rnrs) read))

(define read
  (case-lambda
    (() (read (current-input-port)))
    ((port)
     ;; TODO: Set the reader mode to 'r7rs. The default is permissive
     ;; mode.
     (get-datum port)))))
