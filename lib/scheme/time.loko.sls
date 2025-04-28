;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme time)
  (export
    current-jiffy current-second jiffies-per-second)
  (import
    (only (rnrs) define)
    (rename (loko system time)
            (current-ticks current-jiffy)))

(define (jiffies-per-second)
  1000))
