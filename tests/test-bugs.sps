#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2023 G. Weinholt
#!r6rs

;;; Test suite that verifies bug fixes

(import
  (rnrs))

(assert (equal? '(b a)
                (enum-set->list (make-enumeration '(b a b)))))

(display "Tests passed\n")
