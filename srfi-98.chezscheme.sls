;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Partial SRFI 98 for bootstrapping with Chez Scheme

(library (srfi :98 os-environment-variables)
  (export
    get-environment-variable)
  (import
    (rename (chezscheme) (getenv get-environment-variable))))
