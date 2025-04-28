;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019 G. Weinholt
#!r6rs

(library (loko runtime compat)
  (export
    make-parameter)
  (import
    (only (chezscheme) make-parameter)))
