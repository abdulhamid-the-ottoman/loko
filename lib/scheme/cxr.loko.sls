;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme cxr)
  (export
    caaaar caaadr caaar caadar caaddr caadr cadaar cadadr cadar
    caddar cadddr caddr cdaaar cdaadr cdaar cdadar cdaddr cdadr
    cddaar cddadr cddar cdddar cddddr cdddr)
  (import
    (rnrs)))
