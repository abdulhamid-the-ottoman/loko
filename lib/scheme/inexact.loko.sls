;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme inexact)
  (export
    acos asin atan cos exp finite? infinite? log nan? sin sqrt tan)
  (import
    (except (rnrs) finite? infinite? nan?)
    (prefix (rnrs) r6:))

(define (finite? z)
  (if (real? z)
      (r6:finite? z)
      (and (r6:finite? (real-part z))
           (r6:finite? (imag-part z)))))

(define (infinite? z)
  (if (real? z)
      (r6:infinite? z)
      (or (r6:infinite? (real-part z))
          (r6:infinite? (imag-part z)))))

(define (nan? z)
  (if (real? z)
      (r6:nan? z)
      (or (r6:nan? (real-part z))
          (r6:nan? (imag-part z))))))
