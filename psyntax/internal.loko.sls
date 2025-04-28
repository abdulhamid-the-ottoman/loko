;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 G. Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

(library (psyntax internal)
  (export current-primitive-locations #;compile-core-expr-to-port expanded->core)
  (import (rnrs) (psyntax compat))

(define current-primitive-locations
  (make-parameter
   (lambda (x) #f)
   (lambda (p)
     (assert (procedure? p))
     p)))

(define (expanded->core x)
  x))
