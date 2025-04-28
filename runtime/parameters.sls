;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019 G. Weinholt
#!r6rs

;;; Parameters

(library (loko runtime parameters)
  (export
    ;;parameterize   ; defined in (psyntax expander)
    make-parameter)
  (import
    (only (rnrs) define case-lambda set! unless procedure?
          assertion-violation quote let))

(define make-parameter
  (case-lambda
    ((x)
     (case-lambda
       (() x)
       ((v) (set! x v))))
    ((x fender)
     (unless (procedure? fender)
       (assertion-violation 'make-parameter "Expected a procedure" x fender))
     (let ((x (fender x)))
       (case-lambda
         (() x)
         ((v) (set! x (fender v)))))))))
