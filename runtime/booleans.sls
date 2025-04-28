;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019 G. Weinholt
#!r6rs

;;; Booleans

(library (loko runtime booleans)
  (export
    not boolean? boolean=?)
  (import
    (only (rnrs (6))
          define case-lambda let cond else and unless quote
          eq? car cdr null? assertion-violation apply)
    (prefix (only (rnrs) not boolean?)
            sys:))

(define (not x) (sys:not x))

(define (boolean? x) (sys:boolean? x))

(define boolean=?
  (case-lambda
    ((x y)
     (unless (and (boolean? x) (boolean? y))
       (assertion-violation 'boolean=? "Expected booleans" x y))
     (eq? x y))
    ((x y . rest*)
     (unless (and (boolean? x) (boolean? y))
       (apply assertion-violation 'boolean=? "Expected booleans" x y rest*))
     (let lp ((x* rest*) (ret (eq? x y)))
       (cond ((null? x*) ret)
             (else
              (unless (boolean? (car x*))
                (apply assertion-violation 'boolean=? "Expected booleans" x y rest*))
              (lp (cdr x*) (and ret (eq? x (car x*)))))))))))
