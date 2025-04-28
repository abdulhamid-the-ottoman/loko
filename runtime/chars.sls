;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Characters

(library (loko runtime chars)
  (export
    char? char->integer integer->char
    char=? char<? char>? char<=? char>=?)
  (import
    (except (rnrs)
            char? char->integer integer->char
            char=? char<? char>? char<=? char>=?
            char-upcase char-downcase char-titlecase char-foldcase
            char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
            char-alphabetic? char-numeric? char-whitespace?
            char-upper-case? char-lower-case? char-title-case?
            char-general-category)
    (prefix (rnrs) sys:))

(define (char? x) (sys:char? x))

(define (char->integer x) (sys:char->integer x))

(define (integer->char sv) (sys:integer->char sv))

(define-syntax define-char-predicate
  (lambda (x)
    (syntax-case x ()
      ((_ name same? ->obj)
       #'(define name
           (case-lambda
             ((x y)
              (same? (->obj x) (->obj y)))
             ((x y z)
              (let ((x (->obj x)) (y (->obj y)) (z (->obj z)))
                (and (same? x y) (same? y z))))
             ((x y^ . y*^)
              (let lp ((x y^) (y* y*^) (ret (same? (->obj x) (->obj y^))))
                (if (null? y*)
                    ret
                    (let ((y (car y*)))
                      (lp y (cdr y*) (and (same? (->obj x) (->obj y)) ret))))))))))))

(define-char-predicate char=? eq? (lambda (x) (char->integer x) x))

(define-char-predicate char<? fx<? char->integer)

(define-char-predicate char>? fx>? char->integer)

(define-char-predicate char<=? fx<=? char->integer)

(define-char-predicate char>=? fx>=? char->integer))
