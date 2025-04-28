;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme lazy)
  (export
    delay
    delay-force
    force
    promise?
    make-promise)
  (import
    (rnrs)
    (rnrs mutable-pairs))

;; This implementation is from R7RS-small

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-promise* #f (lambda () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise* #t expression)))))

(define (make-promise obj)
  (make-promise* #t obj))

(define make-promise*
  (lambda (done? proc)
    (list (cons done? proc))))

(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise))))
        (unless (promise-done? promise)
          (promise-update! promise* promise))
        (force promise))))

(define promise-done?
  (lambda (x) (car (car x))))

(define promise-value
  (lambda (x) (cdr (car x))))

(define promise-update!
  (lambda (new old)
    (set-car! (car old) (promise-done? new))
    (set-cdr! (car old) (promise-value new))
    (set-car! new (car old))))

(define (promise? obj)
  (and (pair? obj)
       (null? (cdr obj))
       (pair? (car obj))
       (boolean? (caar obj)))))
