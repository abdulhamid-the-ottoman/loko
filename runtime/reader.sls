;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; The S-expr reader

(library (loko runtime reader)
  (export
    read get-datum

    case-sensitive
    read-annotated
    annotation? annotation-expression annotation-stripped
    annotation-source annotation-source->condition)
  (import
    (except (rnrs) read get-datum)
    (only (loko runtime io) port-reader port-reader-set! port-id)
    (except (laesare reader) read-annotated)
    (prefix (only (laesare reader) read-annotated) laesare:)
    (loko runtime parameters))

(define case-sensitive
  (make-parameter #t))

(define (get-port-reader p fn)
  (cond ((port-reader p))
        (else
         (let ((reader (make-reader p (or fn (port-id p)))))
           (when (not (case-sensitive))
             (reader-fold-case?-set! reader #t))
           (port-reader-set! p reader)
           reader))))

(define read-annotated
  (case-lambda
    ((p)
     (read-annotated p #f))
    ((p fn)
     (let ((x (laesare:read-annotated (get-port-reader p fn))))
       (if (eof-object? (annotation-stripped x))
           (eof-object)
           x)))))

(define (get-datum p)
  (read-datum (get-port-reader p #f)))

(define read
  (case-lambda
    (()
     (get-datum (current-input-port)))
    ((p)
     (get-datum p)))))
