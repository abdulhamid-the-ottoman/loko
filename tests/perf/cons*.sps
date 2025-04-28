#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of cons*

(import
  (rnrs (6))
  (only (loko) time-it*))

(define-syntax timed-applications
  (lambda (x)
    (syntax-case x ()
      [(_ prefix proc n^ repetitions arg)
       (let ((n (syntax->datum #'n^)))
         (let lp ((n n))
           (if (<= n 0)
               #'(list)
               (with-syntax (((repeated ...)
                              (vector->list
                               (make-vector n #'arg))))
                 #`(cons (cons #,n (time-it* (string-append prefix (number->string #,n))
                                             repetitions
                                             (lambda ()
                                               (proc repeated ...))))
                         #,(lp (- n 10)))))))])))

(define (print-csv filename counts)
  (call-with-output-file filename
    (lambda (p)
      (display "# bits, cycles\n" p)
      (for-each (lambda (count)
                  (display (car count) p)
                  (display ", " p)
                  (display (cdr count) p)
                  (newline p))
                (reverse counts)))))

(print-csv "tests/perf/cons*.csv"
           (timed-applications "cons* × " cons* 1000 100000 'x))
