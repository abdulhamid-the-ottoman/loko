#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of fxbit-set?

(import
  (rnrs (6))
  (only (loko) time-it*)
  (loko system random))

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

(do ((n 0 (+ n 1))
     (counts '()
             (let ((t (time-it* (string-append "fxbit-set? x " (number->string n))
                                10000000
                                (let ((bv (make-bytevector 8)))
                                  (get-random-bytevector-n! bv 0 8)
                                  (let* ((x (bitwise-and (bytevector-s64-native-ref bv 0)
                                                         (greatest-fixnum)))
                                         (x (if (fxbit-set? x 0) (- x) x)))
                                    (lambda ()
                                      (fxbit-set? x n)))))))
               (cons (cons n t) counts))))
    ((> n 100)
     (print-csv "tests/perf/fxbit-set?.csv"
                counts)))
