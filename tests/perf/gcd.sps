#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of gcd

(import
  (rnrs (6))
  (only (loko) time-it*))

(do ((i 0 (+ i 1))
     (counts '()
             (let* ((n (expt 3 i))
                    (m (div (+ n i) (expt 2 i))))
               (cons (cons i
                           (time-it* (string-append "* #x" (number->string n 16)
                                                    " #x" (number->string m 16))
                                     300000
                                     (lambda () (gcd n m))))
                     counts))))
    ((> i 64)
     (call-with-output-file "tests/perf/gcd.csv"
       (lambda (p)
         (display "# bits, cycles\n" p)
         (for-each (lambda (count)
                     (display (car count) p)
                     (display ", " p)
                     (display (cdr count) p)
                     (newline p))
                   (reverse counts))))))
