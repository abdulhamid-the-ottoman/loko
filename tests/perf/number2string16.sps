#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of number->string with hex radix

(import
  (rnrs (6))
  (only (loko) time-it*))

(define N (expt #x746E65696369666675736E6920736920646C726F7720656854 2))

(do ((i -320 (+ i 1))
     (end (if (null? (cdr (command-line))) (endianness little) (endianness big)))
     (counts '()
             (cons (cons i
                         (time-it* (string-append "number->string hex for "
                                                  (number->string i) " bits")
                                   (if (> i 50)
                                       (round (/ 100000 (/ i 50)))
                                       100000)
                                   (let* ((n (bitwise-bit-field N 0 (abs i)))
                                          (n (if (negative? i) (- n) n)))
                                     (lambda ()
                                       (number->string n 16)))))
                   counts)))
    ((= i 320)
     (call-with-output-file "tests/perf/number2string16.csv"
       (lambda (p)
         (display "# bits, cycles\n" p)
         (for-each (lambda (count)
                     (display (car count) p)
                     (display ", " p)
                     (display (cdr count) p)
                     (newline p))
                   (reverse counts))))))
