#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of fxbit-count

(import
  (rnrs (6))
  (only (loko) time-it*))

(do ((i (- 1 (fixnum-width)) (+ i 1))
     (n 1 (let ((n (- (bitwise-arithmetic-shift-left 1 (abs i)) 1)))
            (if (negative? i) (- n) n)))
     (counts '()
             (cons (cons i
                         (time-it* (string-append "fxbit-count #x" (number->string n 16))
                                   30000000
                                   (lambda () (fxbit-count n))))
                   counts)))
    ((not (fixnum? n))
     (call-with-output-file "tests/perf/fxbit-count.csv"
       (lambda (p)
         (display "# bits, cycles\n" p)
         (for-each (lambda (count)
                     (display (car count) p)
                     (display ", " p)
                     (display (cdr count) p)
                     (newline p))
                   (reverse counts))))))
