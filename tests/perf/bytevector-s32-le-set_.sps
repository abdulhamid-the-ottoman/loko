#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of bytevector-s32-set! with little endianness

(import
  (rnrs (6))
  (only (loko) time-it*))

(do ((i -31 (+ i 1))
     (end (if (null? (cdr (command-line))) (endianness little) (endianness big)))
     (bv (make-bytevector 9 0))
     (counts '()
             (cons (cons i
                         (time-it* "bytevector-s32-set! little"
                                   5000000
                                   (let* ((n (expt 2 (abs i)))
                                          (n (if (negative? i) (- n) n)))
                                     (lambda ()
                                       (bytevector-s32-set! bv 0 n end)))))
                   counts)))
    ((= i 31)
     (call-with-output-file "tests/perf/bytevector-s32-le-set_.csv"
       (lambda (p)
         (display "# bits, cycles\n" p)
         (for-each (lambda (count)
                     (display (car count) p)
                     (display ", " p)
                     (display (cdr count) p)
                     (newline p))
                   (reverse counts))))))
