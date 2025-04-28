#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of bytevector-u64-native-set!

(import
  (rnrs (6))
  (only (loko) time-it*))

(do ((i 0 (+ i 1))
     (bv (make-bytevector 8 0))
     (counts '()
             (cons (cons i
                         (time-it* "bytevector-u64-native-set!"
                                   5000000
                                   (let ((n (expt 2 i)))
                                     (lambda ()
                                       (bytevector-u64-native-set! bv 0 n)))))
                   counts)))
    ((= i 63)
     (call-with-output-file "tests/perf/bytevector-u64-native-set_.csv"
       (lambda (p)
         (display "# bits, cycles\n" p)
         (for-each (lambda (count)
                     (display (car count) p)
                     (display ", " p)
                     (display (cdr count) p)
                     (newline p))
                   (reverse counts))))))
