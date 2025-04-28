#!/usr/bin/env -S loko --program
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Plot the performance of bytevector-sint-set! with big endianness

(import
  (rnrs (6))
  (only (loko) time-it*))

(do ((i -128 (+ i 1))
     (end (if (null? (cdr (command-line))) (endianness big) (endianness little)))
     (bv (make-bytevector 128 0))
     (counts '()
             (cons (cons i
                         (time-it* (string-append "bytevector-sint-set! big "
                                                  (number->string i) " bits")
                                   (if (> i 50)
                                       (round (/ 500000 (/ i 50)))
                                       500000)
                                   (let* ((n (expt 2 (abs i)))
                                          (n (if (negative? i) (- n) n))
                                          (len (+ 1 (div (+ 7 (bitwise-length n)) 8))))
                                     (lambda ()
                                       (bytevector-sint-set! bv 0 n end len)))))
                   counts)))
    ((= i 128)
     (call-with-output-file "tests/perf/bytevector-sint-be-set_.csv"
       (lambda (p)
         (display "# bits, cycles\n" p)
         (for-each (lambda (count)
                     (display (car count) p)
                     (display ", " p)
                     (display (cdr count) p)
                     (newline p))
                   (reverse counts))))))
