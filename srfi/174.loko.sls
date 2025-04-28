;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; SRFI-174 (POSIX timespecs)

;; This was originally supposed to be used by SRFI 170, but that SRFI
;; went with SRFI 19 instead.

(library (srfi :174)
  (export
    timespec
    timespec?
    timespec-seconds
    timespec-nanoseconds
    inexact->timespec
    timespec->inexact
    timespec=?
    timespec<?
    timespec-hash)
  (import
    (rnrs)
    (srfi :19))

(define (timespec s ns)
  (make-time 'time-utc ns s))

(define timespec? time?)

(define timespec-seconds time-second)

(define timespec-nanoseconds time-nanosecond)

(define (inexact->timespec inexact)
  (cond
    ((and (flonum? inexact) (not (flfinite? inexact)))
     (assertion-violation 'inexact->timespec
                          "Expected a finite value" inexact))
    (else
     (let* ((s (fltruncate inexact))
            (ns (flabs (fl- inexact s))))
       (make-time 'time-utc
                  (exact (round (* 1e9 ns)))
                  (exact s))))))

(define (timespec->inexact timespec)
  (fl+ (fixnum->flonum (time-second timespec))
       (fl/ (inexact (time-nanosecond timespec)) #i1e9)))

(define (timespec=? timespec1 timespec2)
  (time=? timespec1 timespec2))

(define (timespec<? timespec1 timespec2)
  (time<? timespec1 timespec2))

(define (timespec-hash timespec)
  (equal-hash `(,(time-second timespec) . ,(time-nanosecond timespec)))))
