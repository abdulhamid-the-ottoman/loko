;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Primitives needed for time-keeping

(library (loko runtime time)
  (export
    current-time/process
    current-time/utc set-current-time/utc
    current-time/monotonic
    current-second                      ;TAI
    current-ticks                       ;milliseconds

    make-time time?
    time-type
    time-nanosecond
    time-second
    set-time-type!
    set-time-nanosecond!
    set-time-second!

    time-init-set!)
  (import
    (rnrs (6)))

;; This time type is the one used by SRFI 19.
(define-record-type time
  (sealed #t)
  (fields (mutable type       time-type       set-time-type!)
          (mutable nanosecond time-nanosecond set-time-nanosecond!)
          (mutable second     time-second     set-time-second!)))

(define *current-time/process* (lambda () (values 0 0)))
(define (current-time/process)
  (let-values ([(s ns) (*current-time/process*)])
    (make-time 'time-process ns s)))

(define *current-time/utc* (lambda () (values 0 0)))
(define (current-time/utc)
  (let-values ([(s ns) (*current-time/utc*)])
    (make-time 'time-utc ns s)))

(define *set-current-time/utc*
  (lambda (s)
    (error 'set-current-time/utc
           "No such procedure installed" s)))
(define (set-current-time/utc t ticks)
  (assert (eq? (time-type t) 'time-utc))
  (assert (eqv? (time-nanosecond t) 0))
  (assert (and (fixnum? ticks) (fx>=? ticks 0)))
  (*set-current-time/utc* (time-second t) ticks))

(define *current-time/monotonic* (lambda () (values 0 0)))
(define (current-time/monotonic)
  (let-values ([(s ns) (*current-time/monotonic*)])
    (make-time 'time-monotonic ns s)))

(define *current-second*
  (lambda () (error 'current-second
                    "No such procedure installed")))
(define (current-second)
  (*current-second*))

(define *current-ticks*
  (lambda () (error 'current-ticks
                    "No such procedure installed")))
(define (current-ticks)
  (*current-ticks*))

(define *nanosleep*
  (lambda _ (error 'nanosleep "No such procedure installed")))
(define (nanosleep seconds)
  ;; The process sleeps for the given amount. Can be less than a second.
  (*nanosleep* seconds))

(define (time-init-set! what value)
  (case what
    ((current-time/process) (set! *current-time/process* value))
    ((current-time/monotonic) (set! *current-time/monotonic* value))
    ((current-time/utc) (set! *current-time/utc* value))
    ((current-second) (set! *current-second* value))
    ((current-ticks) (set! *current-ticks* value))
    ((set-current-time/utc) (set! *set-current-time/utc* value))
    ((nanosleep) (set! *nanosleep* value))
    (else
     (error 'time-init-set! "Unrecognized key" what value)))))
