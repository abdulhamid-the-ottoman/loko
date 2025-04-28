;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Memory-related constants and such

(library (loko arch amd64 memory)
  (export
    STACK-0-START STACK-0-SIZE
    HEAP-0-START HEAP-0-SIZE
    HEAP-MARGIN
    PAGE-SIZE)
  (import
    (rnrs (6)))

(define K 1024)
(define M (* 1024 K))
(define G (* 1024 M))
(define T (* 1024 G))
(define PAGE-SIZE (* 4 K))
(define HEAP-MARGIN (* 128 K))

;; Stack and heap for process 0. This is the process that gets started
;; by the entry point of the executable image, i.e. the scheduler
;; process on the first CPU/thread.
(define STACK-0-START (* 1 T))
(define STACK-0-SIZE  (* 2 M))
(define HEAP-0-START  (+ STACK-0-START STACK-0-SIZE))
(define HEAP-0-SIZE   (* 16 M)))
