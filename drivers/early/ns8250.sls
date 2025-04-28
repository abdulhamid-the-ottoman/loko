;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019-2022 G. Weinholt
#!r6rs

;;; Early output via 8250 UART

(library (loko drivers early ns8250)
  (export
    init-early-ns8250-driver)
  (import
    (rnrs (6))
    (only (loko runtime io) $init-standard-ports)
    (loko system unsafe))

;; Hard-coded for PCs, for now
(define (init-early-ns8250-driver)
  (define com1 #x3f8)
  (define thb (+ com1 0))
  (define lsr (+ com1 5))
  (define (serial-put-u8 b)
    (let lp ()
      (when (fxzero? (fxand (get-i/o-u8 lsr) #b100000))
        (lp)))
    (put-i/o-u8 thb b))
  (define (serial-put bv start count)
    (do ((end (fx+ start count))
         (i start (fx+ i 1)))
        ((fx=? i end) count)
      (serial-put-u8 (bytevector-u8-ref bv i))))
  ($init-standard-ports (lambda _ 0) serial-put serial-put
                        (buffer-mode none) (eol-style crlf))))
