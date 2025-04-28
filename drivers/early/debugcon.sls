;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019-2022 G. Weinholt
#!r6rs

;;; Early output via Bochs port_e9_hack / QEMU debugcon

(library (loko drivers early debugcon)
  (export
    init-early-debugcon-driver)
  (import
    (rnrs (6))
    (only (loko runtime io) $init-standard-ports)
    (loko system unsafe))

(define (init-early-debugcon-driver)
  (define debugcon #xe9)
  (define (debug-put bv start count)
    (put-i/o-u8-n debugcon
                  (fx+ (bytevector-address bv) start)
                  count)
    count)
  ($init-standard-ports (lambda _ 0) debug-put debug-put
                        (buffer-mode none) (eol-style crlf))))
