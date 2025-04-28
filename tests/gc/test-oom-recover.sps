;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2021 G. Weinholt

;;; Verify recoverable out-of-memory behavior

(import (rnrs))

(let lp ((i 0))
  (unless (= i 4)
    (guard (exn
            (else
             (display "ignoring: ")
             (write exn)
             (newline)
             (display i)
             (newline)
             (lp (+ i 1))))
      (do ((x 0 (cons 1 x)))
          (#f))))
  (display i)
  (newline))

(display "test ok\n")
