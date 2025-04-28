;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2021 G. Weinholt

;;; Verify fatal out-of-memory behavior

;; The process should exit with an out-of-memory error.

(import (rnrs))

(define x #f)

(let lp ()
  (guard (exn
          (else
           (lp)))
    (do ()
        (#f)
      (set! x (cons 1 x)))))

(display "error: should not get here\n")
