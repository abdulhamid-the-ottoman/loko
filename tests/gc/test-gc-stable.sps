;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2021 G. Weinholt

;;; Check that a simple cons in a loop does not leak memory.

(import (rnrs)
        (only (loko system $primitives) $heap-remaining))

(let lp ((x '())
         (prev-free 0)
         (most-free 0)
         (n 0))
  (let ((free ($heap-remaining)))
    (cond ((fx>? free prev-free)
           (display (list 'free free 'prev-free prev-free 'most-free most-free))
           (newline)
           (when (and (fx<? free most-free) (fx>=? n 5))
             (error #f "memory leak"))
           (unless (fx>=? n 10)
             (lp x
                 free
                 (fxmax most-free free)
                 (fx+ n 1))))
          (else
           (lp (cons '() '())
               free
               (fxmax most-free free)
               n)))))

(display "test ok\n")
