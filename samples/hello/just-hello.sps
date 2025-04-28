#!/usr/bin/env scheme-script
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019 G. Weinholt

;;; Free-standing "Hello world" program

(import
  (except (rnrs base) list cons)
  (loko system unsafe)
  ;; XXX: The primitives are not guaranteed to be stable between releases
  (loko system $primitives))

(define (display x) ($debug-display x))
(define (exit x) (syscall 60 x))

(display 'hello)
(display #\space)
(display 'world)
(display #\newline)
(exit 0)
