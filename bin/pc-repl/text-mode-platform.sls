;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

;; Platform-specific code

(library (text-mode platform)
  (export
    default-console)
  (import
    (rnrs (6))
    (text-mode console model)
    )

(define (default-console)
  (make-console
   (lambda (cmd arg)
     (case cmd
       [(get-size)
        (values 80 25 0 0)]
       [else
        #f])))))
