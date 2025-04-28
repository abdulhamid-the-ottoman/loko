;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme eval)
  (export environment eval)
  (import
;;    (rnrs)
    (only (rnrs eval) eval environment))

  #;
(define (environment . lib*)
  (apply r6:environment
         (map (lambda (lib)
                (map (lambda (id)
                       (if (integer? id)
                           (string->symbol (string-append ":" (number->string id)))
                           id))
                     lib))
              lib*))))
