;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme write)
  (export
    display write write-shared write-simple)
  (import
    (except (rnrs) display write)
    (only (loko) parameterize print-dialects)
    (prefix (only (loko) display write) loko:)
    (prefix (only (loko system r7rs) write-shared write-simple) loko:))

(define display
  (case-lambda
    ((obj)
     (display obj (current-output-port)))
    ((obj port)
     (parameterize ([print-dialects '(r7rs)])
       (loko:display obj port)))))

(define write
  (case-lambda
    ((obj)
     (write obj (current-output-port)))
    ((obj port)
     (parameterize ([print-dialects '(r7rs)])
       (loko:write obj port)))))

(define write-shared
  (case-lambda
    ((obj)
     (write-shared obj (current-output-port)))
    ((obj port)
     (parameterize ([print-dialects '(r7rs)])
       (loko:write-shared obj port)))))

(define write-simple
  (case-lambda
    ((obj)
     (write-shared obj (current-output-port)))
    ((obj port)
     (parameterize ([print-dialects '(r7rs)])
       (loko:write-simple obj port))))))
