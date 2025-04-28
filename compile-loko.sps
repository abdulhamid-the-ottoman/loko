#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2022 G. Weinholt
#!r6rs

;;; Program that builds the loko binary

(import
  (rnrs (6))
  (psyntax library-manager)
  (loko config)
  (only (loko compiler cp0) cp0-effort-limit)
  (loko compiler static)

  (only (loko compiler compat) gensym?)
  (prefix (loko arch amd64 pc-asm) pc-asm:)
  (prefix (loko arch amd64 linux-asm) linux-asm:)
  (prefix (loko arch amd64 netbsd-asm) netbsd-asm:)
  (prefix (loko arch amd64 pc-and-linux-asm) pc-and-linux-asm:)
  (prefix (loko arch amd64 polyglot-asm) polyglot-asm:))

;; Workaround for the library visiting semantics in R6RS
gensym?
pc-asm:visit
linux-asm:visit
netbsd-asm:visit
pc-and-linux-asm:visit
polyglot-asm:visit

;; Amp up the optimizations
(cp0-effort-limit 1000)

;; We don't want to load libraries that aren't in the built-in list,
;; but those libraries may include files from the library path.
(include-path '(".akku/lib"))
(library-directories '())

(let ((filename "loko-prebuilt.out"))
  (compile-program filename "loko.sps" '(eval main use-primlocs bootstrap))
  (display "Build finished and written to ")
  (display filename)
  (newline))
