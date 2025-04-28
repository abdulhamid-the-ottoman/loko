#!/usr/bin/env scheme-script
;; -*- coding: utf-8; mode: scheme -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt

(import
  (rnrs (6))
  (loko match))

(call-with-input-file "../../Akku.manifest"
  (lambda (p)
    (read p)
    (match (read p)
      [('akku-package (_ version) . _)
       (when (file-exists? "version.texi")
         (delete-file "version.texi"))
       (call-with-output-file "version.texi"
         (lambda (out)
           (display "@set VERSION " out)
           (display version out)
           (newline out)))])))
