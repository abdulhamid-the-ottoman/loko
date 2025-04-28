;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Initialize the library manager

(library (loko runtime start-libman)
  (export)
  (import
    (rnrs (6))
    (srfi :98 os-environment-variables)
    (only (loko config) config-library-path)
    (only (psyntax library-manager) library-directories library-extensions)
    (only (loko runtime utils) string-split))

;; Read the environment
;; Gerald Klix, 2019-12-11 20:15:25:
;; Consult the environment for library file extensions
(let ((library-extension-list '(".loko.sls" ".sls" ".sld")))
  (library-extensions
   (cond
    ((get-environment-variable "LOKO_LIBRARY_FILE_EXTENSIONS") =>
     (lambda (extensions)
       (append (string-split extensions #\:)
               library-extension-list)))
    (else library-extension-list))))

(cond
  ((get-environment-variable "LOKO_LIBRARY_PATH") =>
   (lambda (path)
     (library-directories (append (string-split path #\:)
                                  (config-library-path)))))
  (else
   (library-directories (cons "." (config-library-path))))))
