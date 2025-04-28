;; -*-scheme-*-
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+
(define-library (include-order-decl-indir)
  (export foo-a foo-b FOO-A FOO-B)

  (import (scheme base))

  (begin
    (define counter 0))

  (include-library-declarations "include-order-decl-indir-aux.scm")

  (begin
    (unless (equal? '(1 2 3 4 4) (list FOO-A FOO-B foo-a foo-b counter))
      (error 'include-order-decl-indir "Test failure" FOO-A FOO-B foo-a foo-b counter))))
