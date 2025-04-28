;; -*-scheme-*-
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+
(define-library (include-order-body)
  (export foo-a foo-b FOO-A FOO-B)

  (import (scheme base))

  (begin
    (define counter 0))

  (begin
    (include "foo-a.scm" "foo-b.scm")     ;FOO-*
    (include-ci "foo-a.scm" "foo-b.scm")  ;foo-*
    (begin
      (unless (equal? '(1 2 3 4 4) (list FOO-A FOO-B foo-a foo-b counter))
        (error 'include-order-body "Test failure" FOO-A FOO-B foo-a foo-b counter)))))
