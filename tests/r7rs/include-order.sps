;; -*-scheme-*-
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+
(import
  (scheme base)
  (scheme write)
  (prefix (include-order-decl) decl:)
  (prefix (include-order-decl-indir) decl-indir:)
  (prefix (include-order-body) body:)
  (prefix (include-order-body-indir) body-indir:)
  )


(define result
  (list decl:FOO-A decl-indir:FOO-A body:FOO-A body-indir:FOO-A
        decl:FOO-B decl-indir:FOO-B body:FOO-B body-indir:FOO-B
        decl:foo-a decl-indir:foo-a body:foo-a body-indir:foo-a
        decl:foo-b decl-indir:foo-b body:foo-b body-indir:foo-b))

(define expect
  '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4))

(write expect)
(display "\n")
(write result)
(display "\n")

(unless (equal? expect result)
  (error #f "test failure\n"))

(display "test ok\n")
