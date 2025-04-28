;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019 G. Weinholt

(library (hello-lib)
  (export hello)
  (import (rnrs))

(define (hello who)
  (display "Hello, ")
  (display who)
  (display "!\n")))
