;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2022 G. Weinholt
#!r6rs

;;; Storage device abstraction

;; This provides a thin abstraction on top of drivers for block
;; devices.

(library (loko drivers storage)
  (export
    make-storage-device storage-device?
    storage-device-name
    storage-device-request-channel
    storage-device-logical-sector-size)
  (import
    (rnrs (6))
    (loko system fibers))

;; A device driver will be attached to this, answering requests.
(define-record-type storage-device
  (sealed #t)
  (fields name
          request-channel
          logical-sector-size)
  (protocol
   (lambda (p)
     (lambda (name logical-sector-size)
       (p name (make-channel)
          logical-sector-size))))))
