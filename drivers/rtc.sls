;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Real-Time Clock abstraction

(library (loko drivers rtc)
  (export
    make-rtc-device
    rtc-device-request-channel
    rtc-device-get-date
    rtc-device-set-date)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (srfi :19 time))

(define-record-type rtc-device
  (sealed #t)
  (fields request-channel)
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel))))))

;; Read the current date (as a SRFI-19 date object) from the device.
;; This waits for a new second to be generated, and might take up to
;; two seconds to return. If the hardware is not working properly or
;; no time has been set then it returns #f.
(define (rtc-device-get-date dev)
  (let ((resp-ch (make-channel)))
    (put-message (rtc-device-request-channel dev) (list 'get-date resp-ch))
    (match (get-message resp-ch)
      [(date . ticks)
       (values date ticks)])))

(define (rtc-device-set-date dev date)
  (assert (date? date))
  (let ((resp-ch (make-channel)))
    (put-message (rtc-device-request-channel dev) (list 'set-date resp-ch date))
    (get-message resp-ch))))
