;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; USB HID mouse driver

(library (loko drivers usb hid-mouse)
  (export
    probe·usb·hid·mouse?
    driver·usb·hid·mouse)
  (import
    (rnrs)
    (loko system fibers)
    (loko system logging)
    (loko drivers mouse)
    (loko drivers usb core)
    (loko drivers usb hid)
    (loko drivers usb hid-parser))

(define (probe·usb·hid·mouse? hiddev)
  (let ((collection (usbhid-device-collection hiddev)))
    (and (eqv? (hid-collection-type collection) HID-COLLECTION-TYPE-APPLICATION)
         (and (member (hid-collection-usage collection)
                      '((#x01 . #x01)
                        (#x01 . #x02)))
              #t))))

(define (driver·usb·hid·mouse hiddev mouse)
  (define reportdesc (usbhid-device-collection hiddev))
  (let lp ()
    (let ((report (perform-operation (usb-pipe-in-operation (usbhid-device-in-pipe hiddev)))))
      (define x 0)
      (define y 0)
      (define z 0)
      (define buttons 0)
      (hid-parse-input-report reportdesc report
                              (lambda _ #f)
                              (lambda (_item page id value)
                                (case page
                                  ((1)
                                   ;; XXX: Assumes the item reports
                                   ;; deltas. Wrong for trackpads.
                                   (case id
                                     ((#x30) (set! x value))
                                     ((#x31) (set! y value))
                                     ((#x38) (set! z value))))
                                  ((9)
                                   (when (fx<=? 1 value 32)
                                     (let ((idx (fx- id 1)))
                                       (let ((mask (fxarithmetic-shift-left 1 idx))
                                             (value (fxarithmetic-shift-left value idx)))
                                         (set! buttons
                                               (fxior value (fxand buttons (fxnot mask)))))))))))
      (put-message (mouse-event-channel mouse) (vector x y z buttons #f))
      (lp)))))
