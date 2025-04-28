;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; USB HID keyboard driver

;; TODO: Properly handle different usage types (OSC, OOC, etc)

(library (loko drivers usb hid-keyboard)
  (export
    probe·usb·hid·keyboard?
    driver·usb·hid·keyboard

    ;; Exposed for testing purposes
    usb-hid-keyboard-parse-report)
  (import
    (rnrs)
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko drivers keyboard)
    (loko drivers usb core)
    (loko drivers usb hid)
    (loko drivers usb hid-parser))

(define (probe·usb·hid·keyboard? hiddev)
  (let ((collection (usbhid-device-collection hiddev)))
    (and (eqv? (hid-collection-type collection) HID-COLLECTION-TYPE-APPLICATION)
         (and (member (hid-collection-usage collection)
                      '((#x01 . #x06)   ;Generic Desktop, Keyboard
                        (#x01 . #x07)   ;Generic Desktop, Keypad
                        ;; XXX: Maybe, just maybe, it's not the right
                        ;; thing to make this work as a keyboard?
                        (#x0C . #x01))) ;Consumer, Consumer Control
              #t))))

(define (usb-hid-keyboard-parse-report reportdesc report down-keys send)
  (define up-keys (hashtable-copy down-keys 'mutable))

  (define (array-item-cb _item page id index)
    (cond
      ((and (eqv? page #x07) (memv id '(#x01 #x02 #x03)))
       ;; XXX: Reports are full of 1s on rollover
       (send-log WARNING (cdr (assq id '((#x01 . "ErrorRollOver")
                                         (#x02 . "POSTFail")
                                         (#x03 . "ErrorUndefined"))))))
      ((eqv? id 0))
      (else
       (let ((usage (cons page id)))
         (cond ((hashtable-contains? down-keys usage)
                ;; The key is still down, so it is not up
                (hashtable-delete! up-keys usage))
               (else
                ;; The key was not down, but now it is
                (hashtable-set! down-keys usage #t)
                (send 'make report (car usage) (cdr usage))))))))

  (define (variable-item-cb _item page id value)
    (when (eqv? value 1)
      (let ((usage (cons page id)))
        (cond ((hashtable-contains? down-keys usage)
               ;; The key is still down, so it is not up
               (hashtable-delete! up-keys usage))
              (else
               ;; The key was not down, but now it is
               (hashtable-set! down-keys usage #t)
               (send 'make report (car usage) (cdr usage)))))))

  (hid-parse-input-report reportdesc report array-item-cb variable-item-cb)
  (vector-for-each (lambda (usage)
                     (send 'break report (car usage) (cdr usage))
                     (hashtable-delete! down-keys usage))
                   (hashtable-keys up-keys))
  (hashtable-copy down-keys 'mutable))

(define (driver·usb·hid·keyboard hiddev keyboard)
  (define reportdesc (usbhid-device-collection hiddev))
  (define (send make/break report page id)
    (put-message (keyboard-event-channel keyboard)
                 (cons keyboard (vector make/break (list 'HID report) page id #f))))
  (let lp ((down-keys (make-hashtable equal-hash equal?)))
    (match (perform-operation
            (choice-operation (wrap-operation (get-operation (keyboard-command-channel keyboard))
                                              (lambda (x) (cons 'cmd x)))
                              (wrap-operation (usb-pipe-in-operation (usbhid-device-in-pipe hiddev))
                                              (lambda (x) (cons 'report x)))))
      [('report . report)
       ;; Keys are sent as variable or array items. Items appearing in
       ;; the array means the key was pressed. If it's absent in the
       ;; following report then it was released. Variables are always
       ;; present, but have a non-zero value to indicate that they are
       ;; pressed.
       (lp (usb-hid-keyboard-parse-report reportdesc report down-keys send))]
      [('cmd . cmd)
       (match cmd
         [('set-leds state)             ;state according to the LED page
          ;; FIXME: Hack! Should use the HID output items!
          (let ((bv (make-bytevector 1)))
            (bytevector-u8-set! bv 0 (fxarithmetic-shift-right state 1))
            (usbhid-device-set-output-report hiddev bv 100))]
         [('set-repeat-rate rate delay)
          (send-log WARNING "TODO: Set the repeat rate")]
         [x
          (send-log WARNING (string-append "Unknown keyboard command"
                                           (call-with-string-output-port
                                             (lambda (p) (write x p)))))])
       (lp down-keys)]))))
