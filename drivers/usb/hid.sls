;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; USB Human Interface Device (HID) driver

;; FIXME: handle reports that span multiple transactions

(library (loko drivers usb hid)
  (export
    probe·usb·hid?
    driver·usb·hid

    usbhid-device?
    usbhid-device-usbdev
    usbhid-device-interface
    usbhid-device-collection
    usbhid-device-in-pipe
    usbhid-device-out-pipe
    usbhid-device-set-output-report
    )
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko drivers usb core)
    (loko drivers usb hid-parser))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'usb-hid))

(define (log/debug . x*)
  (log/x DEBUG x*))

(define (log/error . x*)
  (log/x ERROR x*))

(define-syntax define-vu8-ref
  (lambda (x)
    (syntax-case x ()
      ((_ name index)
       #'(define-vu8-ref name index 1))
      ((_ name index size)
       (eqv? (syntax->datum #'size) 1)
       #'(define (name desc) (bytevector-u8-ref desc index)))
      ((_ name index size)
       (eqv? (syntax->datum #'size) 2)
       #'(define (name desc) (bytevector-u16-ref desc index (endianness little)))))))

;; HID descriptor fields
(define-vu8-ref hiddesc-bcdHID            2 2)
(define-vu8-ref hiddesc-bCountryCode      4)
(define-vu8-ref hiddesc-bNumDescriptors   5)
(define (hiddesc-bDescriptorType desc i)
  (bytevector-u8-ref desc (fx+ 6 (fx* 2 i))))
(define (hiddesc-wDescriptorLength desc i)
  (bytevector-u16-ref desc (fx+ 7 (fx* 2 i)) (endianness little)))

(define-record-type usbhid-device
  (sealed #t)
  (fields usbdev interface collection in-pipe out-pipe))

;; bRequest values
(define GET_REPORT   #x01)
(define GET_IDLE     #x02)
(define GET_PROTOCOL #x03)
(define SET_REPORT   #x09)
(define SET_IDLE     #x0A)
(define SET_PROTOCOL #x0B)

;; Report types (wValue). Set the low byte to the report ID, or 0 if
;; unused.
(define report-type-input #x0100)
(define report-type-output #x0200)
(define report-type-feature #x0300)

(define (probe·usb·hid? _dev interface)
  (eqv? #x03 (intdesc-bInterfaceClass (usb-interface-descriptor interface))))

(define (usb-hid-fetch-report-descriptor dev interface)
  (define interface-number
    (intdesc-bInterfaceNumber (usb-interface-descriptor interface)))
  (define hiddesc
    (find (lambda (desc) (eqv? (desc-bDescriptorType desc) 33))
          (usb-interface-descriptor* interface)))
  (let lp ((i 0))
    (cond ((fx=? i (hiddesc-bNumDescriptors hiddesc))
           (error 'usb-hid-driver "No HID report descriptor for interface"
                  dev interface))
          ((eqv? #x22 (hiddesc-bDescriptorType hiddesc i))
           (let ((bv (make-bytevector (hiddesc-wDescriptorLength hiddesc i))))
             (usb-control-transfer dev #x81 6 #x2200 interface-number bv 100)
             (log/debug "Descriptor " i ": " bv)
             (let ((report-desc (parse-hid-report-descriptor bv)))
               (log/debug (call-with-string-output-port
                            (lambda (p)
                              (print-hid-records report-desc p))))
               report-desc)))
          (else (lp (fx+ i 1))))))

(define (usbhid-device-set-output-report hiddev bv timeout)
  (let* ((report-id (hid-collection-report-id (usbhid-device-collection hiddev)))
         (bv (if (not report-id)
                bv
                (call-with-bytevector-output-port
                  (lambda (p)
                    (put-u8 p report-id)
                    (put-bytevector p bv))))))
    (cond
      ((usbhid-device-out-pipe hiddev) =>
       (lambda (out-pipe)
         ;; Use the OUT endpoint if there is one. It's not mandatory
         ;; for devices to have one.
         (let ((resp-ch (make-channel)))
           (put-message (usb-pipe-out-operation out-pipe)
                        (list resp-ch bv timeout))
           (get-message resp-ch))))
      (else
       ;; Otherwise send the report data in Set_Report to the default
       ;; control endpoint.
       (guard (exn ((serious-condition? exn)
                    (log/error "Failed to send SET_REPORT: " exn)))
         (let ((report-type #x02) ;output
               (report-id (or report-id 0)))
           (usb-control-transfer (usbhid-device-usbdev hiddev) #b00100001 SET_REPORT
                                 (fxior (fxarithmetic-shift-left report-type 8)
                                        report-id)
                                 (intdesc-bInterfaceNumber (usb-interface-descriptor
                                                            (usbhid-device-interface hiddev)))
                                 bv timeout)))))))

;; Driver for single HID interfaces on a configuration. The HID
;; interface can have multiple top-level HID collections, and each one
;; warrants a driver.
(define (driver·usb·hid dev interface start-hid-driver)
  (define in-pipes (filter usb-pipe-in? (usb-interface-pipes interface)))
  (define out-pipe (find usb-pipe-out? (usb-interface-pipes interface)))
  (define top-levels (filter hid-collection? (usb-hid-fetch-report-descriptor dev interface)))

  (log/debug "USB HID driver starting")
  (usb-setup-pipes dev interface)

  ;; Switch from the boot protocol to the report protocol
  (when (eqv? 1 (intdesc-bInterfaceSubClass (usb-interface-descriptor interface)))
    (usb-control-transfer dev #b00100001 SET_PROTOCOL 1 ;report protocol
                          (intdesc-bInterfaceNumber (usb-interface-descriptor interface))
                          #vu8() 500))

  ;; Set it to idle for an indefinite duration. Specific drivers can
  ;; change this themselves later.
  (guard (exn ((serious-condition? exn)
               (log/debug "SET_IDLE request failed")))
    (usb-control-transfer dev #b00100001 SET_IDLE #x0000
                          (intdesc-bInterfaceNumber (usb-interface-descriptor interface))
                          #vu8() 500))

  ;; There should be one driver per top-level collection. If there are
  ;; multiple collections: start a fiber per IN pipe and route based
  ;; on report IDs on the messages. If a single pipe: find a driver,
  ;; but skip the routing part.

  (cond
    ((and (eqv? (length in-pipes) 1)
          (eqv? (length top-levels) 1))
     ;; The simple case of a single IN pipe and a single top-level collection.
     (let* ((hid-collection (car top-levels))
            (hiddev (make-usbhid-device dev interface hid-collection (car in-pipes) out-pipe)))
       (start-hid-driver hiddev)))
    (else
     ;; Start a driver for each top-level collection and remember
     ;; which report-id they had.
     (let ((devs (make-eqv-hashtable)))
       (for-each (lambda (collection)
                   ;; FIXME: the endpoint descriptor is misleading
                   (let* ((in-ch (make-channel))
                          (fake-in-pipe (make-usb-pipe (usb-pipe-descriptor (car in-pipes)) in-ch))
                          (hiddev (make-usbhid-device dev interface collection fake-in-pipe out-pipe)))
                     (spawn-fiber (lambda () (start-hid-driver hiddev)))
                     (hashtable-set! devs (hid-collection-report-id collection) in-ch)))
                 top-levels)

       (let lp ()
         (match (perform-operation
                 (apply choice-operation
                        (map (lambda (pipe)
                               (wrap-operation (usb-pipe-in-operation pipe)
                                               (lambda (msg) (cons pipe msg))))
                             in-pipes)))
           [(pipe . msg)
            (when (and (bytevector? msg) (not (eqv? 0 (bytevector-length msg))))
              ;; Route based on the report-id in the message
              (let ((report-id (bytevector-u8-ref msg 0)))
                (cond
                  ((hashtable-ref devs report-id #f) =>
                   (lambda (in-ch)
                     ;; FIXME: This can stall forever if we only found
                     ;; a driver for one of the collections, or if one
                     ;; driver isn't working right, etc.
                     (put-message in-ch msg)))
                  (else
                   (log/debug "Report id matches no collection: " report-id)))))
            (lp)])))))))
