;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; USB mass storage driver

;; SCSI tunneled over USB bulk-only transport.

;; TODO: This is just the minimum required to handle the happy case.
;; There is basically no error handling here; nothing to handle stalls
;; on the bulk pipes

(library (loko drivers usb mass-storage)
  (export
    probe·usb·mass-storage?
    driver·usb·mass-storage)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko drivers storage)
    (loko drivers usb core))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'usb-msc))

(define (log/debug . x*)
  '(log/x DEBUG x*))

(define (probe·usb·mass-storage? _dev interface)
  (let ((intdesc (usb-interface-descriptor interface)))
    (and (eqv? #x08 (intdesc-bInterfaceClass intdesc))
         (eqv? #x06 (intdesc-bInterfaceSubClass intdesc))
         (eqv? #x50 (intdesc-bInterfaceProtocol intdesc)))))

;; Tunnel SCSI for a single LUN
(define (usb-mass-storage-lun-driver dev interface LUN scsi-req-ch)
  (define pipe-in (find usb-pipe-in? (usb-interface-pipes interface)))
  (define pipe-out (find usb-pipe-out? (usb-interface-pipes interface)))
  (define timeout 1000)

  (define CBW-SIGNATURE #x43425355)
  (define CSW-SIGNATURE #x53425355)

  (define %tag 0)
  (define (next-tag!)
    (set! %tag (fxand #x3fffff (fx+ %tag 1))))

  (let lp ()
    (match (get-message scsi-req-ch)

      [(resp-ch 'in (? bytevector? cdb) data-len)
       (next-tag!)
       (let ((data (make-bytevector 31 0))
             (resp (make-bytevector data-len 0)))
         ;; Wrap the command block in a CBW
         (pack! "<LLLCCC" data 0 CBW-SIGNATURE %tag data-len
                #x80 LUN (bytevector-length cdb))
         (bytevector-copy! cdb 0 data (format-size "<LLLCCC") (bytevector-length cdb))
         (log/debug "CDB: " cdb)
         (usb-bulk-write pipe-out data timeout)
         (let ((len (usb-bulk-read pipe-in resp timeout)))
           (let ((resp (if (eqv? data-len len)
                           resp
                           (let ((bv (make-bytevector len)))
                             (bytevector-copy! resp 0 bv 0 len)
                             bv))))
             (log/debug "Response: " resp " len: " len)
             (let ((CSW (make-bytevector (format-size "<LLLC"))))
               (usb-bulk-read pipe-in CSW timeout)
               (log/debug "CSW: " CSW)
               (unless (eqv? CSW-SIGNATURE (bytevector-u32-ref CSW 0 (endianness little)))
                 (error 'usb-msc-command "Bad signature on CSW"))
               (unless (eqv? %tag (bytevector-u32-native-ref CSW 4))
                 (error 'usb-msc-command "Wrong tag on CSW"))
               (let ((csw-status (bytevector-u8-ref CSW 12)))
                 (case csw-status
                   [(#x00) (put-message resp-ch (list 'ok #f resp))]
                   [(#x01) (put-message resp-ch (list 'error 'check-status))]
                   [else
                    ;; This should hopefully symbolize some error in
                    ;; the driver. TODO: reset.
                    (error 'usb-msc-command "Bad CSW status" csw-status)]))))))]

      [(resp-ch 'non-data (? bytevector? cdb))
       (next-tag!)
       (let ((data (make-bytevector 31 0)))
         ;; Wrap the command block in a CBW
         (pack! "<LLLCCC" data 0 CBW-SIGNATURE %tag 0
                #x80 LUN (bytevector-length cdb))
         (bytevector-copy! cdb 0 data (format-size "<LLLCCC") (bytevector-length cdb))
         (usb-bulk-write pipe-out data timeout)
         (let ((CSW (make-bytevector (format-size "<LLLC"))))
           (usb-bulk-read pipe-in CSW timeout)
           (unless (eqv? CSW-SIGNATURE (bytevector-u32-ref CSW 0 (endianness little)))
             (error 'usb-msc-command "Bad signature on CSW"))
           (unless (eqv? %tag (bytevector-u32-native-ref CSW 4))
             (error 'usb-msc-command "Wrong tag on CSW"))
           (let ((csw-status (bytevector-u8-ref CSW 12)))
             (case csw-status
               [(#x00) (put-message resp-ch (list 'ok #f))]
               [(#x01) (put-message resp-ch (list 'error 'check-status))]
               [else
                ;; This should hopefully symbolize some error in
                ;; the driver. TODO: reset.
                (error 'usb-msc-command "Bad CSW status" csw-status)]))))]

      [(resp-ch 'out (? bytevector? cdb) (? bytevector? payload))
       (next-tag!)
       (let ((data (make-bytevector 31 0)))
         ;; Wrap the command block in a CBW
         (pack! "<LLLCCC" data 0 CBW-SIGNATURE %tag (bytevector-length payload)
                #x00 LUN (bytevector-length cdb))
         (bytevector-copy! cdb 0 data (format-size "<LLLCCC") (bytevector-length cdb))
         (usb-bulk-write pipe-out data timeout)
         (usb-bulk-write pipe-out payload timeout)
         (let ((CSW (make-bytevector (format-size "<LLLC"))))
           (usb-bulk-read pipe-in CSW timeout)
           (unless (eqv? CSW-SIGNATURE (bytevector-u32-ref CSW 0 (endianness little)))
             (error 'usb-msc-command "Bad signature on CSW"))
           (unless (eqv? %tag (bytevector-u32-native-ref CSW 4))
             (error 'usb-msc-command "Wrong tag on CSW"))
           (let ((csw-status (bytevector-u8-ref CSW 12)))
             (case csw-status
               [(#x00) (put-message resp-ch (list 'ok #f))]
               [(#x01) (put-message resp-ch (list 'error 'check-status))]
               [else
                ;; This should hopefully symbolize some error in
                ;; the driver. TODO: reset.
                (error 'usb-msc-command "Bad CSW status" csw-status)]))))]

      [(resp-ch . x)
       (put-message resp-ch (list 'error 'bad-request))])
    (lp)))

(define (driver·usb·mass-storage dev interface get-scsi-req-channel)
  (define req-Get-Max-LUN #xFE)
  (define req-Reset       #xFF)
  (define (get-max-lun dev interface)
    (let ((bv (make-bytevector 1)))
      (let ((status
             (usb-control-transfer dev #b10100001 req-Get-Max-LUN 0
                                   (intdesc-bInterfaceNumber (usb-interface-descriptor interface))
                                   bv 100)))
        (if (eq? status 'stalled)
            0
            (let ((max-lun (bytevector-u8-ref bv 0)))
              (if (eqv? max-lun #xFF) 0 max-lun))))))

  (define (reset dev interface)
    (usb-control-transfer dev #b00100001 req-Reset 0
                          (intdesc-bInterfaceNumber (usb-interface-descriptor interface))
                          #vu8() 500))

  (log/debug "USB Mass Storage Class, Bulk-Only Transport")
  (let ((max-lun (get-max-lun dev interface)))
    (log/debug "Max LUN: " max-lun)
    (usb-setup-pipes dev interface)

    ;; Start a fiber per LUN and give each a SCSI request channel.
    (do ((LUN 0 (fx+ LUN 1)))
        ((fx>? LUN max-lun))
      (let ((scsi-req-ch (get-scsi-req-channel)))
        (spawn-fiber
         (lambda ()
           (usb-mass-storage-lun-driver dev interface LUN scsi-req-ch))))))))
