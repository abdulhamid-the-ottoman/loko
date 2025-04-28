;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; Serial ATA transport layer

;; SATA transfers data between the host and the devices using
;; something called a Frame Information Structure (FIS). This library
;; converts from ATA8-ACS commands/returns to FIS and vice versa.

(library (loko drivers ata sata)
  (export
    ata-command-FIS
    ata-control-FIS
    bytevector->FIS
    FIS->ATA8-ACS-return)
  (import
    (rnrs (6))
    (struct pack)
    (loko match))

;; Constants for the FIS type field. Many of these are only observed
;; on the wire.
(define FIS:host->dev:register        #x27)
(define FIS:dev->host:register        #x34)
(define FIS:dev->host:set-device-bits #xa1)
(define FIS:dev->host:DMA-activate    #x39)
(define FIS:bidir:DMA-setup           #x41)
(define FIS:bidir:BIST-activate       #x58)
(define FIS:dev->host:PIO-setup       #x5f)
(define FIS:bidir:data                #x46)

;; Represent a transfer of the register set from the host to the
;; device. If it's a command update then set command-update? to #t.
(define (make-FIS-register-host->dev pm-port command-update? command
                                     features lba device count
                                     icc control auxiliary)
  (pack "<5L"
        (fxior FIS:host->dev:register
               (if command-update? (expt 2 15) 0)
               (fxarithmetic-shift-left pm-port 8)
               (fxarithmetic-shift-left command 16)
               (fxarithmetic-shift-left (fxbit-field features 0 8) 24))
        (fxior (fxbit-field lba 0 24)
               (fxarithmetic-shift-left (fxbit-field device 8 16) 24))
        (fxior (fxbit-field lba 24 48)
               (fxarithmetic-shift-left (fxbit-field features 8 16) 24))
        (fxior count
               (fxarithmetic-shift-left icc 16)
               (fxarithmetic-shift-left control 24))
        auxiliary))

;; Take an ACS command and convert it to a FIS.
(define (ata-command-FIS cmd pm-port)
  (let ((control 0))
    (match cmd
      [#(feature sector-count lba device command)
       (make-FIS-register-host->dev pm-port #t command
                                    feature lba device sector-count
                                    0 control 0)]
      [#(feature sector-count lba device command icc auxiliary)
       (make-FIS-register-host->dev pm-port #t command
                                    feature lba device sector-count
                                    icc control auxiliary)])))

(define (ata-control-FIS control pm-port)
  (make-FIS-register-host->dev pm-port #f 0
                               0 0 0 0
                               0 control 0))

(define-record-type FIS-PIO-setup
  (sealed #t)
  (fields pm-port device->host? interrupt?
          status error
          lba device
          count e_status
          transfer-count))

(define-record-type FIS-register-dev->host
  (sealed #t)
  (fields pm-port interrupt?
          status error
          lba device
          count))

(define (bytevector->FIS FIS)
  (let ((type (bytevector-u8-ref FIS 0)))
    (cond
      ((and (or (eqv? type FIS:dev->host:PIO-setup)
                (eqv? type FIS:dev->host:register))
            (fx>=? (bytevector-length FIS)
                   (format-size "<xCCC SCC SCx SxC Sxx")))
       (let-values ([(pm-port/flags status error
                      lba0:16 lba16:24 device
                      lba24:40 lba40:48
                      count e_status
                      transfer-count)
                     (unpack "<xCCC SCC SCx SxC Sxx" FIS)])
         (let ((pm-port (fxbit-field pm-port/flags 0 4))
               (device->host? (fxbit-set? pm-port/flags 5))
               (interrupt? (fxbit-set? pm-port/flags 6)))
           (let ((lba (fxior lba0:16
                             (fxarithmetic-shift-left lba16:24 16)
                             (fxarithmetic-shift-left lba24:40 24)
                             (fxarithmetic-shift-left lba40:48 40))))
             (if (eqv? type FIS:dev->host:PIO-setup)
                 (make-FIS-PIO-setup pm-port device->host? interrupt?
                                     status error
                                     lba device count e_status
                                     transfer-count)
                 (make-FIS-register-dev->host pm-port interrupt?
                                              status error
                                              lba device count))))))
      (else #f))))

;; Converts a FIS to an ATA8-ACS compatible return value, which is
;; what's used by the ATA driver layer.
(define (FIS->ATA8-ACS-return FIS)
  (cond ((FIS-PIO-setup? FIS)
         (vector (FIS-PIO-setup-error FIS)
                 (FIS-PIO-setup-count FIS)
                 (FIS-PIO-setup-lba FIS)
                 (FIS-PIO-setup-e_status FIS)))
        ((FIS-register-dev->host? FIS)
         (vector (FIS-register-dev->host-error FIS)
                 (FIS-register-dev->host-count FIS)
                 (FIS-register-dev->host-lba FIS)
                 (FIS-register-dev->host-status FIS)))
        (else #f)))


#;(bytevector->FIS #vu8(95 96 88 0 0 0 0 0 0 0 0 0 255 0 0 80 0 2 0 0))

)