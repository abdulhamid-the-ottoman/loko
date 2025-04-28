;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; List information about disks

(import
  (rnrs (6))
  (loko match)
  (loko system fibers)
  (loko system logging)
  (loko drivers ata atapi)
  (loko drivers ata core)
  (loko drivers ata drive)
  (loko drivers ata ide)
  (loko drivers ata identify)
  (loko drivers pci)
  (loko drivers scsi block)
  (loko drivers scsi core)
  (loko drivers storage)
  (loko drivers usb core)
  (loko drivers usb mass-storage)
  (loko drivers usb uhci)
  (loko drivers usb ehci)
  (loko kernel storage)
  (fs partitions common)
  (fs partitions mbr)
  (fs partitions gpt)
  (fs fatfs))

(define devs (pci-scan-bus #f))

(define logch (make-channel))

(current-log-callback
 (lambda (e)
   (put-message logch e)))

(spawn-fiber
 (lambda ()
   (let lp ()
     (let ((e (get-message logch)))
       (define p (current-error-port))
       (define severity (cdr (assq 'SEVERITY e)))
       (define subsys (cond ((assq 'SUBSYSTEM e) => cdr) (else #f)))
       (display "<" p)
       (display severity p)
       (display ">" p)
       (when subsys
         (display "[" p)
         (display subsys p)
         (display "] " p))
       (cond ((eqv? severity ERROR)
              (display "\x1b;[1;41;33m" p))
             ((eqv? severity INFO)
              (display "\x1b;[1;44;33m" p)))
       (display (cdr (assq 'MESSAGE e)) p)
       (cond ((assq 'EXCEPTION e) =>
              (lambda (e)
                (newline p)
                (display (cdr e) p))))
       (display "\x1b;[m" p)
       (newline p)
       (lp)))))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*)
  (log/x DEBUG x*))

(define (log/info . x*)
  (log/x INFO x*))

(define (log-device-info identify-block)
  (let-values ([(logical physical) (ata-identify:ata-sector-size identify-block)])
    (log/info (list 'model: (ata-identify:model-number identify-block)
                    'serial: (ata-identify:serial-number identify-block)
                    'firmware: (ata-identify:firmware-revision identify-block)
                    'commands: (ata-identify:supported-command-set identify-block)
                    'major-version: (ata-identify:major-revision identify-block)
                    (cond ((ata-identify:ata-device? identify-block) 'ata:)
                          ((ata-identify:atapi-device? identify-block) 'atapi:)
                          (else 'unknown-identify:))
                    (if (ata-identify:ata-device? identify-block)
                        (list 'logical-sector-size: logical
                              'physical-sector-size: physical)
                        (list))
                    'raw: identify-block))))

(define storage-manager-ch (make-channel))

(define (make-sector-reader storage lba-start lba-end)
  (lambda (lba)
    (cond
      ((or (not lba-end) (<= lba-start lba lba-end))
       (let ((resp-ch (make-channel)))
         (put-message (storage-device-request-channel storage)
                      (list resp-ch 'read (+ lba-start lba) 1))
         (match (get-message resp-ch)
           [('ok data)
            data]
           [('error . x)
            (log/info (list "Error reading from medium" lba x))
            (eof-object)])))
      (else
       (log/info "Out of range read")
       (eof-object)))))

(spawn-fiber
 (lambda ()
   (let lp ()
     (match (get-message storage-manager-ch)
       [('new-storage storage)
        (log/info (list 'storage: storage))
        (spawn-fiber
         (lambda ()
           ;; Scan the partition table and list root directories of
           ;; FAT filesystems
           (let ((resp-ch (make-channel)))
             (put-message (storage-device-request-channel storage)
                          (list resp-ch 'read
                                ;; Read sector 0 on normal media and sector 16 on CD/DVD
                                (if (eqv? (storage-device-logical-sector-size storage) 2048)
                                    16
                                    0)
                                1))
             (log/info (list 'sector: (get-message resp-ch))))
           (let ((read-sector (make-sector-reader storage 0 #f))
                 (sector-size (storage-device-logical-sector-size storage)))
             (let ((mbr (read-mbr read-sector)))
               ;; XXX: Real applications should probably check the MBR
               ;; to verify that it's a protective MBR. It could have
               ;; been overwritten with a valid MBR, in which case
               ;; gpt1 is likely an out of date GPT backup.
               (let-values ([(gpt0 gpt1) (read-gpt read-sector #f)])
                 (log/info (list "Partition table:" mbr gpt0 gpt1))
                 (let ((table (cond ((parttable-valid? gpt0) gpt0)
                                    ((parttable-valid? gpt1) gpt1)
                                    (else mbr))))
                   (for-each
                    (lambda (part)
                      (define GPT-TYPE-EFI-System
                        #vu8(#xC1 #x2A #x73 #x28
                                  #xF8 #x1F #x11 #xD2 #xBA #x4B
                                  #x00 #xA0 #xC9 #x3E #xC9 #x3B))
                      (log/info (list "Partition:" part))
                      (when (or (member (part-type part)
                                        '(1 4 6 #xb #xc #xd #xe #xef))
                                (equal? (part-type part) GPT-TYPE-EFI-System))
                        (let* ((dev (open-storage-device storage (part-start-lba part)
                                                         (part-end-lba part)))
                               (fs (open-fatfs dev)))
                          (log/info fs)
                          (log/info (list "FAT root directory:"
                                          (fatfs-directory-list fs '()))))))
                    (parttable-partitions table))))))))])
     (lp))))

(define scsi-manager-ch (make-channel))
(spawn-fiber
 (lambda ()
   (let lp ()
     (match (get-message scsi-manager-ch)
       [('new-device . ch)
        ;; TODO: probe the channel and start an appropriate driver
        (spawn-fiber
         (lambda ()
           (match (probe·scsi ch)
             [((and (or 'SBC 'MMC) class) inq vpd-id)
              (log/info "SCSI SBC/MMC compatible device")
              (log/info (list 'INQUIRY inq 'VPD-ID vpd-id))
              (let* ((scsidev (make-scsi-device ch inq))
                     (storage (let-values ([(max-lba sector-size)
                                            (scsi·block·read-capacity scsidev)])
                                ;; XXX: Maybe this can change later?
                                (let ((sector-size (or sector-size
                                                       (case class
                                                         ((MMC) 2048)
                                                         (else 512)))))
                                  (make-storage-device "SCSI logical unit" sector-size)))))
                (put-message storage-manager-ch (list 'new-storage storage))
                (driver·scsi·block scsidev storage))]
             [(_ inq vpd-id)
              (log/info "No driver for SCSI device")
              (log/info (list 'INQUIRY inq 'VPD-ID vpd-id))])))])
     (lp))))

(define (ata-manager controller)
  (let lp ()
    (match (get-message (ata-controller-notify-channel controller))
      [('new-device . ch)
       ;; Probe the device and start a driver
       (spawn-fiber
        (lambda ()
          (match (probe·ata ch)
            [('ata . identify-block)
             (log/info "ATA drive")
             (log-device-info identify-block)
             (let* ((atadev (make-ata-device controller ch identify-block))
                    (storage (make-storage-device
                              "ATA drive"
                              (let-values ([(logical _)
                                            (ata-identify:ata-sector-size identify-block)])
                                logical))))
               (put-message storage-manager-ch (list 'new-storage storage))
               (driver·ata·drive atadev
                                 storage))]

            [('atapi . identify-block)
             (log/info "ATAPI device")
             (log-device-info identify-block)
             ;; TODO: Make a new SCSI logical unit and set the ATAPI
             ;; device as the request channel
             (let ((scsi-req-ch (make-channel)))
               (put-message scsi-manager-ch (cons 'new-device scsi-req-ch))
               (driver·ata·atapi (make-ata-device controller ch identify-block)
                                 scsi-req-ch))]

            ['no-device
             (log/info "No device")]

            [x
             (log/info (list "No driver:" x))])))
       (lp)])))


(log/info "")
(log/info "Scanning for IDE devices...")
(for-each (lambda (dev)
            (when (probe·pci·ide? dev)
              (log/info dev)
              (let ((controller (make-ata-controller)))
                (spawn-fiber (lambda () (ata-manager controller)))
                (spawn-fiber (lambda () (driver·pci·ide dev controller))))))
          devs)


(define (manage-usb-hci controller)
  (let lp ()
    (match (get-message (usb-controller-notify-channel controller))
      [('new-device . dev)
       (log/debug "Fetching descriptors...")
       ;; The device configuration is already set. At this point we
       ;; should find a driver for the device and spawn a fiber to
       ;; handle it.

       (log/info INFO
                 (call-with-string-output-port
                   (lambda (p)
                     (print-usb-descriptor (usb-get-device-descriptor dev) p)
                     (for-each (lambda (cfgdesc*)
                                 (for-each (lambda (desc)
                                             (print-usb-descriptor desc p))
                                           cfgdesc*))
                               (or (usb-device-$configurations dev) '()))
                     (write dev p))))

       (for-each
        (lambda (interface)
          (cond
            ((probe·usb·mass-storage? dev interface)
             (log/info "USB mass storage device")
             (spawn-fiber
              (lambda ()
                (define (get-scsi-req-channel)
                  (let ((scsi-req-ch (make-channel)))
                    (put-message scsi-manager-ch (cons 'new-device scsi-req-ch))
                    scsi-req-ch))
                (driver·usb·mass-storage dev interface get-scsi-req-channel))))))

        (usb-device-interfaces dev))])
    (lp)))

(for-each
 (lambda (dev)
   (when (probe·pci·ehci? dev)
     (log/info "Found an EHCI controller: " dev)
     (spawn-fiber
      (lambda ()
        (let ((controller (make-usb-controller)))
          ;; (spawn-fiber (lambda () (manage-usb-hci controller)))
          (driver·pci·ehci dev controller))))))
 devs)

(for-each
 (lambda (dev)
   (when (probe·pci·uhci? dev)
     (log/info "Found a UHCI controller: " dev)
     (spawn-fiber
      (lambda ()
        (let ((controller (make-usb-controller)))
          (spawn-fiber (lambda () (manage-usb-hci controller)))
          (driver·pci·uhci dev controller))))))
 devs)


(let lp ()
  (sleep 60)
  (lp))
