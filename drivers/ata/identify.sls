;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2021 G. Weinholt
#!r6rs

;;; ATA/ATAPI "IDENTIFY (PACKET) DEVICE" parsing

;; This is a parser for the 512 byte block that ATA/ATAPI devices send
;; in response to IDENTIFY DEVICE. A variation is the block from the
;; IDENTIFY PACKET DEVICE command. Another variation is between PATA
;; and SATA devices, which have some variations in which fields are
;; valid.

(library (loko drivers ata identify)
  (export
    ata-identify:ata-device?
    ata-identify:atapi-device?
    ata-identify:serial-number
    ata-identify:firmware-revision
    ata-identify:model-number
    ata-identify:dma-supported?
    ata-identify:selected-dma
    ata-identify:major-revision
    ata-identify:supported-command-set
    ata-identify:enabled-command-set

    ;; ATA-specific
    ata-identify:ata-total-sectors
    ata-identify:ata-lba-supported?
    ata-identify:ata-max-sectors-per-interrupt
    ata-identify:ata-current-sectors-per-interrupt
    ata-identify:ata-sector-size
    ata-identify:ata-queue-depth

    ;; ATAPI-specific
    ata-identify:atapi-dmadir-required?
    ata-identify:atapi-packet-length)
  (import
    (rnrs (6)))

(define (copy-identify-string source start length)
  (define (bytevector-swab-u16! source source-start target target-start k)
    (do ((ts target-start (fx+ ts 2))
         (ss source-start (fx+ ss 2))
         (k k (fx- k 2)))
        ((fx<=? k 0))
      (let ((x (bytevector-u16-ref source ss (endianness big))))
        (bytevector-u16-set! target ts x (endianness little)))))
  (define (string-trim-right str)
    (let lp ((i (fx- (string-length str) 1)))
      (cond ((eqv? i -1) "")
            ((char=? #\space (string-ref str i)) (lp (fx- i 1)))
            (else (substring str 0 (fx+ i 1))))))
  (let ((ret (make-bytevector length)))
    (bytevector-swab-u16! source (fx* start 2) ret 0 length)
    (string-trim-right (utf8->string ret))))

(define (word-ref block word)
  (bytevector-u16-ref block (fx* word 2) (endianness little)))

(define (word-bit-set? block word bit)
  (fxbit-set? (word-ref block word) bit))

(define (ata-identify:ata-device? block)
  (not (word-bit-set? block 0 15)))

(define (ata-identify:atapi-device? block)
  (eqv? #b10 (fxbit-field (word-ref block 0) 14 16)))

(define (ata-identify:serial-number block)
  (copy-identify-string block 10 20))

(define (ata-identify:firmware-revision block)
  (copy-identify-string block 23 8))

(define (ata-identify:model-number block)
  (copy-identify-string block 27 40))

(define (ata-identify:ata-total-sectors block)
  (let ((number-sectors-legacy (fxior (fxarithmetic-shift-left (word-ref block 57) 16)
                                      (word-ref block 58)))
        (number-sectors-32 (bitwise-ior (bitwise-arithmetic-shift-left (word-ref block 61) 16)
                                        (word-ref block 60)))
        ;; Maximum user LBA + 1
        (number-sectors-64 (bitwise-ior (bitwise-arithmetic-shift-left (word-ref block 103) 48)
                                        (bitwise-arithmetic-shift-left (word-ref block 102) 32)
                                        (bitwise-arithmetic-shift-left (word-ref block 101) 16)
                                        (word-ref block 100)))
        (uses-lba48 (word-bit-set? block 83 10)))
    (cond (uses-lba48
           number-sectors-64)
          ((not (eqv? number-sectors-32 0))
           number-sectors-32)
          ((word-bit-set? 53 0)
           number-sectors-legacy)
          (else 0))))

;; Sectors per DRQ data block for READ and WRITE MULTIPLE
(define (ata-identify:ata-max-sectors-per-interrupt block)
  (let ((words (word-ref block 47)))
    (if (fxbit-set? words 15)
        (fxbit-field words 0 8)
        1)))

(define (ata-identify:ata-lba-supported? block)
  (word-bit-set? block 49 9))

(define (ata-identify:dma-supported? block)
  (word-bit-set? block 49 8))

(define (ata-identify:ata-current-sectors-per-interrupt block)
   (let ((words (word-ref block 59)))
     (if (fxbit-set? words 8)
         (fxbit-field words 0 8)
         1)))

(define (ata-identify:selected-dma block)
  (or (and
        (word-bit-set? block 53 2)
        (let ((ultra (word-ref block 88)))
          (cond ((fxbit-set? ultra 14) 'udma6)
                ((fxbit-set? ultra 13) 'udma5)
                ((fxbit-set? ultra 12) 'udma4)
                ((fxbit-set? ultra 11) 'udma3)
                ((fxbit-set? ultra 10) 'udma2)
                ((fxbit-set? ultra 9) 'udma1)
                ((fxbit-set? ultra 8) 'udma0)
                (else #f))))
      (let ((mdma (word-ref block 63)))
        (cond ((fxbit-set? mdma 10) 'mdma2)
              ((fxbit-set? mdma 9) 'mdma1)
              ((fxbit-set? mdma 8) 'mdma0)
              (else #f)))))

(define (ata-identify:ata-queue-depth block)
  (fx+ 1 (fxbit-field (word-ref block 75) 0 5)))

(define (ata-identify:major-revision block)
  (let ((x (word-ref block 80)))
    (if (or (eqv? x #x0000) (eqv? x #xFFFF))
        '()
        (append (if (fxbit-set? x 12) '(ACS-5) '())
                (if (fxbit-set? x 11) '(ACS-4) '())
                (if (fxbit-set? x 10) '(ACS-3) '())
                (if (fxbit-set? x 9)  '(ACS-2) '())
                (if (fxbit-set? x 8)  '(ATA8-ACS) '())
                (if (fxbit-set? x 7)  '(ATA/ATAPI-7) '())
                (if (fxbit-set? x 6)  '(ATA/ATAPI-6) '())
                (if (fxbit-set? x 5)  '(ATA/ATAPI-5) '())
                (if (fxbit-set? x 4)  '(ATA/ATAPI-4) '())))))

(define (ata-identify:supported-command-set block)
  (append
   (let ((x (word-ref block 82)))
     (append (if (fxbit-set? x 14) '(cmd:NOP) '())
             (if (fxbit-set? x 13) '(cmd:READ-BUFFER) '())
             (if (fxbit-set? x 12) '(cmd:WRITE-BUFFER) '())
             (if (fxbit-set? x 10) '(feature:host-protected-area) '())
             (if (fxbit-set? x 9) '(cmd:DEVICE-RESET) '())
             (if (fxbit-set? x 8) '(interrupt:SERVICE) '())
             (if (fxbit-set? x 7) '(interrupt:release) '())
             (if (fxbit-set? x 6) '(feature:look-ahead) '())
             (if (fxbit-set? x 5) '(feature:write-cache) '())
             (if (fxbit-set? x 4) '(feature:PACKET) '())
             (if (fxbit-set? x 3) '(feature:mandatory-power-management) '())
             (if (fxbit-set? x 1) '(feature:security-mode) '())
             (if (fxbit-set? x 0) '(feature:SMART) '())))
   (let ((x (word-ref block 83)))
     (append (if (fxbit-set? x 10) '(feature:LBA48) '())
             (if (fxbit-set? x 1) '(feature:TCQ) '())))
   (let ((x (word-ref block 119)))
     (append (if (fxbit-set? x 6) '(feature:sense-data-reporting) '())))))

(define (ata-identify:enabled-command-set block)
  (append
   (let ((x (word-ref block 120)))
     (append (if (fxbit-set? x 6) '(feature:sense-data-reporting) '())))))

;; Returns logical and physical sector sizes.
(define (ata-identify:ata-sector-size block)
  (let ((x (word-ref block 106)))
    (if (not (and (fxbit-set? x 14) (not (fxbit-set? x 15))))
        (values 512 512)
        (let ((multiple (expt 2 (fxbit-field x 0 4)))
              (logical-size
               (if (fxbit-set? x 12)
                   (bitwise-ior (bitwise-arithmetic-shift-left (word-ref block 118) 16)
                                (word-ref block 117))
                   512)))
          (if (fxbit-set? x 13)
              (values logical-size (fx* logical-size multiple))
              (values logical-size logical-size))))))

;; PACKET commands either require or shouldn't use the DMADIR bit
(define (ata-identify:atapi-dmadir-required? block)
  (word-bit-set? block 62 15))

(define (ata-identify:atapi-packet-length block)
  (case (fxbit-field (word-ref block 0) 0 2)
    ((#b00) 12)
    ((#b01) 16)
    (else #f))))
