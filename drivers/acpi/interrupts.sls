;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Multiple APIC Description Table

;; This code deals with ACPI's interrupt information.

(library (loko drivers acpi interrupts)
  (export
    MADT-signature
    parse-madt
    madt?
    madt-&lapic
    madt-flags
    madt-entries

    madt-pc/at-compatible?

    madt-lapic?
    madt-lapic-acpi-processor-id
    madt-lapic-apic-id
    madt-lapic-flags

    madt-ioapic?
    madt-ioapic-id
    madt-ioapic-&addr
    madt-ioapic-int-base

    madt-override?
    madt-override-bus
    madt-override-source
    madt-override-gsi
    madt-override-polarity
    madt-override-trigger-mode

    madt-nmi-source?
    madt-nmi-source-gsi
    madt-nmi-source-polarity
    madt-nmi-source-trigger-mode

    madt-lapic-nmi?
    madt-lapic-nmi-acpi-processor-id
    madt-lapic-nmi-polarity
    madt-lapic-nmi-trigger-mode
    madt-lapic-nmi-lapic-lintn

    madt-lapic-override?
    madt-lapic-override-&lapic)
  (import
    (rnrs)
    (struct pack))

(define MADT-signature "APIC")

(define sizeof-sdt 36)

(define MADT-LOCAL-APIC 0)
(define MADT-IOAPIC 1)
(define MADT-INTERRUPT-SOURCE-OVERRIDE 2)
(define MADT-NMI 3)
(define MADT-LOCAL-APIC-NMI 4)
(define MADT-LOCAL-APIC-ADDRESS-OVERRIDE 5)
(define MADT-IOSAPIC 6)
(define MADT-LOCAL-SAPIC 7)
(define MADT-PLATFORM-INTERRUPT-SOURCE 8)
(define MADT-LOCAL-X2APIC 9)
(define MADT-LOCAL-X2APIC-NMI 10)
(define MADT-LOCAL-GIC #xB)
(define MADT-GIC-DISTRIBUTOR #xC)

(define PCAT_COMPAT #b1)

(define-record-type madt
  (sealed #t)
  (fields &lapic
          flags
          entries))

(define (madt-pc/at-compatible? madt)
  (not (eqv? 0 (fxand PCAT_COMPAT (madt-flags madt)))))

(define-record-type madt-lapic
  (sealed #t)
  (fields acpi-processor-id apic-id flags))

(define-record-type madt-ioapic
  (sealed #t)
  (fields id &addr int-base))

(define-record-type madt-override
  (sealed #t)
  (fields bus source gsi polarity trigger-mode))

(define-record-type madt-nmi-source
  (sealed #t)
  (fields gsi polarity trigger-mode))

(define-record-type madt-lapic-nmi
  (sealed #t)
  (fields acpi-processor-id       ;#xFF = applicable to all processors
          polarity trigger-mode lapic-lintn))

(define-record-type madt-lapic-override
  (sealed #t)
  (fields &lapic))

(define (decode-polarity x)
  (case x
    ((#b00) 'bus-default)
    ((#b01) 'active-high)
    ((#b11) 'active-low)
    (else #f)))

(define (decode-trigger-mode x)
  (case x
    ((#b00) 'bus-default)
    ((#b01) 'edge-triggered)
    ((#b11) 'level-triggered)
    (else #f)))

(define (parse-madt-entry type data)
  (case type
    ((0)                                ;Processor Local APIC
     (and (eqv? (bytevector-length data) (format-size "=uCCL"))
          (let-values ([(acpi-processor-id apic-id flags) (unpack "=uCCL" data)])
            (make-madt-lapic acpi-processor-id apic-id flags))))
    ((1)                                ;I/O APIC
     (and (eqv? (bytevector-length data) (format-size "=uCxLL"))
          (let-values ([(id &ioapic int-base) (unpack "=uCxLL" data)])
            (make-madt-ioapic id &ioapic int-base))))
    ((2)                                ;Interrupt Source Override
     (and (eqv? (bytevector-length data) (format-size "=uCCLS"))
          (let-values ([(bus source gsi flags) (unpack "=uCCLS" data)])
            (let ((polarity (decode-polarity (fxbit-field flags 0 2)))
                  (trigger-mode (decode-trigger-mode (fxbit-field flags 2 4))))
              (make-madt-override bus source gsi polarity trigger-mode)))))
    ((3)                                ;Non-maskable Interrupt Source (NMI)
     (and (eqv? (bytevector-length data) (format-size "=uSL"))
          (let-values ([(flags gsi) (unpack "=uSL" data)])
            (let ((polarity (decode-polarity (fxbit-field flags 0 2)))
                  (trigger-mode (decode-trigger-mode (fxbit-field flags 2 4))))
              (make-madt-nmi-source gsi polarity trigger-mode)))))
    ((4)                                ;Local APIC NMI
     (and (eqv? (bytevector-length data) (format-size "=uCSC"))
          (let-values ([(acpi-processor-id flags lapic-lintn)
                        (unpack "=uCSC" data)])
            (let ((polarity (decode-polarity (fxbit-field flags 0 2)))
                  (trigger-mode (decode-trigger-mode (fxbit-field flags 2 4))))
              (make-madt-lapic-nmi acpi-processor-id polarity trigger-mode lapic-lintn)))))
    ((5)                                ;Local APIC Address Override
     (and (eqv? (bytevector-length data) (format-size "=uxxQ"))
          (let-values ([(&lapic) (unpack "=uxxQ" data)])
            (make-madt-lapic-override &lapic))))

    (else #f)))

(define (parse-madt madt-port)
  (set-port-position! madt-port sizeof-sdt)
  (let-values ([(&lapic flags) (get-unpack madt-port "=LL")])
    (let ((entries (let lp ()
                     (if (port-eof? madt-port)
                         '()
                         (let-values ([(type len) (get-unpack madt-port "CC")])
                           (let* ((data (get-bytevector-n madt-port (fx- len 2)))
                                  (entry (parse-madt-entry type data)))
                             (cons (or entry data) (lp))))))))
      (make-madt &lapic flags entries)))))
