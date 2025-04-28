;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; ACPI platform support

;; Stuff needed to initialize ACPI on the machine, and PCI interrupts.

;; PCI routing tables let us find out how the hamster wheels spun when
;; the motherboard's PCI devices were hooked up to the pins on the
;; interrupt controllers.
;;
;; PCI devices can send these interrupts: INTA, INTB, INTC, INTD.
;; They are connected in some random way to the PIC and/or I/O APICs.
;; A _PRT looks something like this:
;;
;; Example:
;; #(#(65535 0 #<Device (root "_SB" "GSIE")> 0)
;;   #(65535 1 #<Device (root "_SB" "GSIF")> 0)
;;   #(65535 2 #<Device (root "_SB" "GSIG")> 0)
;;   #(65535 3 #<Device (root "_SB" "GSIH")> 0)
;;   #(131071 0 #<Device (root "_SB" "GSIF")> 0)
;;   ...)
;;
;; Each vector has these elements:
;; - a PCI device and function in _ADR format
;; - a pin number (0-4 = INTA-INTD)
;; - "Source"
;; - "Source index"
;;
;; If the function is #xFFFF then it applies to all functions. The bus
;; number is not encoded in this address.
;;
;; If Source is zero then the index is the GSI number for the
;; interrupt and it cannot be changed. This is typically the case when
;; the PIC is used.
;;
;; Otherwise Source is a PCI Interrupt Link Device and the index
;; selects a resource descriptor in the interrupt link device's
;; resource template. The link device's _CRS (current resource
;; setting) method can be used to get to a resource-interrupt record.
;;
;; The root bus has a _PRT and then everything(?) under can also have
;; a _PRT.

(library (loko drivers acpi platform)
  (export
    acpi-load-aml-root
    acpi-initialize-board)
  (import
    (rnrs)
    (only (loko) parameterize)
    (only (loko system r7rs) current-output-port*)
    (loko match)
    (loko system logging)
    (loko drivers acpi aml)
    (loko drivers acpi eisa-id)
    (loko drivers acpi resources))

(define (log/x severity . x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*) (apply log/x DEBUG x*))
(define (log/info . x*) (apply log/x INFO x*))

(define DSDT-signature "DSDT")

;; Arguments to the \_PIC method
(define PIC-method-PIC 0)
(define PIC-method-APIC 1)
(define PIC-method-SAPIC 2)


;; Bits returned by the _STA method on devices
(define DEVICE-STA-PRESENT         0)
(define DEVICE-STA-ENABLED         1)
(define DEVICE-STA-VISIBLE         2)
(define DEVICE-STA-FUNCTIONING     3)
(define DEVICE-STA-BATTERY-PRESENT 4)

;; Return a list of ports for the DSDT and all SSDT tables.
(define (open-aml-tables tables)
  (let lp ((tables tables))
    (if (null? tables)
        '()
        (let* ((x (car tables))
               (name (car x)))
          (cond
            ((or (equal? name DSDT-signature)
                 (and (string? name)
                      (fx>=? (string-length name) 4)
                      (string=? "SSDT" (substring name 0 4))))
             (cons (let ((open (cdr x)))
                     (open))
                   (lp (cdr tables))))
            (else
             (lp (cdr tables))))))))

;; Take a list of ("table-name" . open-procedure) pairs and load the
;; AML from all DSDT and SSDT tables.
(define (acpi-load-aml-root acpi-tables)
  (guard (exn ((serious-condition? exn)
               (send-log CRITICAL "Failed to load DSDT and SSDTs"
                         'EXCEPTION exn)
               #f))
    (let ((ports (open-aml-tables acpi-tables)))
      (log/debug "Loading AML: " ports)
      (aml-load-ports ports))))

;; Check if the _CID or _HID object matches the id.
(define (device-compatible? dev id)
  (define (fumble method-name)
    (cond ((var-find-child dev method-name) =>
           (lambda (var)
             (cond ((and (Name? var) (Name-Object var)) =>
                    (lambda (_cid)
                      (equal? id _cid)))
                   (else #f))))
          (else #f)))
  (or (fumble "_CID")
      (fumble "_HID")))

(define (acpi-initialize-board tables pic-method)
  (define PNP0A03 (text-id->eisa-id "PNP0A03")) ; PCI bus
  (define PNP0C0F (text-id->eisa-id "PNP0C0F")) ; PCI Interrupt Link Device

  (define root (acpi-load-aml-root tables))

  ;; FIXME: use the SMI command stuff in the FADT

  ;; Select a programmable interrupt controller using the optional
  ;; \_PIC method.
  (let ((method (case pic-method
                  ((pic) PIC-method-APIC)
                  ((apic) PIC-method-APIC)
                  ((sapic) PIC-method-SAPIC)
                  (else #f))))
    (when (and method (var-find-child root "_PIC"))
      (aml-eval root `(Call (name root "_PIC") ,method))))

  (send-log DEBUG
            (string-append "AML tree:\n"
                           (call-with-string-output-port
                             (lambda (p)
                               (print-aml-tree p root)))))

  (let* ((pci-bus* (var-filter-children
                    (lambda (dev)
                      (device-compatible? dev PNP0A03))
                    (var-find-child root "_SB"))))
    (log/debug "ACPI reports these PCI Buses: " pci-bus*)

    (for-each (lambda (pci-bus)
                ;; XXX: what is _ADR?
                (when (var-find-child root "_INI")
                  (aml-eval pci-bus `(Call (name "_INI"))))

                (var-filter-children Device? pci-bus)

                (let ((prt (aml-eval pci-bus '(Call (name "_PRT")))))
                  (send-log DEBUG
                            (call-with-string-output-port
                              (lambda (p)
                                (display "PCI routing table:\n" p)
                                (vector-for-each
                                 (lambda (x)
                                   (let ((adr (vector-ref x 0))
                                         (pin (vector-ref x 1)))
                                     (display (number->string (bitwise-bit-field adr 16 32)
                                                              16) p)
                                     (display "." p)
                                     (display (number->string (bitwise-bit-field adr 0 16)
                                                              16) p)
                                     (display " INT" p)
                                     (display (case pin
                                                ((0) #\A)
                                                ((1) #\B)
                                                ((2) #\C)
                                                ((3) #\D)
                                                (else pin))
                                              p)
                                     (match x
                                       [#(_adr _pin 0 idx)
                                        (display " IRQ " p)
                                        (display idx p)]
                                       [#(_adr _pin link idx)
                                        (display #\space p)
                                        (let ((port (open-bytevector-input-port
                                                     (->buffer (aml-eval link '(Call (name "_CRS"))))))
                                              (idx (->integer idx)))
                                          (let lp ((i 0))
                                            (let ((x (acpi-get-resource port)))
                                              (unless (or (eof-object? x)
                                                          (resource-end-tag? x))
                                                (when (= i idx)
                                                  (write x p))
                                                (lp (+ i 1))))))])
                                     (newline p)))
                                 prt))))))
              pci-bus*))

  ;; TODO: Boot the AP processors.

  (var-find-child root "_PR")

  ))
