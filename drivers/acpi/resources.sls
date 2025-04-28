;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; ACPI resource format

(library (loko drivers acpi resources)
  (export
    acpi-get-resource

    resource-interrupt?
    resource-interrupt-wake-capable?
    resource-interrupt-shared?
    resource-interrupt-active-low?
    resource-interrupt-edge-triggered?
    resource-interrupt-consumer?
    resource-interrupt-interrupt-number*

    resource-unsupported?

    resource-end-tag?)
  (import
    (rnrs))

(define-record-type resource-interrupt
  (sealed #t)
  (fields wake-capable?
          shared?
          active-low?
          edge-triggered?
          consumer?
          interrupt-number*))

;; Flags from the extended interruptor descriptor (6.4.3.6 in ACPI 6.4)
(define RESINT-WKC      4)
(define RESINT-SHR      3)
(define RESINT-LL       2)
(define RESINT-HE       1)
(define RESINT-CONSUMER 0)

(define-record-type resource-unsupported
  (sealed #t)
  (fields size item-name data))

(define-record-type resource-end-tag
  (sealed #t)
  (fields data))

(define (get-bytevector-n* port len)
  (let ((bv (get-bytevector-n port len)))
    (cond ((eof-object? bv) #f)
          ((not (fx=? len (bytevector-length bv))) #f)
          (else bv))))

(define (acpi-get-resource port)
  (let ((b0 (get-u8 port)))
    (cond
      ((eof-object? b0)
       (eof-object))
      ((not (fxbit-set? b0 7))
       ;; Small resource item
       (let* ((item-name (fxbit-field b0 3 7))
              (len (fxbit-field b0 0 3))
              (data (get-bytevector-n* port len)))
         (case item-name
           #;
           ((#x04)
            )
           ((#x0F)
            (make-resource-end-tag data))
           (else
            (make-resource-unsupported 'small item-name data)))))
      ((get-bytevector-n* port 2) =>
       (lambda (bv)
         (let* ((item-name (fxbit-field b0 0 7))
                (len (bytevector-u16-ref bv 0 (endianness little)))
                (data (get-bytevector-n* port len)))
           ;; TODO: Advanced Configuration and Power Interface (ACPI) Specification, Version 6.4
           ;; pp 427
           (let ((port (open-bytevector-input-port data)))
             (case item-name
               ((#x09)
                (let* ((flags (get-u8 port))
                       (number-of-interrupts (get-u8 port)))
                  (and
                    (fixnum? len)
                    (let* ((ints-bv (get-bytevector-n* port (fx* 4 number-of-interrupts)))
                           (ints (and ints-bv (bytevector->uint-list ints-bv (endianness little) 4))))
                      ;; TODO: This can be followed by index+source
                      (and
                        (port-eof? port)
                        (make-resource-interrupt (fxbit-set? flags RESINT-WKC)
                                                 (fxbit-set? flags RESINT-SHR)
                                                 (fxbit-set? flags RESINT-LL)
                                                 (fxbit-set? flags RESINT-HE)
                                                 (fxbit-set? flags RESINT-CONSUMER)
                                                 ints))))))
               (else
                (make-resource-unsupported 'large item-name data)))))))
      (else #f))))

#;
(let ((port (open-bytevector-input-port #vu8(137 6 0 9 1 20 0 0 0 121 0))))
  (let* ((a (acpi-get-resource port))
         (b (acpi-get-resource port)))
    (list a b)))

)
