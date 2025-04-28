;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Common ACPI table support

(library (loko drivers acpi tables)
  (export
    acpi-table-header?
    make-acpi-table-header
    acpi-table-header-signature
    acpi-table-header-length
    acpi-table-header-revision
    acpi-table-header-checksum
    acpi-table-header-oem-id
    acpi-table-header-oem-table-id
    acpi-table-header-oem-revision
    acpi-table-header-compiler-id
    acpi-table-header-compiler-revision

    acpi-get-table-header)
  (import
    (rnrs))

(define-record-type acpi-table-header
  (sealed #t)
  (fields signature
          length
          revision
          checksum
          oem-id
          oem-table-id
          oem-revision
          compiler-id
          compiler-revision))

(define (acpi-get-table-header port)
  (let* ((sig (get-bytevector-n port 4))
         (len (get-bytevector-n port 4))
         (spec-compliance (get-u8 port))
         (checksum (get-u8 port))
         (oem-id (get-bytevector-n port 6))
         (oem-table-id (get-bytevector-n port 8))
         (oem-revision (get-bytevector-n port 4))
         (creator-id (get-bytevector-n port 4))
         (creator-revision (get-bytevector-n port 4)))
    (unless (and (bytevector? creator-revision)
                 (eqv? (bytevector-length creator-revision) 4))
      (assertion-violation 'parse-aml-bytecode
                           "Eof while reading the ACPI table header" port))
    (make-acpi-table-header (utf8->string sig)
                            (bytevector-u32-ref len 0 (endianness little))
                            spec-compliance checksum
                            ;; FIXME: NUL-termination
                            (utf8->string oem-id)
                            (utf8->string oem-table-id)
                            (bytevector-u32-ref oem-revision 0 (endianness little))
                            (utf8->string creator-id)
                            (bytevector-u32-ref creator-revision 0 (endianness little))))))
