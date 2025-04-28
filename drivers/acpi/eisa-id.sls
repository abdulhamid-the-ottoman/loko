;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; EISA IDs

;; XXX: bit 7 of byte zero is reserved.

(library (loko drivers acpi eisa-id)
  (export
    eisa-id->text-id
    text-id->eisa-id)
  (import
    (rnrs))

(define (bswap16 n)
  (fxior (fxarithmetic-shift-left (fxbit-field n 0 8) 8)
         (fxbit-field n 8 16)))

;; Takes an integer representing an EISA ID and turns it into a
;; string. Example ID: PNP0A03.
(define (eisa-id->text-id n)
  (define hex "0123456789ABCDEF")
  (let ((prefix (bswap16 n)))
    (string (integer->char (fx+ #x40 (fxbit-field prefix 10 15)))
            (integer->char (fx+ #x40 (fxbit-field prefix 5 10)))
            (integer->char (fx+ #x40 (fxbit-field prefix 0 5)))
            (string-ref hex (fxbit-field n 20 24))
            (string-ref hex (fxbit-field n 16 20))
            (string-ref hex (fxbit-field n 28 32))
            (string-ref hex (fxbit-field n 24 28)))))

;; Takes a string and converts it to a numeric EISA ID.
(define (text-id->eisa-id x)
  (let ((n (bswap16 (string->number (substring x 3 7) 16)))
        (prefix
         (bswap16
          (fxior (fxarithmetic-shift-left (fx- (char->integer (string-ref x 0)) #x40) 10)
                 (fxarithmetic-shift-left (fx- (char->integer (string-ref x 1)) #x40) 5)
                 (fx- (char->integer (string-ref x 2)) #x40)))))
    (fxior prefix (fxarithmetic-shift-left n 16)))))
