;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme char)
  (export
    char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=?
    char-ci>? char-downcase char-foldcase char-lower-case?
    char-numeric? char-upcase char-upper-case? char-whitespace?
    digit-value string-ci<=? string-ci<? string-ci=?
    string-ci>=? string-ci>? string-downcase string-foldcase
    string-upcase)
  (import
    (rnrs))

;; The table can be extracted with:
;; awk -F ';' '/ZERO;Nd/ {print "#x"$1}' UnicodeData.txt
;; Up to date with Unicode 13.0.0

(define decimal-zeroes '#(#x0030 #x0660 #x06F0 #x07C0 #x0966 #x09E6
  #x0A66 #x0AE6 #x0B66 #x0BE6 #x0C66 #x0CE6 #x0D66 #x0DE6 #x0E50
  #x0ED0 #x0F20 #x1040 #x1090 #x17E0 #x1810 #x1946 #x19D0 #x1A80
  #x1A90 #x1B50 #x1BB0 #x1C40 #x1C50 #xA620 #xA8D0 #xA900 #xA9D0
  #xA9F0 #xAA50 #xABF0 #xFF10 #x104A0 #x10D30 #x11066 #x110F0 #x11136
  #x111D0 #x112F0 #x11450 #x114D0 #x11650 #x116C0 #x11730 #x118E0
  #x11950 #x11C50 #x11D50 #x11DA0 #x16A60 #x16B50 #x1D7CE #x1D7D8
  #x1D7E2 #x1D7EC #x1D7F6 #x1E140 #x1E2F0 #x1E950 #x1FBF0))

(define (digit-value char)
  (define (cmp zero ch)
    (if (fixnum? ch)
        (fx- (cmp zero ch))
        (let ((i (char->integer ch)))
          (cond ((fx<? i zero) 1)
                ((fx>? i (fx+ zero 9)) -1)
                (else 0)))))
  (unless (char? char)
    (assertion-violation 'digit-value "Expected a char" char))
  ;; TODO: binary search
  (let ((v (char->integer char)))
    (let lp ((i 0))
      (if (fx=? i (vector-length decimal-zeroes))
          #f
          (let ((zero (vector-ref decimal-zeroes i)))
            (cond ((fx<=? zero v (fx+ zero 9))
                   (fx- v zero))
                  (else
                   (lp (fx+ i 1))))))))))
