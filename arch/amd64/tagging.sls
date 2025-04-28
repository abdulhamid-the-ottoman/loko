;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; amd64 tagging

;; There's a lot going on with the constants. See tools/typetags.py
;; for a proof of the assumed properties.

(library (loko arch amd64 tagging)
  (export
    tag mask shift btag box-header seek-mark)
  (import
    (rnrs (6)))

(define-syntax define-const
  (syntax-rules ()
    ((_ (name) . body)
     (define-syntax name
       (identifier-syntax (lambda () . body))))))

(define-syntax tag
  (lambda (x)
    (syntax-case x ()
      ((_ (q type))
       (eq? (syntax->datum #'q) 'quote)
       (case (syntax->datum #'type)
         ((fixnum)         #b000)
         ((box)            #b001)
         ((pair)           #b010)
         ((procedure)      #b011)
         ((string)         #b100)
         ((vector)         #b110)
         ((bytevector)     #b101)
         ((immsym)        #b0111)
         ;; Objects with 8-bit tags
         ((char)       #b00011111)
         ((boolean)    #b10001111)
         ((flonum)     #b01001111)
         ((null)
          #'(bitwise-ior #b00101111 (* 0 (expt 2 (shift 'singular)))))
         ((eof-object)
          #'(bitwise-ior #b00101111 (* 1 (expt 2 (shift 'singular)))))
         ;; 12-bit tags
         ((kill-mark)  #b100000001111)
         ;; 16-bit tags
         ((void)       #b0000000000001111)
         ((seek-mark)
          ;; XXX: This is handled specially because of the high bit. The
          ;; bit is set so that no canonical address (in particular a
          ;; return address) can be mistaken for a seek-mark.
          #x800000000000800f)
         ((move) #xc00f)
         ((box-header) #x400f)
         (else (syntax-violation 'tag "Unknown type for amd64" x)))))))

(define-syntax mask
  (lambda (x)
    (syntax-case x ()
      ((_ (q type))
       (eq? (syntax->datum #'q) 'quote)
       (case (syntax->datum #'type)
         ((fixnum box pair procedure string vector bytevector)
          #b111)
         ((immsym)
          #b1111)
         ((char boolean flonum)
          #b11111111)
         ((kill-mark void)
          #xfff)
         ((seek-mark)
          (bitwise-ior #xffff (bitwise-arithmetic-shift-left 1 63)))
         ((null eof-object move box-header)
          #xffff)
         ;; These are different from the ones above. A box may start with a
         ;; box header. The length field is the length of the rest of the
         ;; box, in qwords.
         ((box-header:length) #xffffffff)
         ((box-header:refs?) #b1)
         ((box-header:type) #x7f)
         ((box-header:type-number?) #x1)
         ((box-header:value) #xff)
         (else (syntax-violation 'mask "Unknown type for amd64" x)))))))

(define-syntax shift
  (lambda (x)
    (syntax-case x ()
      ((_ (q type))
       (eq? (syntax->datum #'q) 'quote)
       (case (syntax->datum #'type)
         ((fixnum) 3)
         ((immsym) 4)
         ((char boolean) 8)
         ((void) 16)
         ((flonum) 32)
         ((kill-mark) 12)
         ((seek-mark) 16)
         ((move) 16)
         ((singular) 8)
         ((box-header:length) 32)
         ((box-header:refs?) 31)
         ((box-header:type-number?) 30)
         ((box-header:type) 24)
         ((box-header:value) 16)
         (else
          (syntax-violation 'shift "Unknown type for amd64" x)))))))

(define (btag type)
  (case type
    ((symbol)   #x01)
    ((port)     #x02)
    ((rtd)      #x04)
    ((bignum)   #x40)
    ((ratnum)   #x43)
    ((dflonum)  #x45)
    ((pcompnum) #x46)
    ((rcompnum) #x49)
    (else
     (error 'btag "Unknown type for box header type on amd64" type))))

(define (box-header type refs? value length)
  (assert (= length (bitwise-and length (mask 'box-header:length))))
  (assert (boolean? refs?))
  (assert (= value (bitwise-and value (mask 'box-header:value))))
  (bitwise-ior (bitwise-arithmetic-shift-left length (shift 'box-header:length))
               (if refs? (bitwise-arithmetic-shift-left 1 (shift 'box-header:refs?)) 0)
               (bitwise-arithmetic-shift-left value (shift 'box-header:value))
               (bitwise-arithmetic-shift-left (btag type) (shift 'box-header:type))
               (tag 'box-header)))

(define (seek-mark bytes)
  (define (fxalign i alignment)
    (fxand (fx+ i (fx- alignment 1))
           (fx- alignment)))
  ;; The mark is used like this: shift it to the right, then add the
  ;; result to the scan pointer. The scan pointer is pointing at the
  ;; seek mark, so the mark is included in the length.
  (let ((a (fxalign (fx+ bytes 8) 8)))
    (bitwise-ior (tag 'seek-mark)
                 (fxarithmetic-shift-left a (shift 'seek-mark))))))
