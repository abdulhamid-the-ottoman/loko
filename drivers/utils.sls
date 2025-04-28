;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2006-2007, 2020 G. Weinholt
#!r6rs

;;; Utilities common to multiple drivers

(library (loko drivers utils)
  (export
    define-i/o
    define-mem
    define-bytevector)
  (import
    (rnrs)
    (loko system unsafe))

(define-syntax define-i/o
  (syntax-rules ()
    ((_ name end fields ...)
     (define-registers-iter name (+) "i/o" end fields ...))))

(define-syntax define-mem
  (syntax-rules ()
    ((_ name end fields ...)
     (define-registers-iter name (+) "mem" end fields ...))))

(define-syntax define-bytevector
  (syntax-rules ()
    ((_ name end fields ...)
     (define-registers-iter name (+) "bv" end fields ...))))

(define-syntax define-registers-iter
  (syntax-rules ()
    ((_ name (offset ...) space end (type ref ...))
     (begin
       (define-registers-aux name "field" space (offset ...) end type ref ...)
       (define-syntax name
         (syntax-rules (quote get-size)
           ((_ 'get-size)
            (offset ... (define-registers-aux name "size" type)))))))

    ((_ name (offset ...) space end (type) more-fields ...)
     (define-registers-iter name (offset ... (define-registers-aux name "size" type))
                            space end more-fields ...))

    ((_ name (offset ...) space end (type ref ...) more-fields ...)
     (begin
       (define-registers-aux name "field" space (offset ...) end type ref ...)
       (define-registers-iter name (offset ... (define-registers-aux name "size" type))
                              space end more-fields ...)))))

(define-syntax define-registers-aux
  (syntax-rules (u8 u16 u32 s61 u64 u128 endianness little big)
    ((_ name "field" space (offset ...) end type ref)
     (define (ref reg)
       (define-registers-aux name "ref" space type end reg
                             (offset ...))))
    ((_ name "field" space (offset ...) end type ref set)
     (begin
       (define-registers-aux name "field" space (offset ...) end type ref)
       (define (set reg v)
         (define-registers-aux name "set" space type end reg (offset ...) v))))
    ((_ name "size" u8) 1)
    ((_ name "size" u16) 2)
    ((_ name "size" u32) 4)
    ((_ name "size" u64) 8)
    ((_ name "size" u128) 16)
    ;; Read/write I/O space
    ((_ name "ref" "i/o" u8 end reg off) (get-i/o-u8 (fx+ reg off)))
    ((_ name "ref" "i/o" u16 end reg off) (get-i/o-u16 (fx+ reg off)))
    ((_ name "ref" "i/o" u32 end reg off) (get-i/o-u32 (fx+ reg off)))
    ((_ name "ref" "i/o" u64 (endianness little) reg off)
     (bitwise-ior
      (get-i/o-u32 (fx+ reg off))
      (bitwise-arithmetic-shift-left (get-i/o-u32 (fx+ reg (fx+ off 4))) 32)))
    ((_ name "ref" "i/o" u128 (endianness little) reg off)
     (bitwise-ior
      (get-i/o-u32 (fx+ reg off))
      (bitwise-arithmetic-shift-left (get-i/o-u32 (fx+ reg (fx+ off 4))) 32)
      (bitwise-arithmetic-shift-left (get-i/o-u32 (fx+ reg (fx+ off 8))) 64)
      (bitwise-arithmetic-shift-left (get-i/o-u32 (fx+ reg (fx+ off 12))) 96)))

    ((_ name "set" "i/o" u8 end reg off v) (put-i/o-u8 (fx+ reg off) v))
    ((_ name "set" "i/o" u16 end reg off v) (put-i/o-u16 (fx+ reg off) v))
    ((_ name "set" "i/o" u32 end reg off v) (put-i/o-u32 (fx+ reg off) v))
    ((_ name "set" "i/o" u64 (endianness little) reg off v)
     (let ((v^ v))
       (put-i/o-u32 (fx+ reg off) (bitwise-bit-field v^ 0 32))
       (put-i/o-u32 (fx+ reg (fx+ off 4)) (bitwise-bit-field v^ 32 64))))
    ((_ name "set" "i/o" u128 (endianness little) reg off v)
     (let ((v^ v))
       (put-i/o-u32 (fx+ reg off) (bitwise-bit-field v^ 0 32))
       (put-i/o-u32 (fx+ reg (fx+ off 4)) (bitwise-bit-field v^ 32 64))
       (put-i/o-u32 (fx+ reg (fx+ off 8)) (bitwise-bit-field v^ 64 96))
       (put-i/o-u32 (fx+ reg (fx+ off 12)) (bitwise-bit-field v^ 96 128))))

    ;; Read/write I/O or shared structures in memory space
    ((_ name "ref" "mem" u8 end reg off) (get-mem-u8 (fx+ reg off)))
    ((_ name "ref" "mem" u16 (endianness little) reg off) (get-mem-u16le (fx+ reg off)))
    ((_ name "ref" "mem" u32 (endianness little) reg off) (get-mem-u32le (fx+ reg off)))
    ((_ name "ref" "mem" s61 (endianness little) reg off) (get-mem-s61le (fx+ reg off)))
    ((_ name "set" "mem" u8 end reg off v) (put-mem-u8 (fx+ reg off) v))
    ((_ name "set" "mem" u16 (endianness little) reg off v) (put-mem-u16le (fx+ reg off) v))
    ((_ name "set" "mem" u32 (endianness little) reg off v) (put-mem-u32le (fx+ reg off) v))
    ((_ name "set" "mem" s61 (endianness little) reg off v) (put-mem-s61le (fx+ reg off) v))
    ;; Read/write a bytevector
    ((_ name "ref" "bv" u8 end bv off) (bytevector-u8-ref bv off))
    ((_ name "ref" "bv" u16 end bv off) (bytevector-u16-ref bv off end))
    ((_ name "ref" "bv" u32 end bv off) (bytevector-u32-ref bv off end))
    ((_ name "set" "bv" u8 end bv off v) (bytevector-u8-set! bv off v))
    ((_ name "set" "bv" u16 end bv off v) (bytevector-u16-set! bv off v end))
    ((_ name "set" "bv" u32 end bv off v) (bytevector-u32-set! bv off v end))))

(define get-mem-u16le get-mem-u16)
(define get-mem-u32le get-mem-u32)
(define get-mem-s61le get-mem-s61)
(define put-mem-u16le put-mem-u16)
(define put-mem-u32le put-mem-u32)
(define put-mem-s61le put-mem-s61))
