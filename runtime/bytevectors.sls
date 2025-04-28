;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; SPDX-FileCopyrightText: 2019-2021 G. Weinholt
;;
;; The utf8->string! procedure is based on C code:
;; SPDX-FileCopyrightText: 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
;; SPDX-License-Identifier: MIT
;;
;; This file is a part of Loko Scheme, an R6RS Scheme system
#!r6rs

;;; Bytevectors

(library (loko runtime bytevectors)
  (export
    native-endianness bytevector? make-bytevector bytevector-length
    bytevector=? bytevector-fill! bytevector-copy! bytevector-copy

    bytevector-u8-ref bytevector-s8-ref
    bytevector-u8-set! bytevector-s8-set!
    bytevector->u8-list u8-list->bytevector
    bytevector-uint-ref bytevector-sint-ref
    bytevector-uint-set! bytevector-sint-set!
    bytevector->uint-list bytevector->sint-list
    uint-list->bytevector sint-list->bytevector
    bytevector-u16-ref bytevector-s16-ref
    bytevector-u16-native-ref bytevector-s16-native-ref
    bytevector-u16-set! bytevector-s16-set!
    bytevector-u16-native-set! bytevector-s16-native-set!
    bytevector-u32-ref bytevector-s32-ref
    bytevector-u32-native-ref bytevector-s32-native-ref
    bytevector-u32-set! bytevector-s32-set!
    bytevector-u32-native-set! bytevector-s32-native-set!
    bytevector-u64-ref bytevector-s64-ref
    bytevector-u64-native-ref bytevector-s64-native-ref
    bytevector-u64-set! bytevector-s64-set!
    bytevector-u64-native-set! bytevector-s64-native-set!
    bytevector-ieee-single-native-ref bytevector-ieee-single-ref
    bytevector-ieee-double-native-ref bytevector-ieee-double-ref
    bytevector-ieee-single-native-set! bytevector-ieee-single-set!
    bytevector-ieee-double-native-set! bytevector-ieee-double-set!
    string->utf8 string->utf16 string->utf32
    utf8->string utf16->string utf32->string

    bytevector-address
    bytevector-truncate!)
  (import
    (except (rnrs)
            native-endianness bytevector? make-bytevector bytevector-length
            bytevector=? bytevector-fill! bytevector-copy! bytevector-copy
            bytevector-u8-ref bytevector-s8-ref
            bytevector-u8-set! bytevector-s8-set!
            bytevector->u8-list u8-list->bytevector
            bytevector-uint-ref bytevector-sint-ref
            bytevector-uint-set! bytevector-sint-set!
            bytevector->uint-list bytevector->sint-list
            uint-list->bytevector sint-list->bytevector
            bytevector-u16-ref bytevector-s16-ref
            bytevector-u16-native-ref bytevector-s16-native-ref
            bytevector-u16-set! bytevector-s16-set!
            bytevector-u16-native-set! bytevector-s16-native-set!
            bytevector-u32-ref bytevector-s32-ref
            bytevector-u32-native-ref bytevector-s32-native-ref
            bytevector-u32-set! bytevector-s32-set!
            bytevector-u32-native-set! bytevector-s32-native-set!
            bytevector-u64-ref bytevector-s64-ref
            bytevector-u64-native-ref bytevector-s64-native-ref
            bytevector-u64-set! bytevector-s64-set!
            bytevector-u64-native-set! bytevector-s64-native-set!
            bytevector-ieee-single-native-ref bytevector-ieee-single-ref
            bytevector-ieee-double-native-ref bytevector-ieee-double-ref
            bytevector-ieee-single-native-set! bytevector-ieee-single-set!
            bytevector-ieee-double-native-set! bytevector-ieee-double-set!
            string->utf8 string->utf16 string->utf32
            utf8->string utf16->string utf32->string)
    (prefix (rnrs) sys:)
    (prefix (only (loko system unsafe) bytevector-address) sys:)
    (loko system $primitives)
    (rnrs mutable-strings)
    (only (loko) string-truncate!))

;; Answers the question: are the
;; bytevector-{s,u}{8,16,32}-{native-,}{ref,set!} procedures
;; open-coded by the code generator on this architecture when the
;; endianness argument is known?
(define-syntax opencoded?
  (lambda (x)
    (syntax-case x (ref set! u8)
      ;; All archs must have bytevector-u8-{ref,set!} open-coded.
      ((_ ref u8) #'#t)
      ((_ set! u8) #'#t)
      ;; TODO: handle non-opencoding of the native procedures.
      ((_ ref size)
       #'(memq 'size '(s8 u16 s16 u32 s32)))
      ((_ set! size)
       #'#f))))

(define (native-endianness)
  (endianness little))

(define (bytevector? x) (sys:bytevector? x))

(define make-bytevector
  (case-lambda
    ((len)
     (cond (#f
            ;; This version returns uninitialized data. Faster but
            ;; generally not a good idea. There are some places in the
            ;; code that still make the assumption that memory is
            ;; initialized to zero.
            (assert (fx>=? len 0))
            (if (eqv? len 0)
                #vu8()
                ($make-bytevector len)))
           (else
            (make-bytevector len 0))))
    ((len fill)
     (assert (fx>=? len 0))
     (assert (fx<=? -128 fill 255))
     (if (eqv? len 0)
         #vu8()
         (let ((bv ($make-bytevector len)))
           (bytevector-fill! bv fill)
           bv)))))

(define (bytevector-length x) (sys:bytevector-length x))

(define (bytevector=? bv1 bv2)
  (let ((len (bytevector-length bv1)))
    (and (fx=? len (bytevector-length bv2))
         (or
           (eq? bv1 bv2)
           (let ((len32 (fxand len -4)))
             (let lp ((i 0))
               (cond ((fx=? i len32)
                      (do ((i i (fx+ i 1))
                           (diff 0 (fxior diff
                                          (fxxor
                                           (bytevector-u8-ref bv1 i)
                                           (bytevector-u8-ref bv2 i)))))
                          ((fx=? i len) (fxzero? diff))))
                     ((not (fx=? (bytevector-u32-native-ref bv1 i)
                                 (bytevector-u32-native-ref bv2 i)))
                      #f)
                     (else
                      (lp (fx+ i 4))))))))))

(define (bytevector-fill! bv fill)
  (assert (fx<=? -128 fill 255))
  (let* ((fill8 (fxbit-field fill 0 8))
         (fill32 (fx* #x01010101 fill8))
         (end8 (bytevector-length bv))
         (end32 (fxand end8 -4)))
    (do ((i 0 (fx+ i 4)))
        ((fx=? i end32)
         (do ((i end32 (fx+ i 1)))
             ((fx=? i end8))
           (bytevector-u8-set! bv i fill8)))
      (bytevector-u32-native-set! bv i fill32))))

;; This should definitely be open-coded in a lot of cases. It should
;; make use of how objects are aligned in memory, etc.
(define (bytevector-copy! s ss t ts k)
  (assert (fx>=? k 0))
  (assert (and (bytevector? s) (bytevector? t)))
  (let copy! ((ss ss) (ts ts) (k k))
    (cond ((and (eq? t s) (fx<? ss ts))
           (do ((i (fx- k 1) (fx- i 1)))
               ((fx=? i -1))
             (bytevector-u8-set! t (fx+ ts i)
                                 (bytevector-u8-ref s (fx+ ss i)))))
          ((eqv? 0 (fxand #b11 (fxior ss ts)))
           (do ((k^ (fxand k -4))
                (i 0 (fx+ i 4)))
               ((fx=? i k^)
                (do ((i i (fx+ i 1)))
                    ((fx=? i k))
                  (bytevector-u8-set! t (fx+ ts i)
                                      (bytevector-u8-ref s (fx+ ss i)))))
             (bytevector-u32-native-set! t (fx+ ts i)
                                         (bytevector-u32-native-ref s (fx+ ss i)))))
          ((and (fx=? (fxand #b11 ss) (fxand #b11 ts)) (fx>? k 4))
           ;; Align the indices to enable u32 operations
           (let ((a (fx- 4 (fxand #b11 ts))))
             (do ((i 0 (fx+ i 1)))
                 ((fx=? i a))
               (bytevector-u8-set! t (fx+ ts i)
                                   (bytevector-u8-ref s (fx+ ss i))))
             (copy! (fx+ ss a) (fx+ ts a) (fx- k a))))
          ((eqv? 0 (fxand #b1 (fxior ss ts)))
           (do ((k^ (fxand k -2))
                (i 0 (fx+ i 2)))
               ((fx=? i k^)
                (do ((i i (fx+ i 1)))
                    ((fx=? i k))
                  (bytevector-u8-set! t (fx+ ts i)
                                      (bytevector-u8-ref s (fx+ ss i)))))
             (bytevector-u16-native-set! t (fx+ ts i)
                                         (bytevector-u16-native-ref s (fx+ ss i)))))
          (else
           (do ((i 0 (fx+ i 1)))
               ((fx=? i k))
             (bytevector-u8-set! t (fx+ ts i)
                                 (bytevector-u8-ref s (fx+ ss i))))))))

(define (bytevector-copy bv)
  (let ((ret (make-bytevector (bytevector-length bv))))
    (bytevector-copy! bv 0 ret 0 (bytevector-length bv))
    ret))

;;; Bytes and octets

(define (bytevector-u8-ref x i) (sys:bytevector-u8-ref x i))

(define (bytevector-s8-ref x i) (sys:bytevector-s8-ref x i))

(define (bytevector-u8-set! x i v) (sys:bytevector-u8-set! x i v))

(define (bytevector-s8-set! x i v) (sys:bytevector-s8-set! x i v))

(define (bytevector->u8-list bv)
  (when (not (bytevector? bv))
    (assertion-violation 'bytevector->u8-list
                         "Expected a bytevector" bv))
  (do ((i (fx- (bytevector-length bv) 1) (fx- i 1))
       (ret '() (cons (bytevector-u8-ref bv i) ret)))
      ((fx=? i -1) ret)))

(define (u8-list->bytevector list)
  ;; FIXME: length checks for improper lists, but will report the
  ;; wrong &who condition
  (let ((len (length list)))
    (do ((ret (make-bytevector len))
         (i 0 (fx+ i 1))
         (u8s list (cdr u8s)))
        ((fx=? i len) ret)
      (let ((o (car u8s)))
        (when (not (and (fixnum? o) (fx<=? 0 o 255)))
          (assertion-violation 'u8-list->bytevector
                               "Expected a list of octets" list))
        (bytevector-u8-set! ret i o)))))

;;; Integers of arbitrary size

(define (bytevector-uint-ref bv k endian size)
  (assert (fxpositive? size))
  (assert (fx<=? 0 k (fx- (bytevector-length bv) size)))
  (case endian
    ((little)
     (do ((end (fx- k 1))
          (i (fx+ k (fx- size 1)) (fx- i 1))
          (ret 0 (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                              (bytevector-u8-ref bv i))))
         ((fx=? i end)
          ret)))
    ((big)
     (do ((end (fx+ k size))
          (i k (fx+ i 1))
          (ret 0 (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                              (bytevector-u8-ref bv i))))
         ((fx=? i end)
          ret)))
    (else
     (assertion-violation 'bytevector-uint-ref "Unsupported endianness"
                          bv k endian size))))

(define (bytevector-sint-ref bv k endian size)
  (assert (fxpositive? size))
  (assert (fx<=? 0 k (fx- (bytevector-length bv) size)))
  (case endian
    ((little)
     (do ((end (fx- k 1))
          (i (fx+ k (fx- size 2)) (fx- i 1))
          (ret (bytevector-s8-ref bv (fx+ k (fx- size 1)))
               (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                            (bytevector-u8-ref bv i))))
         ((fx=? i end) ret)))
    ((big)
     (do ((end (fx+ k size))
          (i (fx+ k 1) (fx+ i 1))
          (ret (bytevector-s8-ref bv k)
               (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                            (bytevector-u8-ref bv i))))
         ((fx=? i end) ret)))
    (else
     (assertion-violation 'bytevector-uint-ref "Unsupported endianness"
                          bv k endian size))))

(define (bytevector-uint-set! bv k n endian size)
  (assert (fxpositive? size))
  (assert (and (fx>=? k 0) (fx<=? k (fx- (bytevector-length bv) size))))
  (unless (fx<=? (bitwise-length n) (fx* size 8))
    (assertion-violation 'bytevector-uint-set! "Integer out of range"
                         bv k n endian size))
  (case endian
    ((little)
     (if (fixnum? n)
         (do ((i k (fx+ i 1))
              (end (fx+ k size))
              (n n (fxarithmetic-shift-right n 8)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (fxand n #xff)))
         (do ((i k (fx+ i 1))
              (s 0 (fx+ s 8))
              (end (fx+ k size)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (bitwise-bit-field n s (fx+ s 8))))))
    ((big)
     (if (fixnum? n)
         (do ((end (fx- k 1))
              (i (fx+ k (fx- size 1)) (fx- i 1))
              (n n (fxarithmetic-shift-right n 8)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (fxand n #xff)))
         (do ((i k (fx+ i 1))
              (s (fx* (fx- size 1) 8) (fx- s 8))
              (end (fx+ k size)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (bitwise-bit-field n s (fx+ s 8))))))
    (else
     (assertion-violation 'bytevector-uint-set! "Unsupported endianness"
                          bv k n endian size))))

(define (bytevector-sint-set! bv k n endian size)
  (assert (fxpositive? size))
  (assert (and (fx>=? k 0) (fx<=? k (fx- (bytevector-length bv) size))))
  (unless (fx<? (bitwise-length n) (fx* size 8))
    (assertion-violation 'bytevector-uint-set! "Integer out of range"
                         bv k n endian size))
  (case endian
    ((little)
     (if (fixnum? n)
         (do ((end (fx+ k size))
              (i k (fx+ i 1))
              (n n (fxarithmetic-shift-right n 8)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (fxand n #xff)))
         (do ((i k (fx+ i 1))
              (s 0 (fx+ s 8))
              (end (fx+ k size)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (bitwise-bit-field n s (fx+ s 8))))))
    ((big)
     (if (fixnum? n)
         (do ((end (fx- k 1))
              (i (fx+ k (fx- size 1)) (fx- i 1))
              (n n (fxarithmetic-shift-right n 8)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (fxand n #xff)))
         (do ((i k (fx+ i 1))
              (s (fx* (fx- size 1) 8) (fx- s 8))
              (end (fx+ k size)))
             ((fx=? i end))
           (bytevector-u8-set! bv i (bitwise-bit-field n s (fx+ s 8))))))
    (else
     (assertion-violation 'bytevector-sint-set! "Unsupported endianness"
                          bv k n endian size))))

(define (bytevector->uint-list bv endian size)
  (unless (eqv? 0 (fxmod (bytevector-length bv) size))
    (assertion-violation 'bytevector->uint-list
                         "Bytevector length must be a multiple of the size"
                         bv endian size))
  (case size
    ((1)
     (do ((i (fx- (bytevector-length bv) 1) (fx- i 1))
          (ret '() (cons (bytevector-u8-ref bv i) ret)))
         ((fx<? i 0) ret)))
    ((2)
     (do ((i (fx- (bytevector-length bv) 2) (fx- i 2))
          (ret '() (cons (bytevector-u16-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((4)
     (do ((i (fx- (bytevector-length bv) 4) (fx- i 4))
          (ret '() (cons (bytevector-u32-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((8)
     (do ((i (fx- (bytevector-length bv) 8) (fx- i 8))
          (ret '() (cons (bytevector-u64-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    (else
     (do ((i (fx- (bytevector-length bv) size) (fx- i size))
          (ret '() (cons (bytevector-uint-ref bv i endian size) ret)))
         ((fx<? i 0) ret)))))

(define (bytevector->sint-list bv endian size)
  (unless (eqv? 0 (fxmod (bytevector-length bv) size))
    (assertion-violation 'bytevector->sint-list
                         "Bytevector length must be a multiple of the size"
                         bv endian size))
  (case size
    ((1)
     (do ((i (fx- (bytevector-length bv) 1) (fx- i 1))
          (ret '() (cons (bytevector-s8-ref bv i) ret)))
         ((fx<? i 0) ret)))
    ((2)
     (do ((i (fx- (bytevector-length bv) 2) (fx- i 2))
          (ret '() (cons (bytevector-s16-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((4)
     (do ((i (fx- (bytevector-length bv) 4) (fx- i 4))
          (ret '() (cons (bytevector-s32-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((8)
     (do ((i (fx- (bytevector-length bv) 8) (fx- i 8))
          (ret '() (cons (bytevector-s64-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    (else
     (do ((i (fx- (bytevector-length bv) size) (fx- i size))
          (ret '() (cons (bytevector-sint-ref bv i endian size) ret)))
         ((fx<? i 0) ret)))))

(define (uint-list->bytevector list endian size)
  (let ((ret (make-bytevector (fx* size (length list)))))
    (do ((k 0 (fx+ k size))
         (list list (cdr list)))
        ((null? list) ret)
      (bytevector-uint-set! ret k (car list) endian size))))

(define (sint-list->bytevector list endian size)
  (let ((ret (make-bytevector (fx* size (length list)))))
    (do ((k 0 (fx+ k size))
         (list list (cdr list)))
        ((null? list) ret)
      (bytevector-sint-set! ret k (car list) endian size))))

;;; 16-bit integers

(define (bytevector-u16-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-u16-ref "Unsupported endianness"
                         bv idx endian))
  (if (opencoded? ref u16)
      (case endian
        ((little) (sys:bytevector-u16-ref bv idx (endianness little)))
        ((big) (sys:bytevector-u16-ref bv idx (endianness big)))
        (else (wrong)))
      (let ((b2 (bytevector-u8-ref bv (fx+ idx 1)))
            (b1 (bytevector-u8-ref bv idx)))
        (case endian
          ((little)
           (fxior (fxarithmetic-shift-left b2 8) b1))
          ((big)
           (fxior (fxarithmetic-shift-left b1 8) b2))
          (else (wrong))))))

(define (bytevector-s16-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-s16-ref "Unsupported endianness"
                         bv idx endian))
  (if (opencoded? ref s16)
      (case endian
        ((little) (sys:bytevector-s16-ref bv idx (endianness little)))
        ((big) (sys:bytevector-s16-ref bv idx (endianness big)))
        (else (wrong)))
      (case endian
        ((little)
         (let ((b2 (bytevector-s8-ref bv (fx+ idx 1)))
               (b1 (bytevector-u8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b2 8) b1)))
        ((big)
         (let ((b2 (bytevector-u8-ref bv (fx+ idx 1)))
               (b1 (bytevector-s8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b1 8) b2)))
        (else (wrong)))))

(define (bytevector-u16-native-ref bv i) (sys:bytevector-u16-native-ref bv i))

(define (bytevector-s16-native-ref bv i) (sys:bytevector-s16-native-ref bv i))

(define (bytevector-u16-set! bv idx v endianness)
  (assert (fx<=? 0 v #xffff))
  (case endianness
    ((little)
     (bytevector-u8-set! bv idx (fxand v #xff))
     (bytevector-u8-set! bv (fx+ idx 1) (fxarithmetic-shift-right v 8)))
    ((big)
     (bytevector-u8-set! bv idx (fxarithmetic-shift-right v 8))
     (bytevector-u8-set! bv (fx+ idx 1) (fxand v #xff)))
    (else
     (assertion-violation 'bytevector-u16-set! "Unsupported endianness"
                          bv idx v endianness))))

(define (bytevector-s16-set! bv idx v endian)
  (assert (fx<=? (- (expt 2 15)) v (- (expt 2 15) 1)))
  (bytevector-u16-set! bv idx (fxand v #xffff) endian))

(define (bytevector-u16-native-set! bv i v)
  (sys:bytevector-u16-native-set! bv i v))

(define (bytevector-s16-native-set! bv i v)
  (sys:bytevector-s16-native-set! bv i v))

;;; 32-bit integers

(define (bytevector-u32-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-u32-ref "Unsupported endianness"
                         bv idx endian))
  (if (opencoded? ref u32)
      (case endian
        ((little) (sys:bytevector-u32-ref bv idx (endianness little)))
        ((big) (sys:bytevector-u32-ref bv idx (endianness big)))
        (else (wrong)))
      (let ((b4 (bytevector-u8-ref bv (fx+ idx 3)))
            (b3 (bytevector-u8-ref bv (fx+ idx 2)))
            (b2 (bytevector-u8-ref bv (fx+ idx 1)))
            (b1 (bytevector-u8-ref bv idx)))
        (case endian
          ((little)
           (fxior (fxarithmetic-shift-left b4 24)
                  (fxarithmetic-shift-left b3 16)
                  (fxarithmetic-shift-left b2 8)
                  b1))
          ((big)
           (fxior (fxarithmetic-shift-left b1 24)
                  (fxarithmetic-shift-left b2 16)
                  (fxarithmetic-shift-left b3 8)
                  b4))
          (else (wrong))))))

(define (bytevector-s32-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-s32-ref "Unsupported endianness" bv idx endian))
  (if (opencoded? ref u32)
      (case endian
        ((little) (sys:bytevector-s32-ref bv idx (endianness little)))
        ((big) (sys:bytevector-s32-ref bv idx (endianness big)))
        (else (wrong)))
      (case endian
        ((little)
         (let ((b4 (bytevector-s8-ref bv (fx+ idx 3)))
               (b3 (bytevector-u8-ref bv (fx+ idx 2)))
               (b2 (bytevector-u8-ref bv (fx+ idx 1)))
               (b1 (bytevector-u8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b4 24)
                  (fxarithmetic-shift-left b3 16)
                  (fxarithmetic-shift-left b2 8)
                  b1)))
        ((big)
         (let ((b4 (bytevector-u8-ref bv (fx+ idx 3)))
               (b3 (bytevector-u8-ref bv (fx+ idx 2)))
               (b2 (bytevector-u8-ref bv (fx+ idx 1)))
               (b1 (bytevector-s8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b1 24)
                  (fxarithmetic-shift-left b2 16)
                  (fxarithmetic-shift-left b3 8)
                  b4)))
        (else (wrong)))))

(define (bytevector-u32-native-ref bv i) (sys:bytevector-u32-native-ref bv i))

(define (bytevector-s32-native-ref bv i) (sys:bytevector-s32-native-ref bv i))

(define (bytevector-u32-set! bv idx v endian)
  (define who 'bytevector-u32-set!)
  (assert (and (fx>=? v 0) (fx<=? v #xffffffff)))
  (case endian
    ((little)
     (cond ((eqv? 0 (fxand idx #b11))
            (assert (eq? (native-endianness) (endianness little)))
            (sys:bytevector-u32-native-set! bv idx v))
           (else
            (bytevector-u8-set! bv idx (fxand v #xff))
            (bytevector-u8-set! bv (fx+ idx 1) (fxbit-field v 8 16))
            (bytevector-u8-set! bv (fx+ idx 2) (fxbit-field v 16 24))
            (bytevector-u8-set! bv (fx+ idx 3) (fxbit-field v 24 32)))))
    ((big)
     (bytevector-u8-set! bv idx (fxbit-field v 24 32))
     (bytevector-u8-set! bv (fx+ idx 1) (fxbit-field v 16 24))
     (bytevector-u8-set! bv (fx+ idx 2) (fxbit-field v 8 16))
     (bytevector-u8-set! bv (fx+ idx 3) (fxand v #xff)))
    (else
     (assertion-violation who "Unsupported endianness" bv idx v endian))))

(define (bytevector-s32-set! bv idx v endian)
  (define who 'bytevector-s32-set!)
  (assert (and (fx<=? (- (expt 2 31)) v) (fx<=? v (- (expt 2 31) 1))))
  (case endian
    ((little)
     (cond ((eqv? 0 (fxand idx #b11))
            (assert (eq? (native-endianness) (endianness little)))
            (sys:bytevector-s32-native-set! bv idx v))
           (else
            (bytevector-u8-set! bv idx (fxand v #xff))
            (bytevector-u8-set! bv (fx+ idx 1) (fxbit-field v 8 16))
            (bytevector-u8-set! bv (fx+ idx 2) (fxbit-field v 16 24))
            (bytevector-u8-set! bv (fx+ idx 3) (fxbit-field v 24 32)))))
    ((big)
     (bytevector-u8-set! bv idx (fxbit-field v 24 32))
     (bytevector-u8-set! bv (fx+ idx 1) (fxbit-field v 16 24))
     (bytevector-u8-set! bv (fx+ idx 2) (fxbit-field v 8 16))
     (bytevector-u8-set! bv (fx+ idx 3) (fxand v #xff)))
    (else
     (assertion-violation who "Unsupported endianness" bv idx v endian))))

(define (bytevector-u32-native-set! bv i v)
  (sys:bytevector-u32-native-set! bv i v))

(define (bytevector-s32-native-set! bv i v)
  (sys:bytevector-s32-native-set! bv i v))

;;; 64-bit integers

(define (bytevector-u64-ref bv idx endian)
  (case endian
    ((little)
     (let ((dw1 (bytevector-u32-ref bv (fx+ idx 4) (endianness little)))
           (dw0 (bytevector-u32-ref bv idx (endianness little))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw1 32)
                    dw0)))
    ((big)
     (let ((dw1 (bytevector-u32-ref bv (fx+ idx 4) (endianness big)))
           (dw0 (bytevector-u32-ref bv idx (endianness big))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw0 32)
                    dw1)))
    (else
     (assertion-violation 'bytevector-u64-ref "Unsupported endianness" bv idx endian))))

(define (bytevector-s64-ref bv idx endian)
  (case endian
    ((little)
     (let ((dw1 (bytevector-s32-ref bv (fx+ idx 4) (endianness little)))
           (dw0 (bytevector-u32-ref bv idx (endianness little))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw1 32)
                    dw0)))
    ((big)
     (let ((dw1 (bytevector-u32-ref bv (fx+ idx 4) (endianness big)))
           (dw0 (bytevector-s32-ref bv idx (endianness big))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw0 32)
                    dw1)))
    (else
     (assertion-violation 'bytevector-u64-ref "Unsupported endianness" bv idx endian))))

(define (bytevector-u64-native-ref bv idx)
  (assert (fxzero? (fxand idx #b111)))
  (bytevector-u64-ref bv idx (native-endianness)))

(define (bytevector-s64-native-ref bv idx)
  (assert (fxzero? (fxand idx #b111)))
  (bytevector-s64-ref bv idx (native-endianness)))

(define (bytevector-u64-set! bv idx v endian)
  (define who 'bytevector-u64-set!)
  (cond ((and (fixnum? v) (fx>=? v 0))
         (assert (< (greatest-fixnum) (- (expt 2 64) 1)))
         (let ((l (fxand v #xFFFFFFFF))
               (h (fxand (fxarithmetic-shift-right v 32) #xFFFFFFFF)))
           (case endian
             ((little)
              (bytevector-u32-set! bv idx l (endianness little))
              (bytevector-u32-set! bv (fx+ idx 4) h (endianness little)))
             ((big)
              (bytevector-u32-set! bv idx h (endianness big))
              (bytevector-u32-set! bv (fx+ idx 4) l (endianness big)))
             (else
              (assertion-violation who "Unsupported endianness" bv idx v endian)))))
        (else
         (unless (fx<=? (bitwise-length v) 64)
           (assertion-violation who "Expected an unsigned 64-bit integer" bv idx v endian))
         (let ((l (bitwise-bit-field v 0 32))
               (h (bitwise-bit-field v 32 64)))
           (case endian
             ((little)
              (bytevector-u32-set! bv idx l (endianness little))
              (bytevector-u32-set! bv (fx+ idx 4) h (endianness little)))
             ((big)
              (bytevector-u32-set! bv idx h (endianness big))
              (bytevector-u32-set! bv (fx+ idx 4) l (endianness big)))
             (else
              (assertion-violation who "Unsupported endianness" bv idx v endian)))))))

(define (bytevector-s64-set! bv idx v endian)
  (define who 'bytevector-s64-set!)
  (cond ((fixnum? v)
         (assert (< (greatest-fixnum) (- (expt 2 63) 1)))
         (let ((l (fxand v #xFFFFFFFF))
               (h (fxand (fxarithmetic-shift-right v 32) #xFFFFFFFF)))
           (case endian
             ((little)
              (bytevector-u32-set! bv idx l (endianness little))
              (bytevector-u32-set! bv (fx+ idx 4) h (endianness little)))
             ((big)
              (bytevector-u32-set! bv idx h (endianness big))
              (bytevector-u32-set! bv (fx+ idx 4) l (endianness big)))
             (else
              (assertion-violation who "Unsupported endianness" bv idx v endian)))))
        (else
         (unless (fx<=? (bitwise-length v) 63)
           (assertion-violation who "Expected an signed 64-bit integer" bv idx v endian))
         (let ((l (bitwise-bit-field v 0 32))
               (h (bitwise-bit-field v 32 64)))
           (case endian
             ((little)
              (bytevector-u32-set! bv idx l (endianness little))
              (bytevector-u32-set! bv (fx+ idx 4) h (endianness little)))
             ((big)
              (bytevector-u32-set! bv idx h (endianness big))
              (bytevector-u32-set! bv (fx+ idx 4) l (endianness big)))
             (else
              (assertion-violation who "Unsupported endianness" bv idx v endian)))))))

(define (bytevector-u64-native-set! bv idx v)
  (define who 'bytevector-u64-native-set!)
  (unless (fxzero? (fxand idx #b111))
    (assertion-violation who "Invalid index" bv idx v))
  (assert (eq? (native-endianness) (endianness little)))
  (cond ((and (fixnum? v) (fx>=? v 0))
         (sys:bytevector-u32-native-set! bv idx (fxand v #xffffffff))
         (sys:bytevector-u32-native-set! bv (fx+ idx 4) (fxarithmetic-shift-right v 32)))
        (else
         (unless (and (not (negative? v)) (fx<=? (bitwise-length v) 64))
           (assertion-violation who "Expected an unsigned 64-bit integer" bv idx v))
         (sys:bytevector-u32-native-set! bv idx (bitwise-bit-field v 0 32))
         (sys:bytevector-u32-native-set! bv (fx+ idx 4) (bitwise-bit-field v 32 64)))))

(define (bytevector-s64-native-set! bv idx v)
  (define who 'bytevector-s64-native-set!)
  (unless (fxzero? (fxand idx #b111))
    (assertion-violation who "Invalid index" bv idx v))
  (cond ((fixnum? v)
         (let ((l (fxand v #xFFFFFFFF))
               (h (fxand (fxarithmetic-shift-right v 32) #xFFFFFFFF)))
           (case (native-endianness)
             ((little)
              (sys:bytevector-u32-native-set! bv idx l)
              (sys:bytevector-u32-native-set! bv (fx+ idx 4) h))
             ((big)
              (sys:bytevector-u32-native-set! bv idx h)
              (sys:bytevector-u32-native-set! bv (fx+ idx 4) l))
             (else
              (assertion-violation who "Unsupported endianness" bv idx v (native-endianness))))))
        (else
         (unless (fx<=? (bitwise-length v) 63)
           (assertion-violation who "Expected an signed 64-bit integer" bv idx v))
         (let ((l (bitwise-bit-field v 0 32))
               (h (bitwise-bit-field v 32 64)))
           (case (native-endianness)
             ((little)
              (sys:bytevector-u32-native-set! bv idx l)
              (sys:bytevector-u32-native-set! bv (fx+ idx 4) h))
             ((big)
              (sys:bytevector-u32-native-set! bv idx h)
              (sys:bytevector-u32-native-set! bv (fx+ idx 4) l))
             (else
              (assertion-violation who "Unsupported endianness" bv idx v (native-endianness))))))))

;;; IEEE-754 representations

(define (bytevector-ieee-single-native-ref bv idx)
  (sys:bytevector-ieee-single-native-ref bv idx))

(define (bytevector-ieee-single-ref bv idx endian)
  (let ((tmp (make-bytevector 4)))
    (bytevector-u32-native-set! tmp 0 (bytevector-u32-ref bv idx endian))
    (bytevector-ieee-single-native-ref tmp 0)))

(define (bytevector-ieee-double-native-ref bv idx)
  (sys:bytevector-ieee-double-native-ref bv idx))

(define (bytevector-ieee-double-ref bv idx endian)
  (let ((tmp (make-bytevector 8)))
    (bytevector-u64-native-set! tmp 0 (bytevector-u64-ref bv idx endian))
    (bytevector-ieee-double-native-ref tmp 0)))

(define (bytevector-ieee-single-native-set! bv idx v)
  (sys:bytevector-ieee-single-native-set! bv idx v))

(define (bytevector-ieee-single-set! bv idx v endian)
  (let ((tmp (make-bytevector 4)))
    (bytevector-ieee-single-native-set! tmp 0 v)
    (bytevector-u32-set! bv idx (bytevector-u32-native-ref tmp 0) endian)))

(define (bytevector-ieee-double-native-set! bv idx v)
  (sys:bytevector-ieee-double-native-set! bv idx v))

(define (bytevector-ieee-double-set! bv idx v endian)
  (let ((tmp (make-bytevector 8)))
    (bytevector-ieee-double-native-set! tmp 0 v)
    (bytevector-u64-set! bv idx (bytevector-u64-native-ref tmp 0) endian)))

;;; Strings

(define (string->utf8 x)
  (call-with-bytevector-output-port
    (lambda (p) (put-string p x))
    (native-transcoder)))

(define string->utf16
  (case-lambda
    ((x)
     (string->utf16 x (endianness big)))
    ((x endian)
     (unless (memq endian '(big little))
       (assertion-violation 'string->utf16 "Unsupported endianness" x endian))
     (let* ((len (do ((i (fx- (string-length x) 1) (fx- i 1))
                      (slen 0 (if (fx<? (char->integer (string-ref x i)) #x10000)
                                  (fx+ slen 2) (fx+ slen 4))))
                     ((eqv? i -1) slen)))
            (ret (make-bytevector len)))
       (let lp ((si 0) (bi 0))
         (unless (fx=? bi len)
           (let ((cp (char->integer (string-ref x si))))
             (cond
               ((fx<? cp #x10000)
                (bytevector-u16-set! ret bi cp endian)
                (lp (fx+ si 1) (fx+ bi 2)))
               (else
                (let ((cp^ (fx- cp #x10000)))
                  (let ((high (fx+ #xD800 (fxarithmetic-shift-right cp^ 10)))
                        (low (fx+ #xDC00 (fxbit-field cp^ 0 10))))
                    (bytevector-u16-set! ret bi high endian)
                    (bytevector-u16-set! ret (fx+ bi 2) low endian)))
                (lp (fx+ si 1) (fx+ bi 4)))))))
       ret))))

(define string->utf32
  (case-lambda
    ((x)
     (string->utf32 x (endianness big)))
    ((x endian)
     (let* ((len (string-length x))
            (ret (make-bytevector (fx* 4 len))))
       (case endian
         ((big)
          (do ((i 0 (fx+ i 1)))
              ((fx=? i len))
            (bytevector-u32-set! ret (fx* i 4) (char->integer (string-ref x i))
                                 (endianness big))))
         ((little)
          (do ((i 0 (fx+ i 1)))
              ((fx=? i len))
            (bytevector-u32-set! ret (fx* i 4) (char->integer (string-ref x i))
                                 (endianness little))))
         (else
          (assertion-violation 'string->utf32 "Unsupported endianness" x endian)))
       ret))))

(define (utf8->string! str bv)
  ;; See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
  ;; Based on C code which carried this copyright:
  ;; Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
  ;;
  ;; Permission is hereby granted, free of charge, to any person
  ;; obtaining a copy of this software and associated documentation files
  ;; (the "Software"), to deal in the Software without restriction,
  ;; including without limitation the rights to use, copy, modify, merge,
  ;; publish, distribute, sublicense, and/or sell copies of the Software,
  ;; and to permit persons to whom the Software is furnished to do so,
  ;; subject to the following conditions:
  ;;
  ;; The above copyright notice and this permission notice shall be
  ;; included in all copies or substantial portions of the Software.
  ;;
  ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  ;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  ;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  ;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  ;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  ;; SOFTWARE.
  (define UTF8_ACCEPT 0)
  (define UTF8_REJECT 12)
  ;; The first part of the table maps bytes to character classes to
  ;; reduce the size of the transition table and create bitmasks.
  (define utf8d
    #vu8( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
          7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7   7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
          8 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2   2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
         10 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3  11 6 6 6 5 8 8 8 8 8 8 8 8 8 8 8))
  ;; The second part is a transition table that maps a combination
  ;; of a state of the automaton and a character class to a state.
  (define utf8t
    #vu8( 0 12 24 36 60 96 84 12 12 12 48 72  12 12 12 12 12 12 12 12 12 12 12 12
         12  0 12 12 12 12 12  0 12  0 12 12  12 24 12 12 12 12 12 24 12 24 12 12
         12 12 12 12 12 12 12 24 12 12 12 12  12 24 12 12 12 12 12 12 12 24 12 12
         12 12 12 12 12 12 12 36 12 36 12 12  12 36 12 12 12 12 12 36 12 36 12 12
         12 36 12 12 12 12 12 12 12 12 12 12))
  (let lp ((i 0) (j 0))
    (if (fx=? i (bytevector-length bv))
        j
        (let lp* ((i i) (j j) (state UTF8_ACCEPT) (codep 0))
          (cond ((fx=? i (bytevector-length bv))
                 ;; Truncated input
                 (string-set! str j #\xfffd)
                 (fx+ j 1))
                (else
                 (let* ((byte (sys:bytevector-u8-ref bv i))
                        (type (sys:bytevector-u8-ref utf8d byte))
                        (codep (if (not (eqv? state UTF8_ACCEPT))
                                   (fxior (fxand byte #x3f)
                                          (fxarithmetic-shift-left codep 6))
                                   (fxand (fxarithmetic-shift-right #xff type)
                                          byte)))
                        (state (sys:bytevector-u8-ref utf8t (fx+ state type))))
                   (cond ((eqv? state UTF8_ACCEPT)
                          (string-set! str j (integer->char codep))
                          (lp (fx+ i 1) (fx+ j 1)))
                         ((eqv? state UTF8_REJECT)
                          (string-set! str j #\xfffd)
                          (lp (fx+ i 1) (fx+ j 1)))
                         (else
                          (lp* (fx+ i 1) j state codep))))))))))

(define (utf8->string bv)
  (let ((str (make-string (bytevector-length bv))))
    (string-truncate! str (utf8->string! str bv))))

(define utf16->string
  (case-lambda
    ((bv endian)
     (utf16->string bv endian #f))
    ((bv endian endianness-mandatory?)
     (call-with-string-output-port
       (lambda (p)
         (define (put c)
           (put-char p c))
         (let* ((BOM (if (fx<? (bytevector-length bv) 2)
                         #f
                         (let ((bom (bytevector-u16-ref bv 0 (endianness big))))
                           (cond ((fx=? bom #xFEFF) (endianness big))
                                 ((fx=? bom #xFFFE) (endianness little))
                                 (else #f)))))
                (endian (if endianness-mandatory? endian (or BOM endian)))
                (skip (if (and BOM (not endianness-mandatory?)) 2 0)))
           (let lp ((i skip)
                    (rem (fx- (bytevector-length bv) skip)))
             (cond
               ((eqv? rem 0))
               ((eqv? rem 1) (put #\xFFFD))
               (else
                (let ((w0 (bytevector-u16-ref bv i endian))
                      (i^ (fx+ i 2))
                      (rem^ (fx- rem 2)))
                  (cond
                    ((fx<=? #xD800 w0 #xDFFF)
                     ;; Surrogate pair
                     (cond
                       ((fx<? rem^ 2)
                        (put #\xFFFD)
                        (lp i^ rem^))
                       (else
                        ;; Interesting: the ordering of the
                        ;; surrogate pairs forms a second level of
                        ;; endianness, which thankfully is fixed as
                        ;; big endian.
                        (let ((w1 (bytevector-u16-ref bv i^ endian))
                              (i^^ (fx+ i^ 2))
                              (rem^^ (fx- rem^ 2)))
                          (cond ((fx<=? #xD800 w1 #xDFFF)
                                 (let ((w (fx+ (fxior (fxarithmetic-shift-left (fx- w0 #xD800) 10)
                                                      (fxbit-field (fx- w1 #xDC00) 0 10))
                                               #x10000)))
                                   (cond ((fx>? w #x10FFFF)
                                          (put #\xFFFD)
                                          (lp i^ rem^))
                                         (else
                                          (put (integer->char w))
                                          (lp i^^ rem^^)))))
                                (else
                                 (put #\xFFFD)
                                 (lp i^ rem^)))))))
                    (else
                     (put (integer->char w0))
                     (lp i^ rem^)))))))))))))

(define utf32->string
  (case-lambda
    ((bv endian)
     (utf32->string bv endian #f))
    ((bv endian endianness-mandatory?)
     (call-with-string-output-port
       (lambda (p)
         (define (put c)
           (put-char p c))
         (let* ((BOM (if (fx<? (bytevector-length bv) 4)
                         #f
                         (let ((bom (bytevector-u32-ref bv 0 (endianness big))))
                           (cond ((eqv? bom #x0000FEFF) (endianness big))
                                 ((eqv? bom #xFFFE0000) (endianness little))
                                 (else #f)))))
                (endian (if endianness-mandatory? endian (or BOM endian)))
                (skip (if (and BOM (not endianness-mandatory?)) 4 0)))
           (let lp ((i skip)
                    (rem (fx- (bytevector-length bv) skip)))
             (cond
               ((eqv? rem 0))
               ((fx<? rem 4)
                (put #\xFFFD))
               (else
                (let ((w0 (bytevector-u32-ref bv i endian))
                      (i^ (fx+ i 4))
                      (rem^ (fx- rem 4)))
                  (cond
                    ((or (fx<=? #xD800 w0 #xDFFF)
                         (fx>? w0 #x10FFFF))
                     ;; Surrogate pair or out of range
                     (put #\xFFFD)
                     (lp i^ rem^))
                    (else
                     (put (integer->char w0))
                     (lp i^ rem^)))))))))))))

(define (bytevector-address bv)
  (sys:bytevector-address bv))

(define (bytevector-truncate! bv n)
  (let ((old-len (bytevector-length bv))
        (word-size (if (> (fixnum-width) 32) 8 4)))
    (cond ((eqv? n 0)
           #vu8())
          ((fx=? n old-len)
           bv)
          (else
           (assert (fx<=? 0 n (bytevector-length bv)))
           ;; The primitive wants to know if the bytevector shrinks so
           ;; much that the updated seek mark no longer covers the
           ;; whole bytevector.
           (let* ((old (fxand (fx+ old-len (fx- word-size 1)) (fx- word-size)))
                  (new (fxand (fx+ n (fx- word-size 1)) (fx- word-size)))
                  (diff (fx- old new)))
             (if (eqv? diff 0)
                 ($bytevector-truncate! bv n #f #f)
                 ($bytevector-truncate! bv n (fxdiv new word-size)
                                        (fxdiv diff word-size)))))))))
