;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Pseudo-random number generation

(library (loko runtime random)
  (export
    get-random-bytevector-n!
    get-random-u8)
  (import
    (rnrs)
    (only (loko runtime init) get-random-seed))

;; RFC 7539. Industria also has a version of this code.
(define (chacha20-block! out key block-count nonce)
  (define-syntax rot32
    (syntax-rules ()
      ((_ n^ a^)
       (let ((n n^) (a a^))
         (fxior (fxarithmetic-shift-left (fxand n (fx- (fxarithmetic-shift-left 1 (fx- 32 a)) 1))
                                         a)
                (fxarithmetic-shift-right n (fx- 32 a)))))))
  (define-syntax add32
    (syntax-rules ()
      ((_ a b) (fxand #xFFFFFFFF (fx+ a b)))))
  (define-syntax let*-quarter-round
    (syntax-rules ()
      ((_ ((a b c d) rest* ...)
          body* ...)
       (let* ((a (add32 a b)) (d (fxxor d a)) (d (rot32 d 16))
              (c (add32 c d)) (b (fxxor b c)) (b (rot32 b 12))
              (a (add32 a b)) (d (fxxor d a)) (d (rot32 d 8))
              (c (add32 c d)) (b (fxxor b c)) (b (rot32 b 7)))
         (let*-quarter-round (rest* ...) body* ...)))
      ((_ () body ...)
       (let () body ...))))
  (define (set32! bv i v) (bytevector-u32-set! bv (* i 4) v (endianness little)))
  (define (get32 bv i) (bytevector-u32-ref bv (* i 4) (endianness little)))
  (define c0 #x61707865)
  (define c1 #x3320646e)
  (define c2 #x79622d32)
  (define c3 #x6b206574)
  (let ((i0 c0)            (i1 c1)               (i2 c2)               (i3 c3)
        (i4 (get32 key 0)) (i5 (get32 key 1))    (i6 (get32 key 2))    (i7 (get32 key 3))
        (i8 (get32 key 4)) (i9 (get32 key 5))    (i10 (get32 key 6))   (i11 (get32 key 7))
        (i12 block-count)  (i13 (get32 nonce 0)) (i14 (get32 nonce 1)) (i15 (get32 nonce 2)))
    (let lp ((i 0)
             (s0  i0) (s1  i1) (s2  i2)  (s3  i3)  (s4  i4)  (s5  i5)  (s6  i6)  (s7  i7)
             (s8  i8) (s9  i9) (s10 i10) (s11 i11) (s12 i12) (s13 i13) (s14 i14) (s15 i15))
      (cond
        ((eqv? i 10)
         (set32! out 15 (add32 i15 s15))
         (set32! out 14 (add32 i14 s14))
         (set32! out 13 (add32 i13 s13))
         (set32! out 12 (add32 i12 s12))
         (set32! out 11 (add32 i11 s11))
         (set32! out 10 (add32 i10 s10))
         (set32! out 11 (add32 i11 s11))
         (set32! out 10 (add32 i10 s10))
         (set32! out 9 (add32 i9 s9))
         (set32! out 8 (add32 i8 s8))
         (set32! out 7 (add32 i7 s7))
         (set32! out 6 (add32 i6 s6))
         (set32! out 5 (add32 i5 s5))
         (set32! out 4 (add32 i4 s4))
         (set32! out 3 (add32 i3 s3))
         (set32! out 2 (add32 i2 s2))
         (set32! out 1 (add32 i1 s1))
         (set32! out 0 (add32 i0 s0)))
        (else
         (let*-quarter-round ((s0  s4 s8  s12)
                              (s1  s5 s9  s13)
                              (s2  s6 s10 s14)
                              (s3  s7 s11 s15)
                              (s0  s5 s10 s15)
                              (s1  s6 s11 s12)
                              (s2  s7 s8  s13)
                              (s3  s4 s9  s14))
           (lp (fx+ i 1)
               s0 s1 s2  s3  s4  s5  s6  s7
               s8 s9 s10 s11 s12 s13 s14 s15)))))))

(define (chacha20-keystream)
  (define buf (make-bytevector (* 16 4)))
  (define key (get-random-seed (* 4 8)))
  (define nonce (make-bytevector (* 4 3)))
  (define block-count 0)
  (define offset 0)
  (define (re-key)
    (bytevector-copy! (get-random-seed (bytevector-length nonce)) 0
                      nonce 0 (bytevector-length nonce))
    (set! block-count 0)
    (set! offset 0)
    (chacha20-block! buf key block-count nonce))
  (define (read! bv start count)
    (when (fx>? block-count #xffffffff)
      (re-key))
    (when (eqv? offset (fx* 16 4))
      (set! block-count (fx+ block-count 1))
      (set! offset 0)
      (chacha20-block! buf key block-count nonce))
    (let ((k (fxmin count (fx- (fx* 16 4) offset))))
      (bytevector-copy! buf offset bv start k)
      (set! offset (fx+ offset k))
      k))
  (define (set-position! pos)
    (let-values ([(block-count^ offset^) (fxdiv-and-mod pos (fx* 16 4))])
      (set! block-count block-count^)
      (set! offset offset^)
      (chacha20-block! buf key block-count nonce)))
  (re-key)
  (make-custom-binary-input-port "chacha20" read! #f set-position! #f))

(define *keystream* #f)

(define (init-keystream)
  (set! *keystream* (chacha20-keystream)))

(define (get-random-bytevector-n! bv start n)
  (unless *keystream*
    (init-keystream))
  (get-bytevector-n! *keystream* bv start n))

(define (get-random-u8)
  (unless *keystream*
    (init-keystream))
  (get-u8 *keystream*)))
