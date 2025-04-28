;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; A place for various utilities

;; This should be kept portable. It is used when bootstrapping.

(library (loko runtime utils)
  (export
    map-in-order
    string-split
    last
    butlast
    take
    strip-akku-prefix
    bytevector-hash
    string-hash*
    string-index)
  (import
    (rnrs))

(define map-in-order
  (case-lambda
    ((f x*)
     (let lp ((ret '()) (x* x*))
       (if (null? x*)
           (reverse ret)
           (lp (cons (f (car x*)) ret)
               (cdr x*)))))
    ((f x* y*)
     (let lp ((ret '()) (x* x*) (y* y*))
       (if (null? x*)
           (reverse ret)
           (lp (cons (f (car x*) (car y*)) ret)
               (cdr x*) (cdr y*)))))
    ((f x* y* z*)
     (let lp ((ret '()) (x* x*) (y* y*) (z* z*))
       (if (null? x*)
           (reverse ret)
           (lp (cons (f (car x*) (car y*) (car z*)) ret)
               (cdr x*) (cdr y*) (cdr z*)))))
    ((f x* y* . z**)
     (let lp ((ret '()) (x* x*) (y* y*) (z** z**))
       (if (null? x*)
           (reverse ret)
           (lp (cons (apply f (car x*) (car y*) (map car z**)) ret)
               (cdr x*) (cdr y*) (map cdr z**)))))))

(define (string-split str c)
  (let lp ((start 0) (end 0))
    (cond ((fx=? end (string-length str))
           (list (substring str start end)))
          ((char=? c (string-ref str end))
           (cons (substring str start end)
                 (lp (fx+ end 1) (fx+ end 1))))
          (else
           (lp start (fx+ end 1))))))

(define (last x)
  (let last ((x x))
    (if (null? (cdr x))
        (car x)
        (last (cdr x)))))

(define (butlast x)
  (if (null? (cdr x))
      '()
      (cons (car x) (butlast (cdr x)))))

(define (take x i)
  (if (fxzero? i)
      '()
      (cons (car x)
            (take (cdr x) (fx- i 1)))))

(define (strip-akku-prefix x)
  (define prefix ".akku/lib/")
  (define plen (string-length prefix))
  (let lp ((i 0))
    (cond ((and (fx>=? (string-length x) (fx+ plen i))
                (string=? prefix (substring x i (fx+ plen i))))
           (substring x (fx+ i plen) (string-length x)))
          ((fx<? i (fx- (string-length x) plen))
           (lp (fx+ i 1)))
          (else x))))

;;; Bytevector and string hashes

;; These are used in various situations when hashing objects both
;; during bootstrap and normal run time. The hashes can be encoded in
;; 32 bits. To be fast, they only hash a fixed amount of the input.

(define offset_basis 2166136261)
(define FNV_prime 16777619)

(define-syntax FNV-combine
  (lambda (x)
    (syntax-case x ()
      [(_ d b)
       #'(if (fx>? (fixnum-width) 57)
             (fxand (fxxor (fx* d FNV_prime) b) #xffffffff)
             (bitwise-and (bitwise-xor (* d FNV_prime) b) #xffffffff))])))

(define bytevector-hash
  (case-lambda
    ((bv)
     (bytevector-hash bv offset_basis))
    ((bv d)
     (let ((len (bytevector-length bv)))
       (if (or (fx<? len 19)     ;approx. breakeven for the case below
               (not (eqv? d offset_basis)))
           (do ((i 0 (fx+ i 1))
                (d d (FNV-combine d (bytevector-u8-ref bv i))))
               ((fx=? i len) d))
           ;; Sample the ends.
           (do ((i 0 (fx+ i 1))
                (len-1 (fx- len 1))
                (d d (FNV-combine (FNV-combine d (bytevector-u8-ref bv i))
                                  (bytevector-u8-ref bv (fx- len-1 i)))))
               ((fx=? i 8)
                ;; Sample approx. eight bytes evenly distributed
                ;; around the bytevector.
                (do ((skip (fxdiv len 8))
                     (i 8 (fx+ i skip))
                     (d d (FNV-combine d (bytevector-u8-ref bv i))))
                    ((fx>=? i len)
                     (FNV-combine d len))))))))))

(define string-hash*
  (case-lambda
    ((s)
     (string-hash* s offset_basis))
    ((s d)
     (let ((len (string-length s)))
       (if (or (fx<? len 16)
               (not (eqv? d offset_basis)))
           ;; XXX: when d is set explicitly, use every part of the
           ;; string (for hashtable->minimal-perfect-hashtable).
           (do ((i 0 (fx+ i 1))
                (d d (FNV-combine d (char->integer (string-ref s i)))))
               ((fx=? i len) d))
           ;; Sample the ends.
           (do ((i 0 (fx+ i 1))
                (len-1 (fx- len 1))
                (d d (FNV-combine (FNV-combine d (char->integer (string-ref s i)))
                                  (char->integer (string-ref s (fx- len-1 i))))))
               ((fx=? i 8)
                ;; Sample approx. eight characters evenly distributed
                ;; around the string.
                (do ((skip (fxdiv len 8))
                     (i 8 (fx+ i skip))
                     (d d (FNV-combine d (char->integer (string-ref s i)))))
                    ((fx>=? i len)
                     (FNV-combine d len))))))))))

(define (string-index s c)
  (let ((len (string-length s)))
    (let lp ((i 0))
      (and (not (fx=? i len))
           (let ((c* (string-ref s i)))
             (if (eq? c c*)             ;XXX: not portable
                 i
                 (lp (fx+ i 1)))))))))
