;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Enumeration sets

(library (loko runtime enums)
  (export
    enum-set?
    make-enumeration
    enum-set-universe enum-set-indexer enum-set-constructor
    enum-set->list enum-set-member? enum-set-subset?
    enum-set=? enum-set-union enum-set-intersection
    enum-set-difference enum-set-complement
    enum-set-projection)
  (import
    (rnrs arithmetic bitwise)
    (rnrs arithmetic fixnums)
    (rnrs base)
    (rnrs control)
    (rnrs hashtables)
    (rnrs lists)
    (rnrs records syntactic)
    (only (rnrs io simple) display)
    (only (loko) record-writer))

(define-record-type enum-type
  (opaque #t)
  (sealed #t)
  (nongenerative)
  (fields symbols ht))

(define-record-type enum-set
  (sealed #t)
  (nongenerative)
  (fields type v))

(define (delete-duplicates x*)
  (let ((seen (make-eq-hashtable)))
    (let lp ((ret '())
             (x* x*))
      (cond ((null? x*)
             (reverse ret))
            ((hashtable-contains? seen (car x*))
             (lp ret (cdr x*)))
            (else
             (hashtable-set! seen (car x*) #t)
             (lp (cons (car x*) ret) (cdr x*)))))))

(define (make-enumeration symbol-list)
  (unless (for-all symbol? symbol-list)
    (assertion-violation 'make-enumeration
                         "Expected a list of symbols" symbol-list))
  (let ((ht (make-eq-hashtable))
        (symbol-list (delete-duplicates symbol-list)))
    (do ((s* symbol-list (cdr s*))
         (i 0 (fx+ i 1)))
        ((null? s*)
         (let ((t (make-enum-type symbol-list ht)))
           (make-enum-set t (- (expt 2 i) 1))))
      (hashtable-set! ht (car s*) i))))

(define (enum-set-universe enum-set)
  (let* ((t (enum-set-type enum-set))
         (s* (enum-type-symbols t)))
    (make-enum-set t (- (expt 2 (length s*)) 1))))

(define (enum-set-indexer enum-set)
  (let ((ht (enum-type-ht (enum-set-type enum-set))))
    (lambda (s) (hashtable-ref ht s #f))))

(define (enum-set-constructor enum-set)
  (let* ((t (enum-set-type enum-set))
         (ht (enum-type-ht t)))
    (lambda (s*)
      (unless (for-all symbol? s*)
        (assertion-violation #f "Expected a list of symbols" s*))
      (do ((s* s* (cdr s*))
           (v 0 (cond ((hashtable-ref ht (car s*) #f)
                       => (lambda (b) (bitwise-ior v (expt 2 b))))
                      (else
                       (assertion-violation #f "Expected symbols in the enum-set universe"
                                            (car s*) (apply list (enum-type-symbols t)))))))
          ((null? s*)
           (make-enum-set t v))))))

(define (enum-set->list enum-set)
  (let ((t (enum-set-type enum-set))
        (v (enum-set-v enum-set)))
    (do ((s* (enum-type-symbols t) (cdr s*))
         (b 1 (bitwise-arithmetic-shift-left b 1))
         (ret '() (if (eqv? 0 (bitwise-and b v))
                      ret
                      (cons (car s*) ret))))
        ((null? s*) (reverse ret)))))

(define (enum-set-member? symbol enum-set)
  (let ((t (enum-set-type enum-set))
        (v (enum-set-v enum-set)))
    (let ((ht (enum-type-ht t)))
      (cond ((hashtable-ref ht symbol #f) =>
             (lambda (b) (bitwise-bit-set? v b)))
            (else #f)))))

(define (enum-set-subset? enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (eqv? 0 (bitwise-and v1 (bitwise-not v2)))
        (and (for-all (lambda (sym) (memq sym (enum-type-symbols t2)))
                      (enum-type-symbols t1))
             (for-all (lambda (sym) (enum-set-member? sym enum-set2))
                      (enum-set->list enum-set1))))))

(define (enum-set=? enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (= v1 v2)
        ;; lazy
        (and (enum-set-subset? enum-set1 enum-set2)
             (enum-set-subset? enum-set2 enum-set1)))))

(define (enum-set-union enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (make-enum-set t1 (bitwise-ior v1 v2))
        (assertion-violation 'enum-set-union "Expected enum-sets of the same type"
                             enum-set1 enum-set2))))

(define (enum-set-intersection enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (make-enum-set t1 (bitwise-and v1 v2))
        (assertion-violation 'enum-set-intersection "Expected enum-sets of the same type"
                             enum-set1 enum-set2))))

(define (enum-set-difference enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (make-enum-set t1 (bitwise-and v1 (bitwise-not v2)))
        (assertion-violation 'enum-set-difference "Expected enum-sets of the same type"
                             enum-set1 enum-set2))))

(define (enum-set-complement enum-set)
  (let ((t (enum-set-type enum-set))
        (v (enum-set-v enum-set)))
    (let ((s* (enum-type-symbols t)))
      (make-enum-set t (bitwise-xor v (- (expt 2 (length s*)) 1))))))

(define (enum-set-projection enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2)))
    (if (eq? t1 t2)
        enum-set1
        (let ((ht (enum-type-ht t2)))
          (do ((s* (enum-type-symbols t1) (cdr s*))
               (b 1 (bitwise-arithmetic-shift-left b 1))
               (v 0 (cond ((eqv? 0 (bitwise-and v1 b)) v)
                          ((hashtable-ref ht (car s*) #f)
                           => (lambda (b) (bitwise-ior v (expt 2 b))))
                          (else v))))
              ((null? s*)
               (make-enum-set t2 v)))))))

(record-writer (record-type-descriptor enum-set)
               (lambda (v p wr)
                 (display "#[enum-set " p)
                 (wr (enum-set->list v) p)
                 (display "]" p))))
