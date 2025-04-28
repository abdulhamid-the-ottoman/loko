;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Misc utilities used by compiler-related code

(library (loko compiler utils)
  (export
    hashtable->minimal-perfect-hashtable
    make-random-u16-maker
    division-magic)
  (import
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map))

(define (hashtable->minimal-perfect-hashtable dict)
  ;; Algorithm from http://stevehanov.ca/blog/index.php?id=119
  (let ((EMPTY (vector 'empty)))
    (let* ((size (hashtable-size dict))
           (buckets (make-vector size '()))
           (G (make-vector size 0))
           (K (make-vector size))
           (V (make-vector size EMPTY)))
      (vector-for-each
       (lambda (key)
         (let ((idx (fxmod (string-hash* key) size)))
           (vector-set! buckets idx (cons key (vector-ref buckets idx)))))
       (vector-sort string<? (hashtable-keys dict))) ;reproducability
      ;; Longest bucket first
      (vector-sort! (lambda (x y) (> (length x) (length y)))
                    buckets)
      (let loop-bucket ((b 0))
        (unless (= b size)
          (let ((bucket (vector-ref buckets b)))
            (if (and (pair? bucket) (pair? (cdr bucket)))
                (let lp ((d 1) (b* bucket) (slots '()))
                  (cond ((pair? b*)
                         (let ((slot (fxmod (string-hash* (car b*) d) size)))
                           (if (or (not (eq? (vector-ref V slot) EMPTY))
                                   (memv slot slots))
                               (lp (+ d 1) bucket '())
                               (lp d (cdr b*) (cons slot slots)))))
                        (else
                         (vector-set! G (fxmod (string-hash* (car bucket)) size) d)
                         (for-each (lambda (slot key)
                                     (vector-set! V slot (hashtable-ref dict key #f))
                                     (vector-set! K slot key))
                                   (reverse slots) bucket)
                         (loop-bucket (+ b 1)))))
                (let ((free
                       (let lp ((i 0) (ret '()))
                         (cond ((= i (vector-length V))
                                ret)
                               ((eq? (vector-ref V i) EMPTY)
                                (lp (+ i 1) (cons i ret)))
                               (else
                                (lp (+ i 1) ret))))))
                  ;; All remaining buckets are empty or contain
                  ;; only one element. They are assigned to the
                  ;; free slots.
                  (let lp ((b b) (free free))
                    (unless (= b size)
                      (let ((bucket (vector-ref buckets b)))
                        (cond ((pair? bucket)
                               (let ((slot (car free))
                                     (key (car bucket)))
                                 (vector-set! G (fxmod (string-hash* key) size)
                                              (- -1 slot))
                                 (vector-set! V slot (hashtable-ref dict key #f))
                                 (vector-set! K slot key)
                                 (lp (+ b 1) (cdr free))))
                              (else
                               (lp (+ b 1) free)))))))))))
      (values G K V))))

;; Multiply-with-carry (apparently by George Marsagli, says Wikipedia).
(define (make-random-u16-maker)
  (let ((w #xc0fec0fe)
        (z #x5ea5ea5a))
    (lambda ()
      (define fxasl fxarithmetic-shift-left)
      (define fxasr fxarithmetic-shift-right)
      (set! z (fx+ (fx* 36969 (fxand z #xffff))
                   (fxasr z 16)))
      (set! w (fx+ (fx* 18000 (fxand w #xffff))
                   (fxasr w 16)))
      (fxand (fx+ (fxasl (fxand z #xffff) 16)
                  (fxand w #xfffff))
             #xffff))))

;; Compute the magic number and shift amount for division with a known
;; divisor. From Hacker's Delight.
(define (division-magic d W)
  (assert (not (eqv? d 0)))
  (let* ((twoW-1 (expt 2 (- W 1)))
         (ad (abs d))
         (anc (let ((t (+ twoW-1 (if (< d 0) 1 0))))
                (- t 1 (mod t ad)))))
    (assert (<= 2 ad (- twoW-1 1)))
    (let-values (((q1 r1) (div-and-mod twoW-1 anc))
                 ((q2 r2) (div-and-mod twoW-1 ad)))
      (let lp ((p (- W 1)) (q1 q1) (r1 r1) (q2 q2) (r2 r2))
        (let ((p (+ p 1))
              (q1 (* q1 2))
              (r1 (* r1 2))
              (q2 (* q2 2))
              (r2 (* r2 2)))
          (let-values (((q1 r1) (if (>= r1 anc)
                                    (values (+ q1 1) (- r1 anc))
                                    (values q1 r1)))
                       ((q2 r2) (if (>= r2 ad)
                                    (values (+ q2 1) (- r2 ad))
                                    (values q2 r2))))
            (let ((delta (- ad r2)))
              (if (or (< q1 delta) (and (= q1 delta) (eqv? r1 0)))
                  (lp p q1 r1 q2 r2)
                  (let ((M (+ q2 1))
                        (s (- p W)))
                    (let ((M (cond ((< -1 M (expt 2 (- W 1)))
                                    M)
                                   (else
                                    (assert (<= twoW-1 M (- (expt 2 W) 1)))
                                    (- M (expt 2 W))))))
                      (let ((M (if (< d 0) (- M) M)))
                        (values M s)))))))))))))
