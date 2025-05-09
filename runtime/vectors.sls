;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019 G. Weinholt
#!r6rs

;;; Vector primitives

(library (loko runtime vectors)
  (export
    vector? make-vector vector vector-length vector-ref
    vector-set! vector->list list->vector vector-fill!
    vector-map vector-for-each)
  (import
    (except (rnrs)
            vector? make-vector vector vector-length vector-ref
            vector-set! vector->list list->vector vector-fill!
            vector-map vector-for-each)
    (prefix (only (rnrs) vector? vector-length vector-ref vector-set!)
            sys:)
    (loko system $primitives))

(define (vector? x) (sys:vector? x))

(define make-vector
  (case-lambda
    ((n)
     (make-vector n 0))
    ((n fill)
     (cond ((not (and (fixnum? n) (fx>=? n 0)))
            (assertion-violation 'make-vector "Expected a non-negative fixnum" n fill))
           ((eqv? n 0) '#())
           (else
            ($make-vector n fill))))))

(define vector
  (case-lambda
    (() '#())
    ((a)
     (let ((v ($make-vector 1)))
       (vector-set! v 0 a)
       v))
    ((a b)
     (let ((v ($make-vector 2)))
       (vector-set! v 0 a)
       (vector-set! v 1 b)
       v))
    ((a b c)
     (let ((v ($make-vector 3)))
       (vector-set! v 0 a)
       (vector-set! v 1 b)
       (vector-set! v 2 c)
       v))
    (x
     (list->vector x))))

(define (vector-length x) (sys:vector-length x))

(define (vector-ref v i) (sys:vector-ref v i))

(define (vector-set! v i o) (sys:vector-set! v i o))

(define (vector->list v)
  (do ((i (fx- (vector-length v) 1) (fx- i 1))
       (ret '() (cons (vector-ref v i) ret)))
      ((fx=? i -1) ret)))

(define (list->vector list)
  ;; XXX: finds cycles using length
  (let* ((len (length list))
         (v (make-vector len)))
    (do ((i 0 (fx+ i 1))
         (list list (cdr list)))
        ((fx=? i len) v)
      (vector-set! v i (car list)))))

(define (vector-fill! v fill)
  (do ((i 0 (fx+ i 1)))
      ((fx=? i (vector-length v)))
    (vector-set! v i fill)))

(define vector-map
  (case-lambda
    ((proc v)
     ;; TODO: multiple returns of proc
     (let ((ret (make-vector (vector-length v))))
       (do ((i 0 (fx+ i 1)))
           ((fx=? i (vector-length ret)) ret)
         (vector-set! ret i (proc (vector-ref v i))))))
    ((proc v1 v2)
     (assert (fx=? (vector-length v1) (vector-length v2)))
     (do ((ret (make-vector (vector-length v1)))
          (i 0 (fx+ i 1)))
         ((fx=? i (vector-length ret)) ret)
       (vector-set! ret i (proc (vector-ref v1 i) (vector-ref v2 i)))))
    ((proc v0 . v*)
     (for-each (lambda (v)
                 (unless (fx=? (vector-length v0)
                               (vector-length v))
                   (apply assertion-violation 'vector-map
                          "Expected vectors of the same length"
                          v0 v*)))
               v*)
     (do ((ret (make-vector (vector-length v0)))
          (i 0 (fx+ i 1)))
         ((fx=? i (vector-length ret)) ret)
       (vector-set! ret i
                    (apply proc (vector-ref v0 i)
                           (map (lambda (v) (vector-ref v i)) v*)))))))

(define vector-for-each
  (case-lambda
    ((proc v)
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (vector-length v)))
       (proc (vector-ref v i))))
    ((proc v1 v2)
     (assert (fx=? (vector-length v1) (vector-length v2)))
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (vector-length v1)))
       (proc (vector-ref v1 i)
             (vector-ref v2 i))))
    ;; TODO: Proc is always called in the same dynamic environment as
    ;; vector-for-each itself.
    ((proc v0 . v*)
     (for-each (lambda (v)
                 (unless (fx=? (vector-length v0)
                               (vector-length v))
                   (apply assertion-violation 'vector-for-each
                          "Expected vectors of the same length"
                          v0 v*)))
               v*)
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (vector-length v0)))
       (apply proc (vector-ref v0 i)
              (map (lambda (v) (vector-ref v i)) v*)))))))
