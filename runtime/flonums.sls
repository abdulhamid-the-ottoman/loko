;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Primitives for flonums

(library (loko runtime flonums)
  (export
    flonum?
    fl=? fl<? fl<=? fl>? fl>=?
    flinteger? flzero? flpositive? flnegative?
    flodd? fleven? flfinite? flinfinite? flnan?
    flmax flmin
    fl+ fl* fl- fl/
    flabs
    fldiv-and-mod fldiv flmod
    fldiv0-and-mod0 fldiv0 flmod0
    flnumerator fldenominator
    flfloor flceiling fltruncate flround
    flexp fllog flsin flcos fltan
    flasin flacos flatan flsqrt flexpt
    fixnum->flonum)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs syntax-case)
    (prefix (rnrs) sys:)
    (rnrs arithmetic fixnums)
    (loko system $primitives))

;; More digits than can be used
(define e      2.7182818284590452354)
(define ln2    0.69314718055994530942)
(define ln10   2.30258509299404568402)
(define pi     3.14159265358979323846)
(define pi/2   1.57079632679489661923)
(define pi/4   0.78539816339744830962)
(define /sqrt2 0.70710678118654752440)

(define (flonum? obj) (sys:flonum? obj))

;; real->flonum is in arithmetic

(define-syntax define-comparator
  (lambda (x)
    (syntax-case x ()
      ((_ (CMP args ...) body ...)
       #'(define CMP
           (case-lambda
             ((args ...)
              body ...)
             ((a b c)
              (let ((x (CMP a b)) (y (CMP b c)))
                (and x y)))
             ((a b c d)
              (let ((x (CMP a b)) (y (CMP b c)) (z (CMP c d)))
                (and x y z)))
             ((a b c d . e)
              (let lp ((d d) (e e) (ret (CMP a b c d)))
                (if (null? e)
                    ret
                    (lp (car e) (cdr e)
                        (and (CMP d (car e)) ret)))))))))))

(define-comparator (fl=? a b) (sys:fl=? a b))

(define-comparator (fl<? a b) (sys:fl>? b a))      ;TODO: native

(define-comparator (fl<=? a b) (or (fl<? a b) (fl=? a b)))  ;TODO: native

(define-comparator (fl>? a b) (sys:fl>? a b))

(define-comparator (fl>=? a b) (or (fl>? a b) (fl=? a b)))  ;TODO: native

(define (flinteger? a)
  (and (flfinite? a)
       (fl=? a (fltruncate a))))

(define (flzero? a)
  (fl=? a 0.0))

(define (flpositive? a)
  (fl>? a 0.0))

(define (flnegative? a)
  (fl<? a 0.0))

(define (flodd? a)
  (if (flinteger? a)
      (let ((x (fl/ a 2.0)))
        (not (fl=? x (fltruncate x))))
      (assertion-violation 'flodd? "Expected an integer" a)))

(define (fleven? a)
  (if (flinteger? a)
      (let ((x (fl/ a 2.0)))
        (fl=? x (fltruncate x)))
      (assertion-violation 'fleven? "Expected an integer" a)))

(define (flfinite? a)
  (and (fl=? a a)
       (not (flinfinite? a))))

(define (flinfinite? a)
  (and (fl=? a (fl+ a a))
       (not (fl=? a 0.0))))

(define (flnan? a)
  (not (fl=? a a)))

(define flmax
  (case-lambda
    ((a b)
     ;; Assumes that (sys:flmax a +nan.0) => +nan.0
     (if (flnan? a)
         a
         (sys:flmax a b)))
    ((a)
     (sys:flmax a a))
    ((a b c)
     (sys:flmax (sys:flmax a b) c))
    ((a b c . x*)
     (fold-left flmax (flmax a b c) x*))))

(define flmin
  (case-lambda
    ((a b)
     (if (flnan? a)
         a
         (sys:flmin a b)))
    ((a)
     (sys:flmin a a))
    ((a b c)
     (sys:flmin (sys:flmin a b) c))
    ((a b c . x*)
     (fold-left flmin (flmin a b c) x*))))

(define fl+
  (case-lambda
    ((a b)
     (sys:fl+ a b))
    ((a)
     (sys:fl+ a 0.0))
    ((a b c)
     (sys:fl+ (sys:fl+ a b) c))
    ((a b c . x*)
     (fold-left fl+ (fl+ a b c) x*))
    (()
     0.0)))

(define fl*
  (case-lambda
    ((a b)
     (sys:fl* a b))
    ((a)
     (sys:fl* a 1.0))
    ((a b c)
     (sys:fl* (sys:fl* a b) c))
    ((a b c . x*)
     (fold-left fl* (fl* a b c) x*))
    (()
     1.0)))

(define fl-
  (case-lambda
    ((a b)
     (sys:fl- a b))
    ((a)
     (sys:fl* a -1.0))
    ((a b c)
     (sys:fl- (sys:fl- a b) c))
    ((a b c . x*)
     (fold-left fl- (fl- a b c) x*))))

(define fl/
  (case-lambda
    ((a b)
     (sys:fl/ a b))
    ((a)
     (sys:fl/ 1.0 a))
    ((a b c)
     (sys:fl/ (sys:fl/ a b) c))
    ((a b c . x*)
     (fold-left fl/ (fl/ a b c) x*))))

(define (flabs a)
  (sys:flabs a))

(define (fldiv-and-mod a b)
  (cond ((flnegative? b)
         (let ((d (flceiling (fl/ a b))))
           (let ((m (fl- a (fl* b d))))
             (values d m))))
        (else
         (let ((d (flfloor (fl/ a b))))
           (let ((m (fl- a (fl* b d))))
             (values d m))))))

(define (fldiv a b)
  (cond ((flnegative? b)
         (flceiling (fl/ a b)))
        (else
         (flfloor (fl/ a b)))))

(define (flmod a b)
  (let-values (((_ r) (fldiv-and-mod a b)))
    r))

(define (fldiv0-and-mod0 a b)
  (let-values (((d m) (fldiv-and-mod a b)))
    (if (flnegative? b)
        (if (fl<? m (fl/ (fl- b) 2.0))
            (values d m)
            (values (fl- d 1.0) (fl+ m b)))
        (if (fl<? m (fl/ b 2.0))
            (values d m)
            (values (fl+ d 1.0) (fl- m b))))))

(define (fldiv0 a b)
  (let-values (((q _) (fldiv0-and-mod0 a b)))
    q))

(define (flmod0 a b)
  (let-values (((_ r) (fldiv0-and-mod0 a b)))
    r))

(define (flnumerator a)
  (assert (flonum? a))
  (if (flfinite? a)
      (if (fl=? a 0.0)
          a
          (inexact (numerator (exact a))))
      a))

(define (fldenominator a)
  (assert (flonum? a))
  (if (flfinite? a)
      (inexact (denominator (exact a)))
      (if (flnan? a)
          a
          1.0)))

(define (flfloor fl) (sys:flfloor fl))

(define (flceiling fl) (sys:flceiling fl))

(define (fltruncate fl) (sys:fltruncate fl))

(define (flround fl) (sys:flround fl))

;; e^x
(define (flexp x)
  (cond
    ((not (flfinite? x))
     (cond
       ((eqv? x -inf.0) 0.0)
       ((eqv? x +inf.0) +inf.0)
       (else x)))
    ((fl<=? (flabs x) 1.0)
     (do ((n 40 (fx- n 1))
          (ret 1.0 (fl+ 1.0 (fl* x (fl/ ret (fixnum->flonum n))))))
         ((eqv? n 0) ret)))
    (else
     (let ((e (flexp (fl/ x 2.0))))
       (fl* e e)))))

(define fllog
  (case-lambda
    ((a)
     (cond
       ((not (flonum? a))
        (assertion-violation 'fllog "Expected a flonum" a))
       ((fl=? a +inf.0) +inf.0)
       ((fl=? a 0.0) -inf.0)
       ((flnegative? a) +nan.0)
       ((flnan? a) a)
       ((fl=? a 2.0) ln2)
       ((fl=? a 10.0) ln10)
       ((fl<? 0.0 a 2.0)
        (let ((x (fl- a 1.0)))
          (do ((x^n x (fl* x^n x))
               (sign 1.0 (fl- sign))
               (n 1.0 (fl+ n 1.0))
               (ret 0.0 (fl+ ret (fl/ (fl* sign x^n) n))))
              ((fl>=? n 30.0) ret))))
       (else
        (fl+ ln2 (fllog (fl/ a 2.0))))))
    ((a b)
     (fl/ (fllog a) (fllog b)))))

;; TODO: the transcendental functions are better computed using
;; Chebyshev polynomials.

(define (%flfac n)
  ;; XXX: This has no precision left at n=35
  (let lp ((n n) (ret 1.0))
    (if (fl<=? n 1.0)
        ret
        (lp (fl- n 1.0) (fl* (inexact n) ret)))))

(define (%flnorm x)
  ;; XXX: This loses precision. There are papers about how to do a
  ;; better job.
  (flmod0 x (fl* 2.0 pi)))

(define (flsin x)
  (let* ((x (%flnorm x))
         (xx (fl* x x)))
    (do ((x^n x (fl* x^n xx))
         (n 1.0 (fl+ n 2.0))
         (sign 1.0 (fl- sign))
         (ret 0.0 (fl+ ret (fl/ (fl* sign x^n)
                                (%flfac n)))))
        ((fl>=? n 15.0) ret))))

(define (flcos x)
  (let* ((x (%flnorm x))
         (xx (fl* x x)))
    (do ((x^n 1.0 (fl* x^n xx))
         (n 0.0 (fl+ n 2.0))
         (sign 1.0 (fl- sign))
         (ret 0.0 (fl+ ret (fl/ (fl* sign x^n)
                                (%flfac n)))))
        ((fl>=? n 15.0) ret))))

(define (fltan x)
  (fl/ (flsin x) (flcos x)))

(define (flasin x)
  (cond
    ((fl=? x 0.0) x)
    ((fl<? (flabs x) /sqrt2)
     (do ((xx (fl* x x))
          (x^n x (fl* x^n xx))
          (n 1.0 (fl+ n 2.0))
          (num 1.0 (fl* num n))
          (den 1.0 (fl* den (fl+ n 1.0)))
          (ret 0.0 (fl+ ret (fl* (fl/ num den) (fl/ x^n n)))))
         ((fl>=? n 35.0) ret)))
    ((fl<? 0.0 x 1.0)
     (fl- pi/2 (flasin (flsqrt (fl- 1.0 (fl* x x))))))
    ((fl<? -1.0 x 0.0)
     (fl+ (fl- pi/2) (flasin (flsqrt (fl- 1.0 (fl* x x))))))
    ((fl=? x 1.0)
     pi/2)
    ((fl=? x -1.0)
     (fl- pi/2))
    (else +nan.0)))

(define (flacos x)
  (fl- pi/2 (flasin x)))

;; arctangent, atan, atan2 or tan⁻¹, demonstrating the incredible
;; regularity of mathematics.
(define flatan
  (case-lambda
    ((x)
     (let ((xmag (flabs x)))
       (cond
         ((fl=? x 0.0) x)
         ((fl<? xmag 1.0)
          ;; atan(x) = x - x³/3 + x⁵/5 - x⁷/7 + x⁹/9 + …, if |x| < 1
          (do ((xx (fl* x x))
               (x^n xmag (fl* x^n xx))
               (n 1.0 (fl+ n 2.0))
               (sign 1.0 (fl- sign))
               (ret 0.0 (fl+ ret (fl* sign (fl/ x^n n)))))
              ((fl<=? x^n 1e-20)
               (if (or (flnegative? x) (eqv? x -0.0))
                   (fl- ret)
                   ret))))
         ((fl=? x -inf.0) (fl- pi/2))
         ((flnan? x) x)
         ((fl=? xmag 1.0) (fl* x pi/4))
         ((flnegative? x)
          ;; atan(x) = -π/2 - atan(1/x), if x<0
          (fl- (fl- pi/2) (flatan (fl/ 1.0 x))))
         (else
          ;; atan(x) = π/2 - atan(1/x), if x>0
          (fl- pi/2 (flatan (fl/ 1.0 x)))))))
    ((y x)
     (cond
       ((not (flfinite? y))
        (cond
          ((flnan? x) x)
          ((fl=? y +inf.0)
           (cond ((fl=? x +inf.0) pi/4)
                 ((fl=? x -inf.0) (fl- pi pi/4))
                 (else pi/2)))
          ((fl=? y -inf.0)
           (cond ((fl=? x +inf.0) (fl- pi/4))
                 ((fl=? x -inf.0) (fl- pi/4 pi))
                 (else (fl- pi/2))))
          (else #;+nan.0 y)))
       ((fl>? x 0.0)
        (flatan (fl/ y x)))
       ((or (fl<? x 0.0) (eqv? x -0.0))
        (cond
          ((eqv? y 0.0) pi)
          ((eqv? y -0.0) (- pi))
          ((or (fl>? y 0.0) (eqv? y 0.0))
           (fl+ (flatan (fl/ y x)) pi))
          (else
           (fl- (flatan (fl/ y x)) pi))))
       ((flnan? x) x)
       ;; x is +0.0
       ((eqv? y -0.0)
        -0.0)
       ((fl>? y 0.0)
        pi/2)
       ((fl<? y 0.0)
        (fl- pi/2))
       (else 0.0)))))

(define (flsqrt a)
  (sys:flsqrt a))

(define (flexpt base exponent)
  (cond
    ((fl=? exponent 0.0)
     ;; This was defined before code reviews became popular
     1.0)
    ((flnegative? base)
     (cond
       ((flinteger? exponent)
        (if (flodd? exponent)
            (fl- (flexp (fl* exponent (fllog (flabs base)))))
            (flexp (fl* exponent (fllog (flabs base))))))
       (else
        +nan.0)))                       ;complex
    (else
     (flexp (fl* exponent (fllog base))))))

(define (fixnum->flonum x) (sys:fixnum->flonum x)))
