;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Equality

;; The CASE syntax doesn't work until eqv? is defined.

(library (loko runtime equal)
  (export
    eq? eqv? equal?
    equal-hash
    ;; not public
    eq-hash eqv-hash)
  (import
    (except (rnrs) eq? eqv? equal? equal-hash)
    (prefix (rnrs) sys:)
    (rnrs mutable-pairs)
    (only (loko runtime utils) bytevector-hash)
    (only (loko runtime arithmetic) int? ratnum? compnum? pcompnum? rcompnum? eqv=?)
    (only (loko system $primitives) $immsym? $object->fixnum $void?
          $box? $box-type $box-header-type-eq?))

(define (eq? x y) (sys:eq? x y))

(define (eqv? x y)
  (cond ((sys:eq? x y))
        ((not ($box? x)) #f)
        (else
         (cond
           ((not ($box? y)) #f)
           (else
            (let ((tx ($box-type x)))
              (cond ((not ($box-header-type-eq? tx 'number))
                     #f)
                    ;; The box type encodes exactness and the
                    ;; numerical data type.
                    ((not (eq? tx ($box-type y)))
                     #f)
                    (else
                     (eqv=? x y)))))))))

(define (make-xorshift32 seed)
  ;; http://www.jstatsoft.org/v08/i14/paper
  (let ((state seed))
    (lambda ()
      (let* ((y state)
             (y (fxxor y (fxarithmetic-shift y 13)))
             (y (fxxor y (fxarithmetic-shift y -17)))
             (y (fxxor y (fxarithmetic-shift y 5)))
             (y (fxand y #xffffffff)))
        (set! state y)
        y))))

;; This code is from the paper "Efficient Nondestructive Equality
;; Checking for Trees and Graphs" by Michael D. Adams and R. Kent
;; Dybvig (ICFP '08). It could call r5rs-equal? directly if there were
;; no cyclic structures on the lexical level, no set-cxr! and no
;; vector-set! in the whole program.

(define (equal? x y)
  (define xorshift32 #f)
  (define (random x)
    (unless (procedure? xorshift32)
      (set! xorshift32 (make-xorshift32 2463534242)))
    (fxmod (xorshift32) x))
  (define box? pair?)
  (define box list)
  (define unbox car)
  (define set-box! set-car!)

  (define (union-find ht x y)
    (define (find b)
      (let ([n (unbox b)])
        (if (box? n)
            (let loop ([b b] [n n])
              (let ([nn (unbox n)])
                (if (box? nn)
                    (begin
                      (set-box! b nn)
                      (loop n nn))
                    n)))
            b)))
    (let ([bx (hashtable-ref ht x #f)]
          [by (hashtable-ref ht y #f)])
      (if (not bx)
          (if (not by)
              (let ([b (box 1)])
                (hashtable-set! ht x b)
                (hashtable-set! ht y b)
                #f)
              (let ([ry (find by)])
                (hashtable-set! ht x ry)
                #f))
          (if (not by)
              (let ([rx (find bx)])
                (hashtable-set! ht y rx)
                #f)
              (let ([rx (find bx)] [ry (find by)])
                (or (eq? rx ry)
                    (let ([nx (unbox rx)]
                          [ny (unbox ry)])
                      (if (fx>? nx ny)
                          (begin
                            (set-box! ry rx)
                            (set-box! rx (fx+ nx ny))
                            #f)
                          (begin
                            (set-box! rx ry)
                            (set-box! ry (fx+ ny nx))
                            #f)))))))))

  (define (interleave? x y k)
    (let ([ht (make-eq-hashtable)])
      (define (call-union-find x y)
        (union-find ht x y))
      (define (e? x y k)
        (if (fx<=? k 0)
            (if (fx=? k kb)
                (fast? x y (random (fx* 2 k0)))
                (slow? x y k))
            (fast? x y k)))
      (define (slow? x y k)
        (cond
          [(eq? x y) k]
          [(pair? x)
           (and (pair? y)
                (if (call-union-find x y)
                    0
                    (let ([k (e? (car x) (car y) (fx- k 1))])
                      (and k (e? (cdr x) (cdr y) k)))))]
          [(vector? x)
           (and (vector? y)
                (let ([n (vector-length x)])
                  (and (fx=? (vector-length y) n)
                       (if (call-union-find x y)
                           0
                           (let f ([i 0] [k (fx- k 1)])
                             (if (fx=? i n)
                                 k
                                 (let ([k (e? (vector-ref x i)
                                              (vector-ref y i) k)])
                                   (and k (f (fx+ i 1) k)))))))))]
          [(string? x) (and (string? y) (string=? x y) k)]
          [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
          [else (and (sys:eqv? x y) k)]))
      (define (fast? x y k)
        (let ([k (fx- k 1)])
          (cond
            [(eq? x y) k]
            [(pair? x)
             (and (pair? y)
                  (let ([k (e? (car x) (car y) k)])
                    (and k (e? (cdr x) (cdr y) k))))]
            [(vector? x)
             (and (vector? y)
                  (let ([n (vector-length x)])
                    (and (fx=? (vector-length y) n)
                         (let f ([i 0] [k k])
                           (if (fx=? i n)
                               k
                               (let ([k (e? (vector-ref x i)
                                            (vector-ref y i) k)])
                                 (and k (f (fx+ i 1) k))))))))]
            [(string? x) (and (string? y) (string=? x y) k)]
            [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
            [else (and (sys:eqv? x y) k)])))
      (and (e? x y k) #t)))

  (define (pre? x y k)
    (cond
      [(eq? x y) k]
      [(pair? x)
       (and (pair? y)
            (if (fx<=? k 0)
                k
                (let ([k (pre? (car x) (car y) (fx- k 1))])
                  (and k (pre? (cdr x) (cdr y) k)))))]
      [(vector? x)
       (and (vector? y)
            (let ([n (vector-length x)])
              (and (fx=? (vector-length y) n)
                   (let f ([i 0] [k k])
                     (if (or (fx=? i n) (fx<=? k 0))
                         k
                         (let ([k (pre? (vector-ref x i)
                                        (vector-ref y i)
                                        (fx- k 1))])
                           (and k (f (fx+ i 1) k))))))))]
      [(string? x)
       (and (string? y) (string=? x y) k)]
      [(bytevector? x)
       (and (bytevector? y) (bytevector=? x y) k)]
      [else (and (sys:eqv? x y) k)]))

  (define (r5rs-equal? x y)
    (cond ((eq? x y))
          ((pair? x)
           (and (pair? y)
                (equal? (car x) (car y))
                (equal? (cdr x) (cdr y))))
          ((vector? x)
           (and (vector? y)
                (fx=? (vector-length x)
                      (vector-length y))
                (let lp ((i 0))
                  (or (fx=? (vector-length x) i)
                      (and (equal? (vector-ref x i)
                                   (vector-ref y i))
                           (lp (fx+ i 1)))))))
          ((string? x)
           (and (string? y) (string=? x y)))
          ((bytevector? x)
           (and (bytevector? y) (bytevector=? x y)))
          (else (sys:eqv? x y))))

  (define k0 400)
  (define kb -40)

  (let ([k (pre? x y k0)])
    (and k (or (fx>? k 0) (interleave? x y 0)))))

;; FNV-1 hash. Assumes a 64-bit machine.
(define FNV_prime 16777619)
(define offset_basis 2166136261)

(define-syntax FNV-combine
  (lambda (x)
    (syntax-case x ()
      [(_ prev next)
       #'(fxand (fxxor (fx* prev FNV_prime) next)
                #xffffffff)])))

(define (eq-hash x)
  (cond ((symbol? x)
         (symbol-hash x))
        ((fixnum? x)
         (if (fxnegative? x)
             (fxnot x)
             x))
        ((char? x)
         (char->integer x))
        (else
         (do ((b (fxand ($object->fixnum x) (greatest-fixnum))
                 (fxarithmetic-shift-right b 8))
              (d offset_basis (FNV-combine d (fxand b #xff))))
             ((sys:eqv? b 0) d)))))

(define (eqv-hash x)
  (cond ((fixnum? x)
         (if (fxnegative? x)
             (fxnot x)
             x))
        ((symbol? x) (symbol-hash x))
        ((char? x)
         (char->integer x))
        ((flonum? x)
         ;; XXX: this only works because flonums are not boxed
         (eq-hash x))
        ((int? x)
         (bitwise-bit-field x 0 32))
        ((ratnum? x)
         (bitwise-and (+ (eqv-hash (numerator x))
                         (eqv-hash (denominator x)))
                      (greatest-fixnum)))
        ((rcompnum? x)
         (bitwise-and (+ (eqv-hash (real-part x))
                         (eqv-hash (imag-part x)))
                      (greatest-fixnum)))
        ((pcompnum? x)
         (bitwise-and (+ (eqv-hash (magnitude x))
                         (eqv-hash (angle x)))
                      (greatest-fixnum)))
        (else (eq-hash x))))

(define (recursive-hash x counter d)
  (cond
    ((fx<=? counter 0)
     (values counter d))
    (else
     (let ((counter (fx- counter 1)))
       (cond
         ((pair? x)
          (let-values ([(counter^ d^) (recursive-hash (car x) counter d)])
            (recursive-hash (cdr x) counter^ (FNV-combine 1 d^))))

         ((vector? x)
          (let lp ((i 0) (counter counter) (d d))
            (if (or (fx=? i (vector-length x)) (fx=? i 24))
                (values counter d)
                (let-values ([(counter^ d^) (recursive-hash (vector-ref x i) counter d)])
                  (lp (fx+ i 1) counter^ (FNV-combine i d^))))))

         ((symbol? x)
          (values counter (FNV-combine d (symbol-hash x))))

         ((bytevector? x)
          (values counter (FNV-combine d (bytevector-hash x))))

         ((string? x)
          (values counter (FNV-combine d (string-hash x))))

         ((number? x)
          (values counter (FNV-combine d (eqv-hash x))))

         ((or (char? x) (null? x) (boolean? x))
          (values counter (FNV-combine d (eq-hash x))))

         (else
          (values counter (FNV-combine d 474747))))))))

(define (equal-hash x)
  (let-values ([(_ hash) (recursive-hash x 24 offset_basis)])
    (if (fxnegative? hash)
        (fxnot hash)
        hash))))
