;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Primitives for fixnums

(library (loko runtime fixnums)
  (export
    fixnum? fixnum-width least-fixnum greatest-fixnum
    fx=? fx>? fx<? fx>=? fx<=? fxzero? fxpositive? fxnegative?
    fxodd? fxeven? fxmax fxmin fx+ fx- fx* fxdiv-and-mod
    fxdiv fxmod fxdiv0-and-mod0 fxdiv0 fxmod0
    fx+/carry fx-/carry fx*/carry fxnot fxand fxior fxxor
    fxif fxbit-count fxlength fxfirst-bit-set fxbit-set?
    fxcopy-bit fxbit-field fxcopy-bit-field fxarithmetic-shift
    fxarithmetic-shift-left fxarithmetic-shift-right
    fxrotate-bit-field fxreverse-bit-field)
  (import
    (only (rnrs base (6))
          define define-syntax syntax-rules _ ... and or let let* let-values
          assert if cond else quote
          assertion-violation car cdr null? not eqv? values
          vector-ref
          ;; TODO: These should not be needed
          div0-and-mod0 div0 mod0 expt + - *)
    (rnrs conditions (6))
    (rnrs control (6))
    (rnrs exceptions (6))
    (only (rnrs lists (6)) fold-left)
    (prefix (rnrs arithmetic fixnums (6)) sys:)
    (loko system $primitives))

(define (implementation-restriction who msg . irritants)
  (raise (condition
          (make-implementation-restriction-violation)
          (make-who-condition who)
          (make-message-condition msg)
          (make-irritants-condition irritants))))

(define (fixnum? x) (sys:fixnum? x))
(define (fixnum-width) (sys:fixnum-width))

(define (least-fixnum)
  ;; (sys:fxnot (greatest-fixnum))
  -1152921504606846976)

(define (greatest-fixnum)
  ;; (let ((half (sys:fxarithmetic-shift-left 1 (sys:fx- (fixnum-width) 2))))
  ;;   (sys:fx+ half (sys:fx- half 1)))
  1152921504606846975)

(define-syntax define-comparator
  (syntax-rules ()
    ((_ (CMP args ...) body ...)
     (define CMP
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
          (let lp ((d d)
                   (e e)
                   (ret (let ((x (CMP a b)) (y (CMP b c)) (z (CMP c d)))
                          (and x y z))))
            (if (null? e)
                ret
                (lp (car e) (cdr e)
                    (and (CMP d (car e)) ret))))))))))

(define-comparator (fx=? a b) (sys:fx=? a b))
(define-comparator (fx>? a b) (sys:fx>? a b))
(define-comparator (fx<? a b) (sys:fx<? a b))
(define-comparator (fx>=? a b) (sys:fx>=? a b))
(define-comparator (fx<=? a b) (sys:fx<=? a b))

(define (fxzero? x) (sys:fxzero? x))
(define (fxpositive? x) (sys:fxpositive? x))
(define (fxnegative? x) (sys:fxnegative? x))
(define (fxodd? x) (sys:fxodd? x #b1))
(define (fxeven? x) (sys:fxeven? x #b1))

(define fxmax
  (case-lambda
    ((a b) (if (fx>? a b) a b))
    ((a b c)
     (fxmax a (fxmax b c)))
    ((a) (assert (fixnum? a)) a)
    ((a b c . x)
     (fold-left sys:fxmax (fxmax a b c) x))))

(define fxmin
  (case-lambda
    ((a b) (if (fx<? a b) a b))
    ((a b c)
     (fxmin a (fxmin b c)))
    ((a) (assert (fixnum? a)) a)
    ((a b c . x)
     (fold-left sys:fxmin (fxmin a b c) x))))

(define (fx+ x y)
  (sys:fx+ x y))

(define (fx* x y)
  (sys:fx* x y))

(define fx-
  (case-lambda
    ((a b) (sys:fx- a b))
    ((a) (sys:fx- a))))

;; Euclidean division.
(define (fxdiv-and-mod n d)
  (assert (and (fixnum? n) (fixnum? d)))
  (cond ((eqv? d -1)
         (if (eqv? n (least-fixnum))
             (implementation-restriction 'fxdiv-and-mod
                                         "The result is not a fixnum"
                                         n d)
             (values (fx- n) 0)))
        (else
         (let-values ([(q r) ($fxquo+rem n d)])
           (if (fx>=? r 0)
               (values q r)
               (if (fxnegative? d)
                   (values (fx+ q 1) (fx- r d))
                   (values (fx- q 1) (fx+ r d))))))))

(define (fxdiv n d)
  (assert (and (fixnum? n) (fixnum? d)))
  (cond #;((eq? d 0)
           (error 'fxdiv "Division by zero." n d))
        ((eqv? d -1)
         (if (eqv? n (least-fixnum))
             (implementation-restriction 'fxdiv "The result is not a fixnum"
                                         n d)
             (fx- n)))
        (else
         (let-values ([(q r) ($fxquo+rem n d)])
           (if (fx>=? r 0)
               q
               (if (fxnegative? d)
                   (fx+ q 1)
                   (fx- q 1)))))))

(define (fxmod n d)
  (assert (and (fixnum? n) (fixnum? d)))
  (let-values ([(_ r) ($fxquo+rem n d)])
    (cond ((fx<? d 0)
           (if (fx<? r 0)
               (fx- r d)
               r))
          (else
           (if (fx<? r 0)
               (fx+ r d)
               r)))))

;; TODO: implement this without using bignums. The problem is that
;; (fxdiv b 2) != (/ b 2) or that (fx* rem 2) isn't representable,
;; so it's difficult to do the adjustments.
(define (fxdiv0-and-mod0 a b)
  (assert (and (fixnum? a) (fixnum? b)))
  (let-values (((d m) (div0-and-mod0 a b)))
    (unless (and (fixnum? d) (fixnum? m))
      (implementation-restriction 'fxdiv0-and-mod0 "The result is a fixnum" a b))
    (values d m)))

(define (fxdiv0 n d)
  ;; TODO: direct implementation
  (let-values (((q _) (fxdiv0-and-mod0 n d)))
    q))

(define (fxmod0 n d)
  ;; TODO: direct implementation
  (assert (and (fixnum? n) (fixnum? d)))
  (let-values (((_ m) (div0-and-mod0 n d)))
    (unless (fixnum? m)
      (implementation-restriction 'fxmod0 "The result is not a fixnum" n d))
    m))

;; These /carry procedures appear to be less useful than they appear
;; to be. TODO: surely there are cleverer ways of implementing them?
(define (fx+/carry fx1 fx2 fx3)
  (let-values ([(s0 s1) (div0-and-mod0 (+ fx1 fx2 fx3) (expt 2 (fixnum-width)))])
    (values s1 s0)))

(define (fx-/carry fx1 fx2 fx3)
  (let-values ([(d0 d1) (div0-and-mod0 (- fx1 fx2 fx3) (expt 2 (fixnum-width)))])
    (values d1 d0)))

(define (fx*/carry fx1 fx2 fx3)
  (let-values ([(s0 s1) (div0-and-mod0 (+ (* fx1 fx2) fx3) (expt 2 (fixnum-width)))])
    (values s1 s0)))

(define (fxnot x)
  (sys:fxnot x))

(define fxand
  (case-lambda
    (() -1)
    ((a) (assert (fixnum? a)) a)
    ((a b) (sys:fxand a b))
    ((a b c) (sys:fxand a (sys:fxand b c)))
    ((a b c d) (sys:fxand (sys:fxand a b) (sys:fxand c d)))
    ((a b c d . x)
     (fold-left sys:fxand (sys:fxand (sys:fxand a b) (sys:fxand c d)) x))))

(define fxior
  (case-lambda
    (() 0)
    ((a) (assert (fixnum? a)) a)
    ((a b) (sys:fxior a b))
    ((a b c) (sys:fxior a (sys:fxior b c)))
    ((a b c d) (sys:fxior (sys:fxior a b) (sys:fxior c d)))
    ((a b c d e) (sys:fxior a (sys:fxior (sys:fxior b c) (sys:fxior d e))))
    ((a b c d e . x)
     (fold-left sys:fxior (sys:fxior a b c d e) x))))

(define fxxor
  (case-lambda
    (() 0)
    ((a) (assert (fixnum? a)) a)
    ((a b) (sys:fxxor a b))
    ((a b c) (fxxor a (fxxor b c)))
    ((a b c d) (fxxor (fxxor a b) (fxxor c d)))
    ((a b c d . x)
     (fold-left sys:fxxor (fxxor a b c d) x))))

(define (fxif a b c)
  (fxior (fxand a b)
         (fxand (fxnot a) c)))

(define (fxbit-count v)
  ;; https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
  ;; TODO: use POPCNT
  (cond
    ((eqv? (fixnum-width) 61)
     (let ((inv (sys:fxarithmetic-shift-right v (fx- (fixnum-width) 1))))
       (let ((S '#(1 2 4 8 16 32 60))
             (B '#(#b010101010101010101010101010101010101010101010101010101010101
                   #b001100110011001100110011001100110011001100110011001100110011
                   #b111100001111000011110000111100001111000011110000111100001111
                   #b000011111111000000001111111100000000111111110000000011111111
                   #b000000000000111111111111111100000000000000001111111111111111
                   #b000000000000000000000000000011111111111111111111111111111111)))
         (let* ((c (sys:fxxor inv v))
                (c (sys:fx+ (sys:fxand (sys:fxarithmetic-shift-right c (vector-ref S 0)) (vector-ref B 0)) (sys:fxand c (vector-ref B 0))))
                (c (sys:fx+ (sys:fxand (sys:fxarithmetic-shift-right c (vector-ref S 1)) (vector-ref B 1)) (sys:fxand c (vector-ref B 1))))
                (c (sys:fx+ (sys:fxand (sys:fxarithmetic-shift-right c (vector-ref S 2)) (vector-ref B 2)) (sys:fxand c (vector-ref B 2))))
                (c (sys:fx+ (sys:fxand (sys:fxarithmetic-shift-right c (vector-ref S 3)) (vector-ref B 3)) (sys:fxand c (vector-ref B 3))))
                (c (sys:fx+ (sys:fxand (sys:fxarithmetic-shift-right c (vector-ref S 4)) (vector-ref B 4)) (sys:fxand c (vector-ref B 4))))
                (c (sys:fx+ (sys:fxand (sys:fxarithmetic-shift-right c (vector-ref S 5)) (vector-ref B 5)) (sys:fxand c (vector-ref B 5)))))
           (sys:fxxor inv c)))))
    (else
     (if (fxnegative? v)
         (fxnot (sys:fxbit-count (sys:fxnot v)))
         (let lp ((c 0) (v v))
           (if (eqv? v 0)
               c
               (lp (fx+ c 1) (fxand v (fx- v 1)))))))))

(define (fxlength x)
  (sys:fxlength x))

(define (fxfirst-bit-set x)
  (sys:fxfirst-bit-set x))

(define (fxbit-set? n k)
  (assert (fx>=? k 0))
  (if (fx>=? k (fx- (fixnum-width) 1))
      (fxnegative? n)
      (eqv? #b1 (sys:fxand #b1 (sys:fxarithmetic-shift-right n k)))))

(define (fxcopy-bit n k bit)
  (assert (fx<=? 0 bit 1))
  (let* ((mask (sys:fxarithmetic-shift-left 1 k)))
    (fxif mask
          (sys:fxarithmetic-shift-left bit k)
          n)))

(define (fxbit-field n start end)
  (assert (and (fx<=? 0 start end) (fx<? end (fixnum-width))))
  (let ((mask (fxnot (sys:fxarithmetic-shift-left -1 end))))
    (sys:fxarithmetic-shift-right (fxand n mask) start)))

(define (fxcopy-bit-field to start end from)
  (assert (and (fx<=? 0 start end) (fx<? end (fixnum-width))))
  (let* ((mask1 (sys:fxarithmetic-shift-left -1 start))
         (mask2 (fxnot (sys:fxarithmetic-shift-left -1 end)))
         (mask (fxand mask1 mask2))
         (mask3 (fxnot (sys:fxarithmetic-shift-left -1 (fx- end start)))))
    (fxif mask
          (sys:fxarithmetic-shift-left (fxand from mask3)
                                       start)
          to)))

(define (fxarithmetic-shift n k)
  (unless (sys:fx<=? (sys:fx- 1 (fixnum-width)) k (sys:fx- (fixnum-width) 1))
    (assertion-violation 'fxarithmetic-shift
                         "The given integer is not a valid shift count"
                         n k))
  (if (fxnegative? k)
      (fxarithmetic-shift-right n (fx- k))
      (fxarithmetic-shift-left n k)))

(define (fxarithmetic-shift-left x y)
  (sys:fxarithmetic-shift-left x y))

(define (fxarithmetic-shift-right x y)
  (sys:fxarithmetic-shift-right x y))

(define (fxrotate-bit-field n start end count)
  (let ((width (sys:fx- end start)))
    (sys:fxcopy-bit-field n start end
                          (sys:fxior
                           (sys:fxarithmetic-shift-left
                            (sys:fxbit-field n start (fx- end count)) count)
                           (sys:fxarithmetic-shift-right
                            (sys:fxbit-field n start end) (fx- width count))))))

(define (fxreverse-bit-field v start end)
  (define fxasl sys:fxarithmetic-shift-left)
  (define fxasr sys:fxarithmetic-shift-right)
  (define fxand sys:fxand)
  (define fxior sys:fxior)
  (define (fxreverse-bit-field61 v)
    ;; Based on <http://aggregate.org/MAGIC/#Bit Reversal>.
    (assert (fx=? (fixnum-width) 61))
    (let* ( ;; Swap pairs of bits
           (v (fxior (fxasr (fxand v #b101010101010101010101010101010101010101010101010101010101010) 1)
                     (fxasl (fxand v #b010101010101010101010101010101010101010101010101010101010101) 1)))
           ;; Swap 2-bit fields
           (v (fxior (fxasr (fxand v #b110011001100110011001100110011001100110011001100110011001100) 2)
                     (fxasl (fxand v #b001100110011001100110011001100110011001100110011001100110011) 2)))
           ;; Swap 4-bit fields
           (tmp1     (fxasr (fxand v #b111100000000000000000000000000000000000000000000000000000000) 56))
           (v (fxior (fxasr (fxand v #b000011110000111100001111000011110000111100001111000011110000) 4)
                     (fxasl (fxand v #b000000001111000011110000111100001111000011110000111100001111) 4)))
           ;; Swap bytes
           (tmp2     (fxasr (fxand v #b000011111111000000000000000000000000000000000000000000000000) 44))
           (v (fxior (fxasr (fxand v #b111100000000111111110000000011111111000000001111111100000000) 8)
                     (fxasl (fxand v #b000000000000000000001111111100000000111111110000000011111111) 8)))
           ;; Swap 16-bit fields
           (tmp3     (fxasr (fxand v #b000000000000111111111111111100000000000000000000000000000000) 20))
           (v (fxior (fxasr (fxand v #b111111111111000000000000000011111111111111110000000000000000) 16)
                     (fxasl (fxand v #b000000000000000000000000000000000000000000001111111111111111) 16)))
           ;; Swap 28-bit fields
           (v (fxior (fxasr (fxand v #b111111111111111111111111111100000000000000000000000000000000) 28)
                     (fxasl (fxand v #b000000000000000000000000000011111111111111111111111111111111) 28))))
      (fxior (fxior tmp1 tmp2) (fxior tmp3 v))))

  (define (fxreverse-bit-field30 v)
    (assert (fx=? (fixnum-width) 30))
    (let* (;; Swap pairs of bits
           (tmp1     (fxasr (fxand v #b10000000000000000000000000000) 28))
           (v (fxior (fxasr (fxand v #b01010101010101010101010101010) 1)
                     (fxasl (fxand v #b00101010101010101010101010101) 1)))
           ;; Swap 2-bit fields
           (v (fxior (fxasr (fxand v #b01100110011001100110011001100) 2)
                     (fxasl (fxand v #b00011001100110011001100110011) 2)))
           ;; Swap 4-bit fields
           (tmp2     (fxasr (fxand v #b01111000000000000000000000000) 23))
           (v (fxior (fxasr (fxand v #b10000111100001111000011110000) 4)
                     (fxasl (fxand v #b00000000011110000111100001111) 4)))
           ;; Swap bytes
           (tmp3     (fxasr (fxand v #b00000111111110000000000000000) 11))
           (v (fxior (fxasr (fxand v #b11111000000001111111100000000) 8)
                     (fxasl (fxand v #b00000000000000000000011111111) 8)))
           ;; Swap 13-bit fields
           (v (fxior (fxasr (fxand v #b11111111111110000000000000000) 13)
                     (fxasl (fxand v #b00000000000001111111111111111) 13))))
      (fxior (fxior tmp1 tmp2) (fxior tmp3 v))))

  (assert (fx<? end (fixnum-width)))
  (assert (fx<=? start end))
  (assert (fixnum? v))
  (cond ((fx=? (fixnum-width) 61)
         (fxior (fxasr
                 (fxreverse-bit-field61 (sys:fxbit-field v start end))
                 (fx- 60 end))
                (sys:fxcopy-bit-field v start end 0)))
        ((fx=? (fixnum-width) 30)
         (fxior (fxasr
                 (fxreverse-bit-field30 (sys:fxbit-field v start end))
                 (fx- 29 end))
                (sys:fxcopy-bit-field v start end 0)))
        (else
         (do ((i start (fx+ i 1))
              (ret 0 (if (fxbit-set? v i)
                         (fxior ret (fxasl 1 (fx- (fx- end i) 1)))
                         ret)))
             ((fx=? i end)
              (fxior (fxasl ret start)
                     (sys:fxcopy-bit-field v start end 0))))))))
