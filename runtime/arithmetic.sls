;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Arithmetic. Numbers. Computations. The things computers do.

;; Fixnum operations are in (loko runtime fixnums).

;; Complex numbers can be given a variety of types: Exact and inexact
;; forms; polar and rectangular forms.

;; The bnXYZ procedures work with bignums, but may return fixnums. Any
;; bignums that leave this library must be passed through bnsimplify!.

;; On div0 and mod0:
;; http://srfi.schemers.org/srfi-77/mail-archive/msg00505.html

#|

These predicates on heap-allocated numbers should be easily open coded:

  Standard:
    number?
    complex?
    real?
    exact? inexact?
  Low level:
    int?
    ratnum?
    (dflonum?)
    compnum?
    pcompnum?
    rcompnum?

All heap-allocated numbers are created with:

   ($box ($make-box-header type refs? value length) field* ...)

The type field is one of the btags from tools/typetags.py.

The value field encodes these predicates:

   bit 0: exact?
   bit 1: real?
   bit 2: polar?
   bit 3: compnum?
   bit 4: (unused)
   bit 5: (unused)
   bit 6: (reserved for negative-is-defined?)
   bit 7: (reserved for negative?)

The predicates rational? and integer? depend on the numeric value, so
encoding these as flags is not appropriate. All numbers satisfy
complex?, so no flag is needed.

|#

(library (loko runtime arithmetic)
  (export
    number? complex? real? rational? integer?
    real-valued? rational-valued? integer-valued?
    exact? inexact? inexact exact
    = < > <= >= zero? positive? negative? odd?
    even? finite? infinite? nan? max min
    + * - / abs div-and-mod div mod
    div0-and-mod0 div0 mod0
    gcd lcm numerator denominator
    floor ceiling truncate round
    rationalize exp log sin cos tan asin acos atan
    sqrt exact-integer-sqrt expt make-rectangular
    make-polar real-part imag-part magnitude angle

    bitwise-not bitwise-and bitwise-ior bitwise-xor
    bitwise-if bitwise-bit-count bitwise-length
    bitwise-first-bit-set bitwise-bit-set? bitwise-copy-bit
    bitwise-bit-field
    bitwise-copy-bit-field
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    bitwise-rotate-bit-field
    bitwise-reverse-bit-field

    (rename (inexact exact->inexact)
            (exact inexact->exact))
    quotient remainder modulo

    real->flonum

    number->string string->number

    $display-number                     ;for the printer
    bitwise-lsr
    ;; For (loko runtime equal)
    int? ratnum? compnum? pcompnum? rcompnum? eqv=?)
  (import
    (except (rnrs)
            number? complex? real? rational? integer?
            real-valued? rational-valued? integer-valued?
            exact? inexact? inexact exact
            = < > <= >= zero? positive? negative? odd?
            even? finite? infinite? nan? max min
            + * - / abs div-and-mod div mod
            div0-and-mod0 div0 mod0
            gcd lcm numerator denominator
            floor ceiling truncate round
            rationalize exp log sin cos tan asin acos atan
            sqrt exact-integer-sqrt expt make-rectangular
            make-polar real-part imag-part magnitude angle
            bitwise-not bitwise-and bitwise-ior bitwise-xor
            bitwise-if bitwise-bit-count bitwise-length
            bitwise-first-bit-set bitwise-bit-set? bitwise-copy-bit
            bitwise-bit-field
            bitwise-copy-bit-field
            bitwise-arithmetic-shift
            bitwise-arithmetic-shift-left
            bitwise-arithmetic-shift-right
            bitwise-rotate-bit-field
            bitwise-reverse-bit-field
            real->flonum
            number->string string->number)
    (prefix (rnrs) sys:)
    (rnrs mutable-strings)
    (only (rename (rnrs)
                  (fxarithmetic-shift-left fxasl)
                  (fxarithmetic-shift-right fxasr))
          fxasl fxasr)                  ;for convenience
    (loko system $primitives))

(define number:exact?   #b00000001)
(define number:real?    #b00000010)
(define number:polar?   #b00000100)
(define number:compnum? #b00001000)

(define (ratnum? obj)
  (and ($box? obj)
       ($box-header-type-eq? ($box-type obj) 'ratnum)))
(define (ratnum-n x) (assert (ratnum? x)) ($box-ref x 0))
(define (ratnum-d x) (assert (ratnum? x)) ($box-ref x 1))

(define (make-ratnum num den)
  (define (%make-ratnum n d)
    ($box ($make-box-header 'ratnum #t (fxior number:exact? number:real?) 2)
          n d))
  (unless (or (fixnum? num) (int? num))
    (error 'make-ratnum "Internal error: bad numerator" num den))
  (unless (or (fixnum? den) (int? den))
    (error 'make-ratnum "Internal error: bad denominator" num den))
  (assert (not (eqv? den 0)))
  (if (eqv? den 1)
      num
      (let ((d (gcd num den)))
        (let-values (((num* z0) (div-and-mod num d))
                     ((den* z1) (div-and-mod den d)))
          (unless (and (eqv? z0 0) (eqv? z1 0))
            (error 'make-ratnum "Internal error: bug in gcd"
                   num den d num* den* z0 z1))
          (cond ((eqv? den* 1)
                 num*)
                ((eqv? den* -1)
                 (- num*))
                (else
                 (%make-ratnum
                  (if (eq? (negative? num*) (negative? den*))
                      (abs num*)
                      (- (abs num*)))
                  (abs den*))))))))

(define (rcompnum? obj)                 ;rectangular complex number
  (and ($box? obj)
       ($box-header-type-eq? ($box-type obj) 'rcompnum)))
(define (rcompnum-r x) (assert (rcompnum? x)) ($box-ref x 0))
(define (rcompnum-i x) (assert (rcompnum? x)) ($box-ref x 1))

(define (make-rcompnum r i)
  (cond
    ((eqv? i 0)
     r)
    (else
     (assert (and (real? r) (real? i)))
     (cond
       ((or (flonum? r) (flonum? i))
        (let ((r (inexact r))
              (i (inexact i)))
          ($box ($make-box-header 'rcompnum #t (fxior number:compnum?) 2)
                r i)))
       (else
        ($box ($make-box-header 'rcompnum #t (fxior number:compnum? number:exact?) 2)
              r i))))))

(define (pcompnum? obj)               ;polar complex number
  (and ($box? obj)
       ($box-header-type-eq? ($box-type obj) 'pcompnum)))
(define (pcompnum-m x) (assert (pcompnum? x)) ($box-ref x 0))
(define (pcompnum-a x) (assert (pcompnum? x)) ($box-ref x 1))

(define (make-pcompnum m a)
  (define (%make-pcompnum m a)
    ($box ($make-box-header 'pcompnum #t
                            (if (flonum? m)
                                number:compnum?
                                (fxior number:compnum? number:exact?))
                            2)
          m a))
  (assert (and (real? m) (real? a)))
  (let lp ((m m) (a a))
    (cond ((eqv? a 0)
           m)
          ((not (and (>= a 0) (< a (* 2 pi))))
           ;; Limit the polar angle to [0,2π). If it's ever outside
           ;; this interval then it becomes inexact.
           (lp m (flmod (inexact a) (* 2 pi))))
          ((or (negative? m) (eqv? m -0.0))
           ;; Let's keep the magnitude non-negative.
           (lp (abs m) (+ a pi)))
          ((flonum? m)
           (%make-pcompnum m (inexact a)))
          ((flonum? a)
           (%make-pcompnum (inexact m) a))
          (else
           (%make-pcompnum m a)))))

(define (compnum? obj)
  (and ($box? obj)
       ($box-header-type-eq? ($box-type obj) 'number number:compnum? number:compnum?)))

(define ($display-number n p)
  (define (display-fixnum10 n p)
    (define powers-of-ten
      '#(()
         (10)
         (100 10)
         (1000 100 10)
         (10000 1000 100 10)
         (100000 10000 1000 100 10)
         (1000000 100000 10000 1000 100 10)
         (10000000 1000000 100000 10000 1000 100 10)
         (100000000 10000000 1000000 100000 10000 1000 100 10)
         (1000000000 100000000 10000000 1000000 100000 10000 1000 100 10)
         (10000000000 1000000000 100000000 10000000 1000000 100000 10000 1000 100 10)
         (100000000000 10000000000 1000000000 100000000 10000000 1000000 100000 10000 1000 100 10)
         (1000000000000 100000000000 10000000000 1000000000 100000000 10000000 1000000 100000
                        10000 1000 100 10)
         (10000000000000 1000000000000 100000000000 10000000000 1000000000 100000000 10000000
                         1000000 100000 10000 1000 100 10)
         (100000000000000 10000000000000 1000000000000 100000000000 10000000000 1000000000
                          100000000 10000000 1000000 100000 10000 1000 100 10)
         (1000000000000000 100000000000000 10000000000000 1000000000000 100000000000 10000000000
                           1000000000 100000000 10000000 1000000 100000 10000 1000 100 10)
         (10000000000000000 1000000000000000 100000000000000 10000000000000 1000000000000
                            100000000000 10000000000 1000000000 100000000 10000000 1000000
                            100000 10000 1000 100 10)
         (100000000000000000 10000000000000000 1000000000000000 100000000000000 10000000000000
                             1000000000000 100000000000 10000000000 1000000000 100000000 10000000
                             1000000 100000 10000 1000 100 10)
         (1000000000000000000 100000000000000000 10000000000000000 1000000000000000 100000000000000
                              10000000000000 1000000000000 100000000000 10000000000 1000000000
                              100000000 10000000 1000000 100000 10000 1000 100 10)))
    (cond ((eqv? n (least-fixnum))
           (assert (= (fixnum-width) 61))
           (display "-1152921504606846976" p))
          (else
           (let ((n (cond ((fxnegative? n)
                           (put-char p #\-)
                           (fx- n))
                          (else n))))
             (let lp ((n n)
                      (d (vector-ref powers-of-ten (fxilog10 n))))
               (if (null? d)
                   (put-char p (integer->char (fx+ n (char->integer #\0))))
                   (let-values ([(q r) ($fxquo+rem n (car d))])
                     (put-char p (integer->char (fx+ q (char->integer #\0))))
                     (lp r (cdr d)))))))))
  (define (display-scientific x* port)
    (let ((k (car x*)) (d* (cdr x*)))
      (put-char port (integer->char (fx+ (car d*) (char->integer #\0))))
      (put-char port #\.)
      (for-each (lambda (digit)
                  (put-char port (integer->char (fx+ digit (char->integer #\0)))))
                (cdr d*))
      (let ((k (fx- k 1)))
        (unless (eqv? k 0)
          (put-char port #\e)
          (display k port)))))
  (define (display-normal x* port)
    (let ((k (car x*)) (d* (cdr x*)))
      (cond ((fxpositive? k)
             (let lp ((i 0) (d* d*))
               (when (fx=? i k)
                 (put-char port #\.))
               (cond ((null? d*)
                      (unless (fx>=? i k)
                        (do ((i i (fx+ i 1)))
                            ((fx>=? i k))
                          (put-char port #\0))
                        (put-char port #\.)))
                     (else
                      (put-char port (integer->char (fx+ (car d*) (char->integer #\0))))
                      (lp (fx+ i 1) (cdr d*))))))
            (else
             (put-char port #\.)
             (do ((i k (fx+ i 1)))
                 ((fx=? i 0))
               (put-char port #\0))
             (let lp ((i (fx- k)) (d* d*))
               (unless (null? d*)
                 (put-char port (integer->char (fx+ (car d*) (char->integer #\0))))
                 (lp (fx+ i 1) (cdr d*))))))))
  (define (display-flonum n port)
    (define (expand n bits port)
      ;; Based on the definition from IEEE 754.
      (define bias 1023)
      (define p 53)
      (define w 11)
      (let ((S (sys:bitwise-bit-field bits 63 64))
            (E (sys:bitwise-bit-field bits 52 63))
            (T (sys:bitwise-bit-field bits 0 52)))
        (cond
          ((and (fx=? E (- (sys:expt 2 w) 1)))
           (put-char port (if (eqv? S 1) #\- #\+))
           (put-string port (if (not (eqv? T 0)) "nan.0" "inf.0")))
          ((and (eqv? E 0) (eqv? T 0))
           (when (eqv? S 1)
             (put-char port #\-))
           (put-string port "0.0"))
          (else
           (when (eqv? S 1)
             (put-char port #\-))
           (let ((e (if (eqv? E 0) E (- E bias 52)))
                 (f (if (eqv? E 0) T (fxior T (fxarithmetic-shift-left 1 (- p 1))))))
             (let ((x* (flonum->digits (flabs n) f e)))
               ;; TODO: Show mantissa width (|20 etc) when it's less
               ;; than the full precision of a flonum.
               (if (fx>? (car x*) 10)
                   (display-scientific x* port)
                   (display-normal x* port))))))))
    (let ((bits (let ((bv (make-bytevector 8)))
                  (bytevector-ieee-double-native-set! bv 0 n)
                  (bytevector-u64-native-ref bv 0))))
      (expand n bits port)))
  (cond ((fixnum? n)
         (display-fixnum10 n p))
        ((int? n)
         (display-int n p 10))
        ((ratnum? n)
         (display-int (numerator n) p 10)
         (put-char p #\/)
         (display-int (denominator n) p 10))
        ((flonum? n)
         (display-flonum n p))
        ((number? n)
         ;; Should be fine to allocate a string for every other type
         ;; of number, as they are more rare.
         (display (number->string n) p))
        (else
         (error 'display "Internal error: $display-number called on non-number" n p))))

(define number->string
  (case-lambda
    ((n)
     (number->string n 10))
    ((n base)
     (assert (memv base '(10 16 2 8)))
     (cond ((fixnum? n)
            (if (eqv? base 10)
                (fx->decimal n)
                (fx->binary-base n base)))
           ((int? n)
            (if (eqv? base 10)
                (call-with-string-output-port
                  (lambda (p)
                    (display-int n p base)))
                (bn->binary-base n base)))
           ((ratnum? n)
            (call-with-string-output-port
              (lambda (p)
                (display (number->string (numerator n) base) p)
                (put-char p #\/)
                (display (number->string (denominator n) base) p))))
           ((rcompnum? n)
            (call-with-string-output-port
              (lambda (p)
                (let ((r (rcompnum-r n))
                      (i (rcompnum-i n)))
                  (unless (or (eqv? base 10)
                              (and (exact? r) (exact? i)))
                    (assertion-violation 'number->string
                                         "Unsupported base for inexacts" n base))
                  (unless (eqv? r 0)
                    (if (eqv? base 10)
                        ($display-number r p)
                        (display (number->string r base) p)))
                  (unless (or (negative? i)
                              (and (flonum? i)
                                   (or (eqv? i -0.0)
                                       (flnan? i)
                                       (flinfinite? i))))
                    (put-char p #\+))
                  (cond ((eqv? i 1))
                        ((eqv? i -1)
                         (put-char p #\-))
                        ((eqv? base 10)
                         ($display-number i p))
                        (else
                         (let ((x (number->string i base)))
                           (if (char=? (string-ref x 0) #\#)
                               (display (substring x 2 (string-length x)) p)
                               (display x p)))))
                  (put-char p #\i)))))
           ((pcompnum? n)
            (unless (or (eqv? base 10) (exact? n))
              (assertion-violation 'number->string
                                   "Unsupported base for inexacts" n base))
            (call-with-string-output-port
              (lambda (p)
                (let ((m (pcompnum-m n)))
                  (if (eqv? base 10)
                      ($display-number m p)
                      (display (number->string m base) p)))
                (put-char p #\@)
                (let ((a (pcompnum-a n)))
                  (cond
                    ((eqv? base 10)
                     ($display-number a p))
                    (else
                     (let ((x (number->string a base)))
                       (if (char=? (string-ref x 0) #\#)
                           (display (substring x 2 (string-length x)) p)
                           (display x p)))))))))
           ((flonum? n)
            (unless (eqv? base 10)
              (assertion-violation 'number->string
                                   "Unsupported base for inexacts" n base))
            (call-with-string-output-port
              (lambda (p)
                ($display-number n p))))
           ((number? n)
            (error 'number->string "Internal error: number->string and number? disagree"))
           (else
            (assertion-violation 'number->string
                                 "Expected a number" n base))))
    ((n base precision)
     ;; TODO: inexacts
     (number->string n base))))

(define string->number
  (case-lambda
    ((s)
     (string->number s 10))
    ;; TODO: shortcut when the first character is a digit?
    ((s *radix*)
     (define-syntax const-nan
       (lambda (x)
         (syntax-case x ()
           ((_ sign)
            (let ((bv (make-bytevector 4)))
              (bytevector-u32-native-set! bv 0
                                          (sys:bitwise-ior
                                           (sys:bitwise-arithmetic-shift-left (syntax->datum #'sign) 31)
                                           (sys:bitwise-arithmetic-shift-left #xff 23)
                                           (sys:bitwise-arithmetic-shift-left #b1 22)))
              (bytevector-ieee-single-native-ref bv 0))))))
     (define c-nan.0 (const-nan 1))
     (define c+nan.0 (const-nan 0))
     (define *idx* 0)
     (define (get-char)
       (cond ((fx=? (string-length s) *idx*)
              (eof-object))
             (else
              (let ((c (string-ref s *idx*)))
                (set! *idx* (fx+ *idx* 1))
                c))))
     (define (unget-char c)
       (unless (eof-object? c)
         (set! *idx* (fx- *idx* 1))))

     (define (digit2? c) (char<=? #\0 c #\1))
     (define (digit8? c) (char<=? #\0 c #\7))
     (define (digit10? c) (char<=? #\0 c #\9))
     (define (digit16? c)
       (or (char<=? #\0 c #\9)
           (char<=? #\a c #\f)
           (char<=? #\A c #\F)))

     (define digit?
       (case *radix*
         ((10) digit10?)
         ((16) digit16?)
         ((2) digit2?)
         ((8) digit8?)
         (else
          (assertion-violation 'string->number
                               "Unsupported radix" s *radix*))))

     (define (hexdigit->integer c)
       (let ((i (fx- (fxand (char->integer c) (fxnot #x20)) #x10)))
         (if (fx<=? i 9) i (fx- i (fx- (char->integer #\1) #xA)))))

     (define (get-token*)
       (let ((c (get-char)))
         (cond ((eof-object? c)
                c)
               ((digit? c)
                (let lp ((n (hexdigit->integer c)))
                  (let ((c (get-char)))
                    (cond ((eof-object? c)
                           n)
                          ((digit? c)
                           (lp (+ (* *radix* n) (hexdigit->integer c))))
                          (else
                           (unget-char c)
                           n)))))
               (else
                (case c
                  ((#\#)
                   (case (get-char)
                     ((#\x #\X) 'radix16)
                     ((#\b #\B) 'radix2)
                     ((#\o #\O) 'radix8)
                     ((#\d #\D) 'radix10)
                     ((#\i #\I) 'inexact)
                     ((#\e #\E) 'exact)
                     (else #f)))
                  ((#\-) '-)
                  ((#\+) '+)
                  ((#\n #\N)
                   (and (memv (get-char) '(#\a #\A))
                        (memv (get-char) '(#\n #\N))
                        (eqv? (get-char) #\.)
                        (eqv? (get-char) #\0)
                        'nan))
                  ((#\i #\I)
                   (let ((c (get-char)))
                     (case c
                       ((#\n #\N)
                        (and (memv (get-char) '(#\f #\F))
                             (eqv? (get-char) #\.)
                             (eqv? (get-char) #\0)
                             'inf))
                       (else
                        (unget-char c)
                        'i))))
                  ((#\.)
                   (and (eqv? *radix* 10)
                        (let ((start *idx*))
                          (let lp ()
                            (let ((c (get-char)))
                              (cond ((eof-object? c)
                                     (cons start *idx*))
                                    ((digit? c)
                                     (lp))
                                    (else
                                     (unget-char c)
                                     (cons start *idx*))))))))
                  ((#\/) '/)
                  ((#\|) 'bar)
                  ((#\@) 'polar)
                  ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
                   (and (eqv? *radix* 10) 'exponent))
                  (else #f))))))

     (define *ungotten* '())
     (define (get-token)
       (if (null? *ungotten*)
           (get-token*)
           (let ((t (car *ungotten*)))
             (set! *ungotten* (cdr *ungotten*))
             t)))
     (define (unget-token t)
       (unless (eof-object? t)
         (set! *ungotten* (cons t *ungotten*))))

     (define (p-complex)
       ;; complex(R) --> real(R).
       ;; complex(R) --> real(R), "@", real(R).
       ;; complex(R) --> real(R), *complexI(R).
       ;; complex(R) --> complexI(R).
       (define start-idx *idx*)
       (define start-ungotten *ungotten*)
       (let ((r (p-real)))
         (cond (r
                (let ((t (get-token)))
                  (cond ((eof-object? t)
                         r)
                        ((eq? t 'polar)
                         (let ((r2 (p-real)))
                           (and r2 (make-polar r r2))))
                        (else
                         (unget-token t)
                         (let ((i (p-complexI)))
                           (cond (i
                                  (make-rectangular r i))
                                 (else
                                  ;; Manual backtrack...
                                  (set! *idx* start-idx)
                                  (set! *ungotten* start-ungotten)
                                  (let ((i (p-complexI)))
                                    (and i (make-rectangular 0 i))))))))))
               (else
                (let ((i (p-complexI)))
                  (and i (make-rectangular 0 i)))))))

     (define (p-complexI)
       ;; complexI(R) --> "+", ureal(R), "i".
       ;; complexI(R) --> "-", ureal(R), "i".
       ;; complexI(R) --> "+", naninf, "i".
       ;; complexI(R) --> "-", naninf, "i".
       ;; complexI(R) --> "+", "i".
       ;; complexI(R) --> "-", "i".
       (define (expect-i r)
         (let ((t (get-token)))
           (case t
             ((i) r)
             (else
              (unget-token t)
              #f))))
       (let ((sign (p-sign)))
         (case sign
           ((+ -)
            (let ((r (p-ureal)))
              (if r
                  (case sign
                    ((-)
                     (expect-i (- r)))
                    (else
                     (expect-i r)))
                  (let ((r (p-naninf)))
                    (case r
                      ((nan)
                       (case sign
                         ((+) (expect-i c+nan.0))
                         ((-) (expect-i c-nan.0))
                         (else #f)))
                      ((inf)
                       (case sign
                         ((+) (expect-i +inf.0))
                         ((-) (expect-i -inf.0))
                         (else #f)))
                      (else
                       (case sign
                         ((+) (expect-i +1))
                         ((-) (expect-i -1))
                         (else #f))))))))
           (else #f))))

     (define (p-real)
       ;; real(R) --> sign, ureal(R).
       ;; real(R) --> "+", naninf.
       ;; real(R) --> "-", naninf.
       (let ((sign (p-sign)))
         (let ((r (p-ureal)))
           (if r
               (case sign
                 ((+) r)
                 ((-) (- r))
                 (else r))
               (let ((t (p-naninf)))
                 (case t
                   ((nan)
                    (case sign
                      ((+) c+nan.0)
                      ((-) c-nan.0)
                      (else
                       (unget-token sign)
                       #f)))
                   ((inf)
                    (case sign
                      ((+) +inf.0)
                      ((-) -inf.0)
                      (else
                       (unget-token sign)
                       #f)))
                   (else
                    (unget-token sign)
                    #f)))))))

     (define (p-naninf)
       ;; naninf --> "nan.0".
       ;; naninf --> "inf.0".
       (let ((t (get-token)))
         (case t
           ((nan) t)
           ((inf) t)
           (else
            (unget-token t)
            #f))))

     (define (p-ureal)
       ;; ureal(R) --> decimal(R), mantissa_width.
       ;; ureal(R) --> uinteger(R), "/", uinteger(R).
       ;; ureal(R) --> uinteger(R).
       (let ((r1 (or (p-decimal) (p-uinteger))))
         (and r1
              (let ((m (p-mantissa-width)))
                (cond (m
                       ;; TODO: parsing the mantissa width too late
                       r1)
                      ((integer? r1)
                       (let ((t (get-token)))
                         (case t
                           ((/)
                            (let ((r2 (p-uinteger)))
                              (and r2 (not (zero? r2))
                                   (/ r1 r2))))
                           (else
                            (unget-token t)
                            r1))))
                      (else
                       r1))))))

     (define (p-decimal)
       ;; decimal(10) --> uinteger(10), ".", uinteger(10), suffix.
       ;; decimal(10) --> uinteger(10), ".", suffix.
       ;; decimal(10) --> uinteger(10), suffix.
       ;; decimal(10) --> ".", uinteger(10), suffix.
       (and (eqv? *radix* 10)
            (let ((r (p-uinteger)))
              (let ((t (get-token)))
                (cond ((pair? t)
                       (parse-float (or r 0) t 'TODO (p-suffix)))
                      (else
                       (unget-token t)
                       (let ((s (p-suffix)))
                         (if (and r s)
                             (* r (expt 10 s))
                             r))))))))

     (define (p-uinteger)
       ;; uinteger(R) --> digit(R)+.
       (let ((t (get-token)))
         (cond ((integer? t) t)
               (else
                (unget-token t)
                #f))))

     (define (p-mantissa-width)
       ;; mantissa_width --> "".
       ;; mantissa_width --> "|", digit(10)+.
       (and (eqv? *radix* 10)
            (let ((t (get-token)))
              (case t
                ((bar)
                 (cond ((p-uinteger))
                       (else
                        (unget-token t)
                        #f)))
                (else
                 (unget-token t)
                 #f)))))

     (define (p-sign)
       ;; sign --> "".
       ;; sign --> "+".
       ;; sign --> "-".
       (let ((t (get-token)))
         (case t
           ((+) '+)
           ((-) '-)
           (else
            (unget-token t)
            #f))))

     (define want-exactness 'unspecified)
     (define (p-exactness!)
       ;; exactness --> "".
       ;; exactness --> "#i".
       ;; exactness --> "#I".
       ;; exactness --> "#e".
       ;; exactness --> "#E".
       (let ((t (get-token)))
         (case t
           ((inexact)
            (set! want-exactness 'inexact)
            t)
           ((exact)
            (set! want-exactness 'exact)
            t)
           (else
            (unget-token t)
            #f))))

     (define (p-radix!)
       ;; radix(2) --> "#b".
       ;; radix(2) --> "#B".
       ;; radix(8) --> "#o".
       ;; radix(8) --> "#O".
       ;; radix(10) --> "".
       ;; radix(10) --> "#d".
       ;; radix(10) --> "#D".
       ;; radix(16) --> "#x".
       ;; radix(16) --> "#X".
       (let ((t (get-token)))
         (case t
           ((radix16)
            (set! *radix* 16)
            (set! digit? digit16?)
            t)
           ((radix2)
            (set! *radix* 2)
            (set! digit? digit2?)
            t)
           ((radix8)
            (set! *radix* 8)
            (set! digit? digit8?)
            t)
           ((radix10)
            (set! *radix* 10)
            (set! digit? digit10?)
            t)
           (else
            (unget-token t)
            #f))))

     (define (p-suffix)
       ;; suffix --> "".
       ;; suffix --> exponent_marker, sign, digits(10).
       (let ((t (get-token)))
         (case t
           ((exponent)
            (when (eq? want-exactness 'unspecified)
              (set! want-exactness 'inexact))
            ;; XXX: does not distinguish between different markers
            (let* ((s (p-sign))
                   (e (p-uinteger)))
              (cond (e
                     (case s
                       ((-) (- e))
                       (else e)))
                    (s
                     (unget-token s)
                     (unget-token t)
                     #f)
                    (else
                     (unget-token t)
                     #f))))
           (else
            (unget-token t)
            #f))))

     (define (parse-float a decimals mantissa-width exponent)
       (when (eq? want-exactness 'unspecified)
         (set! want-exactness 'inexact))
       (let ((start (car decimals))
             (end (cdr decimals)))
         ;; Parse decimals, exactly.
         (do ((i (fx- end 1) (fx- i 1))
              (n 0 (+ (/ n 10)
                      (/ (hexdigit->integer
                          (string-ref s i))
                         1))))
             ((fx<? i start)
              (let ((scaled
                     (let ((r (+ a (/ n 10))))
                       (if exponent
                           (* r (expt 10 exponent))
                           r))))
                ;; Unless this is done here then -0.0 and 0.0i will
                ;; not work.
                (if (eq? want-exactness 'inexact)
                    (inexact scaled)
                    scaled))))))

     ;; Take care of the prefix
     (cond ((p-radix!)
            (p-exactness!))
           ((p-exactness!)
            (p-radix!)))

     ;; Read a complex number.
     (let ((ret (p-complex)))
       (and (number? ret)
            (eof-object? (get-token))
            (case want-exactness
              ((inexact)
               (inexact ret))
              (else
               ret)))))))

(define (fxilog10 n)                ;almost!
  ;; Defined for positive fixnums. See Hacker's Delight p.219.
  ;; table1 is reversed for the benefit of fxlength. Tables are also
  ;; extended for greater word length. The first entry should
  ;; normally be zero, but is modified to aid radix conversion.
  (let ((y (bytevector-u8-ref #vu8(#;powers-of-ten-here-be
                                   1  0  0  0  1  1  1  2  2  2
                                   3  3  3  3  4  4  4  5  5  5
                                   6  6  6  6  7  7  7  8  8  8
                                   9  9  9  9  10 10 10 11 11 11
                                   12 12 12 12 13 13 13 14 14 14
                                   15 15 15 15 16 16 16 17 17 17
                                   18 18 18 18 19 19 19)
                              (fxlength n))))
    (if (fx<? n (vector-ref '#(1
                               10
                               100
                               1000
                               10000
                               100000
                               1000000
                               10000000
                               100000000
                               1000000000
                               10000000000
                               100000000000
                               1000000000000
                               10000000000000
                               100000000000000
                               1000000000000000
                               10000000000000000
                               100000000000000000
                               1000000000000000000)
                            y))
        (fx- y 1)
        y)))

;; Super-duper way to convert a fixnum to a decimal string.
(define (fx->decimal n)
  (define (positive n signlen)
    (let ((digits (fx+ (fxilog10 n) 1)))
      (let ((ret (make-string (fx+ signlen digits))))
        (string-set! ret 0 #\-)
        (let lp ((n n)
                 (i (fx+ signlen digits)))
          (let-values ([(n digit) (fxdiv-and-mod n 10)])
            (string-set! ret (fx- i 1)
                         (integer->char
                          (fx+ digit (char->integer #\0))))
            (unless (eqv? n 0)
              (lp n (fx- i 1)))))
        ret)))
  (if (fxnegative? n)
      (cond ((eqv? n (least-fixnum))
             (assert (= (fixnum-width) 61))
             (string-copy "-1152921504606846976")
             ;; Uses bignums.
             #;
             (let-values (((d m) (div-and-mod (- (least-fixnum)) 10)))
               (string-append (positive d 1) (positive m 0))))
            (else
             (positive (fx- n) 1)))
      (positive n 0)))

(define (fx->binary-base n base)
  (define (fx->binary-base* n signlen mask shift digits)
    (let ((ret (make-string (fx+ signlen digits))))
      (string-set! ret 0 #\-)
      (let lp ((n n)
               (i (fx- (fx+ signlen digits) 1)))
        (let ((n (fxarithmetic-shift-right n shift))
              (digit (fxand n mask)))
          (string-set! ret i (string-ref "0123456789ABCDEF" digit))
          (if (eqv? n 0)
              ret
              (lp n (fx- i 1)))))))
  (cond
    ((eqv? n 0)
     (make-string 1 #\0))
    ((eqv? n (least-fixnum))
     (let* ((len (case base
                   ((16) (fx+ 2 (fxdiv (fx+ (fxlength (least-fixnum)) 3) 4)))
                   ((8)  (fx+ 2 (fxdiv (fx+ (fxlength (least-fixnum)) 2) 3)))
                   (else (fx+ 2 (fxdiv (fx+ (fxlength (least-fixnum)) 0) 1)))))
            (ret (make-string len #\0)))
       (string-set! ret 0 #\-)
       (string-set! ret 1 #\1)
       ret))
    (else
     (let ((n (if (fxnegative? n) (fx- n) n))
           (signlen (fx- (fxarithmetic-shift-right n (fx- (fixnum-width) 1)))))
       (case base
         ((16) (fx->binary-base* n signlen #xf 4 (fxdiv (fx+ (fxlength n) 3) 4)))
         ((8)  (fx->binary-base* n signlen #o7 3 (fxdiv (fx+ (fxlength n) 2) 3)))
         (else (fx->binary-base* n signlen #b1 1 (fxlength n))))))))

(define (number? obj)
  ;; Collect all number types here. Anything that this returns true
  ;; for must be handled by $display-number, too.
  (or (fixnum? obj)
      (flonum? obj)
      (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'number))))

(define (complex? obj)
  (number? obj))

(define (real? obj)
  (or (fixnum? obj)
      (flonum? obj)
      (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'number number:real? number:real?))))

(define (rational? obj)
  (or (fixnum? obj)
      (and (flonum? obj) (flfinite? obj))
      (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'number number:real? number:real?))))

(define (integer? obj)
  (or (fixnum? obj)
      (and (flonum? obj) (flinteger? obj))
      (int? obj)))

(define (real-valued? obj)
  ;; #t if obj is = to some real
  (or (fixnum? obj)
      (flonum? obj)
      (and ($box? obj)
           (or ($box-header-type-eq? ($box-type obj) 'number number:real? number:real?)
               (and (compnum? obj)
                    (zero? (imag-part obj)))))))

(define (rational-valued? obj)
  ;; #t if obj is = to some rational
  (or (fixnum? obj)
      (and (flonum? obj) (flfinite? obj))
      (and ($box? obj)
           (or ($box-header-type-eq? ($box-type obj) 'number number:real? number:real?)
               (and (compnum? obj)
                    (and (zero? (imag-part obj))
                         (rational-valued? (real-part obj))))))))

(define (integer-valued? obj)
  ;; #t if obj is = to some integer
  (or (fixnum? obj)
      (and (flonum? obj) (flinteger? obj))
      (int? obj)
      (and (compnum? obj)
           (and (zero? (imag-part obj))
                (integer-valued? (real-part obj))))))

(define (exact? x)
  (cond ((fixnum? x) #t)
        ((flonum? x) #f)
        ((and ($box? x) ($box-header-type-eq? ($box-type x) 'number))
         ($box-header-type-eq? ($box-type x) 'number number:exact? number:exact?))
        (else
         (assertion-violation 'exact? "Expected a number" x))))

(define (inexact? x)
  (cond ((fixnum? x) #f)
        ((flonum? x) #t)
        ((and ($box? x) ($box-header-type-eq? ($box-type x) 'number))
         ($box-header-type-eq? ($box-type x) 'number number:exact? 0))
        (else
         (assertion-violation 'inexact? "Expected a number" x))))

(define (inexact x)
  (cond ((fixnum? x)
         (fixnum->flonum x))
        ((flonum? x)
         x)
        ((and ($box? x)
              ($box-header-type-eq? ($box-type x) 'number number:exact? 0))
         ;; Already inexact
         x)
        ((rcompnum? x)
         (make-rcompnum (inexact (rcompnum-r x))
                        (inexact (rcompnum-i x))))
        ((pcompnum? x)
         (make-rcompnum (inexact (real-part x))
                        (inexact (imag-part x))))
        ((ratnum? x)
         (fl/ (inexact (ratnum-n x))
              (inexact (ratnum-d x))))
        ((int? x)
         (int-inexact x))
        (else
         (assertion-violation 'inexact "Expected a number" x))))

(define (real->flonum x)
  (cond ((fixnum? x) (fixnum->flonum x))
        ((flonum? x) x)
        ((int? x) (inexact x))
        ((ratnum? x) (inexact x))
        (else
         (assertion-violation 'real->flonum "Expected a real number" x))))

(define (exact x)
  (cond ((fixnum? x) x)
        ((and ($box? x)
              ($box-header-type-eq? ($box-type x) 'number number:exact? number:exact?))
         ;; Already exact
         x)
        ((flonum? x)
         (letrec ((ieee-single-exact
                   (lambda (bits)
                     ;; IEEE 754 has the definition.
                     (define bias 127)
                     (define p 24)
                     (define w 8)
                     (let ((S (fxbit-field bits 31 32))
                           (E (fxbit-field bits 23 31))
                           (T (fxbit-field bits 0 23)))
                       (cond
                         ((and (fx=? E (- (expt 2 w) 1)))
                          (assertion-violation 'exact
                                               "No exact representation" x))
                         ((fx<=? 1 E (- (expt 2 w) 2))
                          (* (expt -1 S) (expt 2 (- E bias))
                             (+ 1 (* (expt 2 (- 1 p))
                                     T))))
                         ((and (eqv? E 0) (not (eqv? T 0)))
                          (* (expt -1 S) (expt 2 (- E bias))
                             (* (expt 2 (- 1 p))
                                T)))
                         ((and (eqv? E 0) (eqv? T 0))
                          0)
                         (else
                          (error 'exact "Oops" bits)))))))
           (let ((bits (let ((bv (make-bytevector 4)))
                         (bytevector-ieee-single-native-set! bv 0 x)
                         (bytevector-u32-native-ref bv 0))))
             (ieee-single-exact bits))))
        ((rcompnum? x)
         (make-rcompnum (exact (rcompnum-r x))
                        (exact (rcompnum-i x))))
        ((pcompnum? x)
         (make-rectangular (exact (real-part x))
                           (exact (imag-part x))))
        (else
         (assertion-violation 'exact "Expected a number" x))))

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

;; This is the fallthrough in eqv? for when it decides the numbers are
;; similar enough in type that it should call =. But first this needs
;; to distinguish between 0.0 and -0.0, which = does not do.
(define (eqv=? a b)
  (cond ((exact? a)
         (= a b))
        ((rcompnum? a)
         (and (eqv? (rcompnum-r a) (rcompnum-r b))
              (eqv? (rcompnum-i a) (rcompnum-i b))))
        ((pcompnum? a)
         (and (eqv? (pcompnum-m a) (pcompnum-m b))
              (eqv? (pcompnum-a a) (pcompnum-a b))))
        ;;((dflonum? a) )
        (else
         (= a b))))

(define-comparator (= a b)
  (define (bad-arg x)
    (assertion-violation '= "Expected a number" x))
  ;; XXX: what about pcompnum and rcompnum with inexact zero imag or
  ;; angle? TODO: inexact complex numbers (with imaginary part zero)
  ;; changes a few assumptions here.
  (cond ((fixnum? a)
         (cond ((fixnum? b) (eq? a b))
               ((flonum? b)
                (and (flfinite? b)
                     (fl=? (fixnum->flonum a) b)))
               ((int? b) #f)
               ((ratnum? b) #f)
               ((compnum? b) (= b a))
               (else (bad-arg b))))
        ((flonum? a)
         (cond ((flonum? b) (fl=? a b))
               ((or (fixnum? b) (int? b) (ratnum? b))
                (and (flfinite? a)
                     (fl=? a (inexact b))))
               ((compnum? b) (= b a))
               (else (bad-arg b))))
        ((int? a)
         (cond ((fixnum? b) #f)
               ((flonum? b)
                (and (flfinite? b)
                     (= (inexact a) b)))
               ((int? b) (cmp= a b))
               ((ratnum? b) #f)
               ((compnum? b) (= b a))
               (else (bad-arg b))))
        ((ratnum? a)
         (cond ((or (fixnum? b) (int? b)) #f)
               ((flonum? b) (= (inexact a) b))
               ((ratnum? b)
                (and (= (numerator a) (numerator b))
                     (= (denominator a) (denominator b))))
               ((compnum? b) (= b a))
               (else (bad-arg b))))
        ((rcompnum? a)
         (cond ((or (fixnum? b) (int? b)) #f)
               ((or (ratnum? b) (flonum? b))
                (and (zero? (imag-part a))
                     (= (real-part a) b)))
               ((rcompnum? b)
                (and (= (rcompnum-r a) (rcompnum-r b))
                     (= (rcompnum-i a) (rcompnum-i b))))
               ((pcompnum? b)
                (and (= (real-part a) (real-part b))
                     (= (imag-part a) (imag-part b))))
               (else (bad-arg b))))
        ((pcompnum? a)
         (cond ((or (fixnum? b) (int? b)) #f)
               ((or (ratnum? b) (flonum? b))
                (and (zero? (imag-part a))
                     (= (real-part a) b)))
               ((rcompnum? b)
                (= b a))
               ((pcompnum? b)
                (and (= (pcompnum-m a) (pcompnum-m b))
                     (= (pcompnum-a a) (pcompnum-a b))))
               (else (bad-arg b))))
        (else (bad-arg a))))

(define (fxsign x) (if (fxnegative? x) -1 1))

(define-comparator (< a b)
  (define (bad-arg x)
    (assertion-violation '< "Expected a real number" x))
  (cond ((fixnum? a)
         (cond ((fixnum? b) (fx<? a b))
               ((flonum? b) (fl<? (fixnum->flonum a) b))
               ((int? b)
                ;; The magnitude of a fixnum is always less than
                ;; that of an int from outside this library.
                (if (eq? (fxsign a) (int-sign b))
                    (not (fxnegative? a))
                    (fxnegative? a)))
               ((ratnum? b) (< (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((flonum? a)
         (cond ((flonum? b) (fl<? a b))
               ((fixnum? b) (fl<? a (fixnum->flonum b)))
               ((or (int? b) (ratnum? b)) (fl<? a (inexact b)))
               (else (bad-arg b))))
        ((int? a)
         (cond ((fixnum? b) (int-negative? a))
               ((flonum? b) (< (inexact a) b))
               ((int? b) (cmp< a b))
               ((ratnum? b) (< (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((ratnum? a)
         (cond ((or (fixnum? b) (int? b))
                (< (numerator a) (* b (denominator a))))
               ((flonum? b) (< (inexact a) b))
               ((ratnum? b)
                (< (* (numerator a) (denominator b))
                   (* (numerator b) (denominator a))))
               (else (bad-arg b))))
        (else (bad-arg a))))

(define-comparator (> a b)
  (define (bad-arg x)
    (assertion-violation '> "Expected a real number" x))
  (cond ((fixnum? a)
         (cond ((fixnum? b) (fx>? a b))
               ((flonum? b) (fl>? (fixnum->flonum a) b))
               ((int? b)
                (if (eq? (fxsign a) (int-sign b))
                    (fxnegative? a)
                    (not (fxnegative? a))))
               ((ratnum? b) (> (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((flonum? a)
         (cond ((flonum? b) (fl>? a b))
               ((fixnum? b) (fl>? a (fixnum->flonum b)))
               ((or (int? b) (ratnum? b)) (> a (inexact b)))
               (else (bad-arg b))))
        ((int? a)
         (cond ((fixnum? b) (int-positive? a))
               ((flonum? b) (fl>? (inexact a) b))
               ((int? b) (cmp> a b))
               ((ratnum? b) (> (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((ratnum? a)
         (cond ((or (fixnum? b) (int? b))
                (> (numerator a) (* b (denominator a))))
               ((flonum? b) (> (inexact a) b))
               ((ratnum? b)
                (> (* (numerator a) (denominator b))
                   (* (numerator b) (denominator a))))
               (else (bad-arg b))))
        (else (bad-arg a))))

(define-comparator (<= a b)
  (define (bad-arg x)
    (assertion-violation '<= "Expected a real number" x))
  (cond ((fixnum? a)
         (cond ((fixnum? b) (fx<=? a b))
               ((flonum? b) (fl<=? (fixnum->flonum a) b))
               ((int? b)
                (if (eq? (fxsign a) (int-sign b))
                    (not (fxnegative? a))
                    (fxnegative? a)))
               ((ratnum? b) (<= (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((flonum? a)
         (cond ((flonum? b) (fl<=? a b))
               ((fixnum? b) (fl<=? a (fixnum->flonum b)))
               ((or (int? b) (ratnum? b)) (fl<=? a (inexact b)))
               (else (bad-arg b))))
        ((int? a)
         (cond ((fixnum? b) (int-negative? a))
               ((flonum? b) (<= (inexact a) b))
               ((int? b) (cmp<= a b))
               ((ratnum? b) (<= (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((ratnum? a)
         (cond ((or (fixnum? b) (int? b))
                (<= (numerator a) (* b (denominator a))))
               ((flonum? b) (fl<=? (inexact a) b))
               ((ratnum? b)
                (<= (* (numerator a) (denominator b))
                    (* (numerator b) (denominator a))))
               (else (bad-arg b))))
        (else (bad-arg a))))

(define-comparator (>= a b)
  (define (bad-arg x)
    (assertion-violation '>= "Expected a real number" x))
  (cond ((fixnum? a)
         (cond ((fixnum? b) (fx>=? a b))
               ((flonum? b) (fl>=? (fixnum->flonum a) b))
               ((int? b)
                (if (eq? (fxsign a) (int-sign b))
                    (fxnegative? a)
                    (not (fxnegative? a))))
               ((ratnum? b) (>= (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((flonum? a)
         (cond ((flonum? b) (fl>=? a b))
               ((or (fixnum? b) (int? b) (ratnum? b)) (fl>=? a (inexact b)))
               (else (bad-arg b))))
        ((int? a)
         (cond ((fixnum? b) (int-positive? a))
               ((flonum? b) (>= (inexact a) b))
               ((int? b) (cmp>= a b))
               ((ratnum? b) (>= (* a (denominator b)) (numerator b)))
               (else (bad-arg b))))
        ((ratnum? a)
         (cond ((or (fixnum? b) (int? b))
                (>= (numerator a) (* b (denominator a))))
               ((flonum? b) (fl>=? (inexact a) b))
               ((ratnum? b)
                (>= (* (numerator a) (denominator b))
                    (* (numerator b) (denominator a))))
               (else (bad-arg b))))
        (else (bad-arg a))))

(define (zero? x)
  (cond ((fixnum? x) (fxzero? x))
        ((flonum? x) (fl=? x 0.0))
        ((ratnum? x) #f)
        ((rcompnum? x)
         (let ((r (rcompnum-r x))
               (i (rcompnum-i x)))
           (and (or (eqv? r 0) (and (flonum? r) (fl=? r 0.0)))
                (or (eqv? i 0) (and (flonum? i) (fl=? i 0.0))))))
        ((pcompnum? x)
         (let ((m (pcompnum-m x)))
           (or (eqv? m 0) (and (flonum? m) (fl=? m 0.0)))))
        ((int? x) #f)
        (else
         (assertion-violation 'zero? "Expected a number" x))))

(define (positive? x)
  (cond ((fixnum? x) (fxpositive? x))
        ((flonum? x) (flpositive? x))
        ((int? x) (int-positive? x))
        ((ratnum? x) (positive? (numerator x)))
        (else
         (assertion-violation 'positive? "Expected a real number" x))))

(define (negative? x)
  (cond ((fixnum? x) (fxnegative? x))
        ((flonum? x) (flnegative? x))
        ((int? x) (int-negative? x))
        ((ratnum? x) (negative? (numerator x)))
        (else
         (assertion-violation 'negative? "Expected a real number" x))))

(define (odd? n)
  (cond ((fixnum? n) (fx=? #b1 (fxand n #b1)))
        ((flonum? n) (flodd? n))
        ((int? n) (bitwise-bit-set? n 0))
        (else
         (assertion-violation 'odd? "Expected integers" n))))

(define (even? n)
  (cond ((fixnum? n) (fxzero? (fxand n #b1)))
        ((flonum? n) (fleven? n))
        ((int? n) (not (bitwise-bit-set? n 0)))
        (else
         (assertion-violation 'even? "Expected integers" n))))

(define (finite? x)
  ;; Neither infinite nor nan.
  (cond ((flonum? x) (flfinite? x))
        (else
         (assert (real? x))
         #t)))

(define (infinite? x)
  (cond ((flonum? x)
         (flinfinite? x))
        (else
         (assert (real? x))
         #f)))

(define (nan? x)
  (cond ((flonum? x)
         (flnan? x))
        (else
         (assert (real? x))
         #f)))

(define max
  (case-lambda
    ((a b)
     (define (err x)
       (assertion-violation 'max "Expected a real number" x))
     (cond
       ((flonum? a)
        (if (real? b)
            (let ((b (inexact b)))
              (flmax a b))
            (err b)))
       ((flonum? b)
        (if (real? a)
            (let ((a (inexact a)))
              (flmax a b))
            (err a)))
       (else
        (if (> a b) a b))))
    ((a b c)
     (max (max a b) c))
    ((a b c . x)
     (fold-left sys:max (max a b c) x))
    ((a)
     (unless (real? a)
       (assertion-violation 'max "Expected a real number" a))
     a)))

(define min
  (case-lambda
    ((a b)
     (define (err x)
       (assertion-violation 'max "Expected a real number" x))
     (cond
       ((flonum? a)
        (if (real? b)
            (let ((b (inexact b)))
              (flmin a b))
            (err b)))
       ((flonum? b)
        (if (real? a)
            (let ((a (inexact a)))
              (flmin a b))
            (err a)))
       (else
        (if (> b a) a b))))
    ((a b c)
     (min (min a b) c))
    ((a b c . x)
     (fold-left sys:min (min a b c) x))
    ((a)
     (unless (real? a)
       (assertion-violation 'min "Expected a real number" a))
     a)))

(define +
  (case-lambda
    ((a b)
     (define (err a b)
       (assertion-violation '+ "Expected numbers" a b))
     (cond
       ((fixnum? a)
        (cond ((fixnum? b)
               (or ($fx+/false a b)
                   (+ (fixnum->int a) (fixnum->int b))))
              ((flonum? b)
               (fl+ (fixnum->flonum a) b))
              ((int? b)
               (+ (fixnum->int a) b))
              ((ratnum? b)
               (let ((d (denominator b)))
                 (make-ratnum (+ (* a d) (numerator b))
                              d)))
              ((rcompnum? b)
               (make-rcompnum (+ a (real-part b)) (imag-part b)))
              ((pcompnum? b) (+ b a))
              (else (err a b))))
       ((int? a)
        (cond ((fixnum? b) (bn+ a (fixnum->int b)))
              ((int? b) (bn+ a b))
              ((ratnum? b)
               (let ((d (denominator b)))
                 (make-ratnum (+ (* a d) (numerator b))
                              d)))
              ((rcompnum? b) (+ b a))
              ((pcompnum? b) (+ b a))
              ((flonum? b) (fl+ (inexact a) b))
              (else (err a b))))
       ((flonum? a)
        (cond ((flonum? b) (fl+ a b))
              ((fixnum? b) (fl+ a (fixnum->flonum b)))
              ((or (int? b) (ratnum? b))
               (fl+ a (inexact b)))
              ((or (rcompnum? b) (pcompnum? b))
               (+ b a))
              (else (err a b))))
       ((ratnum? a)
        (cond ((rcompnum? b) (+ b a))
              ((pcompnum? b) (+ b a))
              ((flonum? b) (fl+ (inexact a) b))
              (else
               (let ((x1 (numerator a)) (y1 (denominator a))
                     (x2 (numerator b)) (y2 (denominator b)))
                 (make-ratnum (+ (* x1 y2) (* x2 y1))
                              (* y1 y2))))))
       ((or (rcompnum? a) (pcompnum? a))
        (make-rcompnum (+ (real-part a) (real-part b))
                       (+ (imag-part a) (imag-part b))))
       (else
        (err a b))))
    ((a b c)
     (+ (+ a b) c))
    ((a b c d)
     (+ (+ a b) (+ c d)))
    (() 0)
    ((a) (assert (number? a)) a)
    ((a b c d . x)
     (fold-left sys:+ (+ a b c d) x))))

(define *
  (case-lambda
    ((a b)
     (define (err a b)
       (assertion-violation '* "Expected numbers" a b))
     (cond
       ((fixnum? a)
        (cond ((fixnum? b)
               (or ($fx*/false a b)
                   (* (fixnum->int a) (fixnum->int b))))
              ((flonum? b)
               (fl* (fixnum->flonum a) b))
              ((int? b)
               (if (fx<=? 0 a (digit-mask))
                   (bnfx* b a)
                   (bn* (fixnum->int a) b)))
              ((ratnum? b)
               (make-ratnum (* a (numerator b)) (denominator b)))
              ((rcompnum? b)
               (let ((x1 a) (y1 0) (x2 (real-part b)) (y2 (imag-part b)))
                 (make-rcompnum (* x1 x2) (* x1 y2))))
              ((pcompnum? b)
               (* b a))
              (else (err a b))))
       ((flonum? a)
        (cond ((flonum? b) (fl* a b))
              ((fixnum? b) (* a (fixnum->flonum b)))
              ((or (int? b) (ratnum? b) (rcompnum? b) (pcompnum? b))
               (* (inexact b) a))
              (else (err a b))))
       ((int? a)
        (cond ((fixnum? b)
               (if (fx<=? 0 b (digit-mask))
                   (bnfx* a b)
                   (* a (fixnum->int b))))
              ((int? b)
               (if (eq? a b)
                   (bn² a)
                   (bn* a b)))
              ((flonum? b)
               (* (inexact a) b))
              ((ratnum? b)
               (make-ratnum (* a (numerator b)) (denominator b)))
              ((rcompnum? b) (* b a))
              ((pcompnum? b) (* b a))
              (else (err a b))))
       ((ratnum? a)
        (cond ((rcompnum? b) (* b a))
              ((pcompnum? b) (* b a))
              ((flonum? b) (fl* (inexact a) b))
              (else
               (make-ratnum (* (numerator a) (numerator b))
                            (* (denominator a) (denominator b))))))
       ((rcompnum? a)
        ;; TODO: flonum
        (let ((x1 (real-part a)) (y1 (imag-part a))
              (x2 (real-part b)) (y2 (imag-part b)))
          (make-rcompnum (- (* x1 x2) (* y1 y2))
                         (+ (* x1 y2) (* x2 y1)))))
       ((pcompnum? a)
        ;; TODO: flonum
        (let ((r1 (magnitude a)) (t1 (angle a))
              (r2 (magnitude b)) (t2 (angle b)))
          (make-polar (* r1 r2) (+ t1 t2))))
       (else (err a b))))
    (() 1)
    ((a) (assert (number? a)) a)
    ((a b c)
     (* (* a b) c))
    ((a b c . x)
     (fold-left sys:* (* a b c) x))))

(define -
  (case-lambda
    ((a b)
     (define (err a b)
       (assertion-violation '- "Expected numbers" a b))
     (cond
       ((fixnum? a)
        (cond ((fixnum? b)
               (or ($fx-/false a b)
                   (bn- (fixnum->int a) (fixnum->int b))))
              ((flonum? b)
               (fl- (fixnum->flonum a) b))
              ((int? b)
               (bn- (fixnum->int a) b))
              ((ratnum? b)
               (let ((d (denominator b)))
                 (make-ratnum (- (* a d) (numerator b))
                              d)))
              ((rcompnum? b)
               (make-rcompnum (- a (real-part b)) (- (imag-part b))))
              (else (err a b))))
       ((int? a)
        (cond ((fixnum? b) (bn- a (fixnum->int b)))
              ((int? b) (bn- a b))
              ((flonum? b) (fl- (inexact a) b))
              ((ratnum? b)
               (let ((d (denominator b)))
                 (make-ratnum (- (* a d) (numerator b))
                              d)))
              ((rcompnum? b)
               (make-rcompnum (- a (real-part b)) (- (imag-part b))))
              (else (err a b))))
       ((flonum? a)
        (cond ((flonum? b)
               (fl- a b))
              ((fixnum? b)
               (fl- a (fixnum->flonum b)))
              ((or (int? b) (ratnum? b))
               (fl- a (inexact b)))
              ((or (rcompnum? b) (pcompnum? b))
               (- (make-rectangular a 0.0) b))
              (else (err a b))))
       ((ratnum? a)
        (cond ((rcompnum? b) (sys:+ a (- b)))
              ((pcompnum? b) (sys:+ a (- b)))
              ((flonum? b) (fl- (inexact a) b))
              (else
               (make-ratnum (- (* (numerator a) (denominator b))
                               (* (numerator b) (denominator a)))
                            (* (denominator a) (denominator b))))))
       ((or (rcompnum? a) (pcompnum? a))
        (make-rcompnum (- (real-part a) (real-part b))
                       (- (imag-part a) (imag-part b))))
       (else (err a b))))
    ((a)
     (cond
       ((fixnum? a)
        (cond ((eqv? a (least-fixnum))
               (sys:+ (greatest-fixnum) 1))
              (else
               (fx- a))))
       ((int? a)
        (bnneg a))
       ((flonum? a)
        (fl- a))
       ((ratnum? a)
        (make-ratnum (- (numerator a)) (denominator a)))
       ((rcompnum? a)
        (make-rcompnum (- (rcompnum-r a)) (- (rcompnum-i a))))
       ((pcompnum? a)
        (* a -1))
       (else
        (assertion-violation '- "Expected a number" a))))
    ((a b c)
     (- (- a b) c))
    ((a b c d)
     (- (- a b c) d))
    ((a b c d . x)
     (fold-left sys:- (- a b c d) x))))

(define /
  (case-lambda
    ((a b)
     (define (err a b)
       (assertion-violation '/ "Expected numbers" a b))
     (cond
       ((fixnum? a)
        (cond ((fixnum? b)
               (if (eqv? b -1)
                   (- a)              ;handles (/ (least-fixnum) -1)
                   (let-values (((d m) (fxdiv-and-mod a b)))
                     (if (fxzero? m) d (make-ratnum a b)))))
              ((flonum? b)
               (fl/ (fixnum->flonum a) b))
              ((int? b)
               (make-ratnum a b))
              ((ratnum? b)
               (make-ratnum (* a (denominator b)) (numerator b)))
              ((rcompnum? b)
               (let ((x1 a) (y1 0) (x2 (real-part b)) (y2 (imag-part b)))
                 (let ((d (+ (* y2 y2) (* x2 x2))))
                   (make-rcompnum (/ (* x1 x2) d)
                                  (/ (- (* x1 y2)) d)))))
              ((pcompnum? b)
               (let ((r1 a) (t1 0) (r2 (magnitude b)) (t2 (angle b)))
                 (make-polar (/ r1 r2) (- t2))))
              (else
               (err a b))))
       ((flonum? a)
        (cond ((flonum? b)
               (fl/ a b))
              ((fixnum? b)
               (fl/ a (fixnum->flonum b)))
              ((or (int? b) (ratnum? b))
               (fl/ a (inexact b)))
              ((rcompnum? b) (/ (make-rcompnum a 0.0) b))
              ((pcompnum? b) (/ (make-pcompnum a 0.0) b))
              (else (err a b))))
       ((int? a)
        (cond ((fixnum? b) (make-ratnum a b))
              ((flonum? b) (fl/ (inexact a) b))
              ((int? b) (make-ratnum a b))
              ((ratnum? b)
               (make-ratnum (* a (denominator b)) (numerator b)))
              ((rcompnum? b)
               (let ((x1 a) (y1 0) (x2 (real-part b)) (y2 (imag-part b)))
                 (let ((d (+ (* y2 y2) (* x2 x2))))
                   (make-rcompnum (/ (* x1 x2) d)
                                  (/ (- (* x1 y2)) d)))))
              (else
               (err a b))))
       ((ratnum? a)
        (cond ((rcompnum? b) (/ (/ b a)))
              ((pcompnum? b) (/ (/ b a)))
              ((flonum? b) (fl/ (inexact a) b))
              (else
               (make-ratnum (* (numerator a) (denominator b))
                            (* (numerator b) (denominator a))))))
       ((rcompnum? a)
        (let ((x1 (real-part a)) (y1 (imag-part a))
              (x2 (real-part b)) (y2 (imag-part b)))
          (let ((d (+ (* y2 y2) (* x2 x2))))
            (make-rcompnum (/ (+ (* y1 y2) (* x1 x2)) d)
                           (/ (- (* x2 y1) (* x1 y2)) d)))))
       ((pcompnum? a)
        (let ((r1 (magnitude a)) (t1 (angle a))
              (r2 (magnitude b)) (t2 (angle b)))
          (make-polar (/ r1 r2) (- t1 t2))))
       (else
        (err a b))))
    ((a)
     (/ 1 a))
    ((a b c)
     (/ (/ a b) c))
    ((a b c . x)
     (fold-left sys:/ (/ (/ a b) c) x))))

(define (abs x)
  (cond ((fixnum? x)
         (cond ((fx>=? x 0)
                x)
               ((eqv? x (least-fixnum))
                (sys:+ (greatest-fixnum) 1))
               (else
                (fx- x))))
        ((int? x)
         (if (int-negative? x) (bnneg x) x))
        ((flonum? x)
         (flabs x))
        ((ratnum? x)
         (if (negative? (ratnum-n x)) (- x) x))
        (else
         (assertion-violation 'abs "Expected a real number" x))))

(define (%ratnum-div-and-mod a b)
  ;; Expects exact numbers
  (cond ((negative? b)
         (let ((d (ceiling (/ a b))))
           (let ((m (- a (* b d))))
             (values d m))))
        (else
         (let ((d (floor (/ a b))))
           (let ((m (- a (* b d))))
             (values d m))))))

(define (div-and-mod a b)
  (define (err a b)
    ;; FIXME: can leak non-simplified ints and ratnums
    (assertion-violation 'div-and-mod
                         "Expected real numbers" a b))
  (cond ((fixnum? a)
         (cond ((fixnum? b)
                (if (and (eqv? a (least-fixnum)) (eqv? b -1))
                    (values (+ (greatest-fixnum) 1) 0)
                    (fxdiv-and-mod a b)))
               ((int? b)
                (if (fxnegative? a)
                    (if (int-negative? b)
                        (values 1 (+ (- b) a))
                        (values -1 (+ b a)))
                    (values 0 a)))
               ((ratnum? b)
                (%ratnum-div-and-mod a b))
               ((flonum? b)
                (fldiv-and-mod (inexact a) b))
               (else (err a b))))
        ((int? a)
         (cond ((fixnum? b)
                (cond
                  ((eqv? b 0)
                   (assertion-violation 'div-and-mod "Division by zero" a b))
                  ((fx<=? 1 b (digit-mask))
                   (let-values ([(q r) (bignum-quo+rem-digit a b)])
                     ;; r is already a fixnum.
                     (if (fx>=? r 0)
                         (values (bnsimplify! q) r)
                         (values (- q 1) (+ r b)))))
                  (else
                   (div-and-mod a (fixnum->int b)))))
               ((int? b)
                ;; TODO: avoid the multiplication
                (let ((d (div a b)))
                  (let ((m (- a (* b d))))
                    (values d m))))
               ((ratnum? b)
                (%ratnum-div-and-mod a b))
               ((flonum? b)
                (fldiv-and-mod (inexact a) b))
               (else (err a b))))
        ((ratnum? a)
         (cond ((or (fixnum? b) (int? b) (ratnum? b))
                (%ratnum-div-and-mod a b))
               ((flonum? b)
                (fldiv-and-mod (inexact a) b))
               (else (err a b))))
        ((flonum? a)
         (fldiv-and-mod a (inexact b)))
        (else (err a b))))

(define (div a b)
  (cond ((fixnum? a)
         (cond ((fixnum? b)
                (if (and (eqv? a (least-fixnum))
                         (eqv? b -1))
                    (+ (greatest-fixnum) 1)
                    (fxdiv a b)))
               (else
                (div (fixnum->int a) b))))
        ((int? a)
         (cond ((fixnum? b)
                (cond
                  ((eqv? b 0)
                   (assertion-violation 'div "Division by zero" a b))
                  ((fx<=? 1 b (digit-mask))
                   (let-values ([(q r) (bignum-quo+rem-digit a b)])
                     (if (fx>=? r 0)
                         (bnsimplify! q)
                         (- q 1))))
                  (else
                   (div a (fixnum->int b)))))
               ((int? b)
                (bnsimplify!
                 (cond ((int-negative? b)
                        ;; (ceil (/ a b))
                        (if (int-negative? a)
                            (let-values (((d _) (int-quo+rem (+ a b 1) b))) d)
                            (let-values (((d _) (int-quo+rem a b))) d)))
                       (else
                        ;; (floor (/ a b))
                        (if (int-negative? a)
                            (let-values (((d _) (int-quo+rem (+ (- a b) 1) b))) d)
                            (let-values (((d _) (int-quo+rem a b))) d))))))
               (else
                ;; TODO: direct implementations
                (let-values (((d _) (div-and-mod a b))) d))))
        (else
         (let-values (((d _) (div-and-mod a b))) d))))

(define (mod a b)
  (cond ((fixnum? a)
         (cond ((fixnum? b)
                (fxmod a b))
               (else
                (mod (fixnum->int a) b))))
        ((int? a)
         (cond ((fixnum? b)
                (mod a (fixnum->int b)))
               ((int? b)
                (let-values (((_ r) (int-quo+rem a b)))
                  (if (int-negative? b)
                      (if (int-negative? r) (- r b) (bnsimplify! r))
                      (if (int-negative? r) (+ r b) (bnsimplify! r)))))
               (else
                ;; TODO: direct implementations
                (let-values (((_ m) (div-and-mod a b))) m))))
        (else
         (let-values (((_ m) (div-and-mod a b))) m))))

;; Centered division.
(define (div0-and-mod0 a b)
  (let-values (((d m) (sys:div-and-mod a b)))
    (if (sys:negative? b)
        (if (sys:< m (sys:/ (sys:- b) 2))
            (values d m)
            (values (sys:- d 1) (sys:+ m b)))
        (if (sys:< m (sys:/ b 2))
            (values d m)
            (values (sys:+ d 1) (sys:- m b))))))

(define (div0 a b)
  (let-values (((d _) (div0-and-mod0 a b)))
    d))

(define (mod0 a b)
  (let-values (((_ m) (div0-and-mod0 a b)))
    m))

;; Stein's algorithm with optimizations.
(define gcd
  (case-lambda
    ((u v)
     (define (bad-args u v)
       (assertion-violation 'gcd "Expected integers" u v))
     (let gcd ((u u) (v v))
       (cond
         ((eqv? u 0) v)
         ((eqv? v 0) u)
         ((and (fixnum? u) (fixnum? v)
               (not (eqv? u (least-fixnum)))
               (not (eqv? v (least-fixnum))))
          ;; Fast path for fixnums
          (let* ((asr fxarithmetic-shift-right)
                 (u (if (fxnegative? u) (fx- u) u))
                 (v (if (fxnegative? v) (fx- v) v))
                 (U (fxfirst-bit-set u))
                 (V (fxfirst-bit-set v)))
            (let lp ((u (asr u U)) (v (asr v V)))
              (if (fx>? u v)
                  (lp v u)
                  (let ((v (fx- v u)))
                    (if (eqv? v 0)
                        (fxarithmetic-shift-left u (fxmin U V))
                        (lp (asr v (fxfirst-bit-set v)) u)))))))
         ((or (fixnum? u) (int? u))
          ;; TODO: use Lehmer's gcd algorithm for bignums (Handbook of
          ;; Applied Cryptography, chapter 14).
          (cond
            ((or (fixnum? v) (int? v))
             (let* ((asr sys:bitwise-arithmetic-shift-right)
                    (u (sys:abs u))
                    (v (sys:abs v))
                    (U (sys:bitwise-first-bit-set u))
                    (V (sys:bitwise-first-bit-set v)))
               (let lp ((u (asr u U)) (v (asr v V)))
                 (cond ((and (fixnum? u) (fixnum? v))
                        (gcd u v))      ;switch to the fast path
                       ((sys:> u v)
                        (lp v u))
                       (else
                        (let ((v (sys:- v u)))
                          (if (eqv? v 0)
                              (sys:bitwise-arithmetic-shift-left u (fxmin U V))
                              (lp (asr v (sys:bitwise-first-bit-set v)) u))))))))
            ((flonum? v)
             (gcd (inexact u) v))
            (else (bad-args u v))))
         ((flonum? u)
          (cond ((flonum? v)
                 (unless (and (flinteger? u) (flinteger? v))
                   (bad-args u v))
                 (inexact (sys:gcd (exact u) (exact v))))
                ((or (fixnum? v) (int? v))
                 (gcd u (inexact v)))
                (else (bad-args u v))))
         (else (bad-args u v)))))
    (() 0)
    ((u) (assert (sys:integer? u)) (sys:abs u))
    ((u v . x*)
     (fold-left sys:gcd (sys:gcd u v) x*))))

;; from the bignum book
(define lcm
  (case-lambda
    (() 1)
    ((a) (assert (integer? a)) (abs a))
    ((a b)
     (let ((c (gcd a b))
           (t (abs (* a b))))
       (if (eqv? c 0)
           0
           (div t c))))
    ((a b . x)
     (fold-left sys:lcm (lcm a b) x))))

(define (numerator x)
  (define (err x)
    (assertion-violation 'numerator
                         "Expected a real number" x))
  (cond ((ratnum? x) (ratnum-n x))
        ((fixnum? x) x)
        ((int? x) x)
        ((flonum? x)
         (inexact (numerator (exact x))))
        ((real? x)
         (error 'numerator "TODO" x))
        (else
         (err x))))

(define (denominator x)
  (cond
    ((flonum? x) (fldenominator x))
    ((ratnum? x) (ratnum-d x))
    ((fixnum? x) 1)
    ((int? x) 1)
    ((flonum? x) (denominator (exact x)))
    (else
     (assertion-violation 'denominator
                          "Expected a real number" x))))

(define (floor x)
  (define who 'floor)
  (cond
    ((flonum? x) (flfloor x))
    ((fixnum? x) x)
    ((int? x) x)
    ((ratnum? x)
     (let ((a (ratnum-n x))
           (b (ratnum-d x)))
       ;; TODO: verify
       (if (negative? a)
           (quotient (+ (- a b) 1) b)
           (quotient a b))))
    (else
     (assertion-violation who "Expected a real number" x))))

(define (ceiling x)
  (define who 'floor)
  (cond
    ((flonum? x) (flceiling x))
    ((fixnum? x) x)
    ((int? x) x)
    ((ratnum? x)
     (let ((a (ratnum-n x))
           (b (ratnum-d x)))
       ;; TODO: verify
       (if (negative? a)
           (quotient a b)
           (quotient (+ a b -1) b))))
    (else
     (assertion-violation who "Expected a real number" x))))

(define (truncate x)
  (define who 'truncate)
  (cond
    ((flonum? x) (fltruncate x))
    ((fixnum? x) x)
    ((int? x) x)
    ((ratnum? x)
     (let ((a (ratnum-n x))
           (b (ratnum-d x)))
       ;; TODO: verify
       (quotient a b)))
    (else
     (assertion-violation who "Expected a real number" x))))

(define (round x)
  (define who 'round)
  (cond
    ((flonum? x) (flround x))
    ((fixnum? x) x)
    ((int? x) x)
    ((ratnum? x)
     ;; Assumes the ratnum has been simplified.
     (let ((a (ratnum-n x)) (b (ratnum-d x)))
       (let-values (((d m) (div-and-mod a b)))
         (let ((m+m (+ m m)))
           (cond ((< m+m b) d)
                 ((> m+m b) (+ d 1))
                 ((even? d) d)
                 (else (+ d 1))))))
     #;
     (let ((a (ratnum-n x)) (b (ratnum-d x)))
       (let-values (((d m) (div-and-mod a b)))
         (let ((r (/ (- b m) b)))
           (cond ((> r 1/2) d)
                 ((< r 1/2) (+ d 1))
                 ((even? d) d)
                 (else (+ d 1)))))))
    (else
     (assertion-violation who "Expected a real number" x))))

(define (rationalize x e)
  ;; Implementation by Alan Bawden.
  (define (simplest-rational x y)
    (define (simplest-rational-internal x y)
      ;; Assumes 0 < X < Y
      (let ((fx (floor x))
            (fy (floor y)))
        (cond ((not (< fx x))
               fx)
              ((= fx fy)
               (+ fx
                  (/ (simplest-rational-internal
                      (/ (- y fy))
                      (/ (- x fx))))))
              (else
               (+ 1 fx)))))
    ;; do some juggling to satisfy preconditions
    ;; of simplest-rational-internal.
    (cond ((< y x)
           (simplest-rational y x))
          ((not (< x y))
           (cond ((rational? x)
                  x)
                 ((and (flonum? x) (not (flfinite? x)))
                  (if (and (flonum? e) (or (flnan? e) (fl=? x e)))
                      +nan.0
                      x))
                 (else
                  (assertion-violation 'rationalize
                                       "Expected a real number"
                                       x e))))
          ((positive? x)
           (simplest-rational-internal x y))
          ((negative? y)
           (- (simplest-rational-internal (- y)
                                          (- x))))
          (else
           (if (and (exact? x) (exact? y))
               0
               0.0))))
  (simplest-rational (- x e) (+ x e)))

;; Constants with 35 digits, more than needed.
(define pi 3.1415926535897932384626433832795029)
(define pi/2 1.5707963267948966192313216916397514)

(define (exp x)
  (cond
    ((eqv? x 0) 1)
    ((flonum? x) (flexp x))
    ((compnum? x)
     ;; XXX: don't use inexact always here
     (let ((x (inexact (real-part x)))
           (y (inexact (imag-part x))))
       (* (exp x)
          (+ (cos y) (* +i (sin y)))))) ;(exp (* +i y))
    (else (flexp (inexact x)))))

(define log
  (case-lambda
    ((z)
     (cond
       ;; Paraphrasing R6RS: "The value of log z is defined to be the
       ;; one whose imaginary part lies in the range from −π
       ;; (inclusive) to π (inclusive). log 0 is undefined."
       ((or (compnum? z) (negative? z) (eqv? z -0.0))
        (+ (log (magnitude z))
           (* +i (angle z))))
       ((and (flonum? z) (flnan? z))
        (+ (flabs z) (* +i (angle z))))
       ((eqv? z 0)
        (assertion-violation 'log "This function is undefined for 0" z))
       ((eqv? z 1)
        0)
       (else
        (fllog (inexact z)))))
    ((z b)
     (/ (log z) (log b)))))

(define (sin z)
  (cond ((eqv? z 0)
         0)
        ((flonum? z)
         (flsin z))
        ((or (fixnum? z)
             (int? z)
             (ratnum? z))
         (flsin (inexact z)))
        ((compnum? z)
         (/ (- (exp (* +i z))
               (exp (* -i z)))
            +2i))
        (else
         (assertion-violation 'sin "Expected a number" z))))

(define (cos z)
  (cond ((eqv? z 0)
         1)
        ((flonum? z)
         (flcos z))
        ((or (fixnum? z)
             (int? z)
             (ratnum? z))
         (flcos (inexact z)))
        ((compnum? z)
         (/ (+ (exp (* +i z))
               (exp (* -i z)))
            2))
        (else
         (assertion-violation 'cos "Expected a number" z))))

(define (tan z)
  (if (flonum? z)
      (fltan z)
      (/ (sin z) (cos z))))

(define (asin z)
  (cond
    ((eqv? z 0) 0)
    ((and (flonum? z) (fl<=? -1.0 z 1.0))
     (flasin z))
    ((eqv? z -1) (sys:- pi/2))
    ((eqv? z 1) pi/2)
    ((and (real? z) (< -1 z 1))
     (atan (/ z (sqrt (- 1 (expt z 2))))))
    (else
     (* -i (log (+ (* +i z)
                   (sqrt (- 1 (expt z 2)))))))))

(define (acos z)
  (cond
    ((eqv? z 1) 0)
    ((and (flonum? z) (fl<=? -1.0 z 1.0))
     (flacos z))
    (else
     (- pi/2 (asin z)))))

(define atan
  (case-lambda
    ((z)
     (cond
       ((flonum? z)
        (flatan z))
       ((or (fixnum? z) (int? z) (ratnum? z))
        (flatan (inexact z)))
       ((or (eqv? z -i) (eqv? z +i))
        (assertion-violation 'atan "Undefined" z))
       (else
        (let ((iz (* +i z)))
          (/ (- (log (+ 1 iz))
                (log (- 1 iz)))
             +2i)))))
    ((y x)
     (when (and (eqv? y 0) (eqv? x 0))
       (assertion-violation 'atan "Undefined" y x))
     (unless (and (real? y) (real? x))
       (assertion-violation 'atan "Expected real numbers" y x))
     ;; XXX: If compnums can be mixed exactness, then x=0, y=±0.0 is a
     ;; special case.
     (flatan (inexact y) (inexact x)))))

(define (sqrt z)
  (cond ((and (flonum? z) (fl>=? z 0.0))
         (flsqrt z))
        ((eqv? z -inf.0)
         0+inf.0i)
        ((rcompnum? z)
         (let ((a (rcompnum-r z))
               (b (rcompnum-i z)))      ;b≠0
           (let* ((m (sqrt (+ (* a a) (* b b))))
                  (g (sqrt (/ (+ m a) 2)))
                  (d^ (sqrt (/ (- m a) 2))))
             (make-rectangular g (if (or (negative? b) (eqv? b -0.0))
                                     (- d^)
                                     d^)))))
        ((pcompnum? z)
         (make-polar (sqrt (magnitude z))
                     (/ (angle z) 2)))
        ((or (fixnum? z) (int? z))
         (let ((z^ (abs z)))
           (let-values ([(s r) (exact-integer-sqrt z^)])
             (if (eqv? r 0)
                 (if (negative? z)
                     (make-rectangular 0 s)
                     s)
                 (expt z 1/2)))))
        ((ratnum? z)
         (let ((z^ (abs z)))
           (let-values ([(s0 r0) (exact-integer-sqrt (numerator z^))]
                        [(s1 r1) (exact-integer-sqrt (denominator z^))])
             (if (and (eqv? r0 0) (eqv? r1 0))
                 (if (negative? z)
                     (make-rectangular 0 (/ s0 s1))
                     (/ s0 s1))
                 (expt z 1/2)))))
        (else
         (expt z 1/2))))

(define (exact-integer-sqrt k)
  (define who 'exact-integer-sqrt)
  (unless (and (or (fixnum? k) (int? k))
               (not (sys:negative? k)))
    (assertion-violation who "Expected a positive exact integer" k))
  (let lp ((k k)
           (bit (let lp ((p 0))
                  (if (> (expt 4 (+ p 1)) k)
                      (expt 4 p)
                      (lp (+ p 1)))))
           (ret 0))
    (cond ((zero? bit)
           (values ret k))
          ((>= k (+ ret bit))
           (lp (- k (+ ret bit))
               (bitwise-arithmetic-shift-right bit 2)
               (+ (bitwise-arithmetic-shift-right ret 1) bit)))
          (else
           (lp k
               (bitwise-arithmetic-shift-right bit 2)
               (bitwise-arithmetic-shift-right ret 1))))))

;; Exponentiation. This shy operation does not even need a symbol in
;; the mathematical notation.
(define (expt base exponent)
  (cond ((and (eqv? base 2) (fixnum? exponent) (fx>=? exponent 0))
         (if (fx<? exponent (fx- (fixnum-width) 1))
             (fxarithmetic-shift-left 1 exponent)
             (bnsimplify! (bignum-expt2 exponent 1))))
        ;; TODO: square for (eqv? exponent 2)?
        ((eqv? exponent 0) 1)
        ((eqv? exponent 0.0) 1.0)
        ((or (eqv? base 0) (and (flonum? base) (fl=? base 0.0)))
         (cond
           ((or (and (real? exponent) (negative? exponent))
                (and (compnum? exponent) (exact? exponent)
                     (eqv? 0 (real-part exponent))))
            (raise
              (condition
               (make-who-condition 'expt)
               (make-implementation-restriction-violation)
               (make-message-condition "Undefined")
               (make-irritants-condition (list base exponent)))))
           ((flonum? exponent)
            (flexpt 0.0 exponent))
           ((exact? exponent)
            0)
           (else 0.0)))
        ((pcompnum? base)
         (make-polar (expt (pcompnum-m base) exponent)
                     (* (pcompnum-a base) exponent)))
        ((compnum? exponent)
         (exp (* exponent (log base))))
        ;; real non-zero base
        ((ratnum? exponent)
         (cond ((not (negative? base))
                (flexpt (inexact base) (inexact exponent)))
               ((and (eqv? base -1) (eqv? exponent 1/2))
                +i)
               (else
                (exp (* exponent (log base))))))
        ((flonum? base)
         (flexpt base (inexact exponent)))
        ((flonum? exponent)
         (flexpt (inexact base) exponent))
        #;
        ((negative? exponent)
         (expt (/ base) (- exponent)))
        #;
        ((ratnum? base)
         (/ (expt (numerator base) exponent)
            (expt (denominator base) exponent)))
        ((and (number? base) (or (fixnum? exponent) (int? exponent)))
         (let-values ([(base exponent)
                       (if (negative? exponent)
                           (values (/ base) (- exponent))
                           (values base exponent))])
           (let lp ((base base) (exponent exponent) (result 1))
             (if (zero? exponent)
                 result
                 (lp (* base base)
                     (bitwise-arithmetic-shift-right exponent 1)
                     (if (bitwise-bit-set? exponent 0)
                         (* result base)
                         result))))))
        (else
         (assertion-violation 'expt "Expected numbers" base exponent))))

(define (make-rectangular r i)
  (unless (and (real? r) (real? i))
    (assertion-violation 'make-polar "Expected real numbers" r i))
  (make-rcompnum r i))

(define (make-polar m a)
  (unless (and (real? m) (real? a))
    (assertion-violation 'make-polar "Expected real numbers" m a))
  (make-pcompnum m a))

(define (real-part z)
  (cond ((rcompnum? z)
         (rcompnum-r z))
        ((pcompnum? z)
         (* (pcompnum-m z) (cos (pcompnum-a z))))
        ((number? z) z)
        (else
         (assertion-violation 'real-part
                              "Expected a number" z))))

(define (imag-part z)
  (cond ((rcompnum? z)
         (rcompnum-i z))
        ((pcompnum? z)
         (* (pcompnum-m z) (sin (pcompnum-a z))))
        ((number? z) 0)
        (else
         (assertion-violation 'imag-part
                              "Expected a number" z))))

(define (magnitude z)
  (cond ((pcompnum? z)
         (pcompnum-m z))
        ((rcompnum? z)
         (let ((x (imag-part z)) (y (real-part z)))
           (sqrt (+ (* x x) (* y y)))))
        ((number? z) (abs z))
        (else
         (assertion-violation 'magnitude "Expected a number" z))))

(define (angle z)
  (cond ((pcompnum? z)
         (pcompnum-a z))
        ((rcompnum? z)
         (atan (imag-part z) (real-part z)))
        ((flonum? z)
         (if (or (flnegative? z) (eqv? z -0.0))
             pi
             0.0))
        ((or (fixnum? z) (int? z) (ratnum? z))
         (if (negative? z)
             pi
             0))
        (else
         (assertion-violation 'angle "Expected a number" z))))

;;; Bitwise operations

(define (bitwise-not x)
  (cond ((fixnum? x) (fxnot x))
        ((int? x) (bnnot x))
        (else
         (assertion-violation 'bitwise-not "Expected an exact integer" x))))

(define bitwise-ior
  (case-lambda
    (() 0)
    ((a) (assert (or (fixnum? a) (int? a))) a)
    ((a b)
     (define (err a b)
       (assertion-violation 'bitwise-ior "Expected exact integers" a b))
     (cond
       ((fixnum? a)
        (cond ((fixnum? b) (fxior a b))
              ((int? b) (bnior b (fixnum->int a)))
              (else (err a b))))
       ((int? a)
        (cond ((fixnum? b) (bnior a (fixnum->int b)))
              ((int? b) (bnior a b))
              (else (err a b))))
       (else (err a b))))
    ((a b c)
     (bitwise-ior (sys:bitwise-ior a b) c))
    ((a b c d)
     (bitwise-ior (sys:bitwise-ior a b c) d))
    ((a b c d . x)
     (fold-left sys:bitwise-ior (sys:bitwise-ior a b c d) x))))

(define bitwise-xor
  (case-lambda
    (() 0)
    ((a) (assert (or (fixnum? a) (int? a))) a)
    ((a b)
     (define (err a b)
       (assertion-violation 'bitwise-xor "Expected exact integers" a b))
     (cond
       ((fixnum? a)
        (cond ((fixnum? b)
               (fxxor a b))
              ((int? b)
               (bitwise-xor (fixnum->int a) b))
              (else (err a b))))
       ((int? a)
        (cond ((fixnum? b)
               (bitwise-xor a (fixnum->int b)))
              ((int? b)
               (if (int-negative? a)
                   (if (int-negative? b)
                       (bitwise-xor (bitwise-not a) (bitwise-not b))
                       (bitwise-xor b a))
                   (if (int-negative? b)
                       (bitwise-not (bitwise-xor a (bitwise-not b)))
                       (int-xor a b))))
              (else (err a b))))
       (else (err a b))))
    ((a b c)
     (bitwise-xor (bitwise-xor a b) c))
    ((a b c d)
     (bitwise-xor (bitwise-xor a b c) d))
    ((a b c d . x)
     (fold-left sys:bitwise-xor (bitwise-xor a b c d) x))))

(define bitwise-and
  (case-lambda
    (() -1)
    ((a) (assert (or (fixnum? a) (int? a))) a)
    ((a b)
     (define (err a b)
       (assertion-violation 'bitwise-and "Expected exact integers" a b))
     (cond
       ((fixnum? a)
        (cond ((fixnum? b) (fxand a b))
              ((int? b) (bnand b (fixnum->int a)))
              (else (err a b))))
       ((int? a)
        (cond ((fixnum? b) (bnand a (fixnum->int b)))
              ((int? b) (bnand a b))
              (else (err a b))))
       (else (err a b))))
    ((a b c)
     (sys:bitwise-and (sys:bitwise-and a b) c))
    ((a b c d)
     (sys:bitwise-and (sys:bitwise-and a b c) d))
    ((a b c d . x)
     (fold-left sys:bitwise-and (bitwise-and a b c d) x))))

(define (bitwise-if a b c)
  (bitwise-ior (bitwise-and a b)
               (bitwise-and (bitwise-not a) c)))

(define (bitwise-bit-count x)
  (cond ((fixnum? x) (fxbit-count x))
        ((int? x) (bnbit-count x))
        (else
         (assertion-violation 'bitwise-bit-count
                              "Expected an exact integer" x))))

(define (bitwise-length n)
  (cond ((fixnum? n) (fxlength n))
        ((int? n) (bnlength n))
        (else
         (assertion-violation 'bitwise-length
                              "Expected an exact integer" n))))

(define (bitwise-first-bit-set n)
  (cond ((fixnum? n) (fxfirst-bit-set n))
        ((int? n) (bnfirst-bit-set n))
        (else
         (assertion-violation 'bitwise-first-bit-set
                              "Expected an exact integer" n))))

(define (bitwise-bit-set? n bit)
  (define (err n bit)
    (assertion-violation 'bitwise-bit-set?
                         "Expected exact integers" n bit))
  (cond ((fixnum? bit)
         (assert (not (fxnegative? bit)))
         (cond ((fixnum? n)
                (if (fx<? bit (fixnum-width))
                    (fxbit-set? n bit)
                    (fxnegative? n)))
               ((int? n)
                (if (int-negative? n)
                    (eqv? 1 (bitwise-and 1 (bitwise-arithmetic-shift-right n bit)))
                    (bnbit-set? n bit)))
               (else
                (err n bit))))
        ((int? bit)
         (assert (not (int-negative? bit)))
         (negative? n))
        (else
         (err n bit))))

(define (bitwise-copy-bit n k bit)
  ;; Set bit 'k' of 'n' to 'bit'. TODO: shortcuts. Better errors for
  ;; huge k.
  (assert (fx<=? 0 bit 1))
  (assert (not (fxnegative? k)))
  (let ((mask (expt 2 k)))
    (bitwise-if mask
                (bitwise-arithmetic-shift-left bit k)
                n)))

(define (bitwise-bit-field n start end)
  (assert (fx<=? 0 start end))
  (cond
    ((fx=? start end) 0)
    ((and (int? n)
          (fx<=? (fx- end start) (fx* 2 (digit-width))))
     (assert (fx<? (fx* 2 (digit-width)) (fixnum-width)))
     (bnbit-field1 n start end (int-negative? n)))
    (else
     (let ((mask (sys:bitwise-not (sys:bitwise-arithmetic-shift-left -1 end))))
       (sys:bitwise-arithmetic-shift-right (sys:bitwise-and n mask) start)))))

(define (bitwise-copy-bit-field to start end from)
  (assert (fx<=? 0 start end))
  (let* ((mask1 (bitwise-arithmetic-shift-left -1 start))
         (mask2 (bitwise-not
                 (bitwise-arithmetic-shift-left -1 end)))
         (mask (bitwise-and mask1 mask2)))
    (bitwise-if mask
                (bitwise-arithmetic-shift-left from start)
                to)))

(define (bitwise-arithmetic-shift n a)
  (if (fxpositive? a)
      (bitwise-arithmetic-shift-left n a)
      (bitwise-arithmetic-shift-right n (fx- a))))

(define (bitwise-arithmetic-shift-left n a)
  (define who 'bitwise-arithmetic-shift-left)
  (cond ((fixnum? n)
         (cond ((eqv? n 0)
                (unless (or (and (fixnum? a) (fx>=? a 0)) (int? a))
                  (assertion-violation who "Expected exact integers" n a))
                0)
               (else
                (unless (and (fixnum? a) (fx>=? a 0))
                  (assertion-violation who "Expected exact integers" n a))
                (or ($fxasl/false n a)
                    (bitwise-arithmetic-shift-left (fixnum->int n) a)))))
        ((int? n)
         (unless (and (fixnum? a) (fx>=? a 0))
           (if (not (fixnum? a))
               (raise (condition
                       (make-who-condition who)
                       (make-implementation-restriction-violation)
                       (make-message-condition "Expected a fixnum shift amount")
                       (make-irritants-condition (list n a))))
               (assertion-violation who "Expected a positive shift amount" n a)))
         (if (eqv? a 1)
             (bnasl1 n)
             (bnasl n a)))
        (else
         (assertion-violation who "Expected exact integers" n a))))

(define (bitwise-arithmetic-shift-right n a)
  (define (err n a)
    (define who 'bitwise-arithmetic-shift-right)
    (assertion-violation who "Expected exact integers" n a))
  (cond ((fixnum? a)
         (assert (not (fxnegative? a)))
         (cond ((fixnum? n)
                (if (fx<=? 0 a (fx- (fixnum-width) 1))
                    (fxarithmetic-shift-right n a)
                    (if (fxnegative? n) -1 0)))
               ((int? n)
                (cond ((int-negative? n)
                       ;; FIXME: this should be special-cased
                       (bitwise-not
                        (if (eqv? a 1)
                            (bnasr1 (bitwise-not n))
                            (bnasr (bitwise-not n) a))))
                      (else
                       (if (eqv? a 1)
                           (bnasr1 n)
                           (bnasr n a)))))
               (else
                (err n a))))
        ((int? a)
         (assert (not (int-negative? a)))
         (if (negative? n) -1 0))
        (else
         (err n a))))

(define (bitwise-rotate-bit-field n start end count)
  ;; Straight out of r6rs-lib
  (assert (fx<=? 0 start end))
  (assert (not (fxnegative? count)))
  (let ((width (fx- end start)))
    (cond ((fxpositive? width)
           (let* ((count (fxmod count width))
                  (field0 (bitwise-bit-field n start end))
                  (field1 (bitwise-arithmetic-shift-left field0 count))
                  (field2 (bitwise-arithmetic-shift-right field0 (fx- width count)))
                  (field (bitwise-ior field1 field2)))
             (bitwise-copy-bit-field n start end field)))
          (else
           (unless (or (fixnum? n) (int? n))
             (assertion-violation 'bitwise-rotate-bit-field
                                  "Expected exact integers" n start end count))
           n))))

(define (bitwise-reverse-bit-field n start end)
  (assert (fx<=? 0 start end))
  (do ((i start (fx+ i 1))
       (ret 0 (if (bitwise-bit-set? n i)
                  (bitwise-ior ret (bitwise-arithmetic-shift-left 1 (fx- (fx- end i) 1)))
                  ret)))
      ((fx=? i end)
       (bitwise-ior (bitwise-arithmetic-shift-left ret start)
                    (bitwise-copy-bit-field n start end 0)))))

;;; Loko-specific bitwise operations

;; Name is from "BLSR - Reset Lowest Set Bit". The case for a
;; negative i is not consistent with the name. (FIXME: rename).
(define (bitwise-lsr i)
  (cond
    ((fixnum? i)
     (if (eqv? i (least-fixnum))
         (bitwise-arithmetic-shift-left (least-fixnum) 1)
         (fxand i (fx- i 1))))
    ((int? i)
     (cond
       ((int-negative? i)
        ;; This is less simple than the positive case, because it
        ;; requires a carry. So bail out. XXX: fragile.
        (bitwise-and (- i 1) i))
       (else
        (let* ((ret (make-int (int-used i) 1))
               (ret* (int-digits ret))
               (i* (int-digits i))
               (n (int-used i)))
          (let lp ((j 0))
            (unless (fx=? j n)
              (let ((digit (digit-ref i* j)))
                (cond ((eqv? 0 digit)
                       (lp (fx+ j 1)))
                      (else
                       (digit-set! ret* j (fxand digit (fx- digit 1)))
                       (let lp ((j (fx+ j 1)))
                         (when (fx<? j n)
                           (digit-set! ret* j (digit-ref i* j))
                           (lp (fx+ j 1)))))))))
          (bnsimplify! ret)))))
    (else
     (assertion-violation 'bitwise-lsr "Expected an exact integer" i))))

;;; R5RS compatibility

;; TODO: use the real quotient and remainder procedures for bignums

(define (sign n)                      ;helper
  (cond
    ((negative? n) -1)
    ((positive? n) 1)
    (else 0)))

(define (quotient n1 n2)
  (cond ((and (fixnum? n1) (fixnum? n2))
         (if (and (eqv? n1 (least-fixnum))
                  (eqv? n2 -1))
             (+ (greatest-fixnum) 1)
             (let-values ([(q _r) ($fxquo+rem n1 n2)])
               q)))
        (else
         (* (fx* (sign n1) (sign n2)) (div (abs n1) (abs n2))))))

(define (remainder n1 n2)
  (cond ((and (fixnum? n1) (fixnum? n2))
         (if (and (eqv? n1 (least-fixnum))
                  (eqv? n2 -1))
             0
             (let-values ([(_q r) ($fxquo+rem n1 n2)])
               r)))
        (else
         (* (sign n1) (mod (abs n1) (abs n2))))))

(define (modulo n1 n2)
  (* (sign n2) (mod (* (sign n2) n1) (abs n2))))

;;; Bignum

;; (define (digit-width) (fixnum-width))

(define (digit-width) 30)

(define (digit^-width)
  ;; must not be larger than (- (fixnum-width) 1)
  (fx+ (fx* (digit-width) 2) 1))

(define (digit-radix)
  (fxarithmetic-shift-left 1 (digit-width)))

(define (digit-mask)
  (fx- (digit-radix) 1))

(define-syntax with-carry
  (lambda (x)
    (syntax-case x (+ - *)
      ((_ ((sum carry) (+ (* m1 m2) a c))
          body ...)
       ;; This needs digit^-width bits to represent the result
       #'(let ((tmp^ (fx+ (fx* m1 m2) (fx+ a c))))
           (let ((sum (fxand tmp^ (digit-mask)))
                 (carry (fxarithmetic-shift-right tmp^ (digit-width))))
             body ...)))
      ((_ ((sum carry) (+ a b c))
          body ...)
       #'(let ((tmp (fx+ a (fx+ b c))))
           (let ((sum (fxand tmp (digit-mask)))
                 (carry (fxarithmetic-shift-right tmp (digit-width))))
             body ...)))
      ((_ ((sum carry) (- a b c))
          body ...)
       #'(let ((tmp (fx- (fx- a b) c)))
           (let ((sum (fxand tmp (digit-mask)))
                 (carry
                  (fxand #b1
                         (fxarithmetic-shift-right tmp
                                                   (digit-width)))))
             body ...))))))

(begin
  ;; This representation has the digits in a vector
  (define make-int
    (case-lambda
      ((len sign . _)
       ($box ($make-box-header 'bignum #t
                               (fxior number:exact? number:real?)
                               3)
             len
             sign
             (make-vector len 0)))))
  (define ø (make-int 0 1))
  (define (int? obj)
    (and ($box? obj)
         ($box-header-type-eq? ($box-type obj) 'bignum)))
  (define (int-used x) ($box-ref x 0))
  (define (int-used-set! x v) ($box-set! x 0 v))
  (define (int-sign x) ($box-ref x 1))
  (define (int-sign-set! x v) ($box-set! x 1 v))
  (define (int-digits x) ($box-ref x 2))
  (define (int-size x) (vector-length (int-digits x)))
  (define (int-sign=? x y) (eq? (int-sign x) (int-sign y)))
  (define digit-ref vector-ref)
  (define digit-set! vector-set!))

#;
(begin
  ;; For running on top of a Scheme
  (define fxasl fxarithmetic-shift-left)
  (define fxasr fxarithmetic-shift-right)
  (define-record-type int
    (fields (mutable used) (mutable sign) (mutable digits) name)
    (protocol
     (lambda (p)
       (case-lambda
         (()
          (p 0 1 '#() #f))
         ((len)
          (p len 1 (make-vector len 0) #f))
         ((len sign)
          (p len sign (make-vector len 0) #f))
         ((len sign name)
          (p len sign (make-vector len 0) name))))))
  (define (int-size x) (vector-length (int-digits x)))
  (define (int-sign=? x y) (eq? (int-sign x) (int-sign y)))
  (define digit-ref vector-ref)
  (define digit-set! vector-set!)
  (define ø (make-int)))

(define (digits-copy! target tstart source sstart send)
  ;; Based on public domain code from SRFI-43 by Taylor Campbell.
  (if (or (not (eq? target source)) (fx>? sstart tstart))
      (let lp ((i sstart) (j tstart))
        (when (fx<? i send)
          (digit-set! target j (digit-ref source i))
          (lp (fx+ i 1) (fx+ j 1))))
      (let lp ((i (fx- send 1))
               (j (fx- (fx+ tstart (fx- send sstart)) 1)))
        (when (fx>=? i sstart)
          (digit-set! target j (digit-ref source i))
          (lp (fx- i 1) (fx- j 1))))))

(define (display-int n p base)
  ;; Print bignums by splitting them into fixnums. Uses divide &
  ;; conquer. The numbers are first split almost in half, then the
  ;; halves are split in half etc, approx lg n times, until
  ;; everything's a fixnum.
  (define (powers-of-ten n)
    (let lp ((power base) (digits 1) (ret '()))
      (if (> power n)
          ret
          (lp (* power power)
              (* digits 2)
              (cons (cons power digits) ret)))))
  (define (split n p)
    (let lp ((n n) (pad 0) (powers (powers-of-ten n)))
      ;; (display (list 'split 'n pad powers)) (newline)
      (cond ((fixnum? n)
             (let ((str (if (eqv? base 10)
                            (fx->decimal n)
                            (fx->binary-base n base))))
               (do ((i (string-length str) (fx+ i 1)))
                   ((fx>=? i pad))
                 (put-char p #\0))
               (display str p)))
            ((> (caar powers) n)
             (lp n pad (cdr powers)))
            (else
             ;; XXX: quotient and remainder might be better here.
             (let-values ([(d m) (div-and-mod n (caar powers))])
               (let ((digits (cdar powers)))
                 ;; TODO: fx>? and fx- ?
                 (lp d (if (fx>? pad digits) (fx- pad digits) 0) (cdr powers))
                 (lp m digits (cdr powers))))))))
  (cond ((negative? n)
         (display #\- p)
         (split (abs n) p))
        (else
         (split n p))))

(define (bn->binary-base n base)
  (define bits-per-digit
    (case base
      ((2) 1)
      ((8) 3)
      ((16) 4)
      (else (error 'bn->binary-base "Invalid base" base))))
  (define bits-per-step 48)             ;multiple of (lcm 1 3 4)
  (define digits-per-step (fxdiv bits-per-step bits-per-digit))
  (define mask (fx- (fxarithmetic-shift-left 1 bits-per-digit) 1))
  (let* ((nlen (bnlength n))
         (sign (if (int-negative? n) 1 0))
         (slen (fx+ sign (fxdiv (fx+ nlen (fx- bits-per-digit 1)) bits-per-digit)))
         (ret (make-string slen #\0)))
    (when (eqv? sign 1)
      (string-set! ret 0 #\-))
    (do ((ei (fx- sign 1))
         (si (fx- slen 1) (fx- si digits-per-step))
         (ni 0 (fx+ ni bits-per-step)))
        ((fx<=? si ei)
         ret)
      (let ((part (bnbit-field1 n ni (fx+ ni bits-per-step) #f)))
        (do ((si si (fx- si 1))
             (part part (fxarithmetic-shift-right part bits-per-digit))
             (i 0 (fx+ i 1)))
            ((eqv? part 0))
          (let ((digit (fxand part mask)))
            (unless (eqv? digit 0)
              (string-set! ret si (string-ref "0123456789ABCDEF" digit)))))))))

(define (int-zero? i) (eqv? (int-used i) 0))

(define (int-positive? i)
  (or (eqv? (int-sign i) 1) (int-zero? i)))

(define (int-negative? i)
  (eqv? (int-sign i) -1))

(define (int-inexact int)
  (let ((ret (let ((used (int-used int)))
               (cond
                 ((eqv? used 0)
                  0.0)
                 ((eqv? used 1)
                  (fixnum->flonum (digit-ref (int-digits int) 0)))
                 (else
                  ;; XXX: Single-precision only. Needs more testing.
                  ;; Could be faster.
                  (let* ((len (bitwise-length int))
                         (shift (fx- len (digit-width)))
                         (a (fixnum->flonum
                             (bitwise-bit-field (abs int) shift len))))
                    (do ((a a (fl* 2.0 a))
                         (shift shift (fx- shift 1)))
                        ((eqv? shift 0)
                         a))))))))
    (if (negative? int)
        (fl- ret)
        ret)))

(define (grow! int size)
  ;; XXX:
  (when (fx<? (int-size int) size)
    (error 'grow! "Too little memory allocated" int size)
    #;
    (let ((old (int-digits int))
          (new (make-vector size 0)))
      (unless (eqv? (vector-length old) 0)
        (digits-copy! new 0 old 0 (vector-length old)))
      (int-digits-set! int new)))
  (int-used-set! int size))

(define (clamp! int)
  (let ((int* (int-digits int)))
    (let lp ((used (int-used int)))
      (cond ((eqv? used 0)
             (int-used-set! int 0)
             (int-sign-set! int 1))
            (else
             (let ((used^ (fx- used 1)))
               (if (eqv? (digit-ref int* used^) 0)
                   (lp used^)
                   (int-used-set! int used))))))))

(define (copy! to from)
  (unless (eq? to from)
    (let ((n (int-used from)))
      (int-sign-set! to (int-sign from))
      (grow! to (int-used from))
      ;;(int-used-set! to (int-used from))
      (do ((i 0 (fx+ i 1))
           (src* (int-digits from))
           (dst* (int-digits to)))
          ((fx=? i n))
        (digit-set! dst* i (digit-ref src* i))))))

(define (bnneg src)
  (if (eqv? (int-digits src) 0)
      0
      (let* ((ret (make-int (int-used src) (if (int-negative? src) 1 -1)))
             (ret* (int-digits ret))
             (src* (int-digits src)))
        (digits-copy! ret* 0 src* 0 (int-used src))
        (bnsimplify! ret))))

(define (bnbit-count int)
  (if (not (int-negative? int))
      (let ((n (int-used int))
            (int* (int-digits int)))
        (let lp ((i 0) (ret 0))
          (cond ((fx<? i n)
                 (lp (fx+ i 1) (fx+ ret (fxbit-count (digit-ref int* i)))))
                (else ret))))
      ;; Fused bitwise-not and bit-counting
      (do ((n (int-used int))
           (src* (int-digits int))
           (i 0 (fx+ i 1))
           (c 0 (fx+ c (digit-width))))
          ((not (eqv? (digit-ref src* i) 0)) ;first non-zero digit?
           (do ((i (fx+ i 1) (fx+ i 1))
                (c (fx+ c (fxbit-count (fx- (digit-ref src* i) 1)))
                   (fx+ c (fxbit-count (digit-ref src* i)))))
               ((fx=? i n)
                (fxnot c)))))))

(define (fixnum->int v)
  (cond
    ((eqv? v (least-fixnum))
     (bignum-expt2 (fx- (fixnum-width) 1) -1))
    #;
    (#f
     (do ((sign (if (fxnegative? v) -1 1))
          (v (abs v) (fxarithmetic-shift-right v (digit-width)))
          (digits '() (cons (fxand v (digit-mask)) digits)))
         ((eqv? v 0)
          (let ((i (make-int (length digits) sign)))
            (int-digits-set! i 2 (list->vector (reverse digits)))
            i))))
    (else
     (let* ((sign (if (fxnegative? v) -1 1))
            (v (if (fxnegative? v) (fx- v) v)))
       (cond
         ((eqv? v 0)
          ø)
         ((fx<? v (digit-radix))
          (let* ((i (make-int 1 sign))
                 (digits (int-digits i)))
            (digit-set! digits 0 v)
            i))
         (else
          (let* ((i (make-int 2 sign))
                 (digits (int-digits i)))
            (digit-set! digits 0 (fxand v (digit-mask)))
            (digit-set! digits 1 (fxand (fxarithmetic-shift-right v (digit-width))
                                        (digit-mask)))
            (unless (eqv? 0 (fxarithmetic-shift-right v (fx* 2 (digit-width))))
              (error 'fixnum->int "Unimplemented length" v))
            i)))))))

;; Returns a fixnum if possible.
(define (bnsimplify! i)
  (cond
    ((fixnum? i) i)
    (#f
     (assert (= (fixnum-width) 61))
     (clamp! i)
     (cond ((or (cmp>= i 1152921504606846976)
                (cmp<= i -1152921504606846977))
            i)
           ((eq? (cmp-magnitude i 1152921504606846976) 'eq)
            (least-fixnum))
           (else
            (do ((j (fx- (int-used i) 1) (fx- j 1))
                 (i* (int-digits i))
                 (v 0 (fxior (fxasl v (digit-width))
                             (digit-ref i j))))
                ((eqv? j -1)
                 (if (int-negative? i)
                     (fx- v)
                     v))))))
    (else
     (clamp! i)
     ;; FIXME: Shrink it.
     (case (int-used i)
       ((0) 0)
       ((1)
        (let ((n (digit-ref (int-digits i) 0)))
          (if (int-negative? i)
              (fx- n)
              n)))
       ((2)
        (let ((d (int-digits i)))
          (let ((n0 (digit-ref d 0))
                (n1 (digit-ref d 1)))
            (cond ((fx<=? n1 (fxarithmetic-shift-right (greatest-fixnum) (digit-width)))
                   (let ((n (fxior n0 (fxarithmetic-shift-left n1 (digit-width)))))
                     (if (int-negative? i)
                         (fx- n)
                         n)))
                  (else
                   (error 'bnsimplify! "Unimplemented case" i))))))
       ((3)
        (let ((d (int-digits i)))
          (let ((n0 (digit-ref d 0))
                (n1 (digit-ref d 1))
                (n2 (digit-ref d 2)))
            (assert (eqv? 3 (fxdiv (fx+ (fixnum-width) (fx- (digit-width) 1)) (digit-width))))
            (cond ((and (int-negative? i)
                        (eqv? n0 0) (eqv? n1 0)
                        (eqv? n2 (fxarithmetic-shift-left 1 (fx- (fxmod (fixnum-width) (digit-width)) 1))))
                   (least-fixnum))
                  (else i)))))
       (else i)))))

(define (cmp-magnitude a b)
  (cond ((fx>? (int-used a) (int-used b)) 'gt)
        ((fx>? (int-used b) (int-used a)) 'lt)
        (else
         (let ((a* (int-digits a))
               (b* (int-digits b)))
           (let lp ((i (fx- (int-used a) 1)))
             (cond ((eqv? i -1) 'eq)
                   ((fx>? (digit-ref a* i) (digit-ref b* i)) 'gt)
                   ((fx>? (digit-ref b* i) (digit-ref a* i)) 'lt)
                   (else (lp (fx- i 1)))))))))

(define (cmp a b)
  (if (fx=? (int-sign a) (int-sign b))
      (if (int-negative? a)
          (cmp-magnitude b a)
          (cmp-magnitude a b))
      (if (int-negative? a)
          'lt
          'gt)))

(define (cmp= a b) (eq? 'eq (cmp a b)))

(define (cmp> a b) (eq? 'gt (cmp a b)))

(define (cmp< a b) (eq? 'lt (cmp a b)))

(define (cmp>= a b) (not (eq? 'lt (cmp a b))))

(define (cmp<= a b) (not (eq? 'gt (cmp a b))))

(define (digits+! dst* src1* src2* n1 n2)
  ;; dst[0:n2] = src1[0:n2]+src2[0:n1]
  (let lp ((carry 0) (i 0))
    (cond
      ((fx<? i n1)
       (with-carry ((s carry^) (+ (digit-ref src1* i) (digit-ref src2* i) carry))
         (digit-set! dst* i s)
         (lp carry^ (fx+ i 1))))
      (else
       (let lp ((carry carry) (i i))
         (cond
           ((fx<? i n2)
            (with-carry ((s carry^) (+ (digit-ref src1* i) carry 0))
              (digit-set! dst* i s)
              (lp carry^ (fx+ i 1))))
           (else
            (digit-set! dst* n2 carry))))))))

(define (digits-! dst* src1* src2* n1 n2)
  ;; dst[0:n2] = src1[0:n2]-src2[0:n1]
  (let lp ((carry 0) (i 0))
    (cond
      ((fx<? i n1)
       (with-carry ((s carry^) (- (digit-ref src1* i) (digit-ref src2* i) carry))
         (digit-set! dst* i s)
         (lp carry^ (fx+ i 1))))
      (else
       (let lp ((carry carry) (i i))
         (when (fx<? i n2)
           (with-carry ((s carry^) (- (digit-ref src1* i) carry 0))
             (digit-set! dst* i s)
             (lp carry^ (fx+ i 1)))))))))

(define (high+! dst src1 src2 n1 n2) ;;
  ;; XXX: dst may be eq? to one of the sources
  (cond ((int-sign=? src1 src2)
         (let ((n (fx+ (fxmax n1 n2) 1)))
           (grow! dst n)
           (int-sign-set! dst (int-sign src2))
           (let reorder ((src1 src1) (src2 src2) (n1 n1) (n2 n2))
             (if (fx>=? n1 n2)
                 (digits+! (int-digits dst) (int-digits src1) (int-digits src2) n2 n1)
                 (reorder src2 src1 n2 n1)))
           #;(clamp! dst)))
        ((eq? 'lt (cmp-magnitude src1 src2))   ; abs(src1) < abs(src2)
         (grow! dst n2)
         (int-sign-set! dst (int-sign src2))
         (digits-! (int-digits dst) (int-digits src2) (int-digits src1) n1 n2)
         #;(clamp! dst))
        (else
         (grow! dst n1)
         (int-sign-set! dst (int-sign src1))
         (digits-! (int-digits dst) (int-digits src1) (int-digits src2) n2 n1)
         #;(clamp! dst))))

;; Bignum subtraction
(define (high-! dst src1 src2 n1 n2)   ;;
  ;; XXX: dst may be eq? to one of the sources
  (cond ((not (int-sign=? src1 src2))
         (let ((n (fx+ (fxmax n1 n2) 1)))
           (grow! dst n)
           (int-sign-set! dst (int-sign src1))
           (let reorder ((src1 src1) (src2 src2) (n1 n1) (n2 n2))
             (if (fx>=? n1 n2)
                 (digits+! (int-digits dst) (int-digits src1) (int-digits src2) n2 n1)
                 (reorder src2 src1 n2 n1)))
           #;(clamp! dst)))
        ((not (eq? 'lt (cmp-magnitude src1 src2)))      ; abs(src1) >= abs(src2)
         (grow! dst n1)
         (int-sign-set! dst (int-sign src1))
         (digits-! (int-digits dst) (int-digits src1) (int-digits src2) n2 n1)
         #;(clamp! dst))
        (else
         (grow! dst n2)
         (int-sign-set! dst (fx- (int-sign src1)))
         (digits-! (int-digits dst) (int-digits src2) (int-digits src1) n1 n2)
         #;(clamp! dst))))

;; Bignum addition, properly abstracted
(define (bn+ a b)
  (let ((n1 (int-used a))
        (n2 (int-used b)))
    (let ((n (if (int-sign=? a b)
                 (fx+ 1 (fxmax n1 n2))
                 (fxmax n1 n2))))
      (let ((ret (make-int n 1)))
        (high+! ret a b n1 n2)
        (bnsimplify! ret)))))

;; Bignum subtraction, properly abstracted
(define (bn- a b)
  (let ((n1 (int-used a))
        (n2 (int-used b)))
    (let ((n (if (int-sign=? a b)
                 (fxmax n1 n2)
                 (fx+ 1 (fxmax n1 n2)))))
      (let ((ret (make-int n 1)))
        (high-! ret a b n1 n2)
        (bnsimplify! ret)))))

;; bitwise-arithmetic-shift-left int 1
(define (bnasl1 src)
  (let* ((n (int-used src))
         (dst (make-int (fx+ n 1) (int-sign src))))
    (let ((src* (int-digits src))
          (dst* (int-digits dst)))
      (let lp ((c 0) (i 0))
        (cond ((fx=? i n)
               (digit-set! dst* n c)
               (bnsimplify! dst))
              (else
               (let ((c* (fxasr (digit-ref src* i) (fx- (digit-width) 1)))
                     (s (fxior c (fxasl (fxand (digit-ref src* i)
                                               (fxasr (digit-mask) 1))
                                        1))))
                 (digit-set! dst* i s)
                 (lp c* (fx+ i 1)))))))))

;; bitwise-arithmetic-shift-right int 1
(define (bnasr1 src)
  (let ((dst (make-int (int-used src) (int-sign src))))
    (let ((src* (int-digits src))
          (dst* (int-digits dst)))
      (let lp ((c 0) (i (fx- (int-used src) 1)))
        (cond ((eqv? i -1)
               (bnsimplify! dst))
              (else
               (let ((c* (fxand (digit-ref src* i) 1))
                     (s (fxior (fxasr (digit-ref src* i) 1)
                               (fxasl c (fx- (digit-width) 1)))))
                 (digit-set! dst* i s)
                 (lp c* (fx- i 1)))))))))

;; bitwise-length for ints.
(define (bnlength int)
  (let ((u (int-used int)))
    (cond ((eqv? u 0)
           0)
          ((int-negative? int)
           (let ((int* (int-digits int)))
             (let lp ((carry 1) (i 0) (len 0))
               (let ((d (digit-ref int* i)))
                 (if (fx=? i (fx- u 1))
                     (let ((d^ (fx+ carry (fxnot d))))
                       (fx+ len (fxlength d^)))
                     (lp (if (eqv? d 0) carry 0)
                         (fx+ i 1)
                         (fx+ len (digit-width))))))))
          (else
           (fx+ (fx* (digit-width) (fx- u 1))
                (fxlength (digit-ref (int-digits int) (fx- u 1))))))))

;; Quick (expt 2 n) for non-negative n
(define (bignum-expt2 n sign)
  (let-values ([(d m) (fxdiv-and-mod n (digit-width))])
    (let ((int (make-int (fx+ d 1) sign)))
      (digit-set! (int-digits int) d (fxasl 1 m))
      int)))

(define (bnfirst-bit-set int)
  (let ((used (int-used int))
        (int* (int-digits int)))
    (let lp ((i 0))
      (if (fx=? used i)
          -1
          (let ((d (digit-ref int* i)))
            (if (eqv? d 0)
                (lp (fx+ i 1))
                (fx+ (fxfirst-bit-set d)
                     (fx* i (digit-width)))))))))

;; Quick bitwise-bit-set? for non-negative m
(define (bnbit-set? int n)
  (let-values ([(d m) (fxdiv-and-mod n (digit-width))])
    (cond ((fx<? d (int-used int))
           (fxbit-set? (digit-ref (int-digits int) d) m))
          (else (int-negative? int)))))

;; Shift n1 digits left by n digits
(define (int-shld! int n1 n)
  (assert (fx>=? n 0))
  (unless (eqv? n 0)
    (grow! int (fx+ n1 n))
    (let ((int* (int-digits int)))
      (digits-copy! int* n int* 0 n1)
      (do ((i 0 (fx+ i 1)))
          ((fx=? i n))
        (digit-set! int* i 0)))))

;; Shift n1 digits left by a bits
(define (int-shl! dst n1 a)
  (let-values ([(a-digits a-bits) (fxdiv-and-mod a (digit-width))])
    (let ((n2 (fx+ n1 a-digits)))
      (int-shld! dst n1 a-digits)
      (grow! dst (fx+ n2 1))
      (unless (eqv? a-bits 0)
        (let ((border (fx- (digit-width) a-bits))
              (dst* (int-digits dst)))
          (let* ((m1 (fxnot (fxasl -1 (digit-width))))
                 (m2 (fxnot (fxasl -1 border))))
            (let lp ((carry 0) (i 0))
              (if (fx<? i n2)
                  (let ((digit (digit-ref dst* i)))
                    (let ((carry^ (fxasr (fxand digit m1) border))
                          (s (fxior carry (fxasl (fxand digit m2) a-bits))))
                      (digit-set! dst* i s)
                      (lp carry^ (fx+ i 1))))
                  (digit-set! dst* n2 carry)))))))))

;; Shift right by n digits
(define (int-shrd! int n)
  (unless (fx<=? n 0)
    (cond ((fx<=? (int-used int) n)
           (int-used-set! int 0)
           (int-sign-set! int 1))
          (else
           (let ((int* (int-digits int)))
             (digits-copy! int* 0 int* n (int-used int))
             (int-used-set! int (fx- (int-used int) n)))))))

;; Shift right by n bits
(define (int-shr! dst n)
  (assert (fx>=? n 0))
  (unless (eqv? n 0)
    (let-values ([(n/w n) (fxdiv-and-mod n (digit-width))])
      (unless (eqv? n/w 0)
        (int-shrd! dst n/w))
      (unless (eqv? n 0)
        (let ((mask (fx- (fxasl 1 n) 1))
              (dst* (int-digits dst))
              (border (fx- (digit-width) n)))
          (let lp ((carry 0) (i (fx- (int-used dst) 1)))
            (unless (eqv? i -1)
              (let ((carry^ (fxand (digit-ref dst* i) mask))
                    (s (fxior (fxasr (digit-ref dst* i) n)
                              (fxasl carry border))))
                (digit-set! dst* i s)
                (lp carry^ (fx- i 1)))))))
      (clamp! dst))))

;; Multiply src by a single digit
(define (int-mul-digit! dst src digit)
  (let ((n (int-used src)))
    (grow! dst (fx+ n 1))
    (int-sign-set! dst (int-sign src))
    (let ((src* (int-digits src))
          (dst* (int-digits dst)))
      (let lp ((u 0) (ix 0))
        (cond
          ((fx<? ix n)
           (with-carry ((s u) (+ (* (digit-ref src* ix) digit) u 0))
             (digit-set! dst* ix s)
             (lp u (fx+ ix 1))))
          (else
           (digit-set! dst* n u)))))))

;; Bignum bitwise-arithmetic-shift-left
(define (bnasl n a)
  (let ((ret (make-int (fx+ (int-used n) (fx+ 1 (fxdiv a (digit-width))))
                       (int-sign n))))
    (copy! ret n)
    (int-shl! ret (int-used n) a)
    (bnsimplify! ret)))

;; Bignum bitwise-arithmetic-shift-right
(define (bnasr n a)
  (let ((ret (make-int (int-used n) (int-sign n))))
    (copy! ret n)
    (int-shr! ret a)
    (bnsimplify! ret)))

;; bitwise-bit-field returning a fixnum.
(define (bnbit-field1 int start end negative?)
  (define (fxbit-field n start end)
    (let ((mask (fxnot (fxarithmetic-shift-left -1 end))))
      (fxarithmetic-shift-right (fxand n mask) start)))
  (let-values ([(d s) (fxdiv-and-mod start (digit-width))])
    (let ((used (int-used int))
          (src* (int-digits int))
          (w (fx- end start)))
      (cond ((fx<? d used)
             (if (not negative?)
                 (let ((d0 (digit-ref src* d))
                       (e (fx+ s w)))
                   (cond
                     ((fx<=? e (digit-width))
                      (fxbit-field d0 s e))
                     ((fx<=? e (fx* 2 (digit-width)))
                      (let ((d1 (if (fx<? (fx+ d 1) used) (digit-ref src* (fx+ d 1)) 0)))
                        (let ((r0 (fxbit-field d0 s (digit-width)))
                              (r1 (fxbit-field d1 0 (fx- e (digit-width)))))
                          (fxior r0 (fxarithmetic-shift-left r1 (fx- (digit-width) s))))))
                     (else
                      (let ((d1 (if (fx<? (fx+ d 1) used) (digit-ref src* (fx+ d 1)) 0))
                            (d2 (if (fx<? (fx+ d 2) used) (digit-ref src* (fx+ d 2)) 0)))
                        (let ((r0 (fxbit-field d0 s (digit-width)))
                              (r1 d1)
                              (r2 (fxbit-field d2 0 (fx- e (fx* 2 (digit-width))))))
                          (fxior r0
                                 (fxarithmetic-shift-left r1 (fx- (digit-width) s))
                                 (fxarithmetic-shift-left r2 (fx- (fx* 2 (digit-width)) s))))))))
                 (let* ((d0 (digit-ref src* d))
                        (carry 1)
                        (d0 (fxand (fx+ carry (fxnot d0)) (digit-mask)))
                        (e (fx+ s w)))
                   (cond
                     ((fx<=? e (digit-width))
                      (fxbit-field d0 s e))
                     ((fx<=? e (fx* 2 (digit-width)))
                      (let* ((d1 (if (fx<? (fx+ d 1) used) (digit-ref src* (fx+ d 1)) 0))
                             (carry (if (eqv? d0 0) carry 0))
                             (d1 (fxand (fx+ carry (fxnot d1)) (digit-mask))))
                        (let ((r0 (fxbit-field d0 s (digit-width)))
                              (r1 (fxbit-field d1 0 (fx- e (digit-width)))))
                          (fxior r0 (fxarithmetic-shift-left r1 (fx- (digit-width) s))))))
                     (else
                      (let* ((d1 (if (fx<? (fx+ d 1) used) (digit-ref src* (fx+ d 1)) 0))
                             (carry (if (eqv? d0 0) carry 0))
                             (d1 (fxand (fx+ carry (fxnot d1)) (digit-mask)))
                             (d2 (if (fx<? (fx+ d 2) used) (digit-ref src* (fx+ d 2)) 0))
                             (carry (if (eqv? d1 0) carry 0))
                             (d2 (fxand (fx+ carry (fxnot d2)) (digit-mask))))
                        (let ((r0 (fxbit-field d0 s (digit-width)))
                              (r1 d1)
                              (r2 (fxbit-field d2 0 (fx- e (fx* 2 (digit-width))))))
                          (fxior r0
                                 (fxarithmetic-shift-left r1 (fx- (digit-width) s))
                                 (fxarithmetic-shift-left r2 (fx- (fx* 2 (digit-width)) s))))))))))
            (else
             (if (int-negative? int)
                 (fx- (fxarithmetic-shift-left 1 w) 1)
                 0))))))

;; Bignum multiplication by a digit
(define (bnfx* src1 src2)
  (let ((ret (make-int (fx+ (int-used src1) 1) (int-sign src1))))
    (int-mul-digit! ret src1 src2)
    (bnsimplify! ret)))

;; Bignum multiplication
(define (bn* src1 src2)
  ;; Compute (* (abs src1) (abs src2)) with a limited number of output
  ;; digits. Uses the O(n²) long-hand multiplication algorithm.
  (define (longhand*! src1 src2 digits)
    (let ((dst (make-int digits (if (int-sign=? src1 src2) 1 -1))))
      (let ((dst* (int-digits dst))
            (src1* (int-digits src1))
            (src2* (int-digits src2)))
        (do ((ix 0 (fx+ ix 1)))
            ((fx=? ix (int-used src1))
             dst)
          (let ((pb (fxmin (int-used src2) (fx- digits ix))))
            (when (fxpositive? pb)
              (let lp ((iy 0) (u 0))
                (cond ((fx=? iy pb)
                       (when (fx<? (fx+ ix pb) digits)
                         (digit-set! dst* (fx+ ix pb) u)))
                      (else
                       (let ((i (fx+ ix iy)))
                         (with-carry ((s0 u*) (+ (* (digit-ref src1* ix)
                                                    (digit-ref src2* iy))
                                                 (digit-ref dst* i)
                                                 u))
                           (digit-set! dst* i s0)
                           (lp (fx+ iy 1) u*))))))))))))
  ;; TODO: implement faster methods
  (bnsimplify! (longhand*! src1 src2 (fx+ (fx+ (int-used src1) (int-used src2)) 1))))

(define (bn² src)
  (define (int-square-baseline src)
    (let ((tmp (make-int (fx+ (fx* (int-used src) 2) 1) 1)))
      (let ((tmp* (int-digits tmp))
            (src* (int-digits src)))
        (do ((ix 0 (fx+ ix 1)))
            ((fx=? ix (int-used src))
             tmp)
          (with-carry ((s0 u) (+ (* (digit-ref src* ix)
                                    (digit-ref src* ix))
                                 (digit-ref tmp* (fx+ ix ix))
                                 0))
            (digit-set! tmp* (fx+ ix ix) s0)
            (let lp ((iy (fx+ ix 1))
                     (u u)
                     (i (fx+ (fx+ ix ix) 1)))
              (if (fx=? iy (int-used src))
                  (let lp* ((i i) (u u))
                    (unless (fxzero? u)
                      (with-carry ((l h) (+ (digit-ref tmp* i) u 0))
                        (digit-set! tmp* i l)
                        (lp* (fx+ i 1) h))))
                  (with-carry ((l0 h0) (+ (* (digit-ref src* ix)
                                             (digit-ref src* iy))
                                          0 0))
                    (with-carry ((l1 h1) (+ l0 l0 0))
                      (with-carry ((l2 h2) (+ l1 (digit-ref tmp* i) u))
                        (digit-set! tmp* i l2)
                        (lp (fx+ iy 1)
                            (fx+ (fx+ h0 h0) (fx+ h1 h2))
                            (fx+ i 1))))))))))))
  (let* ((len (bnlength src))
         (first-bit (bnfirst-bit-set src)))
    (if (fx=? len (fx+ first-bit 1))
        (bignum-expt2 (fx* first-bit 2) 1)
        (bnsimplify! (int-square-baseline src)))))

(define (copy src size sign name)
  ;; XXX: size must be at least large enough to hold src
  (let* ((n (int-used src))
         (dst (make-int size sign name)))
    (int-used-set! dst (int-used src))
    (do ((i 0 (fx+ i 1))
         (src* (int-digits src))
         (dst* (int-digits dst)))
        ((fx=? i n) dst)
      (digit-set! dst* i (digit-ref src* i)))))

(define (quo+rem a b)
  (define na (int-used a))
  (define nb (int-used b))
  (let ((q (make-int (fx+ na 2) (if (int-sign=? a b) 1 -1) 'q))
        (x (copy a (fx+ na 2) 1 'x))
        (y (copy b (fx+ nb (fx+ 2 (fxmax 0 (fx- na nb)))) 1 'y))
        (t1 (make-int 3 1 't1))
        (t2 (make-int 3 1 't2))
        (t3 (make-int (fx+ 2 (fx+ (int-used a) (int-used b))) 1 't3)))
    (let* ((q* (int-digits q))
           (t2* (int-digits t2))
           (norm (let ((w (fxmod (bnlength b) (digit-width))))
                   ;; Normalize the inputs
                   (cond ((fx<? w (- (digit-width) 1))
                          (let ((norm (fx- (- (digit-width) 1) w)))
                            (int-shl! x (int-used a) norm)
                            (int-shl! y (int-used b) norm)
                            (clamp! x)
                            (clamp! y)
                            norm))
                         (else 0))))
           (n (fx- (int-used x) 1))
           (t (fx- (int-used y) 1)))
      (int-shld! y (int-used y) (fx- n t))
      (let loop ()
        (when (cmp>= x y)
          (digit-set! q* (fx- n t) (fx+ (digit-ref q* (fx- n t)) 1))
          (high-! x x y (int-used x) (int-used y))
          (clamp! x)
          (loop)))
      (int-shrd! y (fx- n t))
      ;; Step 3
      (do ((i n (fx- i 1)))
          ((fx=? i t))
        (unless (fx>? i (int-used x))
          (let ((j (fx- (fx- i t) 1)))
            ;; Step 3.1
            (let ((x* (int-digits x))
                  (y* (int-digits y)))
              (cond ((fx=? (digit-ref x* i) (digit-ref y* t))
                     (digit-set! q* j (digit-mask)))
                    (else
                     (let ((tmp (fxdiv (fxior (fxasl (digit-ref x* i) (digit-width))
                                              (digit-ref x* (fx- i 1)))
                                       (digit-ref y* t))))
                       (digit-set! q* j (fxand tmp (digit-mask)))))))
            (digit-set! q* j (fxand (fx+ (digit-ref q* j) 1)
                                    (digit-mask)))
            (let loop ()
              (digit-set! q* j (fxand (fx+ (digit-ref q* j) -1)
                                      (digit-mask)))
              ;; Left hand
              (let ((t1* (int-digits t1))
                    (y* (int-digits y)))
                (digit-set! t1* 0 (if (fx<? (fx- t 1) 0) 0 (digit-ref y* (fx- t 1))))
                (digit-set! t1* 1 (digit-ref y* t))
                (int-used-set! t1 2)
                (int-mul-digit! t1 t1 (digit-ref q* j)))
              (clamp! t1)
              ;; Right hand
              (let ((x* (int-digits x)))
                (digit-set! t2* 0 (if (fx<? (fx- i 2) 0) 0 (digit-ref x* (fx- i 2))))
                (digit-set! t2* 1 (if (fx<? (fx- i 1) 0) 0 (digit-ref x* (fx- i 1))))
                (digit-set! t2* 2 (digit-ref x* i)))
              (when (eq? (cmp-magnitude t1 t2) 'gt)
                (loop)))
            ;; Step 3.3
            (int-mul-digit! t3 y (digit-ref q* j))
            (clamp! t3)
            (int-shld! t3 (int-used t3) j)
            (high-! x x t3 (int-used x) (int-used t3))
            (clamp! x)
            (when (int-negative? x)
              (copy! t3 y)
              (int-shld! t3 (int-used t3) j)
              (high+! x x t3 (int-used x) (int-used t3))
              (clamp! x)
              (digit-set! q* j (fxand (fx+ (digit-ref q* j) -1)
                                      (digit-mask)))))))
      ;; Normalize the outputs
      (int-sign-set! x (int-sign a))
      (int-shr! x norm)
      (clamp! q)
      (clamp! x)
      (values q x))))

(define (int-quo+rem a b)
  (let ((a (if (fixnum? a) (fixnum->int a) a))
        (b (if (fixnum? b) (fixnum->int b) b)))
    (cond
      ((int-zero? b)
       (assertion-violation 'int-quo+rem "Division by zero"
                            (bnsimplify! a) (bnsimplify! b)))
      ((eq? 'lt (cmp-magnitude a b))
       (values (fixnum->int 0) a))
      (else
       (quo+rem a b)))))

(define (bignum-quo+rem-digit src digit)
  ;; Divide src by a single non-zero digit. Returns quotient (as
  ;; int) and remainder (as fixnum). TODO: special case for 3.
  (let* ([q (make-int (int-used src) (int-sign src))]
         [q* (int-digits q)]
         [src* (int-digits src)])
    (let lp ([w^ 0]
             [ix (fx- (int-used src) 1)])
      (cond [(eqv? ix -1)
             (clamp! q)
             (values q (if (int-negative? src) (fx- w^) w^))]
            [else
             (let ([w^ (fx+ (fx* w^ (digit-radix)) (digit-ref src* ix))])
               (cond ((fx>=? w^ digit)
                      (let-values ([(t w^) (fxdiv-and-mod w^ digit)])
                        (digit-set! q* ix t)
                        (lp w^ (fx- ix 1))))
                     (else
                      (digit-set! q* ix 0)
                      (lp w^ (fx- ix 1)))))]))))

;; Some useful identities for dealing with negative operands:
;; a & b == ~(~a | ~b)
;; a & b == a - (a & ~b)
;; a ^ b == ~a ^ ~b
;; a ^ b == ~(a & ~b)
;; a | b == ~(~a & ~b)
;; a | b == (a & ~b) + b

(define (bnnot src)
  (let ((n (int-used src))
        (src* (int-digits src)))
    (cond
      ((int-negative? src)
       (let* ((dst (make-int n 1))
              (dst* (int-digits dst)))
         (let lp ((i 0))
           (cond
             ((fx=? i n)
              (error 'int-not! "Invalid int" src n))
             ((not (eqv? (digit-ref src* i) 0))
              ;; i is the first digit with a set bit somewhere
              (digit-set! dst* i (fx- (digit-ref src* i) 1))
              (digits-copy! dst* (fx+ i 1) src* (fx+ i 1) n)
              (bnsimplify! dst))
             (else
              (digit-set! dst* i (digit-mask))
              (lp (fx+ i 1)))))))
      (else
       (let lp ((i 0))
         (cond
           ((fx=? i n)
            ;; All digits have all bits set; -2^(n·r).
            (let* ((dst (make-int (fx+ n 1) -1))
                   (dst* (int-digits dst)))
              (digit-set! dst* n 1)
              (bnsimplify! dst)))
           ((not (eqv? (digit-ref src* i) (digit-mask)))
            ;; i is the first digit with a clear bit somewhere
            (let* ((dst (make-int n -1))
                   (dst* (int-digits dst)))
              (digit-set! dst* i (fx+ (digit-ref src* i) 1))
              (digits-copy! dst* (fx+ i 1) src* (fx+ i 1) n)
              (bnsimplify! dst)))
           (else
            (lp (fx+ i 1)))))))))

(define (digits-not-and! dst* src1* src2* n)
  ;; Do bitwise-and with the complement of src1's digits
  (let lp ((carry 1) (i 0))
    (when (fx<? i n)
      (let* ((d (digit-ref src1* i))
             (c (fxand (fx+ carry (fxnot d)) (digit-mask))))
        (digit-set! dst* i (fxand c (digit-ref src2* i)))
        (lp (if (eqv? d 0) carry 0)
            (fx+ i 1))))))

(define (digits-not-ior! dst* src1* src2* n)
  ;; Do bitwise-ior with the complement of src1's digits
  (let lp ((carry 1) (i 0))
    (if (fx<? i n)
        (let* ((d (digit-ref src1* i))
               (c (fxand (fx+ carry (fxnot d)) (digit-mask))))
          (digit-set! dst* i (fxior c (digit-ref src2* i)))
          (lp (if (eqv? d 0) carry 0)
              (fx+ i 1)))
        carry)))

(define (digits-not-carry! dst* src1* n1 n2 carry)
  ;; Copy the complement of src1's digits, taking in carry
  (let lp ((carry carry) (i n1))
    (when (fx<? i n2)
      (let* ((d (digit-ref src1* i))
             (c (fxand (fx+ carry (fxnot d)) (digit-mask))))
        (digit-set! dst* i c)
        (lp (if (eqv? d 0) carry 0)
            (fx+ i 1))))))

(define (digits-not-copy! dst* src* n1 n2)
  ;; Copy the complement of src1[n1:n2]
  (let lp ((carry 1) (i 0))
    (cond
      ((fx<? i n1)
       (let ((d (digit-ref src* i)))
         (digit-set! dst* i (fxand (fx+ carry (fxnot d)) (digit-mask)))
         (lp (if (eqv? d 0) carry 0)
             (fx+ i 1))))
      (else
       ;; Continue with zeros as source
       (let lp ((carry carry) (i i))
         (when (fx<? i n2)
           (let ((d 0))
             (digit-set! dst* i (fxand (fx+ carry (fxnot d)) (digit-mask)))
             (lp (if (eqv? d 0) carry 0)
                 (fx+ i 1)))))))))

(define (bnior src1 src2)
  (let reorder ((src1 src1) (src2 src2))
    (let ((n1 (int-used src1))
          (n2 (int-used src2))
          (src1* (int-digits src1))
          (src2* (int-digits src2)))
      (cond ((int-negative? src1)
             (cond ((and (int-negative? src2) (fx>=? n1 n2))
                    (let* ((dst (make-int n1 -1))
                           (dst* (int-digits dst)))
                      (digits-not-copy! dst* src2* n2 n1)
                      (let ((carry (digits-not-ior! dst* src1* dst* n1)))
                        (digits-not-carry! dst* src2* n1 n1 carry)
                        (digits-not-copy! dst* dst* n1 n1))
                      (bnsimplify! dst)))
                   (else
                    (reorder src2 src1))))
            ((int-negative? src2)
             (let* ((dst (make-int n2 -1))
                    (dst* (int-digits dst)))
               (cond ((fx>? n2 n1)
                      (let ((carry (digits-not-ior! dst* src2* src1* n1)))
                        (digits-not-carry! dst* src2* n1 n2 carry)
                        (digits-not-carry! dst* dst* 0 n2 1)))
                     (else
                      (digits-not-ior! dst* src2* src1* n2)
                      (digits-not-copy! dst* dst* n2 n2)))
               (bnsimplify! dst)))
            (else
             (let* ((n (fxmax n1 n2))
                    (dst (make-int n 1))
                    (dst* (int-digits dst)))
               (cond
                 ((fx>=? n1 n2)
                  (do ((i 0 (fx+ i 1))) ((fx=? i n2))
                    (digit-set! dst* i (fxior (digit-ref src1* i) (digit-ref src2* i))))
                  (do ((i n2 (fx+ i 1))) ((fx=? i n1))
                    (digit-set! dst* i (digit-ref src1* i))))
                 (else
                  (do ((i 0 (fx+ i 1))) ((fx=? i n1))
                    (digit-set! dst* i (fxior (digit-ref src1* i) (digit-ref src2* i))))
                  (do ((i n1 (fx+ i 1))) ((fx=? i n2))
                    (digit-set! dst* i (digit-ref src2* i)))))
               (bnsimplify! dst)))))))

(define (int-xor src1 src2)
  (define (do-positive min max x)
    (let ((src1* (int-digits src1))
          (src2* (int-digits src2))
          (x* (int-digits x))
          (dst (make-int max 1)))
      (let ((dst* (int-digits dst)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i min))
          (digit-set! dst* i (fxxor (digit-ref src1* i)
                                    (digit-ref src2* i))))
        (do ((i min (fx+ i 1)))
            ((fx=? i max))
          (digit-set! dst* i (digit-ref x* i)))
        (bnsimplify! dst))))
  (let reorder ((src1 src1) (src2 src2))
    (cond ((int-negative? src1)
           (if (int-negative? src2)
               (error 'bitwise-xor "TODO" src1 src2)
               (reorder src2 src1)))
          ((int-negative? src2)
           (error 'bitwise-xor "TODO" src1 src2))
          ((fx>? (int-used src1) (int-used src2))
           (do-positive (int-used src2) (int-used src1) src1))
          (else
           (do-positive (int-used src1) (int-used src2) src2)))))

(define (bnand src1 src2)
  (let reorder ((src1 src1) (src2 src2))
    (let ((n1 (int-used src1))
          (n2 (int-used src2))
          (src1* (int-digits src1))
          (src2* (int-digits src2)))
      (cond ((int-negative? src1)
             (if (and (int-negative? src2) (fx>=? n1 n2))
                 (let* ((n1+1 (fx+ n1 1))
                        (dst (make-int n1+1 -1))
                        (dst* (int-digits dst)))
                   (digits-not-copy! dst* src1* n1 n1+1)
                   (digits-not-and! dst* src2* dst* n2)
                   (digits-not-copy! dst* dst* n1+1 n1+1)
                   (bnsimplify! dst))
                 (reorder src2 src1)))
            ((int-negative? src2)
             ;; src1 is positive and src2 negative, so bits above n2
             ;; are preserved
             (let* ((dst (make-int n1 1))
                    (dst* (int-digits dst)))
               (cond ((fx<=? n1 n2)
                      (digits-not-and! dst* src2* src1* n1))
                     (else
                      (digits-not-and! dst* src2* src1* n2)
                      (digits-copy! dst* n2 src1* n2 n1)))
               (bnsimplify! dst)))
            (else
             (let* ((n (fxmin n1 n2))
                    (dst (make-int n 1))
                    (dst* (int-digits dst)))
               ;; TODO: can do fxand from the top and only allocate
               ;; the required number of digits.
               (do ((i 0 (fx+ i 1)))
                   ((fx=? i n))
                 (digit-set! dst* i (fxand (digit-ref src1* i) (digit-ref src2* i))))
               (bnsimplify! dst)))))))

;;; Floating point to string

;; An adaptation of "fp-free10.ss" until Dragon4 or something is
;; implemented in its place. This code originally carried the notice
;; below. It has been modified.

;; © 1996 Robert G. Burger. Permission is hereby granted, free of
;; charge, to any person obtaining a copy of this software, to deal in
;; the software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute,
;; sublicense, and/or sell copies of the software.

;; The software is provided "as is," without warranty of any kind,
;; express or implied, including but not limited to the warranties of
;; merchantability, fitness for a particular purpose and
;; noninfringement. In no event shall the author be liable for any
;; claim, damages or other liability, whether in an action of
;; contract, tort or otherwise, arising from, out of or in connection
;; with the software or the use or other dealings in the software.


;; Free-format algorithm for printing IEEE double-precision positive
;; floating-point numbers in base 10

;; It uses the floating-point logarithm to estimate the scaling factor
;; and a table to look up powers of ten.

;; Input to flonum->digits:
;;       v -- a positive floating-point number, f x 2^e
;;       f -- mantissa of v
;;       e -- exponent of v

;; Output: (k d_1 d_2 ... d_n),
;;   where 0.d_1...d_n x 10^k is the shortest correctly rounded base-10
;;   number that rounds to v when input (it assumes the input
;;   routine rounds to even)

;; See also "Printing Floating-Point Numbers Quickly and Accurately"
;; in Proceedings of the SIGPLAN '96 Conference on Programming Language
;; Design and Implementation.

;; Author: Bob Burger  Date: March 1996

(define flonum->digits
  (let ([min-e -1074]
        [bp-1 (expt 2 52)])
    (lambda (v f e)
      (let ([round? (even? f)])
        (if (>= e 0)
            (if (not (= f bp-1))
                (let ([be (expt 2 e)])
                  (scale (* f be 2) 2 be be 0 round? round? v))
                (let ([be (expt 2 e)])
                  (scale (* f be 4) 4 (* be 2) be 0 round? round? v)))
            (if (or (= e min-e) (not (= f bp-1)))
                (scale (* f 2) (expt 2 (- 1 e)) 1 1 0 round? round? v)
                (scale (* f 4) (expt 2 (- 2 e)) 2 1 0 round? round? v)))))))

(define scale
  (lambda (r s m+ m- k low-ok? high-ok? v)
    (let ([est (exact (ceiling (- (log10 v) 1e-10)))])
      (if (>= est 0)
          (fixup r (* s (expt10 est)) m+ m- est low-ok? high-ok?)
          (let ([scale (expt10 (- est))])
            (fixup (* r scale) s (* m+ scale) (* m- scale)
                   est low-ok? high-ok?))))))

(define fixup
  (lambda (r s m+ m- k low-ok? high-ok?)
    (if ((if high-ok? >= >) (+ r m+) s) ; too low?
        (cons (+ k 1) (generate r s m+ m- low-ok? high-ok?))
        (cons k
              (generate (* r 10) s (* m+ 10) (* m- 10) low-ok? high-ok?)))))

(define generate
  (lambda (r s m+ m- low-ok? high-ok?)
    (let ([d (quotient r s)]
          [r (remainder r s)])
      (let ([tc1 ((if low-ok? <= <) r m-)]
            [tc2 ((if high-ok? >= >) (+ r m+) s)])
        (if (not tc1)
            (if (not tc2)
                (cons d (generate (* r 10) s (* m+ 10) (* m- 10)
                                  low-ok? high-ok?))
                (list (+ d 1)))
            (if (not tc2)
                (list d)
                (if (< (* r 2) s)
                    (list d)
                    (list (+ d 1)))))))))

(define expt10
  (let-syntax ((expt10-table
                (lambda (x)
                  (syntax-case x ()
                    ((_)
                     (let ([table (make-vector 326)])
                       (do ([k 0 (sys:+ k 1)]
                            [v 1 (sys:* v 10)])
                           ((sys:= k 326))
                         (vector-set! table k v))
                       (with-syntax ((t table))
                         #''t)))))))
    (let ([table (expt10-table)])
      (lambda (k)
        (vector-ref table k)))))

(define log10
  (lambda (x)
    (* (log x) (/ (log 10))))))
