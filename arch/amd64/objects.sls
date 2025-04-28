;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; amd64 object serialization, data layout, etc.

(library (loko arch amd64 objects)
  (export
    car-offset cdr-offset
    tag mask shift btag box-header-type-eq-mask box-header
    immediate
    encode-object
    amd64-fixnum-width
    amd64-least-fixnum
    amd64-greatest-fixnum)
  (import
    (rnrs (6))
    (loko arch amd64 tagging)
    (only (loko runtime utils) map-in-order string-hash* string-index)
    (only (loko compiler recordize) const?
          const-value const-ref set-const-ref!

          artd? artd-name artd-parent artd-uid artd-sealed? artd-opaque? artd-fields
          arcd? arcd-rtd arcd-parent-rcd arcd-protocol)
    (only (loko compiler compat) gensym? gensym->unique-string gensym-prefix))

(define-syntax define-const
  (syntax-rules ()
    ((_ (name) . body)
     (define-syntax name
       (identifier-syntax (lambda () . body))))))

(define-const (car-offset) 0)

(define-const (cdr-offset) 8)

(define rtd:parents-start 7)            ;base offset of any parent rtds
(define rtd:min-parents 4)              ;always store at least four parents

(define number:exact?   #b00000001)
(define number:real?    #b00000010)
(define number:polar?   #b00000100)
(define number:compnum? #b00001000)

(define-const (amd64-fixnum-width)
  (- 64 (shift 'fixnum)))

(define-const (amd64-least-fixnum)
  (- (bitwise-arithmetic-shift-left 1 (- (amd64-fixnum-width) 1))))

(define-const (amd64-greatest-fixnum)
  (- (bitwise-arithmetic-shift-left 1 (- (amd64-fixnum-width) 1)) 1))

;; bignum
(define-const (amd64-int-width) 30)
(define-const (amd64-int^-width)
  ;; must not be larger than (- (fixnum-width) 1)
  (fx+ (fx* (amd64-int-width) 2) 1))
(define-const (amd64-int-mask)
  (define (amd64-int-radix)
    (fxarithmetic-shift-left 1 (amd64-int-width)))
  (fx- (amd64-int-radix) 1))

;; Returns m and t suitable for (x&m)==t, optionally including a test
;; for the embedded value.
(define (box-header-type-eq-mask type value-mask value-test)
  (values (fxior (if (eq? type 'number)
                     (fxarithmetic-shift-left (mask 'box-header:type-number?)
                                              (shift 'box-header:type-number?))
                     (fxarithmetic-shift-left (mask 'box-header:type)
                                              (shift 'box-header:type)))
                 (fxarithmetic-shift-left (fxand (mask 'box-header:value)
                                                 value-mask)
                                          (shift 'box-header:value))
                 (mask 'box-header))
          (fxior (if (eq? type 'number)
                     (fxarithmetic-shift-left (mask 'box-header:type-number?)
                                              (shift 'box-header:type-number?))
                     (fxarithmetic-shift-left (btag type)
                                              (shift 'box-header:type)))
                 (fxarithmetic-shift-left (fxand (mask 'box-header:value)
                                                 value-test)
                                          (shift 'box-header:value))
                 (tag 'box-header))))

(define-record-type rcd
  (sealed #t)
  (nongenerative loko-rcd-3ca6464b-5f42-47e1-87e6-19a970f345aa)
  (fields rtd parent-rcd protocol))

;;; Object serialization.

(define-syntax immediate
  (make-variable-transformer
   (lambda (x)
     (syntax-case x ()
       ((_ v)
        (let ((v (syntax->datum #'v)))
          (and (number? v) (exact? v) (integer? v)
               (<= (amd64-least-fixnum) v (amd64-greatest-fixnum))))
        #'(bitwise-arithmetic-shift-left v (shift 'fixnum)))
       ((_ v)
        (char? (syntax->datum #'v))
        #'(bitwise-ior (tag 'char)
                       (bitwise-arithmetic-shift-left (char->integer v)
                                                      (shift 'char))))
       ((_ v)
        (boolean? (syntax->datum #'v))
        #'(bitwise-ior (tag 'boolean)
                       (bitwise-arithmetic-shift-left (if v 1 0)
                                                      (shift 'boolean))))
       ((_ (q v))
        (and (eq? 'quote (syntax->datum #'q))
             (null? (syntax->datum #'v)))
        #'(tag 'null))

       ((_ . rest) #'(immediate* . rest))
       (_ #'immediate*)))))

;; This encodes an immediate object as a bitpattern that can be
;; stored in a register or in memory. It is also OK to use this as
;; an immediate operand in an assembler instruction, but beware that
;; it might need to be loaded with mov if it's large.
(define (immediate* x)
  (cond ((number? x)
         (cond ((and (exact? x) (integer? x)
                     (<= (amd64-least-fixnum) x (amd64-greatest-fixnum)))
                (bitwise-arithmetic-shift-left x (shift 'fixnum)))
               ((and (inexact? x) (real? x))
                (let ((bv (make-bytevector 4)))
                  (bytevector-ieee-single-set! bv 0 x (endianness little))
                  (let ((b (bytevector-u32-ref bv 0 (endianness little))))
                    (bitwise-ior (tag 'flonum)
                                 (bitwise-arithmetic-shift-left b (shift 'flonum))))))
               (else #f)))
        ((and (symbol? x) (not (gensym? x)))
         (let ((str (symbol->string x)))
           (encode-immsym str)))
        ((char? x)
         (bitwise-ior (tag 'char)
                      (bitwise-arithmetic-shift-left (char->integer x)
                                                     (shift 'char))))
        ((boolean? x)
         (bitwise-ior (tag 'boolean)
                      (bitwise-arithmetic-shift-left (if x 1 0)
                                                     (shift 'boolean))))
        ((null? x) (tag 'null))
        ((eof-object? x) (tag 'eof-object))
        ((eqv? x (if #f #f)) (tag 'void))
        (else #f)))

(define (encode-object x objs strings symbols gensyms bytevectors rtds
                       gensym-idx-locations emit)
  (define-syntax with-interning-table
    (lambda (x)
      (syntax-case x ()
        ((_ (table value) body ...)
         #'(cond ((hashtable-ref table value #f))
                 (else
                  (let ((ref (begin body ...)))
                    (hashtable-set! table value ref)
                    ref)))))))
  (define (genref v type-tag)
    `(+ #(const ,v) ,type-tag))
  (define (generate type-tag v . xs)      ;generate assembler
    (define (wrap f)
      (if (bytevector? f)
          (emit `(%vu8 ,f))
          (emit `(%u64 ,f))))
    (let ((ref (genref v type-tag)))
      (emit `(%align 8 0))
      (emit `(%label ,(cadr ref)))
      (for-each wrap xs)
      ref))

  (define (encode v)
    (cond
      ((immediate v))

      ((pair? v)
       (with-interning-table (objs v)
         (if (< (car-offset) (cdr-offset))
             (let* ((head (encode (car v)))
                    (tail (encode (cdr v))))
               (generate (tag 'pair) v head tail))
             (let* ((tail (encode (cdr v)))
                    (head (encode (car v))))
               (generate (tag 'pair) v tail head)))))

      ((vector? v)
       (with-interning-table (objs v)
         (apply generate (tag 'vector) v (immediate (vector-length v))
                (map-in-order encode (vector->list v)))))

      ((and (bytevector? v) (< (bytevector-length v) (expt 2 32))) ;conservative
       (with-interning-table (bytevectors v)
         ;; TODO: Can the seek-mark be replaced by a box header?
         (generate (tag 'bytevector) v (immediate (bytevector-length v))
                   (seek-mark (bytevector-length v))
                   v)))
      ((string? v)
       (with-interning-table (strings v)
         (generate (tag 'string) v (immediate (string-length v))
                   (uint-list->bytevector
                    (map immediate (string->list v))
                    (endianness little) 4))))

      ((gensym? v)
       ;; Gensyms retain their identity and their unique string.
       (with-interning-table (gensyms v)
         (let* ((name (symbol->string v))
                (unique (gensym->unique-string v))
                (hash (string-hash* unique))
                (ret (generate (tag 'box) v (box-header 'symbol #t #b111 4)
                               (encode name)
                               (immediate hash)
                               (encode (string->utf8 unique)))))
           ;; This makes the assembler line for the gensym index
           ;; fields available. It is mutated by the code generator!
           (let ((gensym-env-index (list '%u64 (immediate #f))))
             (hashtable-set! gensym-idx-locations v gensym-env-index)
             (emit gensym-env-index))
           ret)))

      ((symbol? v)
       (with-interning-table (symbols v)
         (let* ((name (symbol->string v))
                (hash (string-hash* name)))
           (generate (tag 'box) v (box-header 'symbol #t #b100 2)
                     (encode name)
                     (immediate hash)))))

      ((and (integer? v) (exact? v)
            ;; XXX: is this just too large?
            (< (bitwise-length v) (fxarithmetic-shift-left 1 20)))
       (with-interning-table (objs v)
         (do ((sign (if (negative? v) -1 1))
              (v (abs v) (bitwise-arithmetic-shift-right v (amd64-int-width)))
              (digits '() (cons (bitwise-and v (amd64-int-mask)) digits)))
             ((eqv? v 0)
              (generate (tag 'box) v (box-header 'bignum #t (fxior number:exact? number:real?) 3)
                        (immediate (length digits))
                        (immediate sign)
                        (encode (list->vector (reverse digits))))))))

      ((and (real? v) (inexact? v))
       (with-interning-table (objs v)
         (let ((bv (make-bytevector 8)))
           (bytevector-ieee-double-set! bv 0 v (endianness little))
           (generate (tag 'box) v (box-header 'dflonum #f number:real? 1)
                     (bytevector-u64-ref bv 0 (endianness little))))))

      ((and (rational? v) (exact? v))
       (with-interning-table (objs v)
         ;; workaround for Ikarus bug #831582
         (let ((v (/ (+ v v) 2)))
           (cond ((= (denominator v) 1)
                  (encode (numerator v)))
                 ((and (rational? v) (exact? v))
                  (generate (tag 'box) v
                            (box-header 'ratnum #t (fxior number:real? number:exact?) 2)
                            (encode (numerator v))
                            (encode (denominator v))))
                 (else
                  (encode v))))))

      ((complex? v)
       (with-interning-table (objs v)
         ;; TODO: when under self-hosting, make it check for complex
         ;; numbers in polar representation.
         (generate (tag 'box) v
                   (box-header 'rcompnum #t
                               (if (exact? v)
                                   (fxior number:compnum? number:exact?)
                                   number:compnum?)
                               2)
                   (encode (real-part v))
                   (encode (imag-part v)))))

      ;; This fallthrough can serialize any non-opaque record.
      ;; Everything that needs special handling goes above this point.

      ((record-type-descriptor? v)
       (let ((rtd v))
         (define (rtd-parents parent-rtd)
           (let lp ((rtd parent-rtd) (ret '()))
             (if (not rtd)
                 ret
                 (lp (record-type-parent rtd) (cons rtd ret)))))
         (with-interning-table (rtds rtd)
           ;; XXX: Two different sizes are encoded: the size of the
           ;; rtd itself and the size of the records.
           (let* ((parents (rtd-parents (record-type-parent rtd)))
                  (flags (fxior (if (record-type-opaque? rtd)     #b001 0)
                                (if (record-type-sealed? rtd)     #b010 0)
                                (if (record-type-generative? rtd) #b100 0)))
                  (padded-len (fx+ rtd:parents-start
                                   (fxmax rtd:min-parents (length parents))))
                  (record-size (let lp ((t rtd))
                                 (if (not t)
                                     0
                                     (fx+ (lp (record-type-parent t))
                                          (vector-length (record-type-field-names t))))))
                  (mutable (do ((fields (record-type-field-names rtd))
                                (i 0 (fx+ i 1))
                                (mask 0 (if (record-field-mutable? rtd i)
                                            (bitwise-ior mask (bitwise-arithmetic-shift-left 1 i))
                                            mask)))
                               ((fx=? i (vector-length fields))
                                mask))))
             (apply generate (tag 'box) rtd
                    (box-header 'rtd #t flags padded-len)
                    (encode (record-type-name rtd))
                    (immediate record-size)
                    (encode (record-type-parent rtd))
                    (encode (record-type-uid rtd))
                    (encode (record-type-field-names rtd))
                    (encode mutable)
                    (immediate 'immutable)
                    (append (map encode parents)
                            (vector->list
                             (make-vector (fxmax 0 (fx- rtd:min-parents (length parents)))
                                          (immediate 0)))))))))

      ((record? v)
       (with-interning-table (objs v)
         (let* ((v (cond ((hashtable? v)
                          (vector-for-each
                           (lambda (k)
                             ;; XXX: hashtable's keys must be
                             ;; unaffected by giving them a new
                             ;; address in the image.
                             (unless (or (symbol? k) (fixnum? k))
                               (assertion-violation 'encode-object
                                                    "Refusing to encode this hashtable"
                                                    v)))
                           (hashtable-keys v))
                          (hashtable-copy v))
                         (else v)))
                ;; XXX: gives an &assertion if it's an opaque record
                (rtd (record-rtd v)))
           (apply generate (tag 'box) v
                  (encode rtd)
                  (let lp ((t rtd))
                    (if (not t)
                        '()
                        (append (lp (record-type-parent t))
                                (do ((i 0 (fx+ i 1))
                                     (fields (record-type-field-names t))
                                     (vals '() (cons (encode ((record-accessor t i) v)) vals)))
                                    ((fx=? i (vector-length fields))
                                     (reverse vals))))))))))

      (else
       (error 'encode-object
              "This object cannot be encoded on amd64" v))))

  (cond ((const-ref x))
        (else
         (let ((ref (encode (const-value x))))
           (set-const-ref! x ref)
           ref))))

;;; 5-bit immediate symbols

(define alphabet     "abcdefghijklmnopqrstuvwxyz-/<=>")
(define end-alphabet "acdefghklmnopqrstvxy!*+-/08<=>?")

(define (encode-immsym s)
  (define (list->immsym l)
    ;; Takes a list of up to 12 5-bit integers and encodes it as a
    ;; 64-bit integer. The lower four bits are left as zero. If the list
    ;; is shorter than 12 it is padded with zeros.
    (do ((l (reverse l) (cdr l))
         (c 0 (bitwise-ior (car l) (bitwise-arithmetic-shift-left c 5))))
        ((null? l)
         (bitwise-ior (tag 'immsym)
                      (bitwise-and #xffffffffffffffff
                                   (bitwise-arithmetic-shift-left c (shift 'immsym)))))))
  (let ((l (reverse (string->list s))))
    (and (<= 1 (string-length s) 12)
         (string-index end-alphabet (car l))
         (for-all (lambda (c) (string-index alphabet c))
                  (cdr l))
         (list->immsym
          (reverse
           (cons 0 (cons (+ 1 (string-index end-alphabet (car l)))
                         (map (lambda (c) (+ 1 (string-index alphabet c)))
                              (cdr l)))))))))

#;(define (decode-immsym i)
    (define (immsym->list c)
      (do ((c (bitwise-arithmetic-shift-right c (shift 'immsym))
              (bitwise-arithmetic-shift-right c 5))
           (l '() (cons (bitwise-and c 31) l)))
          ((zero? c) (reverse l))))
    (define (symbol x)
      (let ((points (if (memv 0 x) (cdr (memv 0 x)) x)))
        (list->string
         (reverse
          (cons (string-ref end-alphabet (- (car points) 1))
                (map (lambda (p) (string-ref alphabet (- p 1)))
                     (cdr points)))))))
    (define (gensym id)
      (string-append "$gensym$" (string-reverse (number->string id 16))))
    (let ((x (immsym->list i)))
      (display x) (newline)
      (if (zero? (car x))
          (gensym (bitwise-arithmetic-shift-right i (+ 5 (shift 'immsym))))
          (symbol (reverse x)))))

;; (immsym->list (list->immsym '(1 2 3 4 5 6 7 8 0 2)))
;; => (1 2 3 4 5 6 7 8 0 2)
;; (immsym->list (list->immsym '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
;; => (1 2 3 4 5 6 7 8 9 10 11 12)

#;(define (encode-gensym id)
    ;; This is an immsym of length zero.
    (assert (<= 0 id (- (bitwise-arithmetic-shift-left 1 (- 64 (shift 'immsym) 5)) 1)))
    (bitwise-and #xffffffffffffffff
                 (bitwise-ior (tag 'immsym)
                              (bitwise-arithmetic-shift-left id (+ 5 (shift 'immsym))))))

;; (decode-immsym (encode-gensym #xffffffffffff))
;; (decode-immsym (encode-gensym #x3ffffffffffff))
;; (decode-immsym (encode-gensym #x7fffffffffffff))
;; (decode-immsym (encode-gensym #xCBA))
;; (decode-immsym (encode-immsym "abc?"))

)
