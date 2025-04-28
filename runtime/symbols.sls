;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Standard library for symbols

(library (loko runtime symbols)
  (export
    symbol? symbol->string symbol=? string->symbol
    symbol-ref symbol-length

    symbol-hash $symbol-hash

    gensym gensym? gensym->unique-string gensym-prefix
    *unbound-hack*
    symbol-value set-symbol-value!
    $gensym-generate-names!)
  (import
    (except (rnrs) symbol? symbol->string symbol=? string->symbol symbol-hash)
    (prefix (rnrs) sys:)
    (rnrs mutable-strings)
    (only (loko runtime utils) string-hash* string-index)
    (loko runtime context)
    (loko system $primitives))

(define *unbound-hack* (vector 'unbound))

;; Slots in the symbol object. The box header value bit 0 is 1 for
;; gensyms from the bootstrap. The bit 1 is 1 for gensyms. Bit 2 is 1
;; for when the string is immutable. The hash is the string-hash* of
;; the name for regular symbols and the string-hash* of the unique
;; string for gensyms.
(define SYMBOL-NAME      0)
(define SYMBOL-HASH      1)
(define SYMBOL-UNIQUE    2)             ;gensyms only
(define SYMBOL-VALUE/IDX 3)             ;gemsyms only

(define LENGTH-SYMBOL 2)
(define LENGTH-GENSYM 4)

(define (symbol? x)
  (or ($immsym? x)
      (and ($box? x)
           ($box-header-type-eq? ($box-type x) 'symbol))))

(define (symbol->string v)
  (define alphabet     "abcdefghijklmnopqrstuvwxyz-/<=>")
  (define end-alphabet "acdefghklmnopqrstvxy!*+-/08<=>?")
  (cond
    (($immsym? v)
     (let* ((s ($immsym->fixnum v))
            (len (fxdiv (fx+ (fxlength s) 4) 5))
            (str (make-string len)))
       (if (eqv? len 0)
           str
           (let lp ((s s) (i 0))
             (let ((s (fxarithmetic-shift-right s 5))
                   (c (fx- (fxand s #b11111) 1)))
               (cond ((eqv? s 0)
                      (string-set! str i (string-ref end-alphabet c))
                      str)
                     (else
                      (string-set! str i (string-ref alphabet c))
                      (lp s (fx+ i 1)))))))))
    ((and ($box? v)
          ($box-header-type-eq? ($box-type v) 'symbol))
     (when (and (gensym? v) (not ($box-ref v SYMBOL-NAME)))
       ($gensym-generate-names! v))
     ;; Pass through if it's immutable; otherwise copy it.
     (if (eqv? #b100 (fxand #b100 ($box-header-value ($box-type v))))
         ($box-ref v SYMBOL-NAME)
         (string-copy ($box-ref v SYMBOL-NAME))))
    (else
     (assertion-violation 'symbol->string "Expected a symbol" v))))

(define (symbol-ref v k)
  (define alphabet     "abcdefghijklmnopqrstuvwxyz-/<=>")
  (define end-alphabet "acdefghklmnopqrstvxy!*+-/08<=>?")
  (unless (and (fixnum? k) (fx>=? k 0))
    (assertion-violation 'string-ref "Expected a non-negative fixnum" (symbol->string v) k))
  (cond
    (($immsym? v)
     (let ((len (fxdiv (fx+ 4 (fxlength ($immsym->fixnum v))) 5)))
       (unless (fx<? k len)
         (assertion-violation 'string-ref "The string index is out of range"
                              (symbol->string v) k)))
     (let* ((s (fxarithmetic-shift-right ($immsym->fixnum v) (fx* 5 k)))
            (c (fx- (fxand s #b11111) 1))
            (s^ (fxarithmetic-shift-right s 5)))
       (if (eqv? s^ 0)
           (string-ref end-alphabet c)
           (string-ref alphabet c))))
    ((and ($box? v)
          ($box-header-type-eq? ($box-type v) 'symbol))
     (when (and (gensym? v) (not ($box-ref v SYMBOL-NAME)))
       ($gensym-generate-names! v))
     (string-ref ($box-ref v SYMBOL-NAME) k))
    (else
     (assertion-violation 'symbol->string "Expected a symbol" v))))

(define (symbol-length v)
  (cond
    (($immsym? v)
     (fxdiv (fx+ 4 (fxlength ($immsym->fixnum v))) 5))
    ((and ($box? v)
          ($box-header-type-eq? ($box-type v) 'symbol))
     (when (and (gensym? v) (not ($box-ref v SYMBOL-NAME)))
       ($gensym-generate-names! v))
     (string-length ($box-ref v SYMBOL-NAME)))
    (else
     (assertion-violation 'symbol->string "Expected a symbol" v))))

(define (symbol=? x y . x*)
  (unless (and (symbol? x) (symbol? y))
    (apply assertion-violation 'symbol=? "Expected symbols" x y x*))
  (let lp ((x* x*) (ret (eq? x y)))
    (if (null? x*)
        ret
        (if (symbol? (car x*))
            (lp (cdr x*) (and ret (eq? x (car x*))))
            (apply assertion-violation 'symbol=? "Expected symbols" x y x*)))))

(define *interned* (make-hashtable string-hash* string=?))

(define (string->symbol s)
  (define alphabet     "abcdefghijklmnopqrstuvwxyz-/<=>")
  (define end-alphabet "acdefghklmnopqrstvxy!*+-/08<=>?")
  (define (alphabet-index c)
    (if (fx<=? (char->integer #\a) (char->integer c) (char->integer #\z))
        (fx- (char->integer c) (char->integer #\a))
        (case c
          [(#\-) 26]
          [(#\/) 27]
          [(#\<) 28]
          [(#\=) 29]
          [(#\>) 30]
          [else #f])))
  (define (immsym-encode s)
    (let ((len (string-length s)))
      (and (fx<=? 1 len 12)
           (let ((ret (string-index end-alphabet (string-ref s (fx- len 1)))))
             (and ret
                  (let lp ((ret (fx+ ret 1)) (i (fx- len 2)))
                    (if (eqv? i -1)
                        ($fixnum->immsym ret)
                        (let ((j (alphabet-index (string-ref s i))))
                          (and j
                               (lp (fxior (fxarithmetic-shift-left ret 5)
                                          (fx+ j 1))
                                   (fx- i 1)))))))))))
  (define (find-bootstrap-symbol str)
    (let ((G/V ($bootstrap-symbols)))
      (let ((G (car G/V)) (V (cdr G/V)))
        (let* ((d (vector-ref G (fxmod (string-hash* str) (vector-length G))))
               (idx (if (fxnegative? d)
                        (fx- -1 d)
                        (fxmod (string-hash* str d)
                               (vector-length V))))
               (sym (vector-ref V idx)))
          (and (string=? ($box-ref sym SYMBOL-NAME) str)
               sym)))))
  (unless (string? s)
    (assertion-violation 'string->symbol "Expected a string" s))
  (or (immsym-encode s)
      (find-bootstrap-symbol s)
      (hashtable-ref *interned* s #f)
      (let* ((hash (string-hash* s))
             (s^ (string-copy s))       ;TODO: make immutable
             (sym ($box ($make-box-header 'symbol #t #b000 LENGTH-SYMBOL)
                        s^ hash)))
        (hashtable-set! *interned* s^ sym)
        sym)))

(define ($symbol-hash s)
  (cond (($immsym? s)
         ($immsym->fixnum s))
        (else
         (when (not ($box-ref s SYMBOL-HASH))
           ($gensym-generate-names! s))
         ($box-ref s SYMBOL-HASH))))

(define (symbol-hash s)
  (unless (symbol? s)
    (assertion-violation 'symbol-hash "Expected a symbol" s))
  ($symbol-hash s))

;;; gensyms
;; TODO: collect all the gensym related stuff here

(define (gensym? x)
  ;; If the size field of the box is not two, then there's a
  ;; unique-string there. And maybe a value? And maybe an index into
  ;; the top level environment?
  (and ($box? x)
       (let ((t ($box-type x)))
         ($box-header-type-eq? t 'symbol #b10 #b10))))

(define (gensym->unique-string v)
  (assert (gensym? v))
  (when (not ($box-ref v SYMBOL-UNIQUE))
    ($gensym-generate-names! v))
  (utf8->string ($box-ref v SYMBOL-UNIQUE)))

(define (gensym-prefix v)
  (assert (gensym? v))
  (when (not ($box-ref v SYMBOL-NAME))
    ($gensym-generate-names! v))
  (string-copy ($box-ref v SYMBOL-NAME)))

(define gensym
  (case-lambda
    (()
     (gensym *unbound-hack*))
    ((prefix)
     (let ((p (cond ((eq? prefix *unbound-hack*) #f)
                    ((symbol? prefix) (symbol->string prefix))
                    ((string? prefix) (string-copy prefix))
                    (else
                     (error 'gensym "This procedure needs a string or a symbol"
                            prefix)))))
       (let ((name p)
             (hash #f)                  ;generated later
             (unique #f)                ;generated later
             (value/idx *unbound-hack*))
         ($box ($make-box-header 'symbol #t #b010 LENGTH-GENSYM)
               name hash unique value/idx))))))

(define (gensym-from-bootstrap? symbol)
  (eqv? 1 (fxand 1 ($box-header-value ($box-type symbol)))))

;; Unique strings for gensyms generated lazily.
(define $gensym-generate-names!
  (let ((g-count -1)
        (id-count -1))            ;TODO: should be some kind of UUID
    (lambda (g)
      (assert (gensym? g))
      (unless ($box-ref g SYMBOL-NAME)
        (set! g-count (+ g-count 1))
        ($box-set! g SYMBOL-NAME (string-append "g" (number->string g-count))))
      (unless ($box-ref g SYMBOL-UNIQUE)
        (set! id-count (+ id-count 1))
        (let* ((str (string-append "u" (number->string id-count)))
               (hash* (string-hash* str)))
          ($box-set! g SYMBOL-UNIQUE (string->utf8 str))
          ($box-set! g SYMBOL-HASH hash*)))
      (unless ($box-ref g SYMBOL-HASH)
        (let* ((str (utf8->string ($box-ref g SYMBOL-UNIQUE)))
               (hash* (string-hash* str)))
          ($box-set! g SYMBOL-HASH hash*))))))

(define (set-symbol-value! symbol value)
  (define (gensym-not-from-bootstrap? symbol)
    (and ($box? symbol)
         (let ((t ($box-type symbol)))
           (and (eqv? ($box-header-length t) LENGTH-GENSYM)
                ($box-header-type-eq? t 'symbol #b1 #b0)))))
  (cond ((gensym-not-from-bootstrap? symbol)
         ($box-set! symbol SYMBOL-VALUE/IDX value))
        (else
         (let ((idx ($box-ref symbol SYMBOL-VALUE/IDX)))
           (error 'set-symbol-value! "TODO: Set a bootstrap symbol" symbol value idx)))))

(define (symbol-value symbol)
  (cond ((gensym? symbol)
         (cond ((gensym-from-bootstrap? symbol)
                ;; A bootstrap gensym contains an index into the
                ;; process vector where the symbol's value is
                ;; stored. This is because they are shared between
                ;; all processes.
                (let ((idx ($box-ref symbol SYMBOL-VALUE/IDX)))
                  (if (fixnum? idx)
                      ($pcb-ref idx)
                      (raise (condition (make-undefined-violation)
                                        (make-who-condition 'symbol-value)
                                        (make-message-condition "Unbound bootstrap variable")
                                        (make-irritants-condition (list symbol)))))))
               (else
                ;; A non-bootstrap gensym just contains the value.
                (let ((x ($box-ref symbol SYMBOL-VALUE/IDX)))
                  (if (eq? x *unbound-hack*)
                      (raise (condition (make-undefined-violation)
                                        (make-who-condition 'symbol-value)
                                        (make-message-condition "Unbound variable")
                                        (make-irritants-condition (list symbol))))
                      x)))))
        (else
         (error 'symbol-value "Expected a gensym" symbol)))))
