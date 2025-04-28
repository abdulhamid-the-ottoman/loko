;; Copyright (C) 2008  Abdulaziz Ghuloum, R. Kent Dybvig
;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: MIT
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Unicode string and character library

;; The original is from Ikarus Scheme. There is an updated version in
;; Chez Scheme. This one is adapted to Loko Scheme.

#!r6rs
(library (loko runtime unicode)
  (export
    char-upcase char-downcase char-titlecase char-foldcase
    char-whitespace? char-lower-case? char-upper-case?
    char-title-case? char-numeric?
    char-alphabetic? char-general-category char-ci<? char-ci<=?
    char-ci=? char-ci>? char-ci>=? string-upcase string-downcase
    string-foldcase string-titlecase string-ci<? string-ci<=?
    string-ci=? string-ci>? string-ci>=? string-normalize-nfd
    string-normalize-nfkd string-normalize-nfc string-normalize-nfkc)
  (import
    (only (loko) include/resolve module)
    (except (rnrs)
      char-upcase char-downcase char-titlecase char-foldcase
      char-whitespace? char-lower-case? char-upper-case?
      char-title-case? char-numeric?
      char-alphabetic? char-general-category char-ci<? char-ci<=?
      char-ci=? char-ci>? char-ci>=? string-upcase string-downcase
      string-foldcase string-titlecase string-ci<? string-ci<=?
      string-ci=? string-ci>? string-ci>=? string-normalize-nfd
      string-normalize-nfkd string-normalize-nfc string-normalize-nfkc)
    (rnrs mutable-strings))

(module
  (char-upcase char-downcase char-titlecase char-foldcase
   char-whitespace? char-lower-case? char-upper-case?
   char-title-case? char-numeric?
   char-alphabetic? char-general-category char-ci<? char-ci<=?
   char-ci=? char-ci>? char-ci>=? string-upcase string-downcase
   string-foldcase string-titlecase string-ci<? string-ci<=?
   string-ci=? string-ci>? string-ci>=? string-normalize-nfd
   string-normalize-nfkd string-normalize-nfc string-normalize-nfkc)

(define fx> fx>?)
(define fx< fx<?)
(define fx= fx=?)
(define fx>= fx>=?)
(define fxsrl fxarithmetic-shift-right)
(define fxsll fxarithmetic-shift-left)
(define (fxlogtest x y)
  (not (fxzero? (fxand x y))))
(define (char- x y)
  (fx- (char->integer x) (char->integer y)))
(define (string-copy! s ss t ts k)
  (assert (not (eq? t s)))
  (do ((i (fx- k 1) (fx- i 1))
       (ti ts (fx+ ti 1))
       (si ss (fx+ si 1)))
      ((eqv? i -1))
    (string-set! t ti (string-ref s si))))

(include/resolve ("loko" "runtime" "unicode") "unicode-char-cases.ss")
(include/resolve ("loko" "runtime" "unicode") "unicode-charinfo.ss")

(define (char-error who what)
  (assertion-violation who "Expected a character" what))

(define (string-error who what)
  (assertion-violation who "Expected a string" what))

(define-syntax define-char-op
  (syntax-rules ()
    [(_ name unsafe-op)
     ;; XXX: These operations are safe in Loko
     (define name unsafe-op)]))

(define-char-op char-upcase $char-upcase)
(define-char-op char-downcase $char-downcase)
(define-char-op char-titlecase $char-titlecase)
(define-char-op char-foldcase $char-foldcase)
(define-char-op char-whitespace? $char-whitespace?)
(define-char-op char-lower-case? $char-lower-case?)
(define-char-op char-upper-case? $char-upper-case?)
(define-char-op char-title-case? $char-title-case?)
(define-char-op char-numeric? $char-numeric?)
(define-char-op char-alphabetic? $char-alphabetic?)
(define-char-op char-general-category $char-category)
(define-char-op $constituent? $char-constituent?)
(define-char-op $subsequent? $char-subsequent?)

(define (do-char-cmp a ls cmp who)
  (if (char? a)
      (let f ([a ($char-foldcase a)] [ls ls])
        (cond
          [(null? ls) #t]
          [else
           (let ([b (car ls)])
             (if (char? b)
                 (let ([b ($char-foldcase b)])
                   (if (cmp a b)
                       (f b (cdr ls))
                       (let f ([ls (cdr ls)])
                         (if (null? ls)
                             #f
                             (if (char? (car ls))
                                 (f (cdr ls))
                                 (assertion-violation who
                                   "Expected a character" (car ls)))))))
                 (assertion-violation who "Expected a character" b)))]))
      (assertion-violation who "Expected a character" a)))

(define-syntax define-char-cmp
  (syntax-rules ()
    [(_ name cmp)
     (define name
       (case-lambda
         [(c1 c2)
          (if (char? c1)
              (if (char? c2)
                  (cmp ($char-foldcase c1) ($char-foldcase c2))
                  (assertion-violation 'name "Expected a character" c2))
              (assertion-violation 'name "Expected a character" c1))]
         [(c1 . rest)
          (do-char-cmp c1 rest (lambda (x y) (cmp x y)) 'name)]))]))

(define-char-cmp char-ci<? char<?)
(define-char-cmp char-ci<=? char<=?)
(define-char-cmp char-ci=? char=?)
(define-char-cmp char-ci>? char>?)
(define-char-cmp char-ci>=? char>=?)

(define (handle-special str ac)
  (define (chars ac n)
    (cond
      [(null? ac) n]
      [else
       (chars (cdr ac)
              (let f ([p (cdar ac)] [n n])
                (cond
                  [(pair? p) (f (cdr p) (fx+ n 1))]
                  [else n])))]))
  (define (extend src ac src-len dst-len)
    (let f ([str str] [dst (make-string dst-len)] [i 0] [j 0] [ac (reverse ac)] [sigma* '()])
      (cond
        [(null? ac)
         (string-copy! str i dst j (fx- src-len i))
         (do-sigmas dst sigma*)]
        [else
         (let ([idx (caar ac)] [c* (cdar ac)] [ac (cdr ac)])
           (let ([cnt (fx- idx i)])
             (string-copy! str i dst j cnt)
             (let g ([str str]       [dst dst]
                     [i (fx+ i cnt)] [j (fx+ j cnt)]
                     [ac ac]         [c* c*])
               (cond
                 [(pair? c*)
                  (string-set! dst j (car c*))
                  (g str dst i (fx+ j 1) ac (cdr c*))]
                 [(char? c*)
                  (string-set! dst j c*)
                  (f str dst (fx+ i 1) (fx+ j 1) ac sigma*)]
                 [else ; assume c* = sigma
                  (f str dst (fx+ i 1) (fx+ j 1) ac (cons j sigma*))]))))])))
  (define (do-sigmas str sigma*)
    (define nonfinal-sigma #\x3c3)
    (define final-sigma #\x3c2)
    (define (final? i)
      (define (scan i incr n)
        (and (not (fx= i n))
             (or ($char-cased? (string-ref str i))
                 (and ($char-case-ignorable? (string-ref str i))
                      (scan (fx+ i incr) incr n)))))
      (and (scan (fx- i 1) -1 -1) (not (scan (fx+ i 1) +1 (string-length str)))))
    ;; scanning requires we have some character in place...guess nonfinal sigma
    (for-each (lambda (i) (string-set! str i nonfinal-sigma)) sigma*)
    (for-each (lambda (i) (when (final? i) (string-set! str i final-sigma))) sigma*)
    str)
  (let* ([src-len (string-length str)]
         [dst-len (chars ac src-len)])
    (if (fx= dst-len src-len)
        (do-sigmas str (map car ac))
        (extend str ac src-len dst-len))))

(define (string-change-case str cvt-char)
  (let ([n (string-length str)])
    (let f ([str str] [dst (make-string n)] [i 0] [n n] [ac '()])
      (cond
        [(fx= i n)
         (if (null? ac)
             dst
             (handle-special dst ac))]
        [else
         (let ([c/ls (cvt-char (string-ref str i))])
           (cond
             [(char? c/ls)
              (string-set! dst i c/ls)
              (f str dst (fx+ i 1) n ac)]
             [else
              (f str dst (fx+ i 1) n
                 (cons (cons i c/ls) ac))]))]))))

(define (string-upcase s)
  (unless (string? s) (string-error 'string-upcase s))
  (string-change-case s $str-upcase))

(define (string-foldcase s)
  (unless (string? s) (string-error 'string-foldcase s))
  (string-change-case s $str-foldcase))

(define (string-downcase s)
  (unless (string? s) (string-error 'string-downcase s))
  (string-change-case s $str-downcase))

(define (string-titlecase str)
  (unless (string? str) (string-error 'string-titlecase str))
  (let* ([n (string-length str)] [dst (make-string n)])
    (define (trans2 s i seen-cased? ac)
      (if (fx= i n)
          (handle-special dst ac)
          (s i seen-cased? ac)))
    (define (trans1 s i c/ls seen-cased? ac)
      (cond
        [(char? c/ls)
         (string-set! dst i c/ls)
         (trans2 s (fx+ i 1) seen-cased? ac)]
        [else
         (trans2 s (fx+ i 1) seen-cased? (cons (cons i c/ls) ac))]))
    (define (trans s i c seen-cased? ac)
      (if seen-cased?
          (trans1 s i ($str-downcase c) #t ac)
          (if ($char-cased? c)
              (trans1 s i ($str-titlecase c) #t ac)
              (trans1 s i c #f ac))))
    ;; NB: if used as a pattern for word breaking, take care not to break between CR & LF (WB3)
    ;; NB: and between regional-indicators (WB13c).  also take care not to let handling of WB6 and
    ;; NB: WB7 here prevent breaks in, e.g., "a." when not followed by, e.g., another letter.
    (define (s0 i ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-aletter? c) (trans sAletter i c #f ac)]
          [($wb-hebrew-letter? c) (trans sHebrewletter i c #f ac)]
          [($wb-numeric? c) (trans sNumeric i c #f ac)]
          [($wb-katakana? c) (trans sKatakana i c #f ac)]
          [($wb-extendnumlet? c) (trans sExtendnumlet i c #f ac)]
          [($wb-regional-indicator? c) (trans sRegionalIndicator i c #f ac)]
          [else (string-set! dst i c)
                (let ([i (fx+ i 1)])
                  (if (fx= i n)
                      (handle-special dst ac)
                      (s0 i ac)))])))
    (define (sAletter i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB5
          [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB5
          [(or ($wb-midletter? c) ($wb-midnumlet? c) ($wb-single-quote? c)) (trans sWB6/WB7/WB7a i c seen-cased? ac)] ; WB6/WB7
          [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB9
          [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
          [(or ($wb-extend? c) ($wb-format? c)) (trans sAletter i c seen-cased? ac)] ; WB4
          [else (s0 i ac)])))    ; WB14
    (define (sHebrewletter i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB5
          [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB5
          [(or ($wb-midletter? c) ($wb-midnumlet? c) ($wb-single-quote? c)) (trans sWB6/WB7/WB7a i c seen-cased? ac)] ; WB6/WB7/WB7a
          [($wb-double-quote? c) (trans sWB7b/WB7c i c seen-cased? ac)] ; WB7b, WB7c
          [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB9
          [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
          [(or ($wb-extend? c) ($wb-format? c)) (trans sHebrewletter i c seen-cased? ac)] ; WB4
          [else (s0 i ac)])))    ; WB14
    (define (sWB6/WB7/WB7a i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB6, WB7
          [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB6, WB7
          [(or ($wb-extend? c) ($wb-format? c)) (trans sWB6/WB7/WB7a i c seen-cased? ac)] ; WB4
          ;; word break actually should/could have occurred one character earlier if we got here
          ;; from sAletter rather than sHebrewletter but that was before a midlet, midnumlet, or single
          ;; quote which has no titlecase
          [else (s0 i ac)])))    ; WB14
    (define (sWB7b/WB7c i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB7b, WB7c
          [(or ($wb-extend? c) ($wb-format? c)) (trans sWB7b/WB7c i c seen-cased? ac)] ; WB4
          ;; word break actually should/could have occurred one character earlier
          ;; but that was before a double quote which has no titlecase
          [else (s0 i ac)])))    ; WB14
    (define (sSingleQuote i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; finishing WB6, WB7
          [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; finishing WB6, WB7
          [(or ($wb-extend? c) ($wb-format? c)) (trans sSingleQuote i c seen-cased? ac)] ; WB4
          [else (s0 i ac)])))    ; WB14
    (define (sNumeric i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB8
          [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB10
          [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB10
          [(or ($wb-midnum? c) ($wb-midnumlet? c) ($wb-single-quote? c)) (trans sWB11/WB12 i c seen-cased? ac)] ; WB11, WB12
          [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)]
          [(or ($wb-extend? c) ($wb-format? c)) (trans sNumeric i c seen-cased? ac)] ; WB4
          [else (s0 i ac)])))    ; WB14
    (define (sWB11/WB12 i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)]
          [(or ($wb-extend? c) ($wb-format? c)) (trans sWB11/WB12 i c seen-cased? ac)] ; WB4
          ;; word break actually should/could have occurred one character earlier
          ;; but that was before a midnum, midnumlet, or single quote which has no titltecase
          [else (s0 i ac)])))    ; WB14
    (define (sKatakana i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-katakana? c) (trans sKatakana i c seen-cased? ac)] ; WB13
          [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
          [(or ($wb-extend? c) ($wb-format? c)) (trans sKatakana i c seen-cased? ac)] ; WB4
          [else (s0 i ac)])))    ; WB14
    (define (sExtendnumlet i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
          [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB13b
          [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB13b
          [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB13b
          [($wb-katakana? c) (trans sKatakana i c seen-cased? ac)] ; WB13b
          [(or ($wb-extend? c) ($wb-format? c)) (trans sExtendnumlet i c seen-cased? ac)] ; WB4
          [else (s0 i ac)])))    ; WB14
    (define (sRegionalIndicator i seen-cased? ac)
      (let ([c (string-ref str i)])
        (cond
          [($wb-regional-indicator? c) (trans sRegionalIndicator i c seen-cased? ac)] ; WB13c
          [(or ($wb-extend? c) ($wb-format? c)) (trans sExtendnumlet i c seen-cased? ac)] ; WB4
          [else (s0 i ac)])))    ; WB14
    (if (fx= n 0) dst (s0 0 '()))))


(define-syntax string-relop
  (syntax-rules ()
    [(_ (name x1 x2) pred)
     (define name
       (case-lambda
         [(x1 x2)
          (if (string? x1)
              (if (string? x2)
                  pred
                  (string-error 'name x2))
              (string-error 'name x1))]
         [(x1 x2 . rest)
          (let loop ([x1 x1] [x2 x2] [ls rest])
            (if (or (null? ls) (loop x2 (car ls) (cdr ls)))
                (name x1 x2)
                (begin (name x1 x2) #f)))]))]))

(define string-equal?
  (lambda (s1 s2)
    (or (eq? s1 s2)
        (let ([n (string-length s1)])
          (and (fx= n (string-length s2))
               (let f ([i 0])
                 (or (fx= i n)
                     (and (char=? (string-ref s1 i) (string-ref s2 i))
                          (f (fx+ i 1))))))))))

(define string-less?
  (lambda (s1 s2)
    (and (not (eq? s1 s2))
         (let ([n1 (string-length s1)] [n2 (string-length s2)])
           (let f ([i 0])
             (and (not (fx= i n2))
                  (or (fx= i n1)
                      (let ([c1 (string-ref s1 i)]
                            [c2 (string-ref s2 i)])
                        (or (char<? c1 c2)
                            (and (char=? c1 c2) (f (fx+ i 1))))))))))))

(define string-ci-equal?
  (lambda (s1 s2)
    (or (eq? s1 s2)
        (let ([n1 (string-length s1)] [n2 (string-length s2)])
          (if (fx= n1 0)
              (fx= n2 0)
              (and (not (fx= n2 0))
                   (let f ([i1 1]
                           [i2 1]
                           [c1* ($str-foldcase (string-ref s1 0))]
                           [c2* ($str-foldcase (string-ref s2 0))])
                     (if (char? c1*)
                         (if (char? c2*)
                             (and (char=? c1* c2*)
                                  (if (fx= i1 n1)
                                      (fx= i2 n2)
                                      (and (not (fx= i2 n2))
                                           (f (fx+ i1 1) (fx+ i2 1)
                                              ($str-foldcase (string-ref s1 i1))
                                              ($str-foldcase (string-ref s2 i2))))))
                             (and (char=? c1* (car c2*))
                                  (not (fx= i1 n1))
                                  (f (fx+ i1 1) i2
                                     ($str-foldcase (string-ref s1 i1))
                                     (cdr c2*))))
                         (if (char? c2*)
                             (and (char=? (car c1*) c2*)
                                  (not (fx= i2 n2))
                                  (f i1 (fx+ i2 1) (cdr c1*)
                                     ($str-foldcase (string-ref s2 i2))))
                             (and (char=? (car c1*) (car c2*))
                                  (f i1 i2 (cdr c1*) (cdr c2*))))))))))))

(define string-ci-less?
  (lambda (s1 s2)
    (and (not (eq? s1 s2))
         (let ([n1 (string-length s1)] [n2 (string-length s2)])
           (and (not (fx= n2 0))
                (or (fx= n1 0)
                    (let f ([i1 1]
                            [i2 1]
                            [c1* ($str-foldcase (string-ref s1 0))]
                            [c2* ($str-foldcase (string-ref s2 0))])
                      (if (char? c1*)
                          (if (char? c2*)
                              (or (char<? c1* c2*)
                                  (and (char=? c1* c2*)
                                       (not (fx= i2 n2))
                                       (or (fx= i1 n1)
                                           (f (fx+ i1 1) (fx+ i2 1)
                                              ($str-foldcase (string-ref s1 i1))
                                              ($str-foldcase (string-ref s2 i2))))))
                              (or (char<? c1* (car c2*))
                                  (and (char=? c1* (car c2*))
                                       (or (fx= i1 n1)
                                           (f (fx+ i1 1) i2
                                              ($str-foldcase (string-ref s1 i1))
                                              (cdr c2*))))))
                          (if (char? c2*)
                              (or (char<? (car c1*) c2*)
                                  (and (char=? (car c1*) c2*)
                                       (not (fx= i2 n2))
                                       (f i1 (fx+ i2 1) (cdr c1*)
                                          ($str-foldcase (string-ref s2 i2)))))
                              (or (char<? (car c1*) (car c2*))
                                  (and (char=? (car c1*) (car c2*))
                                       (f i1 i2 (cdr c1*) (cdr c2*)))))))))))))

(string-relop (string=? x1 x2) (string-equal? x1 x2))
(string-relop (string<? x1 x2) (string-less? x1 x2))
(string-relop (string>? x1 x2) (string-less? x2 x1))
(string-relop (string<=? x1 x2) (not (string-less? x2 x1)))
(string-relop (string>=? x1 x2) (not (string-less? x1 x2)))

(string-relop (string-ci=? x1 x2) (string-ci-equal? x1 x2))
(string-relop (string-ci<? x1 x2) (string-ci-less? x1 x2))
(string-relop (string-ci>? x1 x2) (string-ci-less? x2 x1))
(string-relop (string-ci<=? x1 x2) (not (string-ci-less? x2 x1)))
(string-relop (string-ci>=? x1 x2) (not (string-ci-less? x1 x2)))


(module (hangul-sbase hangul-slimit $hangul-decomp
         hangul-lbase hangul-llimit
         hangul-vbase hangul-vlimit
         hangul-tbase hangul-tlimit
         hangul-vcount hangul-tcount)
  ;; adapted from UAX #15
  (define SBase #xAC00)
  (define LBase #x1100)
  (define VBase #x1161)
  (define TBase #x11A7)
  (define LCount 19)
  (define VCount 21)
  (define TCount 28)
  (define NCount (* VCount TCount))
  (define SCount (* LCount NCount))
  (define hangul-sbase (integer->char SBase))
  (define hangul-slimit (integer->char (+ SBase SCount -1)))
  (define hangul-lbase (integer->char LBase))
  (define hangul-llimit (integer->char (+ LBase LCount -1)))
  (define hangul-vbase (integer->char VBase))
  (define hangul-vlimit (integer->char (+ VBase VCount -1)))
  (define hangul-tbase (integer->char TBase))
  (define hangul-tlimit (integer->char (+ TBase TCount -1)))
  (define hangul-vcount VCount)
  (define hangul-tcount TCount)
  (define ($hangul-decomp c)
    (let ([SIndex (char- c hangul-sbase)])
      (let ([L (integer->char (fx+ LBase (fxdiv SIndex NCount)))]
            [V (integer->char (fx+ VBase (fxdiv (fxmod SIndex NCount) TCount)))]
            [adj (fxmod SIndex TCount)])
        (if (fx= adj 0)
            (cons* L V)
            (cons* L V (integer->char (fx+ TBase adj))))))))

(define $decompose
  ;; might should optimize for sequences of ascii characters
  (lambda (s canonical?)
    (let ([n (string-length s)] [ac '()])
      (define (canonical>? c1 c2)
        (fx> ($char-combining-class c1) ($char-combining-class c2)))
      (define (sort-and-flush comb*)
        (unless (null? comb*)
          (set! ac (append (list-sort canonical>? comb*) ac))))
      (define ($char-decomp c)
        (if (and (char<=? hangul-sbase c) (char<=? c hangul-slimit))
            ($hangul-decomp c)
            (if canonical?
                ($str-decomp-canon c)
                ($str-decomp-compat c))))
      (define (push-and-go c* c** i comb*)
        (if (char? c*)
            (go c* c** i comb*)
            (go (car c*) (cons (cdr c*) c**) i comb*)))
      (define (pop-and-go c** i comb*)
        (if (null? c**)
            (if (fx= i n)
                (sort-and-flush comb*)
                (go (string-ref s i) '() (fx+ i 1) comb*))
            (push-and-go (car c**) (cdr c**) i comb*)))
      (define (go c c** i comb*)
        (let ([c* ($char-decomp c)])
          (if (eq? c c*) ; should be eqv?
              (if (fxzero? ($char-combining-class c))
                  (begin
                    (sort-and-flush comb*)
                    (set! ac (cons c ac))
                    (pop-and-go c** i '()))
                  (pop-and-go c** i (cons c comb*)))
              (push-and-go c* c** i comb*))))
      (pop-and-go '() 0 '())
      (list->string (reverse ac)))))

(define $compose
  (let ([comp-table #f])
    (define (lookup-composite c1 c2)
      (hashtable-ref comp-table (cons c1 c2) #f))
    (define (init!)
      (set! comp-table
            (make-hashtable
             (lambda (x)
               (fxxor
                (fxsll (char->integer (car x)) 7)
                (char->integer (cdr x))))
             (lambda (x y)
               (and (char=? (car x) (car y))
                    (char=? (cdr x) (cdr y))))))
      (vector-for-each
       (lambda (c* c) (hashtable-set! comp-table c* c))
       (car ($composition-pairs))
       (cdr ($composition-pairs))))
    (lambda (s)
      (unless comp-table (init!))
      (let ([ac '()] [n (string-length s)])
        (define (dump c acc)
          (set! ac (cons c ac))
          (unless (null? acc) (set! ac (append acc ac))))
        (define (s0 i)
          (unless (fx= i n)
            (let ([c (string-ref s i)])
              (if (fxzero? ($char-combining-class c))
                  (s1 (fx+ i 1) c)
                  (begin (set! ac (cons c ac)) (s0 (fx+ i 1)))))))
        (define (s1 i c)
          (if (fx= i n)
              (set! ac (cons c ac))
              (let ([c1 (string-ref s i)])
                (cond
                  [(and (and (char<=? hangul-lbase c)
                             (char<=? c hangul-llimit))
                        (and (char<=? hangul-vbase c1)
                             (char<=? c1 hangul-vlimit)))
                   (s1 (fx+ i 1)
                       (let ([lindex (char- c hangul-lbase)]
                             [vindex (char- c1 hangul-vbase)])
                         (integer->char
                          (fx+ (char->integer hangul-sbase)
                               (fx* (fx+ (fx* lindex hangul-vcount) vindex)
                                    hangul-tcount)))))]
                  [(and (and (char<=? hangul-sbase c)
                             (char<=? c hangul-slimit))
                        (and (char<=? hangul-tbase c1)
                             (char<=? c1 hangul-tlimit))
                        (let ([sindex (char- c hangul-sbase)])
                          (fxzero? (fxmod sindex hangul-tcount))))
                   (let ([tindex (char- c1 hangul-tbase)])
                     (s1 (fx+ i 1) (integer->char (fx+ (char->integer c) tindex))))]
                  [else (s2 i c -1 '())]))))
        (define (s2 i c class acc)
          (if (fx= i n)
              (dump c acc)
              (let ([c1 (string-ref s i)])
                (let ([class1 ($char-combining-class c1)])
                  (cond
                    [(and (fx< class class1) (lookup-composite c c1)) =>
                     (lambda (c) (s2 (fx+ i 1) c class acc))]
                    [(fx= class1 0)
                     (dump c acc)
                     (s1 (fx+ i 1) c1)]
                    [else (s2 (fx+ i 1) c class1 (cons c1 acc))])))))
        (s0 0)
        (list->string (reverse ac))))))

(define (string-normalize-nfd s)
  (unless (string? s) (string-error 'string-normalize-nfd s))
  ($decompose s #t))

(define (string-normalize-nfkd s)
  (unless (string? s) (string-error 'string-normalize-nfkd s))
  ($decompose s #f))

(define (string-normalize-nfc s)
  (unless (string? s) (string-error 'string-normalize-nfc s))
  ($compose ($decompose s #t)))

(define (string-normalize-nfkc s)
  (unless (string? s) (string-error 'string-normalize-nfkc s))
  ($compose ($decompose s #f)))))
