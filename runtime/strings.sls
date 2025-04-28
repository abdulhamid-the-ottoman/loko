;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Primitives for strings

(library (loko runtime strings)
  (export
    string? make-string string string-length string-ref
    string=? string<? string>? string<=? string>=?
    substring string-append string->list list->string
    string-for-each string-copy
    string-fill! string-set!

    string-truncate!)
  (import
    (except (rnrs)
            string? make-string string string-length string-ref
            string=? string<? string>? string<=? string>=?
            substring string-append string->list list->string
            string-for-each string-copy
            string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
            string-upcase string-downcase string-foldcase string-titlecase
            string-normalize-nfd string-normalize-nfkd
            string-normalize-nfc string-normalize-nfkc)
    (prefix (rnrs) sys:)
    (prefix (rnrs mutable-strings) sys:)
    (loko system $primitives))

(define (string? x) (sys:string? x))

(define make-string
  (case-lambda
    ((len)
     (make-string len #\space))
    ((len fill)
     (assert (and (fixnum? len) (not (fxnegative? len)) (char? fill)))
     (if (eqv? len 0)
         ""
         ($make-string len fill)))))

(define (string . chars)
  (let ((len (length chars)))
    (do ((s (make-string len))
         (i 0 (fx+ i 1))
         (x chars (cdr x)))
        ((null? x) s)
      (when (not (char? (car x)))
        (apply assertion-violation 'string "Expected characters" chars))
      (string-set! s i (car x)))))

(define (string-length s) (sys:string-length s))

(define (string-ref s i) (sys:string-ref s i))

(define string=?
  (case-lambda
    ((x1 x2)
     (and (fx=? (string-length x1) (string-length x2))
          (or (eq? x1 x2)
              (let ((len (string-length x1)))
                (let lp ((i 0))
                  (or (fx=? i len)
                      (and (eq? (string-ref x1 i) (string-ref x2 i))
                           (lp (fx+ i 1)))))))))
    ((x1 x2 . x*)
     (let lp ((x* x*) (ret (string=? x1 x2)))
       (if (null? x*)
           ret
           (lp (cdr x*) (and (string=? x1 (car x*)) ret)))))))

(define string<?
  (case-lambda
    ((x1 x2)
     (let lp ((i 0))
       (cond ((fx=? i (string-length x2)) #f)
             ((fx=? i (string-length x1)) #t)
             ((eq? x1 x2) #f)
             (else
              (let ((c1 (string-ref x1 i))
                    (c2 (string-ref x2 i)))
                (cond ((char>? c1 c2) #f)
                      ((char<? c1 c2) #t)
                      (else (lp (fx+ i 1)))))))))
    ((x1 x2 . x*)
     (let lp ((x x2) (x* x*) (ret (string<? x1 x2)))
       (if (null? x*)
           ret
           (lp (car x*) (cdr x*) (and (string<? x (car x*)) ret)))))))

(define string<=?
  (case-lambda
    ((x1 x2)
     (not (string>? x1 x2)))
    ((x1 x2 . x*)
     (let lp ((x x2) (x* x*) (ret (string<=? x1 x2)))
       (if (null? x*)
           ret
           (lp (car x*) (cdr x*) (and (string<=? x (car x*)) ret)))))))

(define string>?
  (case-lambda
    ((x1 x2)
     (string<? x2 x1))
    ((x1 x2 . x*)
     (let lp ((x x2) (x* x*) (ret (string>? x1 x2)))
       (if (null? x*)
           ret
           (lp (car x*) (cdr x*) (and (string>? x (car x*)) ret)))))))

(define string>=?
  (case-lambda
    ((x1 x2)
     (not (string<? x1 x2)))
    ((x1 x2 . x*)
     (let lp ((x x2) (x* x*) (ret (string>=? x1 x2)))
       (if (null? x*)
           ret
           (lp (car x*) (cdr x*) (and (string>=? x (car x*)) ret)))))))

(define (substring string start end)
  (unless (string? string)
    (assertion-violation 'substring "Expected a string" string start end))
  (let ((len (fx- end start)))
    (if (eqv? len 0)
        ""
        (do ((new ($make-string len))
             (i 0 (fx+ i 1))
             (j (fx+ start 0) (fx+ j 1)))
            ((fx=? i len) new)
          (string-set! new i (string-ref string j))))))

(define (string-append . x*)
  (let ((length (do ((x* x* (cdr x*))
                     (length 0 (fx+ length (string-length (car x*)))))
                    ((null? x*) length))))
    (if (eqv? length 0)
        ""
        (do ((str ($make-string length))
             (x* x* (cdr x*))
             (n 0 (fx+ n (string-length (car x*)))))
            ((null? x*) str)
          (let* ((s (car x*))
                 (len (string-length s)))
            (do ((i 0 (fx+ i 1))
                 (j n (fx+ j 1)))
                ((fx=? i len))
              (string-set! str j (string-ref s i))))))))

(define (string->list s)
  (do ((i (fx- (string-length s) 1) (fx- i 1))
       (ret '() (cons (string-ref s i) ret)))
      ((fx=? i -1) ret)))

(define (list->string list)
  ;; XXX: checks for cycles using length
  (do ((string (make-string (length list)))
       (n 0 (fx+ n 1))
       (l list (cdr l)))
      ((null? l) string)
    (when (not (char? (car l)))
      (apply assertion-violation 'list->string
             "Expected a list of characters" list))
    (string-set! string n (car l))))

(define string-for-each
  (case-lambda
    ((proc s1)
     (do ((length (string-length s1))
          (n 0 (fx+ n 1)))
         ((fx=? n length))
       (proc (string-ref s1 n))))
    ((proc s1 . s*)
     (let ((length (string-length s1)))
       (assert (apply = length (map sys:string-length s*)))
       (do ((n 0 (fx+ n 1)))
           ((fx=? n length))
         (apply proc (string-ref s1 n)
                (map (lambda (sk) (string-ref sk n)) s*)))))))

(define (string-copy string)
  (do ((length (string-length string))
       (copy (make-string (string-length string)))
       (n 0 (fx+ n 1)))
      ((fx=? n length) copy)
    (string-set! copy n (string-ref string n))))

;; (rnrs mutable-strings)

(define (string-set! s i c)
  (sys:string-set! s i c))

(define (string-fill! s c)
  (assert (and (string? s) (char? c)))
  (do ((len (string-length s))
       (i 0 (fx+ i 1)))
      ((fx=? i len))
    (string-set! s i c)))

;; (loko)

(define (string-truncate! str n)
  (cond ((eqv? n 0)
         "")
        (else
         (assert (fx<=? 0 n (string-length str)))
         ($string-truncate! str n)))))
