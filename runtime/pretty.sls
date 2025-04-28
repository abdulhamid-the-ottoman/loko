;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020-2022 G. Weinholt
#!r6rs

;;; Pretty printer

#|

This library is a modified version of a pretty printer by Marc Feeley.
The original was copied from SLIB and carried this notice:

;;"genwrite.scm" generic write used by pretty-print and truncated-print.
;; Copyright (c) 1991, Marc Feeley
;; Author: Marc Feeley (feeley@iro.umontreal.ca)
;; Distribution restrictions: none

I asked Marc for a clarification of the above conditions:

> Subject: Re: Your pretty printer from 1991
> From: Marc Feeley <feeley@iro.umontreal.ca>
> In-Reply-To: <87imchg8i9.fsf@teapot.weinholt.se>
> Date: Sun, 13 Sep 2020 09:22:30 -0400
> Message-Id: <4D04A14C-952F-4158-87C0-1417FEC8E394@iro.umontreal.ca>
>
> Yes you can use it (and modify it) as you wish.  Please preserve some
> historical reference to the original in the comments.  Several Scheme
> implementations use it in a modified form, notably Guile (and I think
> Bigloo)… but not Gambit!  Amazing that 29 year old code is still
> useful and working.
>
> Marc

I've extended it with support for some R6RS/R7RS stuff.
All the bugs are mine. -- weinholt

|#

(library (loko runtime pretty)
  (export
    pretty-print
    pretty-print-simple)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (loko runtime io-printer) find-cycles write-simple))

(define (generic-write obj width use-simple-write output)
  (define (read-macro? l)
    (define (length1? l) (and (pair? l) (null? (cdr l))))
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((quote quasiquote unquote unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing)
         (length1? tail))
        (else #f))))

  (define (read-macro-body l)
    (cadr l))

  (define (read-macro-prefix l)
    (case (car l)
      ((quote)             "'")
      ((quasiquote)        "`")
      ((unquote)           ",")
      ((unquote-splicing)  ",@")
      ((syntax)            "#'")
      ((quasisyntax)       "#`")
      ((unsyntax)          "#,")
      ((unsyntax-splicing) "#,@")))

  (define (out str col)
    (and col (output str) (fx+ col (string-length str))))

  (define (wr obj col)
    (define (wr-expr expr col)
      (if (read-macro? expr)
          (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
          (wr-lst expr col)))
    (define (wr-lst l col)
      (if (pair? l)
	  (let loop ((l (cdr l))
		     (col (and col (wr (car l) (out "(" col)))))
	    (cond ((not col) col)
		  ((pair? l)
		   (loop (cdr l) (wr (car l) (out " " col))))
		  ((null? l) (out ")" col))
		  (else      (out ")" (wr l (out " . " col))))))
	  (out "()" col)))
    (cond ((pair? obj)        (wr-expr obj col))
          ((null? obj)        (wr-lst obj col))
          ((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
          ((bytevector? obj)  (wr-lst (bytevector->u8-list obj) (out "#vu8" col)))
          (else
           (out (call-with-string-output-port
                  (lambda (p)
                    (if use-simple-write
                        (write-simple obj p)
                        (write obj p))))
                col))))

  (define (pp obj col)
    (define (spaces n col)
      (if (fx>? n 0)
          (if (fx>? n 7)
              (spaces (fx- n 8) (out "        " col))
              (out (make-string n #\space) col))
          col))

    (define (indent to col)
      (and col
           (if (fx<? to col)
               (and (out "\n" col) (spaces to 0))
               (spaces (fx- to col) col))))

    (define (pr obj col extra pp-pair)
      (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
          (let ((result '())
                (left (fxmin (fx+ (fx- (fx- width col) extra) 1) max-expr-width)))
            (generic-write obj #f use-simple-write
                           (lambda (str)
                             (set! result (cons str result))
                             (set! left (fx- left (string-length str)))
                             (fx>? left 0)))
            (cond
              ((fx>? left 0)           ; all can be printed on one line
               (out (reverse-string-append result) col))
              ((pair? obj)
               (pp-pair obj col extra))
              (else
               (pp-list (vector->list obj) (out "#" col) extra pp-expr))))
          (wr obj col)))

    (define (pp-expr expr col extra)
      (if (read-macro? expr)
          (pr (read-macro-body expr)
              (out (read-macro-prefix expr) col)
              extra
              pp-expr)
          (let ((head (car expr)))
            (if (symbol? head)
                (let ((proc (style head)))
                  (if proc
                      (proc expr col extra)
                      (if (fx>? (string-length (symbol->string head))
                                max-call-head-width)
                          (pp-general expr col extra #f #f #f pp-expr)
                          (pp-call expr col extra pp-expr))))
                (pp-list expr col extra pp-expr)))))

    ;; (head item1 item2 item3)
    (define (pp-call expr col extra pp-item)
      (let ((col* (wr (car expr) (out "(" col))))
        (and col
             (pp-down (cdr expr) col* (fx+ col* 1) extra pp-item))))

    ;; (item1 item2 item3)
    (define (pp-list l col extra pp-item)
      (let ((col (out "(" col)))
        (pp-down l col col extra pp-item)))

    (define (pp-down l col1 col2 extra pp-item)
      (let loop ((l l) (col col1))
        (and col
             (cond ((pair? l)
                    (let ((rest (cdr l)))
                      (let ((extra (if (null? rest) (fx+ extra 1) 0)))
                        (loop rest
                              (pr (car l) (indent col2 col) extra pp-item)))))
                   ((null? l)
                    (out ")" col))
                   (else
                    (out ")"
                         (pr l
                             (indent col2 (out "." (indent col2 col)))
                             (fx+ extra 1)
                             pp-item)))))))

    (define (pp-general expr col extra named? pp-1 pp-2 pp-3)
      (define (tail1 rest col1 col2 col3)
        (if (and pp-1 (pair? rest))
            (let* ((val1 (car rest))
                   (rest (cdr rest))
                   (extra (if (null? rest) (fx+ extra 1) 0)))
              (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
            (tail2 rest col1 col2 col3)))
      (define (tail2 rest col1 col2 col3)
        (if (and pp-2 (pair? rest))
            (let* ((val1 (car rest))
                   (rest (cdr rest))
                   (extra (if (null? rest) (fx+ extra 1) 0)))
              (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
            (tail3 rest col1 col2)))
      (define (tail3 rest col1 col2)
        (pp-down rest col2 col1 extra pp-3))
      (let* ((head (car expr))
             (rest (cdr expr))
             (col* (wr head (out "(" col))))
        (if (and named? (pair? rest))
            (let* ((name (car rest))
                   (rest (cdr rest))
                   (col** (wr name (out " " col*))))
              (tail1 rest (fx+ col indent-general) col** (fx+ col** 1)))
            (tail1 rest (fx+ col indent-general) col* (fx+ col* 1)))))

    (define (pp-expr-list l col extra)
      (pp-list l col extra pp-expr))

    (define (pp-LAMBDA expr col extra)
      (pp-general expr col extra #f pp-expr-list #f pp-expr))

    (define (pp-SET! expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr))

    (define (pp-IF expr col extra)
      (pp-general expr (fx+ 2 col) extra #f pp-expr #f pp-expr))

    (define (pp-COND expr col extra)
      (pp-call expr col extra pp-expr-list))

    (define (pp-CASE expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr-list))

    (define (pp-AND expr col extra)
      (pp-call expr col extra pp-expr))

    (define (pp-LET expr col extra)
      (let* ((rest (cdr expr))
             (named? (and (pair? rest) (symbol? (car rest)))))
        (pp-general expr col extra named? pp-expr-list #f pp-expr)))

    (define (pp-BEGIN expr col extra)
      (pp-general expr col extra #f #f #f pp-expr))

    (define (pp-DO expr col extra)
      (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

    (define (pp-SYNTAX-CASE expr col extra)
      (pp-general expr col extra #t pp-expr-list #f pp-expr))

;;; define formatting style (change these to suit your style)

    (define indent-general 2)

    (define max-call-head-width 5)

    (define max-expr-width 50)

    (define (style head)
      (case head
        ((lambda let* letrec letrec* define
                 when unless fix guard library
                 let-values let*-values
                 let-syntax let*-syntax letrec-syntax with-syntax
                 parameterize
                 syntax-rules
                 define-library
                 define-syntax
                 define-enumeration
                 define-record-type)
         pp-LAMBDA)
        ((if)                        pp-IF)
        ((set!)                      pp-SET!)
        ((cond)                      pp-COND)
        ((case)                      pp-CASE)
        ((and or)                    pp-AND)
        ((let)                       pp-LET)
        ((begin)                     pp-BEGIN)
        ((do)                        pp-DO)
        ((syntax-case define-condition-type) pp-SYNTAX-CASE)
        (else                        #f)))

    (pr obj col 0 pp-expr))

  (if width
      (out "\n" (pp obj 0))
      (wr obj 0)))

;; (reverse-string-append l) = (apply string-append (reverse l))
(define (reverse-string-append l)
  (define (rev-string-append l i)
    (if (pair? l)
        (let* ((str (car l))
               (len (string-length str))
               (result (rev-string-append (cdr l) (fx+ i len))))
          (let loop ((j 0) (k (fx- (fx- (string-length result) i) len)))
            (if (fx<? j len)
                (begin
                  (string-set! result k (string-ref str j))
                  (loop (fx+ j 1) (fx+ k 1)))
                result)))
        (make-string i)))
  (rev-string-append l 0))

(define pretty-print
  (case-lambda
    ((obj)
     (pretty-print obj (current-output-port)))
    ((obj port)
     ;; TODO: Handle cycles. The pretty-printer doesn't handle cycles
     ;; right now, so check that first.
     (let-values ([(_ x) (hashtable-entries (find-cycles obj))])
       (cond ((not (exists values (vector->list x)))
              (generic-write obj 78 #f
                             (lambda (str)
                               (put-string port str)))
              (if #f #f))
             (else
              (write obj port)
              (newline port)))))))

(define (pretty-print-simple obj port)
  (generic-write obj 78 #t
                 (lambda (str)
                   (put-string port str)))))
