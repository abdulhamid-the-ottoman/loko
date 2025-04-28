;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Compat lib for bootstrapping from Chez Scheme

(library (loko compiler compat)
  (export gensym? gensym->unique-string gensym-prefix call/1cc)
  (import
    (rnrs arithmetic bitwise)
    (except (rnrs base) map)
    (rnrs bytevectors)
    (rnrs control)
    (rnrs hashtables)
    (rnrs programs)
    (rnrs io simple)
    (rnrs io ports)
    (rnrs lists)
    (rnrs files)
    (rnrs unicode)
    (rename (only (chezscheme) gensym? gensym->unique-string gensym-prefix call/1cc)
            (gensym->unique-string chez:gensym->unique-string))
    (loko config)
    (rename (loko runtime utils) (map-in-order map))
    (psyntax expander)
    (psyntax internal)
    (psyntax compat))

;; For reproducible builds
(define (gensym->unique-string s)
  (let ((p (open-string-input-port (chez:gensym->unique-string s))))
    (let lp ()
      (unless (eqv? (get-char p) #\-)
        (lp)))
    (string-append "u" (get-string-all p))))

(let ()
  (define qcons cons)
  (define qlist list)
  (define qappend append)
  (define qvector vector)
  (define qlist->vector list->vector)
  ;; This defines the primitives needed to run all the macros in the
  ;; bootstrap.
  (define-syntax define-prims
    (syntax-rules ()
      ((_ name* ...)
       (let ((g* (map (lambda (x) (gensym)) '(name* ...)))
             (v* (list name* ...)))
         (for-each set-symbol-value! g* v*)
         (let ((alist (map cons '(name* ...) g*)))
           (current-primitive-locations
            (lambda (x)
              (cond
                ((assq x alist) => cdr)
                (else (error 'expand-files "Undefined prim" x))))))))))
  (define-prims
      syntax-dispatch apply cons append map list for-each syntax-error
      generate-temporaries = + - * mod datum->syntax string->symbol
      string-append symbol->string syntax->datum gensym length
      open-string-output-port identifier? free-identifier=? exists
      values call-with-values for-all ellipsis-map assertion-violation
      assertion-error null? car cdr pair? bound-identifier=? vector make-vector vector-set!
      not eq? eqv? reverse list->vector vector->list memq memv <= exact? number?
      syntax-violation string->utf8 string? integer? boolean? char?
      char->integer char-whitespace? char<=? string-ref string-length bitwise-and
      make-variable-transformer
      void
      config-target-kernel
      make-eq-hashtable hashtable-ref hashtable-set! hashtable-delete!
      make-bytevector bytevector-u32-native-set! bytevector-ieee-single-native-ref
      bitwise-ior bitwise-arithmetic-shift-left
      qcons qlist qappend qvector qlist->vector
      )))
