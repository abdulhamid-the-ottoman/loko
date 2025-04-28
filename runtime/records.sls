;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-FileCopyrightText: 2019, 2020, 2021 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+
;;
;; The make-make-seeder procedure is based on code from SRFI-76
;; reference implementation:
;; SPDX-FileCopyrightText: 2005 Michael Sperber
;; SPDX-License-Identifier: MIT
;;
;; This file is a part of Loko Scheme, an R6RS Scheme system
#!r6rs

;;; Records

#|

record-type-descriptor objects satisfy $box? and $box-type is a
$box-header-type with 'rtd, an appropriate length (variable) and the
header type values encode these bits:

       bit 0: is opaque
       bit 1: is sealed
       bit 2: is generative

|#

;; XXX: symbols.scm has not yet been loaded when this top-level runs

(library (loko runtime records)
  (export
    make-record-type-descriptor record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor record-predicate
    record-accessor record-mutator
    record? record-rtd record-type-name record-type-parent
    record-type-uid record-type-generative? record-type-sealed?
    record-type-opaque? record-type-field-names record-field-mutable?
    record-writer

    make-record-type-descriptor/nongenerative
    make-record-type-descriptor/generative
    raise-accessor-error
    raise-mutator-error
    $record?
    (rename ($record? $record/fast?))
    $record)
  (import
    (except (rnrs)
            make-record-type-descriptor record-type-descriptor?
            make-record-constructor-descriptor
            record-constructor record-predicate
            record-accessor record-mutator
            record? record-rtd record-type-name record-type-parent
            record-type-uid record-type-generative? record-type-sealed?
            record-type-opaque? record-type-field-names record-field-mutable?)
    (except (loko system $primitives) $record $record?)
    (prefix (only (loko system $primitives) $record) sys:))

;; XXX: The record size should not be at 0 due to how moved-mark in
;; the GC works.
(define rtd:name 0)                     ;symbol
(define rtd:record-size 1)              ;size of records with this rtd as their type
(define rtd:parent 2)                   ;rtd
(define rtd:uid 3)                      ;symbol
(define rtd:field-names 4)              ;vector
(define rtd:mutable 5)                  ;bitfield
(define rtd:writer 6)                   ;#f, 'immutable or procedure
(define rtd:parents-start 7)            ;base offset of any parent rtds in reverse order

(define rtd:min-parents 4)              ;always store at least four parents

;;; Procedural layer

(define *uids* '())

(define (lookup-uid uid)
  ;; TODO: should switch to hashtables (and weak references?) when
  ;; that library becomes available.
  (cond ((assq uid *uids*) => cdr)
        (else
         (let ((rtds ($bootstrap-rtds)))
           (let lp ((i 0))
             (if (fx=? i (vector-length rtds))
                 #f
                 (let ((rtd (vector-ref rtds i)))
                   (if (eq? (record-type-uid rtd) uid)
                       rtd
                       (lp (fx+ i 1))))))))))

(define (intern-rtd rtd)
  (set! *uids* (cons (cons (record-type-uid rtd) rtd)
                     *uids*)))

;; Find the arguments to make-record-type-descriptor that were used
;; when creating this rtd.
(define (record-type-arguments rtd)
  (values (record-type-name rtd)
          (record-type-parent rtd)
          (record-type-uid rtd)         ;XXX: not portable
          (record-type-sealed? rtd)
          (record-type-opaque? rtd)
          (do ((fields ($record-type-field-names rtd))
               (i 0 (fx+ i 1))
               (spec '() (cons (if (record-field-mutable? rtd i)
                                   `(mutable ,(vector-ref fields i))
                                   `(immutable ,(vector-ref fields i)))
                               spec)))
              ((fx=? i (vector-length fields))
               (list->vector (reverse spec))))))

;; A list of the parent rtds. The grandparent in (rtd-grandparent
;; rtd-parent) will always find itself at index 0 in such a list. This
;; goes onto the end of the rtd box for constant-time type checking of
;; non-sealed records.
(define (rtd-parents parent-rtd)
  (let lp ((rtd parent-rtd) (ret '()))
    (if (not rtd)
        ret
        (lp (record-type-parent rtd) (cons rtd ret)))))

(define ($record-size rtd) ($box-ref rtd rtd:record-size))
(define ($rtd-length rtd) ($box-header-length ($box-type rtd)))
(define ($rtd-mutable rtd) ($box-ref rtd rtd:mutable))
(define ($rtd-parents-ref rtd k) ($box-ref rtd (fx+ k rtd:parents-start)))

(define (make-rtd flags len name parent uid names mutable parents)
  ;; XXX: There are two lengths involved: that of the rtd itself, and
  ;; that of the records it describes.
  (let* ((parents-len (fxmax rtd:min-parents (length parents)))
         (padded-len (fx+ rtd:parents-start parents-len))
         (ret ($make-box ($make-box-header 'rtd #t flags padded-len) padded-len)))
    ($box-set! ret rtd:name name)
    ($box-set! ret rtd:record-size len)
    ($box-set! ret rtd:parent parent)
    ($box-set! ret rtd:uid uid)
    ($box-set! ret rtd:field-names names)
    ($box-set! ret rtd:mutable mutable)
    ($box-set! ret rtd:writer #f)
    (do ((i rtd:parents-start (fx+ i 1))
         (parents parents (cdr parents)))
        ((null? parents))
      ($box-set! ret i (car parents)))
    ret))

(define (make-record-type-descriptor* name parent uid sealed? opaque? generative? fields)
  (define who 'make-record-type-descriptor)

  (define (build-rtd parent-length opaque? generative?)
    (do ((len (vector-length fields))
         (i 0 (fx+ i 1))
         (names (make-vector (vector-length fields)))
         (mut 0
              (case (car (vector-ref fields i))
                ;; Uses a bitmask to indicate mutability.
                ((immutable) mut)
                ((mutable) (bitwise-ior mut (bitwise-arithmetic-shift-left 1 i)))
                (else
                 (assertion-violation who "Incorrect field specification" fields)))))
        ((fx=? i len)
         (make-rtd (fxior (if opaque?     #b001 0)
                          (if sealed?     #b010 0)
                          (if generative? #b100 0))
                   (fx+ (vector-length fields) parent-length)
                   name parent uid names mut
                   (if parent (rtd-parents parent) '())))
      (let ((f (vector-ref fields i)))
        (unless (and (pair? f)
                     (pair? (cdr f))
                     (null? (cddr f))
                     (or (eq? (car f) 'immutable)
                         (eq? (car f) 'mutable))
                     (or (not (procedure? symbol?))
                         (symbol? (cadr f))))
          (assertion-violation who "Expected (mutable name) or (immutable name)"
                               name parent uid sealed? opaque? fields))
        (vector-set! names i (cadr f)))))

  ;; Check that all the little things are what they're supposed to
  ;; be. The contents of fields is checked later.
  (when (and (procedure? symbol?) (not (symbol? name)))
    (assertion-violation who "Expected a symbol as name"
                         name parent uid sealed? opaque? fields))
  (when parent
    (unless (record-type-descriptor? parent)
      (assertion-violation who "Expected an rtd or #f as parent"
                           name parent uid sealed? opaque? fields))
    (when (record-type-sealed? parent)
      (assertion-violation who "Expected an unsealed parent"
                           name parent uid sealed? opaque? fields)))
  (unless (or (not uid) (or (not (procedure? symbol?)) (symbol? uid)))
    (assertion-violation who "Expected the uid argument to be #f or a symbol"
                         name parent uid sealed? opaque? fields))
  (unless (boolean? sealed?)
    (assertion-violation who "Expected the sealed? argument to be a boolean"
                         name parent uid sealed? opaque? fields))
  (unless (boolean? opaque?)
    (assertion-violation who "Expected the opaque? argument to be a boolean"
                         name parent uid sealed? opaque? fields))
  (unless (vector? fields)
    (assertion-violation who "Expected the fields argument to be a vector"
                         name parent uid sealed? opaque? fields))

  ;; See if the rtd already exists or create a new one.
  (cond
    ((and uid (lookup-uid uid)) =>
     (lambda (rtd)
       ;; The requested rtd is nongenerative and already exists.
       ;; Verify that everything matches.
       (let-values ([(name^ parent^ _uid^ sealed?^ opaque?^ fields^)
                     (record-type-arguments rtd)])
         (unless (eq? name name^)
           (assertion-violation who "Generative rtd with a different name"
                                name parent uid sealed? opaque? fields))
         (unless (eq? parent parent^)
           (assertion-violation who "Generative rtd with a different parent"
                                name parent uid sealed? opaque? fields))
         (unless (eq? sealed? sealed?^)
           (assertion-violation who "Generative rtd with a different sealedness"
                                name parent uid sealed? opaque? fields))
         (unless (eq? opaque? opaque?^)
           (assertion-violation who "Generative rtd with a different opaqueness"
                                name parent uid sealed? opaque? fields))
         (unless (equal? fields fields^)
           (assertion-violation who "Generative rtd with different fields"
                                name parent uid sealed? opaque? fields fields^)))
       rtd))

    (else
     (let ((ret (if parent
                    (build-rtd ($record-size parent)
                               (or opaque? (record-type-opaque? parent))
                               generative?)
                    (build-rtd 0 opaque? generative?))))
       (when uid
         ;; Nongenerative record type.
         (intern-rtd ret))
       ret))))

(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  (make-record-type-descriptor* name parent uid sealed? opaque? (not uid) fields))

(define (make-record-type-descriptor/nongenerative name parent uid sealed? opaque? fields)
  (make-record-type-descriptor* name parent uid sealed? opaque? #f fields))

(define (make-record-type-descriptor/generative name parent uid sealed? opaque? fields)
  (make-record-type-descriptor* name parent uid sealed? opaque? #t fields))

(define (record-type-descriptor? obj)
  (and ($box? obj)
       ($box-header-type-eq? ($box-type obj) 'rtd)))

;; (define-record-type (rcd make-rcd* rcd?)
;;   (sealed #t)
;;   (nongenerative loko-rcd-3ca6464b-5f42-47e1-87e6-19a970f345aa)
;;   (fields rtd parent-rcd protocol))

(define &rcd
  (make-record-type-descriptor
   'rcd
   #f
   'loko-rcd-3ca6464b-5f42-47e1-87e6-19a970f345aa
   #t
   #f
   '#((immutable rtd) (immutable parent-rcd) (immutable protocol))))
(define rcd? (record-predicate &rcd))
(define rcd-rtd (record-accessor &rcd 0))
(define rcd-parent-rcd (record-accessor &rcd 1))
(define rcd-protocol (record-accessor &rcd 2))

(define (make-rcd rtd parent-rcd protocol)
  (sys:$record &rcd rtd parent-rcd protocol))

(define (make-record-constructor-descriptor rtd parent-rcd protocol)
  (define who 'make-record-constructor-descriptor)

  (assert (record-type-descriptor? rtd))
  (when parent-rcd
    (assert (rcd? parent-rcd))
    (assert (eq? (rcd-rtd parent-rcd) (record-type-parent rtd))))

  (when protocol
    (assert (procedure? protocol)))

  (let ((parent-rtd (record-type-parent rtd)))
    (when (and parent-rcd (not parent-rtd))
      ;; "If rtd is a base record type then parent-constructor-descriptor must be #f."
      (assertion-violation who "The parent rcd must be #f for base record types"
                           rtd parent-rcd protocol))
    (when (and parent-rcd (not protocol) (rcd-protocol parent-rcd))
      (assertion-violation who "The parent has a protocol so this rcd must also have a protocol"
                           rtd parent-rcd protocol))

    (make-rcd rtd
              (or parent-rcd
                  (and parent-rtd (make-record-constructor-descriptor parent-rtd #f #f)))
              protocol)))

(define (%rtd-make-name rtd)
  (string->symbol (string-append "make-" (symbol->string (record-type-name rtd)))))

(define ($record rtd . x*)
  (let* ((len ($record-size rtd))
         (ret ($make-box rtd len)))
    (do ((i 0 (fx+ i 1))
         (f x* (cdr f)))
        ((fx=? i len)
         (unless (null? f)
           (assertion-violation (%rtd-make-name rtd)
                                "Too many arguments to record constructor"
                                rtd x*))
         ret)
      (when (null? f)
        (assertion-violation (%rtd-make-name rtd)
                             "Too few arguments to record constructor"
                             rtd x*))
      ($box-set! ret i (car f)))))

(define (record-constructor rcd)
  (define who 'record-constructor)
  (define (split-at rtd args i)
    (let lp ((ret '()) (l args) (i i))
      (cond ((eqv? i 0)
             (values (reverse ret) l))
            ((pair? l)
             (lp (cons (car l) ret) (cdr l) (fx- i 1)))
            (else
             (assertion-violation 'record-constructor
                                  "Too few arguments to record constructor"
                                  rtd args)))))

  (define (default-protocol rtd)
    (let ((parent-rtd (record-type-parent rtd)))
      (if parent-rtd
          (let ((parent-size ($record-size parent-rtd)))
            (lambda (p)
              ;; TODO: less consing
              (lambda all-values
                (let-values (((parent-values this-values)
                              (split-at rtd all-values parent-size)))
                  (apply (apply p parent-values) this-values)))))
          (lambda (p) p))))

  (define (rcd-protocol* rcd)
    (or (rcd-protocol rcd)
        (default-protocol (rcd-rtd rcd))))

  (define (base-record-constructor rtd)
    (case ($record-size rtd)
      ((0) (lambda () (sys:$record rtd)))
      ((1) (lambda (a) (sys:$record rtd a)))
      ((2) (lambda (a b) (sys:$record rtd a b)))
      ((3) (lambda (a b c) (sys:$record rtd a b c)))
      ((4) (lambda (a b c d) (sys:$record rtd a b c d)))
      ((5) (lambda (a b c d e) (sys:$record rtd a b c d e)))
      ((6) (lambda (a b c d e f) (sys:$record rtd a b c d e f)))
      ((7) (lambda (a b c d e f g) (sys:$record rtd a b c d e f g)))
      ((8) (lambda (a b c d e f g h) (sys:$record rtd a b c d e f g h)))
      (else
       (lambda (a b c d e f g h . x)
         (let* ((len ($record-size rtd))
                (ret ($make-box rtd len)))
           ($box-set! ret 0 a)
           ($box-set! ret 1 b)
           ($box-set! ret 2 c)
           ($box-set! ret 3 d)
           ($box-set! ret 4 e)
           ($box-set! ret 5 f)
           ($box-set! ret 6 g)
           ($box-set! ret 7 h)
           (do ((i 8 (fx+ i 1))
                (f x (cdr f)))
               ((fx=? i len)
                (unless (null? f)
                  (apply assertion-violation (%rtd-make-name rtd)
                         "Too many arguments to record constructor"
                         rtd a b c d e f g h x))
                ret)
             (when (null? f)
               (apply assertion-violation (%rtd-make-name rtd)
                      "Too few arguments to record constructor"
                      rtd a b c d e f g h x))
             ($box-set! ret i (car f))))))))

  (define (make-make-seeder real-rtd for-desc)
    ;; From the reference implementation of SRFI-76. The license
    ;; below only applies to this complicated procedure. It has
    ;; been modified.
    ;;
    ;; Copyright (C) Michael Sperber (2005). All Rights Reserved.
    ;;
    ;; Permission is hereby granted, free of charge, to any person
    ;; obtaining a copy of this software and associated documentation files
    ;; (the "Software"), to deal in the Software without restriction,
    ;; including without limitation the rights to use, copy, modify, merge,
    ;; publish, distribute, sublicense, and/or sell copies of the Software,
    ;; and to permit persons to whom the Software is furnished to do so,
    ;; subject to the following conditions:
    ;;
    ;; The above copyright notice and this permission notice shall be
    ;; included in all copies or substantial portions of the Software.
    ;;
    ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    ;;, MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    ;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
    ;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
    ;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    ;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    ;; SOFTWARE.
    (let recur ((for-desc for-desc))
      (let ((for-rtd (rcd-rtd for-desc)))
        (cond ((rcd-parent-rcd for-desc) =>
               (lambda (parent-desc)
                 (let ((parent-protocol (rcd-protocol* parent-desc))
                       (parent-make-seeder (recur parent-desc))
                       (for-rtd-count (fx- ($record-size for-rtd)
                                           ($record-size (rcd-rtd parent-desc)))))
                   ;; TODO: less consing
                   (lambda extension-values
                     (lambda parent-protocol-args
                       (lambda for-rtd-values
                         (unless (fx=? (length for-rtd-values) for-rtd-count)
                           (assertion-violation who
                                                "Wrong number of arguments to record constructor"
                                                for-rtd for-rtd-values))
                         (apply (parent-protocol
                                 (apply parent-make-seeder
                                        (append for-rtd-values extension-values)))
                                parent-protocol-args)))))))
              (else
               (let ((for-rtd-count ($record-size for-rtd)))
                 (lambda extension-values
                   ;; Take a shortcut if there are no extension values.
                   (if (null? extension-values)
                       (base-record-constructor real-rtd)
                       (lambda for-rtd-values
                         (unless (fx=? (length for-rtd-values) for-rtd-count)
                           (assertion-violation who
                                                "Wrong number of arguments to record constructor"
                                                for-rtd for-rtd-values))
                         (apply (base-record-constructor real-rtd)
                                (append for-rtd-values extension-values)))))))))))

  (assert (rcd? rcd))
  (let ((rtd (rcd-rtd rcd))
        (protocol (rcd-protocol* rcd)))
    (protocol ((make-make-seeder rtd rcd)))))

(define (record-predicate rtd)
  (if (record-type-sealed? rtd)
      (lambda (obj)
        (and ($box? obj)
             (eq? ($box-type obj) rtd)))
      (if #f
          (lambda (obj)
            (and ($box? obj)
                 (let ((t ($box-type obj)))
                   (or (eq? t rtd)
                       (and (record-type-descriptor? t)
                            (let lp ((t ($record-type-parent t)))
                              (cond ((eq? t rtd) #t)
                                    ((not t) #f)
                                    (else (lp ($record-type-parent t))))))))))
          (let* ((num-parents (let lp ((rtd ($record-type-parent rtd)) (n 0))
                                (if (not rtd)
                                    n
                                    (lp ($record-type-parent rtd) (fx+ n 1)))))
                 (rtd-idx (fx+ num-parents rtd:parents-start)))
            (if (fx<=? num-parents rtd:min-parents)
                (lambda (obj)
                  (and ($box? obj)
                       (let ((t ($box-type obj)))
                         (or (eq? t rtd)
                             (and (record-type-descriptor? t)
                                  (eq? rtd ($box-ref t rtd-idx)))))))
                (lambda (obj)
                  (and ($box? obj)
                       (let ((t ($box-type obj)))
                         (or (eq? t rtd)
                             (and (record-type-descriptor? t)
                                  (and (fx<? rtd-idx ($rtd-length t))
                                       (eq? rtd ($box-ref t rtd-idx)))))))))))))

(define ($record? obj rtd)
  (if (record-type-sealed? rtd)
      (and ($box? obj)
           (eq? ($box-type obj) rtd))
      (and ($box? obj)
           (let ((t ($box-type obj)))
             (or (eq? t rtd)
                 (and (record-type-descriptor? t)
                      (let lp ((t ($record-type-parent t)))
                        (cond ((eq? t rtd) #t)
                              ((not t) #f)
                              (else (lp ($record-type-parent t))))))))))
  #;
  ((record-predicate rtd) obj))

(define (record-field-name rtd k)
  (let lp ((t rtd) (len ($record-size rtd)))
    (let* ((names (record-type-field-names t))
           (idx0 (fx- len (vector-length names))))
      (if (fx>=? k idx0)
          (vector-ref names (fx- k idx0))
          (lp (record-type-parent t) idx0)))))

(define (raise-accessor-error rtd k r)
  ;; XXX: The syntactic layer would know the true name of
  ;; the mutator and would be preferable.
  (let ((fieldname (symbol->string (record-field-name rtd k)))
        (typename (symbol->string (record-type-name rtd))))
    (assertion-violation (string->symbol (string-append typename "-" fieldname))
                         (string-append "Expected a record of type " typename)
                         r)))

(define (record-accessor rtd k)
  (assert (record-type-descriptor? rtd))
  (unless (and (fixnum? k) (fx<? -1 k (vector-length ($record-type-field-names rtd))))
    (assertion-violation 'record-accessor "Invalid field index" rtd k))
  (let* ((prtd ($record-type-parent rtd))
         (k^ (if prtd (fx+ k ($record-size prtd)) k)))
    (assert (fx<? k^ ($record-size rtd)))
    (if (record-type-sealed? rtd)
        (lambda (r)
          (if (and ($box? r) (eq? ($box-type r) rtd))
              ($box-ref r k^)
              (raise-accessor-error rtd k r)))
        (let ((right-type? (record-predicate rtd)))
          (lambda (r)
            (if (right-type? r)
                ($box-ref r k^)
                (raise-accessor-error rtd k r)))))))

(define (raise-mutator-error rtd k r v)
  (let ((fieldname (symbol->string (record-field-name rtd k)))
        (typename (symbol->string (record-type-name rtd))))
    (assertion-violation (string->symbol (string-append typename "-" fieldname "-set!"))
                         (string-append "Expected a record of type " typename)
                         r v)))

(define (record-mutator rtd k)
  (assert (record-type-descriptor? rtd))
  (unless (record-field-mutable? rtd k)
    (assertion-violation 'record-mutator "Expected a mutable field" rtd k))
  (let* ((prtd ($record-type-parent rtd))
         (k^ (if prtd (fx+ k ($record-size prtd)) k)))
    (if (record-type-sealed? rtd)
        (lambda (r v)
          (if (and ($box? r) (eq? ($box-type r) rtd))
              ($box-set! r k^ v)
              (raise-mutator-error rtd k r v)))
        (let ((right-type? (record-predicate rtd)))
          (lambda (r v)
            (if (right-type? r)
                ($box-set! r k^ v)
                (raise-mutator-error rtd k r v)))))))

;;; Inspection

(define (record? x)
  (and ($box? x)
       (let ((t ($box-type x)))
         (and ($box? t)
              ($box-header-type-eq? ($box-type t) 'rtd #b001 #b000)))))

(define (record-rtd* x)
  (if (not ($box? x))
      (assertion-violation 'record-rtd "Expected a record" x)
      (let ((t ($box-type x)))
        (if (not (record-type-descriptor? t))
            (assertion-violation 'record-rtd "Expected a record" x)
            t))))

(define (record-rtd x)
  (let ((t (record-rtd* x)))
    (if (record-type-opaque? t)
        (assertion-violation 'record-rtd "Expected a non-opaque record" x)
        t)))

(define (record-type-name rtd)
  (assert (record-type-descriptor? rtd))
  ($box-ref rtd rtd:name))

(define (record-type-parent rtd)
  (assert (record-type-descriptor? rtd))
  ($box-ref rtd rtd:parent))

(define ($record-type-parent rtd)
  ($box-ref rtd rtd:parent))

(define (record-type-uid rtd)
  (assert (record-type-descriptor? rtd))
  ($box-ref rtd rtd:uid))

(define (record-type-generative? rtd)
  (assert (record-type-descriptor? rtd))
  ($box-header-type-eq? ($box-type rtd) 'rtd #b100 #b100))

(define (record-type-sealed? rtd)
  (assert (record-type-descriptor? rtd))
  ($box-header-type-eq? ($box-type rtd) 'rtd #b010 #b010))

(define (record-type-opaque? rtd)
  (assert (record-type-descriptor? rtd))
  ($box-header-type-eq? ($box-type rtd) 'rtd #b001 #b001))

(define (record-type-field-names rtd)
  (assert (record-type-descriptor? rtd))
  (vector-map values ($box-ref rtd rtd:field-names)))

(define ($record-type-field-names rtd)
  ($box-ref rtd rtd:field-names))

(define (record-field-mutable? rtd k)
  (assert (record-type-descriptor? rtd))
  (unless (and (fixnum? k) (fx<? -1 k (vector-length ($record-type-field-names rtd))))
    (assertion-violation 'record-field-mutable? "Invalid field index" rtd k))
  (bitwise-bit-set? ($rtd-mutable rtd) k))

;;; Record writers

(define (default-record-writer v p wr)
  (let ((t (record-rtd* v)))
    (display "#[" p)
    (display (or (record-type-name t) (record-type-uid t)) p)
    (when (not (record-type-opaque? t))
      (let lp ((t t))
        (when t
          (lp (record-type-parent t))
          (do ((i 0 (fx+ i 1))
               (fields (record-type-field-names t)))
              ((fx=? i (vector-length fields)))
            (let ((field (vector-ref fields i))
                  (value ((record-accessor t i) v)))
              (display #\space p)
              (display field p)
              (display ": " p)
              (wr value p))))))
    (display "]" p)))

(define writers '())                    ;writers for immutable rtds

(define record-writer
  (case-lambda
    ((rtd)
     (assert (record-type-descriptor? rtd))
     (let ((writer ($box-ref rtd rtd:writer)))
       (cond ((procedure? writer) writer)
             ((assq rtd writers) => cdr)
             (else default-record-writer))))
    ((rtd procedure)
     (when (record-type-opaque? rtd)
       (assertion-violation 'record-writer
                            "Tried to set the writer on an opaque rtd"
                            rtd procedure))
     (assert (procedure? procedure))
     (cond
       ((not (eq? ($box-ref rtd rtd:writer) 'immutable))
        ($box-set! rtd rtd:writer procedure))
       (else
        (set! writers (cons (cons rtd procedure) writers))))))))
