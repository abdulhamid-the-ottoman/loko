;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2022 G. Weinholt
#!r6rs

;;; Scheme I/O ports, printing

(library (loko runtime io-printer)
  (export
    newline
    (rename (display* display)
            (write* write))

    find-cycles                         ;also used by pretty-print
    write-shared
    write-simple
    print-dialects
    open-textual-null-port)
  (import
    (except (rnrs) newline)
    (prefix (rnrs io simple) sys:)
    (only (loko runtime arithmetic) $display-number)
    (only (loko runtime symbols) $gensym-generate-names!)
    (only (loko runtime records) record-writer)
    (loko compiler compat)
    (only (loko) make-parameter parameterize
          port-name port-file-descriptor port-closed?)
    (only (loko system $primitives)
          $void? $void->fixnum
          $immsym? $immsym->fixnum
          $procedure-info
          $box? $box-type $box-header-type-eq? $box-ref))

(define print-dialects
  (make-parameter '()
                  (lambda (new)
                    (assert (for-all symbol? new))
                    new)))

(define newline
  (case-lambda
    (() (put-char (current-output-port) #\linefeed))
    ((p) (put-char p #\linefeed))))

(define-syntax %bytevector
  (lambda (x)
    (syntax-case x ()
      ((_ v)
       (string? (syntax->datum #'v))
       (string->utf8 (syntax->datum #'v))))))

(define (display-hex24 i p)
  ;; Displays 24-bit integers in hexadecimal notation
  (define chars (%bytevector "0123456789ABCDEF"))
  (when (eqv? i 0) (put-char p #\0))
  (let lp ((s 24) (lz #t))
    (unless (fx<? s 0)
      (let* ((nibble (fxand (fxarithmetic-shift-right i s) #xf))
             (lz (and lz (eqv? nibble 0))))
        (when (not lz)
          (put-char p (integer->char (bytevector-u8-ref chars nibble))))
        (lp (fx- s 4) lz)))))

(define (display-char-escape c p)
  (put-char p #\\)
  (put-char p #\x)
  (display-hex24 (char->integer c) p)
  (put-char p #\;))

(define (char-initial? c)
  (or (char<=? #\a c #\z)
      (char<=? #\A c #\Z)
      (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
      (and (fx>? (char->integer c) 127)
           (memq (char-general-category c)
                 '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co)))))

(define (char-subsequent? c)
  (or (char<=? #\a c #\z)
      (char<=? #\A c #\Z)
      (char<=? #\0 c #\9)
      (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                #\+ #\- #\. #\@))
      (and (fx>? (char->integer c) 127)
           (memq (char-general-category c)
                 '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co
                      Nd Mc Me)))))

(define (print-symbol sym p write?)
  (define latin1
    (cond ((port-transcoder p) =>
           (lambda (tc)
             (eq? (latin-1-codec) (transcoder-codec tc))))
          (else #f)))
  (let ((len (string-length (symbol->string sym))))
    (cond ((eqv? len 0) (put-string p "||")) ;not in R6RS
          ((eq? sym '+) (put-string p "+"))
          ((eq? sym '-) (put-string p "-"))
          ((eq? sym '...) (put-string p "..."))
          (else
           (let ((c0 (string-ref (symbol->string sym) 0)))
             (if (or (and (char-initial? c0)
                          (or (not latin1) (char<=? c0 #\xff)))
                     (and (eqv? c0 #\-)
                          (fx>=? len 2)
                          (eqv? (string-ref (symbol->string sym) 1) #\>)
                          #;
                          (let lp ((i 2))
                            (cond ((fx=? i len)
                                   #t)
                                  ((char-subsequent? (string-ref (symbol->string sym) i))
                                   (lp (fx+ i 1)))
                                  (else #f)))))
                 (put-char p c0)
                 (display-char-escape c0 p)))
           (do ((i 1 (fx+ i 1)))
               ((fx=? i len))
             (let ((c (string-ref (symbol->string sym) i)))
               (if (or (not (char-subsequent? c))
                       (and latin1 (char>? c #\xff)))
                   (display-char-escape c p)
                   (put-char p c))))))))

(define (display** v p write?)
  (define (char-unprintable? c)
    ;; TODO: unicode
    ;; some are unprintable as chars, some in strings too.
    ;; which ones are printable depends on the encoding
    ;; of the output port!
    (let ((v (char->integer c)))
      (or (fx<? v (char->integer #\space))
          (fx<=? #x7f v #x9f))))
  (define chars
    '#("nul" #f #f #f #f #f #f "alarm" "backspace" "tab"
       "linefeed" "vtab" "page" "return" #f #f #f #f #f #f #f #f
       #f #f #f #f #f "esc" #f #f #f #f "space" #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f "delete"))
  (define string-escapes
    (%bytevector
     "XXXXXXXabtnvfrXXXXXXXXXXXXXXXXXXXX\"XXXXXXXXXXXXXXXXXXXXX\
        XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\\"))
  (cond
    ((char? v)
     (cond (write?
            (put-char p #\#)
            (put-char p #\\)
            (let ((i (char->integer v)))
              (cond ((and (fx<? i (vector-length chars))
                          (vector-ref chars i))
                     => (lambda (str) (put-string p str)))
                    ((char-unprintable? v)
                     (put-char p #\x)
                     (display-hex24 i p))
                    (else
                     (put-char p v)))))
           (else
            (put-char p v))))
    ((bytevector? v)
     (if (memq 'r7rs (print-dialects))
         (display "#u8(" p)
         (display "#vu8(" p))
     (let ((len (bytevector-length v)))
       (unless (eqv? len 0)
         (display (bytevector-u8-ref v 0) p)
         (do ((i 1 (fx+ i 1)))
             ((fx=? i len))
           (put-char p #\space)
           (display (bytevector-u8-ref v i) p))))
     (put-char p #\)))
    ((null? v)
     (put-char p #\()
     (put-char p #\)))
    ((boolean? v)
     (put-char p #\#)
     (if v (put-char p #\t) (put-char p #\f)))
    (($immsym? v)
     (let ((alphabet     "abcdefghijklmnopqrstuvwxyz-/<=>")
           (end-alphabet "acdefghklmnopqrstvxy!*+-/08<=>?"))
       (let ((s ($immsym->fixnum v)))
         (let ((s (fxarithmetic-shift-right s 5))
               (c (fx- (fxand s #b11111) 1)))
           ;; The first character might need to be escaped
           (let ((ch (string-ref (if (eqv? s 0) end-alphabet alphabet) c)))
             (if (and write?
                      (or (memv ch '(#\0 #\8))
                          ;; + is not escaped, but + as a prefix is escaped
                          (and (eqv? ch #\+) (not (eqv? s 0)))
                          ;; - is not escaped, but - as a prefix is
                          ;; escaped, except -> which is not escaped
                          (and (eqv? ch '#\-)
                               (not (or (eqv? s 0)
                                        (eqv? (string-ref (symbol->string v) 1) #\>))))))
                 (display-char-escape ch p)
                 (put-char p ch)))
           ;; Output the rest of the characters, if there are any
           (let lp ((s s))
             (unless (eqv? s 0)
               (let ((s (fxarithmetic-shift-right s 5))
                     (c (fx- (fxand s #b11111) 1)))
                 (cond ((eqv? s 0)
                        (put-char p (string-ref end-alphabet c)))
                       (else
                        (put-char p (string-ref alphabet c))
                        (lp s))))))))))
    ((string? v)
     (if write? (put-char p #\"))
     (do ((len (string-length v))
          (i 0 (fx+ i 1)))
         ((fx=? i len))
       (let* ((c (string-ref v i))
              (v (char->integer c)))
         (cond ((and write? (fx<? v (bytevector-length string-escapes))
                     (not (eqv? (bytevector-u8-ref string-escapes v)
                                (char->integer #\X))))
                (put-char p #\\)
                (put-char p (integer->char (bytevector-u8-ref string-escapes v))))
               ((and write? (char-unprintable? c))
                (display-char-escape c p))
               (else
                (put-char p c)))))
     (if write? (put-char p #\")))
    ((pair? v)
     (cond ((and (pair? (cdr v))
                 (null? (cddr v))
                 (assq (car v) '((quote . "'")
                                 (quasiquote . "`")
                                 (unquote . ",")
                                 (unquote-splicing . ",@")
                                 (syntax . "#'")
                                 (quasisyntax . "#`")
                                 (unsyntax . "#,")
                                 (unsyntax-splicing . "#,@"))))
            => (lambda (a)
                 (display (cdr a) p)
                 (display** (cadr v) p write?)))
           (else
            (put-char p #\()
            (let lp ((i v))
              (unless (null? i)
                (display** (car i) p write?)
                (let ((i (cdr i)))
                  (cond ((null? i))
                        ((pair? i)
                         (put-char p #\space)
                         (lp i))
                        (else
                         (put-char p #\space)
                         (put-char p #\.)
                         (put-char p #\space)
                         (display** i p write?))))))
            (put-char p #\)))))
    ((vector? v)
     (put-char p #\#) (put-char p #\()
     (let ((len (vector-length v)))
       (unless (eqv? len 0)
         (display** (vector-ref v 0) p write?)
         (do ((i 1 (fx+ i 1)))
             ((fx=? i len))
           (put-char p #\space)
           (display** (vector-ref v i) p write?))))
     (put-char p #\)))
    ((number? v)
     ($display-number v p))
    ((procedure? v)
     (let ()
       (define-record-type info
         (sealed #t)
         (nongenerative loko-procinfo-555bfd14-d155-420f-a058-8ccd8ab0301e)
         (fields name (mutable free-length) source label end-label))
       (let ((info ($procedure-info v)))
         (cond ((info? info)
                (if (eqv? (info-free-length info) 0)
                    (display "#<procedure " p)
                    (display "#<closure " p))
                (if (info-name info)
                    (display (info-name info) p)
                    (display "(anonymous)" p))
                (when (info-source info)
                  (put-char p #\space)
                  (display (vector-ref (info-source info) 0) p)
                  (put-char p #\:)
                  (display (vector-ref (info-source info) 1) p)
                  (put-char p #\:)
                  (display (vector-ref (info-source info) 2) p)))
               (else
                ;; really shouldn't happen
                (display "#<procedure>" p)))))
     (display ">" p))
    ((port? v)
     ;; [binary|textual]-[input|output|input/output]-port
     (display "#<" p)
     (if (textual-port? v)
         (display "textual-" p)
         (display "binary-" p))
     (if (input-port? v)
         (if (output-port? v)
             (display "input/output-port " p)
             (display "input-port " p))
         (display "output-port " p))
     (write (port-name v) p)
     (when (output-port? v)
       (display " buffer-mode: " p)
       (display (output-port-buffer-mode v) p))
     (cond ((port-file-descriptor v) =>
            (lambda (fd)
              (display " fd: " p)
              (display fd p))))
     (when (port-closed? v)
       (display " (closed)" p))
     (display #\> p))
    (($box? v)
     (let ((t ($box-type v)))
       (cond (($box-header-type-eq? t 'symbol)
              (cond ((and write? (gensym? v))
                     ;; Gensym write syntax
                     (let ((symbol-name (gensym-prefix v))
                           (unique-string (gensym->unique-string v)))
                       (display "#{" p)
                       (display symbol-name p)
                       (display " |" p)
                       (display unique-string p)
                       (display "|}" p)))
                    (else
                     (print-symbol v p write?))))
             ((record-type-descriptor? t) ;v is a record
              (let ((writer (record-writer t)))
                (writer v p write)))
             (($box-header-type-eq? t 'rtd) ;v is an rtd
              (display "#<rtd " p)
              (write ($box-ref v 0) p)
              (display ">" p))
             (else
              (display "#<" p)
              (display t p)
              (display ">" p)))))
    ((eof-object? v)
     (display "#!eof" p))
    (($void? v)
     (display "#<void #x" p)
     (display (number->string ($void->fixnum v) 16) p)
     (display ">" p))
    (else
     (display "#<unknown>" p))))

(define display*
  (case-lambda
    ((v) (display** v (current-output-port) #f))
    ((v p) (display** v p #f))))

(define write*
  (case-lambda
    ((v)
     (write v (current-output-port)))
    ((v p)
     (let ((counter -1)
           (shared (find-cycles v)))
       (if (eqv? 0 (hashtable-size shared))
           (display** v p #t)
           (letrec ((increment!
                     (lambda ()
                       (set! counter (fx+ counter 1))
                       counter)))
             ;; Cycles were detected.
             (write/c v p shared increment!)))))))

(define write-shared
  (case-lambda
    ((v)
     (write-shared v (current-output-port)))
    ((v p)
     (let ((counter -1)
           (shared (find-shared v)))
       (if (eqv? 0 (hashtable-size shared))
           (display** v p #t)
           (letrec ((increment!
                     (lambda ()
                       (set! counter (fx+ counter 1))
                       counter)))
             ;; Shared structures were detected.
             (write/c v p shared increment!)))))))

(define write-simple
  (case-lambda
    ((v)
     (write-simple v (current-output-port)))
    ((v p)
     (display** v p #t))))

;; Print with cycles in pairs, vectors and records.
(define (write/c v p shared increment!)
  (define (write-prefix id)
    (put-char p #\#)
    (display id p)
    (put-char p #\=))
  (define (write-ref id)
    (put-char p #\#)
    (display id p)
    (put-char p #\#))
  (define (number! v)
    (let ((id (hashtable-ref shared v #f)))
      (when (eq? id 'must-assign)
        (let ((id (increment!)))
          (write-prefix id)
          (hashtable-set! shared v id)))
      id))
  (let ((id (number! v)))
    (cond
      ((fixnum? id)
       (write-ref id))
      ((pair? v)
       (cond ((and (pair? (cdr v))
                   (null? (cddr v))
                   (assq (car v) '((quote . "'")
                                   (quasiquote . "`")
                                   (unquote . ",")
                                   (unquote-splicing . ",@")
                                   (syntax . "#'")
                                   (quasisyntax . "#`")
                                   (unsyntax . "#,")
                                   (unsyntax-splicing . "#,@"))))
              => (lambda (a)
                   (display (cdr a) p)
                   (write/c (cadr v) p shared increment!)))
             (else
              ;; FIXME: cycles are not detected here
              (put-char p #\()
              (let lp ((i v))
                (unless (null? i)
                  (write/c (car i) p shared increment!)
                  (let ((i (cdr i)))
                    (cond ((null? i))
                          ((pair? i)
                           (let ((id (number! i)))
                             (cond ((fixnum? id)
                                    (put-char p #\space)
                                    (put-char p #\.)
                                    (put-char p #\space)
                                    (write-ref id))
                                   (else
                                    (put-char p #\space)
                                    (lp i)))))
                          (else
                           (put-char p #\space)
                           (put-char p #\.)
                           (put-char p #\space)
                           (write/c i p shared increment!))))))
              (put-char p #\)))))
      ((vector? v)
       (put-char p #\#) (put-char p #\()
       (let ((len (vector-length v)))
         (unless (eqv? len 0)
           (write/c (vector-ref v 0) p shared increment!)
           (do ((i 1 (fx+ i 1)))
               ((fx=? i len))
             (put-char p #\space)
             (write/c (vector-ref v i) p shared increment!))))
       (put-char p #\)))
      ((record? v)
       (let* ((t ($box-type v))
              (writer (record-writer t)))
         (writer v p (lambda (v p) (write/c v p shared increment!)))))
      (else
       (write v p)))))

(define (open-textual-null-port)
  (define (write! _buf _start count) count)
  (define (read! _buf _start _count) 0)
  (make-custom-textual-input/output-port "*null*" read! write! #f #f #f))

;; Build a hashtable that maps objects to 'must-assign if they appear
;; inside themselves.
(define (find-cycles v)
  (define dummy (open-textual-null-port))
  (let ((s (make-eq-hashtable)))
    (let f ((v v))
      (when (or (pair? v) (vector? v) (record? v))
        (cond ((hashtable-ref s v #f) =>
               (lambda (id)
                 (when (eq? id 'unassigned)
                   (hashtable-set! s v 'must-assign))))
              ((pair? v)
               (hashtable-set! s v 'unassigned)
               (f (car v))
               (f (cdr v)))
              ((vector? v)
               (when (fxpositive? (vector-length v))
                 (hashtable-set! s v 'unassigned)
                 (vector-for-each f v)))
              ((record? v)
               (hashtable-set! s v 'unassigned)
               (let* ((t ($box-type v))
                      (writer (record-writer t)))
                 (writer v dummy (lambda (v _) (f v))))))
        (when (eq? (hashtable-ref s v #f) 'unassigned)
          ;; The object does not contain a reference to itself.
          (hashtable-set! s v #f))))
    s))

;; Build a hashtable that maps (some) objects to 'must-assign if they
;; appear more than once in the structure.
(define (find-shared v)
  (define dummy (open-textual-null-port))
  (let ((s (make-eq-hashtable)))
    (let f ((v v))
      (when (or (pair? v)
                (and (vector? v) (not (eqv? 0 (vector-length v))))
                (record? v)
                (and (string? v) (not (eqv? 0 (string-length v)))))
        (cond ((hashtable-ref s v #f) =>
               (lambda (id)
                 (when (eq? id 'unassigned)
                   (hashtable-set! s v 'must-assign))))
              ((pair? v)
               (hashtable-set! s v 'unassigned)
               (f (car v))
               (f (cdr v)))
              ((vector? v)
               (when (fxpositive? (vector-length v))
                 (hashtable-set! s v 'unassigned)
                 (vector-for-each f v)))
              ((record? v)
               (hashtable-set! s v 'unassigned)
               (let* ((t ($box-type v))
                      (writer (record-writer t)))
                 (writer v dummy (lambda (v _) (f v)))))
              ((string? v)
               (hashtable-set! s v 'unassigned)))))
    s)))
