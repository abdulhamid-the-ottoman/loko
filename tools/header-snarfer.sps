#!/usr/bin/env scheme-script
;; -*- coding: utf-8; mode: scheme -*-
;; Copyright © 2018, 2019, 2020, 2021, 2022 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+

;;; C header snarfer for syscall ABIs

;; Most or all of these defines should come from linux-libc-dev, but
;; in actuality a lot of it is just not there and has to be taken from
;; from libc instead.

(import (rnrs))

(define library-prefix
  (cddr (command-line)))

(define instructions
  (call-with-input-file (cadr (command-line))
    (lambda (p)
      (let lp ()
        (let ((x (read p)))
          (if (eof-object? x)
              '()
              (cons x (lp))))))))

(define (print . x) (for-each display x) (newline))

(define (print-include x)
  (when (pair? x)
    (print "#ifdef " (cadr x)))
  (print "#include <" (if (pair? x) (car x) x) ">")
  (when (pair? x)
    (print "#endif")))

(define (includes instructions)
  (for-each
   (lambda (x)
     (when (pair? x)
       (case (car x)
         ((ifdef)
          (print "#ifdef " (cadr x)))
         ((c-include)
          (for-each (lambda (name) (print "#define " name)) (cddr x))
          (print-include (cadr x))
          (for-each (lambda (name) (print "#undef " name)) (cddr x)))
         ((endif)
          (print "#endif")))))
   instructions))

(define (lib-start prefix instructions imports)
  (print "    printf(\"(library (\");")
  (for-each (lambda (x)
              (print "    printf(\"" x " \");"))
            prefix)
  (cond
    ((member "arch" prefix)
     (print "#ifdef __amd64__")
     (print "    printf(\"amd64\");")
     (print "#endif")
     (print "#ifdef __aarch64__")
     (print "    printf(\"aarch64\");")
     (print "#endif")
     (print "    struct utsname uts;")
     (print "    uname(&uts);")
     (print "    for (char *p=uts.sysname; *p; p++) *p = tolower(*p);")
     (print "    printf(\" %s\", uts.sysname);")
     (print "    printf(\"-numbers)\\n\");"))
    (else
     (print "    printf(\"numbers)\\n\");")))
  (print "    printf(\"  (export\");")
  (for-each
   (lambda (x)
     (if (pair? x)
         (case (car x)
           ((ifdef)
            (print "#ifdef " (cadr x)))
           ((endif)
            (print "#endif"))
           ((struct struct* struct/sizeof)
            (let ((struct (cadr x))
                  (members (cddr x)))
              (print "    printf(\"\\n    sizeof-" struct "\");")
              (for-each
               (lambda (member)
                 (print "    printf(\"\\n    offsetof-" struct "-" member "\");")
                 (when (eq? (car x) 'struct/sizeof)
                   (print "    printf(\"\\n    sizeof-" struct "-" member "\");")))
               members)))
           ((sizeof)
            (let ((type (cadr x)))
              (print "    printf(\"\\n    sizeof-" type "\");")))
           ((define)
            (print "    printf(\"\\n    " (cadr x) "\");")))
         (print "    printf(\"\\n    " x "\");")))
   instructions)
  (print "    printf(\")\\n\");")
  (print "    printf(\"  (import\");")
  (for-each (lambda (imp)
              (print "    printf(\" " imp "\");"))
            imports)
  (print "    printf(\")\\n\");")
  (printf)
  (printf "(define-syntax define-inlined")
  (printf "  (syntax-rules ()")
  (printf "    ((_ name v)")
  (printf "     (define-syntax name (identifier-syntax v)))))"))

(define (lib-end)
  (print "    printf(\")\\n\");"))

(define (printf-const fmt name)
  (print "    printf(\"(define-inlined " name " " fmt ")\\n\", " name ");"))

(define (printf-struct prefix struct members)
  (print "    printf(\"(define-inlined sizeof-" struct
         " %lu)\\n\", sizeof(" prefix struct "));")
  (for-each
   (lambda (member)
     (print "    printf(\"(define-inlined offsetof-" struct "-" member
            " %lu)\\n\", offsetof(" prefix struct ", " member "));"))
   members))

(define (printf-struct/sizeof prefix struct members)
  (print "    printf(\"(define-inlined sizeof-" struct
         " %lu)\\n\", sizeof(" prefix struct "));")
  (for-each
   (lambda (member)
     (print "    printf(\"(define-inlined offsetof-" struct "-" member
            " %lu)\\n\", offsetof(" prefix struct ", " member "));")
     (print "    printf(\"(define-inlined sizeof-" struct "-" member
            " %lu)\\n\", sizeof(((" prefix struct "*) NULL)-> " member "));"))
   members))

(define (printf-sizeof type)
  (print "    printf(\"(define-inlined sizeof-" type
         " %lu)\\n\", sizeof(" type "));"))

(define (printf-define fmt what as)
  (print "    printf(\"(define-inlined " what " " fmt
         ")\\n\", " as ");"))

(define (printf . x*)
  (print "    printf(\"" (apply string-append
                                (map (lambda (x)
                                       (if (symbol? x)
                                           (symbol->string x)
                                           x))
                                     x*))
         "\\n\");"))

(define (return v)
  (print "    return " v ";"))

(print-include "ctype.h")                     ;tolower
(print-include "stdio.h")                     ;printf
(print-include "sys/utsname.h")               ;uname
(print "#undef __GLIBC__")
(includes instructions)
(print "#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)")

(print "int main(int argc, char *argv[]) {")

(printf ";; -*- mode: scheme; coding: utf-8 -*-")
(printf ";; SPDX-FileCopyrightText: none")
(printf ";; SPDX-License-Identifier: MIT")
(printf ";; Automatically generated by header-snarfer.scm")
(printf ";; This file is not a creative work.")
(printf "#!r6rs")
(printf "")

(lib-start library-prefix
           (if (port-eof? (current-input-port))
               instructions
               (cons 'errno-list instructions))
           '((rnrs (6))))

(let lp ((format "%d") (x* instructions))
  (unless (null? x*)
    (let ((x (car x*)))
      (cond ((pair? x)
             (case (car x)
               ((fmt)
                (lp (cadr x) (cdr x*)))
               ((ifdef)
                (print "#ifdef " (cadr x))
                (lp format (cdr x*)))
               ((endif)
                (print "#endif")
                (lp format (cdr x*)))
               ((c-include)
                (printf "\\n;;; " (cadr x))
                (lp "%d" (cdr x*)))
               ((struct)
                (printf-struct "struct " (cadr x) (cddr x))
                (lp format (cdr x*)))
               ((struct*)               ;typedef'd struct
                (printf-struct "" (cadr x) (cddr x))
                (lp format (cdr x*)))
               ((struct/sizeof)
                (printf-struct/sizeof "struct " (cadr x) (cddr x))
                (lp format (cdr x*)))
               ((sizeof)
                (printf-sizeof (cadr x))
                (lp format (cdr x*)))
               ((define)
                (printf-define format (cadr x) (caddr x))
                (lp format (cdr x*)))
               ((comment)
                (printf ";; " (cadr x))
                (lp format (cdr x*)))
               (else
                (lp format (cdr x*)))))
            (else
             (printf-const format x)
             (lp format (cdr x*)))))))

(unless (port-eof? (current-input-port))
  ;; Suddenly something completely different.
  (let ((table (make-eqv-hashtable)))
    (display "header-snarfer: Getting errno definitions from stdin...\n"
             (current-error-port))
    (printf)
    (let lp-next ((max-errno 0))
      (let ((line (get-line (current-input-port))))
        (cond
          ((eof-object? line)
           (printf "(define errno-list")
           (printf "  '#(")
           (do ((i 0 (+ i 1)))
               ((> i max-errno))
             (cond ((hashtable-ref table i #f) =>
                    (lambda (sym.str)
                      (printf "     "
                              "(" (car sym.str) " . "
                              "\\\"" (cdr sym.str) "\\\""
                              ")")))
                   (else
                    (printf "     #f"))))
           (printf "))"))
          (else
           (let* ((p (open-string-input-port line))
                  (ch (get-char p)))
             (cond ((eqv? ch #\#)
                    (let ((x (get-datum p)))
                      (case x
                        ((define)
                         (let* ((sym (get-datum p))
                                (num (get-datum p))
                                (/* (get-datum p)))
                           (let lp ()
                             (let ((c (peek-char p)))
                               (when (and (char? c) (char-whitespace? c))
                                 (get-char p)
                                 (lp))))
                           (let ((comment (get-line p)))
                             (cond ((number? num)
                                    (let ((comment (substring comment 0 (- (string-length comment) 3))))
                                      (hashtable-set! table num (cons sym comment))
                                      (lp-next (max num max-errno))))
                                   (else
                                    (lp-next max-errno))))))
                        (else
                         (lp-next max-errno)))))
                   (else
                    (lp-next max-errno))))))))))

(lib-end)
(return 0)
(print "}")
