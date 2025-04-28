;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Search for procedures containing a substring

;; The API should be compatible with that in Chez Scheme, although the
;; textual output is slightly different. It might also include libraries
;; that haven't been imported yet.

(library (loko apropos)
  (export apropos-list apropos)
  (import
    (rnrs)
    (only (loko) installed-libraries interaction-environment
          environment-symbols environment))

;; The lazy man's search algorithm (some say K-M-P no longer even
;; matters, especially on these small strings).
(define (string-contains? text pattern)
  (let lp ((i 0))
    (if (<= (string-length pattern) (- (string-length text) i))
        (if (string=? pattern (substring text i (+ i (string-length pattern))))
            #t
            (lp (+ i 1)))
        #f)))

(define (append-map proc list)
  (apply append (map proc list)))

(define (symbol-sort list)
  (list-sort (lambda (x y)
               (string<? (symbol->string x)
                         (symbol->string y)))
             list))

(define apropos-list
  (case-lambda
    ((s)
     (apropos-list s (interaction-environment)))
    ((s env)
     (let ((s (cond ((symbol? s) (symbol->string s))
                    ((string? s) s)
                    (else
                     (assertion-violation 'apropos-list
                                          "Expected a string or symbol" s)))))
       (append
        (append-map (lambda (name)
                      (if (string-contains? (symbol->string name) s)
                          (list name)
                          '()))
                    (symbol-sort (environment-symbols env)))
        (append-map (lambda (lib)
                      (append-map (lambda (name)
                                    (if (string-contains? (symbol->string name) s)
                                        (list (list lib name))
                                        '()))
                                  (symbol-sort (environment-symbols (environment lib)))))
                    (installed-libraries)))))))

(define apropos
  (case-lambda
    ((s)
     (apropos s (interaction-environment)))
    ((s env)
     (let lp ((x* (apropos-list s env)) (prev-lib #f))
       (unless (null? x*)
         (let ((name (car x*)))
           (cond ((symbol? name)
                  (unless (equal? prev-lib 'interaction-environment)
                    (display "interaction environment:\n"))
                  (display "  ")
                  (write name)
                  (newline)
                  (lp (cdr x*) 'interaction-environment))
                 (else
                  (unless (equal? prev-lib (car name))
                    (write (car name))
                    (display ":\n"))
                  (display "  ")
                  (write (cadr name))
                  (newline)
                  (lp (cdr x*) (car name)))))))))))
