;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; REPL and related stuff.

(library (loko runtime repl)
  (export
    banner repl load)
  (import
    (rnrs)
    (rnrs eval)
    (only (loko runtime conditions) continuation-condition?
          condition-continuation)
    (only (loko runtime control) print-condition)
    (only (loko system $primitives) $void?)
    (only (loko system $host) stack-trace)
    (only (psyntax expander) interaction-environment new-interaction-environment)
    (only (psyntax compat) make-parameter parameterize)
    (only (loko) loko-version pretty-print)
    (only (loko runtime reader) read-annotated))

(define (banner p)
  (display "Loko Scheme " p)
  (display (loko-version) p)
  (display " <https://scheme.fail/>" p)
  (display "
Copyright © 2023 G. Weinholt
Licensed under the EUPL-1.2-or-later.\n\n" p))

(define env (make-parameter #f))

(define (repl)
  (define (repl-internal)
    (call/cc
      (lambda (k)
        (with-exception-handler
          (lambda (exn)
            ;; TODO: flush output ports?
            (let ((p (current-error-port)))
              (when (continuation-condition? exn)
                (let ((k (condition-continuation exn)))
                  (stack-trace k p)))
              (print-condition exn p))
            (when (serious-condition? exn)
              (k 'restart)))
          (lambda ()
            (let loop ()
              (flush-output-port (current-error-port))
              (display "> ")
              (flush-output-port (current-output-port))
              (let ((datum (read-annotated (current-input-port) "<repl>")))
                (cond ((eof-object? datum)
                       (display "\nEnd of file read.\n")
                       'exit)
                      (else
                       (call-with-values
                         (lambda () (eval datum (env)))
                         (case-lambda
                           (() #f)
                           ((x)
                            (unless ($void? x)
                              (pretty-print x)))
                           (x*
                            (for-each pretty-print x*))))
                       (loop))))))))))
  (unless (interaction-environment)
    (interaction-environment (new-interaction-environment)))
  (parameterize ((env (interaction-environment)))
    (let lp ()
      (case (repl-internal)
        ((restart) (lp))
        (else #f)))))

(define load
  (case-lambda
    ((filename)
     (unless (env)
       (error 'load "Load can only be called from a repl" filename))
     (load filename (env)))
    ((filename environ)
     (call-with-input-file filename
       (lambda (p)
         (let lp ()
           (let ((datum (read-annotated p filename)))
             (unless (eof-object? datum)
               (eval datum environ)
               (lp))))))))))
