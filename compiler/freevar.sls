;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Free variable analysis

(library (loko compiler freevar)
  (export
    pass-freevar)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map))

(define (pass-freevar x)
  (define who 'pass-freevar)
  ;; TODO: these are a bit dumb, but they are here so there will not
  ;; be a dependency on srfi-1.
  (define (append-map f x) (apply append (map f x)))
  (define (delete-duplicates x)
    (cond ((null? x)
           '())
          ((memq (car x) (cdr x))
           (delete-duplicates (cdr x)))
          (else
           (cons (car x) (delete-duplicates (cdr x))))))

  (define (find x to-find)
    (define (find x)
      (cond ((ref? x)
             (if (memq (ref-name x) to-find) (list (ref-name x)) '()))
            ((bind? x)
             (append (append-map find (bind-rhs* x))
                     (find (bind-body x))))
            ((fix? x)
             (append (append-map find (fix-rhs* x))
                     (find (fix-body x))))
            ((proc? x)
             (append-map (lambda (x) (find (proccase-body x)))
                         (proc-cases x)))
            ((seq? x)
             (append (find (seq-e0 x))
                     (find (seq-e1 x))))
            ((test? x)
             (append (find (test-expr x))
                     (find (test-then x))
                     (find (test-else x))))
            ((funcall? x)
             (append (find (funcall-operator x))
                     (append-map find (funcall-operand* x))))
            ((mv-call? x)
             (append (find (mv-call-producer x))
                     (find (mv-call-consumer x))))
            ((mv-let? x)
             (append (find (mv-let-expr x))
                     (find (mv-let-body x))))
            ((mv-values? x)
             (append-map find (mv-values-expr* x)))
            ((const? x) '())
            ((primref? x) '())
            (else
             (error who "Unknown type" x))))
    (find x))
  (define (pass x env)
    ;; (write x) (display " - ") (write env)
    ;; (newline)
    (cond ((bind? x)
           (make-bind (bind-lhs* x)
                      (map (lambda (x) (pass x env)) (bind-rhs* x))
                      (pass (bind-body x)
                            (append (bind-lhs* x) env))))
          ((fix? x)
           (let ((env (append (fix-lhs* x) env)))
             (make-fix (fix-lhs* x)
                       (map (lambda (x) (pass x env)) (fix-rhs* x))
                       (pass (fix-body x) env))))
          ((proc? x)
           (let* ((free '())
                  (cases
                   (map (lambda (x)
                          (let* ((new-env (append (caseinfo-formals
                                                   (proccase-info x))
                                                  env))
                                 (new-body (pass (proccase-body x) new-env))
                                 (new-free (find (proccase-body x) env)))
                            (set! free (append new-free free))
                            (make-proccase (proccase-info x) new-body)))
                        (proc-cases x))))
             (make-proc (proc-label x)
                        (proc-end-label x)
                        cases
                        (delete-duplicates free)
                        (proc-name x)
                        (proc-source x))))
          ((seq? x)
           (make-seq (pass (seq-e0 x) env)
                     (pass (seq-e1 x) env)))
          ((test? x)
           (make-test (pass (test-expr x) env)
                      (pass (test-then x) env)
                      (pass (test-else x) env)))
          ((funcall? x)
           (let ((op (funcall-operator x))
                 (operands (funcall-operand* x)))
             (make-funcall (pass op env)
                           (map (lambda (x) (pass x env)) operands)
                           (funcall-label x)
                           (funcall-source x))))
          ((mv-call? x)
           (make-mv-call (pass (mv-call-producer x) env)
                         (pass (mv-call-consumer x) env)
                         (mv-call-source x)))
          ((mv-let? x)
           (make-mv-let (pass (mv-let-expr x) env)
                        (mv-let-lhs* x)
                        (pass (mv-let-body x)
                              (append (mv-let-lhs* x) env))
                        (mv-let-source x)))
          ((mv-values? x)
           (make-mv-values (map (lambda (x) (pass x env)) (mv-values-expr* x))
                           (mv-values-source x)))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          (else
           (error who "Unknown type" x))))
  (pass x '())))
