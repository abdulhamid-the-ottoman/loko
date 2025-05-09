;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Recover let expressions

(library (loko compiler let)
  (export
    pass-let)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map))

#| This takes a proc and a list of operands, i.e.
   ((case-lambda
      (f1 e1)
      ...
      (fn en))
    arg1 ... argn)
   and returns the proccase that would be called, or #f if none.
   Later when rest lists are handled and no case matches, there is actually
   an error in the user's code and a warning should be issused.
|#
(define (match-formals op args)
  (define (match? info args)
    ;; TODO: here it is possible to match on formals with rest
    ;; arguments as well, but the resultant binding must create a
    ;; list. For now just handle the case that restores let
    ;; expressions.
    (let ((formals (caseinfo-formals info)))
      (and (caseinfo-proper? info)
           (= (length formals)
              (length args)))))
  (let lp ((cases (proc-cases op)))
    (cond ((null? cases)
           #f)
          ((match? (proccase-info (car cases)) args)
           (car cases))
          (else
           (lp (cdr cases))))))

(define (pass-let x)
  (define who 'pass-let)
  (define (pass x)
    ;; (display x)
    ;; (newline)
    (cond ((bind? x)
           (make-bind (bind-lhs* x)
                      (map pass (bind-rhs* x))
                      (pass (bind-body x))))
          ((fix? x)
           (make-fix (fix-lhs* x)
                     (map pass (fix-rhs* x))
                     (pass (fix-body x))))
          ((rec*? x)
           (make-rec* (rec*-lhs* x)
                      (map pass (rec*-rhs* x))
                      (pass (rec*-body x))))
          ((rec? x)
           (make-rec (rec-lhs* x)
                     (map pass (rec-rhs* x))
                     (pass (rec-body x))))
          ((proc? x)
           (make-proc (proc-label x)
                      (proc-end-label x)
                      (map (lambda (x)
                             (make-proccase (proccase-info x)
                                            (pass (proccase-body x))))
                           (proc-cases x))
                      (proc-free x)
                      (proc-name x)
                      (proc-source x)))
          ((seq? x)
           (make-seq (pass (seq-e0 x))
                     (pass (seq-e1 x))))
          ((mutate? x)
           (make-mutate (mutate-name x)
                        (pass (mutate-expr x))))
          ((test? x)
           (make-test (pass (test-expr x))
                      (pass (test-then x))
                      (pass (test-else x))))
          ((funcall? x)
           (let ((op (funcall-operator x))
                 (operands (funcall-operand* x)))
             (cond ((and (proc? op) (match-formals op operands))
                    => (lambda (c)
                         (make-bind (caseinfo-formals (proccase-info c))
                                    (map pass operands)
                                    (pass (proccase-body c)))))
                   (else
                    (make-funcall (pass op)
                                  (map pass operands)
                                  (funcall-label x)
                                  (funcall-source x))))))
          ((mv-call? x)
           (make-mv-call (pass (mv-call-producer x))
                         (pass (mv-call-consumer x))
                         (mv-call-source x)))
          ((mv-let? x)
           (make-mv-let (pass (mv-let-expr x))
                        (mv-let-lhs* x)
                        (pass (mv-let-body x))
                        (mv-let-source x)))
          ((mv-values? x)
           (make-mv-values (map pass (mv-values-expr* x))
                           (mv-values-source x)))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          (else
           (error who "Unknown type" x))))
  (pass x)))
