;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Simplifications of quasiquoted expressions

(library (loko compiler quasiquote)
  (export
    pass-quasiquote)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map)
    (loko match))

(define (cons-call? x)
  (and (funcall? x)
       (primref? (funcall-operator x))
       (eq? 'cons (primref-name (funcall-operator x)))))

(define (cons*-call? x)
  (and (funcall? x)
       (primref? (funcall-operator x))
       (eq? 'cons* (primref-name (funcall-operator x)))))

(define (pass-quasiquote x)
  (define who 'pass-quasiquote)
  (define (pass2 x)
    (cond
      ((cons-call? x)
       (match (funcall-operand* x)
         [(a (? cons-call? b))
          ;; (cons a (cons b c))
          ;; => (cons* a b c)
          (make-funcall (make-primref 'cons*)
                        (cons a (funcall-operand* b))
                        (funcall-label x)
                        (funcall-source x))]
         [(a (? cons*-call? b))
          ;; (cons a (cons* x ...))
          ;; => (cons* a x ...)
          (make-funcall (make-primref 'cons*)
                        (cons a (funcall-operand* b))
                        (funcall-label x)
                        (funcall-source x))]
         [_ x]))
      (else x)))
  (define (pass x)
    ;; (write (record->sexpr x))
    ;; (write x)
    ;; (newline)
    (cond ((bind? x)
           (make-bind (bind-lhs* x)
                      (map pass (bind-rhs* x))
                      (pass (bind-body x))))
          ((fix? x)
           (make-fix (fix-lhs* x)
                     (map pass (fix-rhs* x))
                     (pass (fix-body x))))
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
          ((test? x)
           (make-test (pass (test-expr x))
                      (pass (test-then x))
                      (pass (test-else x))))
          ((funcall? x)
           (let ((op (funcall-operator x))
                 (operands (map pass (funcall-operand* x)))
                 (source (funcall-source x)))
             ;; cp0 has done constant propagation, so more parts of
             ;; quasiquoted data can be constant now
             (cond
               ((primref? op)
                (let-values ([(proc name)
                              (case (primref-name op)
                                ((qcons) (values cons 'cons))
                                ((qlist) (values list 'list))
                                ((qappend) (values append 'append))
                                ((qvector) (values vector 'vector))
                                ((qlist->vector) (values list->vector 'list->vector))
                                (else (values #f #f)))])
                  (if name
                      (match operands
                        [((? const? x*) ...)
                         ;; (write (cons name (map (lambda (x) (list 'quote x))
                         ;;                        (map const-value x*))))
                         ;; (newline)
                         (guard (exn ((assertion-violation? exn)
                                      (pass2
                                       (make-funcall (make-primref name)
                                                     operands
                                                     (funcall-label x)
                                                     source))))
                           (make-const (apply proc (map const-value x*)) #f))]
                        [e*
                         (pass2
                          (make-funcall (make-primref name)
                                        operands
                                        (funcall-label x)
                                        source))])
                      (pass2
                       (make-funcall (pass op) operands
                                     (funcall-label x)
                                     source)))))
               (else
                (pass2
                 (make-funcall (pass op) operands
                               (funcall-label x)
                               source))))))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          (else
           (error who "Unknown type" x))))
  (pass x)))
