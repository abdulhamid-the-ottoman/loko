;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Multiple values handling

(library (loko compiler values)
  (export
    pass-values)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map)
    (loko match))

;; XXX: Copied from (loko compiler let)
(define (match-formals op args)
  (define (match? info args)
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

;; XXX: Only the open-* procedures really work as producers.
(define multiple-valued-primitives
  '(open-bytevector-output-port
    open-string-output-port
    partition
    div-and-mod
    div0-and-mod0
    $fxquo+rem
    fxdiv-and-mod
    fxdiv0-and-mod0
    fx*/carry
    fx+/carry
    fx-/carry
    fldiv-and-mod0
    fldiv0-and-mod0
    exact-integer-sqrt
    hashtable-entries
    call/cc
    call-with-current-continuation
    apply
    dynamic-wind
    call-with-values
    values
    eval
    cpuid
    force
    $values
    rdrand rdseed
    page-table-lookup))

(define (single-valued? x)
  (cond ((const? x) #t)
        ((ref? x) #t)
        ((mutate? x) #t)
        ((proc? x) #t)
        ((bind? x) (single-valued? (bind-body x)))
        ((fix? x) (single-valued? (fix-body x)))
        ((test? x) (and (single-valued? (test-then x))
                        (single-valued? (test-else x))))
        ((and (funcall? x) (primref? (funcall-operator x))
              (eq? 'dynamic-wind (primref-name (funcall-operator x))))
         (match (funcall-operand* x)
           [(before (? proc? thunk) after)
            (for-all (lambda (x) (single-valued? (proccase-body x)))
                     (proc-cases thunk))]
           [_ #f]))
        ((and (funcall? x) (primref? (funcall-operator x))
              (not (memq (primref-name (funcall-operator x))
                         multiple-valued-primitives)))
         #t)
        (else #f)))

(define (mk-mv-call producer consumer source)
  (cond
    ;; (mv-call ((lambda () e)) consumer) =>
    ;;    (mv-call e consumer)
    ((and (funcall? producer)
          (null? (funcall-operand* producer))
          (proc? (funcall-operator producer))
          (match-formals (funcall-operator producer) '()))
     => (lambda (proccase)
          (let ((info (proccase-info proccase)))
            (if (null? (caseinfo-formals info))
                (mk-mv-call (proccase-body proccase) consumer source)
                producer))))
    ;; (mv-call e_s consumer) =>
    ;;   (consumer e_s)
    ((single-valued? producer)
     (make-funcall consumer (list producer) #f source))
    ;; (mv-call (mv-values e ...) consumer) =>
    ;;    (consumer e ...)
    ((mv-values? producer)
     (make-funcall consumer (mv-values-expr* producer) #f source))
    ;; (mv-call (let ((lhs* rhs*) ...) body) consumer) =>
    ;;    (let ((lhs* rhs*) ...) (mv-call body consumer))
    ((and (bind? producer) (proc? consumer))
     (let ((body (mk-mv-call (bind-body producer) consumer source)))
       (and body (make-bind (bind-lhs* producer) (bind-rhs* producer) body))))
    ;; (mv-call (let ((lhs* rhs*) ...) body) consumer) =>
    ;;    (fix ((lhs* rhs*) ...) (mv-call body consumer))
    ((fix? producer)
     (let ((body (mk-mv-call (fix-body producer) consumer source)))
       (and body (make-fix (fix-lhs* producer) (fix-rhs* producer) body))))
    ;; (mv-call expr (lambda (id ...) body)) =>
    ;;    (mv-let expr (id ...) body)
    ((and (funcall? producer) (proc? consumer) (eqv? 1 (length (proc-cases consumer)))
          (caseinfo-proper? (proccase-info (car (proc-cases consumer)))))
     (let ((c (car (proc-cases consumer))))
       (make-mv-let producer (caseinfo-formals (proccase-info c))
                    (proccase-body c) (proc-source consumer))))
    (else
     (if (funcall? producer)
         (make-mv-call producer consumer source)
         #f))))

(define (pass-values x)
  (define who 'pass-values)
  (define (pass x tail?)
    ;; (write (record->sexpr x))
    ;; (write x)
    ;; (newline)
    (cond ((bind? x)
           (make-bind (bind-lhs* x)
                      (map (lambda (x) (pass x #f)) (bind-rhs* x))
                      (pass (bind-body x) tail?)))
          ((fix? x)
           (make-fix (fix-lhs* x)
                     (map (lambda (x) (pass x #f)) (fix-rhs* x))
                     (pass (fix-body x) tail?)))
          ((proc? x)
           (make-proc (proc-label x)
                      (proc-end-label x)
                      (map (lambda (x)
                             (make-proccase (proccase-info x)
                                            (pass (proccase-body x) 'tail)))
                           (proc-cases x))
                      (proc-free x)
                      (proc-name x)
                      (proc-source x)))
          ((seq? x)
           (make-seq (pass (seq-e0 x) #f)
                     (pass (seq-e1 x) tail?)))
          ((test? x)
           (make-test (pass (test-expr x) #f)
                      (pass (test-then x) tail?)
                      (pass (test-else x) tail?)))
          ((funcall? x)
           (let ((op (funcall-operator x))
                 (operands (funcall-operand* x))
                 (source (funcall-source x)))
             (cond
               ((and (primref? op) (eq? (primref-name op) 'values))
                (match operands
                  [()
                   (if (eq? tail? 'tail)
                       (make-mv-values '() source)
                       (make-funcall (make-primref 'void) '() #f source))]
                  [(e) (pass e tail?)]
                  [e*
                   (if (eq? tail? 'tail)
                       (make-mv-values (map (lambda (x) (pass x tail?)) e*) source)
                       (let lp ((e* (reverse e*)))
                         ;; TODO: Only residualize for effect. Perhaps
                         ;; move this pass to before cp0.
                         (if (null? (cdr e*))
                             (pass (car e*) #f)
                             (make-seq (pass (car e*) #f)
                                       (lp (cdr e*))))))]))
               ((and (primref? op) (eq? (primref-name op) 'call-with-values))
                (or (match operands
                      [(producer consumer)
                       (mk-mv-call (make-funcall (pass producer #f) '() #f source)
                                   (pass consumer tail?)
                                   source)]
                      [_ #f])
                    (make-funcall (pass op #f) (map (lambda (x) (pass x #f)) operands)
                                  (funcall-label x) source)))
               (else
                (make-funcall (pass op #f) (map (lambda (x) (pass x tail?)) operands)
                              (funcall-label x)
                              source)))))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          (else
           (error who "Unknown type" x))))
  (pass x #f)))
