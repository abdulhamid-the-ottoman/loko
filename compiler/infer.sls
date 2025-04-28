;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Simple(!) type inference

;; This introduces infer records. If the infer information part of the
;; record is is not wanted, then just use infer-expr.

;; TODO: "Flow Sensitive Type Recovery In Linear-Log Time"

(library (loko compiler infer)
  (export
    pass-infer)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (loko match)
    (except (rnrs) map)
    (rnrs mutable-pairs))

;; TODO: do this properly! Here and in pass-cp0.
(define (target-fixnum? x)
  (fixnum? x))

(define (target-least-fixnum)
  (least-fixnum))

(define (target-greatest-fixnum)
  (greatest-fixnum))

(define (infer op type)
  ;; This expression gets its own instance of the information. If
  ;; the information is copied, these same conses are reused. If
  ;; there is a mutation, then all copies of the information are
  ;; modified appropriately.
  (make-infer op (list type)))

(define (copy dst src)
  (if (infer? src)
      (make-infer dst (infer-facts src))
      dst))

(define (constant? x)
  (or (const? x)
      (and (infer? x)
           (const? (infer-expr x)))))

(define (inferred-positive-fixnum? x)
  (or (and (constant? x)
           (let ((v (constant-value x)))
             (and (target-fixnum? v) (positive? v))))
      (and (infer? x)
           (let ((facts (infer-facts x)))
             (and (memq 'positive facts)
                  (memq 'fixnum facts))))))

(define (constant-positive-fixnum? x)
  (and (infer? x)
       (let ((e (infer-expr x)))
         (and (const? e)
              (target-fixnum? (const-value e))))))

(define (constant-fixnum-vector? x)
  (and (infer? x)
       (let ((e (infer-expr x)))
         (and (const? e)
              (let ((v (const-value e)))
                (and (vector? v)
                     (let lp ((i 0))
                       (or (fx=? i (vector-length v))
                           (and (target-fixnum? (vector-ref v i))
                                (lp (fx+ i 1)))))))))))

(define (constant-value x)
  (if (infer? x)
      (constant-value (infer-expr x))
      (const-value x)))

(define (pass-infer x)
  (define who 'pass-infer)
  (define (pass x env)
    (cond ((bind? x)
           (let ((lhs*^ (bind-lhs* x))
                 (rhs*^ (map (lambda (e)
                               (pass e env))
                             (bind-rhs* x))))
             (let ((env^ (append (map cons lhs*^ rhs*^) env)))
               (let ((body^ (pass (bind-body x) env^)))
                 (copy (make-bind lhs*^ rhs*^ body^)
                       body^)))))
          ((fix? x)
           (let ((body^ (pass (fix-body x) env)))
             (copy (make-fix (fix-lhs* x)
                             (map (lambda (e) (pass e env)) (fix-rhs* x))
                             body^)
                   body^)))
          ((proc? x)
           (make-proc
            (proc-label x)
            (proc-end-label x)
            (map (lambda (x)
                   (make-proccase (proccase-info x)
                                  (pass (proccase-body x) env)))
                 (proc-cases x))
            (proc-free x)
            (proc-name x)
            (proc-source x)))

          ((seq? x)
           (let ((e1^ (pass (seq-e1 x) env)))
             (copy (make-seq (pass (seq-e0 x) env)
                             e1^)
                   e1^)))
          ((test? x)
           (let ((expr^ (pass (test-expr x) env)))
             (let ((then^ (pass (test-then x) env))
                   (else^ (pass (test-else x) env)))
               (make-test expr^ then^ else^))))
          ((funcall? x)
           (let f ((op^ (pass (funcall-operator x) env))
                   (operand*^ (map (lambda (e) (pass e env)) (funcall-operand* x))))
             (let ((x^ (make-funcall op^
                                     operand*^
                                     (funcall-label x)
                                     (funcall-source x))))
               (case (and (primref? op^) (primref-name op^))
                 ((integer->char)
                  (infer x^ 'char))
                 ((char->integer)
                  (make-infer x^ (list 'fixnum 'positive)))
                 ((string-ref)
                  (infer x^ 'char))
                 ((bytevector-length string-length vector-length
                                     length)
                  (infer x^ 'fixnum))
                 ((bytevector-u8-ref get-mem-u8 get-i/o-u8)
                  (infer x^ 'fixnum))
                 ((bytevector-u16-ref bytevector-u16-native-ref get-mem-u16 get-i/o-u16)
                  (infer x^ 'fixnum))
                 ((bytevector-u32-ref bytevector-u32-native-ref get-mem-u32 get-i/o-u32)
                  (assert (target-fixnum? (- (expt 2 32) 1)))
                  (infer x^ 'fixnum))
                 ((get-mem-s61 rdtsc)
                  (assert (= (target-greatest-fixnum)
                             (- (expt 2 60) 1)))
                  (infer x^ 'fixnum))
                 ((bytevector-s8-ref)
                  (infer x^ 'fixnum))
                 ((bytevector-s16-ref bytevector-s16-native-ref)
                  (infer x^ 'fixnum))
                 ((bytevector-s32-ref bytevector-s32-native-ref)
                  (if (target-fixnum? (- (expt 2 31) 1))
                      (infer x^ 'fixnum)
                      x^))
                 ((bytevector-ieee-single-native-ref
                   bytevector-ieee-single-ref)
                  (infer x^ 'flonum))
                 ((fxmax fxmin fx+ fx* fx- fxdiv0 fxmod0
                         fxnot fxand fxior fxxor fxif
                         fxcopy-bit fxbit-field fxcopy-bit-field
                         fxarithmetic-shift fxarithmetic-shift-left
                         fxarithmetic-shift-right
                         fxrotate-bit-field fxreverse-bit-field
                         fxfirst-bit-set fxlength)
                  (infer x^ 'fixnum))
                 ((fxdiv fxmod)
                  (if (for-all inferred-positive-fixnum? operand*^)
                      (make-infer x^ (list 'fixnum 'positive))
                      (infer x^ 'fixnum)))
                 ((real->flonum flmax flmin fl+ fl* fl- fl/ flabs fldiv flmod
                                fldiv0 flmod0 flnumerator fldenominator
                                flfloor flceiling fltruncate flround
                                flexp fllog flsin flcos fltan
                                flasin flacos flatan flsqrt flexpt
                                fixnum->flonum)
                  (infer x^ 'flonum))
                 ((vector-ref)
                  ;; TODO: fails if immutability of vector constants
                  ;; is not enforced, which it should be.
                  (if (and (pair? operand*^)
                           (constant-fixnum-vector? (car operand*^)))
                      (infer x^ 'fixnum)
                      x^))
                 ((bitwise-and)
                  (if (exists constant-positive-fixnum? operand*^)
                      (infer x^ 'fixnum)
                      x^))
                 ((bitwise-bit-field)
                  (match operand*^
                    [(_ (? constant-positive-fixnum? start^)
                        (? constant-positive-fixnum? end^))
                     (let ((start (constant-value start^))
                           (end (constant-value end^)))
                       (cond ((and (<= start end)
                                   (< (- end start) (fixnum-width))
                                   (infer x^ 'fixnum)))
                             (else x^)))]
                    [_ x^]))
                 ;; Loko stuff.
                 ((syscall bytevector-address
                           $immsym->fixnum
                           $void->fixnum
                           get-random-u8
                           $box-header-length
                           $box-header-value)
                  (infer x^ 'fixnum))
                 ;; Alternative calling conventions, higher order stuff.
                 ((apply fold-left fold-right)
                  (if (pair? operand*^)
                      ;; This is inaccurate.
                      (copy x^ (f (car operand*^) (cdr operand*^)))
                      x^))
                 (else x^)))))
          ((mv-call? x)
           (make-mv-call (pass (mv-call-producer x) env)
                         (pass (mv-call-consumer x) env)
                         (mv-call-source x)))
          ((mv-let? x)
           ;; XXX: missed opportunity
           (make-mv-let (pass (mv-let-expr x) env)
                        (mv-let-lhs* x)
                        (pass (mv-let-body x) env)
                        (mv-let-source x)))
          ((mv-values? x)
           (make-mv-values (map (lambda (x) (pass x env))
                                (mv-values-expr* x))
                           (mv-values-source x)))
          ((const? x)
           (let ((v (const-value x)))
             (cond
               ((target-fixnum? v)
                (if (positive? v)
                    (make-infer x (list 'fixnum 'positive))
                    (infer x 'fixnum)))
               ((flonum? v)
                (infer x 'flonum))
               ;; ((char? v)
               ;;  (infer x 'char))
               ((vector? v)
                (infer x 'vector))
               (else
                x))))
          ((ref? x)
           ;; XXX: Mutation has been changed into pairs already,
           ;; except for tagbody.
           (cond ((assq (ref-name x) env)
                  => (lambda (rhs)
                       (copy x (cdr rhs))))
                 (else
                  x)))
          ((primref? x) x)
          ((goto? x) x)
          ((tagbody? x)
           (let ((body^ (pass (tagbody-body x) env)))
             (copy (make-tagbody (tagbody-label x)
                                 body^
                                 (tagbody-source x))
                   body^)))
          ((mutate? x)
           ;; The mutation in a tagbody.
           ;; (write (map (lambda (l/r)
           ;;               (cons (record->sexpr (car l/r))
           ;;                     (record->sexpr (cdr l/r))))
           ;;             env))
           ;; (newline)
           ;; (write (record->sexpr x)) (newline)
           (let ((rhs (pass (mutate-expr x) env))
                 (v (cdr (assq (mutate-name x) env))))
             (when (infer? v)
               (when (or (not (infer? rhs))
                         (not (equal? (infer-facts v)
                                      (infer-facts rhs))))
                 (let ((facts (infer-facts v)))
                   ;; TODO: It would be better to not put
                   ;; infers on constants.
                   (when (constant? v)
                     (set-infer-facts! v (map values (infer-facts v))))
                   (when (constant? rhs)
                     (set-infer-facts! rhs (map values (infer-facts rhs))))
                   (cond
                     ((and (infer? rhs)
                           (eq? (car (infer-facts v))
                                (car (infer-facts rhs))))
                      ;; (fixnum positive) is rewritten to (fixnum)
                      (set-cdr! facts '()))
                     (else
                      (set-car! facts 'object)
                      (set-cdr! facts '()))))))
             (make-mutate (mutate-name x) rhs)))
          (else
           (error who "Unknown type" x))))
  (pass x '())))
