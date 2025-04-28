;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019 G. Weinholt
#!r6rs

;;; Generate frame size tables for stack unwinding

;; The stack unwinding tables are used when there is a hardware trap
;; (e.g. caused by taking the car of a non-pair). At the time of the
;; trap it is unknown which parts of the stack frame have live
;; references and which may contain any old junk. The error handler
;; needs to be able to invoke the GC, print stack traces, etc. If the
;; error occurs in the scheduler it is not possible to do online
;; liveness analysis, so these tables are used to simply remove the
;; frame in which the trap occurred.

;; It is assumed that traps can't happen outside the body of the
;; procedure (i.e. outside the instructions that adjust rsp), except
;; for the final jump in a tail call. The user of these tables must
;; recognize those indirect jump instructions. Procedures hand-written
;; in assembler must not create their own stack frames.

;; All cases in a case-lambda must have the same frame size, because
;; here they are grouped by the procedure/procedure-end marks.

;; XXX: These tables need to be extended by the online compiler later.

(library (loko arch amd64 tables)
  (export
    table-generator)
  (import
    (only (loko arch amd64 objects) immediate)
    (rnrs))

;; The idea is to pick out all the procedures in the text, sort them
;; by frame size and generate a table that maps address ranges to
;; frame sizes. The table is prepended to the data.

(define (get-procedure text)
  (define who 'get-procedure)
  (cond ((not (equal? (car text) '(%comment procedure)))
         (error who "Junk between procedures" text))
        (else
         (let lp ((proc (list (car text)))
                  (text (cdr text)))
           (cond ((null? text)
                  (error who "Procedure terminated early"))
                 ((equal? (car text) '(%comment procedure-end))
                  (values (cons (car text) proc)
                          (cdr text)))
                 (else
                  (lp (cons (car text) proc)
                      (cdr text))))))))

(define (get-frame-size proc)
  ;; Find and return i from `(%comment frame-size ,i)
  (let lp ((proc proc))
    (cond ((pair? proc)
           (let ((i (car proc)))
             (if (and (pair? i) (pair? (cdr i))
                      (eq? (car i) '%comment)
                      (eq? (cadr i) 'frame-size))
                 (caddr i)
                 (lp (cdr proc)))))
          (else 0))))

(define (table-generator text data)
  (define by-frame-size (make-eqv-hashtable))
  (let lp ((text text))
    (cond ((pair? text)
           (let-values (((proc text*) (get-procedure text)))
             ;; proc is in reverse
             (let ((size (get-frame-size proc)))
               (hashtable-update! by-frame-size size
                                  (lambda (x)
                                    (append proc x))
                                  '())
               (lp text*))))
          (else
           ;; Rebuild the text
           (let ((sizes (hashtable-keys by-frame-size)))
             (vector-sort! < sizes)
             (let lp ((text '()) (i 0) (table '()))
               (cond ((fx<? i (vector-length sizes))
                      ;; The procedures have been grouped by frame
                      ;; size. procs is in reverse. Text and table
                      ;; are built up in reverse order.
                      (let* ((size (vector-ref sizes i))
                             (procs (hashtable-ref by-frame-size size 'empty-bucket))
                             (last-label (cadr (assq '%label procs)))
                             (first-label (cadr (assq '%label (reverse procs)))))
                        (lp (append procs text)
                            (fx+ i 1)
                            `((%u64 ,(immediate size))
                              (%u64 (bitwise-arithmetic-shift-left ,last-label 3))
                              (%u64 (bitwise-arithmetic-shift-left ,first-label 3))
                              ,@table))))
                     (else
                      (let ((table `((%align 8 0)
                                     (%label bootstrap-unwind-table)
                                     (%u64 ,(immediate (length table)))
                                     ,@(reverse table))))
                        (values (reverse text)
                                (append table data))))))))))))
