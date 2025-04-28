;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-FileCopyrightText: 2019, 2020, 2021 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+
;;
;; Parts of call/cc are based on code from SLIB's dynwind.scm:
;; SPDX-FileCopyrightText: 1992, 1993 Aubrey Jaffer
;; SPDX-License-Identifier: LicenseRef-SLIB
;;
;; This file is a part of Loko Scheme, an R6RS Scheme system
#!r6rs

;;; Standard library control operators

(library (loko runtime control)
  (export
    procedure?
    apply values call-with-values
    call/cc (rename (call/cc call-with-current-continuation)) call/1cc
    dynamic-wind
    with-exception-handler raise raise-continuable
    assertion-violation error
    make-promise force

    ;; Internal
    implementation-restriction
    register-error-invoker
    print-condition
    assertion-error
    undefined-variable
    current-winders
    $values)
  (import
    (except (rnrs)
            procedure?
            apply values call-with-values
            call/cc call-with-current-continuation
            dynamic-wind
            with-exception-handler raise raise-continuable
            assertion-violation error)
    (only (loko runtime conditions) make-continuation-condition
          continuation-condition? condition-continuation)
    (only (loko) parameterize make-parameter)
    (prefix (only (rnrs) procedure? apply call-with-values values) sys:)
    (except (loko system $primitives) $values)
    (prefix (only (loko system $primitives) $values) sys:)
    (loko system $host)
    (only (loko runtime context)
          PROCESS-VECTOR:ERROR-INVOKER)
    (only (laesare reader) annotation-source->condition))

(define (procedure? x) (sys:procedure? x))

(define apply
  (case-lambda
    ((f rest) (sys:apply f rest))
    ((f a rest) (sys:apply f a rest))
    ((f a b rest) (sys:apply f a b rest))
    ((f a b c rest) (sys:apply f a b c rest))
    ((f a b c d rest) (sys:apply f a b c d rest))
    ((f a b c d e rest) (sys:apply f a b c d e rest))
    ((f a b c d e . rest)
     (sys:apply f a b c d e
                (let lp ((rest rest))
                  (cond ((pair? (cdr rest))
                         ;; Move this element into the rest list. It
                         ;; was cons'd up by consargs.
                         (cons (car rest) (lp (cdr rest))))
                        (else
                         ;; The last element that was actually
                         ;; provided sent to apply and not constructed
                         ;; by consargs.
                         (car rest))))))))

(define (call-with-values x y)
  (sys:call-with-values x y))

;; This should also work:
;; (define (values . things)
;;    (call/cc (lambda (cont) (apply cont things))))

(define values
  (case-lambda
    (()                            (sys:values))
    ((a)                           a)
    ((a b)                         (sys:values a b))
    ((a b c)                       (sys:values a b c))
    ((a b c d)                     (sys:values a b c d))
    ((a b c d e)                   (sys:values a b c d e))
    ((a b c d e f)                 (sys:values a b c d e f))
    ((a b c d e f g)               (sys:values a b c d e f g))
    ((a b c d e f g h)             (sys:values a b c d e f g h))
    ((a b c d e f g h i)           (sys:values a b c d e f g h i))
    ((a b c d e f g h i j)         (sys:values a b c d e f g h i j))
    ((a b c d e f g h i j k)       (sys:values a b c d e f g h i j k))
    ;; TODO: do this without consing. This conses up a list just to
    ;; turn it back to values (but it works, so it's fine for now)
    (x*                            (sys:$values x*))))

(define ($values x*)
  (sys:$values x*))

;;; Continuations, dynamic-wind, exceptions.

(define *winders* '())

(define current-winders
  (case-lambda
    (() *winders*)
    ((winders) (set! *winders* winders))))

;; TODO: look at june-92-meeting.ps.gz

(define (call/cc proc)
  (let ((k ($copy-stack)))
    ;; The $copy-stack call returns multiple times.
    ;; The first time it returns a copy of the stack.
    ;; The following times it returns values via $restore-stack.
    (cond ((and ($box? k) (eq? ($box-type k) 'stack))
           ;; Based on code from SLIB's dynwind.scm.
           ;;
           ;; Copyright (c) 1992, 1993 Aubrey Jaffer
           ;;
           ;; Permission to copy this software, to modify it, to redistribute it,
           ;; to distribute modified versions, and to use it for any purpose is
           ;; granted, subject to the following restrictions and understandings.
           ;;
           ;; 1.  Any copy made of this software must include this copyright notice
           ;; in full.
           ;;
           ;; 2.  I have made no warranty or representation that the operation of
           ;; this software will be error-free, and I am under no obligation to
           ;; provide any services, by way of maintenance, update, or otherwise.
           ;;
           ;; 3.  In conjunction with products arising from the use of this
           ;; material, there shall be no use of my name in any advertising,
           ;; promotional, or sales literature without prior written consent in
           ;; each case.
           (letrec ((old-winders *winders*)
                    (do-winds
                     (lambda (to delta)
                       (cond ((eq? *winders* to))
                             ((fxnegative? delta)
                              (do-winds (cdr to) (fx+ delta 1))
                              ((caar to))
                              (set! *winders* to))
                             (else
                              (let ((from (cdar *winders*)))
                                (set! *winders* (cdr *winders*))
                                (from)
                                (do-winds to (fx- delta 1)))))))
                    (continuation
                     (lambda v*
                       (do-winds old-winders
                                 (fx- (length *winders*)
                                      (length old-winders)))
                       ;; TODO: Avoid consing here
                       ($restore-stack k v*))))
             (proc continuation)))
          (else
           (apply values k)))))

(define call/1cc call/cc)

(define (dynamic-wind before thunk after)
  (before)
  (set! *winders* (cons (cons before after)
                        *winders*))
  (let-values ((v (thunk)))
    (set! *winders* (cdr *winders*))
    (after)
    (apply values v)))

;;; Exceptions

(define (default-exception-handler x)
  (define EX_SOFTWARE 70)            ;Linux: internal software error
  (define p (transcoded-port (standard-error-port) (native-transcoder)))
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda () (k))
        (lambda ()
          (flush-output-port (current-output-port))
          (flush-output-port (current-error-port))))))
  (display "An exception has been raised, but no exception handler is installed.\n" p)
  (if (continuation-condition? x)
      (let ((k (condition-continuation x)))
        (stack-trace k p))
      (display "No stack trace was captured.\n" p))
  (print-condition x p)
  (flush-output-port p)
  (when (serious-condition? x)
    (exit EX_SOFTWARE)))

(define *exception-handlers*
  (make-parameter (list default-exception-handler
                        (lambda _ (exit 70)))))

(define (with-exception-handler handler thunk)
  (assert (procedure? handler))
  (assert (procedure? thunk))
  (parameterize ([*exception-handlers* (cons handler (*exception-handlers*))])
    (thunk)))

(define (raise obj)
  (let ((handlers (*exception-handlers*)))
    (parameterize ([*exception-handlers* (cdr handlers)])
      ((car handlers) obj)
      (raise (condition
              (make-non-continuable-violation))))))

(define (raise-continuable obj)
  (let ((handlers (*exception-handlers*)))
    (parameterize ([*exception-handlers* (cdr handlers)])
      ((car handlers) obj))))

(define (error who msg . irritants)
  (call/cc
    (lambda (k)
      (raise (condition
              (make-error)
              (if who (make-who-condition who) (condition))
              (make-message-condition msg)
              (make-irritants-condition irritants)
              (make-continuation-condition k))))))

(define (assertion-error expr pos)
  (call/cc
    (lambda (k)
      (raise (condition
              (make-assertion-violation)
              (make-who-condition 'assert)
              (make-message-condition "Assertion failed")
              (make-irritants-condition (list expr))
              (annotation-source->condition pos)
              (make-continuation-condition k))))))

(define (undefined-variable variable-name)
  (call/cc
    (lambda (k)
      (raise (condition
              (make-assertion-violation)
              (make-message-condition "Undefined variable referenced or mutated")
              (make-irritants-condition (list variable-name))
              (make-continuation-condition k))))))

(define (assertion-violation who msg . irritants)
  (define EX_SOFTWARE 70)            ;Linux: internal software error
  (cond
    ((not (procedure? condition))
     (string-for-each (lambda (c) ($debug-display c))
                      "Early assertion violation: ")
     (string-for-each (lambda (c) ($debug-display c))
                      msg)
     ($debug-display #\newline)
     ($debug-display who)
     ($debug-display #\newline)
     ($debug-display irritants)
     ($debug-display #\newline)
     (exit EX_SOFTWARE))             ;XXX: exit might not work
    (who
     (call/cc
       (lambda (k)
         (raise (condition
                 (make-assertion-violation)
                 (make-who-condition who)
                 (make-message-condition msg)
                 (make-irritants-condition irritants)
                 (make-continuation-condition k))))))
    (else
     (call/cc
       (lambda (k)
         (raise (condition
                 (make-assertion-violation)
                 (make-message-condition msg)
                 (make-irritants-condition irritants)
                 (make-continuation-condition k))))))))

(define (implementation-restriction who msg . irritants)
  (call/cc
    (lambda (k)
      (raise (condition
              (make-implementation-restriction-violation)
              (make-who-condition who)
              (make-message-condition msg)
              (make-irritants-condition irritants)
              (make-continuation-condition k))))))

;; Prints conditions with portable R6RS code. It does not print the
;; &condition type. It also prints e.g. "&who:" since that condition
;; is named &who and has a field named who, which is quite common.
(define (print-condition exn p)
  (cond ((condition? exn)
         ;; TODO: does this have to consider opaque condition types?
         (let ((c* (simple-conditions exn)))
           (display "The condition has " p)
           (display (length c*) p)
           (display " components:\n" p)
           (do ((i 1 (fx+ i 1))
                (c* c* (cdr c*)))
               ((null? c*))
             (let* ((c (car c*))
                    (rtd (record-rtd c)))
               (display " " p) (display i p) (display ". " p)
               (let ((suppress-type
                      (and (eq? (record-type-parent rtd)
                                (record-type-descriptor &condition))
                           (let ((name (symbol->string (record-type-name rtd)))
                                 (fields (record-type-field-names rtd)))
                             (and (not (eqv? 0 (string-length name)))
                                  (char=? (string-ref name 0) #\&)
                                  (fx>? (vector-length fields) 0)
                                  (string=? (substring name 1 (string-length name))
                                            (symbol->string (vector-ref fields 0))))))))
                 (if suppress-type
                     (put-char p #\&)
                     (let loop ((rtd rtd))
                       (display (record-type-name rtd) p)
                       (cond ((record-type-parent rtd) =>
                              (lambda (rtd)
                                (unless (eq? rtd (record-type-descriptor &condition))
                                  (display #\space p)
                                  (loop rtd)))))))
                 (let loop ((rtd rtd))
                   (do ((f* (record-type-field-names rtd))
                        (i 0 (fx+ i 1)))
                       ((fx=? i (vector-length f*))
                        (cond ((record-type-parent rtd) => loop)))
                     (unless (and suppress-type (eqv? i 0))
                       (display "\n     " p))
                     (display (vector-ref f* i) p)
                     (display ": " p)
                     (let ((x ((record-accessor rtd i) c)))
                       (cond ((and (eq? rtd (record-type-descriptor &irritants))
                                   (pair? x) (list? x))
                              (display #\( p)
                              ;; TODO: limited pretty-printing
                              (write (car x) p)
                              (for-each (lambda (x)
                                          (display "\n                 " p)
                                          (write x p))
                                        (cdr x))
                              (display #\) p))
                             ((and (eq? '&program-counter (record-type-name rtd))
                                   (fixnum? x))
                              (display "#x" p)
                              (display (number->string x 16) p))
                             (else
                              (write x p))))))))
             (newline p)))
         (display "End of condition components.\n" p))
        (else
         (display "A non-condition object was raised:\n" p)
         (write exn p)
         (newline p))))

;;; Handler for hardware traps and explicit traps in generated code

;; The error invoker is supposed to be called when there's a trap.
;; Any errors trapped before this is useable results in double
;; traps. Any errors trapped before this runs result in a panic.
(define (register-error-invoker x)
  ($pcb-set! PROCESS-VECTOR:ERROR-INVOKER x))

;;; Promises

;; From r6rs-lib

(define force
  (lambda (object)
    (object)))

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
          (result #f))
      (lambda ()
        (if result-ready?
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result-ready? #t)
                         (set! result x)
                         result)))))))))
