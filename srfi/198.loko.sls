;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; SRFI-198 Foreign Interface Error Handling

;; Uses R6RS standard conditions where equivalents exist.

;; Up to date with draft #3.

(library (srfi :198)
  (export
    foreign-error?
    foreign-error:error-set
    foreign-error:code
    foreign-error:scheme-procedure
    foreign-error:foreign-interface
    foreign-error:message
    foreign-error:data

    make-foreign-error
    raise-foreign-error)
  (import
    (rnrs)
    (srfi :198 private))

(define-condition-type &foreign-error &error
  make-foreign-error* foreign-error?
  (error-set foreign-error:error-set*)
  (code foreign-error:code*)
  (foreign-interface foreign-error:foreign-interface*)
  (data foreign-error:data*))

(define (foreign-error:error-set x)
  (cond ((foreign-error? x) (foreign-error:error-set* x))
        ((syscall-errno-condition? x) 'errno)
        (else 'error)))

(define (foreign-error:code x)
  (cond ((foreign-error? x) (foreign-error:code* x))
        ((syscall-errno-condition? x)
         `((number . ,(condition-syscall-errno x))
           (symbol . ,(string->symbol
                       (string-append "errno/" (symbol->string
                                                (condition-syscall-symbol x)))))))
        (else #f)))

(define (foreign-error:foreign-interface x)
  (cond ((foreign-error? x) (foreign-error:foreign-interface* x))
        ((syscall-errno-condition? x) (condition-syscall-function x))
        (else #f)))

(define (foreign-error:scheme-procedure x)
  (and (who-condition? x) (condition-who x)))

(define (foreign-error:message x)
  (cond ((message-condition? x) (condition-message x))
        ((syscall-errno-condition? x) (condition-syscall-message x))
        (else #f)))

(define (foreign-error:data x)
  (let ((data (append (if (irritants-condition? x)
                          (list 'arguments (condition-irritants x))
                          '())
                      (or (and (foreign-error? x) (foreign-error:data* x)) '()))))
    (if (null? data) #f data)))

(define (make-foreign-error alist)
  (define error-set 'error)
  (define code #f)
  (define foreign-interface #f)
  (define data #f)
  (define who-cond (condition))
  (define message-cond (condition))
  (define irritants-cond (condition))
  (cond
    ((and (list? alist) (for-all pair? alist))
     (for-each
      (lambda (x)
        (let ((arg (cdr x)))
          (case (car x)
            ((error-set) (set! error-set arg))
            ((code) (set! code arg))
            ((scheme-procedure)
             (set! who-cond (make-who-condition arg)))
            ((foreign-interface)
             (set! foreign-interface arg))
            ((message)
             (set! message-cond (make-message-condition arg)))
            ((data)
             (set! data (remp (lambda (x) (eq? (car x) 'arguments)) arg))
             (cond
               ((and (list? arg) (for-all pair? arg) (assq 'arguments arg))
                => (lambda (irritants*)
                     (set! irritants-cond
                           (make-irritants-condition (cdr irritants*))))))))))
      alist))
    (else
     (set! who-cond (make-who-condition 'make-foreign-error))))
  (condition who-cond message-cond irritants-cond
             (make-foreign-error* error-set code foreign-interface data)))

(define raise-foreign-error
  (case-lambda
    ((alist)
     (raise-foreign-error alist #f))
    ((alist continuable)
     ((if continuable raise-continuable raise)
      (make-foreign-error alist))))))
