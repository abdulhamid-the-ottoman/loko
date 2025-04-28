;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Central point for logging systems

;; This is not a full logging system. It is meant to be used by any
;; kind of library that needs to generate logs. Applications can hook
;; it up to a log originator.

;; Any extra fields passed to send-log should use the conventions from
;; systemd.journal-fields(7).

(library (loko runtime logging)
  (export
    send-log current-log-fields current-log-callback
    severity->symbol
    EMERGENCY ALERT CRITICAL ERROR WARNING NOTICE INFO DEBUG)
  (import
    (rnrs (6))
    (only (loko) make-parameter parameterize)
    (loko queues)
    (loko system time))

;; These severities are from RFC 5424 ("The Syslog Protocol").
(define EMERGENCY 0)                ; system is unusable
(define ALERT 1)                    ; action must be taken immediately
(define CRITICAL 2)                 ; critical conditions
(define ERROR 3)                    ; error conditions
(define WARNING 4)                  ; warning conditions
(define NOTICE 5)                   ; normal but significant condition
(define INFO 6)                     ; informational messages
(define DEBUG 7)                    ; debug-level messages

(define (severity->symbol severity)
  (vector-ref '#(EMERGENCY ALERT CRITICAL ERROR WARNING NOTICE INFO DEBUG)
              severity))

(define (field-list->alist who plist)
  (let f ((fields plist))
    (cond ((null? fields)
           '())
          ((or (not (pair? fields)) (not (pair? (cdr fields))))
           (assertion-violation who "Short field list" plist))
          (else
           (let ((k (car fields)) (v (cadr fields)))
             (if (not v)
                 (f (cddr fields))
                 (let ((k^ (cond ((symbol? k) k)
                                 (else
                                  (assertion-violation who "Invalid key" k plist))))
                       (v^ (cond ((string? v) v)
                                 ((and (integer? v) (exact? v)) v)
                                 ((bytevector? v) v)
                                 ((condition? v) v)
                                 (else
                                  (call-with-string-output-port
                                    (lambda (p) (write v p)))))))
                   (cons (cons k^ v^)
                         (f (cddr fields))))))))))

(define current-log-fields
  (make-parameter '()
                  (lambda (plist)
                    (field-list->alist 'current-log-fields plist)
                    plist)))

(define current-log-callback
  (let ((num-pending-logs 0)
        (pending-logs (make-queue)))
    (make-parameter (lambda (log-entry)
                      (enqueue! pending-logs log-entry)
                      (if (eqv? num-pending-logs 100)
                          (dequeue! pending-logs)
                          (set! num-pending-logs (+ num-pending-logs 1))))
                    (lambda (hook)
                      (unless (procedure? hook)
                        (assertion-violation 'current-log-hook
                                             "Expected a procedure" hook))
                      (let ((q pending-logs))
                        (set! num-pending-logs 0)
                        (set! pending-logs (make-queue))
                        (let lp ()
                          (unless (queue-empty? q)
                            (hook (dequeue! q))
                            (lp))))
                      hook))))

;; Send a log entry with the given severity and message. This
;; procedure also takes a list of extra keys and values.
(define (send-log severity message . plist)
  (unless (and (fixnum? severity) (fx<=? 0 severity 7))
    (assertion-violation 'send-log "Expected a severity from 0 to 7"
                         severity message plist))
  (unless (string? message)
    (assertion-violation 'send-log "Expected message to be a string"
                         severity message plist))
  (let* ((fields (append plist (current-log-fields)))
         (alist (field-list->alist 'send-log fields)))
    ((current-log-callback) `((SEVERITY . ,severity)
                              (MESSAGE . ,(string-copy message))
                              (JIFFY . ,(current-ticks))
                              ,@alist)))))
