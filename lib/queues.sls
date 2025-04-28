;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-FileCopyrightText: © 2020 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+
#!r6rs

;;; Fast & mutable queues

;; These queues mutate old pairs to refer to new pairs, so they are
;; probably not so good for a generational garbage collector. In that
;; case, (pfds queues) would be a better choice.

;; This code is based on public domain code from SLIB, originally
;; written by Andrew Wilcox in 1992. Please look there if you need
;; this kind of code under another license. It has been changed here
;; to be like a textbook example of a mutable pair-based queue.

(library (loko queues)
  (export
    make-queue
    queue-push!
    enqueue!
    dequeue!
    queue-empty?
    queue-front)
  (import
    (rnrs)
    (rnrs mutable-pairs))

(define (make-queue)
  (cons '() '()))

(define (queue-push! q datum)
  (let* ((old-first-pair (car q))
         (new-first-pair (cons datum old-first-pair)))
    (set-car! q new-first-pair)
    (when (null? old-first-pair)
      (set-cdr! q new-first-pair))))

(define (enqueue! q datum)
  (let ((new-pair (cons datum '())))
    (if (null? (car q))
        (set-car! q new-pair)
        (set-cdr! (cdr q) new-pair))
    (set-cdr! q new-pair)))

(define (dequeue! q)
  (let ((first-pair (car q)))
    (if (null? first-pair)
        (assertion-violation 'dequeue! "attempt to dequeue an empty queue"))
    (let ((first-cdr (cdr first-pair)))
      (set-car! q first-cdr)
      (when (null? first-cdr)
        (set-cdr! q '()))
      (car first-pair))))

(define (queue-empty? q)
  (null? (car q)))

(define (queue-front q)
  (let ((first-pair (car q)))
    (if (null? first-pair)
        (assertion-violation 'queue-front "queue is empty" q)
        (car first-pair)))))
