;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-FileCopyrightText: © 2022 G. Weinholt
;; SPDX-License-Identifier: EUPL-1.2+
#!r6rs

;;; Doubly-linked lists

(library (loko dlists)
  (export
    make-dlist dlist?
    dlist-head
    dlist-tail

    make-dnode dnode?
    dnode-prev
    dnode-data
    dnode-next

    dlist-empty?
    dlist-prepend!
    dlist-append!
    dlist-remove!
    dlist-insert-after!
    dlist-insert-before!
    dlist-print
    dlist->list)
  (import
    (rnrs))

(define-record-type dlist
  (sealed #t)
  (fields (mutable head)
          (mutable tail))
  (protocol
   (lambda (p)
     (lambda ()
       (p #f #f)))))

(define-record-type dnode
  (sealed #t)
  (fields (mutable prev)
          (mutable data)
          (mutable next)))

(define (dlist-empty? dlist)
  (not (dlist-head dlist)))

;; Add to the front of the dlist
(define (dlist-prepend! dlist data)
  (let ((new (make-dnode #f data (dlist-head dlist))))
    (cond ((dlist-head dlist) =>
           (lambda (head)
             (dnode-prev-set! head new))))
    (dlist-head-set! dlist new)
    (when (not (dlist-tail dlist))
      (dlist-tail-set! dlist new))
    new))

;; Add to the end of the dlist
(define (dlist-append! dlist data)
  (let ((new (make-dnode (dlist-tail dlist) data #f)))
    (cond ((dlist-tail dlist) =>
           (lambda (tail)
             (dnode-next-set! tail new))))
    (dlist-tail-set! dlist new)
    (when (not (dlist-head dlist))
      (dlist-head-set! dlist new))
    new))

(define (dlist-remove! dlist dnode)
  (when (eq? (dlist-head dlist) dnode)
    (dlist-head-set! dlist (dnode-next dnode)))
  (when (eq? (dlist-tail dlist) dnode)
    (dlist-tail-set! dlist (dnode-prev dnode)))
  (cond ((dnode-next dnode) =>
         (lambda (next)
           (dnode-prev-set! next (dnode-prev dnode)))))
  (cond ((dnode-prev dnode) =>
         (lambda (prev)
           (dnode-next-set! prev (dnode-next dnode))))))

;; Insert data after prev in the dlist
(define (dlist-insert-after! dlist prev data)
  (assert prev)
  (let ((new (make-dnode prev data (dnode-next prev))))
    (dnode-next-set! prev new)
    (cond ((dnode-next new) =>
           (lambda (next)
             (dnode-prev-set! next new))))
    (when (not (dnode-next new))
      (dlist-tail-set! dlist new))
    new))

;; Insert data before next in the dlist
(define (dlist-insert-before! dlist next data)
  (assert next)
  (let ((new (make-dnode (dnode-prev next) data next)))
    (dnode-prev-set! next new)
    (cond ((dnode-prev new) =>
           (lambda (prev)
             (dnode-next-set! prev new)))
          (else
           (dlist-head-set! dlist new)))
    new))

(define (dlist-print dlist func)
  (let lp ((node (dlist-head dlist))
           (last #f))
    (cond (node
           (func (dnode-data node))
           (display "→")
           (lp (dnode-next node) node))
          (else
           (display "()\n")
           (let lp ((node last))
             (cond (node
                    (func (dnode-data node))
                    (display "→")
                    (lp (dnode-prev node)))
                   (else
                    (display "()\n"))))))))

(define (dlist->list dlist)
  (let lp ((dnode (dlist-head dlist)))
    (if (not dnode)
        '()
        (cons (dnode-data dnode)
              (lp (dnode-next dnode)))))))
