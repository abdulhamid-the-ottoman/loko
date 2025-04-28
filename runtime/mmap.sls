;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; Memory maps for physical and virtual address allocation

;; A memory map is a data structure that keeps track of memory
;; addresses. It's used to implement the physical memory map, the
;; kernel virtual memory map, and user space virtual memory maps.
;; Memory maps track relatively large areas, they are not meant to
;; track individual objects.

;; TODO: Use a balanced binary tree

(library (loko runtime mmap)
  (export
    make-mmap mmap?
    mmap-top
    mmap-mark!
    mmap-filter->list
    mmap-for-each
    mmap-find-free-area

    prot-read prot-write prot-exec

    area?
    area-base area-top area-protection area-type area-info
    area-size)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6)))

(define prot-read  #b001)
(define prot-write #b010)
(define prot-exec  #b100)

;; Represents the addresses [0,top]. Areas that are missing from the
;; areas field are considered to be unused.
(define-record-type mmap
  (sealed #t)
  (fields top
          (mutable areas))               ;ordered list of areas
  (protocol
   (lambda (p)
     (lambda (top)
       (assert (fxpositive? top))
       (p top '())))))

(define-record-type area
  (sealed #t)
  (fields (mutable base)
          (mutable top)
          (mutable protection)
          type
          info))

(define (area-size area)
  (fx+ 1 (fx- (area-top area) (area-base area))))

(define (insert-area-link! mmap prev next)
  (if (not prev)
      (mmap-areas-set! mmap next)
      (set-cdr! prev next)))

(define (mmap-mark! mmap base size protection type info)
  (assert (fx<=? 0 base (fx- (mmap-top mmap) 1)))
  (assert (fx>=? size 0))
  (when (fx>? size 0)
    (let ((top (fx+ base (fx- size 1))))
      (assert (fx<=? 0 top (mmap-top mmap)))
      (let ((A (make-area base top protection type info)))
        (let lp ((prev #f) (areas (mmap-areas mmap)))
          (cond
            ((null? areas)
             ;; No area overlaps with this one, so insert it at the end.
             (insert-area-link! mmap prev (list A)))

            (else
             (let ((B (car areas)))
               (cond
                 ((fx>? (area-base A) (area-top B))
                  (lp areas (cdr areas)))

                 ((fx<? (area-top A) (area-base B))
                  ;; A does not overlap with B, insert it
                  (insert-area-link! mmap prev (cons A areas)))

                 ((and (fx<=? (area-base A) (area-base B))
                       (fx>=? (area-top A) (area-top B)))
                  ;; if A completely overlaps B, remove B
                  ;;
                  ;;  BBBB   BB     BB     BB
                  ;;  AAAA   AAAA  AAAA  AAAA
                  (let ((next (cdr areas)))
                    (insert-area-link! mmap prev next)
                    (lp prev next)))

                 ((fx<=? (area-base A) (area-base B))
                  ;; if A overlaps the start of B, remove
                  ;; that part of B and insert A before it
                  ;;
                  ;;    BBBB     BBBB
                  ;;  AAAA       AA
                  ;;    =>       =>
                  ;;  AAAABB     AABB
                  (area-base-set! B (fx+ 1 (area-top A)))
                  (insert-area-link! mmap prev (cons A areas)))

                 ((and (fx>=? (area-base A) (area-base B))
                       (fx<? (area-top A) (area-top B)))
                  ;; if A overlaps the middle of B, remove
                  ;; that part of B, splitting it B and C
                  ;; BBBBBB
                  ;;   AA
                  ;;   =>
                  ;; BBAACC
                  (let ((C (make-area (fx+ 1 (area-top A)) (area-top B)
                                      (area-protection B)
                                      (area-type B)
                                      (area-info B))))
                    (area-top-set! B (fx+ -1 (area-base A)))
                    (insert-area-link! mmap prev (cons B (cons A (cons C (cdr areas)))))))

                 (else
                  ;; if R overlaps the end of B, remove that
                  ;; part of B and insert A after it
                  (area-top-set! B (fx+ -1 (area-base A)))
                  (insert-area-link! mmap prev (cons B (cons A (cdr areas))))))))))))))

(define (mmap-filter->list proc mmap)
  (filter proc (mmap-areas mmap)))

(define (mmap-for-each proc mmap)
  (for-each proc (mmap-areas mmap)))

;; Find a free area where it's possible to place `size' bytes. The
;; base address will be an even multiple of `page-size'. Returns #f if
;; there is no suitable address. The mmap structure is not modified.
(define (mmap-find-free-area mmap size page-size)
  ;; TODO: Use a faster algorithm, this one's O(n).
  (assert (fx>? size 0))
  (assert (eqv? 1 (fxbit-count page-size)))
  (let ((size (fxand (fx+ size (fx- page-size 1)) (fx- page-size))))
    (let lp ((areas (mmap-areas mmap))
             (addr 0))                    ;candidate address
      (if (null? areas)
          (if (fx>=? (fx- (fx+ (mmap-top mmap) 1) addr) size)
              addr
              #f)
          (let ((A (car areas)))
            (cond
              ((fx<=? (fx+ addr size) (area-base A))
               addr)
              (else
               ;; The next candidate address is immediately after the top
               ;; of this area, plus any alignment needed for the requested
               ;; page size.
               (let ((addr (fxand (fx+ (fx+ (area-top A) 1) (fx- page-size 1))
                                  (fx- page-size))))
                 (lp (cdr areas) addr)))))))))

)
