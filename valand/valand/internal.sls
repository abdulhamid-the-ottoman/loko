;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Valand, internal utilities exported for testability

(library (loko valand internal)
  (export
    rectangle-intersection)
  (import
    (rnrs (6)))

;; Return the intersection of two rectangles. Returns: x y w h.
(define (rectangle-intersection x1 y1 w1 h1 x2 y2 w2 h2)
  (define display (lambda _ #f))
  (cond
    ((or (eqv? w1 0) (eqv? h1 0) (eqv? w2 0) (eqv? h2 0))
     (values 0 0 0 0))
    (else
     ;; XXX: could be a lot of redundancy here, but there are tests
     ;; that prove that it works.
     (let lp-x ((x1 x1) (y1 y1) (w1 w1) (h1 h1) (x2 x2) (y2 y2) (w2 w2) (h2 h2))
       (cond
         ((fx>? x1 x2)                   ;sort in x-axis
          (lp-x x2 y2 w2 h2 x1 y1 w1 h1))
         ((fx<=? (fx+ x1 w1) x2)          ;no intersect in x-axis?
          (display "intersection impossible in x-axis\n")
          (values 0 0 0 0))
         (else
          (let lp-y ((x1 x1) (y1 y1) (w1 w1) (h1 h1) (x2 x2) (y2 y2) (w2 w2) (h2 h2))
            (cond
              ((fx>? y1 y2)              ;sort in y-axis
               (lp-y x2 y2 w2 h2 x1 y1 w1 h1))
              ((fx<=? (fx+ y1 h1) y2)     ;no intersect in y-axis?
               (display "intersection impossible in y-axis\n")
               (values 0 0 0 0))
              ;; The rectangles intersect somewhere; find out where.
              (else
               (cond
                 ((and (fx>=? x2 x1)
                       (fx>=? (fx+ x2 w2) (fx+ x1 w1))
                       (fx>=? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 1\n")
                  (values x2 y2 (fx- (fx+ x1 w1) x2) (fx- (fx+ y1 h1) y2)))

                 ((and (fx<=? x2 x1)
                       (fx<=? (fx+ x2 w2) (fx+ x1 w1))
                       (fx>=? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 2\n")
                  (values x1 y2 (fx- (fx+ x2 w2) x1) (fx- (fx+ y1 h1) y2)))

                 ((and (fx>=? x2 x1)
                       (fx<=? (fx+ x2 w2) (fx+ x1 w1))
                       (fx<=? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 3\n")
                  (values x2 y2 w2 h2))

                 ((and (fx<=? x2 x1)
                       (fx>=? (fx+ x2 w2) (fx+ x1 w1))
                       (fx<=? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 4\n")
                  (values x1 y2 w1 h2))

                 ((and (fx>=? x2 x1)
                       (fx<? (fx+ x2 w2) (fx+ x1 w1))
                       (fx>? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 5\n")
                  (values x2 y2 w2 (fx- (fx+ y1 h1) y2)))

                 ((and (fx<? x2 x1)
                       (fx>=? (fx+ x2 w2) (fx+ x1 w1))
                       (fx>=? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 6\n")
                  (values x1 y2 w1 (fx- (fx+ y1 h1) y2)))

                 ((and (fx>=? x2 x1)
                       (fx>=? (fx+ x2 w2) (fx+ x1 w1))
                       (fx<=? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 7\n")
                  (values x2 y2 (fx- (fx+ x1 w1) x2) h2))

                 ((and (fx<? x2 x1)
                       (fx<? (fx+ x2 w2) (fx+ x1 w1))
                       (fx<=? (fx+ y2 h2) (fx+ y1 h1)))
                  (display "case 8\n")
                  (values x1 y2 (fx- (fx+ x2 w2) x1) h2))

                 (else                  ;should not happen
                  (values 0 0 0 0)))))))))))))
