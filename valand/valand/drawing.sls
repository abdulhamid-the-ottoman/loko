;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Valand, utilities drawing for drawing things.

;; Some simple drawing primitives. Will change as needed.

(library (loko valand drawing)
  (export
    ;; For drawing placeholder graphics
    vl_surface-draw-simple-decorations
    vl_buffer-draw-system-menu
    vl_buffer-draw-checkerboard
    vl_buffer-draw-horizontal-gradient

    vl_buffer-draw-line
    vl_buffer-draw-character
    vl_buffer-copy-from-bytevector)
  (import
    (rnrs (6))
    (loko valand)
    (loko system unsafe))

(define (fxclamp x min max)
  (fxmax min (fxmin max x)))

;; A checkerboard pattern.
(define (vl_buffer-draw-checkerboard buf)
  (let* ((stride (vl_buffer-stride buf))
         (height (vl_buffer-height buf))
         (width (vl_buffer-width buf)))
    (assert (or (eqv? (vl_buffer-format buf) VL_FORMAT_XRGB8888)
                (eqv? (vl_buffer-format buf) VL_FORMAT_ARGB8888)))
    (let ((gray (vl_buffer-color/rgb buf 2/5 2/5 2/5))
          (white (vl_buffer-color/rgb buf 14/15 14/15 14/15)))
      (do ((&data (vl_buffer-&data buf) (fx+ &data stride))
           (y 0 (fx+ y 1)))
          ((fx=? y height))
        (do ((&data &data (fx+ &data 4))
             (x 0 (fx+ x 1)))
            ((fx=? x width))
          (put-mem-u32 &data
                       (if (fx<? (fxmod (fx+ x (fx* (fxdiv y 8) 8)) 16) 8)
                           gray white)))))))

;; A gradient with two colors.
(define (vl_buffer-draw-horizontal-gradient buf r1 g1 b1 r2 g2 b2)
  (define (scale-ch c1 c2 p)
    (+ c1 (* p (- c2 c1))))
  (assert (or (eqv? (vl_buffer-format buf) VL_FORMAT_XRGB8888)
              (eqv? (vl_buffer-format buf) VL_FORMAT_ARGB8888)))
  (let* ((stride (vl_buffer-stride buf))
         (height (vl_buffer-height buf))
         (width (vl_buffer-width buf)))
    (do ((y 0 (fx+ y 1))
         (&data (vl_buffer-&data buf) (fx+ &data stride)))
        ((fx=? y height))
      (let ((p (/ y (- height 1))))
        (do ((color (vl_buffer-color/rgb buf
                                         (scale-ch r1 r2 p)
                                         (scale-ch g1 g2 p)
                                         (scale-ch b1 b2 p)))
             (&data &data (fx+ &data 4))
             (x 0 (fx+ x 1)))
            ((fx=? x width))
          (put-mem-u32 &data color))))))

;; Placeholder graphics for window decorations.
(define (vl_surface-draw-simple-decorations surface)
  (define (draw-some-rectangle buf bg-color color)
    (let ((w (vl_buffer-width buf))
          (h (vl_buffer-height buf)))
      (vl_buffer-fill! buf bg-color)
      (when (fx>? w h)
        (vl_buffer-draw-line buf 0 0 (fx- w 1) 0 color)
        (vl_buffer-draw-line buf 0 (fx- h 1) (fx- w 1) (fx- h 1) color))
      (vl_buffer-draw-line buf 0 0 0 (fx- h 1) color)
      (vl_buffer-draw-line buf (fx- w 1) 0 (fx- w 1) (fx- h 1) color)))
  (vector-for-each
   (lambda (surf)
     (let ((buf (vl_surface-buffer surf)))
       (draw-some-rectangle buf
                            (vl_buffer-color/rgb buf 0.08 0.2 0.33)
                            (vl_buffer-color/rgb buf .45 .46 .53))
       (vl_surface-damage! surf)))
   (vl_surface-decorations surface)))

;; This is just a placeholder for now.
(define (vl_buffer-draw-system-menu buf font font-w font-h)
  (let ((height (vl_buffer-height buf)))
    (assert (fx>=? height 20))
    (assert (eqv? (vl_buffer-format buf) VL_FORMAT_XRGB8888))
    (vl_buffer-fill! buf (vl_buffer-color/rgb buf 0.5 0.5 0.5))
    (vl_buffer-draw-line buf 0 (fx- height 2)
                         (fx- (vl_buffer-width buf) 1) (fx- height 2)
                         (vl_buffer-color/rgb buf 0.15 0.15 0.4))
    (vl_buffer-draw-line buf 0 (fx- height 1)
                         (fx- (vl_buffer-width buf) 1) (fx- height 1)
                         (vl_buffer-color/rgb buf 0.1 0.1 0.1))
    ;; TODO: Very much is TODO here. Very much. There should be some
    ;; structure that describes what to draw.
    (do ((str "Loko Scheme")
         (x 5 (fx+ x font-w))
         (i 0 (fx+ i 1)))
        ((= i (string-length str)))
      (vl_buffer-draw-character buf font (+ x 1) 4 font-w font-h
                                (vl_buffer-color/rgb buf 0.0 0.0 0.0)
                                #f
                                (string-ref str i))
      (vl_buffer-draw-character buf font x 3 font-w font-h
                                (vl_buffer-color/rgb buf 1.0 1.0 1.0)
                                #f
                                (string-ref str i)))))

;; Bresenham
(define (vl_buffer-draw-line buf x0 y0 x1 y1 color)
  (let ((x0 (fxclamp x0 0 (vl_buffer-width buf)))
        (x1 (fxclamp x1 0 (vl_buffer-width buf)))
        (y0 (fxclamp y0 0 (vl_buffer-height buf)))
        (y1 (fxclamp y1 0 (vl_buffer-height buf))))
    (let f ((x0 x0) (y0 y0) (x1 x1) (y1 y1) (steep #f))
      (cond
        ((fx<? (abs (fx- x0 x1)) (abs (fx- y0 y1)))
         (let ((x0 y0) (y0 x0)
               (x1 y1) (y1 x1))
           (f x0 y0 x1 y1 #t)))
        ((fx>? x0 x1)
         (let ((x0 x1) (x1 x0)
               (y0 y1) (y1 y0))
           (f x0 y0 x1 y1 steep)))
        (else
         (let* ((dx (fx- x1 x0))
                (dy (fx- y1 y0))
                (derror2 (fx* (abs dy) 2)))
           (let lp ((x x0) (y y0) (error2 0))
             (when (fx<=? x x1)
               (if steep
                   (vl_buffer-set-pixel! buf y x color)
                   (vl_buffer-set-pixel! buf x y color))
               (let ((x (fx+ x 1))
                     (error2 (fx+ error2 derror2)))
                 (if (fx>? error2 dx)
                     (let ((y (fx+ y (if (fx>? y1 y0) 1 -1)))
                           (error2 (fx- error2 (fx* dx 2))))
                       (lp x y error2))
                     (lp x y error2)))))))))))

;; Draw a character from a monospace font.
(define (vl_buffer-draw-character buf font x y w h fg-color bg-color ch)
  (let lp ((ch ch))
    (let* ((i (char->integer ch))
           (page-num (fxarithmetic-shift-right i 8)))
      (cond
        ((fx<? page-num (vector-length font))
         (let* ((page (vector-ref font page-num))
                (bitmap (and (vector? page)
                             (vector-ref page (fxand i #xff)))))
           (if (bytevector? bitmap)
               (do ((i 0 (fx+ i 1)))
                   ((fx=? i h))
                 (do ((y (fx+ y i))
                      (bitmask (bytevector-u8-ref bitmap i))
                      (xm #x80 (fxarithmetic-shift-right xm 1))
                      (xe (fx+ x w))
                      (x x (fx+ x 1)))
                     ((fx=? x xe))
                   ;; TODO: Restructure this to avoid
                   ;; vl_buffer-set-pixel!.
                   (if (eqv? 0 (fxand xm bitmask))
                       (when (fixnum? bg-color) (vl_buffer-set-pixel! buf x y bg-color))
                       (vl_buffer-set-pixel! buf x y fg-color))))
               (unless (eqv? ch #\nul) (lp #\nul)))))
        (else (unless (eqv? ch #\nul) (lp #\nul)))))))

;; Copies pixel data from a bytevector to a surface.
(define (vl_buffer-copy-from-bytevector buf x y src src-w src-h src-stride src-format)
  (let* ((tgt-format (vl_buffer-format buf))
         (bytes-per-pixel 4)
         (copy-w (fxmin src-w (vl_buffer-width buf)))
         (copy-h (fxmin src-h (vl_buffer-height buf))))
    (assert (or (eqv? src-format VL_FORMAT_XRGB8888)
                (eqv? src-format VL_FORMAT_ARGB8888)))
    (assert (or (eqv? tgt-format VL_FORMAT_XRGB8888)
                (eqv? tgt-format VL_FORMAT_ARGB8888)))
    (assert (eqv? 0 (fxand #b11 src-stride)))
    (let ((tstride (vl_buffer-stride buf)))
      (do ((&tgt (fx+ (vl_buffer-&data buf)
                      (fx+ (fx* tstride y) (fx* bytes-per-pixel x)))
                 (fx+ &tgt tstride))
           (src-index 0 (fx+ src-index src-stride))
           (y 0 (fx+ y 1)))
          ((fx=? y copy-h))
        ;; Copy one line from the bytevector to the target buffer.
        (do ((&tend (fx+ &tgt (fx* bytes-per-pixel copy-w)))
             (&tgt &tgt (fx+ &tgt bytes-per-pixel))
             (src-index src-index (fx+ src-index 4)))
            ((fx=? &tgt &tend))
          (let ((color (bytevector-u32-ref src src-index (endianness little))))
            (unless (and (eqv? src-format VL_FORMAT_ARGB8888)
                         (eqv? 0 (fxarithmetic-shift-right color 24)))
              (put-mem-u32 &tgt color)))))))))
