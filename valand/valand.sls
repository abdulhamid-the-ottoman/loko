;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Valand, an opinionated compositing windowing system

;; This is a compositing window system called Valand, which is a place
;; close to where I live. Any resemblance to Wayland is not purely
;; accidental.

;; The compositing happens in software for now, but I believe that the
;; software architecture will work for hardware acceleration.

;; Surface properties:
;;  DECORATING - the surface ID of a decoration surface
;;  NAME       - name of the window, the title string

(library (loko valand)
  (export
    ;; Surface identifiers
    ID-MAX
    ID-CURSOR-START
    ID-CURSOR-END
    ID-SYSTEM-MENU
    ID-BACKGROUND
    ID-USER-END
    ID-USER-START
    ID-CLEAR

    ;; Surface types. Please keep this in sync with libdrm from Linux.
    VL_FORMAT_XRGB8888
    VL_FORMAT_ARGB8888
    VL_FORMAT_RGB565

    ;; Indices for vl_surface-decorations.
    DECORATION-TOP
    DECORATION-RIGHT
    DECORATION-BOTTOM
    DECORATION-LEFT

    ;; A buffer is a memory area that stores pixels. Most are
    ;; off-screen, but the framebuffer is also a buffer.
    make-vl_buffer
    vl_buffer?
    vl_buffer-width
    vl_buffer-height
    vl_buffer-stride
    vl_buffer-&data
    vl_buffer-format
    vl_buffer-size
    vl_buffer-color/rgb
    vl_buffer-color/argb
    vl_buffer-fill!
    vl_buffer-set-pixel!

    ;; A surface is a visible area on the screen, backed by a buffer.
    vl_surface?
    vl_surface-id
    vl_surface-buffer
    vl_surface-width
    vl_surface-height
    vl_surface-location
    vl_surface-damage!
    (rename (vl_surface-decorations* vl_surface-decorations))
    vl_surface-property
    vl_surface-property-set!
    vl_surface-input-callbacks-set!

    ;; A pointer is a basically a mouse pointer.
    make-vl_pointer
    vl_pointer?
    vl_pointer-surface
    vl_pointer-xd
    vl_pointer-yd

    ;; A player is a pointer, a keyboard and a focused surface, plus
    ;; maybe some other input devices.
    vl_player?
    vl_player-id
    vl_player-keyboard
    vl_player-pointer
    vl_player-focus
    vl_player-focus-set!
    vl_player-handle-mouse-input
    vl_player-handle-keyboard-input

    ;; A display is an output device that that shows surfaces.
    make-vl_display
    vl_display?
    vl_display-width
    vl_display-height
    vl_display-initialize!
    vl_display-add-player!
    vl_display-surfaces                 ;dlist
    vl_display-refill-z-buffer!
    vl_display-add-window-surface!
    vl_display-add-dockapp-surface!
    vl_display-move-surface!
    vl_display-move-surface-relative!
    vl_display-allocate-user-surface
    vl_display-allocate-system-surface
    vl_display-find/id
    vl_display-find/xy
    vl_display-render

    ;; TEMPORARY EXPORTS
    vl_display-print-surfaces           ;
    )
  (import
    (rnrs (6))
    (loko dlists)
    (loko match)
    (loko valand internal)
    (loko system $host)
    (loko system unsafe))

;; Surface identifiers
(define ID-MAX          #xFFFF)
(define ID-CURSOR-START #xFFF0)
(define ID-CURSOR-END   #xFFFF)
(define ID-SYSTEM-MENU  #xFFEF)
(define ID-BACKGROUND   #xFFEE)
(define ID-USER-END     #xFEFF)
(define ID-USER-START   #x0001)
(define ID-CLEAR        #x0000)

;; Indices for vl_surface-decorations
(define DECORATION-TOP     0)
(define DECORATION-RIGHT   1)
(define DECORATION-BOTTOM  2)
(define DECORATION-LEFT    3)

(define-syntax fourcc
  (lambda (x)
    (syntax-case x ()
      [(_ code)
       (string? (syntax->datum #'code))
       #`(bytevector-u32-ref (string->utf8 #,(syntax->datum #'code))
                             0 (endianness little))])))

;; Surface types. Please keep this in sync with libdrm from Linux.
(define VL_FORMAT_XRGB8888 (fourcc "XR24"))
(define VL_FORMAT_ARGB8888 (fourcc "AR24"))
(define VL_FORMAT_RGB565   (fourcc "RG16"))
;; TODO: support everything reasonable from VESA VBE.

(define zbuf-b/p 2)

;; The height of the system menu.
(define MENU-HEIGHT 23)

(define (fxclamp x min max)
  (fxmax min (fxmin max x)))

(define (fxalign i alignment)
  (fxand (fx+ i (fx- alignment 1))
         (fx- alignment)))

(define (format-bytes-per-pixel format)
  (cond ((or (eqv? format VL_FORMAT_ARGB8888)
             (eqv? format VL_FORMAT_XRGB8888))
         4)
        ((eqv? format VL_FORMAT_RGB565)
         2)
        (else
         (assertion-violation 'format-bytes-per-pixel
                              "Unsupported surface format" format))))

(define-record-type rect
  (sealed #t)
  (fields (mutable x) (mutable y) (mutable w) (mutable h)))

;; TODO: add some kind of GC for these
(define-record-type vl_buffer
  (sealed #t)
  (fields width
          height
          stride                        ;bytes per line
          &data                         ;pointer to the pixel data
          format))                      ;pixel format

(define-record-type vl_surface
  (sealed #t)
  ;; TODO: maybe add the display here. Would make some APIs nicer. And
  ;; maybe add the dnode from the surfaces dlist.
  (fields id                            ;unique within each display
          buffer
          damage
          geometry
          (mutable decorations)
          properties
          (mutable mouse-cb)
          (mutable keyboard-cb))
  (protocol
   (lambda (p)
     (lambda (id buffer x y decorations)
       (assert (or (not decorations)
                   (and (vector? decorations)
                        (eqv? (vector-length decorations) 4)
                        (vl_surface? (vector-ref decorations 0))
                        (vl_surface? (vector-ref decorations 1))
                        (vl_surface? (vector-ref decorations 2))
                        (vl_surface? (vector-ref decorations 3)))))
       (let ((w (vl_buffer-width buffer))
             (h (vl_buffer-height buffer)))
         (let ((dmg (make-rect 0 0 w h))
               (geom (make-rect x y w h))
               (properties (make-eq-hashtable))
               (mouse-cb #f)
               (keyboard-cb #f))
           (p id buffer dmg geom decorations properties
              mouse-cb keyboard-cb)))))))

(define (vl_surface-property surf name)
  (assert (symbol? name))
  (hashtable-ref (vl_surface-properties surf) name #f))

(define (vl_surface-property-set! surf name value)
  (assert (symbol? name))
  (hashtable-set! (vl_surface-properties surf) name value))

(define (vl_surface-input-callbacks-set! surf mouse-cb keyboard-cb)
  (assert (or (not mouse-cb) (procedure? mouse-cb)))
  (assert (or (not keyboard-cb) (procedure? keyboard-cb)))
  (vl_surface-mouse-cb-set! surf mouse-cb)
  (vl_surface-keyboard-cb-set! surf keyboard-cb))

(define-record-type vl_pointer
  (sealed #t)
  (fields surface
          ;; Delta from surface top left to where the pointer actually
          ;; clicks.
          (mutable xd)
          (mutable yd))
  (protocol
   (lambda (p)
     (lambda (surface xd yd)
       (assert (fx<=? ID-CURSOR-START (vl_surface-id surface) ID-CURSOR-END))
       (let ((xd (fxclamp xd 0 (fx- (vl_surface-width surface) 1)))
             (yd (fxclamp yd 0 (fx- (vl_surface-height surface) 1))))
         (p surface xd yd))))))

(define-record-type vl_player
  (sealed #t)
  (fields display
          id
          (mutable pointer)
          (mutable keyboard)
          ;; (mutable touch)
          ;; (mutable hid-devices)
          (mutable focused-surface vl_player-focus vl_player-focus-set*!)
          (mutable moving))
  (protocol
   (lambda (p)
     (lambda (disp id pointer keyboard)
       (p disp id pointer keyboard #f #f)))))

(define-record-type vl_display
  (sealed #t)
  (opaque #t)
  (fields framebuf
          zbuf
          surfaces                 ;ordered by z-index
          (mutable user-id-counter)
          (mutable players))
  (protocol
   (lambda (p)
     (lambda (framebuf)
       (assert (vl_buffer? framebuf))
       (let ((zbuf (make-vl_buffer (vl_buffer-width framebuf)
                                   (vl_buffer-height framebuf)
                                   (* (vl_buffer-width framebuf) zbuf-b/p)
                                   (dma-allocate (* (vl_buffer-width framebuf)
                                                    (vl_buffer-height framebuf)
                                                    zbuf-b/p)
                                                 (fxnot #b1))
                                   VL_FORMAT_RGB565))
             (players '()))
         (p framebuf zbuf (make-dlist) ID-USER-START players))))))

(define (vl_display-width disp)
  (vl_buffer-width (vl_display-framebuf disp)))

(define (vl_display-height disp)
  (vl_buffer-height (vl_display-framebuf disp)))

(define (vl_display-fresh-id disp)
  (let ((id (vl_display-user-id-counter disp)))
    (cond ((fx>=? id ID-USER-END)
           (error 'vl_display-fresh-id "TODO: Recycle old surface ids" disp))
          (else
           (vl_display-user-id-counter-set! disp (+ id 1))
           id))))

(define (vl_display-find/id disp id)
  ;; XXX: could also use a hashtable
  (let lp ((dnode (dlist-tail (vl_display-surfaces disp))))
    (cond ((not dnode) #f)
          ((fx=? id (vl_surface-id (dnode-data dnode)))
           (dnode-data dnode))
          (else (lp (dnode-prev dnode))))))

;; Find the non-cursor surface at coordinate x,y
(define (vl_display-find/xy disp x y)
  (let lp ((dnode (dlist-tail (vl_display-surfaces disp))))
    (cond ((not dnode)
           #f)
          (else
           (let ((surf (dnode-data dnode)))
             (cond
               ((fx<=? ID-CURSOR-START (vl_surface-id surf) ID-CURSOR-END)
                (lp (dnode-prev dnode)))
               (else
                (let* ((geom (vl_surface-geometry surf))
                       (x0 (rect-x geom))
                       (y0 (rect-y geom))
                       (x1 (fx+ x0 (rect-w geom)))
                       (y1 (fx+ y0 (rect-h geom))))
                  (cond
                    ((and (fx<=? x0 x (fx- x1 1))
                          (fx<=? y0 y (fx- y1 1)))
                     surf)
                    (else
                     (lp (dnode-prev dnode))))))))))))

(define (vl_display-print-surfaces disp)
  (let ((surfaces (vl_display-surfaces disp)))
    (dlist-print surfaces
                 (lambda (surf)
                   (display (number->string (vl_surface-id surf) 16))))))

(define (vl_display-add-player! disp pointer)
  (assert (vl_display? disp))
  (assert (vl_pointer? pointer))
  (let* ((id (length (vl_display-players disp)))
         (player (make-vl_player disp id pointer #f)))
    (vl_display-players-set! disp (cons player (vl_display-players disp)))
    player))

(define (vl_player-focus-set! player surf)
  (cond
    ((fx<=? ID-CURSOR-START (vl_surface-id surf) ID-CURSOR-END)
     ;; Refuse to focus on a cursor
     #f)
    ((vl_surface-property surf 'DECORATING) =>
     (lambda (id)
       ;; Focus on to the real surface, not the decoration.
       (let ((surf (vl_display-find/id (vl_player-display player) id)))
         (vl_player-focus-set*! player surf))))
    (else
     (vl_player-focus-set*! player surf))))

(define (vl_surface-width surf)
  (rect-w (vl_surface-geometry surf)))

(define (vl_surface-height surf)
  (rect-h (vl_surface-geometry surf)))

(define (vl_surface-location surf)
  (let ((geom (vl_surface-geometry surf)))
    (values (rect-x geom)
            (rect-y geom))))

;; Returns a vector of the surface's four decoration surfaces, or #f
;; if there aren't any.
(define (vl_surface-decorations* surf)
  (cond ((vl_surface-decorations surf) =>
         (lambda (decorations)
           (list->vector (vector->list decorations))))
        (else #f)))

;; Allocate surface with a pre-selected id outside the user range.
(define (vl_display-allocate-system-surface _disp format id x y w h)
  (assert (not (fx<=? ID-USER-START id ID-USER-END)))
  (let* ((pixels/byte (format-bytes-per-pixel format))
         (buf (make-vl_buffer w h
                              (* w pixels/byte)
                              (dma-allocate (* w h pixels/byte) (fxnot #b1111))
                              format)))
    (make-vl_surface id buf x y #f)))

;; A surface that does not need to be mapped into user spcae.
(define (allocate-nonmappable-surface disp format x y w h)
  ;; XXX: The surface should be aligned to page boundaries so that it
  ;; can be mapped into userspace processes.
  (let* ((pixels/byte (format-bytes-per-pixel format))
         (size (* w h pixels/byte))
         (buf (make-vl_buffer w h (* w pixels/byte)
                              (dma-allocate size (fxnot #b1111))
                              format)))
    (make-vl_surface (vl_display-fresh-id disp) buf x y #f)))

;; Allocate a surface that can be mapped into user space.
(define (vl_display-allocate-user-surface disp format x y w h)
  ;; XXX: The surface should be aligned to page boundaries so that it
  ;; can be mapped into userspace processes.
  (let* ((pixels/byte (format-bytes-per-pixel format))
         (page-size 4096)
         (size (* w h pixels/byte))
         (buf (make-vl_buffer w h (* w pixels/byte)
                              (dma-allocate (fxalign size page-size)
                                            (fxnot (- page-size 1)))
                              format)))
    (make-vl_surface (vl_display-fresh-id disp) buf x y #f)))

;; Creates window decorations for a window.
(define (surface-create-decorations! surface disp dnode)
  (let* ((geom (vl_surface-geometry surface))
         (x (rect-x geom))
         (y (rect-y geom))
         (w (rect-w geom))
         (h (rect-h geom))
         (top-h 16)
         (right-w 5)
         (bottom-h 5)
         (left-w 5)
         (right-h h)
         (left-h h)
         (top-w (+ left-w w right-w))
         (bottom-w (+ left-w w right-w))
         (format VL_FORMAT_XRGB8888))
    ;; TODO: Allocate the decorations from a pool. These don't need to
    ;; be mapped into other processes.
    (let ((top (allocate-nonmappable-surface disp format (- x left-w) (- y top-h) top-w top-h))
          (right (allocate-nonmappable-surface disp format (+ x w) y right-w right-h))
          (bottom (allocate-nonmappable-surface disp format (- x left-w) (+ y h) bottom-w bottom-h))
          (left (allocate-nonmappable-surface disp format (- x left-w) y left-w left-h)))
      (vl_surface-decorations-set! surface (vector top right bottom left))
      (vector-for-each
       (lambda (surf)
         (vl_surface-property-set! surf 'DECORATING (vl_surface-id surface)))
       (vl_surface-decorations surface))
      ;; Add the decorations to the display.
      (let ((dlist (vl_display-surfaces disp)))
        (dlist-insert-after! dlist dnode top)
        (dlist-insert-after! dlist dnode right)
        (dlist-insert-after! dlist dnode bottom)
        (dlist-insert-after! dlist dnode left)))))

;; Size in pixels of one of the window decorations.
(define (surface-border-size surf which)
  (cond
    ((vl_surface-decorations surf) =>
     (lambda (decorations)
       (let ((buf (vl_surface-buffer (vector-ref decorations which))))
         (if (or (eqv? which DECORATION-TOP)
                 (eqv? which DECORATION-BOTTOM))
             (vl_buffer-height buf)
             (vl_buffer-width buf)))))
    (else 0)))

;; Move a surface, but don't allow it to move off screen. The
;; non-system surfaces can't move on top of the system menu.
(define (surface-move! disp surface x y)
  (let* ((framebuf (vl_display-framebuf disp))
         (geom (vl_surface-geometry surface))
         (id (vl_surface-id surface))
         (menu-height (cond ((vl_display-find/id disp ID-SYSTEM-MENU)
                             => vl_surface-height)
                            (else 0)))
         (min-x (surface-border-size surface DECORATION-LEFT))
         (min-y (if (fx<=? ID-USER-START id ID-USER-END)
                    (fx+ menu-height
                         (surface-border-size surface DECORATION-TOP))
                    (surface-border-size surface DECORATION-TOP)))
         (max-y (if (fx<=? ID-USER-START id ID-USER-END)
                    (fx- (vl_buffer-height framebuf)
                         (fx+ (rect-h geom)
                              (surface-border-size surface DECORATION-BOTTOM)))
                    (fx- (vl_buffer-height framebuf) 1)))
         (max-x (if (fx<=? ID-USER-START id ID-USER-END)
                    (fx- (vl_buffer-width framebuf)
                         (fx+ (rect-w geom)
                              (surface-border-size surface DECORATION-RIGHT)))
                    (fx- (vl_buffer-width framebuf) 1))))
    (rect-x-set! geom (fxclamp x min-x max-x))
    (rect-y-set! geom (fxclamp y min-y max-y))
    (vl_surface-damage! surface)))

;; Update the positions of the surface's decorations.
(define (surface-reposition-decorations! disp surf)
  (cond
    ((vl_surface-decorations surf) =>
     (lambda (decorations)
       (let* ((geom (vl_surface-geometry surf))
              (x (rect-x geom))
              (y (rect-y geom))
              (w (rect-w geom))
              (h (rect-h geom)))
         (let ((top (vector-ref decorations 0))
               (right (vector-ref decorations 1))
               (bottom (vector-ref decorations 2))
               (left (vector-ref decorations 3)))
           (let ((top-h (vl_buffer-height (vl_surface-buffer top)))
                 (left-w (vl_buffer-width (vl_surface-buffer left))))
             (vl_display-move-surface! disp top (fx- x left-w) (fx- y top-h))
             (vl_display-move-surface! disp right (fx+ x w) y)
             (vl_display-move-surface! disp bottom (fx- x left-w) (fx+ y h))
             (vl_display-move-surface! disp left (fx- x left-w) y))))))))

;; High-level surface movement. Takes care to prepare the display for
;; re-rendering of the damaged parts and moves window decorations.
(define (vl_display-move-surface! disp surf x y)
  ;; Damage any surface that was under the surface's old place and
  ;; remove it from the z-buffer.
  (assert (vl_display? disp))
  (assert (vl_surface? surf))
  (let* ((geom (vl_surface-geometry surf))
         (ox (rect-x geom))
         (oy (rect-y geom))
         (ow (rect-w geom))
         (oh (rect-h geom)))
    (vl_surface-damage! surf)
    (damage-overlapping-surfaces disp surf)
    (surface-move! disp surf x y)
    (let* ((geom (vl_surface-geometry surf))
           (nx (rect-x geom))
           (ny (rect-y geom))
           (nw (rect-w geom))
           (nh (rect-h geom)))
      (vl_display-refill-z-buffer! disp ox oy ow oh)
      (vl_display-refill-z-buffer! disp nx ny nw nh)
      (surface-reposition-decorations! disp surf))))

;; Relative-movement variant of surface-move-highlevel!.
(define (vl_display-move-surface-relative! disp surf dx dy)
  (assert (vl_display? disp))
  (assert (vl_surface? surf))
  (let* ((geom (vl_surface-geometry surf))
         (ox (rect-x geom))
         (oy (rect-y geom)))
    (vl_display-move-surface! disp surf (+ ox dx) (+ oy dy))))

;; Expand the damaged region on a surface. If called with only one
;; argument then the whole surface is marked as damaged. Otherwise
;; damages a rectangle. If you're damaging a surface with
;; transparency, but you have not moved it, then you should also call
;; vl_display-refill-z-buffer!.
(define vl_surface-damage!
  (case-lambda
    ((surface)
     (let* ((sbuf (vl_surface-buffer surface))
            (dmg (vl_surface-damage surface)))
       (rect-x-set! dmg 0)
       (rect-y-set! dmg 0)
       (rect-w-set! dmg (vl_buffer-width sbuf))
       (rect-h-set! dmg (vl_buffer-height sbuf))))
    ((surface x y w h)
     (let* ((buf (vl_surface-buffer surface))
            (sw (vl_buffer-width buf))
            (sh (vl_buffer-height buf)))
       (let f ((x x) (y y) (w w) (h h))
         (cond
           ((or (eqv? w 0) (eqv? h 0)) #f)
           ((< x 0) (f 0 y (+ w (- x)) h))
           ((< y 0) (f x 0 w (+ h (- y))))
           ((> w sw) (f x y sw h))
           ((> h sh) (f x y w sh))
           (else
            (let ((dmg (vl_surface-damage surface)))
              ;; Get the old (x0,y0)--(x1,y1) rectangle.
              (let* ((old-w (rect-w dmg))
                     (old-h (rect-h dmg))
                     (old-x0 (rect-x dmg))
                     (old-y0 (rect-y dmg))
                     (old-x1 (fx+ old-x0 old-w))
                     (old-y1 (fx+ old-y0 old-h)))
                ;; Get the (x0,y0)--(x1,y1) rectangle to add.
                (let* ((add-x0 x)
                       (add-y0 y)
                       (add-x1 (fx+ x w))
                       (add-y1 (fx+ y h)))
                  ;; The sum is a rectangle that covers both the old
                  ;; rectangle and the rectangle to add.
                  (let ((sum-x0 (if (eqv? old-w 0) add-x0 (fxmin old-x0 add-x0)))
                        (sum-y0 (if (eqv? old-h 0) add-y0 (fxmin old-y0 add-y0)))
                        (sum-x1 (fxmax old-x1 add-x1))
                        (sum-y1 (fxmax old-y1 add-y1)))
                    ;; Compute the new rect.
                    (let ((rx sum-x0)
                          (ry sum-y0)
                          (rw (fx- sum-x1 sum-x0))
                          (rh (fx- sum-y1 sum-y0)))
                      ;; Constrain it.
                      (let* ((rx (fxmax 0 (fxmin rx sw)))
                             (ry (fxmax 0 (fxmin ry sh)))
                             (rw (fxmax 0 (fxmin (fx- sw rx) rw)))
                             (rh (fxmax 0 (fxmin (fx- sh ry) rh))))
                        (rect-x-set! dmg rx)
                        (rect-y-set! dmg ry)
                        (rect-w-set! dmg rw)
                        (rect-h-set! dmg rh))))))))))))))

;; Loop over the surfaces and, if the damaging surface covers one of
;; them, then place damage in that place.
(define (damage-overlapping-surfaces disp damaging-surf)
  ;; TODO: It could be good to use a quadtree or something to speed up
  ;; the iteration over the surfaces. Or at least maybe just iterate
  ;; over those at higher z-index than damaging-surf.
  (let* ((damaging-geom (vl_surface-geometry damaging-surf))
         (damaging-dmg (vl_surface-damage damaging-surf)))
    (let ((dx (fx+ (rect-x damaging-geom) (rect-x damaging-dmg)))
          (dy (fx+ (rect-y damaging-geom) (rect-y damaging-dmg)))
          (dw (rect-w damaging-dmg))
          (dh (rect-h damaging-dmg)))
      (do ((surfaces (dlist-head (vl_display-surfaces disp))
                     (dnode-next surfaces)))
          ((not surfaces))
        (let ((damaged-surf (dnode-data surfaces)))
          ;; TODO: at this point we might stop, or maybe only keep
          ;; processing surfaces if they have alpha or key color?
          (unless (eq? damaging-surf damaged-surf)
            (let ((geom (vl_surface-geometry damaged-surf)))
              (let ((sx (rect-x geom))
                    (sy (rect-y geom))
                    (sw (rect-w geom))
                    (sh (rect-h geom)))
                (let-values ([(ix iy iw ih)
                              (rectangle-intersection dx dy dw dh
                                                      sx sy sw sh)])
                  (unless (eqv? iw 0)
                    (vl_surface-damage! damaged-surf
                                        (fx- ix sx) (fx- iy sy)
                                        iw ih)))))))))))

;; Fill in the z-buffer in the given area using the painter's
;; algorithm. Each element of the z-buf is filled in with the id of
;; the surface that should be visible at that element's coordinate.
(define vl_display-refill-z-buffer!
  (case-lambda
    ((disp)                             ;refill the entire buffer
     (let* ((zbuf (vl_display-zbuf disp))
            (&data (vl_buffer-&data zbuf))
            (size (fxalign (fx* zbuf-b/p (fx* (vl_buffer-height zbuf)
                                              (vl_buffer-width zbuf)))
                           8)))
       (assert (eqv? ID-CLEAR 0))
       ;; Clear the z-buf
       (do ((&end (fx+ &data size))
            (&data &data (fx+ &data 8)))
           ((fx=? &data &end))
         (put-mem-s61 &data 0))
       (vl_display-refill-z-buffer! disp 0 0
                                    (vl_buffer-width zbuf)
                                    (vl_buffer-height zbuf))))
    ((disp surface)
     (let ((geom (vl_surface-geometry surface)))
       (vl_display-refill-z-buffer! disp (rect-x geom) (rect-y geom)
                                    (rect-w geom) (rect-h geom))))
    ((disp x y w h)
     (let* ((zbuf (vl_display-zbuf disp))
            (zstride (vl_buffer-stride zbuf))
            (z-bytes-per-pixel zbuf-b/p))
       (assert (eqv? (vl_buffer-format zbuf) VL_FORMAT_RGB565))
       (do ((surfaces (dlist-head (vl_display-surfaces disp))
                      (dnode-next surfaces)))
           ((not surfaces))
         (let* ((surf (dnode-data surfaces))
                (geom (vl_surface-geometry surf))
                (sx (rect-x geom))
                (sy (rect-y geom))
                (sw (rect-w geom))
                (sh (rect-h geom)))
           (let-values ([(zx zy zw zh) (rectangle-intersection sx sy sw sh x y w h)])
             (unless (eqv? zw 0)
               (let* ((id (vl_surface-id surf))
                      (sbuf (vl_surface-buffer surf))
                      (sformat (vl_buffer-format sbuf))
                      (sstride (vl_buffer-stride sbuf))
                      (s-bytes-per-pixel (format-bytes-per-pixel sformat))
                      ;; The area we're refilling intersects with this surface.
                      (rx (fx- zx sx)) (ry (fx- zy sy)) (rw zw) (rh zh))
                 (do ((&tgt (fx+ (vl_buffer-&data zbuf)
                                 (fx+ (fx* zstride zy)
                                      (fx* z-bytes-per-pixel zx)))
                            (fx+ &tgt zstride))
                      (&src (fx+ (vl_buffer-&data sbuf)
                                 (fx+ (fx* sstride ry)
                                      (fx* s-bytes-per-pixel rx)))
                            (fx+ &src sstride))
                      (y 0 (fx+ y 1)))
                     ((fx=? y rh))
                   (cond ((eqv? sformat VL_FORMAT_ARGB8888)
                          (do ((&end (fx+ &tgt (fx* z-bytes-per-pixel rw)))
                               (&tgt &tgt (fx+ &tgt z-bytes-per-pixel))
                               (&src &src (fx+ &src 4)))
                              ((fx=? &tgt &end))
                            (let ((c (get-mem-u32 &src)))
                              (unless (eqv? 0 (fxarithmetic-shift-right c 24))
                                (put-mem-u16 &tgt id)))))
                         (else
                          (do ((&end (fx+ &tgt (fx* z-bytes-per-pixel rw)))
                               (&tgt &tgt (fx+ &tgt z-bytes-per-pixel)))
                              ((fx=? &tgt &end))
                            (put-mem-u16 &tgt id))))))))))))))

;; Copy a line of pixels from one XRGB8888 buffer to another one.
(define (blit-replace-line-XRGB8888 &tgt &src n &zbuf id)
  (assert (eqv? 0 (fxand #b11 (fxior &tgt &tgt n))))
  (do ((&end (fx+ &tgt n))
       (&tgt &tgt (fx+ &tgt 4))
       (&src &src (fx+ &src 4))
       (&zbuf &zbuf (fx+ &zbuf zbuf-b/p)))
      ((fx=? &tgt &end))
    (when (fx=? id (get-mem-u16 &zbuf))
      (put-mem-u32 &tgt (get-mem-u32 &src)))))

(define (blit-replace-line-ARGB8888 &tgt &src n &zbuf id)
  (assert (eqv? 0 (fxand #b11 (fxior &tgt &tgt n))))
  (do ((&end (fx+ &tgt n))
       (&tgt &tgt (fx+ &tgt 4))
       (&src &src (fx+ &src 4))
       (&zbuf &zbuf (fx+ &zbuf zbuf-b/p)))
      ((fx=? &tgt &end))
    (let ((c (get-mem-u32 &src)))
      ;; FIXME: is there a better format for 1-bit transparency?
      (unless (eqv? 0 (fxarithmetic-shift-right c 24))
        (when (fx=? id (get-mem-u16 &zbuf))
          (put-mem-u32 &tgt c))))))

;; Render the surfaces onto the target buffer. The z-buffer tells us
;; which surface should be rendered for each coordinate. Returns #t if
;; some pixels were modified.
(define (vl_display-render disp)
  (define (copy-to-buf! tbuf zbuf surf)
    ;; The geometry is where the surface is located on the screen.
    ;; It tells us where to start drawing the surface, but he
    ;; damage rect tells us which part to actually draw.
    (let* ((bytes/pixel (format-bytes-per-pixel VL_FORMAT_XRGB8888))
           (geom (vl_surface-geometry surf))
           (sbuf (vl_surface-buffer surf))
           (dmg (vl_surface-damage surf)))
      (unless (or (fx<=? (rect-w dmg) 0) (fx<=? (rect-h dmg) 0))
        (let* ((dmg-h (fxmax 0 (fxmin (rect-h dmg)
                                      (fx- (vl_buffer-height tbuf)
                                           (fx+ (rect-y dmg) (rect-y geom))))))
               (dmg-w (fxmax 0 (fxmin (rect-w dmg)
                                      (fx- (vl_buffer-width tbuf)
                                           (fx+ (rect-x dmg) (rect-x geom))))))
               (tstride (vl_buffer-stride tbuf))
               (sstride (vl_buffer-stride sbuf))
               (dstride (fx* bytes/pixel dmg-w))
               (zstride (vl_buffer-stride zbuf))
               (id (vl_surface-id surf)))
          ;; Loop over the lines to redraw.
          (do ((&tgt (fx+ (vl_buffer-&data tbuf)
                          (fx+ (fx* tstride (fx+ (rect-y dmg) (rect-y geom)))
                               (fx* bytes/pixel (fx+ (rect-x dmg) (rect-x geom)))))
                     (fx+ &tgt tstride))
               (&zbuf (fx+ (vl_buffer-&data zbuf)
                           (fx+ (fx* zstride (fx+ (rect-y dmg) (rect-y geom)))
                                (fx* zbuf-b/p (fx+ (rect-x dmg) (rect-x geom)))))
                      (fx+ &zbuf zstride))
               (&src (fx+ (vl_buffer-&data sbuf)
                          (fx+ (fx* sstride (rect-y dmg))
                               (fx* bytes/pixel (rect-x dmg))))
                     (fx+ &src sstride))
               (y 0 (fx+ y 1)))
              ((fx=? y dmg-h))
            ;; TODO: don't look at the z-buffer if there are no
            ;; overlapping windows?
            (cond ((eqv? (vl_buffer-format sbuf) VL_FORMAT_ARGB8888)
                   (blit-replace-line-ARGB8888 &tgt &src dstride &zbuf id))
                  ((eqv? (vl_buffer-format sbuf) VL_FORMAT_XRGB8888)
                   (blit-replace-line-XRGB8888 &tgt &src dstride &zbuf id)))))
        (rect-x-set! dmg 0)
        (rect-y-set! dmg 0)
        (rect-w-set! dmg 0)
        (rect-h-set! dmg 0))))

  (let ((tbuf (vl_display-framebuf disp))
        (zbuf (vl_display-zbuf disp))
        (display-updated
         (let lp ((dnode (dlist-head (vl_display-surfaces disp))))
           (if (not dnode)
               #f                       ;no updates will be made
               (let ((dmg (vl_surface-damage (dnode-data dnode))))
                 (if (not (or (fx<=? (rect-w dmg) 0) (fx<=? (rect-h dmg) 0)))
                     #t                 ;there will be updates
                     (lp (dnode-next dnode))))))))
    ;; For now this is the only supported framebuffer format.
    (when display-updated
      (assert (eqv? (vl_buffer-format tbuf) VL_FORMAT_XRGB8888))
      (do ((dnode (dlist-head (vl_display-surfaces disp))
                  (dnode-next dnode)))
          ((not dnode))
        (copy-to-buf! tbuf zbuf (dnode-data dnode))))
    display-updated))

;; Add a regular window that has decorations.
(define (vl_display-add-window-surface! disp surface)
  ;; Adding a user surface is done by inserting it before the menu
  ;; surface. This puts it on top of all other surfaces. Decorations
  ;; surfaces are then added after the surface.
  (let ((dnode (dlist-insert-before! (vl_display-surfaces disp)
                                     (dlist-tail (vl_display-surfaces disp))
                                     surface)))
    (surface-create-decorations! surface disp dnode)
    (vl_display-move-surface-relative! disp surface 0 0)))

;; Add a dockapp surface. Somewhat inspired by WindowManager dockapps.
(define (vl_display-add-dockapp-surface! disp surface)
  ;; Decoration-less surface for status displays. Should be inserted
  ;; before the menu surface.
  (dlist-insert-before! (vl_display-surfaces disp)
                        (dlist-tail (vl_display-surfaces disp))
                        surface)
  (vl_display-move-surface-relative! disp surface 0 0))

(define (vl_buffer-size buf)
  (fx* (vl_buffer-stride buf) (vl_buffer-height buf)))

(define (vl_buffer-color/rgb buf r g b)
  (let ((format (vl_buffer-format buf)))
    (cond
      ((or (eqv? format VL_FORMAT_XRGB8888)
           (eqv? format VL_FORMAT_ARGB8888))
       (fxior #xFF000000                     ;XXX: hm.
              (fxarithmetic-shift-left (exact (round (* r 255))) 16)
              (fxarithmetic-shift-left (exact (round (* g 255))) 8)
              (fxarithmetic-shift-left (exact (round (* b 255))) 0)))
      (else
       (assertion-violation 'vl_buffer-color/rgb
                            "Unimplemented surface format" buf r g b)))))

(define (vl_buffer-color/argb buf a r g b)
  (let ((format (vl_buffer-format buf)))
    (cond
      ((eqv? format VL_FORMAT_ARGB8888)
       (fxior (fxarithmetic-shift-left (exact (round (* a 255))) 24)
              (fxarithmetic-shift-left (exact (round (* r 255))) 16)
              (fxarithmetic-shift-left (exact (round (* g 255))) 8)
              (fxarithmetic-shift-left (exact (round (* b 255))) 0)))
      (else
       (assertion-violation 'vl_buffer-color/argb
                            "Unimplemented surface format" buf a r g b)))))

;; Fill a buffer with a single color. The color must be a value
;; suitable for the surface.
(define (vl_buffer-fill! buf color)
  (let* ((stride (vl_buffer-stride buf))
         (&data (vl_buffer-&data buf))
         (height (vl_buffer-height buf))
         (width (vl_buffer-width buf)))
    (cond
      ((or (eqv? (vl_buffer-format buf) VL_FORMAT_XRGB8888)
           (eqv? (vl_buffer-format buf) VL_FORMAT_ARGB8888))
       (do ((y 0 (fx+ y 1))
            (&data &data (fx+ &data stride)))
           ((fx=? y height))
         (do ((&data &data (fx+ &data 4))
              (&end (fx+ &data (fx* width 4))))
             ((fx=? &data &end))
           (put-mem-u32 &data color))))
      ((eqv? (vl_buffer-format buf) VL_FORMAT_RGB565)
       (do ((y 0 (fx+ y 1))
            (&data &data (fx+ &data stride)))
           ((fx=? y height))
         (do ((&data &data (fx+ &data 2))
              (&end (fx+ &data (fx* width 2))))
             ((fx=? &data &end))
           (put-mem-u16 &data color))))
      (else
       (assertion-violation 'vl_buffer-fill! "Unsupported buffer format"
                            (vl_buffer-format buf))))))

;; Set the pixel at x,y to color. This is not great for performance.
(define (vl_buffer-set-pixel! buf x y color)
  (unless (and (fx<? -1 x (vl_buffer-width buf))
               (fx<? -1 y (vl_buffer-height buf)))
    (error 'vl_buffer-set-pixel "Pixel outside buffer dimensions"
           buf x y color))
  (cond ((or (eqv? (vl_buffer-format buf) VL_FORMAT_XRGB8888)
             (eqv? (vl_buffer-format buf) VL_FORMAT_ARGB8888))
         (put-mem-u32 (fx+ (vl_buffer-&data buf)
                           (fx+ (fx* (vl_buffer-stride buf) y)
                                (fx* x 4)))
                      color))
        ((eqv? (vl_buffer-format buf) VL_FORMAT_RGB565)
         (put-mem-u16 (fx+ (vl_buffer-&data buf)
                           (fx+ (fx* (vl_buffer-stride buf) y)
                                (fx* x 2)))
                      color))
        (else
         (assertion-violation 'set-pixel "Unsupported buffer format"
                              (vl_buffer-format buf)))))

;; Add the background, menu and cursor surfaces.
(define (vl_display-initialize! disp)
  (define framebuf (vl_display-framebuf disp))
  (let ((background
         (vl_display-allocate-system-surface
          disp VL_FORMAT_XRGB8888 ID-BACKGROUND
          0 0 (vl_buffer-width framebuf) (vl_buffer-height framebuf)))
        (cursor
         (vl_display-allocate-system-surface
          disp VL_FORMAT_ARGB8888 ID-CURSOR-START
          (div (vl_buffer-width framebuf) 2)
          (div (vl_buffer-height framebuf) 2)
          32 32))
        (menu
         (vl_display-allocate-system-surface
          disp VL_FORMAT_XRGB8888 ID-SYSTEM-MENU
          0 0
          (vl_buffer-width framebuf)
          MENU-HEIGHT)))

    ;; Render some placeholder graphics

    (let ((buf (vl_surface-buffer background)))
      (vl_buffer-fill! buf (vl_buffer-color/rgb buf 0 0 0.78)))

    (let ((buf (vl_surface-buffer cursor)))
      (vl_buffer-fill! buf (vl_buffer-color/argb buf 0.0 0.0 0.0 0.0))
      (let ((white (vl_buffer-color/argb buf 1.0 1.0 1.0 1.0))
            (black (vl_buffer-color/argb buf 1.0 0.0 0.0 0.0)))
        (do ((y 0 (fx+ y 1)))
            ((eqv? y 3))
          (do ((x 0 (fx+ x 1)))
              ((eqv? x 3))
            (vl_buffer-set-pixel! buf x y white)))
        (vl_buffer-set-pixel! buf 1 1 black)))

    (let ((buf (vl_surface-buffer menu)))
      (vl_buffer-fill! buf (vl_buffer-color/rgb buf 0.5 0.5 0.5)))

    (let* ((surfaces (vl_display-surfaces disp))
           (backnode (dlist-prepend! surfaces background)))
      (let ((menunode (dlist-insert-after! surfaces backnode menu)))
        (dlist-insert-after! surfaces menunode cursor)))

    (vl_surface-damage! background)
    (vl_surface-damage! cursor)
    (vl_surface-damage! menu)
    (vl_display-refill-z-buffer! disp)))

;;; Input handling

(define (vl_surface-enqueue-mouse-input surf e)
  (cond ((vl_surface-mouse-cb surf) =>
         (lambda (proc) (proc e)))))

(define (vl_surface-enqueue-keyboard-input surf e)
  (cond ((vl_surface-keyboard-cb surf) =>
         (lambda (proc) (proc e)))))

(define (vl_player-handle-mouse-input player e)
  ;; TODO: Make it possible to remap the buttons
  (define MOUSE-BUTTON-LEFT 1)

  (let* ((disp (vl_player-display player))
         ;; FIXME: use the offsets on the pointer
         (pointer (vl_player-pointer player))
         (msurf (vl_pointer-surface pointer)))
    ;; Move the pointer
    (match e
      [#(_ 'abs x y _ ...)
       (vl_display-move-surface! disp msurf x y)]
      [#(_ 'rel dx dy _ ...)
       (vl_display-move-surface-relative! disp msurf dx dy)])

    ;; Handle button presses, dragging the window, passing through
    ;; movement.
    (let-values ([(x y) (vl_surface-location msurf)])
      (match e
        [#('motion _abs/res _x _y _parent-event)
         (vl_display-move-surface! disp msurf x y)
         (match (vl_player-moving player)
           [#(dx dy surf)
            ;; Drag around the window
            (vl_display-move-surface! disp surf (fx+ x dx) (fx+ y dy))]
           [#f
            (cond
              ((vl_display-find/xy disp x y) =>
               (lambda (surf)
                 ;; Focus follows mouse
                 (when (fx<=? ID-USER-START (vl_surface-id surf) ID-USER-END)
                   (vl_player-focus-set! player surf))
                 (when (eq? (vl_player-focus player) surf)
                   (let ((e^ (vector 'motion x y e)))
                     (vl_surface-enqueue-mouse-input surf e^))))))])]

        [#('press _abs/res _x _y button _parent-event)
         (let ((surf (vl_display-find/xy disp x y)))
           (when surf
             (vl_player-focus-set! player surf)
             (cond
               ((vl_surface-property surf 'DECORATING)
                ;; Mouse events on decorations are never passed
                ;; through to the surface.
                (when (eqv? button MOUSE-BUTTON-LEFT)
                  (cond
                    ((vl_player-focus player) =>
                     (lambda (surf)
                       (let-values ([(sx sy) (vl_surface-location surf)])
                         ;; Record where the relative coordinate
                         ;; where mouse hit the surface.
                         (let ((moving (vector (fx- sx x) (fx- sy y) surf)))
                           (vl_player-moving-set! player moving))))))))
               (else
                (let ((e^ (vector 'press x y button e)))
                  (vl_surface-enqueue-mouse-input surf e^))))))]

        [#('release _abs/res _x _y button _parent-event)
         (cond
           ((and (vl_player-moving player)
                 (eqv? button MOUSE-BUTTON-LEFT))
            ;; Stop surface movement
            (vl_player-moving-set! player #f))
           (else
            (let ((surf (vl_display-find/xy disp x y)))
              (when surf
                (vl_player-focus-set! player surf)
                (cond
                  ((vl_surface-property surf 'DECORATING)
                   ;; Mouse events on decorations are never passed
                   ;; through to the surface.
                   #f)
                  (else
                   (let ((e^ (vector 'release x y button e)))
                     (vl_surface-enqueue-mouse-input surf e^))))))))]))))

(define (vl_player-handle-keyboard-input player e)
  (cond
    ((vl_player-focus player) =>
     (lambda (surf)
       (vl_surface-enqueue-keyboard-input surf e))))))
