#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: EUPL-1.2+
;; Test of Valand using the yxskaft package
;; Copyright © 2022 G. Weinholt
#!r6rs

(import
  (rnrs)
  (yxskaft simple)
  (loko dlists)
  (loko match)
  (loko font font-6x13)
  (loko valand)
  (loko valand drawing)
  (loko system unsafe)
  (loko system fibers)
  (loko system logging)
  (struct pack))

(current-log-callback
 (lambda (e)
   (define p (current-error-port))
   (display "<" p)
   (display (cdr (assq 'SEVERITY e)) p)
   (display ">" p)
   (display "\x1b;[1;41;33m" p)
   (display (cdr (assq 'MESSAGE e)) p)
   (display "\x1b;[m" p)
   (newline p)))

(define (read-tga-to-surface p surf)
  (define who 'read-tga-to-surface)
  (let-values ([(id-len cmap-type image-type) (get-unpack p "<3C")])
    (unless (and (eqv? image-type 2)
                 (eqv? cmap-type 0))
      (error who "Wrong TGA format, expected uncompressed 32-bit" image-type cmap-type))
    ;; Ignore color map
    (get-unpack p "<SSC")
    (let-values ([(x y w h pixelsz flags) (get-unpack p "<4S2C")])
      (unless (and (eqv? pixelsz 32) (eqv? flags 8))
        (error who "Expected a 32-bit TGA image" pixelsz flags))
      (unless (eqv? x 0)
        (error who "Expected a left-to-right image" x y))
      (get-bytevector-n p id-len)
      (let ((bv (make-bytevector (* w 4)))
            (buf (vl_surface-buffer surf)))
        (let-values ([(ystart yend ystep)
                      (if (eqv? y 0)
                          (values (- h 1) -1 -1)
                          (values 0 h 1))])
          (let lp ((y ystart))
            (unless (fx=? y yend)
              (get-bytevector-n! p bv 0 (bytevector-length bv))
              (vl_buffer-copy-from-bytevector buf 0 y bv w 1 (bytevector-length bv)
                                              VL_FORMAT_XRGB8888)
              (lp (fx+ y ystep)))))))))

;;; Get a framebuffer from yxskaft and setup Valand

(define window
  (create-simple-window "Valand test using yxskaft" 1024 768))

(simple-window-hide-cursor window)

(define framebuf
  (make-vl_buffer 1024 768 (* 1024 4) (simple-window-ptr window)
                  VL_FORMAT_XRGB8888))

(define the-display (make-vl_display framebuf))

(vl_display-initialize! the-display)

(define backsurf (vl_display-find/id the-display ID-BACKGROUND))
(define mouse-surface (vl_display-find/id the-display ID-CURSOR-START))
(define menusurf (vl_display-find/id the-display ID-SYSTEM-MENU))

(define mouse-pointer (make-vl_pointer mouse-surface 0 0))

(let ()
  (vl_buffer-draw-horizontal-gradient (vl_surface-buffer backsurf)
                                      0 0 0   0 .58 .58)
  (vl_surface-damage! backsurf))

(call-with-port (open-file-input-port "../../bin/pc-repl/pointer.tga")
  (lambda (p)
    (read-tga-to-surface p mouse-surface)
    (vl_surface-damage! mouse-surface)
    (vl_display-refill-z-buffer! the-display mouse-surface)))

(let ((font-w 6)
      (font-h 13))
  (vl_buffer-draw-system-menu (vl_surface-buffer menusurf) font font-w font-h)
  (vl_surface-damage! menusurf))

;;; Do some semi-interactive nonsense

(define checkerboard
  (vl_display-allocate-user-surface the-display VL_FORMAT_XRGB8888
                                    150 250 100 80))
(vl_surface-property-set! checkerboard 'NAME "Checkerboard")

(vl_buffer-draw-checkerboard (vl_surface-buffer checkerboard))
(vl_display-add-window-surface! the-display checkerboard)
(vl_surface-draw-simple-decorations checkerboard)

(define exit-cvar (make-cvar))

(define MOUSE-LEFT-BUTTON 1)

(spawn-fiber
 (lambda ()
   (define player (vl_display-add-player! the-display mouse-pointer))

   (let lp ()
     (when (vl_display-render the-display)
       (render-simple-window window))
     (let lp ()
       (cond
         ((simple-window-poll-for-event window) =>
          (lambda (e)
            (cond
              ((keyboard-event? e)
               ;; FIXME: Convert the event to USB HID (page, usage,
               ;; modifiers).
               (let ((e (vector (if (keyboard-event-press? e) 'make 'break)
                                (keyboard-event-char e)
                                (keyboard-event-modifiers e)
                                e)))
                 (vl_player-handle-keyboard-input player e)))

              ((mouse-event-motion? e)
               (let ((x (mouse-event-x e))
                     (y (mouse-event-y e)))
                 (vl_player-handle-mouse-input player (vector 'motion 'abs x y #f))))

              ((mouse-event-press? e)
               (let ((x (mouse-event-x e))
                     (y (mouse-event-y e))
                     (button (mouse-event-button e)))
                 (vl_player-handle-mouse-input player (vector 'press 'abs x y button #f))))

              ((mouse-event-release? e)
               (let ((x (mouse-event-x e))
                     (y (mouse-event-y e))
                     (button (mouse-event-button e)))
                 (vl_player-handle-mouse-input player (vector 'release 'abs x y button #f))))

              ((delete-window-event? e)
               (destroy-simple-window window)
               (signal-cvar! exit-cvar)))

            (lp)))))
     (sleep 1/60)
     (lp))))

(define (run-lines-demo surface)
  (define max-x (- (vl_surface-width surface) 5))
  (define max-y (- (vl_surface-height surface) 5))
  (define buf (vl_surface-buffer surface))
  (define (color r g b)
    (vl_buffer-color/rgb buf (/ r 255) (/ g 255) (/ b 255)))
  (define colors                       ;Pastels.gpl from gimp 2.0
    (vector (color 226 145 145)
            (color 153 221 146)
            (color 147 216 185)
            (color 148 196 211)
            (color 148 154 206)
            (color 179 148 204)
            (color 204 150 177)
            (color 204 164 153)
            (color 223 229 146)
            (color 255 165 96)
            (color 107 255 99)
            (color 101 255 204)
            (color 101 196 255)
            (color 101 107 255)
            (color 173 101 255)
            (color 255 101 244)
            (color 255 101 132)
            (color 255 101 101)))
  (define bg (vl_buffer-color/rgb buf 0 0 0))
  (define-record-type line
    (sealed #t)
    (fields (mutable color)
            (mutable x0) (mutable y0)
            (mutable x1) (mutable y1)))
  (define lines (make-vector 12))
  (do ((i 0 (fx+ i 1)))
      ((fx=? i (vector-length lines)))
    (vector-set! lines i (make-line (vector-ref colors 0) 10 12 10 12)))
  (vl_buffer-fill! (vl_surface-buffer surface) bg)
  (vl_surface-damage! surface)
  (let lp ((prev (vector-ref lines 0))
           (idx 1)
           (color-idx 0)
           (dx0 2) (dy0 3) (dx1 4) (dy1 5))
    (let ((line (vector-ref lines idx)))
      (vl_buffer-draw-line (vl_surface-buffer surface)
                           (line-x0 line) (line-y0 line)
                           (line-x1 line) (line-y1 line)
                           bg)
      (line-color-set! line (vector-ref colors color-idx))
      (line-x0-set! line (fx+ (line-x0 prev) dx0))
      (line-y0-set! line (fx+ (line-y0 prev) dy0))
      (line-x1-set! line (fx+ (line-x1 prev) dx1))
      (line-y1-set! line (fx+ (line-y1 prev) dy1))
      (vl_buffer-draw-line (vl_surface-buffer surface)
                           (line-x0 line) (line-y0 line)
                           (line-x1 line) (line-y1 line)
                           (line-color line))
      (vl_surface-damage! surface)
      (sleep 1/16)
      (letrec ((flip-x (lambda (x dx) (if (fx<=? 5 x max-x) dx (fx- dx))))
               (flip-y (lambda (y dy) (if (fx<=? 5 y max-y) dy (fx- dy)))))
        (lp line
            (fxmod (fx+ idx 1) (vector-length lines))
            (fxmod (fx+ color-idx 1) (vector-length colors))
            (flip-x (line-x0 line) dx0)
            (flip-y (line-y0 line) dy0)
            (flip-x (line-x1 line) dx1)
            (flip-y (line-y1 line) dy1))))))

;; Run the lines demo as a dockapp.
(let* ((y (- (vl_buffer-height framebuf) 60))
       (lines-dockapp
        (vl_display-allocate-user-surface the-display VL_FORMAT_XRGB8888
                                          0 y 60 60)))
  (vl_display-add-dockapp-surface! the-display lines-dockapp)
  (spawn-fiber (lambda ()
                 (run-lines-demo lines-dockapp))))

;; Run the lines demo in a window with decorations.
(let ((lines-window
       (vl_display-allocate-user-surface the-display VL_FORMAT_XRGB8888
                                         550 500 160 120)))
  (vl_surface-property-set! lines-window 'NAME "Lines")
  (vl_display-add-window-surface! the-display lines-window)
  (vl_surface-draw-simple-decorations lines-window)
  (spawn-fiber (lambda ()
                 (run-lines-demo lines-window))))

(vl_display-print-surfaces the-display)

(wait exit-cvar)
