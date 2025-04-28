;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019-2022 G. Weinholt
#!r6rs

;;; Early output via VGA text mode

(library (loko drivers early vga)
  (export
    init-early-vga-driver)
  (import
    (rnrs (6))
    (only (loko runtime io) $init-standard-ports)
    (loko system unsafe))

(define (init-early-vga-driver)
  (define height 25)
  (define width 80)
  (define attr #x4e)
  (define reg-base #x3c0)
  (define crtc-addr (fx+ reg-base 20))
  (define crtc-data (fx+ reg-base 21))
  (define cursor-location-high #x0e)
  (define cursor-location-low #x0f)
  (define vga-base #xb8000)
  (define (crtc-read addr)
    (put-i/o-u8 crtc-addr addr)
    (get-i/o-u8 crtc-data))
  (define (crtc-write addr byte)
    (put-i/o-u8 crtc-addr addr)
    (put-i/o-u8 crtc-data byte))
  (define (scroll)
    (do ((i 0 (fx+ i 4)))
        ((fx=? i (fx* (fx* width 2) height)))
      (put-mem-u32 (fx+ vga-base i)
                   (get-mem-u32 (fx+ (fx+ vga-base (fx* width 2))
                                     i)))))
  (define (textmode-put-u8 b)
    (let* ((pos (fx* 2 (fxior (fxarithmetic-shift-left (crtc-read cursor-location-high) 8)
                              (crtc-read cursor-location-low))))
           (pos (cond ((eqv? b (char->integer #\newline))
                       (let ((line (fxdiv pos (fx* 2 width))))
                         (cond ((eqv? line (fx- height 1))
                                (scroll)
                                (fx* (fx- height 1) (fx* width 2)))
                               (else
                                (fx* (fx+ line 1) (fx* width 2))))))
                      (else
                       (let ((pos (cond ((fx>=? pos (fx* 2 (fx* width height)))
                                         (scroll)
                                         (fx- pos (fx* 2 width)))
                                        (else pos))))
                         (put-mem-u8 (fx+ vga-base pos) b)
                         (put-mem-u8 (fx+ pos (fx+ vga-base 1)) attr)
                         (fx+ pos 2))))))
      (let ((cursor (fxarithmetic-shift-right pos 1)))
        (crtc-write cursor-location-high (fxbit-field cursor 8 16))
        (crtc-write cursor-location-low (fxbit-field cursor 0 8)))))
  (define (textmode-put bv start count)
    (do ((end (fx+ start count))
         (i start (fx+ i 1)))
        ((fx=? i end) count)
      (textmode-put-u8 (bytevector-u8-ref bv i))))
  (do ((last (fx* (fx* width 2) height))
       (i 0 (fx+ i 4)))
      ((fx=? i (fx* width 2)))
    (put-mem-u32 (fx+ (fx+ last vga-base) i) (fx* attr #x01000100)))
  ($init-standard-ports (lambda _ 0) textmode-put textmode-put
                        (buffer-mode line) (native-eol-style))))
