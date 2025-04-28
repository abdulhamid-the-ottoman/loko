;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019-2020 G. Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

;; TODO: Lift this into the text-mode package

(library (text-mode termios)
  (export
    termios-get-window-size
    termios-raw-mode
    termios-canonical-mode)
  (import
    (rnrs (6))
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls)
    (loko system unsafe))

(define (termios-get-window-size fd)
  (let ((buf (make-bytevector sizeof-winsize)))
    (sys_ioctl fd TIOCGWINSZ (bytevector-address buf))
    (values (bytevector-u16-native-ref buf offsetof-winsize-ws_col)
            (bytevector-u16-native-ref buf offsetof-winsize-ws_row)
            (bytevector-u16-native-ref buf offsetof-winsize-ws_xpixel)
            (bytevector-u16-native-ref buf offsetof-winsize-ws_ypixel))))

(define original-termios #f)

(define (termios-raw-mode fd)
  (let ((buf (make-bytevector sizeof-termios)))
    (when (eqv? 0 (sys_ioctl fd TCGETS (bytevector-address buf)
                             (lambda (errno) #f)))
      (set! original-termios (bytevector-copy buf))
      (let ((iflag (bytevector-u32-native-ref buf offsetof-termios-c_iflag))
            (oflag (bytevector-u32-native-ref buf offsetof-termios-c_oflag))
            (cflag (bytevector-u32-native-ref buf offsetof-termios-c_cflag))
            (lflag (bytevector-u32-native-ref buf offsetof-termios-c_lflag)))
        ;; Raw mode.
        (let ((iflag (fxand iflag (fxnot (fxior IGNBRK BRKINT PARMRK ISTRIP
                                                INLCR IGNCR ICRNL IXON))))
              (oflag (fxand oflag (fxnot (fxior OPOST))))
              (lflag (fxand lflag (fxnot (fxior ECHO ECHONL ICANON ISIG IEXTEN))))
              (cflag (fxior (fxand cflag (fxnot CSIZE)) CS8)))
          (bytevector-u32-native-set! buf offsetof-termios-c_iflag iflag)
          (bytevector-u32-native-set! buf offsetof-termios-c_oflag oflag)
          (bytevector-u32-native-set! buf offsetof-termios-c_cflag cflag)
          (bytevector-u32-native-set! buf offsetof-termios-c_lflag lflag)
          (bytevector-u8-set! buf (+ offsetof-termios-c_cc VMIN) 1)
          (bytevector-u8-set! buf (+ offsetof-termios-c_cc VTIME) 0)))
      (sys_ioctl fd TCSETSW (bytevector-address buf)))))

(define (termios-canonical-mode fd)
  (sys_ioctl fd TCSETSW (bytevector-address original-termios))))
