;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; USB HID definitions

(library (loko drivers usb hid-numbers)
  (export
    HID-page-LED
    HID-LED-Num-Lock
    HID-LED-Caps-Lock
    HID-LED-Scroll-Lock
    HID-LED-Compose
    HID-LED-Kana

    HID-page-Keyboard/Keypad
    Keyboard-LeftControl
    Keyboard-LeftShift
    Keyboard-LeftAlt
    Keyboard-LeftGUI
    Keyboard-RightControl
    Keyboard-RightShift
    Keyboard-RightAlt
    Keyboard-RightGUI

    HID-page-Buttons

    HID-modifiers-Control
    HID-modifiers-Shift
    HID-modifiers-Alt
    HID-modifiers-AltGr
    HID-modifiers-GUI)
  (import
    (only (rnrs (6))
          define-syntax syntax-rules identifier-syntax
          fxior fxarithmetic-shift-left fx-))

(define-syntax define-inlined
  (syntax-rules ()
    ((_ name v)
     (define-syntax name (identifier-syntax v)))))

(define-inlined HID-page-LED         #x08)
(define-inlined HID-LED-Num-Lock     #x01)
(define-inlined HID-LED-Caps-Lock    #x02)
(define-inlined HID-LED-Scroll-Lock  #x03)
(define-inlined HID-LED-Compose      #x04)
(define-inlined HID-LED-Kana         #x05)

(define-inlined HID-page-Keyboard/Keypad #x07)
(define-inlined Keyboard-LeftControl     #xE0)
(define-inlined Keyboard-LeftShift       #xE1)
(define-inlined Keyboard-LeftAlt         #xE2)
(define-inlined Keyboard-LeftGUI         #xE3)
(define-inlined Keyboard-RightControl    #xE4)
(define-inlined Keyboard-RightShift      #xE5)
(define-inlined Keyboard-RightAlt        #xE6)
(define-inlined Keyboard-RightGUI        #xE7)

(define-inlined HID-page-Buttons #x09)
;; XXX: Buttons are just numbered from 1 to 65535

;; Bitmasks for modifiers

(define-inlined HID-modifiers-Control
  (fxior (fxarithmetic-shift-left 1 (fx- Keyboard-LeftControl Keyboard-LeftControl))
         (fxarithmetic-shift-left 1 (fx- Keyboard-RightControl Keyboard-LeftControl))))

(define-inlined HID-modifiers-Shift
  (fxior (fxarithmetic-shift-left 1 (fx- Keyboard-LeftShift Keyboard-LeftControl))
         (fxarithmetic-shift-left 1 (fx- Keyboard-RightShift Keyboard-LeftControl))))

(define-inlined HID-modifiers-Alt
  (fxarithmetic-shift-left 1 (fx- Keyboard-LeftAlt Keyboard-LeftControl)))

(define-inlined HID-modifiers-GUI
  (fxior (fxarithmetic-shift-left 1 (fx- Keyboard-LeftGUI Keyboard-LeftControl))
         (fxarithmetic-shift-left 1 (fx- Keyboard-RightGUI Keyboard-LeftControl))))

(define-inlined HID-modifiers-AltGr
  (fxarithmetic-shift-left 1 (fx- Keyboard-RightAlt Keyboard-LeftControl))))
