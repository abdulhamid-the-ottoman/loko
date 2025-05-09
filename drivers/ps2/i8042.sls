;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; i8042 keyboard/mouse controller (often called KBC)

;; The Intel 8042 datasheet is less than useful for this driver. It
;; basically describes the chip electrically and its instruction set.

;; This driver uses the 8042's I/O ports to talk with the keyboard
;; controller firmware, which in turn talks with the firmware in the
;; PS/2 keyboard.

;; On a PC the i8042 is responsible for the PS/2 keyboard and PS/2
;; mouse, which are used in emulators and sometimes on laptops. It is
;; also hooked up to the reset line.

;; ACPI knows if the system has an i8042 controller. PS/2 ports are
;; legacy ports, but they are still used in emulators and some
;; laptops.

;; This driver assumes that there are two PS/2 ports.

(library (loko drivers ps2 i8042)
  (export
    driver·isa·i8042)
  (import
    (rnrs (6))
    (loko match)
    (loko u8rings)
    (loko drivers ps2 core)
    (loko system fibers)
    (loko system unsafe)
    (only (loko system $host) enable-irq wait-irq-operation))

;; i8042 KBC commands
(define KBC-READ-COMMAND-BYTE   #x20)
(define KBC-WRITE-COMMAND-BYTE  #x60)
(define KBC-DISABLE-AUX         #xA7)
(define KBC-ENABLE-AUX          #xA8)
(define KBC-SELF-TEST-AND-RESET #xAA)
(define KBC-INTERFACE-TEST      #xAB)
(define KBC-DIAGNOSTIC-DUMP     #xAC)
(define KBC-DISABLE-KEYBOARD    #xAD)
(define KBC-ENABLE-KEYBOARD     #xAE)
(define KBC-READ-INPUT-PORT     #xC0)
(define KBC-READ-OUTPUT-PORT    #xD0)
(define KBC-WRITE-OUTPUT-PORT   #xD1)
(define KBC-WRITE-AUX-PORT      #xD4)
(define KBC-READ-TEST-INPUTS    #xE0)
(define KBC-PULSE-OUTPUT-PORT-n #xF0)

;; Driver for the i8042 on the normal PC ports and IRQs. The
;; data-ports argument is a vector of (rx . tx) channel pairs, one per
;; port (normally two, but with a mux it could be six). The command
;; channel is for commands to the i8042 itself. While sending commands
;; on this last channel you should keep the other ports (i.e. keyboard
;; and mouse) disabled.
(define (driver-i8042 data-ports command-ch)
  (define DEBUG #f)
  (define irq-port-1 1)
  (define irq-port-2 12)
  (define reg-data #x60)
  (define reg-status #x64)
  (define reg-command #x64)
  ;; Bits in the status register. Were different before AUX existed.
  ;; Are different when MUX (Synaptics) is enabled.
  (define STS-OUTPUT-BUFFER-FULL #b00000001)
  (define STS-INPUT-BUFFER-FULL  #b00000010)
  (define STS-SYSTEM-FLAG        #b00000100)
  (define STS-COMMAND/DATA       #b00001000)
  (define STS-AUX                #b00100000)
  (define STS-TIMEOUT            #b01000000)
  (define STS-PARITY             #b10000000)
  ;; i8042 register access
  (define (read-data) (get-i/o-u8 reg-data))
  (define (read-status) (get-i/o-u8 reg-status))
  (define (write-data x) (put-i/o-u8 reg-data x))
  (define (write-command x) (put-i/o-u8 reg-command x))

  ;; Wait for the i8042 to be ready to handle data. Timeout in milliseconds.
  (define (i8042-tx-wait timeout)
    (let lp ((timeout timeout))
      (cond ((eqv? timeout 0)
             'timeout)
            ((not (fxzero? (fxand (read-status) STS-INPUT-BUFFER-FULL)))
             (sleep 1/1000)
             (lp (fx- timeout 1)))
            (else #f))))

  (define (i8042-init)
    ;; We don't want data from the keyboard or mouse during setup
    (i8042-tx-wait 100)
    (write-command KBC-DISABLE-KEYBOARD)
    (i8042-tx-wait 100)
    (write-command KBC-DISABLE-AUX)
    (sleep 2/1000)
    (i8042-discard)
    (vector-for-each u8ring-clear! rx-bufs)

    ;; Let's do a self test. Continue even if the self test fails
    ;; because the keyboard can still be working.
    (i8042-configure 'only-controller)
    (let ((resp (i8042-command KBC-SELF-TEST-AND-RESET #f 1 5000)))
      (unless (equal? resp #vu8(#x55))
        (display "i8042 self-test & reset failed: ")
        (display resp)
        (newline)))
    (i8042-discard)

    ;; Enable the ports and send the command to disable scanning. This
    ;; may reset the devices, but it prevents problems during setup.
    (i8042-configure 'polling)
    (do ((port 0 (fx+ port 1)))
        ((fx=? port (vector-length data-ports)))
      (sleep #e0.008)
      (i8042-tx port #xF5 1000)
      (sleep #e0.008)
      (i8042-discard))
    (vector-for-each u8ring-clear! rx-bufs)

    ;; Normal operations. Enables IRQs and the ports.
    (let ((irq-token-1 (enable-irq irq-port-1))
          (irq-token-2 (enable-irq irq-port-2)))
      (i8042-configure 'normal)
      (values irq-token-1 irq-token-2)))

  ;; Configure the controller. We may want to talk with the controller
  ;; only, in polling mode, or we want normal operation with IRQs.
  (define (i8042-configure mode)
    (define CONF-PORT-1-IRQ-ENABLE    #b00000001)
    (define CONF-PORT-2-IRQ-ENABLE    #b00000010)
    (define CONF-SYSTEM-FLAG          #b00000100)
    (define CONF-PORT-1-CLOCK-DISABLE #b00010000)
    (define CONF-PORT-2-CLOCK-DISABLE #b00100000)
    (define CONF-PORT-1-TRANSLATION   #b01000000)
    (let ((conf (i8042-command KBC-READ-COMMAND-BYTE #f 1 1000)))
      (unless (eqv? (bytevector-length conf) 1)
        (error 'i8042-configure "Could not read current command byte"))
      (let* ((cfgbyte (bytevector-u8-ref conf 0))
             (cfgbyte (fxior (fxand cfgbyte
                                    (fxnot (fxior CONF-PORT-1-TRANSLATION
                                                  CONF-PORT-1-CLOCK-DISABLE
                                                  CONF-PORT-2-CLOCK-DISABLE)))
                             (case mode
                               ((polling)
                                0)
                               ((only-controller)
                                (fxior CONF-PORT-1-CLOCK-DISABLE
                                       CONF-PORT-2-CLOCK-DISABLE
                                       CONF-PORT-1-IRQ-ENABLE
                                       CONF-PORT-2-IRQ-ENABLE))
                               (else
                                (fxior CONF-PORT-1-IRQ-ENABLE
                                       CONF-PORT-2-IRQ-ENABLE))))))
        (i8042-command KBC-WRITE-COMMAND-BYTE cfgbyte 0 1000))))

  ;; Send a command to the i8042 itself and read n response bytes. On
  ;; timeout, fewer bytes are returned in the bytevector. Timeout in
  ;; milliseconds.
  (define (i8042-command cmd-byte data-byte n timeout)
    (i8042-tx-wait timeout)
    (write-command cmd-byte)
    (when data-byte
      (i8042-tx-wait timeout)
      (write-data data-byte))
    (call-with-bytevector-output-port
      (lambda (p)
        (let lp ((timeout timeout) (n n))
          (unless (or (eqv? timeout 0) (eqv? n 0))
            (let ((status (read-status)))
              (cond ((fxzero? (fxand status STS-OUTPUT-BUFFER-FULL))
                     (sleep 1/1000)
                     (lp (fx- timeout 1) n))
                    (else
                     (put-u8 p (read-data))
                     (lp timeout (fx- n 1))))))))))

  ;; Receive data from the KBC and place it in the right buffer. The
  ;; STS-AUX flag is only updated ahead of an IRQ.
  (define (i8042-handle-irq)
    ;; FIXME: STS-AUX is not working correctly. Sometimes STS-AUX is
    ;; set but the data is keyboard data. This shows up in QEMU and on
    ;; a test laptop, but not on a another motherboard. Could be that
    ;; these i8042 controllers hope that IRQ 1 will always interrupt
    ;; IRQ 12?
    (let ((status (read-status)))
      (unless (fxzero? (fxand status STS-OUTPUT-BUFFER-FULL))
        (let* ((data (read-data))
               (port-num (if (fxzero? (fxand status STS-AUX)) 0 1)))
          (when (fxzero? (fxand status (fxior STS-TIMEOUT STS-PARITY)))
            (u8ring-enqueue! (vector-ref rx-bufs port-num) data))
          (when DEBUG
            (write (list '< port-num (number->string data 16)))
            (newline))))))

  ;; Discard all buffered data
  (define (i8042-discard)
    (let lp ((n 16))
      (unless (eqv? n 0)
        (let ((status (read-status)))
          ;; XXX: Old systems need a 7µs sleep here if this is not in
          ;; response to an IRQ.
          (sleep #e7e-6)
          (unless (fxzero? (fxand status STS-OUTPUT-BUFFER-FULL))
            (read-data)
            (lp (fx- n 1)))))))

  ;; Transmit data to one of the ports on the KBC (e.g. the keyboard
  ;; or the mouse). Timeout in milliseconds.
  (define (i8042-tx port byte timeout)
    (i8042-tx-wait timeout)
    (when (eqv? port 1)
      (write-command KBC-WRITE-AUX-PORT))
    (or (i8042-tx-wait timeout)
        (begin (write-data byte) #f)))

  ;; Buffers to hold data read from the PS/2 ports
  (define rx-bufs
    (vector (make-u8ring 64)
            (make-u8ring 64)))

  (define (rx-op port)
    (cond ((u8ring-head (vector-ref rx-bufs port)) =>
           (lambda (byte)
             (wrap-operation (put-operation (PS/2-port-rx-channel (vector-ref data-ports port))
                                            byte)
                             (lambda _ (cons 'rx port)))))
          (else (choice-operation))))
  (define (tx-op port)
    (wrap-operation (get-operation (PS/2-port-tx-channel (vector-ref data-ports port)))
                    (lambda (data) (cons 'tx (cons port data)))))

  (let-values ([(irq-token-1 irq-token-2) (i8042-init)])
    (let loop ()
      (match (perform-operation
              (choice-operation
               (wrap-operation (wait-irq-operation irq-token-1) (lambda _ 'int-port-1))
               (wrap-operation (wait-irq-operation irq-token-2) (lambda _ 'int-port-2))
               (wrap-operation (get-operation command-ch) (lambda (x) (cons 'cmd x)))
               (tx-op 0)
               (tx-op 1)
               (rx-op 0)
               (rx-op 1)))
        ;; Handle and acknowledge IRQs
        ['int-port-1
         (i8042-handle-irq)
         (loop)]
        ['int-port-2
         (i8042-handle-irq)
         (loop)]

        [('rx . port)
         ;; We've passed on a byte received on one of the ports
         (u8ring-dequeue! (vector-ref rx-bufs port))
         (loop)]
        [('tx port . #(ch byte timeout))
         ;; We've been asked to relay data to one of the ports. The
         ;; response will be sent over the rx channel.
         ;; TODO: put-operation
         (when DEBUG
           (write (list '> port (number->string byte 16)))
           (newline))
         (put-message ch (i8042-tx port byte timeout))
         (loop)]

        [('cmd . cmd)
         (match cmd
           [((? channel? ch) 'flush (? fixnum? port))
            (cond ((fx<=? 1 port (vector-length rx-bufs))
                   (u8ring-clear! (vector-ref rx-bufs (fx- port 1)))
                   (put-message ch 'ok)
                   (loop))
                  (else
                   (put-message ch 'out-of-range)
                   (loop)))]
           [((? channel? ch) . _)
            (put-message ch 'bad-command)
            (loop)]
           [else
            (loop)])]))))

(define (driver·isa·i8042 controller)
  (define port-1 (make-PS/2-port controller 1))
  (define port-2 (make-PS/2-port controller 2))
  (define ports (vector port-1 port-2))
  (define command-ch (PS/2-controller-command-channel controller))

  (spawn-fiber (lambda () (driver-i8042 ports command-ch)))

  (spawn-fiber
   (lambda ()
     (vector-for-each
      (lambda (port)
        (put-message (PS/2-controller-notify-channel controller)
                     (cons 'new-port port)))
      ports)))))
