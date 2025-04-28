;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Initialize USB controllers and list devices as they appear

(import
  (rnrs (6))
  (loko match)
  (loko system fibers)
  (loko system logging)
  (loko drivers pci)
  (loko drivers usb core)
  (loko drivers usb uhci)
  (loko drivers usb ehci)
  (loko drivers usb ohci)
  (loko drivers usb hid)
  (loko drivers mouse)
  (loko drivers usb hid-mouse)
  (loko drivers keyboard)
  (loko drivers usb hid-keyboard))

(define logch (make-channel))

(current-log-callback
 (lambda (e)
   (put-message logch e)))

(spawn-fiber
 (lambda ()
   (let lp ()
     (let ((e (get-message logch)))
       (define p (current-error-port))
       (define severity (cdr (assq 'SEVERITY e)))
       (define subsys (cond ((assq 'SUBSYSTEM e) => cdr) (else #f)))
       (display "<" p)
       (display severity p)
       (display ">" p)
       (when subsys
         (display "[" p)
         (display subsys p)
         (display "] " p))
       (cond ((eqv? severity ERROR)
              (display "\x1b;[1;41;33m" p))
             ((eqv? severity INFO)
              (display "\x1b;[1;44;33m" p)))
       (display (cdr (assq 'MESSAGE e)) p)
       (cond ((assq 'EXCEPTION e) =>
              (lambda (e)
                (newline p)
                (display (cdr e) p))))
       (display "\x1b;[m" p)
       (newline p)
       (lp)))))

(define (logprint severity . x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (find-hid-driver hiddev)
  ;; Check if it's a keyboard, mouse, joystick, etc and call the
  ;; appropriate driver.
  (cond
    ((probe·usb·hid·mouse? hiddev)
     (let ((mouse (make-mouse)))
       (spawn-fiber (lambda () (driver·usb·hid·mouse hiddev mouse)))
       (let lp ()
         (logprint INFO "Mouse: " (get-message (mouse-event-channel mouse)))
         (lp))))
    ((probe·usb·hid·keyboard? hiddev)
     (let* ((keyboard-manager (make-keyboard-manager))
            (keyboard (make-managed-keyboard keyboard-manager)))
       (spawn-fiber (lambda () (driver·usb·hid·keyboard hiddev keyboard)))
       (let lp ()
         (logprint INFO "Keyboard: "
                   (call-with-string-output-port
                     (lambda (p)
                       (write (get-message (keyboard-event-channel keyboard-manager))
                              p))))
         (lp))))
    (else
     (let lp ()
       (logprint INFO "HID report: "
                 (perform-operation (usb-pipe-in-operation (usbhid-device-in-pipe hiddev))))
       (lp)))))

(define (manage-usb-hci controller)
  (let lp ()
    (match (get-message (usb-controller-notify-channel controller))
      [('new-device . dev)
       (logprint DEBUG "Fetching descriptors...")
       ;; The device configuration is already set. At this point we
       ;; should find a driver for the device and spawn a fiber to
       ;; handle it.

       (logprint INFO
                 (call-with-string-output-port
                   (lambda (p)
                     (print-usb-descriptor (usb-get-device-descriptor dev) p)
                     (for-each (lambda (cfgdesc*)
                                 (for-each (lambda (desc)
                                             (print-usb-descriptor desc p))
                                           cfgdesc*))
                               (or (usb-device-$configurations dev) '()))
                     (write dev p))))

       (for-each
        (lambda (interface)
          (cond
            ((probe·usb·hid? dev interface)
             (spawn-fiber
              (lambda ()
                (driver·usb·hid dev interface find-hid-driver))))))
        (usb-device-interfaces dev))])
    (lp)))

(send-log INFO "Scanning the PCI bus")
(define devs (pci-scan-bus #f))

(for-each
 (lambda (dev)
   (when (probe·pci·ehci? dev)
     (logprint INFO "Found an EHCI controller: " dev)
     (spawn-fiber
      (lambda ()
        (let ((controller (make-usb-controller)))
          ;; (spawn-fiber (lambda () (manage-usb-hci controller)))
          (driver·pci·ehci dev controller))))))
 devs)

(for-each
 (lambda (dev)
   (cond ((probe·pci·uhci? dev)
          (logprint INFO "Found a UHCI controller: " dev)
          (spawn-fiber
           (lambda ()
             (let ((controller (make-usb-controller)))
               (spawn-fiber (lambda () (manage-usb-hci controller)))
               (driver·pci·uhci dev controller)))))
         ((probe·pci·ohci? dev)
          (logprint INFO "Found an OHCI controller: " dev)
          (spawn-fiber
           (lambda ()
             (let ((controller (make-usb-controller)))
               (spawn-fiber (lambda () (manage-usb-hci controller)))
               (driver·pci·ohci dev controller)))))))
 devs)

(send-log INFO "Waiting for USB devices")

;; Keep the process alive
(let lp ()
  (sleep 60)
  (lp))
