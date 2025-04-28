;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Enhanced Host Controller Interface (EHCI)

;; XXX: For now just a dummy driver to reset the controller.

(library (loko drivers usb ehci)
  (export
    probe·pci·ehci?
    driver·pci·ehci
    driver·ehci)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko system unsafe)
    (only (loko system $host) dma-allocate dma-free
          enable-irq wait-irq-operation)
    (loko drivers pci)
    (loko drivers utils)
    (loko drivers usb core)
    (loko drivers usb hub)
    (struct pack))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'usb-ehci))

(define (log/debug . x*)
  (log/x DEBUG x*))

(define (driver·ehci cap-regs regs-size irq controller)
  (define shutdown-cvar (make-cvar))

  ;; Host controller capability registers
  (define-mem ehci-cap-regs (endianness little)
    (u8 CAPLENGTH-ref)
    (u8)
    (u16 HCIVERSION-ref)
    (u32 HCSPARAMS-ref)
    (u32 HCCPARAMS-ref)
    (s61 HCSP-PORTROUTE-ref))

  ;; Host controller operational registers.
  ;; Located at BAR + CAPLENGTH.
  (define-mem ehci-op-regs (endianness little)
    (u32 USBCMD-ref USBCMD-set!)
    (u32 USBSTS-ref USBSTS-set!)
    (u32 USBINTR-ref USBINTR-set!)
    (u32 FRINDEX-ref FRINDEX-set!)
    (u32 CTRLDSSEGMENT-ref CTRLDSSEGMENT-set!)
    (u32 PERIODICLISTBASE-ref PERIODICLISTBASE-set!)
    (u32 ASYNCLISTADDR-ref ASYNCLISTADDR-set!))

  (define regs (fx+ cap-regs (CAPLENGTH-ref cap-regs)))

  (define (PortSCn-ref regs n)
    (get-i/o-u32 (fx+ (fx+ regs #x44) (fx* n 4))))

  (define (PortSCn-set! regs n v)
    (put-i/o-u32 (fx+ (fx+ regs #x44) (fx* n 4)) v))

;;; Register definitions

  (define CMD-HCRESET  #b10)      ;Host Controller Reset

;;; Hardware (DMA) data structures

  (define (init-controller)
    (log/debug "Starting EHCI controller...")

    ;; Reset and wait for it to complete
    (USBCMD-set! regs CMD-HCRESET)
    (do ((i 0 (fx+ i 1)))
        ((or (fx=? i 50)
             (fxzero? (fxand CMD-HCRESET (USBCMD-ref regs)))))
      (sleep 0.001)))

  (define (uninit-controller)
    (log/debug "Stopping EHCI controller...")

    (USBCMD-set! regs 0))

  (define (cleanup)
    (uninit-controller))

;;; Main loop

  (define root-hub (usb-controller-root-hub controller))
  (define request-ch (usb-controller-request-channel controller))
  (define notify-ch (usb-controller-notify-channel controller))

  (log/debug "EHCI dummy driver")

  (init-controller)
  (cleanup))

;; Check that this is a device the driver supports
(define (probe·pci·ehci? dev)
  (and (eqv? (pcidev-base-class dev) #x0c)
       (eqv? (pcidev-sub-class dev) #x03)
       (eqv? (pcidev-interface dev) #x20)))

;; Main procedure for EHCI devices connected by PCI
(define (driver·pci·ehci dev controller)
  (let ((bar (vector-ref (pcidev-BARs dev) 0)))
    (assert (pcibar-mem? bar))
    ;; Enable memory-mapped registers (BAR0), bus mastering and unmask
    ;; interrupts.
    (pci-put-u8 dev PCI-CFG-COMMAND
                (fxand (fxior (fxior PCI-CMD-MEM-SPACE
                                     PCI-CMD-BUS-MASTER)
                              (pci-get-u8 dev PCI-CFG-COMMAND))
                       (fxnot (fxior PCI-CMD-INTERRUPT-DISABLE
                                     PCI-CMD-I/O-SPACE))))

    (driver·ehci (pcibar-base bar)
                 (pcibar-size bar)
                 (pcidev-irq dev)
                 controller))))
