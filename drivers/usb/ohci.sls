;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; Open Host Controller Interface (OpenHCI/OHCI)

;; XXX: This just disables the SMM/BIOS driver for now

(library (loko drivers usb ohci)
  (export
    probe·pci·ohci?
    driver·pci·ohci
    driver·ohci)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko system unsafe)
    (loko system unsafe cache)
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
            'SUBSYSTEM 'usb-ohci))

(define (log/debug . x*) (log/x DEBUG x*))
(define (log/info . x*) (log/x INFO x*))
(define (log/error . x*) (log/x ERROR x*))
(define (log/warn . x*) (log/x WARNING x*))

(define (fxtest a b) (not (eqv? 0 (fxand a b))))

(define (driver·ohci regs regs-size irq controller)
  (define shutdown-cvar (make-cvar))

;;; Register definitions

  (define-mem ohci-regs (endianness little)
    (u32 HcRevision-ref)
    (u32 HcControl-ref HcControl-set!)
    (u32 HcCommandStatus-ref HcCommandStatus-set!)
    (u32 HcInterruptStatus-ref HcInterruptStatus-set!)
    (u32 HcInterruptEnable-ref HcInterruptEnable-set!)
    (u32 HcInterruptDisable-ref HcInterruptDisable-set!)
    (u32 HcHCCA-ref HcHCCA-set!)
    (u32 HcPeriodCurrentED-ref HcPeriodCurrentED-set!)
    (u32 HcControlHeadED-ref HcControlHeadED-set!)
    (u32 HcControlCurrentED-ref HcControlCurrentED-set!)
    (u32 HcBulkHeadED-ref HcBulkHeadED-set!)
    (u32 HcBulkCurrentED-ref HcBulkCurrentED-set!)
    (u32 HcDoneHead-ref)
    (u32 HcFmInterval-ref HcFmInterval-set!)
    (u32 HcFmRemaining-ref)
    (u32 HcFmNumber-ref HcFmNumber-set!)
    (u32 HcPeriodicStart-ref HcPeriodicStart-set!)
    (u32 HcLSThreshold-ref HcLSThreshold-set!)
    (u32 HcRhDescriptorA-ref HcRhDescriptorA-set!)
    (u32 HcRhDescriptorB-ref HcRhDescriptorB-set!)
    (u32 HcRhStatus-ref HcRhStatus-set!))

  (define (HcRhPortStatus-n-ref regs n)
    (get-i/o-u32 (fx+ (fx+ regs #x54) (fx* n 4))))

  (define (HcRhPortStatus-n-set! regs n v)
    (put-i/o-u32 (fx+ (fx+ regs #x54) (fx* n 4)) v))

  ;; HcControl
  (define HcControl-CBSR-mask          #b11)
  (define HcControl-PLE       #b00000000100)
  (define HcControl-IE        #b00000001000)
  (define HcControl-CLE       #b00000010000)
  (define HcControl-BLE       #b00000100000)
  (define HcControl-IR        #b00100000000)
  (define HcControl-RWC       #b01000000000)
  (define HcControl-RWE       #b10000000000)
  (define HcControl-HCFS-mask #b11)
  (define HcControl-HCFS-shift 6)
  (define (HcControl-HCFS-ref regs)
    (fxand HcControl-HCFS-mask
           (fxarithmetic-shift-right (HcControl-ref regs)
                                     HcControl-HCFS-shift)))
  (define HCFS-UsbReset       #b00)
  (define HCFS-UsbResume      #b01)
  (define HCFS-UsbOperational #b10)
  (define HCFS-UsbSuspend     #b11)

  ;; HcCommandStatus
  (define HcCommandStatus-HCR #b0001)
  (define HcCommandStatus-CLF #b0010)
  (define HcCommandStatus-BLF #b0100)
  (define HcCommandStatus-OCR #b1000)
  (define HcCommandStatus-SOC-shift 16)
  (define HcCommandStatus-SOC-mask #b11)

;;; DMA data structures

  ;; Endpoint descriptor format. Four u32s.
  (define ED-dma-mask #xFFFFFFF0)
  (define ED0-FA-shift 0)               ;function address
  (define ED0-FA-mask #b1111111)
  (define ED0-EN-shift 7)               ;endpoint number
  (define ED0-EN-mask #b1111)
  (define ED0-D-shift 11)               ;direction
  (define ED0-D-mask #b11)
  (define ED0-D-from-TD #b00)
  (define ED0-D-OUT     #b01)
  (define ED0-D-IN      #b10)
  (define ED0-S (fxarithmetic-shift-left 1 13)) ;0=full-speed, 1=low-speed
  (define ED0-K (fxarithmetic-shift-left 1 14)) ;HC skips this ED
  (define ED0-F (fxarithmetic-shift-left 1 15)) ;1=isochronous TD, 0=general TD
  (define ED0-MPS-shift 16)                     ;maximum packet size
  (define ED0-MPS-mask #b1111111111)
  (define ED2-H #b01)                   ;halted
  (define ED2-C #b10)                   ;toggle carry

  ;; General transfer descriptor format. Four u32s.
  (define TD-dma-mask #xFFFFFFF0)
  (define TD-idx-flags 0)
  (define TD-idx-CBP 4)                 ;Current Buffer Pointer
  (define TD-idx-NextTD 8)              ;Next TD
  (define TD-idx-BE 12)       ;BufferEnd, physaddr of last buffer byte
  (define TD0-R (fxarithmetic-shift-left 1 18)) ;1=short packets are ok
  (define TD0-DP-shift 19)              ;PID
  (define TD0-DP-mask #b11)
  (define TD0-DP-SETUP #b00)
  (define TD0-DP-OUT   #b01)
  (define TD0-DP-IN    #b10)
  (define TD0-DI-shift 21)              ;delay interrupt (#b111=no interrupt)
  (define TD0-DI-mask #b111)
  (define TD0-T-shift 24)               ;data toggle (can inherit from ED)
  (define TD0-T-mask #b11)
  (define TD0-EC-shift 26)              ;error count
  (define TD0-EC-mask #b11)
  (define TD0-CC-shift 28)              ;condition code
  (define TD0-CC-mask #b1111)

  ;; Isochronous transfer descriptor format. Eight u32s.
  (define TDI0-SF-shift 0)              ;starting frame
  (define TDI0-SF-mask #xffff)
  (define TDI0-DI-shift 21)             ;delay interrupt (#b111=no interrupt)
  (define TDI0-DI-mask #b111)
  (define TDI0-FC-shift 24)             ;frame count
  (define TDI0-FC-mask #b111)
  (define TDI0-CC-shift 28)             ;condition code
  (define TDI0-CC-mask #b1111)

  ;; Completion codes
  (define cc-NOERROR             #b0000)
  (define cc-CRC                 #b0001)
  (define cc-BITSTUFFING         #b0010)
  (define cc-DATATOGGLEMISMATCH  #b0011)
  (define cc-STALL               #b0100)
  (define cc-DEVICENOTRESPONDING #b0101)
  (define cc-PIDCHECKFAILURE     #b0110)
  (define cc-UNEXPECTEDPID       #b0111)
  (define cc-DATAOVERRUN         #b1000)
  (define cc-DATAUNDERRUN        #b1001)
  (define cc-BUFFEROVERRUN       #b1100)
  (define cc-BUFFERUNDERRUN      #b1101)
  (define cc-NOT-ACCESSED-0      #b1110)
  (define cc-NOT-ACCESSED-1      #b1111)

  (define HCCA-size 256)
  (define HccaInterruptTable #x00)      ;32 pointers to interrupt EDs
  (define HccaFrameNumber    #x80)      ;current frame number
  (define HccaPad1           #x82)
  (define HccaDoneHead       #x84)

;;; Some memory management

;;; Driver state

;;; Driver

;;; OHCI Root hub

;;; Interrupt handling

;;; Setup

  (define (claim-controller)
    (cond ((fxtest HcControl-IR (HcControl-ref regs))
           (log/info "Taking over from an active SMM driver")
           (HcCommandStatus-set! regs HcCommandStatus-OCR)
           (let lp ()
             (when (fxtest HcControl-IR (HcControl-ref regs))
               (sleep 0.1)
               ;; FIXME: Timeout?
               (lp))))
          ((not (eqv? HCFS-UsbReset (HcControl-HCFS-ref regs)))
           (log/info "Taking over from an active BIOS driver")
           (when (not (eqv? HCFS-UsbOperational (HcControl-HCFS-ref regs)))
             ;; FIXME: how long to wait for resume?
             (HcControl-set! regs (fxarithmetic-shift-left HCFS-UsbResume HcControl-HCFS-shift))
             (sleep 0.5)))
          (else
           ;; FIXME: how long to wait for reset?
           (log/info "No active driver")
           (sleep 0.5))))

;;; Main HCI loop

  (unless (memv (fxbit-field (HcRevision-ref regs) 0 8)
                '(#x10 #x11))
    (error 'driver·ohci "Invalid HcRevision" (HcRevision-ref regs)))
  (claim-controller)

  #f
)

;; Check that this is a device the driver supports
(define (probe·pci·ohci? dev)
  (and (eqv? (pcidev-base-class dev) #x0c)
       (eqv? (pcidev-sub-class dev) #x03)
       (eqv? (pcidev-interface dev) #x10)))

;; Main procedure for OHCI devices connected by PCI
(define (driver·pci·ohci dev controller)
  (let ((bar (vector-ref (pcidev-BARs dev) 0)))
    (assert (pcibar-mem? bar))
    ;; Enable I/O (BAR0), bus mastering and unmask interrupts.
    (pci-put-u8 dev PCI-CFG-COMMAND
                (fxand (fxior (fxior PCI-CMD-MEM-SPACE
                                     PCI-CMD-BUS-MASTER)
                              (pci-get-u8 dev PCI-CFG-COMMAND))
                       (fxnot PCI-CMD-INTERRUPT-DISABLE)))

    (driver·ohci (pcibar-base bar)
                 (pcibar-size bar)
                 (pcidev-irq dev)
                 controller))))
