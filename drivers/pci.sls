;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; PCI bus enumeration

(library (loko drivers pci)
  (export
    ;; Main entry point
    pci-scan-bus

    ;; Scanning the bus returns a list of these
    pcidev?
    pcidev-bus pcidev-dev pcidev-func
    pcidev-vendor-id pcidev-device-id
    pcidev-base-class pcidev-sub-class pcidev-interface
    pcidev-irq
    pcidev-BARs
    pcidev-ROM-size
    pcidev-bridge

    ;; pcidev-BARs is a vector of these things
    pcibar? pcibar-reg pcibar-base pcibar-size
    pcibar-i/o?
    pcibar-mem? pcibar-mem-type pcibar-mem-prefetchable?

    ;; Get and put in PCI configuration space
    pci-get-u8 pci-get-u16 pci-get-u32
    pci-put-u8 pci-put-u16 pci-put-u32

    ;; Higher-level accessors can go here
    pcidev-header-layout
    pcidev-multifunction?

    ;; Get the capabilities list
    pcidev-get-capabilities
    pcicap?
    pcicap-id
    pcicap-reg

    ;; PCI-to-PCI bridges
    pcibridge?
    pcibridge-i/o
    pcibridge-i/o-32bit?
    pcibridge-memory
    pcibridge-memory-set!
    with-pcibridge-memory
    pcibridge-prefetchable
    pcibridge-prefetchable-64bit?

    ;; Offsets in PCI configuration space
    PCI-CFG-VENDOR-ID
    PCI-CFG-DEVICE-ID
    PCI-CFG-COMMAND
    PCI-CFG-STATUS
    PCI-CFG-REVISION-ID
    PCI-CFG-INTERFACE
    PCI-CFG-SUB-CLASS
    PCI-CFG-BASE-CLASS
    PCI-CFG-CACHE-LINE-SIZE
    PCI-CFG-LATENCY-TIMER
    PCI-CFG-HEADER-TYPE
    PCI-CFG-BIST

    ;; Further offsets within layout 0
    PCI-CFG-00-BASE-ADDRESS-REGISTER-0
    PCI-CFG-00-BASE-ADDRESS-REGISTER-1
    PCI-CFG-00-BASE-ADDRESS-REGISTER-2
    PCI-CFG-00-BASE-ADDRESS-REGISTER-3
    PCI-CFG-00-BASE-ADDRESS-REGISTER-4
    PCI-CFG-00-BASE-ADDRESS-REGISTER-5
    PCI-CFG-00-CARDBUS-CIS-POINTER
    PCI-CFG-00-SUBSYSTEM-VENDOR-ID
    PCI-CFG-00-SUBSYSTEM-ID
    PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS
    PCI-CFG-00-CAPABILITIES-POINTER
    PCI-CFG-00-INTERRUPT-LINE
    PCI-CFG-00-INTERRUPT-PIN
    PCI-CFG-00-MIN-GNT
    PCI-CFG-00-MAX-LAT

    ;; Further offsets within layout 1
    PCI-CFG-01-BASE-ADDRESS-REGISTER-0
    PCI-CFG-01-BASE-ADDRESS-REGISTER-1
    PCI-CFG-01-PRIMARY-BUS
    PCI-CFG-01-SECONDARY-BUS
    PCI-CFG-01-SUBORDINATE-BUS
    PCI-CFG-01-SECONDARY-LATENCY-TIMER
    PCI-CFG-01-I/O-BASE
    PCI-CFG-01-I/O-LIMIT
    PCI-CFG-01-SECONDARY-STATUS
    PCI-CFG-01-MEMORY-BASE
    PCI-CFG-01-MEMORY-LIMIT
    PCI-CFG-01-PREFETCHABLE-BASE
    PCI-CFG-01-PREFETCHABLE-LIMIT
    PCI-CFG-01-PREFETCHABLE-BASE-UPPER-32
    PCI-CFG-01-PREFETCHABLE-LIMIT-UPPER-32
    PCI-CFG-01-I/O-BASE-UPPER-16
    PCI-CFG-01-I/O-LIMIT-UPPER-16
    PCI-CFG-01-CAPABILITIES-POINTER
    PCI-CFG-01-EXPANSION-ROM-BASE-ADDDRESS
    PCI-CFG-01-INTERRUPT-LINE
    PCI-CFG-01-INTERRUPT-PIN
    PCI-CFG-01-BRIDGE-CONTROL

    ;; Bits in the PCI-CFG-COMMAND register
    PCI-CMD-I/O-SPACE
    PCI-CMD-MEM-SPACE
    PCI-CMD-BUS-MASTER
    PCI-CMD-SPECIAL-CYCLES
    PCI-CMD-MEM-WR&INV-ENABLE
    PCI-CMD-VGA-PALETTE-SNOOP
    PCI-CMD-PARITY-ERROR-RESPONSE
    ;;PCI-CMD-RESERVED-ZERO
    PCI-CMD-SERR-ENABLE
    PCI-CMD-FAST-B2B-ENABLE
    PCI-CMD-INTERRUPT-DISABLE

    ;; Bits in the PCI-CFG-01-BRIDGE-CONTROL register
    PCI-BCTRL-PARITY-ERROR-RESPONSE
    PCI-BCTRL-SERR-ENABLE
    PCI-BCTRL-ISA-ENABLE
    PCI-BCTRL-VGA-ENABLE
    PCI-BCTRL-VGA-16-BIT
    PCI-BCTRL-MASTER-ABORT-MODE
    PCI-BCTRL-SECONDARY-BUS-RESET
    PCI-BCTRL-FAST-B2B-ENABLE
    PCI-BCTRL-PRIMARY-DISCARD-TIMER
    PCI-BCTRL-SECONDARY-DISCARD-TIMER
    PCI-BCTRL-DISCARD-TIMER-STATUS
    PCI-BCTRL-DISCARD-TIMER-SERR-ENABLE

    ;; Low-level PCI configuration space access. These should normally
    ;; not be used in drivers.
    (rename (make-selector make-pci-selector)
            (get-conf-u8 pci-get-conf-u8)
            (get-conf-u16 pci-get-conf-u16)
            (get-conf-u32 pci-get-conf-u32)))
  (import
    (rnrs (6))
    (loko system unsafe))

;; A PCI device
(define-record-type pcidev
  (nongenerative pcidev-64fe8487-ec71-4d7a-84e3-b0d0faebd341)
  (fields bus                           ;bus number
          dev                           ;device number
          func                          ;function number
          vendor-id device-id           ;IDs
          ;; The three bytes in the device class field
          base-class sub-class interface
          irq                           ;interrupt line
          BARs                          ;a vector of pcibars
          ROM-size                      ;expansion ROM size in bytes
          bridge))                      ;a pcibridge or #f

;; A PCI-to-PCI bridge
(define-record-type pcibridge
  (nongenerative pcibridge-16c35889-97ea-4ae0-bb42-36d0dec6514f)
  (sealed #t)
  (parent pcidev)
  (fields primary
          secondary
          subordinate))

;; Base address registers. These represent the device registers and
;; they are mapped in either I/O (get-i/o-* / put-i/o-*) space or
;; memory (get-mem-* / put-mem-*).
(define-record-type pcibar
  (nongenerative pcibar-5b770b3e-3d24-47ef-a981-abece7fef84a)
  (fields reg                           ;config header register
          base                          ;base address of the registers
          size))                        ;size of the registers

(define-record-type pcibar-i/o          ;I/O bus
  (nongenerative pcibar-i/o-aa0487d4-ab88-406d-8e3d-1613edeb1d5e)
  (sealed #t)
  (parent pcibar))

(define-record-type pcibar-mem          ;memory bus
  (nongenerative pcibar-mem-27fd4f76-a2ef-4791-99d9-d60fc89dc444)
  (sealed #t)
  (parent pcibar)
  (fields type                          ;32-bit, 64-bit, 20-bit ISA (1 MB)
          prefetchable?))               ;memory can be prefetched

;;; User-visible accessors

(define (pci-get-u8 pcidev idx)
  (get-conf-u8 (pcidev-sel pcidev) idx))

(define (pci-get-u16 pcidev idx)
  (get-conf-u16 (pcidev-sel pcidev) idx))

(define (pci-get-u32 pcidev idx)
  (get-conf-u32 (pcidev-sel pcidev) idx))

(define (pci-put-u8 pcidev idx v)
  (put-conf-u8 (pcidev-sel pcidev) idx v))

(define (pci-put-u16 pcidev idx v)
  (put-conf-u16 (pcidev-sel pcidev) idx v))

(define (pci-put-u32 pcidev idx v)
  (put-conf-u32 (pcidev-sel pcidev) idx v))

;;; Access to specific fields

(define (pcidev-header-layout pcidev)
  ;; Layout #x00 follows PCI-CFG-* and PCI-CFG-00-*
  (fxbit-field (pci-get-u8 pcidev PCI-CFG-HEADER-TYPE) 0 7))

(define (pcidev-multifunction? pcidev)
  (fxbit-set? (pci-get-u8 pcidev PCI-CFG-HEADER-TYPE) 7))

;;; Accessors for the PCI configuration space via I/O registers

;; Normal PC-AT location of the PCI configuration. Only mechanism #1
;; is used.
(define *addr* #xcf8)
(define *data* #xcfc)

;; Constructs a "sel" value, used below.
(define (make-selector bus dev func)
  (define CONFIG-ENABLE #x80000000)
  (bitwise-ior CONFIG-ENABLE
               (bitwise-arithmetic-shift-left bus 16)
               (fxarithmetic-shift-left dev 11)
               (fxarithmetic-shift-left func 8)))

(define (pcidev-sel pcidev)
  (make-selector (pcidev-bus pcidev)
                 (pcidev-dev pcidev)
                 (pcidev-func pcidev)))

(define (config-select! sel reg)
  ;; TODO: Configuration space is little endian.
  (assert (eq? (native-endianness) (endianness little)))
  (put-i/o-u32 *addr* (fxior sel (fxand reg #b11111100)))
  (fx+ *data* (fxand reg #b11)))

;; Reads from PCI configuration space.
(define (get-conf-u8 sel reg)
  (get-i/o-u8 (config-select! sel reg)))

(define (get-conf-u16 sel reg)
  (get-i/o-u16 (config-select! sel reg)))

(define (get-conf-u32 sel reg)
  (get-i/o-u32 (config-select! sel reg)))

;; Writes to PCI configuration space.
(define (put-conf-u8 sel reg value)
  (put-i/o-u8 (config-select! sel reg) value))

(define (put-conf-u16 sel reg value)
  (put-i/o-u16 (config-select! sel reg) value))

(define (put-conf-u32 sel reg value)
  (put-i/o-u32 (config-select! sel reg) value))

;;; PCI configuration space

;; Layout out the PCI configuration space header
(define PCI-CFG-VENDOR-ID       #x00)
(define PCI-CFG-DEVICE-ID       #x02)
(define PCI-CFG-COMMAND         #x04)
(define PCI-CFG-STATUS          #x06)
(define PCI-CFG-REVISION-ID     #x08)
(define PCI-CFG-INTERFACE       #x09)
(define PCI-CFG-SUB-CLASS       #x0A)
(define PCI-CFG-BASE-CLASS      #x0B)
(define PCI-CFG-CACHE-LINE-SIZE #x0C)
(define PCI-CFG-LATENCY-TIMER   #x0D)
(define PCI-CFG-HEADER-TYPE     #x0E)
(define PCI-CFG-BIST            #x0F)

;; Layout of the Type 00h configuration space header
(define PCI-CFG-00-BASE-ADDRESS-REGISTER-0     #x10)
(define PCI-CFG-00-BASE-ADDRESS-REGISTER-1     #x14)
(define PCI-CFG-00-BASE-ADDRESS-REGISTER-2     #x18)
(define PCI-CFG-00-BASE-ADDRESS-REGISTER-3     #x1C)
(define PCI-CFG-00-BASE-ADDRESS-REGISTER-4     #x20)
(define PCI-CFG-00-BASE-ADDRESS-REGISTER-5     #x24)
(define PCI-CFG-00-CARDBUS-CIS-POINTER         #x28)
(define PCI-CFG-00-SUBSYSTEM-VENDOR-ID         #x2C)
(define PCI-CFG-00-SUBSYSTEM-ID                #x2E)
(define PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS #x30)
(define PCI-CFG-00-CAPABILITIES-POINTER        #x34)
(define PCI-CFG-00-INTERRUPT-LINE              #x3C) ;x86: IRQ
(define PCI-CFG-00-INTERRUPT-PIN               #x3D) ;INTx#
(define PCI-CFG-00-MIN-GNT                     #x3E)
(define PCI-CFG-00-MAX-LAT)

;; Layout for Type 01h (bridges)
(define PCI-CFG-01-BASE-ADDRESS-REGISTER-0     #x10)
(define PCI-CFG-01-BASE-ADDRESS-REGISTER-1     #x14)
(define PCI-CFG-01-PRIMARY-BUS                 #x18)
(define PCI-CFG-01-SECONDARY-BUS               #x19)
(define PCI-CFG-01-SUBORDINATE-BUS             #x1A)
(define PCI-CFG-01-SECONDARY-LATENCY-TIMER     #x1B)
(define PCI-CFG-01-I/O-BASE                    #x1C)
(define PCI-CFG-01-I/O-LIMIT                   #x1D)
(define PCI-CFG-01-SECONDARY-STATUS            #x1E)
(define PCI-CFG-01-MEMORY-BASE                 #x20)
(define PCI-CFG-01-MEMORY-LIMIT                #x22)
(define PCI-CFG-01-PREFETCHABLE-BASE           #x24)
(define PCI-CFG-01-PREFETCHABLE-LIMIT          #x26)
(define PCI-CFG-01-PREFETCHABLE-BASE-UPPER-32  #x28)
(define PCI-CFG-01-PREFETCHABLE-LIMIT-UPPER-32 #x2C)
(define PCI-CFG-01-I/O-BASE-UPPER-16           #x30)
(define PCI-CFG-01-I/O-LIMIT-UPPER-16          #x32)
(define PCI-CFG-01-CAPABILITIES-POINTER        #x34)
(define PCI-CFG-01-EXPANSION-ROM-BASE-ADDDRESS #x38)
(define PCI-CFG-01-INTERRUPT-LINE              #x3C) ;x86: IRQ
(define PCI-CFG-01-INTERRUPT-PIN               #x3D) ;INTx#
(define PCI-CFG-01-BRIDGE-CONTROL              #x3E)

;; Constants for the command register
(define PCI-CMD-I/O-SPACE             #b00000000001)
(define PCI-CMD-MEM-SPACE             #b00000000010)
(define PCI-CMD-BUS-MASTER            #b00000000100)
(define PCI-CMD-SPECIAL-CYCLES        #b00000001000)
(define PCI-CMD-MEM-WR&INV-ENABLE     #b00000010000)
(define PCI-CMD-VGA-PALETTE-SNOOP     #b00000100000)
(define PCI-CMD-PARITY-ERROR-RESPONSE #b00001000000)
;;(define PCI-CMD-RESERVED-ZERO         #b00010000000)
(define PCI-CMD-SERR-ENABLE           #b00100000000)
(define PCI-CMD-FAST-B2B-ENABLE       #b01000000000)
(define PCI-CMD-INTERRUPT-DISABLE     #b10000000000)

;; Constants for the bridge control register
(define PCI-BCTRL-PARITY-ERROR-RESPONSE     #b000000000001)
(define PCI-BCTRL-SERR-ENABLE               #b000000000010)
(define PCI-BCTRL-ISA-ENABLE                #b000000000100)
(define PCI-BCTRL-VGA-ENABLE                #b000000001000)
(define PCI-BCTRL-VGA-16-BIT                #b000000010000)
(define PCI-BCTRL-MASTER-ABORT-MODE         #b000000100000)
(define PCI-BCTRL-SECONDARY-BUS-RESET       #b000001000000)
(define PCI-BCTRL-FAST-B2B-ENABLE           #b000010000000)
(define PCI-BCTRL-PRIMARY-DISCARD-TIMER     #b000100000000)
(define PCI-BCTRL-SECONDARY-DISCARD-TIMER   #b001000000000)
(define PCI-BCTRL-DISCARD-TIMER-STATUS      #b010000000000)
(define PCI-BCTRL-DISCARD-TIMER-SERR-ENABLE #b100000000000)

;; PCI configuration space layouts
(define PCI-LAYOUT-BRIDGE #x01)

(define (device-header-layout sel)
  (fxbit-field (get-conf-u8 sel PCI-CFG-HEADER-TYPE) 0 7))

;;; Capabilities

(define-record-type pcicap
  (nongenerative pcicap-1f83c972-0911-492a-a60f-ec50b4a81973)
  (sealed #t)
  (fields id                            ;capability id
          reg))                         ;base register (use pci-get/put-*)

(define (pcidev-get-capabilities dev)
  (define seen (make-eqv-hashtable))
  (case (pcidev-header-layout dev)
    ((#x00 #x01)
     (let lp ((ptr (pci-get-u8 dev PCI-CFG-00-CAPABILITIES-POINTER))
              (caps '()))
       (if (or (eqv? ptr 0)
               (not (eqv? 0 (fxand ptr #b11)))
               (hashtable-ref seen ptr #f))
           (reverse caps)
           (let* ((cap (pci-get-u32 dev ptr))
                  (next (fxbit-field cap 8 16))
                  (cid (fxbit-field cap 0 8)))
             (hashtable-set! seen ptr #t)
             (lp next (cons (make-pcicap cid ptr) caps))))))
    (else '())))

;;; PCI-to-PCI bridges

;; Find the I/O base and limit for a bridge. If the limit is less than
;; the base then no forwarding is done from primary to secondary bus.
(define (pcibridge-i/o dev)
  (assert (pcibridge? dev))
  (let ((base (pci-get-u8 dev PCI-CFG-01-I/O-BASE))
        (limit (pci-get-u8 dev PCI-CFG-01-I/O-LIMIT))
        (base-ext (pci-get-u16 dev PCI-CFG-01-I/O-BASE-UPPER-16))
        (limit-ext (pci-get-u16 dev PCI-CFG-01-I/O-LIMIT-UPPER-16)))
    (let ((base (fxior (fxarithmetic-shift-left (fxbit-field base 4 8) 12)
                       (fxarithmetic-shift-left base-ext 16)))
          (limit (fxior (fxarithmetic-shift-left (fxbit-field limit 4 8) 12)
                        (fxarithmetic-shift-left limit-ext 16))))
      (values base (fx+ limit #xFFF)))))

(define (pcibridge-i/o-32bit? dev)
  (let ((base (pci-get-u8 dev PCI-CFG-01-I/O-BASE)))
    (eqv? (fxbit-field base 0 4) #x1)))

;; Find the base and limit of the memory accesses forwarded by the
;; bridge. If limit<base then no forwarding is done.
(define (pcibridge-memory dev)
  (assert (pcibridge? dev))
  (let ((base (pci-get-u16 dev PCI-CFG-01-MEMORY-BASE))
        (limit (pci-get-u16 dev PCI-CFG-01-MEMORY-LIMIT)))
    (let ((base (fxarithmetic-shift-left (fxbit-field base 4 16) 20))
          (limit (fxarithmetic-shift-left (fxbit-field limit 4 16) 20)))
      (values base (fx+ limit #xFFFFF)))))

(define (pcibridge-memory-set! dev base limit)
  (assert (pcibridge? dev))
  (assert (fx=? (fxand limit #xFFFFF) #xFFFFF))
  (assert (fx=? (fxand base #xFFFFF) 0))
  (assert (fx<=? base #xFFF00000))
  (assert (fx<=? limit #xFFFFFFFF))
  (pci-put-u16 dev PCI-CFG-01-MEMORY-BASE
               (fxarithmetic-shift-left (fxbit-field base 20 32) 4))
  (pci-put-u16 dev PCI-CFG-01-MEMORY-LIMIT
               (fxarithmetic-shift-left (fxbit-field limit 20 32) 4)))

;; Run proc while the bridge (and its bridges) temporarily are
;; configured to forward the memory in [base,base+size). Bridges that
;; are already forwarding accesses are not reconfigured. XXX: this
;; should only be useful if the BIOS is buggy and only briefly, e.g.
;; to copy a ROM. If there are other devices on the same bridge then
;; their memory BARs must not be accessed while this procedure is
;; running, so don't do anything funny in proc that would cause a
;; context switch.
(define (with-pcibridge-memory bridge base size proc)
  (let ((new-base (bitwise-and base (fxnot #xFFFFF)))
        (new-limit (bitwise-ior (+ base (- size 1)) #xFFFFF)))
    (let lp ((bridge bridge))
      (cond ((not bridge)
             (proc))
            (else
             (let-values ([(old-base old-limit) (pcibridge-memory bridge)])
               (cond ((and (<= old-base base old-limit)
                           (<= old-base (+ base (- size 1)) old-limit))
                      (proc))
                     (else
                      (dynamic-wind
                        (lambda ()
                          (pcibridge-memory-set! bridge new-base new-limit))
                        (lambda ()
                          (lp (pcidev-bridge bridge)))
                        (lambda ()
                          (pcibridge-memory-set! bridge old-base old-limit)))))))))))

;; Find the base and limit of the prefetchable memory accesses
;; forwarded by the bridge. If limit<base then no forwarding is done.
;; Prefetchable memory has no side-effects when read.
(define (pcibridge-prefetchable dev)
  (assert (pcibridge? dev))
  (let ((base (pci-get-u16 dev PCI-CFG-01-PREFETCHABLE-BASE))
        (limit (pci-get-u16 dev PCI-CFG-01-PREFETCHABLE-LIMIT))
        (base-ext (pci-get-u32 dev PCI-CFG-01-PREFETCHABLE-BASE-UPPER-32))
        (limit-ext (pci-get-u32 dev PCI-CFG-01-PREFETCHABLE-LIMIT-UPPER-32)))
    (let ((base (bitwise-ior (fxarithmetic-shift-left (fxbit-field base 4 16) 20)
                             (bitwise-arithmetic-shift-left base-ext 32)))
          (limit (bitwise-ior (fxarithmetic-shift-left (fxbit-field limit 4 16) 20)
                              (bitwise-arithmetic-shift-left limit-ext 32))))
      (values base (fx+ limit #xFFFFF)))))

(define (pcibridge-prefetchable-64bit? dev)
  (let ((base (pci-get-u8 dev PCI-CFG-01-PREFETCHABLE-BASE)))
    (eqv? (fxbit-field base 0 4) #x1)))

;;; Base Address Registers (BARs)

(define (pci-probe-base-address-registers sel)
  (define (pci-test-size32 sel reg data mask max)
    ;; Write all ones to this base register. When reading it back
    ;; there will be zeroes in the don't-care bits. TODO: do this for
    ;; 64-bit bars and 32-bit I/O bases
    (put-conf-u32 sel reg #xffffffff)
    (let* ((dc (get-conf-u32 sel reg))
           (size (fx+ 1 (fxxor max (fxand mask dc)))))
      ;; Restore the BAR
      (put-conf-u32 sel reg data)
      size))
  (define (pci-get-BARs sel first-reg max-reg)
    (let lp ((reg first-reg)
             (ret '()))
      (if (fx>? reg max-reg)
          (list->vector (reverse ret))
          (let ((bar-data (get-conf-u32 sel reg)))
            (cond
              ((fxzero? bar-data)
               (lp (fx+ reg 4) (cons #f ret)))
              ((fxbit-set? bar-data 0)
               ;; Pointer to I/O space
               (let* ((mask (fxand #xFFFF (fxnot #b1)))
                      (base (fxand mask bar-data))
                      (size (pci-test-size32 sel reg bar-data mask #xffff)))
                 (lp (fx+ reg 4)
                     (cons (make-pcibar-i/o reg base size)
                           ret))))
              (else
               ;; Memory mapped registers. The 20 type (ISA) needs
               ;; memory in the first 1MB.
               (let* ((mask (fxnot #b1111))
                      (type (vector-ref '#(32 20 64 reserved)
                                        (fxbit-field bar-data 1 3)))
                      (base (fxand mask bar-data))
                      (size (pci-test-size32 sel reg bar-data mask #xffffffff))
                      (prefetchable? (fxbit-set? bar-data 3)))
                 (case type
                   ((32 20)
                    (lp (fx+ reg 4)
                        (cons (make-pcibar-mem reg base size type prefetchable?)
                              ret)))
                   ((64)               ;takes up two BARs
                    ;; FIXME: This is not tested and sizing is not
                    ;; done properly
                    (let ((base^ (fxior (bitwise-arithmetic-shift-left
                                         (get-conf-u32 sel (fx+ reg 4))
                                         32)
                                        base)))
                      (lp (fx+ reg 8)
                          (cons #f
                                (cons (make-pcibar-mem reg base^ size type prefetchable?)
                                      ret)))))
                   (else
                    ;; Reserved type.
                    (lp (fx+ reg 4) (cons #f ret)))))))))))
  (case (device-header-layout sel)
    ((#x00) (pci-get-BARs sel PCI-CFG-00-BASE-ADDRESS-REGISTER-0 PCI-CFG-00-BASE-ADDRESS-REGISTER-5))
    ((#x01) (pci-get-BARs sel PCI-CFG-01-BASE-ADDRESS-REGISTER-0 PCI-CFG-01-BASE-ADDRESS-REGISTER-1))
    (else #f)))

;; Expansion ROM size in bytes
(define (probe-expansion-rom-size sel reg)
  (let ((saved (get-conf-u32 sel reg)))
    (put-conf-u32 sel reg #xfffffffe)
    (let ((bits (get-conf-u32 sel reg)))
      (put-conf-u32 sel reg saved)
      (let ((size (bitwise-and #xffffffff
                               (+ 1 (bitwise-xor #xffffffff
                                                 (bitwise-and #xfffffffe
                                                              bits))))))
        (and (not (eqv? size 0)) size)))))

;; Scan a PCI bus, calling proc on each function it finds
(define (pci-scan-single-bus proc bus)
  (define (device-present? sel)
    (not (eqv? (get-conf-u16 sel PCI-CFG-VENDOR-ID) #xFFFF)))
  (define (device-multifunction? sel)
    (fxbit-set? (get-conf-u8 sel PCI-CFG-HEADER-TYPE) 7))
  ;; Each bus has 32 slots. Each slot may have up to eight functions.
  (do ((dev 0 (fx+ dev 1)))
      ((eqv? dev 32))
    (let ((sel (make-selector bus dev 0)))
      (when (device-present? sel)
        (proc bus dev 0)
        (when (device-multifunction? sel)
          ;; Scan the functions.
          (do ((func 1 (fx+ func 1)))
              ((eqv? func 8))
            (let ((sel (make-selector bus dev func)))
              (when (device-present? sel)
                (proc bus dev func)))))))))

;; Scan all PCI buses on the host
(define (pci-scan-host proc)
  (define *highest-bus* 0)
  (define (new-device bus slot func)
    (let ((sel (make-selector bus slot func)))
      (when (eqv? (device-header-layout sel) PCI-LAYOUT-BRIDGE)
        (set! *highest-bus* (fxmax *highest-bus* (get-conf-u8 sel PCI-CFG-01-SUBORDINATE-BUS))))
      (proc bus slot func)))
  (pci-scan-single-bus new-device 0)
  (do ((bus 1 (fx+ bus 1)))
      ((fx>? bus *highest-bus*))
    (pci-scan-single-bus new-device bus)))

;; Scan the PCI bus
(define (pci-scan-bus _access-metod)
  (define (device-vendor-id sel) (get-conf-u16 sel PCI-CFG-VENDOR-ID))
  (define (device-device-id sel) (get-conf-u16 sel PCI-CFG-DEVICE-ID))
  (define (device-base-class sel) (get-conf-u8 sel PCI-CFG-BASE-CLASS))
  (define (device-sub-class sel) (get-conf-u8 sel PCI-CFG-SUB-CLASS))
  (define (device-interface sel) (get-conf-u8 sel PCI-CFG-INTERFACE))
  (define (device-interrupt-line sel) (get-conf-u8 sel PCI-CFG-00-INTERRUPT-LINE))
  (define ret '())
  (define bridges (make-eqv-hashtable))
  (define (mk-pci-device bus dev func)
    (let* ((sel (make-selector bus dev func))
           (layout (device-header-layout sel))
           (bars (pci-probe-base-address-registers sel)))
      (let* ((pcidev-args (list bus dev func
                                (device-vendor-id sel)
                                (device-device-id sel)
                                (device-base-class sel)
                                (device-sub-class sel)
                                (device-interface sel)
                                (case layout
                                  ((#x00 #x01) (device-interrupt-line sel))
                                  (else #f))
                                bars
                                (case layout
                                  ((#x00) (probe-expansion-rom-size sel PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS))
                                  ((#x01) (probe-expansion-rom-size sel PCI-CFG-01-EXPANSION-ROM-BASE-ADDDRESS))
                                  (else #f))
                                (hashtable-ref bridges bus #f)))
             (dev (if (eqv? #x00 layout)
                      (apply make-pcidev pcidev-args)
                      (let ((bridge-args (list (get-conf-u8 sel PCI-CFG-01-PRIMARY-BUS)
                                               (get-conf-u8 sel PCI-CFG-01-SECONDARY-BUS)
                                               (get-conf-u8 sel PCI-CFG-01-SUBORDINATE-BUS))))
                        (apply make-pcibridge (append pcidev-args bridge-args))))))
        (when (pcibridge? dev)
          (hashtable-set! bridges (pcibridge-secondary dev) dev))
        (set! ret (cons dev ret)))))
  (pci-scan-host mk-pci-device)
  (reverse ret)))
