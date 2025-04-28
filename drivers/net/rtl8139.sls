;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2007, 2020 G. Weinholt
#!r6rs

;;; Realtek RTL8139 network card driver

(library (loko drivers net rtl8139)
  (export
    probe·pci·rtl8139?
    driver·pci·rtl8139)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko drivers pci)
    (loko drivers net)
    (loko drivers utils)
    (loko system unsafe)
    (only (loko system $host) enable-irq wait-irq-operation
          dma-allocate dma-free))

(define (fxalign i alignment)
  (fxand (fx+ i (fx- alignment 1))
         (fx- alignment)))

(define (probe·pci·rtl8139? dev)
  (and (eqv? (pcidev-vendor-id dev) #x10EC)
       (eqv? (pcidev-device-id dev) #x8139)))

(define-i/o rtl8139-regs (endianness little)
  (u8 IDR0-ref) (u8 IDR1-ref) (u8 IDR2-ref) ; NIC hardware address
  (u8 IDR3-ref) (u8 IDR4-ref) (u8 IDR5-ref)
  (u16 res0-ref)
  (u32 MAR0..3-ref MAR0..3-set!)        ; Multicast address filter
  (u32 MAR4..7-ref MAR4..7-set!)
  (u32 TSD0-ref TSD0-set!)
  (u32 TSD1-ref TSD1-set!)
  (u32 TSD2-ref TSD2-set!)
  (u32 TSD3-ref TSD3-set!)
  (u32 TSAD0-ref TSAD0-set!)
  (u32 TSAD1-ref TSAD1-set!)
  (u32 TSAD2-ref TSAD2-set!)
  (u32 TSAD3-ref TSAD3-set!)
  (u32 RBSTART-ref RBSTART-set!)        ; Receive Buffer Start Address
  (u16 ERBCR-ref)
  (u8 ERSR-ref)
  (u8 CR-ref CR-set!)                   ; Command Register
  (u16 CAPR-ref CAPR-set!)              ; Current Adress of Packet Read
  (u16 CBR-ref)
  (u16 IMR-ref IMR-set!)                ; Interrupt Mask Register
  (u16 ISR-ref ISR-set!)                ; Interrupt Status Register
  (u32 TCR-ref)                         ; Transmit Config Register
  (u32 RCR-ref RCR-set!)                ; Receive Config Register
  (u32 TCTR-ref)
  (u32 MPC-ref MPC-set!)
  (u8 r9346CR-ref)
  (u8 CONFIG0-ref CONFIG0-set!)
  (u8 CONFIG1-ref CONFIG1-set!)
  (u8 res1-ref)
  (u32 TimerInt-ref)
  (u8 MSR-ref)                          ; Media status
  (u8 CONFIG3-ref)
  (u8 CONFIG4-ref)
  (u8 res2-ref)
  (u16 MULINT-ref)
  (u8 RERID-ref)
  (u8 res3-ref)
  (u16 TSAD-ref)                        ; Transmit Status of All Descriptors
  (u16 BMCR-ref BMCR-set!)              ; Basic Mode Control Register
  (u16 BMSR-ref)                        ; Basic Mode Status Register
  (u16 ANAR-ref)
  (u16 ANLPAR-ref)
  (u16 ANER-ref)
  ;; ...
  )

(define (TSDn-ref regs n)
  (get-i/o-u32 (fx+ (fx+ regs #x10) (fx* n 4))))

(define (TSADn-ref regs n)
  (get-i/o-u32 (fx+ (fx+ regs #x20) (fx* n 4))))

(define (TSDn-set! regs n v)
  (put-i/o-u32 (fx+ (fx+ regs #x10) (fx* n 4)) v))

(define (TSADn-set! regs n v)
  (put-i/o-u32 (fx+ (fx+ regs #x20) (fx* n 4)) v))

;; Status register in the Rx packet header
(define rx-MAR  #b100000000000000)
(define rx-PAM   #b10000000000000)
(define rx-BAR    #b1000000000000)
(define rx-ISE           #b100000)
(define rx-RUNT           #b10000)
(define rx-LONG            #b1000)
(define rx-CRC              #b100)
(define rx-RAE               #b10)
(define rx-ROK                #b1)
(define rx-errors        #b111110)

;; CR, Command register
(define cr-RST  #b10000)
(define cr-RE    #b1000)
(define cr-TE     #b100)
(define cr-BUFE     #b1)                ;buffer empty

;; Bits for the ISR and IMR registers (interrupt status/mask)
(define int-SERR    #b1000000000000000)
(define int-TimeOut  #b100000000000000)
(define int-LenChg    #b10000000000000)
(define int-FOVW             #b1000000)
(define int-PUN/LinkChg       #b100000)
(define int-RXOVW              #b10000)
(define int-TER                 #b1000)
(define int-TOK                  #b100)
(define int-RER                   #b10)
(define int-ROK                    #b1)
(define int-all     #b1110000001111111)

;; Interrupt status register
(define isr-TER #b1000)
(define isr-TOK  #b100)
(define isr-RER   #b10)
(define isr-ROK    #b1)

;; RCR, u32, Receive Configuration Register (there are more bits)
(define rcr-MXDMA-1024 #b110)
(define rcr-MXDMA-shift 8)
(define rcr-WRAP #b10000000)
(define rcr-AER    #b100000)
(define rcr-AR      #b10000)
(define rcr-AB       #b1000)
(define rcr-AM        #b100)
(define rcr-APM        #b10)
(define rcr-AAP         #b1)

;; TSDn, transmit status register
(define tsd-OWN (fxarithmetic-shift-left 1 13))

(define (TSDn-free? regs n)
  (not (eqv? 0 (fxand tsd-OWN (TSDn-ref regs n)))))

;; BMCR, Basic Mode Control Register
(define bmcr-Reset #b1000000000000000)
(define bmcr-ANE      #b1000000000000)

(define rxbuflen 8192)

(define (rtl8139-reset regs &rxbuf)
  ;; Assert reset and wait for it to respond
  (CR-set! regs cr-RST)
  (do ((try 0 (fx+ try 1)))
      ((eqv? 0 (fxand (CR-ref regs) cr-RST)))
    (when (eqv? try 10)
      (error 'rtl8139-reset "timeout when resetting the device"))
    (sleep 0.001))

  ;; Set receive buffer address.
  (RBSTART-set! regs &rxbuf)

  ;; Enable interrupts
  (IMR-set! regs (fxior int-TOK int-ROK))
  (ISR-ref regs)

  ;; Accept all multicast packets
  (MAR0..3-set! regs #xFFFFFFFF)
  (MAR4..7-set! regs #xFFFFFFFF)

  ;; Enable receiver and transmitter
  (CR-set! regs (fxior cr-RE cr-TE))

  ;; WRAP received packets (packets will be placed after the rx buffer
  ;; even if they don't fit); accept broadcast, multicast, our address
  ;; and set promiscuous mode (AAP). Rx buffer length is 8k + 16.
  (assert (eqv? rxbuflen 8192))
  (RCR-set! regs (fxior rcr-WRAP rcr-AB rcr-AM rcr-APM rcr-AAP
                        (fxarithmetic-shift-left rcr-MXDMA-1024 rcr-MXDMA-shift))))

(define (rtl8139-stop regs)
  (CR-set! regs 0))

(define (get-rx-header addr)
  (let ([status (fxior (get-mem-u8 addr)
                       (fxarithmetic-shift-left (get-mem-u8 (fx+ addr 1)) 8))]
        [size (fxior (get-mem-u8 (fx+ addr 2))
                     (fxarithmetic-shift-left (get-mem-u8 (fx+ addr 3)) 8))])
    (values status size)))

(define (rtl8139-netpkt-ok? pkt)
  (and (eqv? (length (netpkt-bufs pkt)) 1)
       (fx<=? 60 (netpkt-length pkt) #x700)
       (for-all (lambda (buf)
                  (let ((addr (car buf)) (len (cdr buf)))
                    ;; Completely within 32-bit address space and not
                    ;; too long.
                    (and (fx=? addr (fxbit-field addr 0 32))
                         (fx=? (fx+ addr len) (fxbit-field (fx+ addr len) 0 32)))))
                (netpkt-bufs pkt))))

;; Check if the netpkt follows our rules. If it doesn't then we need
;; to allocate a new netpkt and copy the data into it. The caller must
;; then use the return value and not the value given as an argument.
(define (rtl8139-netpkt-fixup! iface pkt)
  (let ((len (netpkt-length pkt)))
    (cond ((rtl8139-netpkt-ok? pkt)
           pkt)
          ((fx>? len #x700)
           #f)
          ((fx<? len 60)
           ;; The RTL8139 does not pad frames itself, so we need to do
           ;; it here. The minimum L2 frame (minus FCS) is 60 bytes.
           ;; TODO: Do this in a way that's better for performance!
           (let* ((pad (fx- 60 len))
                  (padpkt (netpkt-tx-allocate iface pad)))
             (do ((i 0 (fx+ i 1)))
                 ((fx=? i pad))
               (netpkt-u8-set! padpkt i 0))
             (netpkt-transplant! pkt padpkt)
             (rtl8139-netpkt-fixup! iface pkt)))
          (else
           (let ((new-pkt (netpkt-copy pkt #xFFFFFFF0)))
             (netpkt-free! pkt)
             new-pkt)))))

;; Dequeue a packet from the receive buffer
(define (rtl8139-rx-dequeue! regs &rxbuf offset rx-pkt)
  (cond
    (rx-pkt (values offset rx-pkt))
    ((eqv? 0 (fxand (CR-ref regs) cr-BUFE))
     (let-values ([(status size) (get-rx-header (fx+ &rxbuf offset))])
       (let ((pkt (cond
                    ((not (eqv? 0 (fxand status rx-errors)))
                     ;; Errors during reception
                     #f)
                    (else
                     (let* ((hdrlen 4)
                            (&buf (dma-allocate size (fxnot 2)))
                            (size (fx- size 4))) ;skip FCS
                       ;; Construct a netpkt and immediately copy
                       ;; it to a new buffer. This is done mostly
                       ;; because the card doesn't align frames to
                       ;; 16-bit boundaries.
                       (do ((&pkt (fx+ &rxbuf (fx+ offset hdrlen)))
                            (i 0 (fx+ i 1)))
                           ((fx=? i size))
                         (put-mem-u8 (fx+ &buf i) (get-mem-u8 (fx+ &pkt i))))
                       (make-netpkt #f (list &buf)
                                    (list (cons &buf size)))))))
             (offset (fxmod (fxalign (fx+ (fx+ offset size) 4) 4)
                            rxbuflen)))
         (CAPR-set! regs (fxand #xffff (fx- offset 16)))
         (values offset pkt))))
    (else (values offset rx-pkt))))

(define (rtl8139-main irq regs &rxbuf iface)
  (define tx-pkts (make-vector 4 #f))
  (define irq-token (enable-irq irq))
  (send-log DEBUG "RTL8139 driver started")
  (spawn-fiber
   (lambda ()
     (let lp ()
       (match (get-message (iface-ctrl-ch iface))
         [('hwaddr? . (? channel? resp-ch))
          (let ((mac (u8-list->bytevector
                      (list (IDR0-ref regs) (IDR1-ref regs) (IDR2-ref regs)
                            (IDR3-ref regs) (IDR4-ref regs) (IDR5-ref regs)))))
            (put-message resp-ch mac))])
       (lp))))
  (let lp ((offset 0) (rx-pkt #f) (next-txd 0))
    (match (perform-operation
            (choice-operation
             (wrap-operation (wait-irq-operation irq-token) (lambda _ 'int))
             (if (not rx-pkt)
                 (choice-operation)
                 (wrap-operation (put-operation (iface-rx-ch iface) rx-pkt)
                                 (lambda _ 'rx-ack)))
             (if (TSDn-free? regs next-txd)
                 (wrap-operation (get-operation (iface-tx-ch iface))
                                 (lambda (x) (cons 'tx x)))
                 (choice-operation))))
      ['int
       (let ((isr (ISR-ref regs)))
         (let-values ([(offset rx-pkt) (rtl8139-rx-dequeue! regs &rxbuf offset rx-pkt)])
           (unless (eqv? (fxand isr int-all) 0)
             ;; Acknowledge all interrupt reasons
             (ISR-set! regs int-all))
           (lp offset rx-pkt next-txd)))]

      ['rx-ack
       ;; Someone received rx-pkt
       (let-values ([(offset rx-pkt) (rtl8139-rx-dequeue! regs &rxbuf offset #f)])
         (lp offset rx-pkt next-txd))]

      [('tx . pkt)
       (let ((pkt (rtl8139-netpkt-fixup! iface pkt)))
         (cond ((vector-ref tx-pkts next-txd)
                => netpkt-free!))
         (cond ((not pkt)
                (netpkt-free! pkt)      ;drop the pkt
                (lp offset rx-pkt next-txd))
               (else
                (vector-set! tx-pkts next-txd pkt)
                (let ((buf (car (netpkt-bufs pkt))))
                  ;; Tell the nic to transmit the frame
                  (TSADn-set! regs next-txd (car buf))
                  (TSDn-set! regs next-txd (cdr buf)))
                (lp offset rx-pkt (fxand (fx+ next-txd 1) #b11)))))])))

(define (driver·pci·rtl8139 dev iface)
  (define (stop regs &rxbuf)
    (rtl8139-stop regs)
    (pci-put-u8 dev
                PCI-CFG-COMMAND
                (fxand (pci-get-u8 dev PCI-CFG-COMMAND)
                       (fxnot PCI-CMD-BUS-MASTER)))
    (dma-free &rxbuf))
  ;; Enable I/O, bus mastering and unmask interrupts. Disable the
  ;; memory mapped registers.
  (pci-put-u8 dev
              PCI-CFG-COMMAND
              (fxand (fxior (fxior PCI-CMD-I/O-SPACE
                                   PCI-CMD-BUS-MASTER)
                            (pci-get-u8 dev PCI-CFG-COMMAND))
                     (fxnot (fxior PCI-CMD-INTERRUPT-DISABLE
                                   PCI-CMD-MEM-SPACE))))
  ;; BAR0 is in I/O space and BAR1 is in memory space.
  (let ([bar0 (vector-ref (pcidev-BARs dev) 0)]
        [irq (pcidev-irq dev)]
        ;; The manual says 8k + 16 + 1.5K. But that wrongly assumes a
        ;; 1500 byte packet limit.
        [&rxbuf (dma-allocate (+ 8192 16 4096) #xfffffff0)])
    (guard (exn
            (else
             (stop (pcibar-base bar0) &rxbuf)
             (raise exn)))
      (rtl8139-reset (pcibar-base bar0) &rxbuf)
      (rtl8139-main irq (pcibar-base bar0) &rxbuf iface)
      (stop (pcibar-base bar0) &rxbuf)))))
