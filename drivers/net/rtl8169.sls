;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020, 2021 G. Weinholt
#!r6rs

;;; Realtek RTL8169 network card driver

;; This driver supports Realtek 10/1000/1000 Mbit cards. This means
;; the RTL8169 and a number of other devices with slightly different
;; model numbers. If you think your card should work and it isn't
;; detected, then you might just need to add the device id to the
;; probe procedure.

;; The NIC has a few offloading features. It can do TCP segmentation
;; itself and handles IP, UDP and TCP checksums. There's apparently no
;; IPv6 support (according to the datasheet I have). It also has some
;; VLAN tag support that looks a bit pointless.

(library (loko drivers net rtl8169)
  (export
    probe·pci·rtl8169?
    driver·pci·rtl8169)
  (import
    (rnrs)
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko system unsafe)
    (loko drivers net)
    (loko drivers pci)
    (loko drivers utils)
    (loko system unsafe cache)
    (only (loko system $host) enable-irq wait-irq-operation
          dma-allocate dma-free))

(define (probe·pci·rtl8169? dev)
  (and (eqv? (pcidev-vendor-id dev) #x10EC)
       (eqv? (pcidev-device-id dev) #x8168)))

;; The number of rx & tx descriptors to allocate. For rx, there are
;; also buffers allocated. Both can take up to 1024 descriptors.
(define num-rx-desc 1024)
(define num-tx-desc 1024)

;; The largest receive MTU we want to support. The buffers are
;; allocated with dma-allocate, which only gives out 2^n bytes. Can be
;; up to rx-len-max, but ~1500 is the most that will show up unless
;; you're using jumbo frames.
(define rx-mtu 2048)

;; All descriptors are four u32s long. The second half is the buffer
;; pointer (low u32 followed by high u32). The buffer length is in the
;; first 13 bits of the first word. There is no scatter-gather.

;; Clear a descriptor
(define (desc-clear! &rx num-desc i)
  (let ((&desc (fx+ &rx (fx* i 16))))
    (put-mem-u32 &desc (if (eqv? i num-desc) desc-0-EOR 0))
    (put-mem-u32 (fx+ &desc 4) 0)
    (put-mem-s61 (fx+ &desc 8) 0)))

;; Set the address of a descriptor
(define (desc-address-set! &base i &addr)
  ;; TODO: Needs to be endianness-swapped on big endian machines
  (assert (eq? (native-endianness) (endianness little)))
  (put-mem-s61 (fx+ &base (fx+ (fx* i 16) 8)) &addr))

;; Common to all descriptors
(define desc-0-OWN (fxarithmetic-shift-left 1 31)) ;1=owned by NIC
(define desc-0-EOR (fxarithmetic-shift-left 1 30)) ;end of ring
(define desc-0-FS (fxarithmetic-shift-left 1 29)) ;1=first segment
(define desc-0-LS (fxarithmetic-shift-left 1 28)) ;1=last segment

(define (desc-owned-by-driver? &base i)
  (eqv? 0 (fxand desc-0-OWN (get-mem-u32 (fx+ &base (fx* i 16))))))

;; Large-send task offload tx command descriptor
(define tx-large-0-LGSEN (fxarithmetic-shift-left 1 27))
(define tx-large-0-MSS-mask (fx- (fxarithmetic-shift-left 1 11) 1))
(define tx-large-0-MSS-shift 16)
(define tx-large-0-Frame-Length (fx- (fxarithmetic-shift-left 1 16) 1))
(define tx-large-4-TAGC (fxarithmetic-shift-left 1 17))
(define tx-large-4-VLAN_TAG-mask (fx- (fxarithmetic-shift-left 1 16) 1))

;; Normal tx command descriptor
(define tx-0-Frame-Length (fx- (fxarithmetic-shift-left 1 16) 1))
(define tx-0-IPCS (fxarithmetic-shift-left 1 18))
(define tx-0-UDPCS (fxarithmetic-shift-left 1 17))
(define tx-0-TCPCS (fxarithmetic-shift-left 1 16))

;; Receive command or status descriptor
(define rx-0-MAR  (fxarithmetic-shift-left 1 27)) ;multicast
(define rx-0-PAM  (fxarithmetic-shift-left 1 26)) ;physical match
(define rx-0-BAR  (fxarithmetic-shift-left 1 25)) ;broadcast
(define rx-0-RWT  (fxarithmetic-shift-left 1 22)) ;length > 8192
(define rx-0-RES  (fxarithmetic-shift-left 1 21)) ;receive error
(define rx-0-RUNT (fxarithmetic-shift-left 1 20)) ;short packet
(define rx-0-CRC  (fxarithmetic-shift-left 1 19)) ;crc error
(define rx-0-PID1 (fxarithmetic-shift-left 1 18)) ;protocol id
(define rx-0-PID0 (fxarithmetic-shift-left 1 17)) ;protocol id
(define rx-0-IPF  (fxarithmetic-shift-left 1 16)) ;ip checksum failed
(define rx-0-UDPF (fxarithmetic-shift-left 1 15)) ;udp checksum failed
(define rx-0-TCPF (fxarithmetic-shift-left 1 14)) ;tcp checksum failed
(define rx-0-Frame_Length-mask (fx- (fxarithmetic-shift-left 1 14) 1))
(define rx-len-max #x1FF8)
(define rx-4-TAVA (fxarithmetic-shift-left 1 16)) ;vlan tag available
(define rx-4-VLAN_TAG-mask (fx- (fxarithmetic-shift-left 1 16) 1))

;; Fill in an RX command descriptor
(define (rx-desc-command! &rx i &buf len)
  (assert (fx<=? 8 len rx-len-max))
  (let ((&desc (fx+ &rx (fx* i 16))))
    (desc-address-set! &rx i &buf)
    (store-fence)
    (put-mem-u32 (fx+ &desc 4) 0)
    (put-mem-u32 &desc (fxior (if (eqv? i (fx- num-rx-desc 1)) desc-0-EOR 0)
                              len
                              desc-0-OWN))))

;; Get the length of the received data, or #f if nothing has been received
(define (rx-desc-frame-length &rx i)
  (let ((v (get-mem-u32 (fx+ &rx (fx* i 16)))))
    (and (eqv? desc-0-LS (fxand v (fxior desc-0-OWN desc-0-LS)))
         ;; Remove 8 for the trailing CRC.
         (fxmax 0 (fx- (fxand v rx-0-Frame_Length-mask) 4)))))

;; Fill in an TX command descriptor
(define (tx-desc-command! &tx i &buf len)
  (assert (fx<=? 1 len #xffff))
  (let ((&desc (fx+ &tx (fx* i 16))))
    (desc-address-set! &tx i &buf)
    (store-fence)
    (put-mem-u32 (fx+ &desc 4) 0)
    (put-mem-u32 &desc (fxior (if (eqv? i (fx- num-tx-desc 1)) desc-0-EOR 0)
                              len
                              (fxior desc-0-OWN desc-0-FS desc-0-LS)))
    (store-fence)))

(define (clear-descriptors! &rx &tx)
  (do ((i 0 (fx+ i 1)))
      ((fx=? i num-rx-desc))
    (desc-clear! &rx num-rx-desc i))
  (do ((i 0 (fx+ i 1)))
      ((fx=? i num-tx-desc))
    (desc-clear! &tx num-tx-desc i)))

(define-i/o rtl8169-regs (endianness little)
  (u8 IDR0-ref) (u8 IDR1-ref) (u8 IDR2-ref) ; NIC hardware address
  (u8 IDR3-ref) (u8 IDR4-ref) (u8 IDR5-ref)
  (u16)
  (u32 MAR0..3-ref MAR0..3-set!)        ; Multicast address filter
  (u32 MAR4..7-ref MAR4..7-set!)
  (u64 DTCCR-ref DTCCR-set!)
  (u64)
  (u64 TNPDS-ref TNPDS-set!)            ;transmit normal prio descriptors
  (u64 THPDS-ref THPDS-set!)            ;transmit high prio descriptors
  (u32)
  (u16)
  (u8)
  (u8 CR-ref CR-set!)
  (u8 TPPoll-ref TPPoll-set!)
  (u8)
  (u16)
  (u16 IMR-ref IMR-set!)
  (u16 ISR-ref ISR-set!)
  (u32 TCR-ref TCR-set!)
  (u32 RCR-ref RCR-set!)
  (u32 TCTR-ref)
  (u32 MPC-ref)
  (u8 r9346CR-ref r9346CR-set!)
  (u8 CONFIG0-ref CONFIG0-set!)
  (u8 CONFIG1-ref CONFIG1-set!)
  (u8 CONFIG2-ref CONFIG2-set!)
  (u8 CONFIG3-ref CONFIG3-set!)
  (u8 CONFIG4-ref CONFIG4-set!)
  (u8 CONFIG5-ref CONFIG5-set!)
  (u8)
  (u32 TimerInt-ref TimerInt-set!)
  (u32)
  (u32 PHYAR-ref PHYAR-set!)
  (u32 TBICSR0 ref)
  (u16 TBI_ANAR-ref)
  (u16 TBI_LPAR-ref)
  (u8 PHYStatus-ref)
  (u8)
  (u16)
  (u32)
  (u64)
  (u64)
  (u64 Wakeup0-ref)
  (u64 Wakeup1-ref)
  (u128 Wakeup2-ref)
  (u128 Wakeup3-ref)
  (u128 Wakeup4-ref)
  (u16 CRC0-ref)
  (u16 CRC1-ref)
  (u16 CRC2-ref)
  (u16 CRC3-ref)
  (u16 CRC4-ref)
  (u16)
  (u64)
  (u16)
  (u16 RMS-ref)
  (u32)
  (u16 C+CR-ref C+CR-set!)
  (u16)
  (u64 RDSAR-ref RDSAR-set!)
  (u8 MTPS-ref MTPS-set!))

;; CR, Command register
(define cr-RST  #b10000)
(define cr-RE    #b1000)
(define cr-TE     #b100)

;; Bits for the ISR and IMR registers (interrupt status/mask)
(define int-SERR    #b1000000000000000)
(define int-TimeOut  #b100000000000000)
(define int-SWInt          #b100000000)
(define int-TDU             #b10000000)
(define int-FOVW             #b1000000)
(define int-LinkChg           #b100000)
(define int-RDU                #b10000)
(define int-TER                 #b1000)
(define int-TOK                  #b100)
(define int-RER                   #b10)
(define int-ROK                    #b1)
(define int-all     #b1100000111111111)

;; RCR, Receive Configuration Register (there are more bits)
(define rcr-MXDMA-1024  #b110)
(define rcr-MXDMA-nolim #b111)
(define rcr-MXDMA-shift 8)
(define rcr-RXFTH-nolim #b111)
(define rcr-RXFTH-shift 13)
(define rcr-AER    #b100000)
(define rcr-AR      #b10000)
(define rcr-AB       #b1000)
(define rcr-AM        #b100)
(define rcr-APM        #b10)
(define rcr-AAP         #b1)

;; TCR, Transmit Configuration Register
;; RCR, Receive Configuration Register (there are more bits)
(define tcr-MXDMA-nolim #b111)
(define tcr-MXDMA-shift 8)
(define tcr-IFG0 (fxarithmetic-shift-left 1 24))
(define tcr-IFG1 (fxarithmetic-shift-left 1 25))
(define tcr-IFG2 (fxarithmetic-shift-left 1 19))
(define tcr-IFG-default (fxior tcr-IFG0 tcr-IFG1)) ;9.6µs/960ns/96ns

;; TPPoll, transmit priority polling
(define tppoll-HPQ    #b10000000)
(define tppoll-NPQ     #b1000000)
(define tppoll-FSWInt        #b1)

;; 93C46/93C56 command register
(define eeprom-EEM-normal       #b00000000)
(define eeprom-EEM-auto-load    #b01000000)
(define eeprom-EEM-programming  #b10000000)
(define eeprom-EEM-config-write #b11000000)
(define eeprom-EECS             #b00001000)
(define eeprom-EESK             #b00000100)
(define eeprom-EEDI             #b00000010)
(define eeprom-EEDO             #b00000001)

;; CONFIG 1
(define config1-DRVLOAD #b00100000)

(define (rtl8169-reset regs &rx &tx)
  ;; Assert reset and wait for it to respond
  (CR-set! regs cr-RST)
  (do ((try 0 (fx+ try 1)))
      ((eqv? 0 (fxand (CR-ref regs) cr-RST)))
    (when (eqv? try 10)
      (error 'rtl8169-reset "timeout when resetting the device"))
    (sleep 0.001))

  ;; Enable access to CONFIGx
  (r9346CR-set! regs eeprom-EEM-config-write)

  ;; Set receive & transmit descriptor start addresses. The high
  ;; priority tx descriptors are not used.
  (RDSAR-set! regs &rx)
  (TNPDS-set! regs &tx)

  ;; Enable interrupts
  (IMR-set! regs (fxior int-TOK int-ROK))
  (ISR-ref regs)
  (ISR-set! regs int-all)

  ;; Accept all multicast packets
  (MAR0..3-set! regs #xFFFFFFFF)
  (MAR4..7-set! regs #xFFFFFFFF)

  ;; Enable receiver and transmitter
  (CR-set! regs (fxior cr-RE cr-TE))

  ;; Accept broadcast, multicast, our address and all other addresses.
  ;; Unlimited DMA burst size.
  (RCR-set! regs (fxior rcr-AB rcr-AM rcr-APM rcr-AAP
                        (fxarithmetic-shift-left rcr-MXDMA-nolim rcr-MXDMA-shift)
                        (fxarithmetic-shift-left rcr-RXFTH-nolim rcr-RXFTH-shift)))

  ;; Default inter-frame gap and unlimited DMA burst size
  (TCR-set! regs (fxior tcr-IFG-default
                        (fxarithmetic-shift-left tcr-MXDMA-nolim tcr-MXDMA-shift)))

  ;; Indicate that a driver is loaded
  (CONFIG1-set! regs (fxior (CONFIG1-ref regs) config1-DRVLOAD))

  ;; Disable access to CONFIGx
  (r9346CR-set! regs 0))

(define (rtl8169-stop regs)
  (CR-set! regs 0))

(define (rtl8169-netpkt-ok? pkt)
  (and (eqv? (length (netpkt-bufs pkt)) 1)
       (fx<=? 1 (netpkt-length pkt) #xffff)))

;; Check if the netpkt follows our rules. If it doesn't then we need
;; to allocate a new netpkt and copy the data into it. The caller must
;; then use the return value and not the value given as an argument.
(define (rtl8169-netpkt-fixup! iface pkt)
  (let ((len (netpkt-length pkt)))
    (cond ((rtl8169-netpkt-ok? pkt)
           pkt)
          ((fx>? len #xffff)
           #f)
          (else
           (let ((new-pkt (netpkt-copy pkt (fxnot 7))))
             (netpkt-free! pkt)
             new-pkt)))))

(define (rtl8169-main irq regs &rx &tx iface)
  (define rx-bufs (make-vector num-rx-desc #f))
  (define tx-pkts (make-vector num-tx-desc #f))
  (define rxd-i 0)
  (define next-txd 0)
  (define oldest-txd 0)
  (define used-txds 0)
  (define rx-pkt #f)
  (define irq-token (enable-irq irq))

  (define (allocate-rx-buf! i)
    (let ((&buf (dma-allocate rx-mtu (fxnot 7))))
      (vector-set! rx-bufs i &buf)
      (rx-desc-command! &rx i &buf rx-mtu)))

  (define (handle-rx!)
    (when (and (not rx-pkt)
               (desc-owned-by-driver? &rx rxd-i))
      (let ((buf (vector-ref rx-bufs rxd-i))
            (len (fxmin (rx-desc-frame-length &rx rxd-i) rx-mtu)))
        (allocate-rx-buf! rxd-i)
        (when len
          (set! rx-pkt (make-netpkt #f (list buf) (list (cons buf len)))))
        (set! rxd-i (fxmod (fx+ rxd-i 1) num-rx-desc)))))

  (define (handle-tx!)
    (when (and (not (eqv? 0 used-txds))
               (desc-owned-by-driver? &tx oldest-txd))
      (netpkt-free! (vector-ref tx-pkts oldest-txd))
      (vector-set! tx-pkts oldest-txd #f)
      (desc-clear! &tx num-tx-desc oldest-txd)
      (set! oldest-txd (fxmod (fx+ oldest-txd 1) num-tx-desc))
      (set! used-txds (fx- used-txds 1))
      (handle-tx!)))

  (send-log DEBUG "RTL8169 driver started")

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

  ;; Pre-allocate receive buffers
  (do ((i 0 (fx+ i 1)))
      ((fx=? i num-rx-desc))
    (allocate-rx-buf! i))
  (rtl8169-reset regs &rx &tx)

  (let lp ()
    (handle-rx!)
    (handle-tx!)

    (match (perform-operation
            (choice-operation
             (wrap-operation (wait-irq-operation irq-token) (lambda _ 'int))
             (if rx-pkt
                 (wrap-operation (put-operation (iface-rx-ch iface) rx-pkt)
                                 (lambda _ 'rx-ack))
                 (choice-operation))
             (if (fx<? used-txds num-tx-desc)
                 (wrap-operation (get-operation (iface-tx-ch iface))
                                 (lambda (x) (cons 'tx x)))
                 (choice-operation))))

      ['int
       (let ((isr (ISR-ref regs)))
         ;; Acknowledge all interrupt reasons
         (unless (eqv? 0 (fxand isr int-all))
           (ISR-set! regs int-all))
         (lp))]

      ['rx-ack
       (set! rx-pkt #f)                 ;rx-pkt is no longer ours
       (lp)]

      [('tx . pkt)
       (cond
         ((rtl8169-netpkt-fixup! iface pkt) =>
          (lambda (pkt)
            (vector-set! tx-pkts next-txd pkt)
            (let ((bufs (netpkt-bufs pkt)))
              (tx-desc-command! &tx next-txd (caar bufs) (cdar bufs)))

            (TPPoll-set! regs tppoll-NPQ)  ;tell the NIC to poll

            (set! used-txds (fx+ used-txds 1))
            (set! next-txd (fxmod (fx+ next-txd 1) num-tx-desc))))
         (else
          (netpkt-free! pkt)))

       (lp)])))

(define (driver·pci·rtl8169 dev iface)
  (define (stop regs &rx &tx)
    (send-log DEBUG "RTL8169 driver stopped")
    (rtl8169-stop regs)
    (pci-put-u8 dev PCI-CFG-COMMAND
                (fxand (pci-get-u8 dev PCI-CFG-COMMAND)
                       (fxnot PCI-CMD-BUS-MASTER)))
    (clear-descriptors! &rx &tx)
    ;; FIXME: free the packets that we've claimed responsibility for
    (dma-free &rx)
    (dma-free &tx))
  ;; Enable I/O (BAR0), bus mastering and unmask interrupts. Disable
  ;; the memory mapped registers (BAR2).
  (pci-put-u8 dev
              PCI-CFG-COMMAND
              (fxand (fxior (fxior PCI-CMD-I/O-SPACE
                                   PCI-CMD-BUS-MASTER)
                            (pci-get-u8 dev PCI-CFG-COMMAND))
                     (fxnot (fxior PCI-CMD-INTERRUPT-DISABLE
                                   PCI-CMD-MEM-SPACE))))
  (let ([bar0 (vector-ref (pcidev-BARs dev) 0)]
        [irq (pcidev-irq dev)])
    (assert (pcibar-i/o? bar0))
    (let ([&rx (dma-allocate (* num-rx-desc 16) (fxnot 255))]
          [&tx (dma-allocate (* num-tx-desc 16) (fxnot 255))])
      (clear-descriptors! &rx &tx)
      (guard (exn
              (else
               (stop (pcibar-base bar0) &rx &tx)
               (raise exn)))
        (rtl8169-main irq (pcibar-base bar0) &rx &tx iface)
        (stop (pcibar-base bar0) &rx &tx))))))
