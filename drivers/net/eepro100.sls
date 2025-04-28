;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2007, 2020, 2021 G. Weinholt
#!r6rs

;;; Intel 8255x 10/100 Mbps Ethernet Controller driver

;; This network card has a control unit (CU) and a receive unit (RU).
;; They each work with their own descriptor formats. Frames are
;; transmitted with the CU and received with the CU. Configuration is
;; also done with the CU.

(library (loko drivers net eepro100)
  (export
    probe·pci·eepro100?
    driver·pci·eepro100)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (loko match)
    (loko system fibers)
    (loko drivers pci)
    (loko drivers net)
    (loko drivers utils)
    (loko system unsafe)
    (loko system unsafe cache)
    (only (loko system $host) enable-irq wait-irq-operation
          dma-allocate dma-free))

(define (µsleep µs) (sleep (/ µs 1e6)))

(define (msleep ms) (sleep (/ ms 1e3)))

(define fxash fxarithmetic-shift)

(define (fxtest n1 n2)
  (not (eqv? 0 (fxand n1 n2))))

(define (fxalign i alignment)
  (fxand (fx+ i (fx- alignment 1)) (fx- alignment)))

;;; Control registers

(define-i/o i8255x-regs (endianness little)
  (u8 SCB-status-ref)                   ;System Control Block
  (u8 SCB-STAT/ACK-ref SCB-STAT/ACK-set!)
  (u8 SCB-command-ref  SCB-command-set!)
  (u8 SCB-irq-mask-ref SCB-irq-mask-set!)
  (u32 SCB-pointer-ref SCB-pointer-set!)
  (u32 PORT-ref        PORT-set!)
  (u16)
  (u16 EEPROM-ref      EEPROM-set!))

(define SCB-STAT-FR #b100000)           ;frame received
(define SCB-STAT-CNA #b10000)           ;CU left the active state
(define SCB-STAT-RNR  #b1000)           ;RU left the ready state

;; Port selection functions
(define PORT-software-reset   #b0000)
(define PORT-self-test        #b0001)
(define PORT-selective-reset  #b0010)
(define PORT-dump             #b0011)
(define PORT-dump-wake-up     #b0111)

;; Command Unit commands
(define CU-nop                         #b00000000)
(define CU-start                       #b00010000)
(define CU-resume                      #b00100000)
(define CU-load-dump-counters-address  #b01000000)
(define CU-dump-counters               #b01010000)
(define CU-load-CU-base                #b01100000)
(define CU-dump-and-reset-counters     #b01110000)
(define CU-static-resume               #b10100000)

;; Receive Unit commands
(define RU-nop                   #b0000)
(define RU-start                 #b0001)
(define RU-resume                #b0010)
(define RU-receive-DMA-redirect  #b0011)
(define RU-abort                 #b0100)
(define RU-load-header-data-size #b0101)
(define RU-load-RU-base          #b0110)

;;; Descriptors in 32-bit system memory, shared with the device

(define-mem RX-descriptor (endianness little)
  (u16 RX-status-ref RX-status-set!)
  (u16 RX-command-ref RX-command-set!)
  (u32 RX-link-address-ref RX-link-address-set!)
  (u32)
  (u16 RX-actual-count-ref)
  (u16 RX-size-ref RX-size-set!))

(define RX-cmd-end-link #b1000000000000000)
(define RX-cmd-suspend   #b100000000000000)
;; ...

(define RX-status-complete #b1000000000000000)
(define RX-status-ok         #b10000000000000)
;; ...

;; Allocates descriptors for frames. The RU will update the descriptor
;; and write the frame to the area immediately after the descriptor.
;; This is called the "simplified memory model" and it's unclear where
;; they hid the better model. They are returned in the order that they
;; will be used by the RU.
(define (make-receive-frame-area)
  ;; This is a bit arbitrary and doesn't go into jumbo frame
  ;; territory, but at least there are no dropped VLAN tags.
  (define MTU 1900)
  (let lp ((i 64))
    ;; TODO: dma-allocate returns 4k in practice; should really have
    ;; another allocator on top of it.
    (let* ((len (fx+ (RX-descriptor 'get-size) MTU))
           (&buf (dma-allocate len #xFFFFFFF0)))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i (RX-descriptor 'get-size)))
        (put-mem-u8 (fx+ &buf i) 0))
      (RX-size-set! &buf MTU)
      (cond ((eqv? i 0)
             (RX-command-set! &buf RX-cmd-end-link)
             (RX-link-address-set! &buf &buf)
             (list &buf))
            (else
             (let ((&buf* (lp (fx- i 1))))
               (RX-link-address-set! &buf (car &buf*))
               (cons &buf &buf*)))))))

(define-mem command-block (endianness little)
  (u16 CB-status-ref CB-status-set!)
  (u16 CB-command-ref CB-command-set!)
  (u32 CB-link-offset-ref CB-link-offset-set!)
  ;; optional address and data fields follow, the ones for
  ;; transmitting are given here:
  (u32 CB-TBD-address-ref CB-TBD-address-set!)
  (u32 CB-TBD-count-ref CB-TBD-count-set!))

(define-mem transfer-buffer (endianness little)
  (u32 TB-address-ref TB-address-set!)
  (u32 TB-size-ref TB-size-set!))

(define CB-cmd-end-link #b1000000000000000)
(define CB-cmd-suspend   #b100000000000000)
(define CB-cmd-interrupt  #b10000000000000)

(define CB-cmd-NOP                   #b000)
(define CB-cmd-mac-address-setup     #b001)
(define CB-cmd-configure             #b010)
(define CB-cmd-multicast-setup       #b011)
(define CB-cmd-transmit              #b100)
(define CB-cmd-load-microcode        #b101)
(define CB-cmd-dump                  #b110)
(define CB-cmd-diagnose              #b111)

(define CB-cmd-transmit-NC         #b10000) ;0=insert CRC and source address
(define CB-cmd-transmit-flexible    #b1000) ;1=flexible mode

(define CB-status-complete #b1000000000000000)
(define CB-status-ok         #b10000000000000)

(define CB-TBD-cmd-EOF     #b1000000000000000)

;; Netpkts going to tx can have this many bufs
(define max-tx-bufs 8)

;; Create a circular list of command blocks (transfer descriptors)
(define (make-transfer-descriptors)
  (define num-bufs 4)
  (define hdrlen (command-block 'get-size))
  (define len (fxalign (fx+ hdrlen (fx* 8 max-tx-bufs)) 16))
  (define &buf (dma-allocate (fx* len num-bufs) #xFFFFFFF0))
  (define (bufaddr i) (fx+ &buf (fx* len i)))
  (let lp ((i 0) (&tx* '()))
    (let ((&tx (bufaddr i)))
      (CB-status-set! &tx (if (eqv? i 0) 0 CB-status-complete))
      (CB-TBD-address-set! &tx (fx+ &tx hdrlen))
      (CB-command-set! &tx (fxior CB-cmd-suspend CB-cmd-NOP))
      (cond ((eqv? i (fx- num-bufs 1))
             ;; Link the last descriptor to the first descriptor and
             ;; do the same for the list
             (CB-link-offset-set! &tx &buf)
             (let ((&tx* (reverse (cons &tx &tx*))))
               (let lp ((x* &tx*))
                 (if (null? (cdr x*))
                     (set-cdr! x* &tx*)
                     (lp (cdr x*))))
               ;; &buf is the pointer to free
               (values &buf &tx*)))
            (else
             (CB-link-offset-set! &tx (bufaddr (fx+ i 1)))
             (lp (fx+ i 1) (cons &tx &tx*)))))))

(define (e100-tx-fill-netpkt &tx pkt)
  (define hdrlen (command-block 'get-size))
  (CB-status-set! &tx 0)
  (CB-command-set! &tx (fxior CB-cmd-suspend CB-cmd-transmit CB-cmd-transmit-flexible))
  (CB-TBD-address-set! &tx (fx+ &tx hdrlen))
  (do ((bufs (netpkt-bufs pkt) (cdr bufs))
       (i 0 (fx+ i 1)))
      ((null? bufs)
       (CB-TBD-count-set! &tx (fxior (fxash 16 16)   ;threshold
                                     (fxash i 24)    ;TBD count
                                     (fxash 1 16)))) ;EOF
    (let ((buf (car bufs))
          (&tb (fx+ &tx (fx+ hdrlen (fx* i (transfer-buffer 'get-size))))))
      ;; Buffer address & length
      (TB-address-set! &tb (car buf))
      (TB-size-set! &tb (if (null? (cdr bufs))
                            (fxior (cdr buf) #x10000) ;EL
                            (cdr buf))))))

;;; Code for reading the EEPROM (where the MAC address is stored)

;; Bit-banged SPI
(define EESK       #b1)                 ;Serial Clock
(define EECS      #b10)                 ;Chip Select
(define EEDI     #b100)                 ;Serial Data In
(define EEDO    #b1000)                 ;Serial Data Out
(define EE-read #b0110)                 ;'read' opcode with start bits

(define (eeprom-send-bits! regs bits length detect-size)
  ;; Send bits in MSB order
  (do ((ret #f)
       (i 0 (fx+ i 1)))
      ((or ret (fx=? i length))
       ret)
    (let ((data (fxand EEDI (fxash bits (fx+ (fx- 3 length) i)))))
      (EEPROM-set! regs (fxior EECS data))
      (µsleep 4)
      (EEPROM-set! regs (fxior EECS data EESK))
      (µsleep 4)
      ;; Look for the "dummy0" bit, which indicates the end of the
      ;; address field.
      (when (and detect-size (eqv? 0 (fxand EEDO (EEPROM-ref regs))))
        (set! ret (fx+ i 1))))))

(define (eeprom-read-bits! regs length)
  ;; Read bits in MSB order
  (do ((sum 0)
       (i 0 (fx+ i 1)))
      ((fx>? i length) sum)
    (EEPROM-set! regs (fxior EECS EESK))
    (µsleep 4)
    (set! sum (fxior (fxash (fxand (EEPROM-ref regs) EEDO) -3)
                     (fxash sum 1)))
    (EEPROM-set! regs (fxior EECS))
    (µsleep 4)))

(define (eeprom-read! regs address address-length)
  (EEPROM-set! regs EECS)               ;activate the EEPROM
  (µsleep 4)
  (eeprom-send-bits! regs EE-read 4 #f)
  (eeprom-send-bits! regs address address-length #f)
  (let ((result (eeprom-read-bits! regs 16)))
    (EEPROM-set! regs 0)
    result))

(define (eeprom-detect-size! regs)
  (EEPROM-set! regs EECS)
  (µsleep 4)
  (eeprom-send-bits! regs EE-read 4 #f)
  (let ((size (eeprom-send-bits! regs 0 8 #t)))
    (eeprom-read-bits! regs 16)
    (EEPROM-set! regs 0)
    size))

(define (eeprom-dump regs)
  (let* ((eeprom-size (eeprom-detect-size! regs))
         (eeprom (make-bytevector (fxash 1 eeprom-size) 0))
         (maxaddr (fxash 1 (fx- eeprom-size 1))))
    (do ((i 0 (fx+ i 1)))
        ;; FIXME: try to read all words on real hardware
        ((or (fx>? i 6) (fx=? i maxaddr))
         eeprom)
      (bytevector-u16-set! eeprom (fx* i 2)
                           (eeprom-read! regs i eeprom-size)
                           (endianness little)))))

;;; Configuration

;; Create a configuration. See the 8255x OpenSDM for a complete map of
;; these configuration bytes.
(define (make-e100-configuration promisc?)
  (define cfglen 22)
  (let ((bv (make-bytevector cfglen 0)))
    (define (set i v) (bytevector-u8-set! bv i v))
    ;; TODO: Clean up and go through with a comb to support all the
    ;; various models.
    (set 0 cfglen)
    ;; See the 8255x open source software developer manual for a
    ;; complete map of these configuration bytes.
    (set 1 8)                        ;receive FIFO limit
    ;; TODO: enable MWI if the model is not 82557
    (set 6 (fxior (if promisc? #b10000000 #b0) ;save bad frames
                    #b100000          ;standard statistical counters
                    #b10000))         ;standard TCB

    ;; TODO: enable dynamic TBD unless the device is an 82557
    (set 7 (fxior #b10             ;one retransmission on underrun
                    (if promisc? #b0 #b1)))    ;1=discard short frames

    ;; TODO: on the 82557 bit 0 should be 0 for 503 mode, 1 for MII
    ;; mode. Detect the PHY...
    (set 8 (fxior #b1))

    (set 10 #b101000)                ;7 bytes pre-amble, no source
                                        ;address insertion

    (set 12 (fxior (fxash 6 4)        ;interframe spacing: 6*16 bit times
                   #b1))            ;reserved except for the
                                        ;82557, where it changes the
                                        ;linear priority mode

    (set 13 0)                       ;these two are some ARP filter...
    (set 14 #xF2)

    ;; TODO: for 82557/82503 devices, bit 7 should be 1
    (set 15 (if promisc? #b1 #b0)) ;promiscous mode

    ;; TODO: for 82557 bit 3 should be 0 here
    (set 18 #b1111011)               ;frames longer than 1500 bytes
                                        ;are ok, transmitted packets
                                        ;are padded to 64 bytes, short
                                        ;received frames are stripped
                                        ;of their padding

    (set 19 #b10000000)              ;full duplex pin enable

    (set 20 #b100000)                ;priority FC location
    (set 21 #b1000)                  ;FIXME: receive all multicast
                                        ;frames, change this when
                                        ;filtering is implemented
    bv))

(define (e100-send-configuration regs promisc?)
  (let* ((cfg (make-e100-configuration promisc?))
         (&buf (dma-allocate 32 #xFFFFFFF0)))
    (CB-command-set! &buf (fxior CB-cmd-end-link CB-cmd-configure))
    (CB-link-offset-set! &buf &buf)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i (bytevector-length cfg)))
      (put-mem-u8 (fx+ (fx+ &buf 8) i) (bytevector-u8-ref cfg i)))
    (memory-fence)
    (SCB-pointer-set! regs &buf)
    (SCB-command-set! regs CU-start)
    (let ((status (let lp ((n 100))
                    (memory-fence)
                    (cond ((fxtest CB-status-complete (CB-status-ref &buf))
                           'ok)
                          ((eqv? n 0)
                           (newline)
                           'timeout)
                          (else
                           (msleep 1)
                           (lp (fx- n 1)))))))
      (dma-free &buf)
      status)))

(define (e100-mac-address-setup regs mac-address)
  (let ((&buf (dma-allocate 32 #xFFFFFFF0)))
    (CB-command-set! &buf (fxior CB-cmd-end-link CB-cmd-mac-address-setup))
    (CB-link-offset-set! &buf &buf)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i 6))
      (put-mem-u8 (fx+ (fx+ &buf 8) i) (bytevector-u8-ref mac-address i)))
    (memory-fence)
    (SCB-pointer-set! regs &buf)
    (SCB-command-set! regs CU-start)
    (let ((status (let lp ((n 100))
                    (memory-fence)
                    (cond ((fxtest CB-status-complete (CB-status-ref &buf))
                           'ok)
                          ((eqv? n 0)
                           (newline)
                           'timeout)
                          (else
                           (msleep 1)
                           (lp (fx- n 1)))))))
      (dma-free &buf)
      status)))

;;; The driver

(define (e100-reset regs)
  (PORT-set! regs PORT-software-reset)
  (µsleep 10))

(define (SCB-wait regs)
  (let lp ()
    (unless (eqv? 0 (SCB-command-ref regs))
      (µsleep 1)
      (lp))))

(define (e100-rx-dequeue regs &rx* rx-pkt)
  (let-values ([(&rx* rx-pkt)
                (if (or rx-pkt (not (fxtest RX-status-complete (RX-status-ref (car &rx*)))))
                    (values &rx* rx-pkt)
                    (let* ((&rx (car &rx*))
                           (len (fxand #x3fff (RX-actual-count-ref &rx)))
                           (hdrlen (RX-descriptor 'get-size))
                           (rx-pkt (make-netpkt #f (list &rx)
                                                (list (cons (fx+ &rx hdrlen) len)))))
                      (values (cdr &rx*) rx-pkt)))])
    (let ((&rx* (cond
                  ((pair? &rx*) &rx*)
                  (else
                   (let ((&rx* (make-receive-frame-area)))
                     (SCB-wait regs)
                     (SCB-pointer-set! regs (car &rx*))
                     (SCB-command-set! regs RU-start)
                     &rx*)))))
      (values &rx* rx-pkt))))

;; True if the CU can transmit the packet
(define (e100-netpkt-ok? pkt)
  (and (fx<=? (length (netpkt-bufs pkt)) max-tx-bufs)
       (fx<=? (netpkt-length pkt) #x3fff)
       (for-all (lambda (buf)
                  (let ((addr (car buf)) (len (cdr buf)))
                    ;; Completely within 32-bit address space and not
                    ;; too long.
                    (and (fx=? addr (fxbit-field addr 0 32))
                         (fx=? (fx+ addr len) (fxbit-field (fx+ addr len) 0 32))
                         (fx=? len (bitwise-bit-field len 0 14)))))
                (netpkt-bufs pkt))))

;; Check if the netpkt follows our rules. If it doesn't then we need
;; to allocate a new netpkt and copy the data into it. The caller must
;; then use the return value and not the value given as an argument.
(define (e100-netpkt-fixup! pkt)
  (if (e100-netpkt-ok? pkt)
      pkt
      (let ((new-pkt (netpkt-copy pkt #xFFFFFFF0)))
        (netpkt-free! pkt)
        new-pkt)))

;; pending* is a list of (&tx . netpkt) pairs for packets that have
;; been given to the CU to transmit. When a transfer is complete its
;; netpkt needs to be freed. The transfers happen in order so we only
;; need to look at the head of the list.
(define (e100-free-sent-packets! pending*)
  (let lp ((pending* pending*))
    (if (null? pending*)
        '()
        (let* ((pending (car pending*))
               (&tx (car pending))
               (pkt (cdr pending)))
          (cond ((fxtest CB-status-complete (CB-status-ref &tx))
                 (netpkt-free! pkt)
                 (lp (cdr pending*)))
                (else pending*))))))

(define (e100-driver regs irq iface)
  (define eeprom (eeprom-dump regs))
  ;; Clear the CU and RU offsets
  (SCB-pointer-set! regs 0)
  (SCB-command-set! regs (fxior CU-load-CU-base RU-load-RU-base))
  (SCB-wait regs)

  ;; Configure the card
  (unless (eq? (e100-send-configuration regs #f) 'ok)
    (error 'e100-init "Failed to configure the interface" iface))
  (unless (eq? (e100-mac-address-setup regs eeprom) 'ok)
    (error 'e100-init "Failed to configure the MAC" iface))

  (spawn-fiber
   (lambda ()
     (let lp ()
       (match (get-message (iface-ctrl-ch iface))
         [('hwaddr? . (? channel? resp-ch))
          (let ((mac (make-bytevector 6)))
            (bytevector-copy! eeprom 0 mac 0 6)
            (put-message resp-ch mac))])
       (lp))))

  (let-values ([(&rx*) (make-receive-frame-area)]
               [(&tx-bufs &tx*) (make-transfer-descriptors)])
    (define irq-token (enable-irq irq))
    ;; Start the receive and control units
    (SCB-pointer-set! regs (car &rx*))
    (SCB-command-set! regs RU-start)
    (SCB-wait regs)
    (SCB-pointer-set! regs (car &tx*))
    (SCB-command-set! regs CU-start)

    ;; Enable IRQs for all reasons
    (SCB-irq-mask-set! regs 0)

    ;; XXX: (car &tx*) always points to a descriptor with the
    ;; suspend bit set. &tx* is circularly linked. &rx* is consumed
    ;; and a fresh list is allocated.
    (let lp ((&rx* &rx*) (rx-pkt #f) (&tx* &tx*) (pending* '()))
      ;; XXX: Check if the next &tx is free before freeing its
      ;; buffer. Otherwise we might not free it, then see that it
      ;; completed and reuse it.
      (let* ((have-free-tx? (fxtest CB-status-complete (CB-status-ref (cadr &tx*))))
             (pending* (e100-free-sent-packets! pending*)))
        (match (perform-operation
                (choice-operation
                 ;; If we have received a packet then forward it on the rx-ch
                 (if rx-pkt
                     (wrap-operation (put-operation (iface-rx-ch iface) rx-pkt)
                                     (lambda _ 'rx-ack))
                     (wrap-operation (wait-irq-operation irq-token) (lambda _ 'int)))
                 ;; If we have a free tx descriptor then we can accept
                 ;; a packet to transmit
                 (if have-free-tx?
                     (wrap-operation (get-operation (iface-tx-ch iface))
                                     (lambda (x) (cons 'tx x)))
                     (choice-operation))))

          ['int
           (let-values ([(&rx* rx-pkt) (e100-rx-dequeue regs &rx* rx-pkt)])
             (SCB-STAT/ACK-set! regs #xff) ;ack all interrupts
             (lp &rx* rx-pkt &tx* pending*))]

          ['rx-ack
           (let-values ([(&rx* rx-pkt) (e100-rx-dequeue regs &rx* #f)])
             (lp &rx* rx-pkt &tx* pending*))]

          [('tx . pkt)
           (let ((pkt (e100-netpkt-fixup! pkt)))
             (cond ((not pkt)
                    (netpkt-free! pkt)  ;drop the pkt
                    (lp &rx* rx-pkt &tx* pending*))
                   (else
                    (let ((&prev (car &tx*))
                          (&tx (cadr &tx*)))
                      (e100-tx-fill-netpkt &tx pkt)
                      (CB-command-set! &prev (fxand (fxnot CB-cmd-suspend)
                                                    (CB-command-ref &prev)))
                      (SCB-wait regs)
                      (SCB-command-set! regs CU-resume)
                      (lp &rx* rx-pkt (cdr &tx*) (cons (cons &tx pkt) pending*))))))])))))

(define (probe·pci·eepro100? dev)
  (and (eqv? (pcidev-vendor-id dev) #x8086)
       (memv (pcidev-device-id dev) '(#x1209 #x1229 #x103d))
       (pcibar-i/o? (vector-ref (pcidev-BARs dev) 1))))

(define (driver·pci·eepro100 dev iface)
  (let ((regs (pcibar-base (vector-ref (pcidev-BARs dev) 1))))
    (e100-reset regs)
    (pci-put-u8 dev
                PCI-CFG-COMMAND
                (fxand (fxior (fxior PCI-CMD-I/O-SPACE
                                     PCI-CMD-BUS-MASTER)
                              (pci-get-u8 dev PCI-CFG-COMMAND))
                       (fxnot (fxior PCI-CMD-INTERRUPT-DISABLE
                                     PCI-CMD-MEM-SPACE))))
    (e100-driver regs (pcidev-irq dev) iface))))
