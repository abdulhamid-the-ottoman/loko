;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020, 2021 G. Weinholt
#!r6rs

;;; Virtio network card driver

;; XXX: Written for the legacy virtio

(library (loko drivers net virtio)
  (export
    probe·pci·virtio-net?
    driver·pci·virtio-net)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko system random)
    (loko drivers pci)
    (loko drivers net)
    (loko drivers virtio)
    (only (loko system unsafe) put-mem-u16)
    (loko system unsafe cache)
    (only (loko system $host) enable-irq wait-irq-operation
          dma-allocate dma-free))

;; Max number of buffers per netpkt
(define *max-supported-buffers* 8)

;; Feature flags for network cards
(define VIRTIO_NET_F_CSUM                0)
(define VIRTIO_NET_F_GUEST_CSUM          1)
(define VIRTIO_NET_F_CTRL_GUEST_OFFLOADS 2)
(define VIRTIO_NET_F_MTU                 3)
(define VIRTIO_NET_F_MAC                 5)
(define VIRTIO_NET_F_GUEST_TSO4          7)
(define VIRTIO_NET_F_GUEST_TSO6          8)
(define VIRTIO_NET_F_GUEST_ECN           9)
(define VIRTIO_NET_F_GUEST_UFO           10)
(define VIRTIO_NET_F_HOST_TSO4           11)
(define VIRTIO_NET_F_HOST_TSO6           12)
(define VIRTIO_NET_F_HOST_ECN            13)
(define VIRTIO_NET_F_HOST_UFO            14)
(define VIRTIO_NET_F_MRG_RXBUF           15)
(define VIRTIO_NET_F_STATUS              16)
(define VIRTIO_NET_F_CTRL_VQ             17)
(define VIRTIO_NET_F_CTRL_RX             18)
(define VIRTIO_NET_F_CTRL_VLAN           19)
(define VIRTIO_NET_F_GUEST_ANNOUNCE      21)
(define VIRTIO_NET_F_MQ                  22)
(define VIRTIO_NET_F_CTRL_MAC_ADDR       23)
(define VIRTIO_NET_F_RSC_EXT             61)
(define VIRTIO_NET_F_STANDBY             62)

(define supported-features
  (fxior (fxarithmetic-shift-left 1 VIRTIO_NET_F_MAC)
         (fxarithmetic-shift-left 1 VIRTIO_NET_F_CTRL_VQ)
         (fxarithmetic-shift-left 1 VIRTIO_NET_F_STATUS)
         (fxarithmetic-shift-left 1 VIRTIO_NET_F_CSUM)))

;; Status bits
(define VIRTIO_NET_S_LINK_UP  1)
(define VIRTIO_NET_S_ANNOUNCE 2)

;; Configuration registers
(define offsetof-virtio_net_config-mac 0)
(define offsetof-virtio_net_config-status 6) ;u16

(define VIRTIO_NET_HDR_F_NEEDS_CSUM 1)
(define VIRTIO_NET_HDR_GSO_NONE     0)
(define VIRTIO_NET_HDR_GSO_TCPV4    1)
(define VIRTIO_NET_HDR_GSO_UDP      3)
(define VIRTIO_NET_HDR_GSO_TCPV6    4)
(define VIRTIO_NET_HDR_GSO_ECN      #x80)

(define (probe·pci·virtio-net? dev)
  (and (eqv? (pcidev-vendor-id dev) #x1AF4)
       (eqv? (pcidev-device-id dev) #x1000)))

(define (virtio-net-get-MAC reg-ctl)
  (do ((i 5 (fx- i 1))
       (MAC '() (cons (virtio-get-config-u8 reg-ctl (fx+ i offsetof-virtio_net_config-mac))
                      MAC)))
      ((fx=? i -1) (u8-list->bytevector MAC))))

;; Fill the rx queue with descriptors
(define (prepare-rxq! rxq)
  (let lp ()
    (unless (eqv? 0 (virtq-num-free rxq))
      (let ((bufs (list (cons (dma-allocate 2048 (fxnot 2)) 2048))))
        (virtq-add-buffers! rxq bufs 0 (caar bufs))
        (lp))))
  ;; FIXME: only kick if there were previously no available buffers
  (virtq-kick rxq))

;; Get a frame from the rx queue if there is one available and return
;; it as a netpkt.
(define (rxq-get-frame rxq)
  (let-values ([(len cookie) (virtq-get-buffer rxq)])
    (if (not len)
        #f
        (let ((addr cookie))
          (cond ((fx<? len 10)
                 ;; XXX: One of those things that should never happen
                 (dma-free addr)
                 (rxq-get-frame rxq))
                (else
                 ;; TODO: handle the offloading data
                 (let ((header #f))
                   (make-netpkt header
                                (list addr)
                                (list (cons (fx+ addr 10) (fx- len 10)))))))))))

;; Free all used TX descriptors
(define (process-txq txq)
  (let lp-free ()
    (let-values ([(len pkt) (virtq-get-buffer txq)])
      (when len
        (netpkt-free! pkt)
        (lp-free)))))

(define (driver·pci·virtio-net dev iface)
  (define reg-ctl (pcibar-base (vector-ref (pcidev-BARs dev) 0)))
  (define irq (pcidev-irq dev))
  (define common-features
    (begin
      (pci-put-u8 dev
                  PCI-CFG-COMMAND
                  (fxand (fxior (fxior PCI-CMD-I/O-SPACE
                                       PCI-CMD-BUS-MASTER)
                                (pci-get-u8 dev PCI-CFG-COMMAND))
                         (fxnot (fxior PCI-CMD-INTERRUPT-DISABLE
                                       PCI-CMD-MEM-SPACE))))
      (virtio-device-status-set! reg-ctl 0) ;reset
      (virtio-device-status-set! reg-ctl (fxior VIRTIO_CONFIG_S_DRIVER
                                                VIRTIO_CONFIG_S_ACKNOWLEDGE))
      (virtio-negotiate reg-ctl supported-features)))
  (define nethdr-len 10)       ;XXX: changed by VIRTIO_NET_F_MRG_RXBUF
  (let ((MAC (if (fxbit-set? common-features VIRTIO_NET_F_MAC)
                 (virtio-net-get-MAC reg-ctl)
                 (u8-list->bytevector (list #x02 (get-random-u8) (get-random-u8)
                                            (get-random-u8) (get-random-u8)
                                            (get-random-u8)))))
        ;; Setup queues
        (rxq (virtio-allocate-virtq reg-ctl 0))
        (txq (virtio-allocate-virtq reg-ctl 1))
        (ctrlq (and (fxbit-set? common-features VIRTIO_NET_F_CTRL_VQ)
                    (virtio-allocate-virtq reg-ctl 2))))
    (define irq-token (enable-irq irq))
    (spawn-fiber
     (lambda ()
       (let lp ()
         (match (get-message (iface-ctrl-ch iface))
           [('hwaddr? . (? channel? resp-ch))
            (put-message resp-ch (bytevector-copy MAC))])
         (lp))))

    (virtio-device-status-set! reg-ctl (fxior VIRTIO_CONFIG_S_DRIVER
                                              VIRTIO_CONFIG_S_ACKNOWLEDGE
                                              VIRTIO_CONFIG_S_DRIVER_OK))
    (prepare-rxq! rxq)
    ;; The main loop. We want an interrupt if the tx virtq is full
    ;; or if the rx virtq is empty.
    (let lp ((rx-pkt #f))
      (process-txq txq)
      (match (perform-operation
              (choice-operation
               (if (or (eqv? 0 (virtq-num-avail txq))
                       (not rx-pkt))
                   (wrap-operation (wait-irq-operation irq-token) (lambda _ 'int))
                   (choice-operation))
               (if (not rx-pkt)
                   (choice-operation)
                   (wrap-operation (put-operation (iface-rx-ch iface) rx-pkt)
                                   (lambda _ 'rx-ack)))
               (if (fx<? (virtq-num-free txq) *max-supported-buffers*)
                   (sleep-operation 0.001) ;Hmm.
                   (wrap-operation (get-operation (iface-tx-ch iface))
                                   (lambda (x) (cons 'tx x))))))
        ['int
         ;; The device wants us to know that it has used a
         ;; descriptor (or it's from another device). If we're not
         ;; trying to forward a netpkt to rx-ch then we try to get a
         ;; buffer from the receive virtq.
         (virtio-isr-status! reg-ctl)   ;resets interrupt status
         (memory-fence)
         (let ((rx-pkt (or rx-pkt (rxq-get-frame rxq))))
           (when rx-pkt
             (virtq-disable-interrupts! rxq))
           (lp rx-pkt))]

        ['rx-ack
         ;; We have forwarded a frame on rx-ch, let's get another
         ;; frame from the receive virtq.
         (let ((rx-pkt (rxq-get-frame rxq)))
           (prepare-rxq! rxq)
           (when (not rx-pkt)
             (virtq-enable-interrupts! rxq))
           (lp rx-pkt))]

        [('tx . (? netpkt? pkt))
         ;; Someone wants us to transmit a frame and we have enough
         ;; space for it.
         (let ((&nethdr (dma-allocate nethdr-len (fxnot 2))))
           ;; TODO: Convert the offloading stuff in the header
           (do ((i 0 (fx+ i 2)))
               ((fx=? i nethdr-len))
             (put-mem-u16 (fx+ &nethdr i) 0))
           (let ((pkt (make-netpkt (netpkt-header pkt)
                                   (cons &nethdr (netpkt-addrs pkt))
                                   (cons (cons &nethdr nethdr-len) (netpkt-bufs pkt)))))
             (virtq-add-buffers! txq (netpkt-bufs pkt) (length (netpkt-bufs pkt)) pkt)
             (virtq-kick txq)
             (lp rx-pkt)))]

        [_ (lp rx-pkt)]))

    ;; Reset the device and free all memory
    (virtio-device-status-set! reg-ctl 0) ;reset
    (virtq-free! rxq)
    (virtq-free! txq)
    (when ctrlq
      (virtq-free! ctrlq)))))
