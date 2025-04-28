;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Linux user space tun interface driver

;; TODO: Get the MAC address from the interface and send it on the
;; notification channel.

(library (loko drivers net tun)
  (export
    driver·kernel·tun)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko drivers net)

    (loko system unsafe)
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls)
    (only (loko system $host) dma-allocate dma-free))

;; Open a TUN/TAP device. The device name is either a name of an
;; interface or a format string like "tun%d"/"tap%d". TAP interfaces
;; use ethernet headers and TUN interfaces do not.
(define (driver·kernel·tun devname iface)
  (define who 'driver·kernel·tun)
  (assert (eq? (iface-type iface) 'ethernet)) ;IFF_TAP only right now
  (let* ((fn (string->utf8 "/dev/net/tun"))
         (fd (sys_open (bytevector-address fn)
                       (fxior O_RDWR O_NOCTTY O_NONBLOCK)
                       #x0644))
         (ifr (make-bytevector sizeof-ifreq)))
    (let ((ifname (string->utf8 devname)))
      (assert (fx<? (bytevector-length ifname) IFNAMSIZ))
      (bytevector-copy! ifname 0 ifr 0 (bytevector-length ifname)))
    (bytevector-u16-native-set! ifr offsetof-ifreq-ifr_flags IFF_TAP)
    (let retry ()
      ;; XXX: The kernel fills in the actual interface name in ifr, so
      ;; we can just reuse the same buffer for future requests.
      (sys_ioctl fd TUNSETIFF (bytevector-address ifr)
                 (lambda (errno)
                   (cond ((eqv? errno EINTR)
                          (retry))
                         (else
                          (sys_close fd)
                          (raise
                            (condition
                             (make-who-condition who)
                             (make-message-condition "Failed to open the interface")
                             (make-irritants-condition (list fd 'TUNSETIFF devname 'IFF_TAP))
                             (make-syscall-error 'ioctl errno))))))))
    ;; Get the hardware address
    (sys_ioctl fd SIOCGIFHWADDR (bytevector-address ifr))
    (let ((MAC (make-bytevector 6)))
      (bytevector-copy! ifr (+ offsetof-ifreq-ifr_hwaddr offsetof-sockaddr-sa_data)
                        MAC 0 6)
      ;; This is the MAC used by the Linux kernel. Invert the last bit
      ;; to make one of our own design. (It is supposed to be random,
      ;; but I've been getting the same every time).
      (bytevector-u8-set! MAC 5 (fxxor 1 (bytevector-u8-ref MAC 5)))
      (spawn-fiber
       (lambda ()
         (let lp ()
           (match (get-message (iface-ctrl-ch iface))
             [('hwaddr? . (? channel? resp-ch))
              (put-message resp-ch (bytevector-copy MAC))])
           (lp)))))
    ;; The receive loop
    (spawn-fiber
     (lambda ()
       (let lp-rx ()
         ;; XXX: This assumes that the packets are freed with dma-free
         ;; at some reasonable (almost 1-to-1) pace by whoever
         ;; receives them.
         (let* ((buflen 2048)
                (&buf (dma-allocate buflen (fxnot 2)))
                (pktlen
                 (let retry ()
                   (sys_read fd &buf buflen
                             (lambda (errno)
                               (cond ((eqv? errno EINTR)
                                      (retry))
                                     ((eqv? errno EAGAIN)
                                      (wait-for-readable fd)
                                      (retry))
                                     (else
                                      (sys_close fd)
                                      (raise
                                        (condition
                                         (make-who-condition who)
                                         (make-message-condition "Receive error")
                                         (make-irritants-condition (list fd))
                                         (make-syscall-error 'read errno)))))))))
                (header #f))
           (put-message (iface-rx-ch iface)
                        (make-netpkt header
                                     (list &buf)
                                     (list (cons (fx+ &buf sizeof-tun_pi)
                                                 (fx- pktlen sizeof-tun_pi))))))
         (lp-rx))))
    ;; The transmit loop
    (let ((pi (make-bytevector sizeof-tun_pi)))
      (let lp-tx ()
        (let ((pkt (get-message (iface-tx-ch iface))))
          (when (netpkt? pkt)
            ;; Use scatter-gather to hand all buffers to the kernel in
            ;; one syscall
            (let* ((bufs (netpkt-bufs pkt))
                   (len (fx+ (length bufs) 1))
                   (iov (make-bytevector (fx* sizeof-iovec len))))
              (bytevector-u64-native-set! iov 0 (bytevector-address pi))
              (bytevector-u64-native-set! iov 8 (bytevector-length pi))
              (let lp ((offset 16) (bufs bufs))
                (unless (null? bufs)
                  (bytevector-u64-native-set! iov offset (caar bufs))
                  (bytevector-u64-native-set! iov (fx+ offset 8) (cdar bufs))
                  (lp (fx+ offset 16) (cdr bufs))))
              (let retry ()
                (sys_pwritev2 fd (bytevector-address iov) len -1 -1 0
                              (lambda (errno)
                                (cond ((eqv? errno EINTR)
                                       (retry))
                                      ((eqv? errno EAGAIN)
                                       (wait-for-writable fd)
                                       (retry))
                                      (else
                                       (sys_close fd)
                                       (raise
                                         (condition
                                          (make-who-condition who)
                                          (make-message-condition "Send error")
                                          (make-irritants-condition (list fd))
                                          (make-syscall-error 'pwritev2 errno))))))))
              (netpkt-free! pkt))))
        (lp-tx)))
    (sys_close fd))))
