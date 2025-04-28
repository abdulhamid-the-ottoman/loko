;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Network interface and packet abstractions

;; XXX: Network packets, by convention, are u16-aligned in memory.

(library (loko drivers net)
  (export
    make-iface
    iface-type
    iface-rx-ch
    iface-tx-ch
    iface-ctrl-ch
    iface-notify-ch
    iface-get-hwaddr

    make-netpkt netpkt?
    netpkt-header
    netpkt-addrs
    netpkt-bufs
    netpkt-length
    netpkt-copy
    netpkt-free!
    netpkt-free->bytevector!
    netpkt-netinfo netpkt-netinfo-set!

    netpkt-tx-allocate

    netpkt-next-length
    netpkt-u8-ref  netpkt-u8-set!
    netpkt-u16-ref netpkt-u16-set! netpkt-u16-native-set!
    netpkt-u32-ref netpkt-u32-set!
    netpkt-u48-ref netpkt-u48-set!
    netpkt-consume!
    netpkt-truncate!
    netpkt-transplant!

    )
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (loko system fibers)
    (only (loko system unsafe) get-mem-u8 put-mem-u8
          get-mem-u16 put-mem-u16)
    (only (loko system $host) dma-allocate dma-free))

;; A network interface with its channels.
(define-record-type iface
  (sealed #t)
  (fields type
          ;; Receive. Drivers send netpkt records here.
          rx-ch
          ;; Transmit. Put netpkts here to transmit them.
          tx-ch
          ;; Device control.
          ctrl-ch
          ;; Notifications. Nothing implemented yet.
          notify-ch)
  (protocol
   (lambda (p)
     (lambda (type)
       (p type (make-channel) (make-channel)
          (make-channel) (make-channel))))))

(define (iface-get-hwaddr iface)
  (let ((resp-ch (make-channel)))
    (put-message (iface-ctrl-ch iface) (cons 'hwaddr? resp-ch))
    (get-message resp-ch)))

;; Represents a packet as a chain of buffers in physical memory. The
;; first buffer should be the start of the link-layer header.
(define-record-type netpkt
  (sealed #t)
  (fields header                        ;offloading stuff
          (mutable addrs)               ;addresses for dma-free
          (mutable bufs)                ;((addr . len) ...)
          (mutable netinfo))            ;Internet-level info
  (protocol
   (lambda (p)
     (lambda (header addrs bufs)
       (assert (for-all fixnum? addrs))
       (assert (for-all (lambda (x)
                          (and (pair? x)
                               (fixnum? (car x))
                               (fixnum? (cdr x))
                               (fx<=? 0 (cdr x) 65535)))
                        bufs))
       (p header addrs bufs #f)))))

;; Get the total length of all bytes in the packet's buffers.
(define (netpkt-length pkt)
  (let lp ((bufs (netpkt-bufs pkt))
           (len 0))
    (if (null? bufs)
        len
        (lp (cdr bufs) (fx+ (cdar bufs) len)))))

;; Copy the packet to buffers that are allocated with the given DMA
;; mask. This also moves all data to a single buffer.
(define (netpkt-copy pkt dma-mask)
  (let ((len (netpkt-length pkt)))
    (and (fx=? len (fxbit-field len 0 14))
         (let* ((&buf (dma-allocate len dma-mask))
                (newpkt (make-netpkt (netpkt-header pkt)
                                     (list &buf)
                                     (list (cons &buf len)))))
           ;; Copy each buffer from the old netpkt into the new
           ;; netpkt. This reduces the number of buffers to a
           ;; supported number and moves all the data to 32-bit
           ;; memory.
           (let lp ((buf* (netpkt-bufs pkt))
                    (buf-i 0))
             (unless (null? buf*)
               (do ((&src (caar buf*))
                    (len (cdar buf*))
                    (src-i 0 (fx+ src-i 1))
                    (buf-i buf-i (fx+ buf-i 1)))
                   ((fx>=? src-i len)
                    (lp (cdr buf*) buf-i))
                 (put-mem-u8 (fx+ &buf buf-i)
                             (get-mem-u8 (fx+ &src src-i))))))
           newpkt))))

;; Free a netpkt. If you get a packet from an rx-ch then you're
;; responsible for freeing it. If you put a packet on a tx-ch then the
;; driver is reponsible for freeing it.
(define (netpkt-free! pkt)
  (for-each dma-free (netpkt-addrs pkt))
  (netpkt-bufs-set! pkt '())
  (netpkt-addrs-set! pkt '()))

;; Copy the data in the buffers to a bytevector.
(define (netpkt-free->bytevector! pkt)
  (let* ((bv (make-bytevector (netpkt-length pkt))))
    (let lp ((buf* (netpkt-bufs pkt))
             (i 0))
      (unless (null? buf*)
        (do ((&src (caar buf*))
             (len (cdar buf*))
             (src-i 0 (fx+ src-i 1))
             (i i (fx+ i 1)))
            ((fx>=? src-i len)
             (lp (cdr buf*) i))
          (bytevector-u8-set! bv i (get-mem-u8 (fx+ &src src-i))))))
    (netpkt-free! pkt)
    bv))

;;; For transmission

;; We require that headers are never split between netpkt buffers.

;; Where R6RS safety makes it safe to elide explicit type checks, they
;; are elided. The buffer pointers and sizes are always checked
;; explicitly and an &assertion is raised if there is a problem. Code
;; that reads from netpkt must check netpkt-next-length first. All
;; reads and writes must be properly aligned.

;; TODO: make a built-in get-mem-u16be

(define (netpkt-next-length pkt)
  (let ((bufs (netpkt-bufs pkt)))
    (if (null? bufs) 0 (cdar bufs))))

(define (get-mem-u16be &addr)
  (let ((v (get-mem-u16 &addr)))
    (fxior (fxarithmetic-shift-right v 8)
           (fxarithmetic-shift-left (fxand v #xff) 8))))

(define (put-mem-u16be &addr v)
  (let ((v (fxior (fxarithmetic-shift-right v 8)
                  (fxarithmetic-shift-left (fxand v #xff) 8))))
    (put-mem-u16 &addr v)))

(define (netpkt-u8-ref pkt idx)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 1))))
    (get-mem-u8 (fx+ (car buf0) idx))))

(define (netpkt-u16-ref pkt idx)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 2))))
    (get-mem-u16be (fx+ (car buf0) idx))))

(define (netpkt-u32-ref pkt idx)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 4))))
    (let ((&addr (fx+ (car buf0) idx)))
      (fxior (fxarithmetic-shift-left (get-mem-u16be &addr) 16)
             (get-mem-u16be (fx+ &addr 2))))))

(define (netpkt-u48-ref pkt idx)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 6))))
    (let ((&addr (fx+ (car buf0) idx)))
      (fxior (fxarithmetic-shift-left (get-mem-u16be &addr) 32)
             (fxior (fxarithmetic-shift-left (get-mem-u16be (fx+ &addr 2)) 16)
                    (get-mem-u16be (fx+ &addr 4)))))))

(define (netpkt-u8-set! pkt idx v)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 1))))
    (put-mem-u8 (fx+ (car buf0) idx) v)))

(define (netpkt-u16-set! pkt idx v)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 2))))
    (put-mem-u16be (fx+ (car buf0) idx) v)))

(define (netpkt-u16-native-set! pkt idx v)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 2))))
    (put-mem-u16 (fx+ (car buf0) idx) v)))

(define (netpkt-u32-set! pkt idx v)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 4))))
    (let ((&addr (fx+ (car buf0) idx)))
      (put-mem-u16be &addr (fxbit-field v 16 32))
      (put-mem-u16be (fx+ &addr 2) (fxbit-field v 0 16)))))

(define (netpkt-u48-set! pkt idx v)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (assert (and (fx>=? idx 0) (fx>=? (cdr buf0) (fx+ idx 6))))
    (let ((&addr (fx+ (car buf0) idx)))
      (put-mem-u16be &addr (fxbit-field v 32 48))
      (put-mem-u16be (fx+ &addr 2) (fxbit-field v 16 32))
      (put-mem-u16be (fx+ &addr 4) (fxbit-field v 0 16)))))

;; Consume part of the first buffer, moving the pointer forward. XXX:
;; Do not consume across more than one buffer, it's not supported at
;; the moment.
(define (netpkt-consume! pkt amount)
  (let ((buf0 (car (netpkt-bufs pkt))))
    (let ((buflen (cdr buf0)))
      (cond ((fx=? buflen amount)
             (netpkt-bufs-set! pkt (cdr (netpkt-bufs pkt))))
            (else
             (assert (fx>=? buflen amount))
             (set-cdr! buf0 (fx- (cdr buf0) amount))
             (set-car! buf0 (fx+ (car buf0) amount)))))))

(define (netpkt-truncate! pkt len)
  (cond
    ((eqv? len 0)
     (netpkt-bufs-set! pkt '()))
    (else
     (let lp ((bufs (netpkt-bufs pkt))
              (len^ 0))
       (cond ((null? bufs)
              (unless (eqv? len^ len)
                (error 'netpkt-truncate! "Packet too short" pkt len)))
             (else
              (let ((len^ (fx+ (cdar bufs) len^)))
                (cond ((fx>? len^ len)
                       ;; We've now found too much data, so truncate this
                       ;; buffer and make it the last one.
                       (set-cdr! (car bufs) (fx- (cdar bufs) (fx- len^ len)))
                       (set-cdr! bufs '()))
                      ((fx=? len^ len)
                       (set-cdr! bufs '()))
                      (else
                       (lp (cdr bufs) len^))))))))))

;; Allocate a new netpkt for transmission on an interface.
;;
;; * The caller ensures that the minimum MTU size is respected, which
;;   may mean adding padding. On Ethernet the minimum MTU size is 64
;;   bytes. The layer just about the iface layer should have a
;;   convenience procedure that wraps this procedure.
;;
;; * The caller is careful to initialize padding bytes.
;;
;; * The caller is expected to start writing from the end of the
;;   buffer and to call netpkt-consume! before transmitting the
;;   packet, to adjust the buffer start.
;;
;; * The caller keeps track of the write offset.
;;
;; * The caller is expected to include enough headroom in the length
;;   to handle all lower layer headers. It is also possible to prepend
;;   extra buffers, but this lowers performance.
(define (netpkt-tx-allocate iface len)
  ;; FIXME: the interface should have a DMA mask for outgoing packets
  (define dma-mask (fxnot 7))
  (let ((&buf (dma-allocate len dma-mask)))
    (make-netpkt #f
                 (list &buf)
                 (list (cons &buf len)))))

;; Transplant the donor packet onto the end of the recipient packet.
;; Used for echoing and forwarding.
(define (netpkt-transplant! recipient-pkt donor-pkt)
  (netpkt-addrs-set! recipient-pkt (append (netpkt-addrs recipient-pkt) (netpkt-addrs donor-pkt)))
  (netpkt-bufs-set! recipient-pkt (append (netpkt-bufs recipient-pkt) (netpkt-bufs donor-pkt)))
  (netpkt-addrs-set! donor-pkt '())
  (netpkt-bufs-set! donor-pkt '())))
