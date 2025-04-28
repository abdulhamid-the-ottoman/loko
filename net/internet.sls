;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Internet over Ethernet

;; This is the layer between an Ethernet interface and the upper layer
;; that handles UDP and TCP.

;; For now there's an unwarranted assumption that IP is only used over
;; Ethernet.

;; RFC 768, UDP
;; RFC 791, IP
;; RFC 792, ICMP
;; RFC 793, TCP
;; RFC 826, ARP (Symbolics!)
;; RFC 894, IP over Ethernet (also from Symbolics)

;; RFC 2464, Transmission of IPv6 Packets over Ethernet Networks
;; RFC 4291, IP Version 6 Addressing Architecture
;; RFC 8200, Internet Protocol, Version 6
;; RFC 4861, Neighbor Discovery for IP version 6
;; RFC 8504, IPv6 Node Requirements
;; RFC 6890, Special-Purpose IP Address Registries

(library (loko net internet)
  (export
    make-ether
    ether-hw-address
    ether-add-address
    ether-add-router
    ether-add-dns-server
    send-ipv4
    send-ipv6
    send-udp
    send-tcp
    listen-udp
    listen-tcp
    ipv4-address?
    ipv6-address?

    net·ethernet·internet-stack

    ether-default-ipv4-source
    ether-default-ipv6-source
    ether-dns-servers
    )
  (import
    (rnrs)
    (loko match)
    (loko drivers net)
    (loko system fibers)
    (loko net numbers)
    (loko system unsafe)) ;TODO: this should ideally not be needed here

(define DEFAULT-HLIM 64)

(define (netpkt-u128-set/indir! pkt offset &ptr)
  (do ((i 0 (fx+ i 1)))
      ((eqv? i 16))
    (netpkt-u8-set! pkt (fx+ offset i) (get-mem-u8 (fx+ &ptr i)))))

(define (netpkt-u128-set/bv! pkt offset bv)
  (do ((i 0 (fx+ i 1)))
      ((eqv? i 16))
    (netpkt-u8-set! pkt (fx+ offset i) (bytevector-u8-ref bv i))))

(define (netpkt-u128-ref/bv pkt offset)
  (do ((bv (make-bytevector 16))
       (i 0 (fx+ i 1)))
      ((eqv? i 16) bv)
    (bytevector-u8-set! bv i (netpkt-u8-ref pkt (fx+ offset i)))))

;; An Ethernet interface with ARP, IPv4 and IPv6 on top of it. The
;; underlying interface's driver must already have started so it can
;; tell us the hardware address.
(define-record-type ether
  (sealed #t)
  (fields iface

          ;; XXX: Don't expose the send channel directly,
          ;; but only through an API that checks the arguments
          internet-send-ch
          control-ch
          stop-cvar

          hw-address                    ;EUI-48 address
          (mutable ipv4-addresses)
          (mutable ipv6-addresses)

          ipv4-neighbors
          ipv6-neighbors

          (mutable ipv4-routers)
          (mutable ipv6-routers)

          (mutable dns-ipv4-servers)
          (mutable dns-ipv6-servers)

          (mutable listeners))
  (protocol
   (lambda (p)
     (lambda (iface)
       ;; FIXME: Hash tables are *not* suitable as neighbor tables.
       ;; FIXME: Also need a good data structure for listeners, or it
       ;; should just be handled in the TCP and UDP layers.
       ;; The IPv4 table is address-as-int => hwaddr-as-int.
       ;; For IPv6 it is addr-as-bytevector => hwaddr-as-int.
       (let* ((hwaddr (bytevector-uint-ref (iface-get-hwaddr iface) 0
                                           (endianness big) 6))
              (ipv6-lladdr (make-bytevector 16)))
         (bytevector-uint-set! ipv6-lladdr 0
                               (bitwise-ior (bitwise-arithmetic-shift-left #xFE80 112)
                                            (eui48->modified-eui64 hwaddr))
                               (endianness big) 16)
         (p iface

            (make-channel)
            (make-channel)
            (make-cvar)

            hwaddr
            ;; Internet addresses. List of (ip-as-int-or-bv . prefix-length).
            '()
            (list (cons ipv6-lladdr 64))
            ;; neighbors
            (make-eqv-hashtable)
            (make-hashtable equal-hash equal?)
            ;; routers
            '()
            '()
            ;; dns servers
            '()
            '()
            ;; listeners
            '()))))))

(define (eui48->modified-eui64 x)
  (bitwise-xor (bitwise-arithmetic-shift-left 1 57)
               (bitwise-ior
                (bitwise-bit-field x 0 24)
                (bitwise-arithmetic-shift-left #xFFFE 24)
                (bitwise-arithmetic-shift-left (bitwise-bit-field x 24 48) 40))))

(define (ipv4-address? x) (and (fixnum? x) (fx<=? 0 x #xffffffff)))

(define (ipv6-address? x) (and (bytevector? x) (fx=? (bytevector-length x) 16)))

(define (ether-default-ipv4-source ether)
  (and (pair? (ether-ipv4-addresses ether))
       (caar (ether-ipv4-addresses ether))))

;; XXX: This is likely a bad API
(define (ether-default-ipv6-source ether)
  (and (pair? (ether-ipv6-addresses ether))
       (bytevector-copy (caar (ether-ipv6-addresses ether)))))

(define (ether-my-ipv4-address? ether addr)
  (exists (lambda (prefix) (fx=? addr (car prefix)))
          (ether-ipv4-addresses ether)))

(define (ether-my-ipv6-address? ether addr)
  (exists (lambda (prefix) (equal? addr (car prefix)))
          (ether-ipv6-addresses ether)))

;; Takes a pointer to an IPv6 address from a netpkt buffer and checks
;; if it is ours.
(define (ether-my-ipv6-address-indir? ether &target)
  (exists (lambda (prefix)
            (let ((addr (car prefix)))
              (let lp ((i 0))
                (cond ((fx=? i 16) #t)
                      ((fx=? (bytevector-u8-ref addr i) (get-mem-u8 (fx+ &target i)))
                       (lp (fx+ i 1)))
                      (else #f)))))
          (ether-ipv6-addresses ether)))

(define (ether-local-ipv6-address ether)
  (let lp ((addr* (ether-ipv6-addresses ether)))
    (cond ((null? addr*)
           (error 'ether-local-ipv6-address "No link-local address" ether))
          ;; Match FE80::/10
          ((eqv? (fxand #xFFC0 (bytevector-u16-ref (caar addr*) 0 (endianness big)))
                 #xFE80)
           (caar addr*))
          (else (lp (cdr addr*))))))

(define (ether-neighbor-ipv4-ref ether addr)
  (if (eqv? addr #xFFFFFFFF)            ;broadcast?
      #xFFFFFFFFFFFF
      (hashtable-ref (ether-ipv4-neighbors ether) addr #f)))

(define (ether-neighbor-ipv4-set! ether addr hwaddr)
  (hashtable-set! (ether-ipv4-neighbors ether) addr hwaddr))

(define (ether-neighbor-ipv6-ref ether addr)
  (hashtable-ref (ether-ipv6-neighbors ether) addr #f))

(define (ether-neighbor-ipv6-set! ether addr hwaddr)
  (hashtable-set! (ether-ipv6-neighbors ether) addr hwaddr))

(define (ether-neighbor-ipv6-indir-ref ether &addr)
  ;; TODO: Define special hash and equal procedures that handle
  ;; indirect hashes and comparisons
  (hashtable-ref (ether-ipv6-neighbors ether) (get-mem-ipv6 &addr) #f))

(define (ether-ipv6-next-hop ether &?dst)
  (let ((dst/int (if (bytevector? &?dst)
                     (bytevector-uint-ref &?dst 0 (endianness big) 16)
                     (do ((i 0 (fx+ i 1))
                          (addr 0 (bitwise-ior (bitwise-arithmetic-shift-left addr 8)
                                               (get-mem-u8 (fx+ &?dst i)))))
                         ((fx=? i 16) addr)))))
    (cond
      ((find (lambda (prefix)
               ;; FIXME: This is slow code
               (let ((addr (bytevector-uint-ref (car prefix) 0 (endianness big) 16))
                     (plen (cdr prefix)))
                 (let ((netmask (bitwise-arithmetic-shift-left (- (bitwise-arithmetic-shift-left 1 plen) 1)
                                                               (fx- 128 plen))))
                   (= (bitwise-and dst/int netmask)
                      (bitwise-and addr netmask)))))
             (ether-ipv6-addresses ether))
       &?dst)
      (else
       (let ((routers (ether-ipv6-routers ether)))
         (if (null? routers)
             #f
             (car routers)))))))

(define (ether-ipv4-next-hop ether dst)
  (cond
    ((eqv? dst #xFFFFFFFF)
     dst)
    ((find (lambda (prefix)
             (let ((addr (car prefix)) (plen (cdr prefix)))
               (let ((netmask (fxarithmetic-shift-left (fx- (fxarithmetic-shift-left 1 plen) 1)
                                                       (fx- 32 plen))))
                 (fx=? (fxand dst netmask) (fxand addr netmask) ))))
           (ether-ipv4-addresses ether))
     dst)
    (else
     (let ((routers (ether-ipv4-routers ether)))
       (if (null? routers)
           #f
           (car routers))))))

(define (get-mem-ipv6 &addr)
  (do ((bv-addr (make-bytevector 16))
       (i 0 (fx+ i 1)))
      ((fx=? i 16) bv-addr)
    (bytevector-u8-set! bv-addr i (get-mem-u8 (fx+ &addr i)))))

(define (ether-find-listener ether proto src sport dst dport)
  (let lp ((listeners (ether-listeners ether)))
    (if (null? listeners)
        #f
        (match (car listeners)
          [(proto^ src^ sport^ dst^ dport^ recv-ch)
           (cond ((and (eq? proto proto^)
                       (or (eq? src^ '*) (equal? src^ src))
                       (or (eq? sport^ '*) (fx=? sport^ sport))
                       (or (eq? dst^ '*) (equal? dst^ dst))
                       (or (eq? dport^ '*) (fx=? dport^ dport)))
                  recv-ch)
                 (else (lp (cdr listeners))))]))))

(define (ether-find-listener/indir ether proto &src sport &dst dport)
  (let ((src (get-mem-ipv6 &src))   ;; FIXME: don't require this
        (dst (get-mem-ipv6 &dst)))
    (ether-find-listener ether proto src sport dst dport)))

;; Allocate a packet for transmission of len upper-layer bytes.
;; Returns the packet and an offset to the end of the buffer. Treat it
;; as a stack: subtract the offset and then write.
(define (ether-netpkt-allocate ether len)
  ;; XXX: The headroom here is taken out of thin air. It should be
  ;; able to contain all lower-layer headers.
  ;; FIXME: handle the out-of-memory situation.
  (let ((iface (ether-iface ether))
        (headroom 128))
    (let ((buflen (fx+ len headroom)))
      (values (netpkt-tx-allocate iface buflen) buflen))))

;;; IP checksum

;; The IP checksum is described in RFC 1071. The computation is done
;; in native endianness and the result must also be stored in the
;; packet using native endianness.

;; Compute part of the IP checksum. Only the last buffer can have an
;; odd number of bytes. Computation is done with native endianness.
;; The offset is for the first buffer.
(define (netpkt-ip-native-checksum sum pkt offset)
  (let lp ((sum sum)
           (bufs (netpkt-bufs pkt))
           (offset offset))
    (if (null? bufs)
        sum
        (let ((buf (car bufs)))
          (let ((&ptr (fx+ (car buf) offset)) (len (fx- (cdr buf) offset)))
            (do ((&ptr &ptr (fx+ &ptr 2))
                 (len (fxand len -2) (fx- len 2))
                 (sum (if (fxodd? len) (fx+ sum (get-mem-u8 (fx+ &ptr (fx- len 1)))) sum)
                      (fx+ sum (get-mem-u16 &ptr))))
                ((eqv? len 0)
                 (lp sum (cdr bufs) 0))))))))

;; Compute part of the IP checksum on [offset,offset+len) in the first
;; buffer of the netpkt. It must be an even number of bytes.
(define (netpkt-ip-native-checksum-part sum pkt offset len)
  (let ((buf (car (netpkt-bufs pkt))))
    (let ((&ptr (fx+ (car buf) offset))
          (len^ (fx- (cdr buf) offset)))
      (assert (fx<=? len len^))
      (do ((&ptr &ptr (fx+ &ptr 2))
           (len len (fx- len 2))
           (sum sum (fx+ sum (get-mem-u16 &ptr))))
          ((eqv? len 0) sum)))))

;; Compute part of the IP checksum for a bytevector.
(define (netpkt-ip-native-checksum-bytevector sum bv)
  (do ((i 0 (fx+ i 2))
       (len (fxand (bytevector-length bv) -2))
       (sum (if (fxodd? (bytevector-length bv))
                (fx+ sum (bytevector-u8-ref bv (fx- (bytevector-length bv) 1)))
                sum)
            (fx+ sum (bytevector-u16-native-ref bv i))))
      ((fx=? i len) sum)))

;; Compute part of the IP checksum on an IPv6 pseudo header.
(define (ipv6-pseudo-header-native-checksum sum &src &dst len next-header)
  (if (eq? (native-endianness) (endianness little))
      (do ((i 0 (fx+ i 2))
           (sum (fx+ (fx+ sum (fxarithmetic-shift-left next-header 8))
                     (fx+ (fxior (fxarithmetic-shift-right (fxand len #x00ff0000) 8)
                                 (fxarithmetic-shift-right (fxand len #xff000000) 24))
                          (fxior (fxarithmetic-shift-left  (fxand len #x000000ff) 8)
                                 (fxarithmetic-shift-right (fxand len #x0000ff00) 8))))
                (fx+ sum (fx+ (get-mem-u16 (fx+ &src i))
                              (get-mem-u16 (fx+ &dst i))))))
          ((eqv? i 16) sum))
      (let ((bv (make-bytevector 8)))
        (bytevector-u32-set! bv 0 len (endianness big))
        (bytevector-u32-set! bv 4 next-header (endianness big))
        (do ((i 0 (fx+ i 2))
             (sum (fx+ (fx+ sum (bytevector-u16-native-ref bv 0))
                       (fx+ (bytevector-u16-native-ref bv 2)
                            (fx+ (bytevector-u16-native-ref bv 4)
                                 (bytevector-u16-native-ref bv 6))))
                  (fx+ sum (fx+ (get-mem-u16 (fx+ &src i))
                                (get-mem-u16 (fx+ &dst i))))))
            ((eqv? i 16) sum)))))

(define (ipv6-pseudo-header-native-checksum/bv sum src dst len next-header)
  (if (eq? (native-endianness) (endianness little))
      (do ((i 0 (fx+ i 2))
           (sum (fx+ (fx+ sum (fxarithmetic-shift-left next-header 8))
                     (fx+ (fxior (fxarithmetic-shift-right (fxand len #x00ff0000) 8)
                                 (fxarithmetic-shift-right (fxand len #xff000000) 24))
                          (fxior (fxarithmetic-shift-left  (fxand len #x000000ff) 8)
                                 (fxarithmetic-shift-right (fxand len #x0000ff00) 8))))
                (fx+ sum (fx+ (bytevector-u16-native-ref src i)
                              (bytevector-u16-native-ref dst i)))))
          ((eqv? i 16) sum))
      (let ((bv (make-bytevector 8)))
        (bytevector-u32-set! bv 0 len (endianness big))
        (bytevector-u32-set! bv 4 next-header (endianness big))
        (do ((i 0 (fx+ i 2))
             (sum (fx+ (fx+ sum (bytevector-u16-native-ref bv 0))
                       (fx+ (bytevector-u16-native-ref bv 2)
                            (fx+ (bytevector-u16-native-ref bv 4)
                                 (bytevector-u16-native-ref bv 6))))
                  (fx+ sum (fx+ (bytevector-u16-native-ref src i)
                                (bytevector-u16-native-ref dst i)))))
            ((eqv? i 16) sum)))))

(define (ip-native-checksum-finish sum)
  (fxxor #xffff (fxand #xffff (fx+ (fxarithmetic-shift-right sum 16) sum))))

(define (ip-native-ipv4-pseudo-checksum sum src dst len proto)
  (let ((bv (make-bytevector 12)))
    (bytevector-u32-set! bv 0 src (endianness big))
    (bytevector-u32-set! bv 4 dst (endianness big))
    (bytevector-u16-set! bv 8 len (endianness big))
    (bytevector-u16-set! bv 10 proto (endianness big))
    (fx+ sum
         (fx+ (bytevector-u16-native-ref bv 0)
              (fx+ (bytevector-u16-native-ref bv 2)
                   (fx+ (bytevector-u16-native-ref bv 4)
                        (fx+ (bytevector-u16-native-ref bv 6)
                             (fx+ (bytevector-u16-native-ref bv 8)
                                  (bytevector-u16-native-ref bv 10)))))))))

;;; IPv6


(define (ipv6-prepend-header! pkt off vfc nxt hlim &?src &?dst)
  (let ((payload-length (fx- (netpkt-length pkt) off)))
    (let ((off (fx- off sizeof-ip6_hdr))
          (vfc (fxior (fxarithmetic-shift-left 6 28) ;version
                      (fxand vfc #xfffffff))))
      (netpkt-u32-set! pkt (fx+ off offsetof-ip6_hdr-ip6_vfc) vfc)
      (netpkt-u16-set! pkt (fx+ off offsetof-ip6_hdr-ip6_plen) payload-length)
      (netpkt-u8-set! pkt (fx+ off offsetof-ip6_hdr-ip6_nxt) nxt)
      (netpkt-u8-set! pkt (fx+ off offsetof-ip6_hdr-ip6_hlim) hlim)
      (if (bytevector? &?src)
          (netpkt-u128-set/bv! pkt (fx+ off offsetof-ip6_hdr-ip6_src) &?src)
          (netpkt-u128-set/indir! pkt (fx+ off offsetof-ip6_hdr-ip6_src) &?src))
      (if (bytevector? &?dst)
          (netpkt-u128-set/bv! pkt (fx+ off offsetof-ip6_hdr-ip6_dst) &?dst)
          (netpkt-u128-set/indir! pkt (fx+ off offsetof-ip6_hdr-ip6_dst) &?dst))
      off)))

(define (ether-tx-ipv6 ether pkt off vfc nxt hlim &?src &?dst target-hwaddr)
  (let ((off (ipv6-prepend-header! pkt off vfc nxt hlim &?src &?dst))
        (next-hop (ether-ipv6-next-hop ether &?dst)))
    (cond
      ((not (or target-hwaddr next-hop))
       'no-route)
      ((or target-hwaddr
           (if (bytevector? next-hop)
               (ether-neighbor-ipv6-ref ether next-hop)
               (ether-neighbor-ipv6-indir-ref ether next-hop)))
       => (lambda (target-hwaddr)
            (ether-tx-pkt ether pkt off ETHERTYPE_IPV6 target-hwaddr)
            'ok))
      (else
       ;; No entry in the neighbor table, so send a neighbor solicitation
       (ether-tx-ipv6-neighbor-solicitation-indir ether next-hop)
       (netpkt-free! pkt)
       'no-entry))))

(define (ether-tx-ipv6-neighbor-solicitation-indir ether &?target)
  (let-values ([(pkt off) (ether-netpkt-allocate ether (+ sizeof-icmp6_hdr 16 8))])
    ;; Add an option with our link-layer address
    (let ((off (fx- off 8)))
      (netpkt-u8-set! pkt (fx+ off offsetof-nd_opt_hdr-nd_opt_type) ND_OPT_SOURCE_LINKADDR)
      (netpkt-u8-set! pkt (fx+ off offsetof-nd_opt_hdr-nd_opt_len) 1)
      (netpkt-u48-set! pkt (fx+ off sizeof-nd_opt_hdr) (ether-hw-address ether))
      (let ((off (fx- off 16)))
        (if (bytevector? &?target)
            (netpkt-u128-set/bv! pkt off &?target)
            (netpkt-u128-set/indir! pkt off &?target))
        ;; Construct a solicited neighbor advertisement
        (let ((off (fx- off sizeof-icmp6_hdr)))
          (netpkt-u8-set! pkt (fx+ off offsetof-icmp6_hdr-icmp6_type) ND_NEIGHBOR_SOLICIT)
          (netpkt-u8-set! pkt (fx+ off offsetof-icmp6_hdr-icmp6_code) 0)
          (netpkt-u16-set! pkt (fx+ off offsetof-icmp6_hdr-icmp6_cksum) 0)
          (netpkt-u32-set! pkt (fx+ off offsetof-icmp6_hdr-icmp6_data32) 0)
          (let ((target (make-bytevector 16 0)))
            ;; Solicited-Node Address: FF02:0:0:0:0:1:FFXX:XXXX
            (bytevector-u8-set! target 0 #xff)
            (bytevector-u8-set! target 1 #x02)
            (bytevector-u8-set! target 11 #x01)
            (bytevector-u8-set! target 12 #xff)
            (cond ((bytevector? &?target)
                   (bytevector-copy! &?target 13 target 13 3))
                  (else
                   (bytevector-u8-set! target 13 (get-mem-u8 (fx+ &?target 13)))
                   (bytevector-u8-set! target 14 (get-mem-u8 (fx+ &?target 14)))
                   (bytevector-u8-set! target 15 (get-mem-u8 (fx+ &?target 15)))))
            (let* ((pseudo (ipv6-pseudo-header-native-checksum/bv 0 (ether-local-ipv6-address ether) target
                                                                  (fx- (netpkt-length pkt) off) IPPROTO_ICMPV6))
                   (cksum (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo pkt off))))
              (netpkt-u16-native-set! pkt (fx+ off offsetof-icmp6_hdr-icmp6_cksum) cksum)
              (let ((target-hwaddr (fxior #x333300000000 (bytevector-u32-ref target 12 (endianness big)))))
                (ether-tx-ipv6 ether pkt off 0 IPPROTO_ICMPV6 255
                               (ether-local-ipv6-address ether)
                               target target-hwaddr)))))))))

(define (handle-ipv6-udp-packet ether pkt vfc payload-length hlim &src &dst)
  ;; XXX: give the packet to the upper layer via a channel. Each
  ;; receiver should have at most a single packet pending.
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-udphdr)
     (netpkt-free! pkt))
    (else
     (let ((len (netpkt-u16-ref pkt offsetof-udphdr-uh_ulen)))
       (cond
         ((or (fx<? len sizeof-udphdr)
              (fx<? (netpkt-length pkt) len))
          (netpkt-free! pkt))
         (else
          (netpkt-truncate! pkt len)
          (let ((sport (netpkt-u16-ref pkt offsetof-udphdr-uh_sport))
                (dport (netpkt-u16-ref pkt offsetof-udphdr-uh_dport)))
            (let* ((pseudo (ipv6-pseudo-header-native-checksum 0 &src &dst len IPPROTO_UDP))
                   (checksum (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo pkt 0))))
              (cond
                ((not (eqv? 0 checksum))
                 ;; XXX: Zero checksums are not allowed here (but see RFC 6936)
                 (netpkt-free! pkt))
                (else
                 (cond ((ether-find-listener/indir ether 'udp &src sport &dst dport)
                        => (lambda (recv-ch) (put-message recv-ch pkt)))
                       (else
                        ;; FIXME: send an error
                        (netpkt-free! pkt)))))))))))))

(define (handle-ipv6-tcp-packet ether pkt vfc payload-length hlim &src &dst)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-tcphdr)
     (netpkt-free! pkt))
    (else
     (let* ((pseudo (ipv6-pseudo-header-native-checksum 0 &src &dst (netpkt-length pkt) IPPROTO_TCP))
            (checksum (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo pkt 0))))
       (cond
         ((not (eqv? 0 checksum))
          (netpkt-free! pkt))
         (else
          (let ((sport (netpkt-u16-ref pkt offsetof-tcphdr-th_sport))
                (dport (netpkt-u16-ref pkt offsetof-tcphdr-th_dport)))
            (cond ((ether-find-listener/indir ether 'tcp &src sport &dst dport)
                   => (lambda (recv-ch) (put-message recv-ch pkt)))
                  (else
                   ;; FIXME: send an error
                   (netpkt-free! pkt))))))))))

;; Get an option from the ICMPv6 packet. It must be positioned exactly
;; at the start of the options and it should have been truncated to
;; the payload length. Returns the last value of the option, as
;; decoded with proc, or #f if not found or the options are invalid.
(define (icmp6-get-option pkt option-type min-len proc)
  ;; FIXME: This is not a good way of doing things, at all.
  (let lp ((value #f))
    (let ((next-len (netpkt-next-length pkt)))
      (cond ((eqv? next-len 0) value)
            ((fx<? next-len sizeof-nd_opt_hdr) #f)
            (else
             (let ((type (netpkt-u8-ref pkt offsetof-nd_opt_hdr-nd_opt_type))
                   (len (fx* 8 (netpkt-u8-ref pkt offsetof-nd_opt_hdr-nd_opt_len))))
               (cond ((or (eqv? len 0)
                          (fx<? len (netpkt-next-length pkt)))
                      #f)
                     ((and (fx=? type option-type) (fx>=? len min-len))
                      (let ((value^ (proc pkt sizeof-nd_opt_hdr)))
                        (netpkt-consume! pkt len)
                        (lp value^)))
                     (else
                      (netpkt-consume! pkt len)
                      (lp value)))))))))

(define (handle-ipv6-icmp-packet ether pkt vfc payload-length hlim &src &dst)
  ;; RFC 2463
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-icmp6_hdr)
     (netpkt-free! pkt))
    (else
     (let* ((type (netpkt-u8-ref pkt offsetof-icmp6_hdr-icmp6_type))
            (code (netpkt-u8-ref pkt offsetof-icmp6_hdr-icmp6_code))
            (pseudo (ipv6-pseudo-header-native-checksum 0 &src &dst payload-length IPPROTO_ICMPV6))
            (csum (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo pkt 0))))
       (cond
         ((not (eqv? csum 0))
          (netpkt-free! pkt))

         ((eqv? type ICMP6_ECHO_REQUEST)
          ;; Ping
          (netpkt-u8-set! pkt offsetof-icmp6_hdr-icmp6_type ICMP6_ECHO_REPLY)
          (netpkt-u16-set! pkt offsetof-icmphdr-checksum 0)
          (netpkt-u16-native-set! pkt offsetof-icmphdr-checksum
                                  (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo pkt 0)))
          (let-values ([(resp off) (ether-netpkt-allocate ether 0)])
            (netpkt-transplant! resp pkt)
            (ether-tx-ipv6 ether resp off vfc IPPROTO_ICMPV6 DEFAULT-HLIM &dst &src #f)))

         ((eqv? type ND_NEIGHBOR_SOLICIT)
          ;; RFC 4861. Someone has an IPv6 address and wants to know
          ;; the hardware address of the owner. The replacement for
          ;; ARP in IPv4.
          ;; FIXME: See RFC 4861 §7.2.3.
          (cond
            ((or (not (eqv? hlim 255))
                 (not (eqv? code 0))
                 (fx<? (netpkt-next-length pkt) (fx+ sizeof-icmp6_hdr 16)))
             (netpkt-free! pkt))
            (else
             (let ((&target (fx+ (caar (netpkt-bufs pkt)) sizeof-icmp6_hdr)))
               (cond
                 ((not (ether-my-ipv6-address-indir? ether &target))
                  (netpkt-free! pkt))
                 (else
                  (netpkt-consume! pkt (fx+ sizeof-icmp6_hdr 16))
                  (let ((source-lladdr (icmp6-get-option pkt ND_OPT_SOURCE_LINKADDR 8 netpkt-u48-ref)))
                    (let-values ([(resp off) (ether-netpkt-allocate ether (+ sizeof-icmp6_hdr 16 8))])
                      ;; Add an option with our link-layer address
                      (let ((off (fx- off 8)))
                        (netpkt-u8-set! resp (fx+ off offsetof-nd_opt_hdr-nd_opt_type) ND_OPT_TARGET_LINKADDR)
                        (netpkt-u8-set! resp (fx+ off offsetof-nd_opt_hdr-nd_opt_len) 1)
                        (netpkt-u48-set! resp (fx+ off sizeof-nd_opt_hdr) (ether-hw-address ether))
                        (let ((off (fx- off 16)))
                          (netpkt-u128-set/indir! resp off &target)
                          ;; Construct a solicited neighbor advertisement
                          (let ((off (fx- off sizeof-icmp6_hdr)))
                            (netpkt-u8-set! resp (fx+ off offsetof-icmp6_hdr-icmp6_type) ND_NEIGHBOR_ADVERT)
                            (netpkt-u8-set! resp (fx+ off offsetof-icmp6_hdr-icmp6_code) 0)
                            (netpkt-u16-set! resp (fx+ off offsetof-icmp6_hdr-icmp6_cksum) 0)
                            (let ((ND_NA_FLAG_ROUTER    #x80000000)
                                  (ND_NA_FLAG_SOLICITED #x40000000)
                                  (ND_NA_FLAG_OVERRIDE  #x20000000))
                              (netpkt-u32-set! resp (fx+ off offsetof-icmp6_hdr-icmp6_data32)
                                               (fxior ND_NA_FLAG_SOLICITED ND_NA_FLAG_OVERRIDE))
                              (let* ((pseudo (ipv6-pseudo-header-native-checksum 0 &target &src (fx- (netpkt-length resp) off) IPPROTO_ICMPV6))
                                     (cksum (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo resp off))))
                                (netpkt-u16-native-set! resp (fx+ off offsetof-icmp6_hdr-icmp6_cksum) cksum)
                                (ether-tx-ipv6 ether resp off vfc IPPROTO_ICMPV6 255 &target &src source-lladdr)))))))
                    (netpkt-free! pkt))))))))

         ((eqv? type ND_NEIGHBOR_ADVERT)
          ;; FIXME: See RFC 4861 §7.2.5. Particularly the stuff about
          ;; the different states in the neighbor cache.
          (netpkt-consume! pkt sizeof-icmp6_hdr)
          (cond
            ((or (not (eqv? hlim 255))
                 (not (eqv? code 0))
                 (fx<? (netpkt-next-length pkt) 16))
             (netpkt-free! pkt))
            (else
             (let ((target (netpkt-u128-ref/bv pkt 0)))
               (netpkt-consume! pkt 16)
               (let ((target-lladdr (icmp6-get-option pkt ND_OPT_TARGET_LINKADDR 8 netpkt-u48-ref)))
                 (when target-lladdr
                   (ether-neighbor-ipv6-set! ether target target-lladdr)))
               (netpkt-free! pkt)))))

         (else
          (netpkt-free! pkt)))))))

(define (handle-ipv6-packet ether pkt)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-ip6_hdr)
     (netpkt-free! pkt))
    (else
     (let* ((vfc (netpkt-u32-ref pkt offsetof-ip6_hdr-ip6_vfc))
            (version (fxarithmetic-shift-right vfc 28))
            (vfc (fxbit-field vfc 0 28)) ;strip the version
            (payload-length (netpkt-u16-ref pkt offsetof-ip6_hdr-ip6_plen)))
       (cond
         ((or (not (eqv? version 6))
              (fx<? (netpkt-length pkt) (fx+ sizeof-ip6_hdr payload-length)))
          (netpkt-free! pkt))
         (else
          (let* ((nxt (netpkt-u8-ref pkt offsetof-ip6_hdr-ip6_nxt))
                 (hlim (netpkt-u8-ref pkt offsetof-ip6_hdr-ip6_hlim))
                 (buf0 (car (netpkt-bufs pkt)))
                 (&src (fx+ (car buf0) offsetof-ip6_hdr-ip6_src))
                 (&dst (fx+ (car buf0) offsetof-ip6_hdr-ip6_dst)))
            (netpkt-consume! pkt sizeof-ip6_hdr)
            (netpkt-truncate! pkt payload-length)
            (cond
              ((eqv? nxt IPPROTO_UDP)    (handle-ipv6-udp-packet ether pkt vfc payload-length hlim &src &dst))
              ((eqv? nxt IPPROTO_TCP)    (handle-ipv6-tcp-packet ether pkt vfc payload-length hlim &src &dst))
              ((eqv? nxt IPPROTO_ICMPV6) (handle-ipv6-icmp-packet ether pkt vfc payload-length hlim &src &dst))
              (else
               (netpkt-free! pkt))))))))))

;;; IPv4

;; Note that TTL was meant to be time measured in seconds, but in
;; practice it is decremented once per router hop, so it's really a
;; hop limit, and is rightly called hlim in IPv6.

(define (ipv4-prepend-header! pkt off src dst proto TOS TTL option-length id flags fragment-offset)
  (let ((header-length (fx+ 20 option-length))
        (payload-length (fx- (netpkt-length pkt) (fx+ option-length off))))
    (let ((off (fx- off sizeof-iphdr)))
      (netpkt-u8-set! pkt (fx+ off 0) (fxior (fxarithmetic-shift-left 4 4) (fxdiv header-length 4)))
      (netpkt-u8-set! pkt (fx+ off offsetof-iphdr-tos) TOS)
      (netpkt-u16-set! pkt (fx+ off offsetof-iphdr-tot_len) (fx+ header-length payload-length))
      (netpkt-u16-set! pkt (fx+ off offsetof-iphdr-id) id)
      (netpkt-u16-set! pkt (fx+ off offsetof-iphdr-frag_off) (fxior fragment-offset
                                                                    (fxarithmetic-shift-left flags 12)))
      (netpkt-u8-set! pkt (fx+ off offsetof-iphdr-ttl) TTL)
      (netpkt-u8-set! pkt (fx+ off offsetof-iphdr-protocol) proto)
      (netpkt-u16-set! pkt (fx+ off offsetof-iphdr-check) 0)
      (netpkt-u32-set! pkt (fx+ off offsetof-iphdr-saddr) src)
      (netpkt-u32-set! pkt (fx+ off offsetof-iphdr-daddr) dst)
      (netpkt-u16-native-set! pkt (fx+ off offsetof-iphdr-check)
                              (ip-native-checksum-finish
                               (netpkt-ip-native-checksum-part 0 pkt off header-length)))
      off)))

(define (ether-tx-ipv4 ether pkt off src dst proto TOS ttl option-length id)
  ;; TODO: Fragment the packet if it's too large, and support
  ;; rejecting it instead if dont-fragment? is true
  (let ((flags 0)
        (fragment-offset 0))
    (let ((off (ipv4-prepend-header! pkt off src dst proto TOS ttl option-length id
                                     flags fragment-offset))
          (next-hop (ether-ipv4-next-hop ether dst)))
      (cond
        ((not next-hop)
         'no-route)
        ((ether-neighbor-ipv4-ref ether next-hop) =>
         (lambda (target-hwaddr)
           (ether-tx-pkt ether pkt off ETHERTYPE_IP target-hwaddr)
           'ok))
        (else
         ;; No entry in the neighbor table, so send an ARP request instead
         (netpkt-free! pkt)
         (ether-tx-arp ether ARPOP_REQUEST src #f next-hop)
         'no-entry)))))

(define (handle-ipv4-udp-packet ether pkt src dst)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-udphdr)
     (netpkt-free! pkt))
    (else
     (let ((len (netpkt-u16-ref pkt offsetof-udphdr-uh_ulen)))
       (cond
         ((or (fx<? len sizeof-udphdr)
              (fx<? (netpkt-length pkt) len))
          (netpkt-free! pkt))
         (else
          (netpkt-truncate! pkt len)
          (let ((sport (netpkt-u16-ref pkt offsetof-udphdr-uh_sport))
                (dport (netpkt-u16-ref pkt offsetof-udphdr-uh_dport))
                (csum (netpkt-u16-ref pkt offsetof-udphdr-uh_sum)))
            (let* ((pseudo (ip-native-ipv4-pseudo-checksum 0 src dst len IPPROTO_UDP))
                   (checksum (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo pkt 0))))
              (cond
                ((not (or (eqv? 0 checksum) (eqv? 0 csum)))
                 (netpkt-free! pkt))
                (else
                 (cond ((ether-find-listener ether 'udp src sport dst dport)
                        => (lambda (recv-ch)
                             ;; FIXME: Need to pass along IP-layer
                             ;; data in some nice format.
                             (put-message recv-ch pkt)))
                       (else
                        ;; FIXME: send an error
                        (netpkt-free! pkt)))))))))))))

(define (handle-ipv4-tcp-packet ether pkt src dst)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-tcphdr)
     (netpkt-free! pkt))
    (else
     (let* ((pseudo (ip-native-ipv4-pseudo-checksum 0 src dst (netpkt-length pkt) IPPROTO_TCP))
            (checksum (ip-native-checksum-finish (netpkt-ip-native-checksum pseudo pkt 0))))
       (cond
         ((not (eqv? 0 checksum))
          (netpkt-free! pkt))
         (else
          (let ((sport (netpkt-u16-ref pkt offsetof-tcphdr-th_sport))
                (dport (netpkt-u16-ref pkt offsetof-tcphdr-th_dport)))
            (netpkt-netinfo-set! pkt (vector src sport dst dport))
            (cond ((ether-find-listener ether 'tcp src sport dst dport)
                   => (lambda (recv-ch) (put-message recv-ch pkt)))
                  (else
                   ;; FIXME: send an error
                   (netpkt-free! pkt))))))))))

(define (handle-ipv4-icmp-packet ether pkt src dst)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-icmphdr)
     (netpkt-free! pkt))
    (else
     (let ((type (netpkt-u8-ref pkt offsetof-icmphdr-type))
           (code (netpkt-u8-ref pkt offsetof-icmphdr-code)))
       (cond
         ((eqv? type ICMP_ECHO)
          (let ((checksum (ip-native-checksum-finish (netpkt-ip-native-checksum 0 pkt 0))))
            (cond
              ((not (eqv? checksum 0))
               (netpkt-free! pkt))
              (else
               (netpkt-u8-set! pkt offsetof-icmphdr-type ICMP_ECHOREPLY)
               (netpkt-u16-set! pkt offsetof-icmphdr-checksum 0)
               (netpkt-u16-native-set! pkt offsetof-icmphdr-checksum
                                       (ip-native-checksum-finish (netpkt-ip-native-checksum 0 pkt 0)))
               (let-values ([(resp off) (ether-netpkt-allocate ether 0)])
                 (netpkt-transplant! resp pkt)
                 (let ((tos 0) (option-length 0) (id 0))
                   (ether-tx-ipv4 ether resp off dst src IPPROTO_ICMP tos DEFAULT-HLIM option-length id)))))))

         ;; TODO: Handle error messages
         (else
          (netpkt-free! pkt)))))))

(define (handle-ipv4-packet ether pkt)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-iphdr)
     (netpkt-free! pkt))
    (else
     (let* ((ihl/version (netpkt-u8-ref pkt 0))
            (version (fxarithmetic-shift-right ihl/version 4))
            (header-length (fx* 4 (fxand ihl/version #xf)))
            (total-length (netpkt-u16-ref pkt offsetof-iphdr-tot_len))
            (flags/frags (netpkt-u16-ref pkt offsetof-iphdr-frag_off))
            (flags (fxarithmetic-shift-right flags/frags 12))
            (frag-off (fxand #xfff (netpkt-u16-ref pkt offsetof-iphdr-frag_off))))
       (cond
         ((or (not (eqv? version 4))
              (fx<? header-length sizeof-iphdr)
              (fx>? header-length (netpkt-next-length pkt))
              (fx<? (netpkt-length pkt) total-length))
          ;; This is some other internet protocol (or a bad size).
          ;; There's an interesting list of these in RFC 1700. IPv6
          ;; would have been here, but I guess major implementations
          ;; existed that did not check the version field before
          ;; treating it as IPv4.
          (netpkt-free! pkt))
         ((or (not (eqv? 0 frag-off))
              (not (eqv? 0 (fxand flags #b001))))
          ;; FIXME: handle fragmentation.
          (netpkt-free! pkt))
         (else
          ;; TODO: parse the options
          (let ((checksum (ip-native-checksum-finish
                           (netpkt-ip-native-checksum-part 0 pkt 0 header-length))))
            (cond
              ((not (eqv? checksum 0))
               (netpkt-free! pkt))
              (else
               (let ((src (netpkt-u32-ref pkt offsetof-iphdr-saddr))
                     (dst (netpkt-u32-ref pkt offsetof-iphdr-daddr))
                     (prot (netpkt-u8-ref pkt offsetof-iphdr-protocol)))
                 (netpkt-truncate! pkt total-length)
                 (netpkt-consume! pkt header-length)
                 (cond
                   ((eqv? prot IPPROTO_UDP)  (handle-ipv4-udp-packet ether pkt src dst))
                   ((eqv? prot IPPROTO_TCP)  (handle-ipv4-tcp-packet ether pkt src dst))
                   ((eqv? prot IPPROTO_ICMP) (handle-ipv4-icmp-packet ether pkt src dst))
                   (else
                    (netpkt-free! pkt)))))))))))))

;;; ARP

;; ARP supports multiple link types but here
;; we only go for IP over Ethernet.

(define (ether-tx-arp ether op source-ip target-hwaddr target-ip)
  (let-values ([(pkt off) (ether-netpkt-allocate ether sizeof-ether_arp)])
    (let ((off (fx- off sizeof-ether_arp)))
      (netpkt-u16-set! pkt (fx+ off offsetof-ether_arp-arp_hrd) ARPHRD_ETHER)
      (netpkt-u16-set! pkt (fx+ off offsetof-ether_arp-arp_pro) ETHERTYPE_IP)
      (netpkt-u8-set! pkt (fx+ off offsetof-ether_arp-arp_hln) ETH_ALEN)
      (netpkt-u8-set! pkt (fx+ off offsetof-ether_arp-arp_pln) 4)
      (netpkt-u16-set! pkt (fx+ off offsetof-ether_arp-arp_op) op)
      (netpkt-u48-set! pkt (fx+ off offsetof-ether_arp-arp_sha) (ether-hw-address ether))
      (netpkt-u32-set! pkt (fx+ off offsetof-ether_arp-arp_spa) source-ip)
      (netpkt-u48-set! pkt (fx+ off offsetof-ether_arp-arp_tha) (or target-hwaddr 0))
      (netpkt-u32-set! pkt (fx+ off offsetof-ether_arp-arp_tpa) target-ip)
      (ether-tx-pkt ether pkt off ETHERTYPE_ARP (or target-hwaddr #xFFFFFFFFFFFF)))))

(define (handle-arp-packet ether pkt)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-ether_arp)
     (netpkt-free! pkt))
    (else
     (cond
       ((or (not (eqv? (netpkt-u16-ref pkt offsetof-ether_arp-arp_hrd) ARPHRD_ETHER))
            (not (eqv? (netpkt-u16-ref pkt offsetof-ether_arp-arp_pro) ETHERTYPE_IP))
            (not (eqv? (netpkt-u8-ref pkt offsetof-ether_arp-arp_hln) ETH_ALEN))
            (not (eqv? (netpkt-u8-ref pkt offsetof-ether_arp-arp_pln) 4)))
        (netpkt-free! pkt))
       (else
        (let ((merge? #f)
              (source-ip (netpkt-u32-ref pkt offsetof-ether_arp-arp_spa))
              (target-ip (netpkt-u32-ref pkt offsetof-ether_arp-arp_tpa)))
          (cond
            ((ether-neighbor-ipv4-ref ether source-ip) =>
             (lambda (old-hwaddr)
               (let ((sender-hwaddr (netpkt-u48-ref pkt offsetof-ether_arp-arp_sha)))
                 (unless (fx=? sender-hwaddr old-hwaddr)
                   (ether-neighbor-ipv4-set! ether source-ip sender-hwaddr)))
               (set! merge? #t))))
          (cond
            ((not (ether-my-ipv4-address? ether target-ip))
             (netpkt-free! pkt))
            (else
             (let ((source-hwaddr (netpkt-u48-ref pkt offsetof-ether_arp-arp_sha)))
               (when (not merge?)
                 (ether-neighbor-ipv4-set! ether source-ip source-hwaddr))
               (cond
                 ((eqv? (netpkt-u16-ref pkt offsetof-ether_arp-arp_op) ARPOP_REQUEST)
                  (netpkt-free! pkt)
                  (ether-tx-arp ether ARPOP_REPLY target-ip source-hwaddr source-ip))
                 (else
                  (netpkt-free! pkt))))))))))))

;;; Ethernet II

(define (ether-prepend-header! ether pkt off type target-hwaddr)
  (let ((off (fx- off sizeof-ether_header)))
    (netpkt-u48-set! pkt (fx+ off offsetof-ether_header-ether_dhost) target-hwaddr)
    (netpkt-u48-set! pkt (fx+ off offsetof-ether_header-ether_shost) (ether-hw-address ether))
    (netpkt-u16-set! pkt (fx+ off offsetof-ether_header-ether_type) type)
    off))

(define (ether-tx-pkt ether pkt off type target-hwaddr)
  (let ((off (ether-prepend-header! ether pkt off type target-hwaddr)))
    (netpkt-consume! pkt off)           ;remove headroom
    (put-message (iface-tx-ch (ether-iface ether))
                 pkt)))

(define (handle-ethernet-frame ether pkt)
  (cond
    ((fx<? (netpkt-next-length pkt) sizeof-ether_header)
     (netpkt-free! pkt))
    (else
     (let ((dhost (netpkt-u48-ref pkt offsetof-ether_header-ether_dhost))
           (proto (netpkt-u16-ref pkt offsetof-ether_header-ether_type)))
       (cond
         ((not (or (fxbit-set? dhost 40)
                   (fx=? dhost (ether-hw-address ether))))
          ;; Not for us and not a multicast/broadcast destination. See EUI-48.
          ;; (Should ideally be filtered by the hardware).
          (netpkt-free! pkt))
         (else
          ;; Remove the framing and pass it to the upper layers. The
          ;; upper layers expect the packet to start with their own
          ;; protocol header.
          (netpkt-consume! pkt sizeof-ether_header)
          (cond
            ((eqv? proto ETHERTYPE_IPV6) (handle-ipv6-packet ether pkt))
            ((eqv? proto ETHERTYPE_IP)   (handle-ipv4-packet ether pkt))
            ((eqv? proto ETHERTYPE_ARP)  (handle-arp-packet ether pkt))
            (else
             (netpkt-free! pkt)))))))))

;;; Interface for the upper layer

(define (transmit-internet-packet ether pkt-info)
  ;; XXX: The pkt-info contents should have been verified before they
  ;; get here.
  (match pkt-info
    [#('ipv4 src dst proto tos ttl id dont-fragment? opt upper-hdr payload)
     (cond
       ((not (= (bytevector-length opt) 0))
        'options-are-a-todo)
       (else
        (let-values ([(pkt off) (ether-netpkt-allocate ether (fx+ (bytevector-length upper-hdr)
                                                                  (bytevector-length payload)))])
          (let ((off (fx- off (bytevector-length payload))))
            (do ((i 0 (fx+ i 1)))
                ((fx=? i (bytevector-length payload)))
              (netpkt-u8-set! pkt (fx+ off i) (bytevector-u8-ref payload i)))
            (let ((off (fx- off (bytevector-length upper-hdr))))
              (do ((i 0 (fx+ i 1)))
                  ((fx=? i (bytevector-length upper-hdr)))
                (netpkt-u8-set! pkt (fx+ off i) (bytevector-u8-ref upper-hdr i)))
              ;; FIXME: pass through dont-fragment?.
              (ether-tx-ipv4 ether pkt off src dst proto tos ttl (bytevector-length opt) id))))))]

    [#('ipv6 src dst vfc nxt hlim upper-hdr payload)
     (let-values ([(pkt off) (ether-netpkt-allocate ether (fx+ (bytevector-length upper-hdr)
                                                               (bytevector-length payload)))])
       (let ((off (fx- off (bytevector-length payload))))
         (do ((i 0 (fx+ i 1)))
             ((fx=? i (bytevector-length payload)))
           (netpkt-u8-set! pkt (fx+ off i) (bytevector-u8-ref payload i)))
         (let ((off (fx- off (bytevector-length upper-hdr))))
           (do ((i 0 (fx+ i 1)))
               ((fx=? i (bytevector-length upper-hdr)))
             (netpkt-u8-set! pkt (fx+ off i) (bytevector-u8-ref upper-hdr i)))
           (ether-tx-ipv6 ether pkt off vfc nxt hlim src dst #f))))]

    [_ 'bad-match]))

(define (send-ipv4 ether src dst proto tos ttl id dont-fragment? opt header payload)
  (assert (and (bytevector? header) (bytevector? payload)
               (fx<? (fx+ (bytevector-length header)
                          (bytevector-length payload))
                     65536)))
  (assert (or (ether-my-ipv4-address? ether src)
              (and (eqv? src 0) (eqv? dst #xFFFFFFFF))))
  (assert (and (fx<=? 0 src #xffffffff) (fx<=? 0 dst #xffffffff)))
  (let ((resp-ch (make-channel)))
    (put-message (ether-internet-send-ch ether)
                 `(,resp-ch . #(ipv4 ,src ,dst ,proto ,tos ,ttl ,id ,dont-fragment? ,opt ,header ,payload)))
    (let ((resp (get-message resp-ch)))
      (unless (memq resp '(ok no-entry))
        (error 'send-ipv4 "Send error" src dst proto resp))
      resp)))

(define (send-ipv6 ether src dst vfc nxt hlim header payload)
  (assert (and (bytevector? header) (bytevector? payload)
               (fx<? (fx+ (bytevector-length header)
                          (bytevector-length payload))
                     65536)))
  (assert (ether-my-ipv6-address? ether src))
  (assert (and (fx=? (bytevector-length src) 16) (fx=? (bytevector-length dst) 16)))
  (let ((resp-ch (make-channel)))
    (put-message (ether-internet-send-ch ether)
                 `(,resp-ch . #(ipv6 ,src ,dst ,vfc ,nxt ,hlim ,header ,payload)))
    (let ((resp (get-message resp-ch)))
      (unless (memq resp '(ok no-entry))
        (error 'send-ipv6 "Send error" src dst vfc nxt hlim header resp))
      resp)))

(define (send-udp ether src sport dst dport payload)
  (assert (and (fx<=? 0 sport 65535) (fx<=? 0 dport 65535)))
  (let ((resp-ch (make-channel))
        (hlim DEFAULT-HLIM)
        (dont-fragment? #f)
        (opt #vu8()))
    (let ((hdr (make-bytevector sizeof-udphdr))
          (ulen (fx+ sizeof-udphdr (bytevector-length payload))))
      (bytevector-u16-set! hdr offsetof-udphdr-uh_sport sport (endianness big))
      (bytevector-u16-set! hdr offsetof-udphdr-uh_dport dport (endianness big))
      (bytevector-u16-set! hdr offsetof-udphdr-uh_ulen ulen (endianness big))
      (bytevector-u16-set! hdr offsetof-udphdr-uh_sum 0 (endianness big))
      (let ((csum-upper (netpkt-ip-native-checksum-bytevector
                         (netpkt-ip-native-checksum-bytevector 0 payload) hdr)))
        (cond ((and (ipv4-address? src) (ipv4-address? dst))
               (let ((tos 0)
                     (id 42) ;FIXME: choose a random one automatically
                     (csum (ip-native-checksum-finish
                            (ip-native-ipv4-pseudo-checksum csum-upper src dst ulen IPPROTO_UDP))))
                 (bytevector-u16-native-set! hdr offsetof-udphdr-uh_sum csum)
                 (send-ipv4 ether src dst IPPROTO_UDP tos hlim id dont-fragment? opt hdr payload)))
              ((and (ipv6-address? src) (ipv6-address? dst))
               (let ((vfc 0)
                     (csum (ip-native-checksum-finish
                            (ipv6-pseudo-header-native-checksum/bv csum-upper src dst ulen IPPROTO_UDP))))
                 (bytevector-u16-native-set! hdr offsetof-udphdr-uh_sum csum)
                 (send-ipv6 ether src dst vfc IPPROTO_UDP hlim hdr payload)))
              (else
               (assertion-violation 'send-udp "Bad addresses" ether src sport dst dport)))))))

(define (send-tcp ether src sport dst dport seq ack tcp-flags window-size option-length urgent-pointer payload)
  (assert (and (fixnum? sport) (fx<=? 0 sport 65535)))
  (assert (and (fixnum? dport) (fx<=? 0 dport 65535)))
  (assert (or (ipv4-address? src) (ipv6-address? src)))
  (assert (or (ipv4-address? dst) (ipv6-address? dst)))
  (let ((resp-ch (make-channel))
        (hlim DEFAULT-HLIM)
        (dont-fragment? #f)
        (ip-opt #vu8())
        (data-offset (fxdiv (fx+ sizeof-tcphdr option-length) 4)))
    (let ((hdr (make-bytevector sizeof-tcphdr)))
      (bytevector-u16-set! hdr offsetof-tcphdr-th_sport sport (endianness big))
      (bytevector-u16-set! hdr offsetof-tcphdr-th_dport dport (endianness big))
      (bytevector-u32-set! hdr offsetof-tcphdr-th_seq seq (endianness big))
      (bytevector-u32-set! hdr offsetof-tcphdr-th_ack ack (endianness big))
      (bytevector-u8-set! hdr offsetof-tcphdr-th_off (fxarithmetic-shift-left data-offset 4))
      (bytevector-u8-set! hdr offsetof-tcphdr-th_flags tcp-flags)
      (bytevector-u16-set! hdr offsetof-tcphdr-th_win window-size (endianness big))
      (bytevector-u16-set! hdr offsetof-tcphdr-th_sum 0 (endianness big))
      (bytevector-u16-set! hdr offsetof-tcphdr-th_urp urgent-pointer (endianness big))
      ;; TCP options are included in the payload
      (let ((csum-upper (netpkt-ip-native-checksum-bytevector
                         (netpkt-ip-native-checksum-bytevector 0 payload) hdr))
            (len (fx+ (bytevector-length payload) (bytevector-length hdr))))
        (cond ((and (ipv4-address? src) (ipv4-address? dst))
               (let ((tos 0)
                     (id 42) ;FIXME: choose a random one automatically
                     (csum (ip-native-checksum-finish
                            (ip-native-ipv4-pseudo-checksum csum-upper src dst len IPPROTO_TCP))))
                 (bytevector-u16-native-set! hdr offsetof-tcphdr-th_sum csum)
                 (send-ipv4 ether src dst IPPROTO_TCP tos hlim id dont-fragment? ip-opt hdr payload)))
              ((and (ipv6-address? src) (ipv6-address? dst))
               (let ((vfc 0)
                     (csum (ip-native-checksum-finish
                            (ipv6-pseudo-header-native-checksum/bv csum-upper src dst len IPPROTO_TCP))))
                 (bytevector-u16-native-set! hdr offsetof-tcphdr-th_sum csum)
                 (send-ipv6 ether src dst vfc IPPROTO_TCP hlim hdr payload)))
              (else
               (assertion-violation 'send-tcp "Bad addresses" ether src sport dst dport)))))))

(define (%check-addresses who src sport dst dport)
  (unless (or (eq? sport '*) (and (fixnum? sport) (fx<=? 0 sport 65535)))
    (assertion-violation who "Bad source port" src sport dst dport))
  (unless (and (fixnum? dport) (fx<=? 0 dport 65535))
    (assertion-violation who "Bad destination port" src sport dst dport))
  (when (or (and (ipv4-address? src) (not (or (eq? dst '*) (ipv4-address? dst))))
            (and (ipv6-address? src) (not (or (eq? dst '*) (ipv6-address? dst))))
            (not (and (or (eq? src '*) (ipv4-address? src) (ipv6-address? src))
                      (or (eq? dst '*) (ipv4-address? dst) (ipv6-address? dst)))))
    (assertion-violation who "Bad address combination" src sport dst dport)))

(define (listen-udp ether recv-ch src sport dst dport)
  (%check-addresses 'listen-udp src sport dst dport)
  (assert (channel? recv-ch))
  (let ((resp-ch (make-channel)))
    (put-message (ether-control-ch ether) (list 'listen 'udp resp-ch recv-ch src sport dst dport))
    (let ((resp (get-message resp-ch)))
      (unless (eq? resp 'ok)
        (error 'listen-udp "Listen failed" recv-ch src sport dst dport resp)))))

(define (listen-tcp ether recv-ch src sport dst dport)
  (%check-addresses 'listen-tcp src sport dst dport)
  (assert (channel? recv-ch))
  (let ((resp-ch (make-channel)))
    (put-message (ether-control-ch ether) (list 'listen 'tcp resp-ch recv-ch src sport dst dport))
    (let ((resp (get-message resp-ch)))
      (unless (eq? resp 'ok)
        (error 'listen-tcp "Listen failed" recv-ch src sport dst dport resp)))))

;;; Run Internet on an Ethernet interface

;; XXX: This isn't preemption-safe. Both the transmit and receive
;; fibers manipulate the same neighbor cache, etc.

(define (net·ethernet·internet-stack ether)
  ;; Transmit fiber
  (spawn-fiber
   (lambda ()
     (let lp ()
       (match (perform-operation
               (choice-operation
                (wrap-operation (get-operation (ether-internet-send-ch ether))
                                (lambda (x) (cons 'tx x)))
                (wrap-operation (get-operation (ether-control-ch ether))
                                (lambda (x) (cons 'ctrl x)))
                (wrap-operation (wait-operation (ether-stop-cvar ether))
                                (lambda _ 'stop))))
         [('tx . (resp-ch . pkt-info))
          ;; Transmit an IP packet. This is here because we might need
          ;; to do neighbor discovery instead and this fiber does not
          ;; pass on ARP/ND packets. Other designs are possible.
          (put-message resp-ch (transmit-internet-packet ether pkt-info))
          (lp)]
         [('ctrl . ('listen ip-proto (? channel? resp-ch) (? channel? recv-ch) src sport dst dport))
          ;; FIXME: This needs a way to stop listening. Also needs to
          ;; check that there isn't a conflict with an existing
          ;; listener. Listeners with a * component need to be checked
          ;; last. And this flat list is the worst possible structure.
          ;; And there should be some buffering of packets between
          ;; this fiber and the resp-ch, at least something that stops
          ;; this fiber from blocking. (This is lazy coding to get up
          ;; and running faster).
          (ether-listeners-set! ether (cons (list ip-proto src sport dst dport recv-ch)
                                            (ether-listeners ether)))
          (put-message resp-ch 'ok)
          (lp)]
         [('ctrl . ('add-address . (and (address . prefix-len) addr/len)))
          (cond ((ipv4-address? address)
                 (ether-ipv4-addresses-set! ether (cons addr/len (remove addr/len (ether-ipv4-addresses ether)))))
                ((ipv6-address? address)
                 (ether-ipv6-addresses-set! ether (cons addr/len (remove addr/len (ether-ipv6-addresses ether))))))
          (lp)]
         [('ctrl . ('add-router . address))
          (cond ((ipv4-address? address)
                 (ether-ipv4-routers-set! ether (cons address (remv address (ether-ipv4-routers ether)))))
                ((ipv6-address? address)
                 (ether-ipv6-routers-set! ether (cons address (remove address (ether-ipv6-routers ether))))))
          (lp)]
         ['stop #f]))))

  ;; Receive fiber
  (let lp ()
    (match (perform-operation
            (choice-operation
             (wrap-operation (get-operation (iface-rx-ch (ether-iface ether)))
                             (lambda (x) (cons 'rx x)))
             (wrap-operation (wait-operation (ether-stop-cvar ether))
                             (lambda _ 'stop))))
      [('rx . pkt)
       (handle-ethernet-frame ether pkt)
       (lp)]
      ['stop #f])))

(define (ether-add-address ether address prefix-length)
  (cond ((ipv4-address? address)
         (unless (fx<=? 0 prefix-length 32)
           (assertion-violation 'ether-add-address "Invalid prefix length for IPv4" ether address prefix-length)))
        ((ipv6-address? address)
         (unless (fx<=? 0 prefix-length 128)
           (assertion-violation 'ether-add-address "Invalid prefix length for IPv6" ether address prefix-length)))
        (else
         (assertion-violation 'ether-add-address "Invalid address"
                              ether address prefix-length)))
  (put-message (ether-control-ch ether) (cons 'add-address (cons address prefix-length))))

(define (ether-add-router ether address)
  (assert (or (ipv4-address? address) (ipv6-address? address)))
  (put-message (ether-control-ch ether) (cons 'add-router address)))

(define (ether-add-dns-server ether address)
  (cond ((ipv4-address? address)
         (ether-dns-ipv4-servers-set! ether (cons address (remv address (ether-dns-ipv4-servers ether)))))
        ((ipv6-address? address)
         (ether-dns-ipv6-servers-set! ether (cons address (remove address (ether-dns-ipv6-servers ether)))))
        (else
         (assertion-violation 'ether-add-dns-server "Invalid address" ether address))))

(define (ether-dns-servers ether)
  (append (ether-dns-ipv4-servers ether)
          (map bytevector-copy (ether-dns-ipv6-servers ether))))

)
