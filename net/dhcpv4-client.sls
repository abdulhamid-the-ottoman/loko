;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Dynamic Host Configuration Protocol for IPv4 (DHCP)

;; RFC 951   BOOTSTRAP PROTOCOL (BOOTP)
;; RFC 2131  Dynamic Host Configuration Protocol
;; RFC 2132  DHCP Options and BOOTP Vendor Extensions
;; https://www.iana.org/assignments/bootp-dhcp-parameters/bootp-dhcp-parameters.xhtml

(library (loko net dhcpv4-client)
  (export
    net·ethernet·dhcpv4-client)
  (import
    (rnrs)
    (struct pack)
    (loko system fibers)
    (loko system logging)
    (loko system time)
    (loko system random)
    (loko drivers net)
    (loko net internet)
    (loko net numbers))

;; Monotonically increasing seconds
(define (now)
  (div (current-ticks) 1000))

(define server-port 67)
(define client-port 68)
(define default-source #x00000000)
(define limited-broadcast #xFFFFFFFF)
(define magic #x63825363)

(define BOOTREQUEST 1)
(define BOOTREPLY   2)

(define DHCPDISCOVER 1)
(define DHCPOFFER    2)
(define DHCPREQUEST  3)
(define DHCPDECLINE  4)
(define DHCPACK      5)
(define DHCPNAK      6)
(define DHCPRELEASE  7)
(define DHCPINFORM   8)

(define OPTION-SUBNET-MASK     1)
(define OPTION-ROUTERS         3)
(define OPTION-DNS-SERVERS     6)
(define OPTION-HOSTNAME        12)
(define OPTION-DOMAIN-NAME     15)
(define OPTION-DEFAULT-IP-TTL  23)
(define OPTION-STATIC-ROUTE    33)
(define OPTION-DEFAULT-IRC     74)

(define OPTION-REQUESTED-IP      50)
(define OPTION-LEASE-TIME        51)
(define OPTION-MESSAGE-TYPE      53)
(define OPTION-SERVER-ID         54)
(define OPTION-PARAMETER-REQUEST 55)
(define OPTION-MESSAGE           56)
(define OPTION-MAX-SIZE          57)
(define OPTION-RENEWAL-TIME      58)
(define OPTION-REBINDING-TIME    59)

(define requested-options
  `(,OPTION-SUBNET-MASK
    ,OPTION-ROUTERS
    ,OPTION-DNS-SERVERS
    ,OPTION-HOSTNAME
    ,OPTION-DOMAIN-NAME
    ,OPTION-DEFAULT-IP-TTL
    ,OPTION-STATIC-ROUTE
    ,OPTION-DEFAULT-IRC))

(define-record-type dhcpv4-message
  (sealed #t)
  (fields op                            ;BOOTP operation
          htype hlen                    ;Hardware type / hwaddr len
          hops                          ;used in relaying
          xid                           ;transaction id
          secs                          ;seconds since client started
          flags                         ;unused flags
          ciaddr                        ;client addr (if ARP works)
          yiaddr                        ;your address
          siaddr                        ;TFTP server
          giaddr                        ;relay agent
          chaddr                        ;client hardware address
          sname                         ;server hostname
          file                          ;TFTP filename
          options))

(define (dhcpv4-message-option-ref msg option parse)
  (cond ((assv option (dhcpv4-message-options msg))
         => (lambda (opt)
              (parse (cdr opt))))
        (else #f)))

(define (parse-u8 bv)
  (and (eqv? (bytevector-length bv) 1)
       (unpack "C" bv)))

(define (parse-u32 bv)
  (and (eqv? (bytevector-length bv) 4)
       (unpack "!L" bv)))

(define (dhcpv4-message-type msg)
  (dhcpv4-message-option-ref msg OPTION-MESSAGE-TYPE parse-u8))

(define (dhcpv4-message-default-router msg)
  (dhcpv4-message-option-ref msg OPTION-ROUTERS
                             (lambda (bv)
                               (and (fx>=? (bytevector-length bv) 4)
                                    (unpack "!L" bv)))))

(define (dhcpv4-message-prefix-length msg)
  ;; The subnet mask is in the old school format 255.255.255.0. We
  ;; need the prefix length (24 in the case above). This code counts
  ;; the number of zeros on the right and subtracts that from 32.
  ;; Giving it something dumb like 255.0.255.0 also returns 24.
  (dhcpv4-message-option-ref msg OPTION-SUBNET-MASK
                             (lambda (bv)
                               (and (eqv? (bytevector-length bv) 4)
                                    (let ((mask (unpack "!L" bv)))
                                      (fx- 32
                                           (fxlength
                                            (fxxor mask #xffffffff))))))))

(define (dhcpv4-message-dns-servers msg)
  (or
    (dhcpv4-message-option-ref msg OPTION-DNS-SERVERS
                               (lambda (bv)
                                 (let ((p (open-bytevector-input-port bv)))
                                   (do ((n (fxmin 4 (fxdiv (bytevector-length bv) 4)))
                                        (i 0 (fx+ i 1))
                                        (servers '() (cons (get-unpack p "!L") servers)))
                                       ((fx=? i n)
                                        (reverse servers))))))
    '()))

;; Convert a DHCPv4 message to a bytevector
(define (dhcpv4-message->bytevector message)
  (call-with-bytevector-output-port
    (lambda (p)
      ;; Fixed header.
      (put-pack p "!CCCCLSSLLLL" (dhcpv4-message-op message)
                (dhcpv4-message-htype message)
                (dhcpv4-message-hlen message)
                (dhcpv4-message-hops message)
                (dhcpv4-message-xid message)
                (dhcpv4-message-secs message)
                (dhcpv4-message-flags message)
                (dhcpv4-message-ciaddr message)
                (dhcpv4-message-yiaddr message)
                (dhcpv4-message-siaddr message)
                (dhcpv4-message-giaddr message))
      ;; Client hardware address
      (let ((addr (make-bytevector (dhcpv4-message-hlen message))))
        (bytevector-uint-set! addr 0 (dhcpv4-message-chaddr message) (endianness big)
                              (dhcpv4-message-hlen message))
        (put-bytevector p addr)
        (put-bytevector p (make-bytevector (fx- 16 (dhcpv4-message-hlen message)) 0)))
      ;; 64 bytes server name
      (let ((sname (string->utf8 (dhcpv4-message-sname message))))
        (put-bytevector p sname 0 (fxmin 64 (bytevector-length sname)))
        (put-bytevector p (make-bytevector (fx- 64 (bytevector-length sname)) 0)))
      ;; 128 bytes filename
      (let ((file (string->utf8 (dhcpv4-message-file message))))
        (put-bytevector p file 0 (fxmin 128 (bytevector-length file)))
        (put-bytevector p (make-bytevector (fx- 128 (bytevector-length file)) 0)))
      ;; Options
      (put-pack p "!L" magic)
      (for-each (lambda (opt)
                  (put-u8 p (car opt))
                  (put-u8 p (bytevector-length (cdr opt)))
                  (put-bytevector p (cdr opt)))
                (dhcpv4-message-options message))
      ;; End of options
      (put-u8 p #xFF))))

;; Convert a (possibly) NUL terminated bytevector to a string.
(define (utf8z->string src)
  (do ((len 0 (fx+ len 1)))
      ((or (fx=? len (bytevector-length src))
           (eqv? (bytevector-u8-ref src len) 0))
       (let ((tmp (make-bytevector len)))
         (bytevector-copy! src 0 tmp 0 (bytevector-length tmp))
         (utf8->string tmp)))))

;; Parse a bytevector as a DHCPv4 message, or return #f if the data is
;; invalid.
(define (bytevector->dhcpv4-message bv)
  (define (get-dhcp-options p)
    (let ((field (get-u8 p)))
      (if (eof-object? field)
          '()
          (let ((len (if (memv field '(0 255)) 0 (get-u8 p))))
            (if (eof-object? len)
                '()
                (let ((data (get-bytevector-n p len)))
                  (if (or (eof-object? data) (fx<? (bytevector-length data) len))
                      '()
                      (cons (cons field data)
                            (get-dhcp-options p)))))))))
  (let ((p (open-bytevector-input-port bv)))
    (cond
      ((fx<? (bytevector-length bv) (+ (format-size "!CCCCLSSLLLL") 16 64 128 4))
       #f)
      (else
       (let-values ([(op htype hlen hops xid secs flags ciaddr yiaddr siaddr giaddr)
                     (get-unpack p "!CCCCLSSLLLL")])
         (cond
           ((not (fx<=? 1 hlen 16))
            #f)
           (else
            (let* ((chaddr (get-bytevector-n p hlen))
                   (pad (get-bytevector-n p (fx- 16 hlen)))
                   (sname (get-bytevector-n p 64))
                   (file (get-bytevector-n p 128)))
              (and (eqv? magic (get-unpack p "!L"))
                   (let ((chaddr (bytevector-uint-ref chaddr 0 (endianness big) hlen))
                         (options (get-dhcp-options p)))
                     ;; TODO: option overload (option 52)
                     (make-dhcpv4-message op
                                          htype hlen hops xid secs flags
                                          ciaddr
                                          yiaddr
                                          siaddr
                                          giaddr
                                          chaddr
                                          (utf8z->string sname)
                                          (utf8z->string file)
                                          options)))))))))))

;; Send a DHCPv4 message (client → server).
(define (dhcpv4-send ether src dst message)
  (send-udp ether src client-port dst server-port
            (dhcpv4-message->bytevector message)))

;; Receive a DHCPv4 message with a timeout in seconds.
(define (dhcpv4-recv recv-ch timeout)
  (perform-operation
   (choice-operation (wrap-operation (get-operation recv-ch)
                                     (lambda (pkt)
                                       (netpkt-consume! pkt sizeof-udphdr)
                                       (let ((bv (netpkt-free->bytevector! pkt)))
                                         (bytevector->dhcpv4-message bv))))
                     (wrap-operation (sleep-operation timeout)
                                     (lambda _ 'timeout)))))

(define (mk-dhcpdiscover xid start-time chaddr)
  (let ((secs (bitwise-and #xffff (- (now) start-time)))
        (hops 0) (flags 0) (sname "") (file ""))
    (make-dhcpv4-message BOOTREQUEST
                         ARPHRD_ETHER ETH_ALEN
                         hops xid (fxand secs #xffff) flags
                         0 0 0 0
                         chaddr
                         sname file
                         `((,OPTION-MESSAGE-TYPE . ,(pack "C" DHCPDISCOVER))
                           (,OPTION-MAX-SIZE . ,(pack "!S" 1500))
                           (,OPTION-PARAMETER-REQUEST . ,(u8-list->bytevector
                                                          requested-options))))))

(define (mk-dhcprequest xid start-time chaddr client-ip server-id bound?)
  (let ((secs (bitwise-and #xffff (- (now) start-time)))
        (hops 0) (flags 0) (sname "") (file ""))
    (make-dhcpv4-message BOOTREQUEST
                         ARPHRD_ETHER ETH_ALEN
                         hops xid (fxand secs #xffff) flags
                         (if bound? client-ip 0) 0 0 0
                         chaddr
                         sname file
                         `((,OPTION-MESSAGE-TYPE . ,(pack "C" DHCPREQUEST))
                           ,@(if server-id
                                 `((,OPTION-SERVER-ID . ,(pack "!L" server-id)))
                                 '())
                           (,OPTION-REQUESTED-IP . ,(pack "!L" client-ip))
                           (,OPTION-MAX-SIZE . ,(pack "!S" 1500))
                           (,OPTION-PARAMETER-REQUEST . ,(u8-list->bytevector
                                                          requested-options))))))

(define (random-u32)
  (let ((bv (make-bytevector 4)))
    (get-random-bytevector-n! bv 0 4)
    (bytevector-u32-native-ref bv 0)))

(define (prefix->string prefix plen)
  (call-with-string-output-port
    (lambda (p)
      (display (fxbit-field prefix 24 32) p)
      (display "." p)
      (display (fxbit-field prefix 16 24) p)
      (display "." p)
      (display (fxbit-field prefix 8 16) p)
      (display "." p)
      (display (fxbit-field prefix 0 8) p)
      (when plen
        (display "/" p)
        (display plen p)))))

(define (print-nak msg)
  (cond ((dhcpv4-message-option-ref msg OPTION-MESSAGE utf8->string)
         => (lambda (x)
              (send-log WARNING (string-append "DHCPNAK: " x))))
        (else (send-log WARNING "DHCPNAK with no explanation"))))

(define (net·ethernet·dhcpv4-client ether)
  (define start-time (now))
  (let ((recv-ch (make-channel)))
    (define (INIT)
      (define xid (random-u32))

      (dhcpv4-send ether default-source limited-broadcast
                   (mk-dhcpdiscover xid start-time (ether-hw-address ether)))

      (let lp ()
        ;; TODO: Exponential backoff
        (let ((msg (dhcpv4-recv recv-ch 1)))
          (cond
            ((not msg)
             ;; Invalid data
             (lp))

            ((eq? msg 'timeout)
             (dhcpv4-send ether default-source limited-broadcast
                          (mk-dhcpdiscover xid start-time (ether-hw-address ether)))
             (lp))

            ((or (not (fx=? BOOTREPLY (dhcpv4-message-op msg)))
                 (not (fx=? xid (dhcpv4-message-xid msg)))
                 (not (eqv? DHCPOFFER (dhcpv4-message-type msg))))
             ;; Not for us
             (lp))

            ((not (dhcpv4-message-option-ref msg OPTION-SERVER-ID parse-u32))
             ;; Sanity check failed
             (lp))

            (else
             ;; It's a DHCPOFFER for us. (The SELECTING state is
             ;; implemented by just selecting the first response).
             (REQUESTING msg))))))

    (define (REQUESTING offer)
      (define xid (random-u32))
      (define server-id (dhcpv4-message-option-ref offer OPTION-SERVER-ID parse-u32))
      (define client-ip (dhcpv4-message-yiaddr offer))

      (dhcpv4-send ether default-source limited-broadcast
                   (mk-dhcprequest xid start-time (ether-hw-address ether)
                                   client-ip server-id #f))

      (let lp ()
        ;; TODO: Exponential backoff
        (let ((msg (dhcpv4-recv recv-ch 1)))
          (cond
            ((not msg)
             ;; Invalid data
             (lp))

            ((eq? msg 'timeout)
             (dhcpv4-send ether default-source limited-broadcast
                          (mk-dhcprequest xid start-time (ether-hw-address ether)
                                          client-ip server-id #f))
             (lp))

            ((or (not (fx=? BOOTREPLY (dhcpv4-message-op msg)))
                 (not (fx=? xid (dhcpv4-message-xid msg))))
             ;; Not for us
             (lp))

            (else
             (let ((type (dhcpv4-message-type msg)))
               (cond
                 ((eqv? type DHCPNAK)
                  ;; The server did not like our DHCPREQUEST
                  (print-nak msg)
                  (INIT))

                 ((not (eqv? type DHCPACK))
                  (REQUESTING))

                 (else
                  ;; The server enjoyed our request.
                  (let ((plen (or (dhcpv4-message-prefix-length msg) 24))
                        (lease-time (dhcpv4-message-option-ref offer OPTION-LEASE-TIME parse-u32)))
                    (send-log INFO
                             (string-append "Bound to " (prefix->string client-ip plen)
                                            " for "
                                            (if lease-time (number->string lease-time) "?")
                                            " seconds"))
                    (ether-add-address ether client-ip plen))
                  (cond ((dhcpv4-message-default-router msg) =>
                         (lambda (router)
                           (send-log INFO (string-append "IPv4 router: " (prefix->string router #f)))
                           (ether-add-router ether router)))
                        (else
                         (send-log WARNING "No IPv4 router")))
                  (let ((dns* (dhcpv4-message-dns-servers offer)))
                    (send-log INFO
                             (call-with-string-output-port
                               (lambda (p)
                                 (display "DNS servers: " p)
                                 (let lp ((servers dns*))
                                   (cond ((null? servers)
                                          (display "none" p))
                                         ((null? (cdr servers))
                                          (display (prefix->string (car servers) #f) p))
                                         (else
                                          (display (prefix->string (car servers) #f) p)
                                          (display ", " p)
                                          (lp (cdr servers))))))))
                    (for-each (lambda (server)
                                (ether-add-dns-server ether server))
                              (reverse dns*)))

                  (BOUND offer server-id (now))))))))))

    (define (BOUND offer server-id ack-time)
      (define lease-time
        (or (dhcpv4-message-option-ref offer OPTION-LEASE-TIME parse-u32) 600))
      (define T1 (or (dhcpv4-message-option-ref offer OPTION-RENEWAL-TIME parse-u32)
                     (round (* #e0.5 lease-time))))
      ;; Eat packets until T1 expires
      (let ((msg (dhcpv4-recv recv-ch (- (+ T1 ack-time) (now)))))
        (cond
          ((not (ether-default-ipv4-source ether))
           ;; We don't have an IP address anymore
           (INIT))

          ((eq? msg 'timeout)
           (send-log DEBUG "T1 expired; renewing")
           (RENEWING offer server-id ack-time))

          (else
           (BOUND offer server-id ack-time)))))

    ;; T1 expired, it's time to ask the server if we still have the
    ;; address.
    (define (RENEWING offer server-id ack-time)
      (define xid (random-u32))
      (define lease-time
        (or (dhcpv4-message-option-ref offer OPTION-LEASE-TIME parse-u32) 600))
      (define T2 (or (dhcpv4-message-option-ref offer OPTION-REBINDING-TIME parse-u32)
                     (round (* #e0.875 lease-time))))
      (define client-ip (dhcpv4-message-yiaddr offer))
      (define my-ip (or (ether-default-ipv4-source ether) 0))

      (dhcpv4-send ether my-ip server-id
                   (mk-dhcprequest xid start-time (ether-hw-address ether)
                                   client-ip #f my-ip))

      (let lp ()
        ;; TODO: exponential backoff
        (let ((msg (dhcpv4-recv recv-ch 1)))
          (cond
            ((not (ether-default-ipv4-source ether))
             ;; We don't have an IP address anymore
             (INIT))

            ((eq? msg 'timeout)
             (cond ((< (+ T2 ack-time) (now))
                    (send-log DEBUG "T2 expired; rebinding")
                    (REBINDING offer (now)))
                   (else
                    (RENEWING offer server-id ack-time))))

            ((or (not (fx=? BOOTREPLY (dhcpv4-message-op msg)))
                 (not (fx=? xid (dhcpv4-message-xid msg))))
             ;; Not for us
             (lp))

            (else
             (let ((type (dhcpv4-message-type msg)))
               (cond
                 ((eqv? type DHCPNAK)
                  ;; The server did not like our DHCPREQUEST
                  (print-nak msg)
                  (INIT))

                 ((eqv? type DHCPACK)
                  ;; The server enjoys our continued cooperation.
                  ;; Replace the OFFER with the ACK so that we use the
                  ;; new lease time.
                  (send-log DEBUG "Lease renewed")
                  (BOUND msg server-id (now)))

                 (else
                  ;; Wrong type; drop
                  (lp)))))))))

    ;; T2 expired and we're getting desperate. Maybe there is another
    ;; server out there that will let us have our current address.
    (define (REBINDING offer rebind-time)
      (define xid (random-u32))
      (define client-ip (dhcpv4-message-yiaddr offer))
      (define my-ip (or (ether-default-ipv4-source ether) 0))

      (dhcpv4-send ether my-ip limited-broadcast
                   (mk-dhcprequest xid start-time (ether-hw-address ether)
                                   client-ip #f my-ip))

      (let lp ()
        ;; TODO: exponential backoff
        (let ((msg (dhcpv4-recv recv-ch 1)))
          (cond
            ((not (ether-default-ipv4-source ether))
             ;; We don't have an IP address anymore
             (INIT))

            ((eq? msg 'timeout)
             (cond ((< (+ rebind-time 60) (now))
                    ;; No response within a minute, give up.
                    ;; TODO: Remove the configuration of the initial DHCPOFFER
                    (send-log ERROR "No DHCPACK arrived in time; network operation stopped")
                    (set! start-time (now))
                    (INIT))
                   (else
                    (REBINDING offer rebind-time))))

            ((or (not (fx=? BOOTREPLY (dhcpv4-message-op msg)))
                 (not (fx=? xid (dhcpv4-message-xid msg))))
             ;; Not for us
             (lp))

            (else
             (let ((type (dhcpv4-message-type msg)))
               (cond
                 ((eqv? type DHCPNAK)
                  ;; A server did not like our DHCPREQUEST
                  (print-nak msg)
                  (set! start-time (now))
                  (INIT))

                 ((eqv? type DHCPACK)
                  ;; By desperately broadcasting our request we have
                  ;; found a server that sent us an ack.
                  (let ((server-id (dhcpv4-message-option-ref offer OPTION-SERVER-ID parse-u32)))
                    (BOUND offer server-id (now))))

                 (else
                  ;; Wrong type; drop
                  (lp)))))))))

    (listen-udp ether recv-ch '* server-port '* client-port)
    (send-log DEBUG "DHCP client started")
    (sleep (/ (mod (get-random-u8) 10) 10))
    (INIT))))
