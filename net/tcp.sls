;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; Transmission Control Protocol

;; See RFC 793 for the initial specification complete with military
;; jargon. RFC 1122 is also important reading.

;; https://www.rfc-editor.org/errata/rfc793

;; The design here is not completely fleshed out, it needs some way to
;; give user programs a default tcp instance.

;; TODO: RFC 3514
;; TODO: SYN cookies

(library (loko net tcp)
  (export
    make-tcp
    net·tcp/ip

    tcp-open
    tcp-accept
    tcp-send
    tcp-receive-some
    tcp-receive-n!
    tcp-close
    tcp-abort
    tcp-status

    tcp-conn-input/output-port
    tcp-conn-input-port
    tcp-conn-output-port
    )
  (import
    (rnrs)
    (struct pack)
    (only (loko) record-writer)
    (only (loko runtime time) current-ticks)
    (loko match)
    (loko queues)
    (loko system fibers)
    (loko system logging)
    (loko system random)
    (loko drivers net)
    (loko net internet)
    (loko net numbers))

(define (random-u16)
  (fxior (fxarithmetic-shift-left (get-random-u8) 8)
         (get-random-u8)))

(define (log/debug . x*)
  (send-log DEBUG
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'tcp))

(define (log/error . x*)
  (send-log ERROR
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'tcp))

;; TODO: get the Internet layer to help with the MSS
(define DEFAULT-IPV4-MSS-OUT 536)
(define DEFAULT-IPV6-MSS-OUT 1220)
(define DEFAULT-IPV4-MSS-IN (- 1500 sizeof-iphdr sizeof-tcphdr))
(define DEFAULT-IPV6-MSS-IN (- 1500 sizeof-ip6_hdr sizeof-tcphdr))

(define-record-type tcp
  (sealed #t)
  (fields ether
          ctrl-ch)
  (protocol
   (lambda (p)
     (lambda (ether)
       (p ether (make-channel))))))

;; This maintains the table of TCP connections
(define (net·tcp/ip tcp)
  (let lp ((conns '()))        ;XXX: should be a better data structure
    (match (get-message (tcp-ctrl-ch tcp))
      [('open resp-ch local-addr local-port remote-addr remote-port
              active/passive timeout precedence security/compartment options)
       ;; Look up the connection for the given addresses. If none
       ;; exists then create a new one.
       ;; FIXME: Select a local port number that is unique to the address triple
       (let* ((local-port (if (eq? local-port '*) (random-u16) local-port))
              (addresses (list local-addr local-port remote-addr remote-port)))
         (cond ((find (lambda (conn)
                        (equal? addresses (tcp-conn-addresses conn)))
                      conns)
                => (lambda (conn)
                     (put-message resp-ch conn)
                     (lp conns)))
               (else
                (let ((conn (make-tcp-conn tcp addresses
                                           (make-channel) (make-channel)
                                           (make-channel) (make-cvar))))
                  (spawn-fiber
                   (lambda ()
                     (tcp-tcb conn local-addr local-port remote-addr remote-port active/passive
                              timeout precedence security/compartment options #f)))
                  (put-message resp-ch conn)
                  (lp (cons conn conns))))))]
      [('accept conn local-addr local-port remote-addr remote-port
                timeout precedence security/compartment options
                segment-0)
       (spawn-fiber
        (lambda ()
          (tcp-tcb conn local-addr local-port remote-addr remote-port 'active
                   timeout precedence security/compartment options segment-0)))
       (lp (cons conn conns))]
      [('close conn)
       (lp (remq conn conns))])))

;; A TCP connection
(define-record-type tcp-conn
  (sealed #t)
  (fields tcp                           ;the TCP
          addresses                     ;(local-addr local-port remote-addr remote-port)
          rx-ch                         ;for app to receive data
          tx-ch                         ;for app to transmit data
          listen-ch                     ;for app to listen for connections
          established-cvar))

(define tcp-conn-writer
  (record-writer (record-type-descriptor tcp-conn)
                 (lambda (v p wr)
                   (display "#<tcp-conn " p)
                   (wr (tcp-conn-addresses v) p)
                   (display ">" p))))

;;; User calls

;; Based on the API in RFC 793.

(define-syntax define-optional
  (lambda (x)
    (define (opt-clauses name args* opt*)
      (syntax-case opt* ()
        [() '()]
        [((lhs rhs) (lhs* rhs*) ...)
         (with-syntax ([(args ...) args*])
           #`([(args ...) (#,name args ... rhs)]
              #,@(opt-clauses name #'(args ... lhs) #'((lhs* rhs*) ...))))]))
    (syntax-case x ()
      [(_ (name args ... [(lhs* rhs*) ...])
          . body)
       #`(define name
           (case-lambda
             #,@(opt-clauses #'name #'(args ...) #'((lhs* rhs*) ...))
             [(args ... lhs* ...) . body]))])))

(define-optional (tcp-open tcp local-addr local-port remote-addr remote-port active/passive
                           [(timeout 60) (precedence #f) (security/compartment #f) (options #f)])
  (define (err msg)
    (assertion-violation 'tcp-open msg tcp local-addr local-port remote-addr remote-port
                         active/passive timeout precedence security/compartment options))
  (assert (tcp? tcp))
  (assert (memq active/passive '(active passive)))
  (when (and (eq? active/passive 'active) (or (eq? remote-addr '*) (eq? remote-port '*)))
    (err "The foreign socket is unspecified"))
  (let ((local-addr (if (bytevector? local-addr) (bytevector-copy local-addr) local-addr))
        (remote-addr (if (bytevector? remote-addr) (bytevector-copy remote-addr) remote-addr)))
    (let ((resp-ch (make-channel)))
      (put-message (tcp-ctrl-ch tcp)
                   (list 'open resp-ch local-addr local-port remote-addr remote-port
                         active/passive timeout precedence security/compartment options))
      (let ((conn (get-message resp-ch)))
        (assert (tcp-conn? conn))
        conn))))

;; Queue the given data to be sent on the TCP connection. Returns the
;; number of bytes accepted and a condition variable that is signalled
;; when the remote has acknowledged reception of the data. This
;; differs slightly from the API in RFC 793 in that it blocks until at
;; least one byte fits in the transmit buffers.
(define-optional (tcp-send conn bv start count push? urgent? [(timeout #f)])
  (assert (bytevector? bv))
  (cond
    ((eqv? count 0)
     (error 'tcp-send "TODO: What is the meaning of this?"
            conn bv start count push? urgent?))
    (else
     (let ((end (fx+ start count)))
       (assert (fx<=? 0 start end (bytevector-length bv))))
     (assert (boolean? push?))
     (assert (boolean? urgent?))
     (assert (or (not timeout) (real? timeout)))
     (let ((resp-ch (make-channel))
           (sent-cvar (make-cvar)))
       ;; FIXME: it should be possible to pass through urgent? even if
       ;; the TCP send buffers are full
       (put-message (tcp-conn-tx-ch conn)
                    (list resp-ch sent-cvar bv start count push? urgent? timeout))
       (match (get-message resp-ch)
         [('ok . n)
          (values n sent-cvar)]
         [(? condition? c)
          (raise (condition (make-error)
                            (make-who-condition 'tcp-send)
                            c
                            (make-irritants-condition conn bv start count
                                                      push? urgent? timeout)))])))))

(define (tcp-receive-n! conn bv start count)
  (assert (fx<=? 0 start (fx+ start count) (bytevector-length bv)))
  (let ((resp-ch (make-channel)))
    ;; XXX: Unlike most other uses of put-message, this API assumes
    ;; that the bytevector is shared between the fibers. Which it is,
    ;; for now.
    (put-message (tcp-conn-rx-ch conn) (list resp-ch bv start count))
    (match (get-message resp-ch)
      [(count^ push? urgent?)
       (values count^ push? urgent?)]
      [(? condition? c)
       (raise (condition (make-error)
                         (make-who-condition 'tcp-receive-n!)
                         c
                         (make-irritants-condition (list conn bv start count))))])))

(define (tcp-receive-some conn)
  (let ((resp-ch (make-channel)))
    (put-message (tcp-conn-rx-ch conn) (list resp-ch 'some))
    (match (get-message resp-ch)
      [(bv push? urgent?)
       (values bv push? urgent?)]
      [(? condition? c)
       (raise (condition (make-error)
                         (make-who-condition 'tcp-receive-some)
                         c
                         (make-irritants-condition (list conn))))])))

(define (tcp-close conn)
  (let ((resp-ch (make-channel)))
    (put-message (tcp-conn-tx-ch conn) (list resp-ch 'close))
    (match (get-message resp-ch)
      ['ok (values)]
      [(? condition? c)
       (raise (condition (make-error)
                         (make-who-condition 'tcp-close)
                         c
                         (make-irritants-condition (list conn))))])))

(define (tcp-abort conn)
  (let ((resp-ch (make-channel)))
    (put-message (tcp-conn-tx-ch conn) (list resp-ch 'abort))
    (match (get-message resp-ch)
      ['ok (values)]
      [(? condition? c)
       (raise (condition (make-error)
                         (make-who-condition 'tcp-abort)
                         c
                         (make-irritants-condition (list conn))))])))

(define (tcp-status conn)
  (let ((resp-ch (make-channel)))
    (put-message (tcp-conn-tx-ch conn) (list 'status? resp-ch))
    (get-message resp-ch)))

(define (tcp-accept conn)
  (let ((new-conn (get-message (tcp-conn-listen-ch conn))))
    (values new-conn
            (tcp-conn-addresses new-conn))))

(define (tcp-conn-input/output-port conn addresses)
  (define id "tcp")
  (define (read! bv start count)
    (tcp-receive-n! conn bv start count))
  (define (write! bv start count)
    (let-values ([(n _sent-cvar) (tcp-send conn bv start count #f #f)])
      n))
  (define (close)
    (tcp-close conn))
  (define p
    (make-custom-binary-input/output-port id read! write! #f #f close))
  p)

(define (tcp-conn-input-port conn addresses)
  (define id "tcp")
  (define (read! bv start count)
    (tcp-receive-n! conn bv start count))
  (define (close)
    (tcp-close conn))
  (define p
    (make-custom-binary-input-port id read! #f #f close))
  p)

(define (tcp-conn-output-port conn addresses sync?)
  (define id "tcp")
  (define (write! bv start count)
    (let-values ([(n sent-cvar) (tcp-send conn bv start count #f #f)])
      (when sync?
        (wait sent-cvar))
      n))
  (define (close)
    (tcp-close conn))
  (define p
    (make-custom-binary-output-port id write! #f #f close))
  p)

;;; Segments

;; FIXME: Think about the wrap-around logic here. A lot of this is
;; just wrong.

;; a ≤ b (mod 2³²)
(define (seq<= a b)
  (fx<=? (fxand a #xFFFFFFFF) (fxand b #xFFFFFFFF)))

(define (seq= a b)
  (fx=? (fxand a #xFFFFFFFF) (fxand b #xFFFFFFFF)))

(define seq> fx>?)

(define (seq< a b)
  (fx<? a b))

(define (seq+ a b)
  (fxand (fx+ a b) #xFFFFFFFF))

(define (seq- a b)
  (fxand (fx- a b) #xFFFFFFFF))

(define (seq-max a b)
  (fxmax a b))

(define (make-ISS)
  ;; FIXME: This is wrong.
  (fxior (fxarithmetic-shift-left (random-u16) 16)
         (random-u16))
  ;; FIXME: This is the old school way of doing things, which is too
  ;; predictable. See <https://tools.ietf.org/html/rfc1948>.
  #;(fxand (fxdiv (current-ticks) 4) #xFFFFFFFF))

;; Each packet on a TCP connection carries a segment.
(define-record-type segment
  (fields SEQ                         ;sequence number
          ACK                         ;acknowledgment number
          (mutable LEN)               ;length of non-option data
          WND                         ;window
          UP                          ;urgent pointer
          PRC                         ;precedence value
          CTL                         ;flags
          (mutable option-length)     ;length of option data in buffer
          (mutable buffer)            ;packet or bytevector
          ack-cvar))                  ;cvar for acks in tcp-send

(define (segment-flag? seg flag)
  (not (eqv? 0 (fxand (segment-CTL seg) flag))))

(define (segment-FIN? seg) (segment-flag? seg TH_FIN))
(define (segment-SYN? seg) (segment-flag? seg TH_SYN))
(define (segment-RST? seg) (segment-flag? seg TH_RST))
(define (segment-PSH? seg) (segment-flag? seg TH_PUSH))
(define (segment-ACK? seg) (segment-flag? seg TH_ACK))
(define (segment-URG? seg) (segment-flag? seg TH_URG))

;; Transforms the buffer from a netpkt to a bytevector. This lets us
;; keep the data without later having to care about freeing the
;; netpkt. The option data is removed; it is assumed to already have
;; been processed. XXX: It would be good to revisit this later on
;; because it's just unnecessary copying, assuming there are otherwise
;; enough free DMA buffers.
(define (segment-detach seg)            ;FIXME: bad naming
  (when (netpkt? (segment-buffer seg))
    (let ((len (segment-LEN seg))
          (pkt (segment-buffer seg)))
      (do ((buffer (make-bytevector len))
           (i 0 (fx+ i 1))
           (j (segment-option-length seg) (fx+ j 1)))
          ((fx=? i len)
           (netpkt-free! pkt)
           (segment-option-length-set! seg 0)
           (segment-buffer-set! seg buffer))
        (bytevector-u8-set! buffer i (netpkt-u8-ref pkt j))))))

(define (segment-options->bytevector seg)
  (if (netpkt? (segment-buffer seg))
      (let ((len (segment-option-length seg))
            (pkt (segment-buffer seg)))
        (do ((bv (make-bytevector len))
             (i 0 (fx+ i 1)))
            ((fx=? i len) bv)
          (bytevector-u8-set! bv i (netpkt-u8-ref pkt i))))
      #vu8()))

;; Parse the TCP options. Currently just returns the outgoing MSS (or #f).
(define (segment-options/mss seg)
  (let ((bv (segment-options->bytevector seg)))
    (let lp ((i 0) (mss #f))
      (if (fx=? i (bytevector-length bv))
          mss
          (let ((kind (unpack "C" bv i)))
            (case kind
              ((0) mss)
              ((1) (lp (fx+ i 1) mss))
              (else
               (if (fx<? (fx- (bytevector-length bv) i) (format-size "C"))
                   #f
                   (let ((len (unpack "C" bv (fx+ i 1))))
                     (if (or (fx<? len 2) (fx>? len (fx- (bytevector-length bv) i)))
                         #f
                         (case kind
                           ((2)
                            (and (fx=? len 4)
                                 (let ((MSS (unpack "!uS" bv (fx+ i 2))))
                                   (and (fx>=? MSS 64)
                                        (lp (fx+ i len) MSS)))))
                           (else
                            (lp (fx+ i len) mss)))))))))))))

(define (segment-free! seg)
  (when (netpkt? (segment-buffer seg))
    (netpkt-free! (segment-buffer seg))
    (segment-buffer-set! seg 'invalid)))

(define segment-writer
  (record-writer (record-type-descriptor segment)
                 (lambda (v p wr)
                   (display "#<segment " p)
                   (display (append (if (segment-FIN? v) '(FIN) '())
                                    (if (segment-SYN? v) '(SYN) '())
                                    (if (segment-RST? v) '(RST) '())
                                    (if (segment-PSH? v) '(PSH) '())
                                    (if (segment-ACK? v) '(ACK) '())
                                    (if (segment-URG? v) '(URG) '())
                                    (if (segment-flag? v #x40) '(ECE) '())
                                    (if (segment-flag? v #x80) '(CWR) '())
                                    (if (segment-flag? v #x100) '(NS) '()))
                            p)
                   (display " Seq=" p) (wr (segment-SEQ v) p)
                   (when (segment-flag? v TH_ACK)
                     (display " Ack=" p) (wr (segment-ACK v) p))
                   (display " Win=" p) (wr (segment-WND v) p)
                   (display " Len=" p) (wr (segment-LEN v) p)
                   (when (segment-flag? v TH_URG)
                     (display " Urg=" p) (wr (segment-UP v) p))
                   (unless (eqv? 0 (segment-option-length v))
                     (display " OptLen=" p) (wr (segment-option-length v) p))
                   (display " Buffer=" p)
                   (let ((buffer (segment-buffer v)))
                     (cond ((bytevector? buffer)
                            (wr buffer p))
                           (else
                            (display "#<netpkt Len=" p)
                            (display (netpkt-length buffer) p)
                            (display ">" p))))
                   (display ">" p))))

;;; TCP transmission control block

;; Each TCP connection runs as a fiber that is in this procedure.
;; These events need to be handled: user calls, incoming segments,
;; timeouts.

(define (tcp-tcb conn local-addr local-port remote-addr remote-port
                 active/passive timeout precedence^ security/compartment options
                 segment-0)
  (define security #f)
  (define precedence (or precedence^ 0))
  (define packet-rx-ch (make-channel))  ;packets from the network

  ;; Maximum Segment Size, the largest number of
  (define MSS-IN (if (ipv4-address? local-addr) DEFAULT-IPV4-MSS-IN DEFAULT-IPV6-MSS-IN))
  (define MSS-OUT (if (ipv4-address? local-addr) DEFAULT-IPV4-MSS-OUT DEFAULT-IPV6-MSS-OUT))

  ;; Send Sequence Variables
  (define SND.UNA 0)
  (define SND.NXT 0)
  (define SND.WND 0)
  (define SND.UP 0)
  (define SND.WL1 0)
  (define SND.WL2 0)
  (define ISS (make-ISS))

  ;; Receive Sequence Variables
  (define max-receive-window 65535)
  (define RCV.NXT 0)
  (define RCV.WND max-receive-window)
  (define RCV.UP 0)
  (define IRS 0)
  (define retransmit-queue (make-queue))
  (define text-rx-queue (make-queue))

  ;; Helpers
  (define perform perform-operation)
  (define choice choice-operation)
  (define wrap wrap-operation)

  (define (make-SYN-options)
    (pack "!CCS" 2 4 MSS-IN))

  (define netinfo #f)

  (define (parse-packet pkt)
    ;; What's known: there's a TCP header, the checksum and the
    ;; addresses/ports match. The packet starts with the TCP header.
    (let ((SEQ (netpkt-u32-ref pkt offsetof-tcphdr-th_seq))
          (ACK (netpkt-u32-ref pkt offsetof-tcphdr-th_ack))
          (data-offset (fx* 4 (fxarithmetic-shift-right (netpkt-u8-ref pkt offsetof-tcphdr-th_off) 4)))
          (CTL (fxbit-field (netpkt-u16-ref pkt offsetof-tcphdr-th_off) 0 9))
          (WND (netpkt-u16-ref pkt offsetof-tcphdr-th_win))
          (UP (netpkt-u16-ref pkt offsetof-tcphdr-th_urp))
          (PRC 'FIXME))
      (cond
        ((or (fx<? data-offset sizeof-tcphdr)
             (fx>? data-offset (netpkt-length pkt)))
         (netpkt-free! pkt)
         ;; TODO: What should really be done?
         (let ((LEN 0) (option-length 0))
           (make-segment SEQ ACK LEN WND UP PRC CTL option-length #vu8() #f)))
        (else
         (let* ((option-length (fx- data-offset sizeof-tcphdr))
                (LEN (fx- (netpkt-length pkt) data-offset)))
           (netpkt-consume! pkt sizeof-tcphdr)
           (let ((seg (make-segment SEQ ACK LEN WND UP PRC CTL option-length pkt #f)))
             (log/debug "<= " seg)
             (set! netinfo (netpkt-netinfo pkt))
             seg))))))

  (define (prune-queue!)
    ;; Remove any segments that have been acknowledged according to
    ;; SND.UNA
    (unless (queue-empty? retransmit-queue)
      (let ((seg (queue-front retransmit-queue)))
        (when (seq< (seq+ (segment-SEQ seg) (seq- (segment-LEN seg) 1))
                    SND.UNA)
          (cond ((segment-ack-cvar seg) => signal-cvar!))
          (dequeue! retransmit-queue)
          (prune-queue!)))))

  (define (handle-ACK seg)
    (when (segment-ACK? seg)
      (set! SND.UNA (segment-ACK seg))
      (prune-queue!)))

  (define (send-space-available)
    (fx- SND.WND (seq- SND.NXT SND.UNA)))

  (define (send segment)
    (guard (exn
            ((and (who-condition? exn)
                  (memq (condition-who exn) '(send-ipv4 send-ipv6)))
             #f))
      (log/debug "=> " segment)
      ;; FIXME: This is also used for RST when the remote address/port can be '*
      (when (and (not (eq? local-addr '*)) (fixnum? local-port)
                 (not (eq? remote-addr '*)) (fixnum? remote-port))
        (send-tcp (tcp-ether (tcp-conn-tcp conn))
                  local-addr local-port remote-addr remote-port
                  (segment-SEQ segment)
                  (segment-ACK segment)
                  (segment-CTL segment)
                  (segment-WND segment)
                  (segment-option-length segment)
                  (segment-UP segment)
                  (segment-buffer segment)))
      #t))

  (define (send/enqueue segment)
    (enqueue! retransmit-queue segment)
    (send segment))

  (define (retransmit)
    (unless (queue-empty? retransmit-queue)
      (let ((pkt (queue-front retransmit-queue)))
        (send pkt))))

  (define (enqueue-tx-data resp-ch sent-cvar bv start count push? urgent?)
    ;; The count is adjusted to only accept up to one segment of data,
    ;; and only up to the amount of data the remote is willing to
    ;; receive.
    (let* ((count (fxmin count (send-space-available) MSS-OUT
                         ;; TODO: Ask the Internet layer for help with the MSS
                         DEFAULT-IPV6-MSS-IN))
           (buffer (make-bytevector count)))
      (log/debug "Accepting " count " bytes for tx: " (list count (send-space-available) MSS-OUT))
      (bytevector-copy! bv start buffer 0 count)
      (when urgent?
        (set! SND.UP (seq- SND.NXT 1)))
      (send/enqueue (make-segment SND.NXT RCV.NXT count RCV.WND 0 precedence
                                  (fxior (if push? TH_ACK 0)
                                         (if urgent? TH_URG 0)
                                         TH_ACK)
                                  0
                                  buffer sent-cvar))
      (set! SND.NXT (seq+ SND.NXT count))
      (put-message resp-ch (cons 'ok count))))

  ;; Acceptability test for incoming segments
  (define (incoming-segment-ok? seg)
    (let ((SEG.SEQ (segment-SEQ seg)))
      (if (segment-LEN seg)
          (if (eqv? 0 RCV.WND)
              (seq= SEG.SEQ RCV.NXT)
              (and (seq<= RCV.NXT SEG.SEQ)
                   (seq< SEG.SEQ (+ RCV.NXT RCV.WND))))
          (if (eqv? 0 RCV.WND)
              #F #;
              (and (eqv? 0 (segment-LEN seg) 0) ;provision for valid ACK,URG,RST
                   (or (segment-ACK? seg)
                       (segment-URG? seg)
                       (segment-RST? seg)))
              (or (and (seq<= RCV.NXT SEG.SEQ)
                       (seq< SEG.SEQ (+ RCV.NXT RCV.WND)))
                  (let ((x (+ SEG.SEQ (fx- (segment-LEN seg) 1))))
                    (and (seq<= RCV.NXT x)
                         (seq< x (+ RCV.NXT RCV.WND)))))))))

  (define rx-queue '())

  (define (enqueue-rx-segment segment)
    ;; Segments can arrive out of order, be lost, and be duplicated.
    ;; They should be handled in order. FIXME: This is a bad way to do
    ;; this.
    (set! rx-queue (list-sort (lambda (x y)
                                (seq< (segment-SEQ x) (segment-SEQ y)))
                              (cons segment rx-queue))))

  (define (dequeue-rx-segment)
    (if (null? rx-queue)
        #f
        (let ((seg (car rx-queue)))
          (cond
            ((not (incoming-segment-ok? seg))
             (set! rx-queue (cdr rx-queue))
             (dequeue-rx-segment))
            ((and (seq= (segment-SEQ seg) RCV.NXT)
                  (seq< (segment-SEQ seg) (+ RCV.NXT RCV.WND)))
             (set! rx-queue (cdr rx-queue))
             seg)
            #;
            ((seq< (segment-SEQ seg) RCV.NXT)
             (let ((trimmed ))
               (set! rx-queue (cdr rx-queue))
               (error #f "FIXME: trim everything from outside the window"
                      seg RCV.NXT)
               seg))
            (else #f)))))

  (define close-queued #f)

  ;;; States

  (define state 'CLOSED)

  (define (set-state new-state)
    (unless (eq? state new-state)
      (log/debug "State " state " -> " new-state)
      (set! state new-state)))

  (define (LISTEN)
    (set-state 'LISTEN)
    (match (cond (segment-0
                  ;; Get the segment passed to us from the original
                  ;; passive LISTEN.
                  (let ((seg segment-0))
                    (set! segment-0 #f)
                    (cons 'rx seg)))
                 (else
                  (perform (choice (wrap (get-operation packet-rx-ch)
                                         (lambda (x) (cons 'rx (parse-packet x))))))))
      [('rx . seg)
       (cond
         ((segment-RST? seg)
          (segment-free! seg)
          (LISTEN))

         ((segment-ACK? seg)
          ;; <SEQ=SEG.ACK><CTL=RST>
          (send (make-segment (segment-ACK seg) 0 0 0 0 precedence TH_RST 0 #vu8() #f))
          (segment-free! seg)
          (LISTEN))

         ((segment-SYN? seg)
          (cond
            ((eq? active/passive 'passive)
             (cond ((segment-options/mss seg) =>
                    (lambda (MSS) (set! MSS-OUT MSS))))
             (match netinfo
               [#(src sport dst dport)
                (let ((remote-addr (if (eq? remote-addr '*) src remote-addr))
                      (remote-port (if (eq? remote-port '*) sport remote-port))
                      (local-addr (if (eq? local-addr '*) dst local-addr)))
                  ;; There's an incoming connection request. We let
                  ;; the application know the details and wait for it
                  ;; to (possibly) accept the connection.
                  (log/debug "New connection request: "
                             remote-addr ":" remote-port " -> "
                             local-addr ":" local-port)
                  (let ((new-conn (make-tcp-conn (tcp-conn-tcp conn)
                                                 (list local-addr local-port
                                                       remote-addr remote-port)
                                                 (make-channel) (make-channel)
                                                 (make-channel) (make-cvar))))
                    (match (perform (choice (wrap (put-operation (tcp-conn-listen-ch conn)
                                                                 new-conn)
                                                  (lambda _ 'accept))
                                            (wrap (sleep-operation 5) (lambda _ 'timeout))))
                      ['accept
                       (put-message (tcp-ctrl-ch (tcp-conn-tcp conn))
                                    (list 'accept new-conn local-addr local-port
                                          remote-addr remote-port timeout precedence
                                          security/compartment options seg))
                       (LISTEN)]
                      ['timeout
                       (segment-detach seg)
                       (LISTEN)])))]
               [x (log/error "Invalid netinfo: " x)])
             (LISTEN))
            (else                       ;active
             (set! RCV.NXT (seq+ (segment-SEQ seg) 1))
             (set! IRS (segment-SEQ seg))
             (handle-ACK seg)
             (cond ((segment-options/mss seg) =>
                    (lambda (MSS) (set! MSS-OUT MSS))))
             (segment-free! seg)
             ;; FIXME: enqueue handling of data and control in this
             ;; segment(?)
             (let ((options (make-SYN-options)))
               ;; <SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>
               (send/enqueue (make-segment ISS RCV.NXT 0 RCV.WND 0 precedence
                                           (fxior TH_SYN TH_ACK)
                                           (bytevector-length options)
                                           options #f)))
             (set! SND.UNA ISS)
             (set! SND.NXT (seq+ ISS 1))
             (SYN-RECEIVED))))
         (else
          (segment-free! seg)
          (LISTEN)))]))

  (define (SYN-SENT)
    (set-state 'SYN-SENT)
    (let lp ((timeout 0.2))
      (match (perform (choice (wrap (get-operation packet-rx-ch)
                                    (lambda (x) (cons 'rx (parse-packet x))))
                              (wrap (sleep-operation timeout)
                                    (lambda _ 'timeout))))
        [('rx . seg)
         (cond
           ((and (segment-ACK? seg)
                 (or (seq<= (segment-ACK seg) ISS)
                     (seq> (segment-ACK seg) SND.NXT)))
            (unless (segment-RST? seg)
              ;; <SEQ=SEG.ACK><CTL=RST>
              (send (make-segment (segment-ACK seg) 0 0 0 0 precedence TH_RST 0 #vu8() #f)))
            (segment-free! seg)
            (SYN-SENT))
           ((segment-RST? seg)
            (segment-free! seg)
            (put-message (tcp-conn-rx-ch conn)
                         (make-message-condition "Connection reset"))
            (CLOSED))
           ;; TODO: check security and precedence? :)
           ((segment-SYN? seg)
            (set! RCV.NXT (seq+ (segment-SEQ seg) 1))
            (set! IRS (segment-SEQ seg))
            (handle-ACK seg)
            (cond ((segment-options/mss seg) =>
                   (lambda (MSS) (set! MSS-OUT MSS))))
            (segment-free! seg)
            (cond ((seq> SND.UNA ISS)
                   ;; <SEQ=SND.NXT><ACK=RCV.NXT><CTL=ACK>
                   (send (make-segment SND.NXT RCV.NXT 0 RCV.WND 0 precedence
                                       TH_ACK 0 #vu8() #f))
                   (unless (eqv? 0 (segment-LEN seg))
                     (error 'tcp-tcb "TODO: Handle data in SYN-SENT" seg))
                   ;; FIXME: "If there are other controls or text in
                   ;; the segment then continue processing at the
                   ;; sixth step below where the URG bit is checked,
                   ;; otherwise return."
                   (set! SND.WND (segment-WND seg))
                   (set! SND.WL1 (segment-SEQ seg))
                   (set! SND.WL2 (segment-ACK seg))
                   (signal-cvar! (tcp-conn-established-cvar conn))
                   (ESTABLISHED))
                  (else
                   (let ((options (make-SYN-options)))
                     ;; <SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>
                     (send/enqueue (make-segment ISS RCV.NXT 0 RCV.WND 0 precedence
                                                 (fxior TH_SYN TH_ACK)
                                                 (bytevector-length options)
                                                 options #f)))
                   (SYN-RECEIVED))))
           (else
            (segment-free! seg)
            (SYN-SENT)))]
        ['timeout
         (retransmit)
         (lp (min 5 (* timeout 2)))])))

  (define (SYN-RECEIVED) (set-state 'SYN-RECEIVED) (active))

  (define (ESTABLISHED) (set-state 'ESTABLISHED) (active))

  (define (FIN-WAIT-1) (set-state 'FIN-WAIT-1) (active))

  (define (FIN-WAIT-2) (set-state 'FIN-WAIT-2) (active))

  (define (CLOSE-WAIT) (set-state 'CLOSE-WAIT) (active))

  (define (CLOSING) (set-state 'CLOSING) (active))

  (define (LAST-ACK) (set-state 'LAST-ACK) (active))

  (define (TIME-WAIT) (set-state 'TIME-WAIT) (active))

  (define (active)
    (let lp ((timeout 0.1))
      (match (perform (choice (wrap (get-operation packet-rx-ch)
                                    (lambda (x) (cons 'net-rx (parse-packet x))))
                              (if (not (fxpositive? (send-space-available)))
                                  (choice)
                                  (wrap (get-operation (tcp-conn-tx-ch conn))
                                        (lambda (x) (cons 'tx x))))
                              (if (queue-empty? text-rx-queue)
                                  (choice)
                                  (wrap (get-operation (tcp-conn-rx-ch conn))
                                        (lambda (x) (cons 'user-rx x))))
                              (wrap (sleep-operation timeout)
                                    (lambda _ 'timeout))))
        [('net-rx . seg)
         ;; Step 1, check the sequence number
         (cond
           ((incoming-segment-ok? seg)
            (segment-detach seg)
            (enqueue-rx-segment seg))
           (else
            (segment-free! seg)
            (unless (segment-RST? seg)
              ;; <SEQ=SND.NXT><ACK=RCV.NXT><CTL=ACK>
              (send (make-segment SND.NXT RCV.NXT 0 RCV.WND 0 precedence
                                  TH_ACK 0 #vu8() #f)))))

         ;; Process segments in sequence order
         (let ((seg (dequeue-rx-segment)))
           (cond
             ((not seg)
              (active))

             ;; Step 2, RST
             ((segment-RST? seg)
              (put-message (tcp-conn-rx-ch conn)
                           (make-message-condition "Connection reset"))
              (CLOSED))

             ;; Step 3, security & precedence

             ;; Step 4, SYN
             ((segment-SYN? seg)
              (send (make-segment (segment-ACK seg) 0 0 0 0
                                  precedence TH_RST 0 #vu8() #f))
              (put-message (tcp-conn-rx-ch conn)
                           (make-message-condition "Connection reset"))
              (CLOSED))

             ;; Step 5, ACK
             ((not (segment-ACK? seg))
              (active))

             (else
              (let reprocess ()
                (case state
                  ((SYN-RECEIVED)
                   (cond ((and (seq<= SND.UNA (segment-ACK seg))
                               (seq<=         (segment-ACK seg) SND.NXT))
                          (set! SND.WND (segment-WND seg))
                          (set! SND.WL1 (segment-SEQ seg))
                          (set! SND.WL2 (segment-ACK seg))
                          (signal-cvar! (tcp-conn-established-cvar conn))
                          (set-state 'ESTABLISHED)
                          (reprocess))
                         (else
                          (send (make-segment (segment-ACK seg) 0 0 0 0
                                              precedence TH_RST 0 #vu8() #f))
                          (SYN-RECEIVED))))
                  (else
                   (when (and (seq< SND.UNA (segment-ACK seg))
                              (seq<=        (segment-ACK seg) SND.NXT))
                     (handle-ACK seg))

                   (cond
                     ((seq> (segment-ACK seg) SND.NXT)
                      ;; The ACK is for something not yet sent by us
                      (send (make-segment SND.NXT RCV.NXT 0 RCV.WND 0 precedence
                                          TH_ACK 0 #vu8() #f))
                      (active))
                     (else
                      (when (and (seq< SND.UNA (segment-ACK seg))
                                 (seq<= (segment-ACK seg) SND.NXT))
                        ;; Update the send window
                        (when (or (seq< SND.WL1 (segment-SEQ seg))
                                  (and (seq= SND.WL1 (segment-SEQ seg))
                                       (seq<= SND.WL2 (segment-ACK seg))))
                          (set! SND.WND (segment-WND seg))
                          (set! SND.WL1 (segment-SEQ seg))
                          (set! SND.WL2 (segment-ACK seg))))

                      ;; Step 6, URG
                      (when (segment-URG? seg)
                        (set! RCV.UP (seq-max RCV.UP (segment-UP seg))))

                      ;; Step 7, process the text segment
                      (unless (eqv? (segment-LEN seg) 0)
                        (set! RCV.NXT (seq+ RCV.NXT (segment-LEN seg)))
                        (set! RCV.WND (seq- RCV.WND (segment-LEN seg)))
                        (enqueue! text-rx-queue seg)

                        ;; <SEQ=SND.NXT><ACK=RCV.NXT><CTL=ACK>
                        (send (make-segment SND.NXT RCV.NXT 0 RCV.WND 0 precedence
                                            TH_ACK 0 #vu8() #f)))

                      ;; Step 8, FIN
                      (cond
                        ((segment-FIN? seg)
                         ;; TODO: signal the user "connection closing"
                         (set! RCV.NXT (seq+ RCV.NXT 1))
                         (send (make-segment SND.NXT RCV.NXT 0 RCV.WND 0 precedence
                                             TH_ACK 0 #vu8() #f))
                         (CLOSE-WAIT))
                        (else
                         (active)))))))))))]

        [('tx . (resp-ch sent-cvar bv start count push? urgent? new-timeout))
         (when new-timeout
           'TODO)
         (enqueue-tx-data resp-ch sent-cvar bv start count push? urgent?)
         (lp timeout)]

        [('tx . (resp-ch 'close))
         (case state
           ((SYN-RECEIVED)
            ;; FIXME: Needs to be handled later in ESTABLISHED
            (set! close-queued resp-ch)
            (active))
           ((ESTABLISHED)
            (set! SND.NXT (seq+ SND.NXT 1))
            (send/enqueue (make-segment SND.NXT RCV.NXT 0 RCV.WND 0 precedence
                                        (fxior TH_FIN TH_ACK) 0 #vu8() #f))
            (put-message resp-ch 'ok)
            (FIN-WAIT-1))
           ((FIN-WAIT-1 FIN-WAIT-2)
            (put-message resp-ch 'ok)
            (active))
           ((CLOSE-WAIT)
            (set! SND.NXT (seq+ SND.NXT 1))
            (send/enqueue (make-segment SND.NXT RCV.NXT 0 RCV.WND 0 precedence
                                        (fxior TH_FIN TH_ACK) 0 #vu8() #f))
            (put-message resp-ch 'ok)
            (CLOSING))
           ((CLOSING LAST-ACK TIME-WAIT)
            (put-message resp-ch (make-message-condition "Connection closing"))
            (active)))]

        ;; The user wants to receive some amount of data. Give them a
        ;; segment.
        [('user-rx . (resp-ch 'some))
         (let ((seg (dequeue! text-rx-queue)))
           (set! RCV.WND (seq+ RCV.WND (segment-LEN seg)))
           ;; TODO: Maybe indicate urgent data by saying how much
           ;; non-urgent data is ahead of it.
           (put-message resp-ch
                        (list (segment-buffer seg) (segment-PSH? seg)
                              (seq> RCV.UP (segment-SEQ seg))))
           (lp timeout))]

        ;; The user wants to receive at most *count* bytes directly
        ;; into the given bytevector.
        [('user-rx . (resp-ch bv start count))
         (let ((seg (queue-front text-rx-queue)))
           (let ((n (fxmin (segment-LEN seg) count))
                 (buf (segment-buffer seg)))
             (bytevector-copy! buf (fx- (bytevector-length buf)
                                        (segment-LEN seg))
                               bv start
                               n)
             (set! RCV.WND (seq+ RCV.WND n))
             (segment-LEN-set! seg (fx- (segment-LEN seg) n))
             (when (eqv? (segment-LEN seg) 0)
               (dequeue! text-rx-queue))
             (put-message resp-ch
                          (list n (segment-PSH? seg)
                                (seq> RCV.UP (segment-SEQ seg)))))
           (lp timeout))]

        ['timeout
         (retransmit)
         (lp (min 5 (* timeout 2)))])))

  (define (CLOSED)
    (set-state 'CLOSED)
    (put-message (tcp-ctrl-ch (tcp-conn-tcp conn)) (list 'close conn))
    (set! retransmit-queue (make-queue))
    (match (perform (choice (wrap (put-operation (tcp-conn-rx-ch conn)
                                                 (make-message-condition "Connection closed"))
                                  (lambda _ 'rx))
                            (wrap (get-operation (tcp-conn-tx-ch conn))
                                  (lambda (x) (cons 'tx x)))))
      [('tx . (resp-ch . _))
       (put-message resp-ch (make-message-condition "Connection does not exist"))
       (CLOSED)]
      [_
       (CLOSED)]))

  ;; We get here from the initial CLOSED state
  (case active/passive
    ((active)
     (cond (segment-0
            (listen-tcp (tcp-ether (tcp-conn-tcp conn)) packet-rx-ch
                        remote-addr remote-port local-addr local-port)
            (LISTEN))
           (else
            (let* ((options (make-SYN-options))
                   ;; <SEQ=ISS><CTL=SYN>
                   (initial-syn (make-segment ISS 0 0 SND.WND 0 precedence TH_SYN
                                              (bytevector-length options)
                                              options #f)))
              ;; Instruct the lower layer to send matching packets our way
              (listen-tcp (tcp-ether (tcp-conn-tcp conn)) packet-rx-ch
                          remote-addr remote-port local-addr local-port)
              ;; Send the initial SYN
              (send/enqueue initial-syn)
              (set! SND.UNA ISS)
              (set! SND.NXT (seq+ ISS 1))
              (SYN-SENT)))))
    (else
     (listen-tcp (tcp-ether (tcp-conn-tcp conn)) packet-rx-ch
                 remote-addr remote-port local-addr local-port)
     (LISTEN)))
  (error 'tcp-tcb "State machine returned" conn)))
