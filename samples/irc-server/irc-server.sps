#!/usr/bin/env -S loko --program
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt

(import
  (rnrs)
  (chibi match)
  (loko system fibers)
  (irc-protocol format)
  (irc-protocol parse)
  (industria strings)
  (srfi :106 socket)
  (wak fmt))

(define log-ch (make-channel))

(spawn-fiber
 (lambda ()
   (let lp ()
     (apply fmt #t (get-message log-ch))
     (newline)
     (lp))))

(define (log/info . x)
  (put-message log-ch (cons "info: " x)))

;; Get an IRC message from a binary input port. Returns a bytevector
;; or the eof object if there was no data to read. RFC 2812, § 2.3.1,
;; says empty messages are ignored. This returns #vu8() in that case.
;; TODO: would be a good addition to (irc-protocol parse)
(define (get-irc-message in-port)
  (let-values ([(p extract) (open-bytevector-output-port)])
    (let lp ((i 512) (prev #f))
      (unless (eqv? i 0)
        (let ((b (get-u8 in-port)))
          (cond ((or (eof-object? b)
                     (eqv? b (char->integer #\linefeed))
                     (eqv? b (char->integer #\return)))
                 (let ((msg (extract)))
                   (if (and (equal? msg #vu8()) (eof-object? b))
                       b
                       msg)))
                (else
                 (put-u8 p b)
                 (lp (fx- i 1) b))))))))

;; Server

(define hostname "irc.loko")

;; Registry that maintains a connection between targets and (fiber)
;; channels. Clients and IRC channels register themselves here.

(define target-registry-ch (make-channel))
(spawn-fiber
 (lambda ()
   (let ((targets (make-hashtable string-ci-hash string-ci=?)))
     (let lp ()
       (match (get-message target-registry-ch)
         [('register target ch)
          (hashtable-set! targets target ch)
          (lp)]
         [('unregister target)
          (hashtable-delete! targets target)
          (lp)]
         [('send target relayed-message response-ch)
          (cond ((hashtable-ref targets target #f) =>
                 (lambda (target-ch)
                   (spawn-fiber
                    (lambda ()
                      ;; In real IRC servers there would be a fixed
                      ;; size send queue limiting how many of these
                      ;; there can be pending. But the relevant thing
                      ;; to learn here is that if the target client
                      ;; dies then this fiber is garbage collected,
                      ;; and doing it this way means that clients are
                      ;; not blocked by other clients that are slow to
                      ;; receive.
                      (put-message target-ch relayed-message)))
                   (put-message response-ch 'ok)))
                (else
                 (put-message response-ch #f)))
          (lp)])))))

(define (register-target target message-ch)
  (put-message target-registry-ch (list 'register target message-ch)))

(define (unregister-target target)
  (put-message target-registry-ch (list 'unregister target)))

(define (send-to-target source target cmd . args)
  (log/info `(send-to-target ,source ,target ,cmd ,@args))
  (let ((ch (make-channel)))
    ; This sends a message to the target's relay-ch
    (put-message target-registry-ch (list 'send target (list source cmd args) ch))
    (let ((status (get-message ch)))
      ;; 'ok if the target exists, #f is not
      status)))

;; IRC channels

(define (make-irc-channel control-ch channel-name)
  (lambda ()
    (define relay-ch (make-channel))
    (register-target channel-name relay-ch)
    (let lp ((users '()))
      (match (perform-operation (choice-operation
                                 (wrap-operation (get-operation control-ch)
                                                 (lambda (x) (cons 'control x)))
                                 (wrap-operation (get-operation relay-ch)
                                                 (lambda (x) (cons 'relay x)))))
        [('control . ('join source))
         (log/info "join " source)
         (let-values ([(nick user host) (prefix-split source)])
           (for-each (lambda (user)
                       (send-to-target source user 'JOIN channel-name))
                     (cons nick users))
           (lp (cons nick users)))]

        [('control . ('part source message))
         (log/info "part " source)
         (let ((nick (prefix-nick source)))
           (for-each (lambda (user)
                       (send-to-target source user 'PART channel-name message))
                     (cons nick users))
           (lp (remove nick users)))]

        [('control . ('quit source message))
         ;; XXX: actually the QUIT should be distributed to the
         ;; clients in the joined channels, but not once per channel.
         (log/info "quit " source)
         (let ((nick (prefix-nick source)))
           (for-each (lambda (user)
                       (send-to-target source user 'QUIT message))
                     (cons nick users))
           (lp (remove nick users)))]

        [('relay . (source cmd args))
         (log/info "relay " (list source cmd args))
         (let ((nick (prefix-nick source)))
           (for-each (lambda (user)
                       (unless (and (memq cmd '(PRIVMSG NOTICE))
                                    (equal? nick user))
                         (apply send-to-target source user cmd args)))
                     users))
         (lp users)]))))

(define channel-registry-ch (make-channel))
(spawn-fiber
 (lambda ()
   (let ((channels (make-hashtable string-ci-hash string-ci=?)))
     (let lp ()
       (match (get-message channel-registry-ch)
         [('join (? string? source) (? string? channel))
          (unless (hashtable-contains? channels channel)
            (log/info "New channel " channel " created by " source)
            (let ((control-ch (make-channel)))
              (spawn-fiber (make-irc-channel control-ch channel))
              (hashtable-set! channels channel control-ch)))
          (put-message (hashtable-ref channels channel #f)
                       (list 'join source))]

         [('part (? string? source) (? string? channel) message)
          (cond ((hashtable-ref channels channel #f) =>
                 (lambda (ch)
                   (put-message ch (list 'part source message)))))]

         [('quit (? string? source) (? string? channel) message)
          (cond ((hashtable-ref channels channel #f) =>
                 (lambda (ch)
                   (put-message ch (list 'quit source message)))))])
       (lp)))))

(define (join-channel channel source)
  (put-message channel-registry-ch (list 'join source channel)))

(define (part-channel channel source message)
  (put-message channel-registry-ch (list 'part source channel message)))

(define (quit-channel channel source message)
  (put-message channel-registry-ch (list 'quit source channel message)))

;; Main loop, one fiber per client

(define (client-loop sock)
  (define (put-message/timeout ch msg timeout)
    (perform-operation
     (choice-operation
      (wrap-operation (put-operation ch msg) (lambda _ 'put))
      (wrap-operation (sleep-operation timeout) (lambda _ 'timeout)))))

  (let ((in (socket-input-port sock))
        (out (socket-output-port sock))
        (shutdown-cvar (make-cvar)))

    (define *nick* #f)
    (define *user* #f)
    (define *prefix* "x!~y@z")
    (define *realname* #f)
    (define *pong-pending* #f)
    (define *joined-channels* '())

    (let ((relay-ch (make-channel))
          (client-in-ch (make-channel))
          (client-out-ch (make-channel)))
      (define (send prefix cmd . args)
        (case (put-message/timeout client-out-ch (list prefix cmd args) 30)
          ((timeout)
           (signal-cvar! shutdown-cvar))))
      (define (flush)
        ;; The version without a timeout:
        ;; (put-message client-out-ch 'flush)
        (case (put-message/timeout client-out-ch 'flush 30)
          ((timeout)
           (signal-cvar! shutdown-cvar))))
      (define (quit message)
        (when *nick*
          (for-each (lambda (channel)
                      (quit-channel channel *prefix* message))
                    *joined-channels*)))

      ;; Handle shutdown of the client
      (spawn-fiber
       (lambda ()
         (wait shutdown-cvar)
         (when *nick*
           (unregister-target *nick*))
         (socket-close sock)))

      ;; Handle PRIVMSGs from other clients
      (spawn-fiber
       (lambda ()
         (let lp ()
           (match (get-message relay-ch)
             [(source cmd args)
              (apply send source cmd args)
              (flush)
              (lp)]))))

      ;; Handle reading from the client
      (spawn-fiber
       (lambda ()
         (let lp ()
           (let ((msg (get-irc-message in)))
             (put-message client-in-ch msg)
             (unless (eof-object? msg)
               (lp))))))

      ;; Handle writing to the client
      (spawn-fiber
       (lambda ()
         (let lp ()
           (match (get-message client-out-ch)
             [(prefix cmd args)
              (log/info "=> " cmd " "
                        (map (lambda (x) (if (bytevector? x) (utf8->string x) x))
                             args))
              (apply format-message-raw out (utf-8-codec) prefix cmd args)
              (lp)]
             ['flush
              (flush-output-port out)
              (lp)]))))

      (send hostname 'NOTICE "*" "*** Server source available from <https://scheme.fail/>")
      (let lp ()
        (flush)
        (let ((msg (perform-operation (choice-operation
                                       (get-operation client-in-ch)
                                       (wrap-operation (sleep-operation 60)
                                                       (lambda _ 'timeout))
                                       (wrap-operation (wait-operation shutdown-cvar)
                                                       (lambda _ 'shutdown))))))
          (cond
            ((eof-object? msg)
             (signal-cvar! shutdown-cvar)
             (quit "Disconnected"))
            ((eq? msg 'shutdown)
             #f)
            ((eq? msg 'timeout)
             (cond ((not *nick*)
                    (send #f 'ERROR "Registration timeout")
                    (flush)
                    (signal-cvar! shutdown-cvar))
                   ((not *pong-pending*)
                    (send #f 'PING hostname)
                    (set! *pong-pending* #t)
                    (lp))
                   (else
                    (send #f 'ERROR "Closing link: timeout")
                    (flush)
                    (signal-cvar! shutdown-cvar)
                    (quit "Connection timed out"))))
            ((equal? msg #vu8())
             (lp))
            (else
             (let-values ([(server cmd args) (parse-message-bytevector msg)])
               (log/info "<= " server " " cmd " "
                         (map (lambda (x) (if (bytevector? x) (utf8->string x) x))
                              args))
               ;; Handle the minimal amount of commands to make the
               ;; client happy.
               (match (cons server (cons cmd args))
                 [(_ 'PRIVMSG target msg)
                  (let ((target (utf8->string target)))
                    (when *nick*
                      (unless (eq? 'ok (send-to-target *prefix* target 'PRIVMSG target msg))
                        (send hostname 401 *nick* target "No such nick/channel"))))]
                 [(_ 'PING server)
                  (send hostname 'PONG server server)]
                 [(_ 'PONG server . _)
                  (set! *pong-pending* #f)]
                 [(_ 'NICK new-nick)
                  (let ((new-nick (utf8->string new-nick)))
                    (when *nick*
                      (send *prefix* 'NICK new-nick)
                      (set! *prefix* (string-append *nick* "!" *user* "@loko"))
                      (register-target new-nick relay-ch)
                      (unregister-target *nick*))
                    (set! *nick* new-nick))]
                 [(_ 'USER user mode _ realname)
                  (when *nick*
                    (set! *user* (utf8->string user))
                    (set! *prefix* (string-append *nick* "!" *user* "@loko"))
                    (register-target *nick* relay-ch)
                    (send hostname 001 *nick* "Welcome to the IRC sample server"))]
                 [(_ 'JOIN channel)
                  (when *nick*
                    (for-each (lambda (channel)
                                (unless (member channel *joined-channels*)
                                  (set! *joined-channels* (cons channel *joined-channels*))
                                  (join-channel channel *prefix*)))
                              (string-split (utf8->string channel) #\,)))]
                 [(_ 'MODE target mods)
                  #f]
                 [(_ 'QUIT message)
                  (send hostname 'ERROR "Closing link, good bye!")
                  (flush)
                  (signal-cvar! shutdown-cvar)
                  (quit (call-with-bytevector-output-port
                          (lambda (p)
                            (put-u8 p (char->integer #\"))
                            (put-bytevector p message)
                            (put-u8 p (char->integer #\")))))]
                 [(_ (? symbol? cmd) . _)
                  (when *nick*
                    (send hostname 421 *nick* cmd "Unknown command or too few arguments"))]))
             (lp))))))))

(define (server-loop server)
  (log/info "Listening on " server)
  (let lp ()
    (let ((client (socket-accept server)))
      (spawn-fiber (lambda ()
                     (log/info "New client: " (wrt client))
                     (client-loop client))))
    (lp)))

(server-loop (make-server-socket "7000" *af-inet6*))
