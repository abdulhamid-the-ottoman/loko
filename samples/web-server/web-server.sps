#!/usr/bin/env scheme-script
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2021 G. Weinholt
#!r6rs

(import
  (rnrs (6))
  (srfi :106 socket)
  (loko system fibers))

;;; Web server

(define (http-client-handler in out)
  (let ((out (transcoded-port out (make-transcoder (utf-8-codec)
                                                   (eol-style crlf)))))
    (let ((req (get-bytevector-some in)))
      (unless (eof-object? req)
        (let ((content "Loko Scheme"))
          (display "HTTP/1.1 200 OK\n" out)
          (display "Content-Length: " out)
          (display (string-length content) out) ;XXX: should be encoded length
          (newline out)
          (display "Connection: close\n" out)
          (newline out)
          (display content out)
          (flush-output-port out))))))

(define (test-server port)
  (display "Waiting for connections on port ")
  (display port)
  (newline)

  (call-with-socket (make-server-socket port *af-inet6*)
    (lambda (s)
      (let lp ()
        (let ((c (socket-accept s)))
          ;; (display "New client: ")
          ;; (display c)
          ;; (newline)
          (spawn-fiber (lambda ()
                         (guard (exn
                                 ((condition? exn)
                                  (write exn)
                                  (newline)))
                           (http-client-handler (socket-input-port c)
                                                (socket-output-port c)))
                         (socket-close c))))
        (lp)))))

(run-fibers
 (lambda ()
   (test-server "3000")))
