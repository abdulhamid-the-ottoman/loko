;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; I/O port transcoders

(library (loko runtime io-tc)
  (export
    transcode-port)
  (import
    (except (rnrs) transcoded-port)
    (rnrs mutable-strings))

(define (make-latin-1-transcoder eol-style error-handling-mode buffer-mode)
  (define (decoder str stri strc buf bufi bufc)
    (let ((stre (fx+ stri strc))
          (bufe (fx+ bufi bufc)))
      (let lp ((stri stri) (bufi bufi))
        (cond
          ((or (fx=? stri stre) (fx=? bufi bufe))
           (values stri bufi))
          (else
           (string-set! str stri (integer->char (bytevector-u8-ref buf bufi)))
           (lp (fx+ stri 1) (fx+ bufi 1)))))))
  (define (encoder p str stri strc)
    (do ((stri stri (fx+ stri 1))
         (c strc (fx- c 1)))
        ((eqv? c 0) strc)
      (let ((c (string-ref str stri)))
        (cond ((eqv? c #\linefeed)
               (case eol-style
                 ((none lf)
                  (put-u8 p (char->integer #\linefeed)))
                 ((crlf)
                  (put-u8 p (char->integer #\return))
                  (put-u8 p (char->integer #\linefeed)))
                 ((cr)
                  (put-u8 p (char->integer #\return)))
                 ((nel)
                  (put-u8 p (char->integer #\x0085)))
                 ((crnel)
                  (put-u8 p (char->integer #\return))
                  (put-u8 p (char->integer #\x0085)))
                 ((ls)                  ;unlucky combination
                  (put-u8 p (char->integer #\?))))
               (when (eq? buffer-mode 'line)
                 (flush-output-port p)))
              (else
               (let ((b (char->integer c)))
                 (cond ((fx<=? b #xff)
                        (put-u8 p b))
                       ((eq? error-handling-mode 'raise)
                        (raise
                          (condition
                           (make-who-condition 'put-char)
                           (make-i/o-encoding-error p c)
                           (make-message-condition "Outside of latin-1")
                           (make-irritants-condition (list p c)))))
                       ((eq? error-handling-mode 'ignore))
                       (else
                        (put-u8 p (char->integer #\?))))))))))
  (values decoder encoder))

(define (make-utf-8-transcoder eol-style error-handling-mode buffer-mode)
  (define (tail? b) (fx=? (fxand b #b11000000) #b10000000))
  (define fxasl fxarithmetic-shift-left)
  (define at-start #t)
  (define (decoder str stri0 strc buf bufi bufc)
    ;; If bufc is zero then we have an eof
    (let ((stre (fx+ stri0 strc))
          (bufe (fx+ bufi bufc)))
      (let lp ((stri stri0) (bufi bufi))
        (define-syntax err
          (lambda (x)
            (syntax-case x ()
              ((_ skip)
               (with-syntax ([(bufi^) (generate-temporaries #'(bufi))])
                 #'(let ((bufi^ (fx+ bufi skip)))
                     (case error-handling-mode
                       ((replace)
                        (string-set! str stri #\xFFFD)
                        (lp (fx+ stri 1) bufi^))
                       ((ignore)
                        (lp stri bufi^))
                       (else
                        (if (fx=? stri stri0)
                            (values 'error bufi^)
                            (values stri bufi))))))))))
        (cond
          ((or (fx=? stri stre) (fx=? bufi bufe))
           (set! at-start #f)
           (values stri bufi))
          (else
           (let ((b (bytevector-u8-ref buf bufi)))
             (cond
               ((fx<? b #x80)
                (let ((c (integer->char b)))
                  (string-set! str stri c)
                  (lp (fx+ stri 1) (fx+ bufi 1))))
               ((fx=? (fxand b #b11100000) #b11000000)
                ;; Two bytes
                (cond ((fx<? (fx+ bufi 1) bufe)
                       (let* ((b2 (bytevector-u8-ref buf (fx+ bufi 1)))
                              (v (fxior (fxasl (fxand #b00011111 b) 6)
                                        (fxand #b00111111 b2))))
                         (cond ((or (fx<? v #x80) (not (tail? b2)))
                                (err 1))
                               (else
                                (let ((c (integer->char v)))
                                  (string-set! str stri c)
                                  (lp (fx+ stri 1) (fx+ bufi 2)))))))
                      (else (values stri bufi))))
               ((fx=? (fxand b #b11110000) #b11100000)
                ;; Three bytes
                (cond ((fx<? (fx+ bufi 2) bufe)
                       (let* ((b2 (bytevector-u8-ref buf (fx+ bufi 1)))
                              (b3 (bytevector-u8-ref buf (fx+ bufi 2)))
                              (v (fxior (fxasl (fxand #b00001111 b) 12)
                                        (fxasl (fxand #b00111111 b2) 6)
                                        (fxand #b00111111 b3))))
                         (cond ((or (fx<? v #x800)
                                    (fx<=? #xD800 v #xDFFF))
                                (err 1))
                               ((not (tail? b2))
                                (err 1))
                               ((not (tail? b3))
                                (err 2))
                               ((and (eqv? v #xFEFF) (eqv? stri 0) at-start) ;skip BOM
                                (lp stri (fx+ bufi 3)))
                               (else
                                (string-set! str stri (integer->char v))
                                (lp (fx+ stri 1) (fx+ bufi 3))))))
                      ((and (fx<? (fx+ bufi 1) bufe)
                            (not (tail? (bytevector-u8-ref buf (fx+ bufi 1)))))
                       ;; Too few bytes, but the next byte doesn't
                       ;; contribute to the encoding.
                       (err 1))
                      (else (values stri bufi))))
               ((fx=? (fxand b #b11111000) #b11110000)
                ;; Four bytes
                (cond ((fx<? (fx+ bufi 3) bufe)
                       (let* ((b2 (bytevector-u8-ref buf (fx+ bufi 1)))
                              (b3 (bytevector-u8-ref buf (fx+ bufi 2)))
                              (b4 (bytevector-u8-ref buf (fx+ bufi 3)))
                              (v (fxior (fxasl (fxand #b00000111 b) 18)
                                        (fxasl (fxand #b00111111 b2) 12)
                                        (fxasl (fxand #b00111111 b3) 6)
                                        (fxand #b00111111 b4))))
                         (cond ((not (fx<=? #x10000 v #x10FFFF))
                                (err 1))
                               ((not (tail? b2))
                                (err 1))
                               ((not (tail? b3))
                                (err 2))
                               ((not (tail? b4))
                                (err 3))
                               (else
                                (string-set! str stri (integer->char v))
                                (lp (fx+ stri 1) (fx+ bufi 4))))))
                      ((fx<? (fx+ bufi 2) bufe)
                       (let* ((b2 (bytevector-u8-ref buf (fx+ bufi 1)))
                              (b3 (bytevector-u8-ref buf (fx+ bufi 2))))
                         (if (tail? b2)
                             (if (tail? b3)
                                 (values stri bufi)
                                 (err 2))
                             (err 1))))
                      ((and (fx<? (fx+ bufi 1) bufe)
                            (not (tail? (bytevector-u8-ref buf (fx+ bufi 1)))))
                       (err 1))
                      (else (values stri bufi))))
               (else                 ;invalid byte
                (err 1)))))))))
  (define-syntax put-utf-8
    (lambda (x)
      (syntax-case x ()
        [(_  p c)
         #'(let ((i (char->integer c)))
             (define-syntax put              ;force inlining
               (lambda (x)
                 (syntax-case x ()
                   ((put len)
                    #'(when (fx>=? len 1)
                        (put-u8 p (fxior (fxarithmetic-shift-right i (fx* 6 (fx- len 1)))
                                         (bytevector-u8-ref #vu8(#x00 #x00 #xC0 #xE0 #xF0) len)))
                        (when (fx>=? len 2)
                          (put-u8 p (fxand (fxior (fxarithmetic-shift-right i (fx* 6 (fx- len 2)))
                                                  #x80)
                                           #xBF))
                          (when (fx>=? len 3)
                            (put-u8 p (fxand (fxior (fxarithmetic-shift-right i (fx* 6 (fx- len 3)))
                                                    #x80)
                                             #xBF))
                            (when (fx>=? len 4)
                              (put-u8 p (fxand (fxior i #x80) #xBF))))))))))
             (cond ((fx<? i #x80) (put-u8 p i))
                   ((fx<? i #x800) (put 2))
                   ((fx<? i #x10000) (put 3))
                   (else (put 4))))])))
  (define (encoder p str stri strc)
    (do ((stri stri (fx+ stri 1))
         (c strc (fx- c 1)))
        ((eqv? c 0) strc)
      (let ((ch (string-ref str stri)))
        (cond ((eqv? ch #\linefeed)
               (case eol-style
                 ((none lf)
                  (put-utf-8 p #\linefeed))
                 ((crlf)
                  (put-utf-8 p #\return)
                  (put-utf-8 p #\linefeed))
                 ((cr)
                  (put-utf-8 p #\return))
                 ((nel)
                  (put-utf-8 p #\x0085))
                 ((crnel)
                  (put-utf-8 p #\return)
                  (put-utf-8 p #\x0085))
                 ((ls)
                  (put-utf-8 p #\x2028)))
               (when (eq? buffer-mode 'line)
                 (flush-output-port p)))
              (else
               (put-utf-8 p ch))))))
  (values decoder encoder))

(define (make-utf-16-transcoder eol-style error-handling-mode buffer-mode)
  (define endian #f)
  (define (decoder str stri0 strc buf bufi bufc)
    (let ((stre (fx+ stri0 strc))
          (bufe (fx+ bufi bufc)))
      (let* ((skip
              (if (or endian (fx<? bufc 2))
                  0
                  (let ((bom (bytevector-u16-ref buf bufi (endianness big))))
                    (cond ((eqv? bom #xFEFF) (set! endian (endianness big)) 2)
                          ((eqv? bom #xFFFE) (set! endian (endianness little)) 2)
                          (else         ;RFC 2718
                           (set! endian (endianness big))
                           0)))))
            (endian endian))
        (let lp ((stri stri0) (bufi (fx+ bufi skip)))
          (cond
            ((or (fx=? stri stre) (fx=? bufi bufe) (fx=? bufi (fx- bufe 1)))
             (values stri bufi))
            (else
             (let ((w0 (bytevector-u16-ref buf bufi endian))
                   (bufi^ (fx+ bufi 2)))
               (cond
                 ((fx<=? #xD800 w0 #xDFFF) ;surrogate pair
                  (cond
                    ((fx<? bufi^ bufe)
                     (values stri bufi))
                    (else
                     (let ((w1 (bytevector-u16-ref buf bufi^ endian)))
                       (cond
                         ((fx<=? #xD800 w1 #xDFFF)
                          (let ((w (fx+ (fxior (fxarithmetic-shift-left (fx- w0 #xD800) 10)
                                               (fxbit-field (fx- w1 #xDC00) 0 10))
                                        #x10000)))
                            (cond ((fx>? w #x10FFFF)
                                   (case error-handling-mode
                                     ((ignore)
                                      (lp stri bufi^))
                                     ((replace)
                                      (string-set! str stri #\xFFFD)
                                      (lp (fx+ stri 1) (fx+ bufi^ 2)))
                                     (else
                                      (if (fx=? stri stri0)
                                          (values 'error bufi^)
                                          (values stri bufi^)))))
                                  (else
                                   (string-set! str stri (integer->char w))
                                   (lp (fx+ stri 1) (fx+ bufi^ 2))))))
                         (else
                          (string-set! str stri #\xFFFD)
                          (lp (fx+ stri 1) bufi^)))))))
                 (else
                  (string-set! str stri (integer->char w0))
                  (lp (fx+ stri 1) bufi^))))))))))
  (define (put-utf-16 p c)
    (unless endian
      ;; Output a BOM (UTF-16BE)
      (put-u8 p #xfe)
      (put-u8 p #xff)
      (set! endian 'big))
    (let ((cp (char->integer c)))
      (cond
        ((fx<? cp #x10000)
         (case endian
           ((big)
            (put-u8 p (fxbit-field cp 8 16))
            (put-u8 p (fxbit-field cp 0 8)))
           (else
            (put-u8 p (fxbit-field cp 0 8))
            (put-u8 p (fxbit-field cp 8 16)))))
        (else
         (let ((cp^ (fx- cp #x10000)))
           (let ((high (fx+ #xD800 (fxarithmetic-shift-right cp^ 10)))
                 (low (fx+ #xDC00 (fxbit-field cp^ 0 10))))
             (case endian
               ((big)
                (put-u8 p (fxbit-field high 8 16))
                (put-u8 p (fxbit-field high 0 8))
                (put-u8 p (fxbit-field low 8 16))
                (put-u8 p (fxbit-field low 0 8)))
               (else
                (put-u8 p (fxbit-field high 0 8))
                (put-u8 p (fxbit-field high 8 16))
                (put-u8 p (fxbit-field low 0 8))
                (put-u8 p (fxbit-field low 8 16))))))))))
  (define (encoder p str stri strc)
    (do ((stri stri (fx+ stri 1))
         (c strc (fx- c 1)))
        ((eqv? c 0) strc)
      (let ((c (string-ref str stri)))
        (cond ((eqv? c #\linefeed)
               (case eol-style
                 ((none lf)
                  (put-utf-16 p #\linefeed))
                 ((crlf)
                  (put-utf-16 p #\return)
                  (put-utf-16 p #\linefeed))
                 ((cr)
                  (put-utf-16 p #\return))
                 ((nel)
                  (put-utf-16 p #\x0085))
                 ((crnel)
                  (put-utf-16 p #\return)
                  (put-utf-16 p #\x0085))
                 ((ls)
                  (put-utf-16 p #\x2028)))
               (when (eq? buffer-mode 'line)
                 (flush-output-port p)))
              (else
               (put-utf-16 p c))))))
  (values decoder encoder))

(define (instantiate-transcoder binp tc)
  (let ((eol-style (transcoder-eol-style tc))
        (error-handling-mode (transcoder-error-handling-mode tc))
        (buffer-mode (and (output-port? binp) (output-port-buffer-mode binp))))
    (let-values ([(decoder encoder)
                  (let ((tc (transcoder-codec tc)))
                    (cond
                      ((eqv? tc (utf-8-codec))
                       (make-utf-8-transcoder eol-style error-handling-mode buffer-mode))
                      ((eqv? tc (latin-1-codec))
                       (make-latin-1-transcoder eol-style error-handling-mode buffer-mode))
                      ((eqv? tc (utf-16-codec))
                       (make-utf-16-transcoder eol-style error-handling-mode buffer-mode))
                      (else
                       (error 'transcoded-port "Not implemented" (transcoder-codec tc)))))])
      (cond
        ((and (input-port? binp) (output-port? binp))
         (values decoder encoder))
        ((input-port? binp)
         (values decoder #f))
        (else
         (values #f encoder))))))

;; Translates any of lf, cr, crlf, nel, crnel and ls to lf. Returns
;; the new end of the string.
(define (translate-linefeeds! str start end prev-ch)
  (let lp ((prev-ch prev-ch) (wi start) (ri start))
    (if (fx=? ri end)
        wi
        (let ((ch (string-ref str ri)))
          (cond
            ((and (eqv? prev-ch #\return) (or (eqv? ch #\linefeed) (eqv? ch #\x85)))
             (lp #f wi (fx+ ri 1)))
            ((or (eqv? ch #\x85) (eqv? ch #\x2028))
             (string-set! str wi #\linefeed)
             (lp ch (fx+ wi 1) (fx+ ri 1)))
            ((eqv? ch #\return)
             (string-set! str wi #\linefeed)
             (lp ch (fx+ wi 1) (fx+ ri 1)))
            (else
             (string-set! str wi ch)
             (lp ch (fx+ wi 1) (fx+ ri 1))))))))

(define (transcode-port id binp tc)
  (let-values ([(decoder encoder) (instantiate-transcoder binp tc)])
    ;; Input buffering. We can't just decode as many characters as
    ;; were requested. The binary port might be connected to a
    ;; terminal and it would be inappropriate to block until the whole
    ;; buffer is filled.
    (define buf #f)
    (define bufi 0)
    (define bufc 0)
    (define prev-ch #f)
    (define (read! str start count)
      (let retry ()
        (when (not buf)
          (set! buf (get-bytevector-some binp))
          (cond ((bytevector? buf)
                 (set! bufi 0)
                 (set! bufc (bytevector-length buf)))
                (else
                 (set! buf #f)
                 (set! bufc 0))))
        (let-values ([(stri bufi^) (decoder str start count buf bufi bufc)])
          ;; Decoder returns the new string and buffer indices
          (set! bufc (fx- bufc (fx- bufi^ bufi)))
          (set! bufi bufi^)
          (cond
            ((eq? stri 'error)
             (raise (make-i/o-decoding-error p))
             (retry))
            (else
             (let* ((stri (if (or (eq? 'none (transcoder-eol-style tc))
                                  (fx=? stri start))
                              stri
                              (let* ((prev-ch^ (string-ref str (fx- stri 1)))
                                     (stri (translate-linefeeds! str start stri prev-ch)))
                                (set! prev-ch prev-ch^)
                                stri)))
                    (decoded (fx- stri start)))
               (cond ((eqv? bufc 0)        ;whole buffer decoded
                      (set! buf #f)
                      decoded)
                     ((eqv? decoded 0)
                      ;; There are undecoded bytes, so we do not really
                      ;; have an eof. Need to get at least one more byte.
                      (let ((b (get-u8 binp)))
                        (cond
                          ((eof-object? b)
                           (set! buf #f)
                           (case (transcoder-error-handling-mode tc)
                             ((ignore) 0)
                             ((replace)
                              (string-set! str start #\xFFFD)
                              1)
                             (else
                              (raise-continuable (make-i/o-decoding-error p))
                              0)))
                          (else
                           (let ((bv (make-bytevector (fx+ bufc 1))))
                             (bytevector-copy! buf bufi bv 0 bufc)
                             (bytevector-u8-set! bv bufc b)
                             (set! buf bv)
                             (set! bufi 0)
                             (set! bufc (fx+ bufc 1))
                             (retry))))))
                     (else
                      decoded))))))))
    (define (write! str start count)
      (let ((n (encoder binp str start count)))
        (flush-output-port binp)
        n))
    (define get-position
      (and (port-has-port-position? binp)
           (lambda ()
             ;; XXX: This is on shakey ground.
             (let ((pos (port-position binp)))
               (if (not buf)
                   pos
                   (fx+ pos (fx- (bytevector-length buf) bufi)))))))
    (define set-position!
      (and (port-has-set-port-position!? binp)
           (lambda (pos)
             (set! prev-ch #f)
             (set! buf #f)
             (set-port-position! binp pos))))
    (define (close)
      (close-port binp))
    (define p
      (cond
        ((and (input-port? binp) (output-port? binp))
         (make-custom-textual-input/output-port id read! write!
                                                get-position set-position! close))
        ((input-port? binp)
         (make-custom-textual-input-port id read!
                                         get-position set-position! close))
        ((output-port? binp)
         (make-custom-textual-output-port id write!
                                          get-position set-position! close))
        (else
         (error 'transcoded-port "Internal error: unrecognized port type"
                binp tc))))
    p)))
