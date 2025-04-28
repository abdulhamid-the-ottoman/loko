;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2023 G. Weinholt
#!r6rs

;;; Scheme I/O ports

;; There are tricky dependencies at startup related to ports.

;; Buffering combinations:
;; input/none: read one byte at a time
;; input/block: try to fill the buffer
;; input/line: same as block. tty line discipline provides line buffering.
;; output/none: pass each u8 or bytevector in one go. put-char puts whole char.
;; output/block: try to fill and send the whole buffer
;; output/line: try to fill the buffer until there's a linefeed

;; TODO: make ports friendlier to concurrent access through fibers

(library (loko runtime io)
  (export
    buffer-mode? latin-1-codec utf-8-codec utf-16-codec
    native-eol-style

    make-transcoder native-transcoder
    transcoder-codec transcoder-eol-style
    transcoder-error-handling-mode
    bytevector->string string->bytevector

    eof-object eof-object?

    port? port-transcoder textual-port? binary-port?
    transcoded-port port-has-port-position?
    port-position port-has-set-port-position!?
    set-port-position! close-port
    call-with-port

    input-port? port-eof? open-bytevector-input-port
    open-string-input-port standard-input-port current-input-port
    make-custom-binary-input-port make-custom-textual-input-port
    get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
    get-bytevector-some get-bytevector-all
    get-char lookahead-char get-string-n get-string-n! get-string-all
    get-line
    output-port? flush-output-port output-port-buffer-mode
    open-bytevector-output-port
    call-with-bytevector-output-port open-string-output-port
    call-with-string-output-port standard-output-port
    standard-error-port current-output-port current-error-port
    make-custom-binary-output-port make-custom-textual-output-port
    put-u8 put-bytevector put-char put-string put-datum
    make-custom-binary-input/output-port make-custom-textual-input/output-port
    call-with-input-file call-with-output-file with-input-from-file
    with-output-to-file open-input-file open-output-file
    close-input-port close-output-port read-char peek-char
    write-char newline display write

    make-file-options                   ;for psyntax/expander
    $port-buffer-mode-set!              ;for various open-file-stuff
    $init-standard-ports
    open-output-string
    get-output-string
    port-file-descriptor
    port-file-descriptor-set!
    port-buffer-mode-set!
    port-reader
    port-reader-set!
    port-closed?
    port-id (rename (port-id port-name))
    add-fdes-finalizer!
    call-fd-finalizer
    input-port-open? output-port-open?
    current-output-port*
    current-error-port*
    current-input-port*
    u8-ready?
    char-ready?)
  (import
    (except (rnrs)
            buffer-mode? latin-1-codec utf-8-codec utf-16-codec
            native-eol-style
            make-transcoder native-transcoder
            transcoder-codec transcoder-eol-style
            transcoder-error-handling-mode
            bytevector->string string->bytevector
            eof-object eof-object?
            port? port-transcoder textual-port? binary-port?
            transcoded-port port-has-port-position?
            port-position port-has-set-port-position!?
            set-port-position! close-port
            call-with-port
            input-port? port-eof? open-bytevector-input-port
            open-string-input-port standard-input-port current-input-port
            make-custom-binary-input-port make-custom-textual-input-port
            get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
            get-bytevector-some get-bytevector-all
            get-char lookahead-char get-string-n get-string-n! get-string-all
            get-line get-datum
            output-port? flush-output-port output-port-buffer-mode
            open-bytevector-output-port
            call-with-bytevector-output-port open-string-output-port
            call-with-string-output-port standard-output-port
            standard-error-port current-output-port current-error-port
            make-custom-binary-output-port make-custom-textual-output-port
            put-u8 put-bytevector put-char put-string put-datum
            make-custom-binary-input/output-port
            make-custom-textual-input/output-port
            call-with-input-file call-with-output-file with-input-from-file
            with-output-to-file open-input-file open-output-file
            close-input-port close-output-port read-char peek-char read
            write-char)
    (only (rnrs mutable-strings) string-set!)
    (rnrs mutable-pairs)
    (prefix (rnrs io ports) sys:)
    (prefix (rnrs io simple) sys:)
    (loko runtime io-tc)
    (only (loko runtime arithmetic) $display-number)
    (only (loko runtime symbols) $gensym-generate-names!)
    (only (loko runtime records) record-writer)
    (only (loko system fibers) make-cvar signal-cvar! wait)
    (only (loko) make-parameter parameterize
          string-truncate! bytevector-truncate!)
    (prefix (only (loko) open-output-string get-output-string) sys:)
    (loko system $primitives)
    (loko compiler compat)
    (loko system fibers)
    (only (loko runtime io-printer) open-textual-null-port))

;; Tracing this only works when $debug-put-u8 is used directly.
(define-syntax trace
  (syntax-rules ()
    #;
    ((_ . args)
     (begin
       (for-each display (list . args))
       (newline)))
    ((_ . args) 'dummy)))

;; The `file-options' macro residualizes a call to make-file-options
(define file-options-set (make-enumeration '(no-create no-fail no-truncate)))
(define make-file-options (enum-set-constructor file-options-set))

(define (buffer-mode? obj)
  (and (memq obj '(none line block)) #t))

(define (latin-1-codec) 'latin-1-codec)

(define (utf-8-codec) 'utf-8-codec)

(define (utf-16-codec) 'utf-16-codec)

(define (native-eol-style) 'none)

(define-record-type transcoder
  (fields codec eol-style error-handling-mode)
  (sealed #t)
  (opaque #f)
  (nongenerative transcoder-56083a04-ec39-49a3-9a4e-a1891ed48f72)
  (protocol
   (lambda (p)
     (define make-transcoder
       (case-lambda
         ((c) (make-transcoder c (native-eol-style) (error-handling-mode replace)))
         ((c e) (make-transcoder c e (error-handling-mode replace)))
         ((c e h)
          ;; codec eol-style handling-mode
          (assert (memq c '(utf-8-codec latin-1-codec utf-16-codec)))
          (assert (memq e '(none lf crlf cr nel crnel ls)))
          (assert (memq h '(replace ignore raise)))
          (p c e h))))
     make-transcoder)))

(define %native-transcoder
  (make-transcoder (utf-8-codec)
                   (eol-style none)
                   (error-handling-mode replace)))

(define (native-transcoder)
  %native-transcoder)

(define (bytevector->string bytevector transcoder)
  (get-string-all (open-bytevector-input-port bytevector transcoder)))

(define (string->bytevector string transcoder)
  (let-values ([(p extract) (open-bytevector-output-port transcoder)])
    (put-string p string)
    (flush-output-port p)
    (extract)))

(define (eof-object) (sys:eof-object))

(define (eof-object? x) (sys:eof-object? x))

;; File descriptor finalizers. Used to do something special when an fd
;; is closed.
(define *finalizers* (make-eqv-hashtable))

(define (add-fdes-finalizer! fdes finalizer)
  (hashtable-update! *finalizers* fdes
                     (lambda (old)
                       (cons finalizer old))
                     '()))

(define (call-fd-finalizer fd)
  (cond ((hashtable-ref *finalizers* fd #f)
         => (lambda (finalizer*)
              (hashtable-delete! *finalizers* fd)
              (for-each (lambda (finalizer)
                          (finalizer fd))
                        finalizer*)))))

(define print-dialects
  (make-parameter '()
                  (lambda (new)
                    (assert (for-all symbol? new))
                    new)))

;;; Port data type

(define PORT-DIR-OUTPUT     #b000001)
(define PORT-DIR-INPUT      #b000010)
(define PORT-TYPE-TEXTUAL   #b000100)

(define BUFFER-MODE-MASK    #b011000)
(define BUFFER-MODE-NONE    #b000000)
(define BUFFER-MODE-LINE    #b001000)
(define BUFFER-MODE-BLOCK   #b010000)

(define PORT-FLAG-IN-MEMORY #b100000)

(define (fxtest a b) (not (eqv? 0 (fxand a b))))

(define-syntax define-box-type
  (lambda (x)
    (define (symcat . x*)
      (string->symbol
       (apply string-append
              (map (lambda (x)
                     (let ((x (syntax->datum x)))
                       (if (string? x)
                           x
                           (symbol->string x))))
                   x*))))
    (define (iota n)
      (do ((n n (- n 1))
           (x* '() (cons (- n 1) x*)))
          ((eqv? n 0) x*)))
    (define (mkname prefix name fname suffix)
      (datum->syntax name
                     (symcat prefix (syntax->datum name) "-"
                             (syntax->datum fname) suffix)))
    (syntax-case x (fields mutable protocol)
      ((_ name
          (fields (mutable flagfield) field* ...)
          (protocol prot))
       (identifier? #'name)
       (with-syntax ((make (datum->syntax #'name (symcat "make-" (syntax->datum #'name))))
                     (pred (datum->syntax #'name (symcat (syntax->datum #'name) "?")))
                     ((arg* ...) (generate-temporaries #'(field* ...)))
                     ((idx* ...) (iota (length (syntax->datum #'(field* ...)))))
                     (len (length (syntax->datum #'(field* ...))))
                     (flagref (mkname "" #'name #'flagfield ""))
                     (flagset (mkname "" #'name #'flagfield "-set!")))
         (letrec ((expand-field
                   (lambda (field idx)
                     (syntax-case field (mutable)
                       [(mutable fname)
                        (identifier? #'fname)
                        (with-syntax ((ref (mkname "" #'name #'fname ""))
                                      (set (mkname "" #'name #'fname "-set!"))
                                      ;; Unchecked variants
                                      (uref (mkname "$" #'name #'fname ""))
                                      (uset (mkname "$" #'name #'fname "-set!")))
                          #`(begin
                              (define (ref b)
                                (assert ($box-header-type-eq? ($box-type b) 'name))
                                ($box-ref b #,idx))
                              (define (set b v)
                                (assert ($box-header-type-eq? ($box-type b) 'name))
                                ($box-set! b #,idx v))
                              (define (uref b) ($box-ref b #,idx))
                              (define (uset b v) ($box-set! b #,idx v))))]
                       [fname
                        (identifier? #'fname)
                        (with-syntax ((ref (mkname "" #'name #'fname "")))
                          #`(define (ref b)
                              (assert ($box-header-type-eq? ($box-type b) 'name))
                              ($box-ref b #,idx)))]))))
           #`(begin
               (define make
                 (prot (lambda (flagval arg* ...)
                         ($box ($make-box-header 'name #t flagval len) arg* ...))))
               (define (pred obj)
                 (and ($box? obj)
                      ($box-header-type-eq? ($box-type obj) 'name)))
               (define (flagref b)
                 (assert ($box-header-type-eq? ($box-type b) 'name))
                 ($box-header-value ($box-type b)))
               (define (flagset b v)
                 (assert ($box-header-type-eq? ($box-type b) 'name))
                 ($box-type-set! b ($make-box-header 'name #t v len)))
               #,@(map expand-field #'(field* ...) #'(idx* ...)))))))))

(define-box-type port
  (fields
   ;; Flags bits:
   ;; 0     1 = output port
   ;; 1     1 = input port (combined ports exist)
   ;; 2     1 = textual, 0 = binary
   ;; 3     \
   ;; 4     /  0=none, 1=line, 2=block
   ;; 5     1 = string/bytevector input/output port
   (mutable flags)
   (mutable buffer)
   (mutable buffer-rpos)
   (mutable buffer-rend)
   (mutable buffer-wpos)
   (mutable buffer-wend)
   (mutable nl)                         ;#\newline or #f

   ;; Hooks
   (mutable sink)
   (mutable source)
   (mutable get-positioner)
   (mutable set-positioner)
   (mutable closer)
   (mutable extractor)                  ;for in-memory output ports

   id
   (mutable file-descriptor)            ;fd or #f
   (mutable transcoder)
   (mutable cvar)
   (mutable reader)
   (mutable at-eof))
  (protocol
   (lambda (p)
     (lambda (buffer flags
                     sink source getpos setpos closer extractor
                     id transcoder)
       (assert (string? id))
       (assert (or sink source))
       (let ((buffer-rpos 0)
             (buffer-rend 0)
             (buffer-wpos 0)
             (buffer-wend
              (cond ((not sink)
                     0)
                    ((eqv? (fxand flags BUFFER-MODE-MASK) BUFFER-MODE-NONE)
                     0)
                    ((bytevector? buffer)
                     (bytevector-length buffer))
                    (else (string-length buffer))))
             (file-descriptor #f)
             (cvar #f)
             (nl (if (eqv? (fxand flags BUFFER-MODE-MASK) BUFFER-MODE-LINE)
                     #\newline
                     #f))
             (reader #f)
             (at-eof #f))
         (p flags buffer buffer-rpos buffer-rend buffer-wpos buffer-wend nl
            sink source getpos setpos closer extractor
            id file-descriptor transcoder cvar reader at-eof))))))

(define (port-closed? p)
  (not (port-buffer p)))

(define ($port-buffer-mode p)
  (let ((mode (fxand BUFFER-MODE-MASK (port-flags p))))
    (cond ((eqv? mode BUFFER-MODE-NONE) 'none)
          ((eqv? mode BUFFER-MODE-LINE) 'line)
          (else 'block))))

(define ($port-buffer-mode-set! port mode)
  (assert (not (fxtest (port-flags port) PORT-FLAG-IN-MEMORY)))
  ;; It's the caller's responsibility to check that the mode is a
  ;; valid buffer-mode. The buffer mode can also only be changed once,
  ;; right after the port has been created.
  (port-flags-set! port (fxior (case mode
                                 ((none) BUFFER-MODE-NONE)
                                 ((line) BUFFER-MODE-LINE)
                                 (else BUFFER-MODE-BLOCK))
                               (fxand (fxnot BUFFER-MODE-MASK)
                                      (port-flags port))))
  (case mode
    ((line)
     (port-nl-set! port #\newline))
    ((none)
     ;; Make the buffer only hold one value. This way there's no need
     ;; to check if the buffer mode is none every time the port is
     ;; used.
     (port-buffer-wend-set! port 0)
     (if (string? (port-buffer port))
         (port-buffer-set! port (make-string 1))
         (port-buffer-set! port (make-bytevector 1))))))

(define (port-buffer-mode-set! port mode)
  (unless (port? port)
    (assertion-violation 'port-buffer-mode-set! "Expected a port" port mode))
  (unless (memq mode '(none line block))
    (assertion-violation 'port-buffer-mode-set! "Expected a buffer mode" port mode))
  ($port-buffer-mode-set! port mode))

(define (make-custom-binary-input-port id read! getpos setpos close)
  (make-port ($make-bytevector 4096) (fxior PORT-DIR-INPUT BUFFER-MODE-BLOCK)
             #f read! getpos setpos close #f
             id #f))

(define (make-custom-textual-input-port id read! getpos setpos close)
  (make-port (make-string 512) (fxior PORT-TYPE-TEXTUAL PORT-DIR-INPUT
                                      BUFFER-MODE-BLOCK)
             #f read! getpos setpos close #f
             id #f))

(define (make-custom-binary-output-port id write! getpos setpos close)
  (make-port ($make-bytevector 4096) (fxior PORT-DIR-OUTPUT BUFFER-MODE-BLOCK)
             write! #f getpos setpos close #f
             id #f))

(define (make-custom-textual-output-port id write! getpos setpos close)
  (make-port (make-string 512) (fxior PORT-TYPE-TEXTUAL PORT-DIR-OUTPUT
                                      BUFFER-MODE-BLOCK)
             write! #f getpos setpos close #f
             id #f))

;; input/output ports are unbuffered because the port would otherwise
;; need to be repositioned when going between reading and writing.
;; FIXME: just use separate read/write buffer capacities

(define (make-custom-binary-input/output-port id read! write! getpos setpos close)
  (make-port (make-bytevector 1) (fxior PORT-DIR-INPUT PORT-DIR-OUTPUT
                                        BUFFER-MODE-NONE)
             write! read! getpos setpos close #f
             id #f))

(define (make-custom-textual-input/output-port id read! write! getpos setpos close)
  (make-port (make-string 1) (fxior PORT-TYPE-TEXTUAL
                                    PORT-DIR-INPUT PORT-DIR-OUTPUT
                                    BUFFER-MODE-NONE)
             write! read! getpos setpos close #f
             id #f))

;; Make a new port object that mirrors exactly the original port. The
;; original port is closed. Used by transcoded-port: "transcoded-port
;; closes binary-port in a special way…".
;; FIXME: investigate if this is really needed
(define ($clone/close-port p tc)
  (assert (port? p))
  (assert (port-buffer p))
  (let* ((ret (make-port (port-buffer p)

                         (port-flags p)
                         (port-sink p)
                         (port-source p)
                         (port-get-positioner p)
                         (port-set-positioner p)
                         (port-closer p)
                         (port-extractor p)

                         (port-id p)
                         #f)))
    (port-buffer-rpos-set! ret (port-buffer-rpos p))
    (port-buffer-rend-set! ret (port-buffer-rend p))
    (port-buffer-wpos-set! ret (port-buffer-wpos p))
    (port-file-descriptor-set! ret (port-file-descriptor p))
    (port-cvar-set! ret (port-cvar p))
    ;; Release the memory of the old port
    (port-sink-set! p #f)
    (port-source-set! p #f)
    (port-get-positioner-set! p #f)
    (port-set-positioner-set! p #f)
    (port-closer-set! p #f)
    (port-extractor-set! p #f)
    (port-file-descriptor-set! p #f)
    (port-buffer-set! p #f)
    (port-buffer-rpos-set! p 0)
    (port-buffer-rend-set! p 0)
    (port-buffer-wpos-set! p 0)
    (port-cvar-set! p #f)
    ret))

(define (transcoded-port binp tc)
  (unless (and (binary-port? binp) (port-buffer binp))
    (assertion-violation 'transcoded-port
                         "Expected an open binary port as the first argument" binp tc))
  (unless (transcoder? tc)
    (assertion-violation 'transcoded-port
                         "Expected a transcoder as the second argument" binp tc))
  (let* ((id (port-id binp))
         (binp ($clone/close-port binp tc)))
    (let ((p (transcode-port id binp tc)))
      (port-file-descriptor-set! p (port-file-descriptor binp))
      ($port-buffer-mode-set! p ($port-buffer-mode binp))
      ($port-transcoder-set! p tc)
      p)))

;;; Predicates and general stuff

(define (textual-port? p)
  (unless (port? p)
    (assertion-violation 'textual-port? "Expected a port" p))
  (eqv? (fxand (port-flags p) #b100) #b100))

(define (binary-port? p)
  (unless (port? p)
    (assertion-violation 'binary-port? "Expected a port" p))
  (eqv? (fxand (port-flags p) #b100) 0))

(define (input-port? obj)
  #;(and (port? obj) (eqv? #b10 (fxand (port-flags obj) #b10)))
  (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'port #b10 #b10)))

(define (output-port? obj)
  #;(and (port? obj) (eqv? #b01 (fxand (port-flags obj) #b01)))
  (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'port #b01 #b01)))

(define (binary-input-port? obj)
  (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'port #b110 #b010)))

(define (binary-output-port? obj)
  (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'port #b101 #b001)))

(define (textual-input-port? obj)
  (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'port #b110 #b110)))

(define (textual-output-port? obj)
  (and ($box? obj) ($box-header-type-eq? ($box-type obj) 'port #b101 #b101)))

(define (input-port-open? p)
  (and (port-source p) #t))

(define (output-port-open? p)
  (and (port-sink p) #t))

(define (port-has-port-position? p)
  (and (port-get-positioner p) #t))

(define (port-position p)
  (if (port-has-port-position? p)
      (let ((underlying-pos ((port-get-positioner p))))
        (cond ((not (eqv? 0 (port-buffer-rend p)))
               ;; In input mode the underlying object is at the
               ;; position immediately after the last byte/char in our
               ;; buffer.
               (- underlying-pos (fx- (port-buffer-rend p) (port-buffer-rpos p))))
              (else
               ;; In output mode the underlying object is positioned
               ;; at the immediate beginning of our buffer.
               (+ underlying-pos (port-buffer-wpos p)))))
      (assertion-violation 'port-position
                           "This port does not support port-position" p)))

(define (port-has-set-port-position!? p)
  (and (port-set-positioner p) #t))

(define (set-port-position! p pos)
  (assert (fx>=? pos 0))
  (cond
    ((port-set-positioner p) =>
     (lambda (setpos)
       (when (output-port? p)
         (flush-output-port p))
       (setpos pos)
       ($port-buffer-rpos-set! p 0)
       ($port-buffer-rend-set! p 0)
       ($port-buffer-wpos-set! p 0)
       ($port-at-eof-set! p #f)))
    (else
     (assertion-violation 'set-port-position!
                          "This port does not support set-port-position!" p))))

(define (close-port p)
  ;; TODO: it would be swell if ports with no references to them
  ;; also could close the affected sink/source (which might be a
  ;; file descriptor). The question is if the buffer should be
  ;; flushed in those cases. What language is it that flushes all
  ;; output ports on exit?
  (assert (port? p))
  (when (port-buffer p)
    (when (output-port? p)
      (flush-output-port p))
    (let ((closer (port-closer p)))
      (when closer
        (closer)))
    ($port-sink-set! p #f)
    ($port-source-set! p #f)
    ($port-get-positioner-set! p #f)
    ($port-set-positioner-set! p #f)
    ($port-closer-set! p #f)
    ($port-buffer-set! p #f)
    ($port-buffer-rpos-set! p 0)
    ($port-buffer-rend-set! p 0)
    ($port-buffer-wpos-set! p 0))
  (values))

(define (call-with-port port proc)
  (assert (port? port))
  (let-values ((v (proc port)))
    (close-port port)
    (apply values v)))

;;; Input ports

(define (port-eof? p)
  (assert (input-port? p))
  ;; FIXME: what if error-mode is raise?
  (if (textual-port? p)
      (eof-object? (lookahead-char p))
      (eof-object? (lookahead-u8 p))))

;; open-file-input-port is defined elsewhere.

(define open-bytevector-input-port
  (case-lambda
    ((bv)
     (open-bytevector-input-port bv #f))
    ((bv tc)
     (define pos 0)
     (define (read! buf start count)
       (let ((n (fxmin (fx- (bytevector-length bv) pos)
                       count)))
         (bytevector-copy! bv pos buf start n)
         (set! pos (fx+ n pos))
         n))
     (define (get-position) pos)
     (define (set-position! new-pos) (set! pos new-pos))
     (define (close)
       (set! bv 'closed))
     ;; FIXME: should not need to copy the bv. it's for the position
     ;; stuff to work.
     (unless (bytevector? bv)
       (assertion-violation 'open-bytevector-input-port
                            "Expected a bytevector" bv tc))
     (let ((p (make-custom-binary-input-port
               "*bytevector*" read! get-position set-position! close)))
       (if tc (transcoded-port p tc) p)))))

(define (open-string-input-port str)
  (define pos 0)
  (define (read! buf start count)
    (let* ((str str)
           (n (fxmin (fx- (string-length str) pos)
                     count)))
      (do ((end (fx+ start n))
           (i start (fx+ i 1))
           (j pos (fx+ j 1)))
          ((fx=? i end)
           (set! pos (fx+ n pos))
           n)
        (string-set! buf i (string-ref str j)))))
  (define (get-position) pos)
  (define (set-position! new-pos) (set! pos new-pos))
  (define (close)
    (set! str 'closed))
  (assert (string? str))
  (make-custom-textual-input-port "*string*" read! get-position set-position! close))

;;; Binary input

(define (port-fill-buffer p who)
  ;; Called iff the port input buffer is empty. Ensures that there is
  ;; at least one item in the buffer and then returns the item.
  (let ((b (port-buffer p)))
    (unless (or (string? b) (bytevector? b))
      (assertion-violation who "The input port is closed" p))
    (unless (eqv? 0 (port-buffer-wend p))
      ;; Switch the port from writing to reading
      (flush-output-port p)
      (port-buffer-wend-set! p 0))
    (let* ((source (port-source p))
           (req (if (bytevector? b) (bytevector-length b) (string-length b)))
           (items (source b 0 req)))
      (assert (fx<=? 0 items req))
      (cond ((eqv? items 0)
             (eof-object))
            (else
             ($port-buffer-rpos-set! p (if (memq who '(get-u8 get-char))
                                           1 0))
             ($port-buffer-rend-set! p items)
             (if (bytevector? b)
                 (bytevector-u8-ref b 0)
                 (string-ref b 0)))))))

(define (get-u8 p)
  (unless (binary-input-port? p)
    (assertion-violation 'get-u8 "Expected an open input port" p))
  (let ((r ($port-buffer-rpos p)))
    (cond ((eq? r ($port-buffer-rend p))
           (cond (($port-at-eof p)
                  ($port-at-eof-set! p #f)
                  (eof-object))
                 (else
                  (let ((x (port-fill-buffer p 'get-u8)))
                    ($port-at-eof-set! p #f)
                    x))))
          (else
           ($port-buffer-rpos-set! p (fx+ r 1))
           (bytevector-u8-ref ($port-buffer p) r)))))

(define (lookahead-u8 p)
  (unless (binary-input-port? p)
    (assertion-violation 'lookahead-u8 "Expected an open input port" p))
  (let ((r ($port-buffer-rpos p)))
    (cond ((eq? r ($port-buffer-rend p))
           (if ($port-at-eof p)
               (eof-object)
               (let ((x (port-fill-buffer p 'lookahead-u8)))
                 ($port-at-eof-set! p (eof-object? x))
                 x)))
          (else
           (bytevector-u8-ref ($port-buffer p) r)))))

(define ($input-port-ready? p)
  (let ((r ($port-buffer-rpos p)))
    (cond ((eq? r ($port-buffer-rend p))
           ;; R7RS does not have custom ports, so it should be ok if
           ;; this only works on ports with file descriptors. And then
           ;; not even SRFI-181 (custom ports) has char- & u8-ready?.
           ;;
           ;; A complication is that transcoded ports might have extra
           ;; data buffered, so maybe this isn't foolproof.
           (cond
             ((port-file-descriptor p) =>
              (lambda (fd)
                ;; Trickery. The way that wait-for-readable was
                ;; designed isn't really in line with CML principles,
                ;; so it's not actually composable. This uses a trick
                ;; to have cvar-not-readable be signalled only after
                ;; wait-for-readable has had time to check the fd.
                (let ((cvar-readable (make-cvar))
                      (cvar-not-readable (make-cvar)))
                  (spawn-fiber (lambda ()
                                 ;; TODO: This can probably be
                                 ;; lookahead-{char,u8} if the
                                 ;; concurrent access problem is
                                 ;; fixed.
                                 (wait-for-readable fd)
                                 (signal-cvar! cvar-readable)))
                  (spawn-fiber (lambda ()
                                 (let ((next-step (make-cvar)))
                                   (spawn-fiber (lambda ()
                                                  (signal-cvar! next-step)))
                                   (wait next-step)
                                   (signal-cvar! cvar-not-readable))))
                  (perform-operation
                   (choice-operation (wrap-operation
                                      (wait-operation cvar-readable)
                                      (lambda _ #t))
                                     (wrap-operation
                                      (wait-operation cvar-not-readable)
                                      (lambda _ #f)))))))
             (else #t)))
          (else #t))))

(define u8-ready?
  (case-lambda
    ((p)
     (unless (binary-input-port? p)
       (assertion-violation 'u8-ready? "Expected an open binary input port" p))
     ($input-port-ready? p))
    (() (u8-ready? (current-input-port*)))))

(define (get-bytevector-n p n)
  (unless (fx>=? n 0)
    (assertion-violation 'get-bytevector-n
                         "Expected a non-negative count" p n))
  (unless (binary-input-port? p)
    (assertion-violation 'get-bytevector
                         "Expected a binary input port" p))
  (let ((buf (port-buffer p))
        (rpos (port-buffer-rpos p))
        (rend (port-buffer-rend p)))
    (cond
      ((eqv? n 0) #vu8())
      ((and (eqv? rpos 0) (fx=? rend n) (fx=? (bytevector-length buf) n))
       ;; Steal the whole buffer.
       (port-buffer-set! p ($make-bytevector (bytevector-length buf)))
       (port-buffer-rpos-set! p rend)
       (bytevector-truncate! buf n))
      (($port-at-eof p)
       ($port-at-eof-set! p #f)
       (eof-object))
      (else
       (let ((i (fxmin n (fx- rend rpos)))
             (dst ($make-bytevector n)))
         ;; Take as much as possible from the port buffer
         (bytevector-copy! buf rpos dst 0 i)
         (port-buffer-rpos-set! p (fx+ rpos i))
         ;; Use the source directly until eof or the buffer is filled
         (let ((source (port-source p)))
           (let lp ((i i) (remaining (fx- n i)))
             (if (eqv? remaining 0)
                 (if (eqv? i 0) (eof-object) dst)
                 (let ((bytes (source dst i remaining)))
                   (if (eqv? bytes 0)   ;early eof
                       (cond ((eqv? i 0)
                              ($port-at-eof-set! p #f)
                              (eof-object))
                             (else
                              ($port-at-eof-set! p #t)
                              (bytevector-truncate! dst i)))
                       (lp (fx+ i bytes) (fx- remaining bytes))))))))))))

(define (get-bytevector-n! p dst start n)
  (unless (and (fixnum? n) (not (fxnegative? n))
               (fixnum? n) (not (fxnegative? n)))
    (assertion-violation 'get-bytevector-n!
                         "Expected an exact, non-negative start and count"
                         p dst start n))
  (unless (binary-input-port? p)
    (assertion-violation 'get-bytevector-n!
                         "Expected a binary input port" p))
  (let ((buf (port-buffer p))
        (rpos (port-buffer-rpos p))
        (rend (port-buffer-rend p)))
    (cond
      ((eqv? n 0) 0)
      (($port-at-eof p)
       ($port-at-eof-set! p #f)
       (eof-object))
      (else
       (let ((i (fxmin n (fx- rend rpos))))
         ;; Take as much as possible from the port buffer
         (bytevector-copy! buf rpos dst start i)
         (port-buffer-rpos-set! p (fx+ rpos i))
         ;; Use the source directly until eof or the buffer is filled
         (let ((source (port-source p)))
           (let lp ((i (fx+ start i)) (remaining (fx- n i)))
             (if (eqv? remaining 0)
                 (if (eqv? i 0) (eof-object) (fx- i start))
                 (let ((bytes (source dst i remaining)))
                   (if (eqv? bytes 0)   ;early eof
                       (cond ((eqv? i 0)
                              ($port-at-eof-set! p #f)
                              (eof-object))
                             (else
                              ($port-at-eof-set! p #t)
                              (fx- i start)))
                       (lp (fx+ i bytes) (fx- remaining bytes))))))))))))

(define (get-bytevector-some p)
  (unless (binary-input-port? p)
    (assertion-violation 'get-bytevector-some
                         "Expected a binary input port" p))
  (when (and (fx=? (port-buffer-rpos p) (port-buffer-rend p))
             (not ($port-at-eof p)))
    ($port-at-eof-set! p (eof-object? (port-fill-buffer p 'get-bytevector-some))))
  (let ((rpos (port-buffer-rpos p))
        (rend (port-buffer-rend p))
        (buf (port-buffer p)))
    (cond (($port-at-eof p)
           ($port-at-eof-set! p #f)
           (eof-object))
          ((eqv? rpos 0)
           ;; Steal the buffer
           (port-buffer-set! p ($make-bytevector (bytevector-length buf)))
           (port-buffer-rpos-set! p rend)
           (bytevector-truncate! buf rend))
          (else
           (let* ((n (fx- rend rpos))
                  (bv ($make-bytevector n)))
             (bytevector-copy! buf rpos bv 0 n)
             (port-buffer-rpos-set! p rend)
             bv)))))

(define (get-bytevector-all ip)
  (unless (binary-input-port? ip)
    (assertion-violation 'get-bytevector-all
                         "Expected a binary input port" ip))
  (cond
    (($port-at-eof ip)
     ($port-at-eof-set! ip #f)
     (eof-object))
    (else
     ;; XXX: This reads until the next eof. The eof is consumed.
     (let ((datum (get-bytevector-n ip 4096)))
       (if (eof-object? datum)
           datum
           (call-with-bytevector-output-port
             (lambda (op)
               (put-bytevector op datum)
               (let lp ()
                 (let ((datum (get-bytevector-n ip 4096)))
                   (when (bytevector? datum)
                     (put-bytevector op datum)
                     (when (not ($port-at-eof ip))
                       (lp))))))))))))

;;; Textual input

(define (get-char p)
  (unless (textual-input-port? p)
    (assertion-violation 'get-char "Expected a textual input port" p))
  (let ((rpos ($port-buffer-rpos p)))
    (cond ((fx=? rpos ($port-buffer-rend p))
           (cond (($port-at-eof p)
                  ($port-at-eof-set! p #f)
                  (eof-object))
                 (else
                  (let ((x (port-fill-buffer p 'get-char)))
                    ($port-at-eof-set! p #f)
                    x))))
          (else
           ($port-buffer-rpos-set! p (fx+ rpos 1))
           (string-ref ($port-buffer p) rpos)))))

(define (lookahead-char p)
  (unless (textual-input-port? p)
    (assertion-violation 'lookahead-char "Expected a textual input port" p))
  (let ((rpos ($port-buffer-rpos p)))
    (cond ((fx=? rpos ($port-buffer-rend p))
           (if ($port-at-eof p)
               (eof-object)
               (let ((x (port-fill-buffer p 'lookahead-char)))
                 ($port-at-eof-set! p (eof-object? x))
                 x)))
          (else
           (string-ref ($port-buffer p) rpos)))))

(define char-ready?
  (case-lambda
    ((p)
     (unless (textual-input-port? p)
       (assertion-violation 'char-ready? "Expected an open textual input port" p))
     ($input-port-ready? p))
    (() (char-ready? (current-input-port*)))))

(define (string-copy! s ss t ts k)
  (assert (not (eq? t s)))
  (do ((i (fx- k 1) (fx- i 1))
       (ti ts (fx+ ti 1))
       (si ss (fx+ si 1)))
      ((eqv? i -1))
    (string-set! t ti (string-ref s si))))

(define (get-string-n p n)
  (unless (fx>=? n 0)
    (assertion-violation 'get-string-n
                         "Expected a non-negative count" p n))
  (unless (textual-input-port? p)
    (assertion-violation 'get-string
                         "Expected a binary input port" p))
  (let ((buf (port-buffer p))
        (rpos (port-buffer-rpos p))
        (rend (port-buffer-rend p)))
    (cond
      ((eqv? n 0) "")
      ((and (eqv? rpos 0) (fx=? rend n) (fx=? (string-length buf) n))
       ;; Steal the whole buffer.
       (port-buffer-set! p ($make-string (string-length buf)))
       (port-buffer-rpos-set! p rend)
       (string-truncate! buf n))
      (($port-at-eof p)
       ($port-at-eof-set! p #f)
       (eof-object))
      (else
       (let ((i (fxmin n (fx- rend rpos)))
             (dst ($make-string n)))
         ;; Take as much as possible from the port buffer
         (string-copy! buf rpos dst 0 i)
         (port-buffer-rpos-set! p (fx+ rpos i))
         ;; Use the source directly until eof or the buffer is filled
         (let ((source (port-source p)))
           (let lp ((i i) (remaining (fx- n i)))
             (if (eqv? remaining 0)
                 (if (eqv? i 0) (eof-object) dst)
                 (let ((chars (source dst i remaining)))
                   (if (eqv? chars 0)   ;early eof
                       (cond ((eqv? i 0)
                              ($port-at-eof-set! p #f)
                              (eof-object))
                             (else
                              ($port-at-eof-set! p #t)
                              (string-truncate! dst i)))
                       (lp (fx+ i chars) (fx- remaining chars))))))))))))

(define (get-string-n! p dst start n)
  (unless (and (fixnum? n) (not (fxnegative? n))
               (fixnum? n) (not (fxnegative? n)))
    (assertion-violation 'get-string-n!
                         "Expected an exact, non-negative start and count"
                         p dst start n))
  (unless (textual-input-port? p)
    (assertion-violation 'get-string-n!
                         "Expected a binary input port" p))
  (let ((buf (port-buffer p))
        (rpos (port-buffer-rpos p))
        (rend (port-buffer-rend p)))
    (cond
      ((eqv? n 0) 0)
      (($port-at-eof p)
       ($port-at-eof-set! p #f)
       (eof-object))
      (else
       (let ((i (fxmin n (fx- rend rpos))))
         ;; Take as much as possible from the port buffer
         (string-copy! buf rpos dst start i)
         (port-buffer-rpos-set! p (fx+ rpos i))
         ;; Use the source directly until eof or the buffer is filled
         (let ((source (port-source p)))
           (let lp ((i (fx+ start i)) (remaining (fx- n i)))
             (if (eqv? remaining 0)
                 (if (eqv? i 0) (eof-object) (fx- i start))
                 (let ((chars (source dst i remaining)))
                   (if (eqv? chars 0)   ;early eof
                       (cond ((eqv? i 0)
                              ($port-at-eof-set! p #f)
                              (eof-object))
                             (else
                              ($port-at-eof-set! p #t)
                              (fx- i start)))
                       (lp (fx+ i chars) (fx- remaining chars))))))))))))

(define (get-string-all ip)
  (unless (textual-input-port? ip)
    (assertion-violation 'get-string-all
                         "Expected a textual input port" ip))
  (cond
    (($port-at-eof ip)
     ($port-at-eof-set! ip #f)
     (eof-object))
    (else
     ;; XXX: This reads until the next eof, consuming the eof, kind of
     ;; like get-line.
     (let ((datum (get-string-n ip 512)))
       (if (eof-object? datum)
           datum
           (call-with-string-output-port
             (lambda (op)
               (put-string op datum)
               (let lp ()
                 (let ((datum (get-string-n ip 512)))
                   (when (string? datum)
                     (put-string op datum)
                     (unless ($port-at-eof ip)
                       (lp))))))))))))

(define (get-line ip)
  (unless (textual-input-port? ip)
    (assertion-violation 'get-line
                         "Expected a textual input port" ip))
  (cond
    (($port-at-eof ip)
     ($port-at-eof-set! ip #f)
     (eof-object))
    (else
     (let ((c (sys:get-char ip)))
       (if (eof-object? c)
           c
           (call-with-string-output-port
             (lambda (op)
               ;; TODO: it would be faster to scan the buffer to a
               ;; newline character.
               (unless (eqv? c #\newline)
                 (put-char op c)
                 (let lp ()
                   (unless ($port-at-eof ip)
                     (let ((c (get-char ip)))
                       (when (eof-object? c)
                         ($port-at-eof-set! ip #t))
                       (when (and (char? c)
                                  (not (eqv? c #\newline)))
                         (put-char op c)
                         (lp)))))))))))))

;;; Output ports

(define (flush-output-port p)
  (assert (output-port? p))
  (cond
    ((fxtest (port-flags p) PORT-FLAG-IN-MEMORY)
     ;; Let the sink make the buffer larger, if necessary.
     (let ((sink (port-sink p)))
       (sink p)))
    (else
     (let ((sink (port-sink p))
           (buffer (port-buffer p)))
       (unless (procedure? sink)
         (error 'flush-output-port "Expected an open output port" p))
       (let lp ((i 0)
                (k (port-buffer-wpos p)))
         (when (fxpositive? k)
           (let ((items (sink buffer i k)))
             (unless (fx<=? 0 items k)
               (error 'put-bytevector "Bad write! return value" p items))
             (let ((i* (fx+ i items))
                   (k* (fx- k items)))
               (port-buffer-wpos-set! p i*)
               (lp i* k*))))))
     (port-buffer-rpos-set! p 0)
     (port-buffer-rend-set! p 0)
     (port-buffer-wpos-set! p 0))))

(define (output-port-buffer-mode p)
  (assert (output-port? p))
  ($port-buffer-mode p))

;; open-file-output-port is defined elsewhere.

(define open-bytevector-output-port
  (case-lambda
    ((transcoder)
     (define buf #vu8())
     (define pos 0)
     (define (grow! minimum)
       (when (fx<? (bytevector-length buf) minimum)
         (let ((new ($make-bytevector (fxmax minimum (fx* (bytevector-length buf) 2)))))
           (bytevector-copy! buf 0 new 0 (bytevector-length buf))
           (set! buf new))))
     (define (bytevector-extractor)
       (trace "extract flushes: " buf " " pos)
       (flush-output-port port)
       (trace "extract flushed: " buf " " pos)
       (let ((buffer buf)
             (position pos))
         (set! buf #vu8())
         (set! pos 0)
         (trace "extracting " position " bytes from " buffer)
         (bytevector-truncate! buffer position)))
     (define (write! bv start count)
       (trace "-write! bv=" bv " start=" start " count=" count " buf=" buf)
       (grow! (fx+ pos count))
       (bytevector-copy! bv start buf pos count)
       (set! pos (+ pos count))
       (trace "+write! bv=" bv " start=" start " count=" count " buf=" buf)
       count)
     (define (get-position)
       pos)
     (define set-position! #f)          ;TODO: implement
     (define (close)
       (set! buf #vu8())
       (set! pos 0))
     (define port
       (let ((p (make-custom-binary-output-port "*bytevector*"
                                                write! get-position
                                                set-position! close)))
         (if transcoder
             (transcoded-port p transcoder)
             p)))
     (values port bytevector-extractor))
    (()
     (open-bytevector-output-port #f))))

(define call-with-bytevector-output-port
  (case-lambda
    ((proc)
     (call-with-bytevector-output-port proc #f))
    ((proc transcoder)
     (let-values (((port extractor) (open-bytevector-output-port transcoder)))
       (proc port)
       (sys:flush-output-port port)
       (let ((ret (extractor)))
         (sys:close-port port)
         ret)))))

(define (open-string-output-port)
  (let ((port (sys:open-output-string)))
    (letrec ((string-extractor
              (lambda ()
                (sys:get-output-string port))))
      (values port string-extractor))))

(define (call-with-string-output-port proc)
  (let ((port (sys:open-output-string)))
    (proc port)
    (let ((ret (sys:get-output-string port)))
      (sys:close-port port)
      ret)))

;;; SRFI 6-style string ports

(define (open-output-string)
  (define (grow-output-string! port)
    ;; Grow the string output port's buffer
    (when (eq? ($port-buffer-wpos port)
               ($port-buffer-wend port))
      (let* ((buf (port-buffer port))
             (old-len (string-length buf))
             (new-len (fxmax 8 (fx* old-len 2))))
        (do ((new ($make-string new-len #\nul))
             (i 0 (fx+ i 1)))
            ((fx=? i old-len)
             (port-buffer-set! port new)
             (port-buffer-wend-set! port (string-length new)))
          (string-set! new i (string-ref buf i))))))
  (define close #f)
  (define port
    (make-port (make-string 8)
               (fxior PORT-TYPE-TEXTUAL PORT-FLAG-IN-MEMORY
                      PORT-DIR-OUTPUT BUFFER-MODE-BLOCK)
               grow-output-string! #f
               #f #f ;; TODO: getpos setpos
               close #f
               "*string*" #f))
  port)

(define (get-output-string port)
  (unless (fxtest (port-flags port) PORT-FLAG-IN-MEMORY)
    (assertion-violation 'get-output-string
                         "Expected a string output port" port))
  (let ((buffer (port-buffer port))
        (position (port-buffer-wpos port)))
    (port-buffer-set! port (make-string 1))
    (port-buffer-wpos-set! port 0)
    (port-buffer-wend-set! port (string-length (port-buffer port)))
    (string-truncate! buffer position)))

;;; Binary output

(define (port-output-overflow-u8 p u8 who)
  (unless (output-port-open? p)
    (assertion-violation who "Expected an open port" p u8))
  (flush-output-port p)
  (bytevector-u8-set! ($port-buffer p) 0 u8)
  ($port-buffer-wpos-set! p 1)
  (if (eqv? BUFFER-MODE-NONE (fxand BUFFER-MODE-MASK (port-flags p)))
      (flush-output-port p)
      (let ((buf (port-buffer p)))
        ($port-buffer-wend-set! p (if (string? buf)
                                      (string-length buf)
                                      (bytevector-length buf))))))

(define (put-u8 p u8)
  (unless (binary-output-port? p)
    (assertion-violation 'put-u8 "Expected a binary output port" p u8))
  (let ((wpos ($port-buffer-wpos p)))
    (cond ((fx=? wpos ($port-buffer-wend p))
           (port-output-overflow-u8 p u8 'put-u8))
          (else
           (bytevector-u8-set! ($port-buffer p) wpos u8)
           ($port-buffer-wpos-set! p (fx+ wpos 1))))))

(define put-bytevector
  (case-lambda
    ((p bv)
     (put-bytevector p bv 0 (bytevector-length bv)))
    ((p bv start)
     (put-bytevector p bv start (fx- (bytevector-length bv) start)))
    ((p bv start count)
     (unless (binary-output-port? p)
       (error 'put-bytevector "Expected a binary output port" p bv start count))
     (let ((end (fx+ start count))
           (buf (port-buffer p)))
       (assert (fx<=? 0 start end (bytevector-length bv)))
       (let ((wend (port-buffer-wend p))
             (wpos (port-buffer-wpos p)))
         (when (fx=? wpos wend)
           (flush-output-port p))
         (trace "put-bytevector " p " bv=" bv " start=" start " count=" count
                " end=" end " mode=" mode)
         (cond
           ((fx<? count wend)
            ;; The data will fit in the buffer, possibly after
            ;; flushing. Reduces calls to the sink.
            (unless (fx<? count (fx- wend wpos))
              (flush-output-port p))
            (let ((wpos (port-buffer-wpos p)))
              (bytevector-copy! bv start buf wpos count)
              (port-buffer-wpos-set! p (fx+ wpos count))))
           (else
            ;; The data to write is larger than the buffer, so flush
            ;; the buffer and use the sink directly. This also reduces
            ;; calls to the sink, assuming the sink can handle
            ;; relatively large buffers.
            (flush-output-port p)
            (let ((sink (port-sink p)))
              (let lp ((i start)
                       (k count))
                (when (fxpositive? k)
                  (let ((bytes (sink bv i k)))
                    (unless (fx<=? 0 bytes k)
                      (error 'put-bytevector "Bad write! return value" p bytes))
                    (let ((i* (fx+ i bytes))
                          (k* (fx- k bytes)))
                      (lp i* k*)))))))))))))

;;; Textual output

(define (port-output-overflow-char p c who)
  (unless (output-port-open? p)
    (assertion-violation who "Expected an open port" p c))
  (when (and (not (eqv? 0 (port-buffer-wpos p)))
             (fx=? (port-buffer-wpos p) (port-buffer-wend p)))
    (flush-output-port p))
  (let* ((wpos (port-buffer-wpos p))
         (wpos+1 (fx+ wpos 1)))
    (string-set! (port-buffer p) wpos c)
    (port-buffer-wpos-set! p wpos+1)
    (when (or (eq? c ($port-nl p))
              (fx>=? wpos+1 ($port-buffer-wend p)))
      (flush-output-port p))))

(define (put-char p c)
  (unless (char? c)
    (assertion-violation 'put-char "Expected a character" p c))
  (unless (textual-output-port? p)
    (assertion-violation 'put-char "Expected a textual output port" p c))
  (let ((wpos ($port-buffer-wpos p)))
    (cond ((or (fx=? wpos ($port-buffer-wend p))
               (eq? c ($port-nl p)))
           (port-output-overflow-char p c 'put-char))
          (else
           (string-set! ($port-buffer p) wpos c)
           ($port-buffer-wpos-set! p (fx+ wpos 1))))))

(define put-string
  (case-lambda
    ((p str)
     (put-string p str 0 (string-length str)))
    ((p str start)
     (put-string p str start (fx- (string-length str) start)))
    ((p str start count)
     (do ((end (fx+ start count))
          (i start (fx+ i 1)))
         ((fx=? i end))
       (sys:put-char p (string-ref str i))))))

(define (put-datum tp d)
  (write d tp))

;;; Input/output ports

;; open-file-input/output-port is defined elsewhere.

;;; Simple I/O

(define (call-with-input-file filename proc)
  (let ((p (open-input-file filename)))
    (let-values ((ret (proc p)))
      (sys:close-input-port p)
      (apply values ret))))

(define (call-with-output-file filename proc)
  (let ((p (open-output-file filename)))
    (let-values ((ret (proc p)))
      (sys:close-output-port p)
      (apply values ret))))

(define (with-input-from-file filename thunk)
  (call-with-port (open-input-file filename)
    (lambda (p)
      (parameterize ([current-input-port* p])
        (thunk)))))

(define (with-output-to-file filename thunk)
  (call-with-port (open-output-file filename)
    (lambda (p)
      (parameterize ([current-output-port* p])
        (thunk)))))

(define (open-input-file filename)
  (sys:open-file-input-port filename
                            (file-options)
                            (buffer-mode block)
                            (native-transcoder)))

(define (open-output-file filename)
  (sys:open-file-output-port filename
                             (file-options)
                             (buffer-mode block)
                             (native-transcoder)))

(define (close-input-port p)
  (assert (input-port? p))
  (sys:close-port p))

(define (close-output-port p)
  (assert (output-port? p))
  (sys:close-port p))

(define read-char
  (case-lambda
    (() (read-char (current-input-port*)))
    ((tp) (get-char tp))))

(define peek-char
  (case-lambda
    (() (peek-char (current-input-port*)))
    ((tp) (lookahead-char tp))))

(define write-char
  (case-lambda
    ((ch) (write-char ch (current-output-port*)))
    ((ch tp) (sys:put-char tp ch))))

;; See (loko runtime io-printer) for the printer.

;;; File system

;; See (loko runtime init).

;;; Standard ports

;; These are registered with $init-standard-ports. Each new process
;; will get its own set of these.
(define *stdin-read*)
(define *stdout-write*)
(define *stderr-write*)
(define *stdout-bufmode* (buffer-mode line))

(define (standard-input-port)
  (let ((p (sys:make-custom-binary-input-port "*stdin*" *stdin-read* #f #f #f)))
    (port-file-descriptor-set! p 0)
    p))

(define (standard-output-port)
  (let ((p (sys:make-custom-binary-output-port "*stdout*" *stdout-write* #f #f #f)))
    (port-file-descriptor-set! p 1)
    ($port-buffer-mode-set! p *stdout-bufmode*)
    p))

(define (standard-error-port)
  (let ((p (sys:make-custom-binary-output-port "*stderr*" *stderr-write* #f #f #f)))
    (port-file-descriptor-set! p 2)
    ($port-buffer-mode-set! p (buffer-mode line))
    p))

(define current-output-port*
  (make-parameter (open-textual-null-port)
                  (lambda (p)
                    (assert (output-port? p))
                    p)))

(define current-error-port*
  (make-parameter (open-textual-null-port)
                  (lambda (p)
                    (assert (output-port? p))
                    p)))

(define current-input-port*
  (make-parameter (open-textual-null-port)
                  (lambda (p)
                    (assert (input-port? p))
                    p)))

(define (current-output-port) (current-output-port*))
(define (current-error-port) (current-error-port*))
(define (current-input-port) (current-input-port*))

(define ($init-standard-ports stdin-read stdout-write stderr-write
                              stdout-bufmode eolstyle)
  (set! *stdin-read* stdin-read)
  (set! *stdout-write* stdout-write)
  (set! *stderr-write* stderr-write)
  (set! *stdout-bufmode* stdout-bufmode)
  (let ((tc (make-transcoder (utf-8-codec) eolstyle (error-handling-mode replace))))
    (current-input-port* (transcoded-port (standard-input-port) tc))
    (current-output-port* (transcoded-port (standard-output-port) tc))
    (current-error-port* (transcoded-port (standard-error-port) tc))))

($init-standard-ports (lambda _ 0)
                      (lambda (bv start count)
                        (do ((end (+ start count))
                             (i start (fx+ i 1)))
                            ((fx=? i end) count)
                          ($debug-put-u8 (bytevector-u8-ref bv i))))
                      (lambda (_bv _start count) count)
                      (buffer-mode none)
                      (native-eol-style)))
