;; SPDX-License-Identifier: EUPL-1.2+
;; Loko Scheme PC bare-metal graphical REPL
;; Copyright © 2019-2022 G. Weinholt
#!r6rs

;;; REPL that runs in a Valand window

(library (loko pc-repl repl-window)
  (export
    start-new-repl-window
    replwin-window
    replwin-text-mode-event-ch)
  (import
    (rnrs)
    (rnrs eval)
    (rnrs mutable-strings)
    (srfi :98)

    (only (loko)
          parameterize
          make-parameter
          pretty-print
          library-directories
          port-buffer-mode-set!)

    (only (loko system r7rs) current-output-port* current-input-port* current-error-port*)
    (only (loko system $primitives) $void?)

    (only (loko runtime repl) banner)

    (only (psyntax expander) interaction-environment new-interaction-environment)

    (loko match)
    (loko queues)

    (loko font font-6x13)

    (loko system logging)
    (loko system fibers)

    (loko valand)
    (loko valand drawing)
    (loko kernel osabi-linux)

    (loko drivers usb hid-numbers)

    (text-mode console)
    (text-mode console events)
    (text-mode console model))

(define font-h 13)
(define font-w 6)

(define-record-type replwin
  (sealed #t)
  (fields display
          window
          (mutable text-cursor-color)   ;TODO: maybe use cursor attr from text-mode
          text-mode-event-ch
          tui-output-ch))

(define (log/x severity . x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*) (apply log/x DEBUG x*))
(define (log/info . x*) (apply log/x INFO x*))
(define (log/warning . x*) (apply log/x WARNING x*))
(define (log/error . x*) (apply log/x ERROR x*))

(define (buffer-channel in-ch out-ch len)
  (define q (make-queue))
  (assert (and (channel? in-ch) (channel? out-ch) (fixnum? len)))
  (spawn-fiber
   (lambda ()
     (let lp ((qlen 0))
       (match (perform-operation (choice-operation
                                  (wrap-operation (get-operation in-ch)
                                                  (lambda (x) (cons 'get x)))
                                  (if (queue-empty? q)
                                      (choice-operation)
                                      (wrap-operation (put-operation out-ch (queue-front q))
                                                      (lambda _ 'put)))))
         [('get . msg)
          (cond ((fx>? qlen len)
                 ;; Drop the oldest message.
                 (dequeue! q)
                 (enqueue! q msg)
                 (lp qlen))
                (else
                 (enqueue! q msg)
                 (lp (fx+ qlen 1))))]
         ['put
          (dequeue! q)
          (lp (fx- qlen 1))])))))

(define (valand-textmode-backend replwin)
  (define window (replwin-window replwin))
  (define (tm-color->fb-color color)
    ;; XXX: text-mode colors are compatible with VL_FORMAT_XRGB8888.
    color)
  (lambda (cmd arg)
    (case cmd
      [(get-size)
       (let* ((w (vl_surface-width window))
              (h (vl_surface-height window))
              (cols (fxdiv w font-w))
              (rows (fxdiv h font-h)))
         (values cols rows w h))]
      [(init)
       #f]
      [(update redraw)
       (let* ((c arg)
              ;; Cursor location in absolute coordinates
              (cx (fx+ (console-x c) (console-x1 c)))
              (cy (fx+ (console-y c) (console-y1 c)))
              (cols (fxdiv (vl_surface-width window) font-w))
              (rows (fxdiv (vl_surface-height window) font-h))
              (winbuf (vl_surface-buffer window))
              (buf-width (vl_buffer-width winbuf))
              (default-color (vl_buffer-color/rgb winbuf 2/3 2/3 2/3))
              (x-offset (fxdiv (fxmod (vl_surface-width window) font-w) 2)))
         (do ((y 0 (fx+ y 1))) ((fx=? y rows))
           (let ((ch-y (fx* font-h y)))
             (when (console-row-dirty? c y 'absolute)
               (vl_surface-damage! window 0 ch-y buf-width (fx+ ch-y font-h))
               (let-values ([(buf mbuf fgbuf bgbuf abuf idx) (%index/nowin c 0 y)])
                 (do ((x 0 (fx+ x 1))) ((fx=? x cols))
                   (let ((ch-x (fx+ x-offset (fx* font-w x)))
                         (ch (text-ref buf (fx+ idx x))))
                     (if (and (eqv? x cx) (eqv? y cy))
                         (vl_buffer-draw-character winbuf font ch-x ch-y
                                                   font-w font-h
                                                   (rgb-color 0 0 0)
                                                   (replwin-text-cursor-color replwin)
                                                   (if (textcell-unused? ch)
                                                       #\space
                                                       ch))
                         ;; TODO: Bold, italics, etc
                         (unless (textcell-unused? ch)
                           (let ((fg (tm-color->fb-color (fg-ref fgbuf (fx+ idx x))))
                                 (bg (tm-color->fb-color (bg-ref bgbuf (fx+ idx x)))))
                             (vl_buffer-draw-character winbuf font ch-x ch-y font-w font-h
                                                       (if (eqv? fg Default)
                                                           default-color
                                                           fg)
                                                       (if (eqv? bg Default)
                                                           (rgb-color 0 0 0)
                                                           bg)
                                                       ch))))))))))
         (clear-console-dirty! c)
         (set-console-dirty! c (console-y c)))]
      [(read-event)
       (get-message (replwin-text-mode-event-ch replwin))]
      [else
       #f])))

(define (println/newlines str)
  (let ((p (open-string-input-port str)))
    (let lp ()
      (let ((line (get-line p)))
        (unless (eof-object? line)
          (println line)
          (lp))))))

(define (print/write datum)
  (print
   (call-with-string-output-port
     (lambda (p) (write datum p)))))

(define (print-lines string)
  (let ((p (open-string-input-port string)))
    (let lp ()
      (let ((line (get-line p)))
        (unless (eof-object? line)
          (println line)
          (lp))))))

;; TODO: Support styles on the standard implementation of print-condition
(define (print-condition exn)
  (cond ((condition? exn)
         (let ((c* (simple-conditions exn)))
           (text-color LightRed)
           (println "An unhandled condition was raised:")
           (do ((i 1 (fx+ i 1))
                (c* c* (cdr c*)))
               ((null? c*))
             (let* ((c (car c*))
                    (rtd (record-rtd c)))
               (text-color Default)
               (print " ")
               (let loop ((rtd rtd))
                 (text-attribute 'bold (eq? rtd (record-rtd c)))
                 (print (record-type-name rtd))
                 (text-attribute 'bold #f)
                 (cond ((record-type-parent rtd) =>
                        (lambda (rtd)
                          (unless (eq? rtd (record-type-descriptor &condition))
                            (print " ")
                            (loop rtd))))))
               (let loop ((rtd rtd))
                 (do ((f* (record-type-field-names rtd))
                      (i 0 (fx+ i 1)))
                     ((fx=? i (vector-length f*))
                      (cond ((record-type-parent rtd) => loop)))
                   (println)
                   (print "  ")
                   (text-color Default)
                   (text-attribute 'italic #t)
                   (print (vector-ref f* i))
                   (print ": ")
                   (text-attribute 'italic #f)
                   (let ((x ((record-accessor rtd i) c)))
                     (cond ((and (eq? rtd (record-type-descriptor &irritants))
                                 (pair? x) (list? x))
                            (print "(")
                            (let ((list-x (wherex)))
                              (text-color LightRed)
                              (print/write (car x))
                              (for-each (lambda (value)
                                          (println)
                                          (gotoxy list-x (wherey))
                                          (print/write value))
                                        (cdr x)))
                            (text-color Default)
                            (print ")"))
                           (else
                            (text-color LightRed)
                            (print/write x)))))))
             (println))))
        (else
         (println "A non-condition object was raised:")
         (print/write exn))))


(define (make-console-output-port color bufmode replwin)
  (define id "console")
  (define (write! str start count)
    ;; TODO: Parse ANSI codes
    (put-message (replwin-tui-output-ch replwin)
                 (list color (substring str start (+ start (min 200 count)))))
    count)
  (letrec ((close
            (lambda ()
              ;; TODO: Maybe it's better to allow this and then repair it
              (error 'close-port "Closing the console ports is not permitted" p)))
           (p (make-custom-textual-output-port id write! #f #f close)))
    (port-buffer-mode-set! p bufmode)
    p))

(define (eval-expr datum env)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (exn)
          (send-log WARNING
                    (call-with-string-output-port
                      (lambda (p)
                        (display "Exception from the expression " p)
                        (write datum p)))
                    'EXCEPTION exn)
          (when (serious-condition? exn)
            (k #t)))
        (lambda ()
          (call-with-values
            (lambda ()
              (let-values ((v* (eval datum env)))
                (flush-output-port (current-output-port))
                (flush-output-port (current-error-port))
                (apply values v*)))
            (case-lambda
              ((x)
               (unless ($void? x)
                 (pretty-print x)))
              (() #f)
              (x*
               (for-each pretty-print x*))))
          (flush-output-port (current-output-port))
          (flush-output-port (current-error-port)))))))

(define (spawn-program replwin cmdline)
  ;; XXX: This is a pretty cheap command line parser that's just meant
  ;; to be used to launch some initial program which then does more
  ;; advanced shell stuff.
  (define (parse-cmdline cmdline)
    (let ((p (open-string-input-port cmdline)))
      (let f ()
        (let-values ([(strp extract) (open-string-output-port)])
          (let lp ((prev #f))
            (let ((c (get-char p)))
              (cond ((eof-object? c)
                     (if prev
                         (list (extract))
                         '()))
                    ((char-whitespace? c)
                     (cond
                       ((not prev)
                        (lp prev))
                       ((char-whitespace? prev)
                        (lp c))
                       (else
                        (cons (extract) (f)))))
                    ((eqv? c #\")
                     (let lp* ()
                       (let ((c (get-char p)))
                         (cond ((eof-object? c)
                                (error 'spawn-program "Unterminated quote" cmdline))
                               ((eqv? c #\")
                                (cons (extract) (f)))
                               (else
                                (put-char strp c)
                                (lp*))))))
                    (else
                     (put-char strp c)
                     (lp c)))))))))
  (guard (exn
          ((serious-condition? exn)
           (send-log ERROR "Command start failed"
                     'EXCEPTION exn)))
    (let ((command-line (parse-cmdline cmdline)))
      (when (null? command-line)
        (error 'spawn-program "Empty command line" cmdline))
      (spawn-linux-process (car command-line)
                           (map string->utf8 command-line)
                           (map (lambda (var)
                                  (string->utf8
                                   (string-append (car var) "=" (cdr var))))
                                (get-environment-variables))
                           (replwin-display replwin)))))

(define (log-view console outport errport replwin)
  (define event-ch (make-channel))
  (define visible-severity INFO)
  (define log-ch (make-channel))

  ;; Receive logs, with buffering
  ;; FIXME: is the buffering necessary?
  (current-log-callback
   (let ((logw-ch (make-channel)))
     (buffer-channel logw-ch log-ch 100)
     (lambda (e) (put-message logw-ch e))))

  (text-color DarkGray)
  (println "Logging started...")
  (text-color Default)

  ;; Receive text-mode events
  (spawn-fiber
   (lambda ()
     (let lp ()
       (let ((ev (read-event console)))
         (assert ev)
         (put-message event-ch ev)
         (lp)))))

  (do ((stop #f)) (stop)
    (match (perform-operation
            (choice-operation
             (wrap-operation (get-operation log-ch) (lambda (x) (cons 'log x)))
             (wrap-operation (get-operation event-ch) (lambda (x) (cons 'event x)))))

      ;; An input event
      [('event . ev)
       (cond
         ((and (key-press-event? ev) (eqv? (keyboard-event-char ev) #\+)
               (enum-set=? (keyboard-event-mods ev) (modifier-set)))
          (unless (eqv? visible-severity DEBUG)
            (set! visible-severity (fx+ visible-severity 1)))
          (print "Current log level: ")
          (println (severity->symbol visible-severity)))

         ((and (key-press-event? ev) (eqv? (keyboard-event-char ev) #\-)
               (enum-set=? (keyboard-event-mods ev) (modifier-set)))
          (unless (eqv? visible-severity EMERGENCY)
            (set! visible-severity (fx- visible-severity 1)))
          (print "Current log level: ")
          (println (severity->symbol visible-severity))))]

      [('log . e)
       (let ((severity (cdr (assq 'SEVERITY e)))
             (message (cdr (assq 'MESSAGE e)))
             (subsystem (cond ((assq 'SUBSYSTEM e) => cdr)
                              (else #f)))
             (exe (cond ((assq '_EXE e) => cdr) (else #f))))
         (when (fx<=? severity visible-severity)
           (cond ((assq 'JIFFY e) =>
                  (lambda (x)
                    (text-color Gray)
                    (print (cdr x))
                    (print " "))))
           (when subsystem
             (text-color DarkGray)
             (print subsystem)
             (print " "))
           (when exe
             (text-color White)
             (print exe)
             (text-color Default)
             (print "[")
             (text-color White)
             (print (cond ((assq '_PID e) => cdr) (else #f)))
             (text-color Default)
             (print "] "))
           (text-color Default)
           (print "[")
           (cond ((eqv? severity DEBUG) (text-color White))
                 ((eqv? severity INFO) (text-color LightGreen))
                 ((eqv? severity ERROR) (text-color LightRed))
                 ((eqv? severity WARNING) (text-color Yellow))
                 ((eqv? severity CRITICAL) (text-color LightMagenta))
                 (else (text-color Default)))
           (print (severity->symbol severity))
           (text-color Default)
           (print "] ")
           (println message)
           (cond ((assq 'EXCEPTION e) =>
                  (lambda (exn)
                    (print-condition (cdr exn)))))))])))

(define (tui console outport errport replwin)
  (define event-ch (make-channel))
  (define tui-output-ch (replwin-tui-output-ch replwin))
  (define line "")
  (define cur 0)
  (define prev #f)
  (define prompty (wherey))
  (define outputx 0)
  (define outputy (wherey))

  (define (redraw-prompt)
    (gotoxy 0 prompty)
    (text-color LightGreen)
    (print "> ")
    (let ((prompt-width (wherex)))
      (text-color Default)
      (print line)
      (clreol)
      (gotoxy (fx+ cur prompt-width) (wherey))))

  ;; Receive text-mode events
  (spawn-fiber
   (lambda ()
     (let lp ()
       (let ((ev (read-event console)))
         (assert ev)
         (put-message event-ch ev)
         (lp)))))

  (print-lines (call-with-string-output-port banner))
  (set! prompty (wherey))
  (set! outputy (wherey))
  (redraw-prompt)

  (unless (interaction-environment)
    (interaction-environment (new-interaction-environment)))
  (do ((stop #f)) (stop)
    (match (perform-operation
            (choice-operation
             (wrap-operation (get-operation event-ch) (lambda (x) (cons 'event x)))
             (wrap-operation (get-operation tui-output-ch) (lambda (x) (cons 'output x)))))
      ;; An input event
      [('event . ev)
       (replwin-text-cursor-color-set! replwin Gray)

       (when #f
         ;; Debug event handling
         (cond
           ((key-press-event? ev)
            (log/debug (list 'press (enum-set->list (keyboard-event-mods ev))
                             (keyboard-event-char ev) (keyboard-event-key ev)
                             (keyboard-event-location ev))))
           ((unknown-event? ev)
            (log/debug (list 'unknown (unknown-event-source ev)
                             (if (integer? (unknown-event-data ev))
                                 (number->string (unknown-event-data ev) 16)
                                 (unknown-event-data ev)))))
           (else
            (log/debug ev))))

       (cond
         ((and (key-press-event? ev)
               (enum-set=? (keyboard-event-mods ev) (modifier-set ctrl))
               (eqv? (keyboard-event-key ev) #\l))
          (clrscr)
          (set! prompty 0)
          (set! outputx 0)
          (set! outputy 0))

         ((resize-event? ev)
          #f)

         ((and (key-press-event? ev)
               (or (and
                     (enum-set=? (keyboard-event-mods ev) (modifier-set ctrl))
                     (eqv? (keyboard-event-key ev) #\a))
                   (eq? (keyboard-event-key ev) 'Home)))
          (set! cur 0))

         ((and (key-press-event? ev)
               (or (and
                     (enum-set=? (keyboard-event-mods ev) (modifier-set ctrl))
                     (eqv? (keyboard-event-key ev) #\e))
                   (eq? (keyboard-event-key ev) 'End)))
          (set! cur (string-length line)))

         ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'Backspace)
               (enum-set=? (enum-set-difference (keyboard-event-mods ev)
                                                (modifier-set shift))
                           (modifier-set)))
          (unless (eqv? cur 0)
            (set! line (string-append (substring line 0 (fx- cur 1))
                                      (substring line cur (string-length line))))
            (set! cur (fx- cur 1))))

         ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'ArrowLeft)
               (not (keyboard-event-has-modifiers? ev)))
          (unless (eqv? cur 0)
            (set! cur (fx- cur 1))))

         ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'ArrowRight)
               (not (keyboard-event-has-modifiers? ev)))
          (unless (eqv? cur (string-length line))
            (set! cur (fx+ cur 1))))

         ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'ArrowUp)
               (not (keyboard-event-has-modifiers? ev)))
          (set! line prev)
          (set! cur (string-length line)))

         ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'Tab)
               (not (keyboard-event-has-modifiers? ev)))
          #f)

         ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'Enter)
               (not (keyboard-event-has-modifiers? ev)))
          (cond
            ((equal? line ""))

            ((char=? (string-ref line 0) #\@)
             ;; Program invocation
             (set! prev line)
             (let ((cmdline (substring line 1 (string-length line))))
               (set! line "")
               (set! cur 0)
               (println)
               (clreol)
               (set! prompty (wherey))
               (set! outputx 0)
               (set! outputy (wherey))
               (spawn-program replwin cmdline)))

            (else
             ;; Scheme expression
             (set! prev line)
             (call/cc
               (lambda (k)
                 (guard (exn ((lexical-violation? exn)
                              (replwin-text-cursor-color-set! replwin LightRed)
                              (k #t)))
                   (let* ((line-p (open-string-input-port line))
                          (datum (read line-p)))
                     (set! line (if (port-eof? line-p) "" (get-string-all line-p)))
                     (set! cur 0)
                     (println)
                     (clreol)
                     (set! prompty (wherey))
                     (set! outputx 0)
                     (set! outputy (wherey))
                     (spawn-fiber
                      (lambda ()
                        (parameterize ([current-console console]
                                       [current-output-port* outport]
                                       [current-error-port* errport])
                          (eval-expr datum (interaction-environment))))))))))))

         ((and (key-press-event? ev) (keyboard-event-char ev)) =>
          (lambda (char)
            (set! line (string-append (substring line 0 cur)
                                      (string char)
                                      (substring line cur (string-length line))))
            (set! cur (fx+ cur 1)))))

       ;; Update the screen immediately after handling user input
       (redraw-prompt)
       (update-screen)
       #;
       (vl_display-render the-display)]

      [('output color str)
       (gotoxy 0 prompty)
       (clreol)
       (gotoxy outputx outputy)
       (text-color color)
       (do ((buf (make-string 1))
            (i 0 (fx+ i 1)))
           ((fx=? i (string-length str)))
         (let ((c (string-ref str i)))
           (cond ((eqv? c #\newline)
                  (println))
                 (else
                  ;; XXX: The text-mode API is a bit unfortunate for
                  ;; this particular use case.
                  (string-set! buf 0 c)
                  (print buf)))))
       (set! outputx (wherex))
       (set! outputy (wherey))
       (unless (eqv? 0 outputx)
         (println)
         (when (fx=? outputy (wherey))
           (set! outputy (fx- outputy 1))))
       (set! prompty (wherey))
       (redraw-prompt)
       ;; FIXME: call update-screen, with a rate limiter
       ])))

;; Translate Valand keyboard and mouse events to text-mode events.
(define (repl-keyboard/mouse-driver replwin)
  (define (send-text-mode-event event)
    (put-message (replwin-text-mode-event-ch replwin) event))
  (define (hid->key-symbol page usage)
    ;; FIXME: recover the key location
    (case page
      ((#x07)
       (cond ((assv usage
                    '((#x28 . Enter)
                      (#x2a . Backspace)
                      (#x2b . Tab)
                      (#x4a . Home)
                      (#x4b . PageUp)
                      (#x4c . Delete)
                      (#x4d . End)
                      (#x4e . PageDown)
                      (#x4f . ArrowRight)
                      (#x50 . ArrowLeft)
                      (#x51 . ArrowDown)
                      (#x52 . ArrowUp)))
              => cdr)
             (else #f)))
      (else #F)))
  (define (hid->modifier-set leds mods)
    (define (fxtest? mods mask)
      (not (eqv? 0 (fxand mods mask))))
    ((enum-set-constructor (modifier-set))
     (filter values
             (list
              (and (fxtest? mods HID-modifiers-Control) (modifier ctrl))
              (and (fxtest? mods HID-modifiers-Shift) (modifier shift))
              (and (fxtest? mods HID-modifiers-Alt) (modifier alt))
              (and (fxtest? mods HID-modifiers-GUI) (modifier meta))
              #;(and (fxtest? mods HID-modifiers-AltGr) (modifier alt-graph))
              #;(and (fxbit-set? leds HID-LED-Caps-Lock) (modifier caps-lock))
              #;(and (fxbit-set? leds HID-LED-Num-Lock) (modifier num-lock))
              #;(and (fxbit-set? leds HID-LED-Scroll-Lock) (modifier scroll-lock))))))
  (define ev-ch (make-channel))
  (let ((write-ev-ch (make-channel)))
    (buffer-channel write-ev-ch ev-ch 100)
    (vl_surface-input-callbacks-set! (replwin-window replwin)
                                     (lambda (ev)
                                       (put-message write-ev-ch (cons 'mouse ev)))
                                     (lambda (ev)
                                       (put-message write-ev-ch (cons 'kbd ev)))))
  (let lp ()
    (match (get-message ev-ch)
      [('mouse . foo)
       ;; (println (list 'mouse foo))
       (lp)]
      ;; TODO: Well, this got complicated and there should be record
      ;; types for this stuff. The inner vector is the one from
      ;; keyboard-manager, the outer vector comes from what was added
      ;; to let Valand understand the key.
      [('kbd . #(_make/break _page _usage _key _mods
                             #(make/break _x page usage (leds mods key))))
       ;; (log/info "kbd: " (vector make/break _x page usage (list leds mods key)))
       (let ((modifiers (hid->modifier-set leds mods)))
         (when (eq? make/break 'make)
           (cond
             ((hid->key-symbol page usage) =>
              (lambda (keysym)
                (send-text-mode-event (make-key-press-event modifiers key keysym 'standard))))
             ((char? key)
              (send-text-mode-event (make-key-press-event modifiers key key 'standard)))
             (else
              (send-text-mode-event (make-unknown-event 'keyboard (list page usage key)))))))
       (lp)]
      [('kbd . event)
       (log/warning "Unhandled keyboard event: " event)
       (send-text-mode-event (make-unknown-event 'keyboard event))
       (lp)])))

;;; FIXME: don't switch between repl and logging with a flag
(define (start-new-repl-window disp x y w h only-logging?)
  ;; FIXME: dynamic placement
  (define window
    (vl_display-allocate-user-surface disp VL_FORMAT_XRGB8888
                                      x y
                                      (+ 2 (* font-w w))
                                      (* font-h h)))

  (define replwin
    (make-replwin disp window Gray (make-channel) (make-channel)))

  #;
  (vl_buffer-draw-horizontal-gradient (vl_surface-buffer window)
                                      0 0 0   0 0 .58)

  (vl_display-add-window-surface! disp window)
  (vl_surface-draw-simple-decorations window)

  (let ((console (make-console (valand-textmode-backend replwin)))
        (outport (make-console-output-port Default (buffer-mode line) replwin))
        (errport (make-console-output-port LightRed (buffer-mode none) replwin)))

    (spawn-fiber
     (lambda ()
       (parameterize ([current-console console]
                      [current-output-port* outport]
                      [current-error-port* errport])
         (clrscr)
         (gotoxy 0 0)
         (text-color Gray)
         (if only-logging?
             (log-view console outport errport replwin)
             (tui console outport errport replwin)))))

    (spawn-fiber
     (lambda ()
       (parameterize ([current-console console]
                      [current-output-port* outport]
                      [current-error-port* errport])
         (repl-keyboard/mouse-driver replwin))))

    ;; FIXME: don't do this, the lag is noticable. Update ASAP, but
    ;; with a rate limit.
    (spawn-fiber
     (lambda ()
       (let lp ()
         (update-screen console)
         (sleep 1/10)
         (lp))))

    replwin)))
