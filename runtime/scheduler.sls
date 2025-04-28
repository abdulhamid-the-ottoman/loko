;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;;

(library (loko runtime scheduler)
  (export
    ;; Memory handling
    dma-allocate
    dma-free

    ;; IRQ handling
    enable-irq
    wait-irq-operation

    ;; POSIX signals
    signal-signal!
    enable-signal
    acknowledge-signal
    wait-signal-operation

    ;; Misc calls to the scheduler
    get-environment
    get-pid
    get-boot-loader
    get-boot-modules
    get-command-line
    scheduler-wait
    new-process
    new-usermode-process
    wait-process-operation
    process-exit
    process-resume
    out-of-memory

    ;; Process identifiers
    pid? pid-id

    ;; XXX: Stands to be cleaned up
    pc-current-ticks

    ;; Scheduler
    scheduler-loop)
  (import
    (rnrs (6))
    (loko match)
    (loko system $primitives)
    (loko system logging)
    (loko runtime buddy)
    (loko runtime fibers)
    (loko runtime mmap)
    (except (loko system $host) dma-allocate dma-free
            enable-irq acknowledge-irq wait-irq-operation
            signal-signal! enable-signal acknowledge-signal
            wait-signal-operation new-usermode-process
            process-exit process-resume wait-process-operation)
    (only (loko runtime context) CPU-VECTOR:SCHEDULER-RUNNING?
          CPU-VECTOR:SCHEDULER-SP)
    (prefix (srfi :98 os-environment-variables) srfi-98:)
    ;; XXX: should be cleaned up
    (only (loko system $x86) rdtsc))

(define (log/x severity . x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/error . x*) (apply log/x ERROR x*))
(define (wrt x)
  (call-with-string-output-port (lambda (p) (write x p))))

(define ($process-yield msg)
  (let ((sched-sp ($processor-data-ref CPU-VECTOR:SCHEDULER-SP)))
    ;; sched-sp is 0 if the scheduler is running
    ;; (display "Yielding back to SCHED-SP=")
    ;; (display (number->string sched-sp 16))
    ;; (newline)
    (when (eqv? sched-sp 0)
      (error '$process-yield "The scheduler tried to yield"))
    ;; IRQs should be disabled when the scheduler is running
    ($disable-interrupts)
    ($processor-data-set! CPU-VECTOR:SCHEDULER-RUNNING? #t) ;currently yielding
    (let ((msg ($switch-stack sched-sp msg)))
      ($processor-data-set! CPU-VECTOR:SCHEDULER-RUNNING? #f) ;no longer yielding
      ($enable-interrupts)    ;FIXME: should not be used under Linux or BSD
      ;; (display "Secret code from scheduler: ")
      ;; (write msg)
      ;; (newline)
      msg)))

(define subprocesses (make-eqv-hashtable))

(define-record-type pid
  (sealed #t) (opaque #f)
  (fields id
          ;; One pending received message, or #f
          (mutable msg)
          (mutable msg-cvar))
  (protocol
   (lambda (p)
     (lambda (id)
       (p id #f (make-cvar))))))

(define (get-pid)
  (let ((id ($process-yield '(get-pid))))
    (assert (fixnum? id))
    (make-pid id)))

(define (new-process)
  (let ((status ($process-yield (vector 'new-process #f))))
    (assert (eq? status 'ok))
    (make-pid (vector-ref status 1))))

(define (new-usermode-process ustate)
  (assert (fixnum? ustate))
  (let* ((msg (vector 'new-usermode-process #f ustate))
         (status ($process-yield msg)))
    (assert (eq? status 'ok))
    (let ((pid (make-pid (vector-ref msg 1))))
      (hashtable-set! subprocesses (pid-id pid) pid)
      pid)))

(define (process-resume pid)
  (assert (pid? pid))
  (let* ((msg (vector 'resume-process #f (pid-id pid)))
         (status ($process-yield msg)))
    (assert (eq? status 'ok))))

;; An operation that waits for an updated status on a pid. Only
;; supposed to be used by a single fiber. Returns 'syscall or '(trap .
;; <trap-number>).
(define (wait-process-operation pid)
  (wrap-operation (wait-operation (pid-msg-cvar pid))
                  (lambda _
                    (let ((msg (pid-msg pid)))
                      (pid-msg-set! pid #f)
                      (pid-msg-cvar-set! pid (make-cvar))
                      msg))))

(define (process-exit pid status)
  (assert (or (pid? pid) (not pid)))
  (let* ((msg (vector 'exit-process #f (and pid (pid-id pid)) status))
         (status ($process-yield msg)))
    (assert (eq? status 'ok))))

;;; Misc calls to the scheduler

(define (get-command-line)
  (let ((cmdline ($process-yield '(command-line))))
    (assert (list? cmdline))
    (map string-copy cmdline)))

(define (get-environment)
  (let ((env ($process-yield '(environment))))
    (assert (list? env))
    (map (lambda (var)
           (cons (string-copy (car var))
                 (string-copy (cdr var))))
         env)))

(define (get-boot-modules)
  (let ((modules ($process-yield '(boot-modules))))
    (assert (list? modules))
    (map (match-lambda
          [((? string? fn) _args (? fixnum? start) (? fixnum? len))
           (list (string-copy fn) '() start len)])
         modules)))

(define (get-boot-loader)
  ($process-yield '(boot-loader)))

(define (out-of-memory)
  ($process-yield '#(out-of-memory)))

(define pc-current-ticks
  (let ((clocks/tick #f))
    (lambda ()
      (unless clocks/tick
        (set! clocks/tick ($process-yield '(clocks/tick))))
      (fxdiv (rdtsc) clocks/tick))))

(define (dma-allocate size mask)
  (assert (fixnum? size))
  (assert (fixnum? mask))
  (let* ((v `#(allocate ,size ,mask #f))
         (s ($process-yield v)))
    (unless (eq? s 'ok)
      (error 'dma-allocate "Memory allocation failed" size mask))
    (let ((cpu-addr (vector-ref v 3)))
      cpu-addr)))

(define (dma-free addr)
  (assert (fixnum? addr))
  (let* ((v `#(free ,addr))
         (s ($process-yield v)))
    (unless (eq? s 'ok)
      (error 'dma-free "Memory release failed" addr))))

;; Makes a vector that is passed directly to the scheduler, and which
;; is filled in when the wait is over. If $process-yield returns
;; 'message then it is filled in with a message.
(define (make-wait-vector ns-timeout)
  (vector 'wait ns-timeout #f #f))

(define (scheduler-wait ns-timeout)
  ;; TODO: this message should take a number back from the scheduler
  ;; that indicates for how long it slept. This is so that if the
  ;; process is awoken by an uninteresting message it can go back to
  ;; sleeping with the original timeout without asking the scheduler
  ;; for the current time.
  (when ns-timeout
    (assert (fxpositive? ns-timeout)))
  (let ((vec (make-wait-vector ns-timeout)))
    (handle-scheduler-wait-reply vec ($process-yield vec)
                                 'scheduler-wait)))

;;; IRQ support

;; The handling of the interrupt controller is done in the scheduler.
;; This code gets notified by the scheduler when an interrupt has been
;; triggered, and tells the scheduler to enable or acknowledge an
;; interrupt. Multiple devices and driver can share an IRQ number, so
;; some mechanism is needed to support that. The solution here is to
;; assign sequence numbers to interrupts and require that every user
;; of this API passes around some state.

(define *interrupt-cvars* (make-vector 256 #f))
(define *interrupt-seq* (make-vector 16 0))
(define *interrupt-ack* (make-vector 16 -1))

(define (handle-scheduler-wait-reply vec reply who)
  ;; The scheduler will update vec.
  (case reply
    ((timeout)
     #f)
    ((message)
     (let ((msg (vector-ref vec 2))
           (payload (vector-ref vec 3)))
       (cond ((and (not payload)
                   (fixnum? msg)
                   (fx<? -1 msg (vector-length *interrupt-cvars*)))
              (cond
                ((vector-ref *interrupt-cvars* msg)
                 => (lambda (cvar)
                      ;; An IRQ was received by the processor. Wake up
                      ;; whatever fibers were waiting for it.
                      (vector-set! *interrupt-seq* msg (+ 1 (vector-ref *interrupt-seq* msg)))
                      (signal-cvar! cvar)))
                (else
                 (log/error "Unknown IRQ signalled: " msg))))
             (else
              (let ((process-id msg))
                (cond
                  ((hashtable-ref subprocesses process-id #f)
                   => (lambda (pid)
                        ;; A subprocess has been suspended due to a
                        ;; trap or syscall. Some fiber should be
                        ;; waiting around for this.
                        (pid-msg-set! pid payload)
                        (signal-cvar! (pid-msg-cvar pid))))
                  (else
                   (log/error "Unknown message from the scheduler: "
                              (wrt vec)))))))))
    (else
     (error who "Unknown reply from scheduler" reply))))

(define (enable-irq irq)
  (assert (fx<=? 0 irq 15))
  (unless (vector-ref *interrupt-cvars* irq)
    (vector-set! *interrupt-cvars* irq (make-cvar)))
  ($process-yield `#(enable-irq ,irq))
  ;; This is an IRQ token:
  ;; - IRQ number
  ;; - Sequence number being serviced
  (let ((ack (vector-ref *interrupt-ack* irq)))
    (vector irq ack)))

(define (acknowledge-irq token)
  (assert (vector? token))
  (let* ((irq (vector-ref token 0))
         (servicing (vector-ref token 1))
         (latest (vector-ref *interrupt-seq* irq))
         (latest-acked (vector-ref *interrupt-ack* irq)))
    ;; If the interrupt being serviced is later than the latest ack'd,
    ;; then ack everything up until the latest seen sequence number.
    (when (> servicing latest-acked)
      (vector-set! *interrupt-ack* irq latest)
      (vector-set! *interrupt-cvars* irq (make-cvar))
      (let ((vec (make-wait-vector 0)))
        (handle-scheduler-wait-reply vec ($process-yield `#(acknowledge-irq ,irq ,vec))
                                     'acknowledge-irq)))))

(define (wait-irq-operation token)
  (assert (vector? token))
  (let* ((irq (vector-ref token 0))
         (servicing (vector-ref token 1))
         (latest (vector-ref *interrupt-seq* irq)))
    (cond ((not (> latest servicing))
           ;; No new IRQ arrived since the one being serviced.
           ;; Wait for a new one.
           (acknowledge-irq token)
           (wrap-operation (wait-operation (vector-ref *interrupt-cvars* irq))
                           (lambda _
                             (vector-set! token 1 (vector-ref *interrupt-seq* irq))
                             token)))
          (else
           ;; A new IRQ has already arrived, there is no need to wait.
           (let ((cvar (make-cvar)))
             (signal-cvar! cvar)
             (wrap-operation (wait-operation cvar)
                             (lambda _
                               (vector-set! token 1 latest)
                               token)))))))

;;; POSIX signal support

(define *signal-cvars* *interrupt-cvars*)

(define (signal-signal! signo)
  (cond ((vector-ref *signal-cvars* signo) => signal-cvar!)))

(define (enable-signal signo)
  (unless (vector-ref *signal-cvars* signo)
    (vector-set! *signal-cvars* signo (make-cvar))))

(define (acknowledge-signal signo)
  (unless (vector-ref *signal-cvars* signo)
    (error 'acknowledge-signal "Signal not enabled" signo))
  (vector-set! *interrupt-cvars* signo (make-cvar)))

(define (wait-signal-operation signo)
  (wait-operation (vector-ref *signal-cvars* signo)))

;;; This is an actual scheduler implementation, pid 0

;; This is pid 0 on Linux and NetBSD. It is not a complete scheduler
;; at this point.

(define (scheduler-loop kernel buddies memory-map)
  ;; TODO: if msg is 'preempted then SIGURG delivery has been
  ;; disabled. When scheduling a process that was preempted there
  ;; is no need to manually unmask SIGURG. But if SIGURG is
  ;; blocked and we're scheduling a process that yielded or that
  ;; has not been started yet it is necessary to unmask SIGURG. If
  ;; SIGURG is unmasked and we're returning to a process that was
  ;; preempted it is likely necessary to mask SIGURG first, to
  ;; prevent a race condition. See sigprocmask.

  ;; TODO: most of the messages should have an implementation in
  ;; common with the one in pc-init, and probably the basic scheduling
  ;; as well.

  ;; TODO: more than one process

  ;; TODO: use epoll/kqueue here to do the equivalent of IRQs and HLT,
  ;; to allow fiber schedulers to exist in multiple processes
  (define (print-area x)
    (define (fmt-addr x) (and x (number->string x 16)))
    (define (print . x) (for-each display x) (newline))
    (define (fmt-human-readable-bytes x)
      (define k (* 1024))
      (define M (* k 1024))
      (define G (* M 1024))
      (define T (* G 1024))
      (cond ((> x (* 10 T)) (string-append (number->string (div x T)) "TB"))
            ((> x (* 10 G)) (string-append (number->string (div x G)) "GB"))
            ((> x (* 10 M)) (string-append (number->string (div x M)) "MB"))
            ((> x (* 10 k)) (string-append (number->string (div x k)) "kB"))
            (else (string-append (number->string x) "B"))))
    (let ((i (area-info x)))
      (print " [" (fmt-addr (area-base x)) "," (fmt-addr (area-top x)) "] "
             (fmt-human-readable-bytes (area-size x))
             ;; " {" (fmt-addr (meminfo-start i)) "+" (fmt-addr (meminfo-length i)) "}: "
             ": "(area-type x))))

  (let ((sp* ($process-start 1))
        (m* #f)
        (pid* 1)
        (will-unmask?* #f)
        (URG-masked #f))
    (define-syntax print
      (syntax-rules ()
        #;
        ((_ args ...) (begin (display args) ... (newline)))
        ((_ . args) (begin 'dummy))))
    (let lp ((sp* sp*))
      ;; (newline)
      ;; (for-each print-area (mmap-filter->list values memory-map))

      (print "In scheduler! SP=" (number->string sp* 16))
      ;; Switch back to the process
      (let ((msg ($switch-stack sp* m*)))
        (print "Message: " msg)
        (cond ((eq? msg 'preempted)
               ;; The SIGURG signal handler preempted the process,
               ;; so SIGURG is now masked.
               (set! URG-masked #t)
               (set! will-unmask?* #t))
              (else
               ;; The process yielded, which means SIGURG is
               ;; unmasked.
               (set! URG-masked #f)
               (set! will-unmask?* #f)))
        (when (vector? msg)
          (case (vector-ref msg 0)
            ((exit-process)
             (let ((pid (vector-ref msg 2))
                   (status (vector-ref msg 3)))
               ;; XXX: Can't use conditions as reason because they are
               ;; generative.
               (exit (if (fixnum? status) status (eqv? status #t)))))))
        (when (pair? msg)
          (case (car msg)
            ((new-process)
             ;; TODO:
             (set! m* 'ok))
            ((boot-loader)
             (set! m* kernel))
            ((command-line)
             ;; XXX: command-line and environment are extremely iffy.
             ;; Process must immediately copy the variables to its own
             ;; storage, before we do a GC here.
             (set! m* (command-line)))
            ((environment)
             (set! m* (srfi-98:get-environment-variables)))
            ((get-pid)
             (set! m* pid*))))
        (when (vector? msg)
          (case (vector-ref msg 0)
            ((allocate)
             (let ((size (vector-ref msg 1))
                   (mask (vector-ref msg 2)))
               ;; Allocate a consecutive memory region.
               (cond ((buddies-allocate! buddies size mask) =>
                      (lambda (addr)
                        (vector-set! msg 3 addr)
                        (set! m* 'ok)))
                     (else
                      (set! m* #f)))))
            ((free)
             (let ((addr (vector-ref msg 1)))
               ;; Deallocate a previously allocated region.
               (cond ((buddies-free! buddies addr) =>
                      (lambda (addr)
                        (set! m* 'ok)))
                     (else
                      (set! m* #f)))))
            ((out-of-memory)
             (display "Fatal error: out of heap memory\n" (current-error-port))
             (exit 1)))))
      (let ((sp ($processor-data-ref CPU-VECTOR:SCHEDULER-SP)))
        ($processor-data-set! CPU-VECTOR:SCHEDULER-SP 0)  ;indicate scheduler is running
        (lp sp))))))
