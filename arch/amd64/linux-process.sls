;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Process-specific initialization for Linux

;; This library is responsible for initializing processes on Linux
;; (except schedulers). These are preemptible Loko processes. It
;; communicates with other processes (and its scheduler) by using
;; $process-yield.

;; TODO: Longer syscalls should be put on worker threads.

(library (loko arch amd64 linux-process)
  (export)
  (import
    (rnrs)
    (srfi :98 os-environment-variables)
    (only (loko) include/resolve)
    (loko match)
    (loko system unsafe)
    (loko runtime fibers)
    (loko runtime scheduler)
    (only (loko runtime init) init-set! install-vfs)
    (only (loko runtime scheduler) signal-signal!)
    (loko arch amd64 processes)
    (only (loko runtime io) $init-standard-ports $port-buffer-mode-set!
          port-file-descriptor-set! add-fdes-finalizer!)
    (only (loko runtime time) time-init-set!)
    (loko system $primitives)
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls))

(define (linux-open-i/o-poller)
  (define who 'linux-i/o-poller)
  (define NULL 0)
  (define maxevents 10)
  (define event-size sizeof-epoll_event)
  (define fds (make-eqv-hashtable))
  (define events (make-bytevector (* maxevents event-size)))
  (define num-waiting 0)
  (define (poll-type->events poll-type)
    ;; TODO: EPOLLPRI     ;out of band data
    (let ((flags (fxior EPOLLRDHUP           ;peer closed
                        EPOLLERR EPOLLHUP    ;can't be unset
                        EPOLLET              ;event-triggered
                        EPOLLONESHOT)))     ;one event only
      (case poll-type
        ((read) (fxior flags EPOLLIN))
        ((write) (fxior flags EPOLLOUT))
        (else flags))))
  (define (epoll_ctl epfd op fd events user-data)
    (if (eqv? op EPOLL_CTL_DEL)
        (sys_epoll_ctl epfd EPOLL_CTL_DEL fd NULL)
        (let ((event (make-bytevector event-size)))
          (bytevector-u32-native-set! event 0 events)
          (bytevector-u32-native-set! event 4 user-data)
          (sys_epoll_ctl epfd op fd (bytevector-address event)))))
  (define pollfd (sys_epoll_create1 EPOLL_CLOEXEC))
  (define (poll wakeup)
    (if (and (eqv? (hashtable-size fds) 0)
             (eq? wakeup 'no-wait))
        0               ;nothing to do
        (let ((timeout
               (cond ((eq? wakeup 'no-wait) 0)
                     ((eq? wakeup 'forever) -1)
                     (else
                      ;; Let's wait until the wakeup time. It doesn't
                      ;; matter if we wait shorter.
                      (max 0 (min 60000 (- wakeup (linux-current-ticks))))))))
          (if (and (eqv? (hashtable-size fds) 0)
                   (not (fx>=? timeout 0)))
              0  ;even more nothing to do
              (sys_epoll_pwait pollfd (bytevector-address events) maxevents
                               timeout NULL 0)))))
  (define (parse-event offset ret)
    (let ((events (bytevector-u32-native-ref events (fx+ offset offsetof-epoll_event-events)))
          (fd (bytevector-u32-native-ref events (fx+ offset offsetof-epoll_event-data))))
      (match (hashtable-ref fds fd #f)
        [(and #(readers writers) fd-data)
         ;; XXX: This never deletes fds from the epoll set.
         (let ((wakeup
                (cond ((not (eqv? 0 (fxand events (fxior EPOLLRDHUP EPOLLERR EPOLLHUP))))
                       ;; Notify everyone about errors
                       (vector-set! fd-data 0 '())
                       (vector-set! fd-data 1 '())
                       (append readers writers))
                      ((not (eqv? 0 (fxand events (fxior EPOLLIN EPOLLPRI))))
                       (vector-set! fd-data 0 '())
                       readers)
                      ((not (eqv? 0 (fxand events EPOLLOUT)))
                       (vector-set! fd-data 1 '())
                       writers)
                      (else
                       (error who "Unknown event on fd" pollfd fd events)))))
           (cond ((and (null? (vector-ref fd-data 0))
                       (null? (vector-ref fd-data 1)))
                  #f)
                 ((pair? (vector-ref fd-data 1))
                  (epoll_ctl pollfd EPOLL_CTL_MOD fd (poll-type->events 'write) fd))
                 ((pair? (vector-ref fd-data 0))
                  (epoll_ctl pollfd EPOLL_CTL_MOD fd (poll-type->events 'read) fd)))
           (append wakeup ret))]
        [else
         (error who "Event on unknown fd" pollfd fd)])))
  (define (parse-events n)
    (do ((i 0 (fx+ i 1))
         (offset 0 (fx+ offset event-size))
         (ret '() (parse-event offset ret)))
        ((fx=? i n) ret)))
  (define epoller
    (case-lambda
      (()
       num-waiting)
      ((wakeup)            ;wakeup = no-wait / forever / <timeout>
       (let ((ret (parse-events (poll wakeup))))
         (set! num-waiting (fx- num-waiting (length ret)))
         ret))
      ((cmd fd poll-type user-value)
       (case cmd
         ((add)
          ;; Add a file descriptor to the epoll set. Need to be
          ;; careful if the fd is already in there. Two fibers can
          ;; be waiting for the same fd.
          (let ((old-fd-data (hashtable-ref fds fd #f)))
            (match (or old-fd-data (vector '() '()))
              [(and #(readers writers) fd-data)
               (set! num-waiting (fx+ num-waiting 1))
               (if (eq? poll-type 'read)
                   (vector-set! fd-data 0 (cons user-value readers))
                   (vector-set! fd-data 1 (cons user-value writers)))
               (let* ((read-events (if (null? (vector-ref fd-data 0))
                                       0
                                       (poll-type->events 'read)))
                      (write-events (if (null? (vector-ref fd-data 1))
                                        0
                                        (poll-type->events 'write)))
                      (events (fxior read-events write-events)))
                 (cond ((not old-fd-data)
                        ;; XXX: closed fds are automatically
                        ;; removed from the epoll set.
                        (add-fdes-finalizer! fd (lambda (fd)
                                                  (hashtable-delete! fds fd)))
                        (hashtable-set! fds fd fd-data)
                        (epoll_ctl pollfd EPOLL_CTL_ADD fd events fd))
                       ((or (not (boolean=? (null? readers) (null? (vector-ref fd-data 0))))
                            (not (boolean=? (null? writers) (null? (vector-ref fd-data 1)))))
                        ;; The events mask has changed
                        (epoll_ctl pollfd EPOLL_CTL_MOD fd events fd))))])))
         ((close)
          (sys_close pollfd))
         (else
          (error who "Unhandled command" cmd))))))
  epoller)

(define (linux-current-ticks)
  ;; TODO: See if the vDSO is better for this
  (let* ((x (make-bytevector sizeof-timespec))
         (_ (sys_clock_gettime CLOCK_MONOTONIC (bytevector-address x)))
         (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
         (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
    (fx+ (fx* seconds 1000)
         (fxdiv nanoseconds #e1e+6))))

(define (linux-current-second)
  (let* ((x (make-bytevector sizeof-timespec))
         (_ (sys_clock_gettime CLOCK_TAI (bytevector-address x)))
         (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
         (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
    ;; FIXME: Once IEEE doubles are implemented, this should be inexact
    (+ seconds
       (/ nanoseconds #e1e+9))))

(define (linux-listen-signals)
  (define NULL 0)
  (let ((fd (let ((buf (make-bytevector sizeof-sigset_t 0))
                  (mask (fxior (fxarithmetic-shift-left 1 (- SIGCHLD 1))
                               (fxarithmetic-shift-left 1 (- SIGWINCH 1)))))
              (bytevector-uint-set! buf 0 mask (native-endianness) sizeof-sigset_t)
              (sys_signalfd4 -1 (bytevector-address buf) (bytevector-length buf)
                             (fxior SFD_NONBLOCK SFD_CLOEXEC)))))
    (let lp ()
      (wait-for-readable fd)
      (let ((buf (make-bytevector sizeof-signalfd_siginfo)))
        (when (eqv? (sys_read fd (bytevector-address buf) (bytevector-length buf))
                    (bytevector-length buf))
          (let ((signo (bytevector-u32-native-ref buf offsetof-signalfd_siginfo-ssi_signo)))
            (when (fx<? signo 256)
              (signal-signal! signo)))))
      (lp))))

(define (linux-get-random-seed n)
  (let ((buf (make-bytevector n)))
    ;; XXX: Requires Linux 3.17
    (let ((m (sys_getrandom (bytevector-address buf) (bytevector-length buf) 0)))
      (unless (fx=? m n)
        (error 'linux-get-random-seed "Short read" m)))
    buf))

(define (linux-process-setup)
  ;; XXX: Potential trouble here: https://cr.yp.to/unix/nonblock.html
  (define (set-fd-nonblocking fd)
    (let ((prev (sys_fcntl fd F_GETFL 0)))
      (when (eqv? 0 (fxand O_NONBLOCK prev))
        (sys_fcntl fd F_SETFL (fxior O_NONBLOCK prev)))))
  (define (terminal? fd)
    (let ((buf (make-bytevector sizeof-termios)))
      (and
        (let retry ()
          (sys_ioctl fd TCGETS (bytevector-address buf)
                     (lambda (errno)
                       (cond ((eqv? errno EINTR) (retry))
                             ((eqv? errno ENOTTY) #f)
                             (else
                              (raise
                                (condition
                                 (make-who-condition 'terminal?)
                                 (make-syscall-error 'ioctl fd 'TCGETS)
                                 (make-irritants-condition (list fd)))))))))
        #t)))
  (include/resolve ("loko" "runtime") "posixlib.scm")
  (unless (get-environment-variable "__AFL_SHM_ID")
    (set-fd-nonblocking STDIN_FILENO)
    (set-fd-nonblocking STDOUT_FILENO)
    (set-fd-nonblocking STDERR_FILENO))
  ($init-standard-ports (lambda (bv start count)
                          (posix-read STDIN_FILENO bv start count))
                        (lambda (bv start count)
                          (posix-write STDOUT_FILENO bv start count))
                        (lambda (bv start count)
                          (posix-write STDERR_FILENO bv start count))
                        (if (terminal? STDOUT_FILENO)
                            (buffer-mode line)
                            (buffer-mode block))
                        (eol-style lf))
  (port-file-descriptor-set! (current-input-port) STDIN_FILENO)
  (port-file-descriptor-set! (current-output-port) STDOUT_FILENO)
  (port-file-descriptor-set! (current-error-port) STDERR_FILENO)
  (init-set! 'open-i/o-poller linux-open-i/o-poller)
  (init-set! 'get-random-seed linux-get-random-seed)
  (install-vfs 'delete-file posix-delete-file
               'file-exists? posix-file-exists?
               'open-file posix-open-file
               'set-file-mode posix-set-file-mode)
  (time-init-set! 'current-time/process (lambda () (posix-clock_gettime CLOCK_THREAD_CPUTIME_ID)))
  (time-init-set! 'current-time/utc (lambda () (posix-clock_gettime CLOCK_REALTIME)))
  (time-init-set! 'current-time/monotonic (lambda () (posix-clock_gettime CLOCK_MONOTONIC)))
  (time-init-set! 'current-ticks linux-current-ticks)
  (time-init-set! 'current-second linux-current-second)
  (spawn-fiber linux-listen-signals))

(define (linux-process-init stage)
  (when (eq? stage 'pre-fibers)
    (init-set! 'exit (lambda (obj) (process-exit #f obj)))
    (init-set! 'emergency-exit (lambda (obj) (process-exit #f obj)))
    (init-set! 'command-line (get-command-line))
    (init-set! 'environment-variables (get-environment))
    (init-set! 'machine-type '#(amd64 linux))
    (let ((pid (get-pid)))
      (case (pid-id pid)
        ((1)
         (linux-process-setup))
        (else
         (error '$init-process "Internal error: no code for this pid" pid))))))

(when (and (eq? ($boot-loader-type) 'scheme)
           (eq? (get-boot-loader) 'linux))
  (init-set! 'init linux-process-init)))
