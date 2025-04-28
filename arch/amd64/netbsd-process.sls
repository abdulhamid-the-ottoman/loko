;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Process-specific initialization for NetBSD

;; This library is responsible for initializing processes on NetBSD
;; (except schedulers). These are preemptible Loko processes. It
;; communicates with other processes (and its scheduler) by using
;; $process-yield.

;; TODO: Longer syscalls should be put on worker threads.

(library (loko arch amd64 netbsd-process)
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
    (loko arch amd64 processes)
    (only (loko runtime io) $init-standard-ports $port-buffer-mode-set!
          port-file-descriptor-set! add-fdes-finalizer!)
    (only (loko runtime time) time-init-set!)
    (loko system $primitives)
    (loko arch amd64 netbsd-numbers)
    (loko arch amd64 netbsd-syscalls))

(define (netbsd-open-i/o-poller)
  (define who 'netbsd-i/o-poller)
  (define NULL 0)
  (define maxevents 10)
  (define event-size sizeof-kevent)
  (define fds (make-eqv-hashtable))
  (define events (make-bytevector (* maxevents event-size)))
  (define num-waiting 0)
  (define-syntax sys_kevent (identifier-syntax sys___kevent50))
  (define (poll-type->filter poll-type)
    (case poll-type
      ((read) EVFILT_READ)
      ((write) EVFILT_WRITE)
      (else 0)))
  (let-values ([(pport pextract) (open-bytevector-output-port)])
    (define (kevent-put kq ident filter flags fflags data udata)
      (let ((ev (make-bytevector event-size)))
        (bytevector-u64-native-set! ev offsetof-kevent-ident ident)
        (bytevector-u32-native-set! ev offsetof-kevent-filter filter)
        (bytevector-u32-native-set! ev offsetof-kevent-flags flags)
        (bytevector-u32-native-set! ev offsetof-kevent-fflags fflags)
        (bytevector-s64-native-set! ev offsetof-kevent-data data)
        (bytevector-s64-native-set! ev offsetof-kevent-udata udata)
        (put-bytevector pport ev)))
    (define (fd-add fd for-read? for-write?)
      (when for-read?
        (kevent-put pollfd fd EVFILT_READ (fxior EV_ADD EV_ENABLE EV_DISPATCH) 0 0 0))
      (when for-write?
        (kevent-put pollfd fd EVFILT_WRITE (fxior EV_ADD EV_ENABLE EV_DISPATCH) 0 0 0)))
    (define pollfd (sys_kqueue1 (fxior O_CLOEXEC O_NOSIGPIPE)))
    (define (poll wakeup)
      (if (and (eqv? (hashtable-size fds) 0)
               (eq? wakeup 'no-wait))
          0                             ;nothing to do
          (let ((timeout
                 (cond ((eq? wakeup 'no-wait) 0)
                       ((eq? wakeup 'forever) -1)
                       (else
                        ;; Let's wait until the wakeup time. It
                        ;; doesn't matter if we wait shorter.
                        (max 0 (min 60000 (- wakeup (netbsd-current-ticks))))))))
            (if (and (eqv? (hashtable-size fds) 0)
                     (not (fx>=? timeout 0)))
                0                       ;even more nothing to do
                (let ((ts (if (eqv? timeout -1)
                              #f
                              (let-values ([(s ms) (div-and-mod timeout 1000)])
                                (let ((ts (make-bytevector sizeof-timespec)))
                                  (bytevector-u64-native-set! ts offsetof-timespec-tv_sec s)
                                  (bytevector-u64-native-set! ts offsetof-timespec-tv_nsec
                                                              (* #e1e6 ms))
                                  ts))))
                      (pending (pextract)))
                  (sys_kevent pollfd (bytevector-address pending)
                              (fxdiv (bytevector-length pending) event-size)
                              (bytevector-address events) maxevents
                              (if (bytevector? ts) (bytevector-address ts) NULL)))))))
    (define (parse-event offset ret)
      (let ((flags (bytevector-u32-native-ref events (fx+ offset offsetof-kevent-fflags)))
            (filter (bytevector-u32-native-ref events (fx+ offset offsetof-kevent-filter)))
            (fd (bytevector-u64-native-ref events (fx+ offset offsetof-kevent-ident))))
        (match (hashtable-ref fds fd #f)
          [(and #(readers writers) fd-data)
           (let ((wakeup
                  (cond ((not (eqv? 0 (fxand flags EV_ERROR)))
                         (error who "Error while polling" pollfd fd filter))
                        ((not (eqv? 0 (fxand flags 0)))
                         ;; Notify everyone about errors
                         (vector-set! fd-data 0 '())
                         (vector-set! fd-data 1 '())
                         (append readers writers))
                        ((eqv? filter EVFILT_READ)
                         (vector-set! fd-data 0 '())
                         readers)
                        ((eqv? filter EVFILT_WRITE)
                         (vector-set! fd-data 1 '())
                         writers)
                        (else
                         (error who "Unknown event on fd" pollfd fd filter)))))
             (when #f
               (cond ((and (null? (vector-ref fd-data 0))
                           (null? (vector-ref fd-data 1)))
                      #f)
                     ((pair? (vector-ref fd-data 1))
                      (fd-add fd #f 'write))
                     ((pair? (vector-ref fd-data 0))
                      (fd-add fd 'read #f))))
             (append wakeup ret))]
          [else
           (error who "Event on unknown fd" pollfd fd)])))
    (define (parse-events n)
      (do ((i 0 (fx+ i 1))
           (offset 0 (fx+ offset event-size))
           (ret '() (parse-event offset ret)))
          ((fx=? i n) ret)))
    (define poller
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
                 (let ((for-read? (not (null? (vector-ref fd-data 0))))
                       (for-write? (not (null? (vector-ref fd-data 1)))))
                   (cond ((not old-fd-data)
                          (add-fdes-finalizer! fd (lambda (fd)
                                                    (hashtable-delete! fds fd)))
                          (hashtable-set! fds fd fd-data)
                          (fd-add fd for-read? for-write?))
                         ((or (not (boolean=? (null? readers) (null? (vector-ref fd-data 0))))
                              (not (boolean=? (null? writers) (null? (vector-ref fd-data 1)))))
                          (fd-add fd for-read? for-write?))))])))
           ((close)
            (sys_close pollfd))
           (else
            (error who "Unhandled command" cmd))))))
    poller))

(define-syntax sys_clock_getres (identifier-syntax sys___clock_getres50))
(define-syntax sys_clock_gettime (identifier-syntax sys___clock_gettime50))

(define (netbsd-current-ticks)
  (let* ((x (make-bytevector sizeof-timespec))
         (_ (sys_clock_gettime CLOCK_MONOTONIC (bytevector-address x)))
         (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
         (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
    (fx+ (fx* seconds 1000)
         (fxdiv nanoseconds #e1e+6))))

(define (netbsd-process-setup)
  ;; XXX: Potential trouble here: https://cr.yp.to/unix/nonblock.html
  (define (set-fd-nonblocking fd)
    (let ((prev (sys_fcntl fd F_GETFL 0)))
      (when (eqv? 0 (fxand O_NONBLOCK prev))
        (sys_fcntl fd F_SETFL (fxior O_NONBLOCK prev)))))
  (define (terminal? fd)
    (let ((buf (make-bytevector sizeof-termios)))
      (and
        (let retry ()
          (sys_ioctl fd TIOCGETA (bytevector-address buf)
                     (lambda (errno)
                       (cond ((eqv? errno EINTR)
                              (retry))
                             ((eqv? errno ENOTTY)
                              #f)
                             (else
                              (raise
                                (condition
                                 (make-who-condition 'terminal?)
                                 (make-syscall-error 'ioctl fd 'TIOCGETA)
                                 (make-irritants-condition (list fd)))))))))
        #t)))
  (include/resolve ("loko" "runtime") "posixlib.scm")
  (set-fd-nonblocking STDIN_FILENO)
  (set-fd-nonblocking STDOUT_FILENO)
  (set-fd-nonblocking STDERR_FILENO)

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
  (init-set! 'open-i/o-poller netbsd-open-i/o-poller)
  (install-vfs 'delete-file posix-delete-file
               'file-exists? posix-file-exists?
               'open-file posix-open-file
               'set-file-mode posix-set-file-mode)
  (time-init-set! 'current-time/process (lambda () (posix-clock_gettime CLOCK_PROCESS_CPUTIME_ID)))
  (time-init-set! 'current-time/utc (lambda () (posix-clock_gettime CLOCK_REALTIME)))
  (time-init-set! 'current-ticks netbsd-current-ticks))

(define (netbsd-process-init stage)
  (when (eq? stage 'pre-fibers)
    (init-set! 'exit (lambda (obj) (process-exit #f obj)))
    (init-set! 'emergency-exit (lambda (obj) (process-exit #f obj)))
    (init-set! 'command-line (get-command-line))
    (init-set! 'environment-variables (get-environment))
    (init-set! 'machine-type '#(amd64 netbsd))
    (let ((pid (get-pid)))
      (case (pid-id pid)
        ((1)
         (netbsd-process-setup))
        (else
         (error '$init-process "Internal error: no code for this pid" pid))))))

(when (and (eq? ($boot-loader-type) 'scheme)
           (eq? (get-boot-loader) 'netbsd))
  (init-set! 'init netbsd-process-init)))
