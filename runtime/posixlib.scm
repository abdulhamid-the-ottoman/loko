;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; POSIX runtime library

;; This code runs on POSIX systems via their syscall interfaces. The
;; functions themselves do not necessarily have anything to do with
;; POSIX, they just assume the host is a POSIX operating system.

(define (filename->c-string who fn)
  (string-for-each
   (lambda (c)
     (when (eqv? c #\nul)
       ;; The filename is not representable as a C string
       (raise (condition
               (make-who-condition who)
               (make-i/o-filename-error fn)))))
   fn)
  (string->utf8 (string-append fn "\x0;")))

(define (make-syscall-i/o-error errno filename attempted-write?)
  (let ((filename (string-copy filename)))
    (cond ((or (eqv? errno ENOENT) (eqv? errno ENOTDIR))
           (make-i/o-file-does-not-exist-error filename))
          ((or (eqv? errno EROFS) (eqv? errno ETXTBSY))
           (make-i/o-file-is-read-only-error filename))
          ((eqv? errno EACCES)
           (if attempted-write?
               (make-i/o-file-is-read-only-error filename)
               (make-i/o-file-protection-error filename)))
          ((eqv? errno EEXIST)
           (make-i/o-file-already-exists-error filename))
          (else
           (make-i/o-filename-error filename)))))

(define (posix-delete-file filename)
  (let ((fn (filename->c-string 'delete-file filename)))
    (let retry ()
      (sys_unlinkat AT_FDCWD (bytevector-address fn) 0
                    (lambda (errno)
                      (if (eqv? errno EINTR)
                          (retry)
                          (raise (condition
                                  (make-who-condition 'delete-file)
                                  (make-message-condition "Failed to delete the file")
                                  (make-irritants-condition (list filename))
                                  (make-syscall-i/o-error errno filename #f)
                                  (make-syscall-error 'delete-file errno))))))
      (values))))

(define (posix-file-exists? filename)
  ;; XXX: This follows symlinks.
  (define F_OK 0)
  (let* ((fn (filename->c-string 'file-exists? filename))
         (status
          (let retry ()
            (sys_faccessat AT_FDCWD (bytevector-address fn) F_OK 0
                           (lambda (errno)
                             (cond ((eqv? errno ENOENT) #f)
                                   ;; These are arguable. They hide errors in
                                   ;; some way.
                                   ((eqv? errno EACCES) #f)
                                   ((eqv? errno ENOTDIR) #f)
                                   ((eqv? errno EINTR) (retry))
                                   (else
                                    (raise
                                      (condition
                                       (make-who-condition 'file-exists?)
                                       (make-message-condition "Failed to check if the file exists")
                                       (make-irritants-condition (list filename))
                                       (make-syscall-i/o-error errno filename #f)
                                       (make-syscall-error 'faccessat errno))))))))))
    (eqv? 0 status)))

(define (posix-set-file-mode fn mode)
  (let ((path (filename->c-string 'set-file-mode fn)))
    (sys_fchmodat AT_FDCWD (bytevector-address path) mode 0)))

(define (posix-open-file filename file-options buffer-mode who)
  (define no-create (enum-set-member? 'no-create file-options))
  (define no-fail (enum-set-member? 'no-fail file-options))
  (define no-truncate (enum-set-member? 'no-truncate file-options))
  (define create (not no-create))
  (define fail (not no-fail))
  (define truncate (not no-truncate))
  (let* ((fn (filename->c-string 'open-file-input-port filename))
         (flags (case who
                  ((open-file-output-port open-file-input/output-port)
                   (if (and fail create)
                       (fxior O_CREAT O_EXCL)
                       (if truncate
                           (if (and no-fail create)
                               (fxior O_TRUNC O_CREAT)
                               O_TRUNC)
                           (if (and no-fail create)
                               O_CREAT
                               0))))
                  (else 0)))
         (access-mode (case who
                        ((open-file-output-port) O_WRONLY)
                        ((open-file-input-port) O_RDONLY)
                        ((open-file-input/output-port) O_RDWR)))
         (fd (let retry ()
               (sys_openat AT_FDCWD
                           (bytevector-address fn)
                           (fxior flags access-mode
                                  (fxior O_NOCTTY O_LARGEFILE O_NONBLOCK O_CLOEXEC))
                           #o644
                           (lambda (errno)
                             (if (eqv? errno EINTR)
                                 (retry)
                                 (raise (condition
                                         (make-who-condition who)
                                         (make-message-condition "Failed to open the file")
                                         (make-irritants-condition (list filename))
                                         (make-syscall-i/o-error errno filename #f)
                                         (make-syscall-error 'open errno))))))))
         (position 0))
    (define (handle-read-error errno)
      (raise
        (condition
         (make-syscall-i/o-error errno filename #f)
         (make-syscall-error 'read errno))))
    (define (read! bv start count)
      (assert (fx<=? (fx+ start count) (bytevector-length bv)))
      (let ((status (let retry ()
                      (sys_read fd (fx+ (bytevector-address bv) start) count
                                (lambda (errno)
                                  (cond ((eqv? errno EAGAIN)
                                         (wait-for-readable fd)
                                         (retry))
                                        ((eqv? errno EINTR)
                                         (retry))
                                        (else
                                         (handle-read-error errno))))))))
        (set! position (+ position status))
        status))
    (define (handle-write-error errno)
      (raise
        (condition
         (make-syscall-i/o-error errno filename #f)
         (make-syscall-error 'write errno))))
    ;; TODO! pwrite/pread
    (define (write! bv start count)
      (assert (fx<=? (fx+ start count) (bytevector-length bv)))
      (let ((status
             (let retry ()
               (sys_write fd (fx+ (bytevector-address bv) start) count
                          (lambda (errno)
                            (cond ((eqv? errno EAGAIN)
                                   (wait-for-writable fd)
                                   (retry))
                                  ((eqv? errno EINTR)
                                   (retry))
                                  (else
                                   (handle-write-error errno))))))))
        (set! position (+ position status))
        status))
    (define (get-position)
      position)
    (define (set-position! off)
      (sys_lseek fd off SEEK_SET)
      (set! position off))
    (define (close)
      ;; TODO: Obviously closing should be done in some automated
      ;; manner as well. And closing can block, so it should be done
      ;; in another thread. XXX: Linux always closes the fd even if
      ;; this fails with EINTR.
      (sys_close fd (lambda (errno)
                      ;; TODO: What to do on NetBSD?
                      (unless (eqv? errno EINTR)
                        (raise
                          (make-who-condition 'close-port)
                          (make-message-condition "Error while closing the file")
                          (make-syscall-i/o-error errno filename #f)
                          (make-syscall-error 'close errno))))))
    (let ((p (case who
               ((open-file-input-port)
                (make-custom-binary-input-port filename read!
                                               get-position set-position! close))
               ((open-file-output-port)
                (make-custom-binary-output-port filename write!
                                                get-position set-position! close))
               (else
                (make-custom-binary-input/output-port filename read! write!
                                                      get-position set-position! close)))))
      ($port-buffer-mode-set! p buffer-mode)
      (port-file-descriptor-set! p fd)
      p)))

(define (posix-read fd bv start count)
  (assert (fx<=? (fx+ start count) (bytevector-length bv)))
  ;; TODO: For Linux 4.14+, try preadv2() with RWF_NOWAIT
  (let retry ()
    (sys_read fd (fx+ (bytevector-address bv) start) count
              (lambda (errno)
                (cond ((eqv? errno EAGAIN)
                       (wait-for-readable fd)
                       (retry))
                      ((eqv? errno EINTR)
                       (retry))
                      (else
                       (raise
                         (condition (make-syscall-error 'read errno)
                                    (make-irritants-condition
                                     (list fd 'bv start count))))))))))

(define (posix-write fd bv start count)
  (assert (fx<=? (fx+ start count) (bytevector-length bv)))
  (let retry ()
    (sys_write fd (fx+ (bytevector-address bv) start) count
               (lambda (errno)
                 (cond ((eqv? errno EAGAIN)
                        (wait-for-writable fd)
                        (retry))
                       ((eqv? errno EINTR)
                        (retry))
                       (else
                        (raise
                          (condition (make-syscall-error 'write errno)
                                     (make-irritants-condition
                                      (list fd 'bv start count))))))))))

(define (posix-clock_gettime clock)
  (let* ((x (make-bytevector sizeof-timespec))
         (_ (sys_clock_gettime clock (bytevector-address x)))
         (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
         (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
    (values seconds nanoseconds)))

(define (posix-clock_getres clock)
  (let* ((x (make-bytevector sizeof-timespec))
         (_ (sys_clock_getres clock (bytevector-address x)))
         (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
         (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
    (values seconds nanoseconds)))
