;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2020 G. Weinholt
#!r6rs

;;; Mostly thin wrappers around NetBSD system calls

;; Rules for syscalls:
;; * Do not use any side effects in an argument to a sys_* form!
;; * Do not even allocate memory in an argument to a sys_* form!
;; * The error continuation is an exception to the above rules.

(library (loko arch amd64 netbsd-syscalls)
  (export
    make-syscall-error

    sys___clock_getres50
    sys___clock_gettime50
    sys___kevent50
    sys___sigaction_sigtramp
    sys___sigaltstack14
    sys___sigprocmask14
    (rename (sys_close/finalizer sys_close))
    sys_exit
    sys_faccessat
    sys_fchmodat
    sys_fcntl
    sys_fork
    sys_ioctl
    sys_kqueue1
    sys_lseek
    sys_mmap
    sys_munmap
    sys_openat
    sys_read
    sys_unlinkat
    sys_write
    )
  (import
    (rnrs (6))
    (srfi :198 private)
    (loko system unsafe)
    (only (loko system $host) call-fd-finalizer)
    (only (loko system $primitives) $syscall/carry!)
    (loko arch amd64 netbsd-numbers))

(define (make-syscall-error who errno)
  (cond ((and (fx<? errno (vector-length errno-list)) (vector-ref errno-list errno))
         => (lambda (x)
              (make-syscall-error* who errno (car x) (cdr x))))
        (else (make-syscall-error* who errno #f #f))))

;; FIXME: fcntl and dup3 can also close file descriptors
(define sys_close/finalizer
  (case-lambda
    ((fd)
     (call-fd-finalizer fd)
     (sys_close fd))
    ((fd k-failure)
     (call-fd-finalizer fd)
     (sys_close fd k-failure))))

(define-syntax define-syscall
  (lambda (x)
    (define (symcat prefix name)
      (string->symbol (string-append prefix (symbol->string (syntax->datum name)))))
    (syntax-case x ()
      ((_ (name arg ...))
       (with-syntax ([SYS_ (datum->syntax #'name (symcat "SYS_" #'name))]
                     [sys_name (datum->syntax #'name (symcat "sys_" #'name))])
         ;; Temporaries are not used for the arguments because the
         ;; error continuation may have called the GC and invalidated
         ;; the bytevector-address.
         #'(define-syntax sys_name
             (lambda (x)
               (syntax-case x ()
                 [(_ arg ...)
                  ;; Default: raise errors automatically and retry on
                  ;; EINTR (except for close).
                  #'(let retry ()
                      (let ((tmp (cons #f #f)))
                        ($syscall/carry! tmp SYS_ arg ...)
                        ;; FIXME: How does NetBSD/amd64 return errors?
                        (if (car tmp)
                            (let ((errno (cdr tmp)))
                              ;; Retry interrupted EINTR syscalls.
                              (if (eqv? errno EINTR) ;FIXME: Maybe yield-current-task here
                                  (retry)
                                  (raise (condition (make-syscall-error 'name errno)
                                                    (make-irritants-condition
                                                     (list arg ...))))))
                            (cdr tmp))))]
                 [(_ arg ... k-failure)
                  ;; This does not automatically handle EINTR; that's
                  ;; up to the caller. It could be useful when
                  ;; syscalls are intentionally interrupted.
                  #'(let ((tmp (cons #f #f)))
                      ($syscall/carry! tmp SYS_ arg ...)
                      (if (car tmp)
                          (let ((errno (cdr tmp)))
                            (k-failure errno))
                          (cdr tmp)))]))))))))

(define-syscall (__clock_getres50 clk-id *ts))
(define-syscall (__clock_gettime50 clk-id *ts))
(define-syscall (__kevent50 fd *changelist nchanges *eventlist nevents *timeout))
(define-syscall (__sigaction_sigtramp signum *nsa *osa *tramp vers))
(define-syscall (__sigaltstack14 *ss *oss))
(define-syscall (__sigprocmask14 how *set *oset))
(define-syscall (close fd))
(define-syscall (exit status))
(define-syscall (faccessat dfd *filename mode flags))
(define-syscall (fchmodat dfd *filename mode flags))
(define-syscall (fcntl fd cmd maybe-arg))
(define-syscall (fork))
(define-syscall (ioctl fd request *buf))
(define-syscall (kqueue1 flags))
(define-syscall (lseek fd offset whence))
(define-syscall (mmap addr len prot flags fd PAD pos))
(define-syscall (munmap addr length))
(define-syscall (openat dfd *pathname flags mode))
(define-syscall (read fd *buf count))
(define-syscall (unlinkat dfd *pathname flags))
(define-syscall (write fd *buf count))
)
