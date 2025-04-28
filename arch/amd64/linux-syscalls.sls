;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Mostly thin wrappers around Linux system calls

;; Rules for syscalls:
;; * Do not use any side effects in an argument to a sys_* form!
;; * Do not even allocate memory in an argument to a sys_* form!
;; * The error continuation is an exception to the above rules.

(library (loko arch amd64 linux-syscalls)
  (export
    make-syscall-error

    sys_accept4
    sys_bind
    sys_chdir
    sys_clock_getres
    sys_clock_gettime
    (rename (sys_close/finalizer sys_close))
    sys_connect
    ;; instead of sys_dup you can use (sys_fcntl fd F_DUPFD 0)
    sys_dup3
    sys_epoll_create1
    sys_epoll_ctl
    sys_epoll_pwait
    sys_execve
    sys_execveat
    sys_exit
    sys_exit_group
    sys_faccessat
    sys_fchdir
    sys_fchmod
    sys_fchmodat
    sys_fchown
    sys_fchownat
    sys_fcntl
    sys_fork
    sys_fstat
    sys_fstatfs
    sys_ftruncate
    sys_getcwd
    sys_getdents64
    sys_getegid
    sys_geteuid
    sys_getgid
    sys_getgroups
    sys_getpgid
    sys_getpid
    sys_getppid
    sys_getpriority
    sys_getrandom
    sys_getsid
    sys_getsockopt
    sys_getuid
    sys_io_uring_enter
    sys_io_uring_register
    sys_io_uring_setup
    sys_ioctl
    sys_kill
    sys_lchown
    sys_linkat
    sys_listen
    sys_lseek
    sys_lstat
    sys_madvise
    sys_mkdirat
    sys_mknodat
    sys_mmap
    sys_mprotect
    sys_munmap
    sys_open
    sys_openat
    sys_pipe2
    sys_preadv2
    sys_pwritev2
    sys_read
    sys_readlinkat
    sys_recvfrom
    sys_renameat2
    ;; sys_rmdir: use sys_unlinkat with AT_REMOVEDIR
    sys_rt_sigaction
    sys_rt_sigprocmask
    sys_sendto
    sys_setpgid
    sys_setpriority
    sys_setregid
    sys_setreuid
    sys_setsid
    sys_setsockopt
    sys_shmat
    sys_shmctl
    sys_shmdt
    sys_shmget
    sys_shutdown
    sys_sigaltstack
    sys_signalfd4
    sys_socket
    sys_stat
    sys_statfs
    sys_symlinkat
    sys_timer_create
    sys_timer_settime
    sys_truncate
    sys_umask
    sys_uname
    sys_unlinkat
    sys_utimensat
    sys_wait4
    sys_waitid
    sys_write
    )
  (import
    (rnrs (6))
    (srfi :198 private)
    (loko system unsafe)
    (only (loko system $host) call-fd-finalizer)
    (loko arch amd64 linux-numbers))

(define (make-syscall-error who errno)
  (cond ((and (fx<? -1 errno (vector-length errno-list)) (vector-ref errno-list errno))
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

;; TODO: extra "safe" variants of syscalls that check bytevector limits

(define-syntax define-syscall
  (lambda (x)
    (define (symcat prefix name)
      (string->symbol (string-append prefix (symbol->string (syntax->datum name)))))
    (syntax-case x ()
      ((_ (name arg ...))
       (with-syntax ([__NR (datum->syntax #'name (symcat "__NR_" #'name))]
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
                      (let ((v (syscall __NR arg ...)))
                        (if (fx<=? -4095 v -1)
                            (let ((errno (fx- v)))
                              ;; If EINTR is returned then the syscall
                              ;; was interrupted and should be
                              ;; retried. On Linux, close(2) may
                              ;; return EINTR, but the fd is closed
                              ;; all the same.
                              ;;
                              ;; See http://ewontfix.com/4/ and "Worse
                              ;; is Better" for a deeper explanation.
                              (if (eqv? errno EINTR) ;FIXME: Maybe yield-current-task here
                                  (if (eqv? __NR __NR_close)
                                      0   ;pretend it's all fine
                                      (retry))
                                  (raise (condition (make-syscall-error 'name errno)
                                                    (make-irritants-condition
                                                     (list arg ...))))))
                            v)))]
                 [(_ arg ... k-failure)
                  ;; This does not automatically handle EINTR; that's
                  ;; up to the caller. It could be useful when
                  ;; syscalls are intentionally interrupted.
                  #'(let ((v (syscall __NR arg ...)))
                      (if (fx<=? -4095 v -1)
                          (let ((errno (fx- v)))
                            (k-failure errno))
                          v))]
                 #;
                 [(_ arg ... k-failure k-success)
                  #'(let ((v (syscall __NR arg ...)))
                      (if (fx<=? -4095 v -1)
                          (k-failure (fx- v))
                          (k-success v)))]))))))))

(define-syscall (accept4 fd *upeer_sockaddr *upeer_addrlen flags))
(define-syscall (bind sockfd *umyaddr addrlen))
(define-syscall (chdir *filename))
(define-syscall (clock_getres clk-id *tp))
(define-syscall (clock_gettime clk-id *tp))
(define-syscall (close fd))
(define-syscall (connect fd *uservaddr addrlen))
(define-syscall (dup3 oldfd newfd flags))
(define-syscall (epoll_create1 flags))
(define-syscall (epoll_ctl epfd op fd *event))
(define-syscall (epoll_pwait epfd *events maxevents timeout *sigmask sigsetsize))
(define-syscall (execve *filename *argv *envp))
(define-syscall (execveat fd *filename *argv *envp flags))
(define-syscall (exit status))
(define-syscall (exit_group status))
(define-syscall (faccessat dfd *filename mode flags))
(define-syscall (fchdir fd))
(define-syscall (fchmod fd mode))
(define-syscall (fchmodat dfd *filename mode flags))
(define-syscall (fchown fd user group))
(define-syscall (fchownat dfd *filename user group flag))
(define-syscall (fcntl fd cmd maybe-arg))
(define-syscall (fork))
(define-syscall (fstat fd *statbuf))
(define-syscall (fstatfs fd *buf))
(define-syscall (ftruncate fd length))
(define-syscall (getcwd *buf size))
(define-syscall (getdents64 fd *dirent count))
(define-syscall (getegid))
(define-syscall (geteuid))
(define-syscall (getgid))
(define-syscall (getgroups gidsetsize *grouplist))
(define-syscall (getpgid pid))
(define-syscall (getpid))
(define-syscall (getppid))
(define-syscall (getpriority which who))
(define-syscall (getrandom *buf count flags))
(define-syscall (getsid pid))
(define-syscall (getsockopt fd level optname *optval *optlen))
(define-syscall (getuid))
(define-syscall (io_uring_enter fd to_submit min_complete flags *sig sigsz))
(define-syscall (io_uring_register fd opcode *arg nr_args))
(define-syscall (io_uring_setup entries *params))
(define-syscall (ioctl fd request *buf))
(define-syscall (kill pid sig))
(define-syscall (lchown *filename user group))
(define-syscall (linkat olddfd *oldname newdfd *newname flags))
(define-syscall (listen fd backlog))
(define-syscall (lseek fd offset whence))
(define-syscall (lstat *pathname *statbuf))
(define-syscall (madvise start len behavior))
(define-syscall (mkdirat dfd *pathname mode))
(define-syscall (mknodat dfd *pathname mode dev))
(define-syscall (mmap addr length prot flags fd offset))
(define-syscall (mprotect addr length prot))
(define-syscall (munmap addr length))
(define-syscall (open *pathname flags mode))
(define-syscall (openat dfd *pathname flags mode))
(define-syscall (pipe2 *filedes flags))
(define-syscall (preadv2 fd *vec vlen pos_l pos_h flags))
(define-syscall (pwritev2 fd *vec vlen pos_l pos_h flags))
(define-syscall (read fd *buf count))
(define-syscall (readlinkat dfd *path *buf bufsiz))
(define-syscall (recvfrom fd *ubuf size flags *addr *addr_len))
(define-syscall (renameat2 olddfd *oldname newdfd *newname flags))
(define-syscall (rt_sigaction signal *act *oact sigsetsize))
(define-syscall (rt_sigprocmask how *set *oldset sigsetsize))
(define-syscall (sendto fd *buff len flags *addr addr_len))
(define-syscall (setpgid pid pgid))
(define-syscall (setpriority which who niceval))
(define-syscall (setregid rgid egid))
(define-syscall (setreuid ruid euid))
(define-syscall (setsid))
(define-syscall (setsockopt fd level optname *optval optlen))
(define-syscall (shmat shmid *shmaddr shmflg))
(define-syscall (shmctl shmid cmd *buf))
(define-syscall (shmdt *shmaddr))
(define-syscall (shmget key size shmflg))
(define-syscall (shutdown fd how))
(define-syscall (sigaltstack *uss *uoss))
(define-syscall (signalfd4 ufd *user_mask sizemask flags))
(define-syscall (socket family type protocol))
(define-syscall (stat *pathname *statbuf))
(define-syscall (statfs *pathname *buf))
(define-syscall (symlinkat *oldname newdfd *newname))
(define-syscall (timer_create clock *event-spec created-id))
(define-syscall (timer_settime timer flags *new-setting *old-setting))
(define-syscall (truncate *path length))
(define-syscall (umask mask))
(define-syscall (uname *buf))
(define-syscall (unlinkat dfd *pathname flags))
(define-syscall (utimensat dfd *filename *utimes flags))
(define-syscall (wait4 pid *status options *rusage))
(define-syscall (waitid which id *infop options *ru))
(define-syscall (write fd *buf count))
)
