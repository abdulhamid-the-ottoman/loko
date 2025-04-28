;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Linux-specific initialization

;; At this point the standard library has been loaded and should be
;; available.

;; This code runs in pid 0. Fibers are not available here.

(library (loko arch amd64 netbsd-init)
  (export)
  (import
    (rnrs (6))
    (only (loko runtime init) init-set!)
    (loko system unsafe)
    (loko runtime buddy)
    (loko runtime elf)
    (loko runtime mmap)
    (loko arch amd64 common-init)
    (loko arch amd64 netbsd-numbers)
    (loko arch amd64 netbsd-syscalls)
    (loko arch amd64 processes)
    (only (loko runtime io) $init-standard-ports)
    (only (loko runtime scheduler) scheduler-loop)
    (except (loko system $host) $process-start)
    (loko system $primitives)
    (only (loko runtime context)
          CPU-VECTOR:ALTSIGSTK-BASE
          CPU-VECTOR:ALTSIGSTK-SIZE
          CPU-VECTOR:SCHEDULER-SP)
    (srfi :98 os-environment-variables))

;; ;; Less general than the libc counterpart
;; (define (timer-create clock-id signal)
;;   (let ((evp (make-bytevector sizeof-sigevent))
;;         (timer-id (make-bytevector 8 0)))
;;     (bytevector-u32-native-set! evp offsetof-sigevent-sigev_signo signal)
;;     (bytevector-u32-native-set! evp offsetof-sigevent-sigev_notify SIGEV_SIGNAL)
;;     (let nuts ()
;;       (unless (sys_timer_create clock-id
;;                                 (bytevector-address evp)
;;                                 (bytevector-address timer-id)
;;                                 (lambda (errno)
;;                                   (if (or (eqv? errno EAGAIN)
;;                                           (eqv? errno EINTR))
;;                                       #f
;;                                       (raise
;;                                         (make-syscall-error 'timer_create errno)))))
;;         (nuts)))
;;     (bytevector-u64-native-ref timer-id 0)))

;; ;; Less general than the libc counterpart
;; (define (timer-settime timer seconds nanoseconds)
;;   (let ((itimerspec (make-bytevector sizeof-itimerspec))
;;         (flags 0)
;;         (NULL 0))
;;     ;; FIXME: better code for the offsets
;;     (bytevector-u64-native-set! itimerspec #x00 seconds)
;;     (bytevector-u64-native-set! itimerspec #x08 nanoseconds)
;;     (bytevector-u64-native-set! itimerspec #x10 seconds)
;;     (bytevector-u64-native-set! itimerspec #x18 nanoseconds)
;;     (sys_timer_settime timer flags (bytevector-address itimerspec)
;;                        NULL)))

(define (netbsd-init-signal-handlers)
  (define NULL 0)
  (define (sigaction signal sa_sigaction sa_mask sa_flags)
    ;; __sigaction_sigtramp signum *nsa *osa *tramp vers
    (let ((buf (make-bytevector sizeof-sigaction)))
      (bytevector-u64-native-set! buf offsetof-sigaction-sa_sigaction sa_sigaction)
      (bytevector-uint-set! buf offsetof-sigaction-sa_mask sa_mask (native-endianness)
                            sizeof-sigset_t)
      (bytevector-s32-native-set! buf offsetof-sigaction-sa_flags sa_flags)
      (sys___sigaction_sigtramp signal (bytevector-address buf) NULL
                                ($linker-address '__sigtramp_siginfo_2) 2)))
  (define (sigprocmask how set)
    (let ((buf (make-bytevector sizeof-sigset_t)))
      (bytevector-u64-native-set! buf 0 set)
      (sys___sigprocmask14 how (bytevector-address buf) NULL)))
  (define (sigaltstack ss_sp ss_size ss_flags)
    (let ((buf (make-bytevector sizeof-sigaltstack)))
      (bytevector-s64-native-set! buf offsetof-sigaltstack-ss_sp ss_sp)
      (bytevector-u64-native-set! buf offsetof-sigaltstack-ss_size ss_size)
      (bytevector-s32-native-set! buf offsetof-sigaltstack-ss_flags ss_flags)
      (sys___sigaltstack14 (bytevector-address buf) NULL)))
  ;; Stack used when SA_ONSTACK is set
  (sigaltstack ($processor-data-ref CPU-VECTOR:ALTSIGSTK-BASE)
               ($processor-data-ref CPU-VECTOR:ALTSIGSTK-SIZE)
               0)
  ;; SIGPIPE is not needed because the syscalls that would raise it
  ;; will instead get errors that are handled properly.
  (sigprocmask SIG_BLOCK (fxarithmetic-shift-left 1 (- SIGPIPE 1)))
  (sigaction SIGBUS ($linker-address 'netbsd:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  ;; TODO: SIGSEGV should have the alternate stack, too (for stack overflow)
  (sigaction SIGSEGV ($linker-address 'netbsd:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  (sigaction SIGFPE ($linker-address 'netbsd:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  (sigaction SIGILL ($linker-address 'netbsd:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  (sigaction SIGURG ($linker-address 'netbsd:preempt) 0 (fxior SA_SIGINFO SA_RESTART SA_ONSTACK)))

;; Initialize the terminal (just for the scheduler)
(define (netbsd-init-terminal)
  ($init-standard-ports (lambda (bv start count)
                          (assert (fx<=? (fx+ start count) (bytevector-length bv)))
                          (sys_read STDIN_FILENO (fx+ (bytevector-address bv) start) count))
                        (lambda (bv start count)
                          (assert (fx<=? (fx+ start count)
                                         (bytevector-length bv)))
                          (sys_write STDOUT_FILENO (fx+ (bytevector-address bv) start) count))
                        (lambda (bv start count)
                          (assert (fx<=? (fx+ start count)
                                         (bytevector-length bv)))
                          (sys_write STDERR_FILENO (fx+ (bytevector-address bv) start) count))
                        (buffer-mode line)
                        (eol-style lf)))

;; Verify that #AC is working and triggers SIGBUS
(define (netbsd-init-check-alignment)
  (guard (exn (else #f))
    (get-mem-u32 #x200001)              ;safe way to trigger #AC
    (cond
      ((eqv? (valgrind #x1001) 0)
       (display "Fatal: Loko can't run because the system does not support #AC.\n"
                (current-error-port))
       (exit 70))
      (else
       (display "Warning: Valgrind does not emulate #AC, expect trouble!\n"
                (current-error-port))))))

(define (netbsd-init-process-data)
  (let-values ([(command-line environment-variables auxiliary-vector _stk-maxaddr)
                (elf-parse-stack-data ($boot-loader-data))])
    (init-set! 'command-line command-line)
    (init-set! 'environment-variables environment-variables)
    (init-set! 'auxiliary-vector auxiliary-vector)))

;; ;; Preemption timer
;; (define (netbsd-init-preemption-timer)
;;   ;; Create a timer that sends a SIGURG to the current thread at a
;;   ;; frequency of 50 Hz, but only counts up while the thread is
;;   ;; running.
;;   (let ((freq 1/50)
;;         (timer (timer-create CLOCK_THREAD_CPUTIME_ID SIGURG)))
;;     ;; The frequency of the SIGURG signals is at most once per
;;     ;; second.
;;     (let ((freq (if (>= freq 1) 999/1000 freq)))
;;       (let ((seconds 0)
;;             (nanoseconds (* freq (expt 10 9))))
;;         (timer-settime timer seconds nanoseconds)))))

(define (netbsd-init-heap memory-map)
  ;; Just a modest little heap for use with dma-allocate/dma-free.
  ;; FIXME: Bake this into the image
  (define tiny-heap
    (let ((base (* 4 1024 1024 1024))
          (size (* 2 1024 1024)))
      (mmap-mark! memory-map base size (fxior PROT_READ PROT_WRITE) 'dma-heap #f)
      (sys_mmap base size (fxior PROT_READ PROT_WRITE)
                (fxior MAP_PRIVATE MAP_FIXED MAP_ANONYMOUS)
                -1 0 0)
      (make-buddy base size 12)))
  (list tiny-heap))

(define (netbsd-init)
  (define memory-map (make-mmap (- (expt 2 47) 1)))
  (define (netbsd-mmap start length type)
    (sys_mmap start length
              (case type
                ((stack heap) (fxior PROT_READ PROT_WRITE))
                #;((trap) (mmap-protection 'PROT_NONE))
                ((text) (fxior PROT_READ PROT_WRITE PROT_EXEC))
                (else (error 'mmap "Unsupported type" type)))
              (case type
                ((stack) (fxior MAP_PRIVATE MAP_FIXED MAP_ANONYMOUS))
                ((heap text) (fxior MAP_PRIVATE MAP_FIXED MAP_ANONYMOUS))
                (else (error 'mmap "Unsupported type" type)))
              -1 0 0)
    (if #f #f))

  (init-set! 'machine-type '#(amd64 netbsd))
  (init-set! '$mmap netbsd-mmap)
  (init-set! 'exit (lambda (status)
                     ;; XXX: status must be a byte?
                     (let lp ()
                       (sys_exit (if (fixnum? status)
                                     status
                                     (if (not status) 1 0)))
                       (lp))))
  (netbsd-init-signal-handlers)
  (netbsd-init-terminal)
  (netbsd-init-check-alignment)
  (amd64-init-memory-map memory-map)
  (netbsd-init-process-data)
  ;; (netbsd-init-preemption-timer)
  (let ((buddies (netbsd-init-heap memory-map)))
    (scheduler-loop 'netbsd buddies memory-map)))

(when (eq? ($boot-loader-type) 'netbsd)
  (init-set! 'init (lambda (stage)
                     (netbsd-init)))))
