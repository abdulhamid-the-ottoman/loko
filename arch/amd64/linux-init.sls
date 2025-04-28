;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Linux-specific initialization

;; At this point the standard library has been loaded and should be
;; available.

;; This code runs in pid 0. Fibers are not available here.

(library (loko arch amd64 linux-init)
  (export)
  (import
    (rnrs (6))
    (only (loko runtime init) init-set! elf-auxiliary-vector)
    (loko system unsafe)
    (loko runtime buddy)
    (loko runtime elf)
    (loko runtime mmap)
    (loko arch amd64 common-init)
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls)
    (loko arch amd64 processes)
    (loko arch amd64 memory)
    (only (loko runtime io) $init-standard-ports)
    (only (loko runtime scheduler) scheduler-loop)
    (loko system $primitives)
    (only (loko system $host) valgrind)
    (only (loko runtime context)
          CPU-VECTOR:ALTSIGSTK-BASE
          CPU-VECTOR:ALTSIGSTK-SIZE
          CPU-VECTOR:SCHEDULER-SP)
    ;; (machine-code format elf)
    (srfi :98 os-environment-variables))

;; Less general than the libc counterpart
(define (timer-create clock-id signal)
  (let ((evp (make-bytevector sizeof-sigevent))
        (timer-id (make-bytevector 8 0)))
    (bytevector-u32-native-set! evp offsetof-sigevent-sigev_signo signal)
    (bytevector-u32-native-set! evp offsetof-sigevent-sigev_notify SIGEV_SIGNAL)
    (let nuts ()
      (unless (sys_timer_create clock-id
                                (bytevector-address evp)
                                (bytevector-address timer-id)
                                (lambda (errno)
                                  (if (or (eqv? errno EAGAIN)
                                          (eqv? errno EINTR))
                                      #f
                                      (raise
                                        (make-syscall-error 'timer_create errno)))))
        (nuts)))
    (bytevector-u64-native-ref timer-id 0)))

;; Less general than the libc counterpart
(define (timer-settime timer seconds nanoseconds)
  (let ((itimerspec (make-bytevector sizeof-itimerspec))
        (flags 0)
        (NULL 0))
    ;; FIXME: better code for the offsets
    (bytevector-u64-native-set! itimerspec #x00 seconds)
    (bytevector-u64-native-set! itimerspec #x08 nanoseconds)
    (bytevector-u64-native-set! itimerspec #x10 seconds)
    (bytevector-u64-native-set! itimerspec #x18 nanoseconds)
    (sys_timer_settime timer flags (bytevector-address itimerspec)
                       NULL)))

(define (linux-init-signal-handlers)
  (define NULL 0)
  (define put! bytevector-u64-native-set!)
  (define (sigaction signal sa_sigaction sa_mask sa_flags)
    (let ((buf (make-bytevector sizeof-sigaction)))
      (bytevector-u64-native-set! buf offsetof-sigaction-sa_handler sa_sigaction)
      (bytevector-uint-set! buf offsetof-sigaction-sa_mask sa_mask (native-endianness)
                            sizeof-sigset_t)
      (bytevector-s32-native-set! buf offsetof-sigaction-sa_flags (fxior SA_RESTORER sa_flags))
      (bytevector-s64-native-set! buf offsetof-sigaction-sa_restorer
                                  ($linker-address 'linux:signal-return))
      (sys_rt_sigaction signal (bytevector-address buf) NULL sizeof-sigset_t)))
  (define (sigprocmask how set)
    (let ((buf (make-bytevector sizeof-sigset_t)))
      (bytevector-u64-native-set! buf 0 set)
      (sys_rt_sigprocmask how (bytevector-address buf) NULL (bytevector-length buf))))
  (define (sigaltstack ss_sp ss_size ss_flags)
    (let ((buf (make-bytevector sizeof-sigaltstack)))
      (bytevector-s64-native-set! buf offsetof-sigaltstack-ss_sp ss_sp)
      (bytevector-u64-native-set! buf offsetof-sigaltstack-ss_size ss_size)
      (bytevector-s32-native-set! buf offsetof-sigaltstack-ss_flags ss_flags)
      (sys_sigaltstack (bytevector-address buf) NULL)))
  ;; Stack used when SA_ONSTACK is set
  (sigaltstack ($processor-data-ref CPU-VECTOR:ALTSIGSTK-BASE)
               ($processor-data-ref CPU-VECTOR:ALTSIGSTK-SIZE)
               0)
  (sigprocmask SIG_BLOCK
               (fxior (fxarithmetic-shift-left 1 (- SIGPIPE 1)) ;not used
                      (fxarithmetic-shift-left 1 (- SIGCHLD 1)) ;signalfd
                      (fxarithmetic-shift-left 1 (- SIGWINCH 1)))) ;signalfd
  (sigaction SIGBUS ($linker-address 'linux:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  ;; TODO: SIGSEGV should have the alternate stack, too (for stack overflow)
  (sigaction SIGSEGV ($linker-address 'linux:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  (sigaction SIGFPE ($linker-address 'linux:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  (sigaction SIGILL ($linker-address 'linux:signal-handler) 0 (fxior SA_SIGINFO SA_NODEFER))
  (sigaction SIGURG ($linker-address 'linux:preempt) 0 (fxior SA_SIGINFO SA_RESTART SA_ONSTACK)))

;; Initialize the terminal (just for the scheduler)
(define (linux-init-terminal)
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
(define (linux-init-check-alignment)
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

(define (linux-elf-init-process-data memory-map)
  (let-values ([(command-line environment-variables auxiliary-vector stk-maxaddr)
                (elf-parse-stack-data ($boot-loader-data))])
    (init-set! 'command-line command-line)
    (init-set! 'environment-variables environment-variables)
    (init-set! 'auxiliary-vector auxiliary-vector)
    (let* ((linux-stack-start (fxand ($boot-loader-data) -4096))
           (linux-stack-end (fxand (fx+ stk-maxaddr 4095) -4096))
           (linux-stack-size (- linux-stack-end linux-stack-start)))
      ;; Stop the grows-down stack segment from growing down.
      ;; (Carefully designed to not get a weird PROT_NONE segment that
      ;; nevertheless grows down before giving SEGV).
      (let ((unused (+ 1 (apply max (map area-top (mmap-filter->list values memory-map))))))
        (sys_munmap unused (- linux-stack-start unused))
        (sys_mmap (- linux-stack-start 8192) 8192 PROT_NONE
                  (fxior MAP_ANONYMOUS MAP_PRIVATE MAP_FIXED) -1 0))
      (let ((linux-pde (fxand linux-stack-start (- (* 2 1024 1024)))))
        ;; Reserve this page directory entry for Linux weirdness. It's
        ;; sad that Linux doesn't just come out and say what the
        ;; memory map it, this has to be parsed via /proc/self/maps,
        ;; which might not even be mounted.
        (mmap-mark! memory-map linux-pde (* 2 1024 1024) (fxior PROT_READ PROT_WRITE)
                    'linux #f)))))

(define (linux-init-vdso)
  (define (open-memory-input-port fn addr)
    (define pos 0)
    (define (read! bv start count)
      (do ((i 0 (fx+ i 1)))
          ((fx=? i count)
           (set! pos (fx+ pos i))
           count)
        (bytevector-u8-set! bv (fx+ start i) (get-mem-u8 (fx+ addr (fx+ pos i))))))
    (define (port-position) pos)
    (define (set-port-position! new-pos) (set! pos new-pos))
    (make-custom-binary-input-port fn read! port-position set-port-position! #f))
  ;; TODO: Use this for gettimeofday. Need to implement a basic
  ;; dynamic linker.
  '
  (cond ((assv AT_SYSINFO_EHDR (elf-auxiliary-vector)) =>
         (lambda (x)
           (let ((elf (open-elf-image (open-memory-input-port "vDSO" (cdr x)))))
             'TODO)))))

;; Setup for AFL, American Fuzzy Lop.
(define (linux-init-setup-afl)
  (define afl-map-size (expt 2 16))
  (define (afl-fork-server)
    (define FORKSRV_FD 198)
    (define SIZE 4)
    (define NULL 0)
    (define FS_OPT_ENABLED     #x80000001)
    (define FS_OPT_MAPSIZE     #x40000000)
    (define FS_OPT_SNAPSHOT    #x20000000)
    (define FS_OPT_AUTODICT    #x10000000)
    (define FS_OPT_SHDMEM_FUZZ #x01000000)
    (define FS_OPT_NEWCMPLOG   #x02000000)
    (let ((tmp (make-bytevector SIZE 0)))
      ;; Tell AFL that the fork server has started and let it know
      ;; some things about us.
      (bytevector-u32-native-set! tmp 0
                                  (fxior FS_OPT_ENABLED
                                         FS_OPT_MAPSIZE
                                         (fxarithmetic-shift-left (- afl-map-size 1) 1)))
      (when (eqv? (sys_write (+ FORKSRV_FD 1) (bytevector-address tmp) SIZE
                             (lambda (errno) #f))
                  SIZE)
        (let loop ()
          ;; Wait for a command and then fork a new process
          (unless (eqv? SIZE (sys_read FORKSRV_FD (bytevector-address tmp) SIZE))
            (exit 2))
          (let ((pid (sys_fork)))
            (cond ((eqv? pid 0)
                   ;; Child.
                   (put-mem-s61 ($linker-address 'afl-location) afl-map-size)
                   (sys_close FORKSRV_FD)
                   (sys_close (+ FORKSRV_FD 1)))
                  (else
                   (bytevector-u32-native-set! tmp 0 pid)
                   (sys_write (+ FORKSRV_FD 1) (bytevector-address tmp) SIZE)
                   (sys_wait4 pid (bytevector-address tmp) WUNTRACED NULL)
                   (sys_write (+ FORKSRV_FD 1) (bytevector-address tmp) SIZE)
                   (loop))))))))
  ;; Having "__AFL_SHM_ID" as a bytevector is necessary for AFL to
  ;; recognize that the binary is instrumented.
  (define __AFL_SHM_ID (utf8->string #vu8(95 95 65 70 76 95 83 72 77 95 73 68)))
  (let ((id (cond ((get-environment-variable __AFL_SHM_ID)
                   => string->number)
                  (else #f))))
    (when id
      (guard (exn (else #f))
        (sys_shmat id (fx+ afl-map-size ($linker-address 'afl-map)) SHM_REMAP))
      (afl-fork-server))))

;; Preemption timer
(define (linux-init-preemption-timer)
  ;; Create a timer that sends a SIGURG to the current thread at a
  ;; frequency of 50 Hz, but only counts up while the thread is
  ;; running.
  (let ((freq 1/50)
        (timer (timer-create CLOCK_THREAD_CPUTIME_ID SIGURG)))
    ;; The frequency of the SIGURG signals is at most once per
    ;; second.
    (let ((freq (if (>= freq 1) 999/1000 freq)))
      (let ((seconds 0)
            (nanoseconds (* freq (expt 10 9))))
        (timer-settime timer seconds nanoseconds)))))

;; Try to use huge pages (2 MB) and fall back to 4k pages.
(define linux-mmap-huge-maybe
  (let ((try-huge? #t))
    (lambda (addr length prot flags fd offset)
      (define (huge-page-multiple? n)
        (eqv? 0 (fxand n (- (* 2 1024 1024) 1))))
      (if (and #f  ;XXX: disabled because Linux hugetlb support seems pointless
               try-huge?
               (huge-page-multiple? addr)
               (huge-page-multiple? length))
          (sys_mmap addr length prot (fxior flags (fxior MAP_HUGETLB MAP_HUGE_2MB))
                    fd offset
                    (lambda (errno)
                      (set! try-huge? #f)
                      (sys_mmap addr length prot flags fd offset)))
          (sys_mmap addr length prot flags fd offset)))))

(define (linux-init-heap memory-map)
  ;; A heap for use with dma-allocate/dma-free on Linux. Would be
  ;; better if this could grow dynamically, but in practice this
  ;; largish heap uses very little memory until it is actually used.
  (define tiny-heap
    (let ((base (* 4 1024 1024 1024))
          (size (* 512 1024 1024)))
      (mmap-mark! memory-map base size (fxior PROT_READ PROT_WRITE) 'dma-heap #f)
      (linux-mmap-huge-maybe base size (fxior PROT_READ PROT_WRITE)
                             (fxior MAP_PRIVATE MAP_FIXED MAP_ANONYMOUS)
                             -1 0)
      (make-buddy base size 12)))
  (list tiny-heap))

(define (linux-init)
  (define memory-map (make-mmap (- (expt 2 47) 1)))
  (define (linux-mmap start length type)
    (let ((prot (case type
                  ((stack heap) (fxior PROT_READ PROT_WRITE))
                  #;((trap) (mmap-protection 'PROT_NONE))
                  ((text) (fxior PROT_READ PROT_WRITE PROT_EXEC))
                  (else (error 'mmap "Unsupported type" type)))))
      (linux-mmap-huge-maybe start length prot
                             (fxior MAP_PRIVATE MAP_FIXED MAP_ANONYMOUS)
                             -1 0)
      (mmap-mark! memory-map start length prot type #f))
    (if #f #f))

  (init-set! 'machine-type '#(amd64 linux))
  (init-set! '$mmap linux-mmap)
  (init-set! 'exit (lambda (status)
                     ;; XXX: status must be a byte?
                     (let lp ()
                       (sys_exit_group (if (fixnum? status)
                                           status
                                           (if (not status) 1 0)))
                       (lp))))
  (linux-init-signal-handlers)
  (linux-init-terminal)
  (linux-init-check-alignment)
  (amd64-init-memory-map memory-map)
  (linux-elf-init-process-data memory-map)
  (linux-init-vdso)
  (let ((buddies (linux-init-heap memory-map)))
    (linux-init-setup-afl)
    (linux-init-preemption-timer)

    (scheduler-loop 'linux buddies memory-map)))

(when (eq? ($boot-loader-type) 'linux)
  (init-set! 'init (lambda (stage)
                     (linux-init)))))
