;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Process-specific initialization for bare metal

;; This library is responsible for initializing processes on bare
;; metal (except schedulers). These are preemptible Loko processes. It
;; communicates with other processes (and its scheduler) by using
;; $process-yield.

(library (loko arch amd64 pc-process)
  (export)
  (import
    (rnrs)
    (loko match)
    (loko system logging)
    (loko system unsafe)
    (only (loko runtime control) print-condition)
    (loko runtime fibers)
    (loko runtime scheduler)
    (only (loko runtime init) init-set! install-vfs)
    (only (loko runtime io) $init-standard-ports $port-buffer-mode-set!)
    (only (loko runtime time) time-init-set!)
    (loko system $primitives)
    (loko system $x86)
    (loko arch amd64 pc-acpi)
    (loko drivers acpi platform)
    (loko drivers uart ns8250))

(define (pc-acpi-setup)
  (let ((tables (pc-acpi-enumerate-tables (pc-acpi-find-rsdp-structure))))
    ;; Switch to I/O APIC, get the PCI routing table, do other
    ;; initialization.
    '
    (guard (exn
            ((serious-condition? exn)
             (send-log CRITICAL "Failed to initialize ACPI"
                       'EXCEPTION exn)))
      (acpi-initialize-board tables 'apic))))

(define (pc-com1-setup)
  ;; Start standard input/output on COM1
  (define debugcon #xe9)
  (define com1 #x3f8)
  (define com1-irq 4)
  (define com2 #x2f8)
  (define com2-irq 3)
  (let ((read-ch (make-channel))
        (write-ch (make-channel)))
    (spawn-fiber (lambda ()
                   (driver·uart com1 com1-irq read-ch write-ch)))
    (let ((read (lambda (bv start count)
                  (assert (fx>=? count 1))
                  (let ((b (get-message read-ch)))
                    (bytevector-u8-set! bv start b))
                  1))
          (write (lambda (bv start count)
                   (let ((ch (make-channel)))
                     (put-message write-ch (vector ch bv start count))
                     (get-message ch))))
          (debug-write (lambda (bv start count)
                         (do ((end (fx+ start count))
                              (i start (fx+ i 1)))
                             ((fx=? i end) count)
                           (put-i/o-u8 debugcon (bytevector-u8-ref bv i))))))
      ($init-standard-ports read write write (buffer-mode none)
                            (eol-style crlf)))))

;; Hook up a minimal /boot filesystem consisting of the multiboot
;; modules.
(define (pc-setup-boot-filesystem)
  (define boot-modules (get-boot-modules))
  (define (find-module filename)
    (find (lambda (mod)
            (let ((mod-fn
                   (if (and (not (equal? (car mod) ""))
                            (char=? #\/ (string-ref (car mod) 0)))
                       (string-append "/boot" (car mod))
                       (string-append "/boot/" (car mod)))))
              (string=? mod-fn filename)))
          boot-modules))
  (define (pc-file-exists? filename)
    (cond ((find-module filename) #t)
          ((member filename '("/" "/boot")) #t)
          (else #f)))
  (define (pc-open-file filename file-options buffer-mode who)
    (cond
      ((memq who '(open-file-output-port open-file-input/output-port))
       (error who "Not implemented" filename file-options buffer-mode))
      ((find-module filename) =>
       (lambda (mod)
         (let ((base (caddr mod)) (size (cadddr mod)) (position 0))
           (define (read! bv start count)
             (define remaining (fx- size position))
             (do ((n (fxmin count remaining))
                  (addr (fx+ base position) (fx+ addr 1))
                  (i start (fx+ i 1)))
                 ((fx=? i n)
                  (set! position (fx+ position n))
                  n)
               (bytevector-u8-set! bv i (get-mem-u8 addr))))
           (define (get-position)
             position)
           (define (set-position! off)
             (set! position (fxmin off size)))
           (define (close)
             (if #f #f))
           (let ((p (make-custom-binary-input-port
                     filename read! get-position set-position! close)))
             ($port-buffer-mode-set! p buffer-mode)
             p))))
      (else
       (raise (condition
               (make-who-condition who)
               (make-i/o-file-does-not-exist-error filename)
               (make-message-condition "Could not open boot module")
               (make-irritants-condition (list filename)))))))
  (install-vfs 'file-exists? pc-file-exists?
               'open-file pc-open-file))

(define (pc-open-i/o-poller)
  (define pc-poll
    (case-lambda
      (()
       ;; TODO: Count the number of IRQs being waited on
       1)
      ((wakeup)                ;wakeup = no-wait / forever / <timeout>
       (if (eq? wakeup 'no-wait)
           0                            ;nothing to do
           (let ((timeout
                  (cond ((eq? wakeup 'no-wait) 0)
                        ((eq? wakeup 'forever) 10000)
                        (else
                         ;; Let's wait until the wakeup time. It
                         ;; doesn't matter if we wait shorter.
                         (max 0 (min (- wakeup (pc-current-ticks))
                                     10000))))))
             (if (not (fx>? timeout 0))
                 0                      ;even more nothing to do
                 (scheduler-wait (* #e1e6 timeout)))))
       '())
      ((cmd . x)
       (unless (eq? cmd 'close)
         (apply error 'pc-poll "Unhandled command" x)))))
  pc-poll)

(define (pc-get-random-seed n)
  (define (fxseed)
    (let retry ((i 0))
      (if (fx=? i 100000)
          #f
          (let-values ([(valid? v) (rdseed)])
            (if valid? v (retry (fx+ i 1)))))))
  (define (fxseed-fill! bv)
    (let lp ((i 0))
      (if (fx=? i (bytevector-length bv))
          'ok
          (let ((seed (fxseed)))
            (cond ((not seed)
                   #f)
                  (else
                   (bytevector-u8-set! bv i (fxand seed #xff))
                   (lp (fx+ i 1))))))))
  (define (some-fixnum)
    (mod (* (let* ((a (guard (exn ((assertion-violation? exn) (rdtsc)))
                        (car #f)))
                   (b (guard (exn ((assertion-violation? exn) (rdtsc)))
                        (car '()))))
              (- b a))
            (let* ((a (rdtsc)) (b (rdtsc))) (- b a))
            (fxxor (rdtsc) (rdtsc))
            (* (rdtsc) (rdtsc)))
         1152921504606846043))
  (let ((buf (make-bytevector n))
        (rdseed-supported?
         (let-values ([(_eax ebx _ecx _edx) (cpuid #x07 #x00)])
           ;; CPUID.(EAX=07H, ECX=0H):EBX.RDSEED[bit 18] = 1
           (fxbit-set? ebx 18))))
    (cond
      ;; Try to use RDSEED first. Can fail if the instruction is
      ;; unavailable or if it isn't returning valid data.
      ((and rdseed-supported?
            (eq? 'ok (fxseed-fill! buf))))
      (else
       ;; No RDSEED. This will make a cryptographer blush. TODO: I
       ;; fully expect this to be replaced when replacing it becomes
       ;; important. It is used as a seed and it's just not random
       ;; enough.
       (do ((i 0 (fx+ i 1)))
           ((fx=? i n))
         (bytevector-u8-set! buf i (fxand (some-fixnum) #xff)))))
    buf))

;; The last moment the time was updated
(define pc-time-ticks 0)
(define pc-time-second 0)

(define (pc-current-time/utc)
  (let ((t (fx- (pc-current-ticks) pc-time-ticks)))
    (let-values ([(s ms) (fxdiv-and-mod t 1000)])
      (values (fx+ pc-time-second s) (fx* ms #e1e6)))))

(define (pc-set-current-time/utc s ticks)
  (set! pc-time-ticks ticks)
  (set! pc-time-second s))

(define (pc-process-init stage)
  (case stage
    ((pre-fibers)
     (init-set! 'exit (lambda (obj) (process-exit #f obj)))
     (init-set! 'command-line (get-command-line))
     (init-set! 'environment-variables (get-environment))
     (init-set! 'machine-type '#(amd64 pc))
     (init-set! 'open-i/o-poller pc-open-i/o-poller)
     (init-set! 'get-random-seed pc-get-random-seed)
     (time-init-set! 'current-time/utc pc-current-time/utc)
     (time-init-set! 'set-current-time/utc pc-set-current-time/utc)
     (time-init-set! 'current-ticks pc-current-ticks)
     (let ((pid (get-pid)))
       (case (pid-id pid)
         ((1)
          (pc-com1-setup)
          (pc-setup-boot-filesystem))
         (else
          (error '$init-process "Internal error: no code for this pid" pid)))))
    ((post-fibers)
     (case (pid-id (get-pid))
       ((1)
        (spawn-fiber (lambda ()
                       (pc-acpi-setup))))))))

(when (and (eq? ($boot-loader-type) 'scheme)
           (eq? (get-boot-loader) 'multiboot))
  (init-set! 'init pc-process-init)))
