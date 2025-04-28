;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019-2021 G. Weinholt
#!r6rs

;;; Multiboot-specific initialization

;; At this point the standard library has been loaded and should be
;; available.

;; This library is responsible for reading the boot loader data,
;; starting up processes and doing process scheduling.

;; Fibers do not work fully here.

;;; TODO: mmap must be able to manage virtual addresses itself (like
;;; Linux's mmap).

(library (loko arch amd64 pc-init)
  (export)
  (import
    (rnrs)
    (srfi :98 os-environment-variables)
    (loko dlists)
    (loko queues)
    (loko drivers acpi interrupts)
    (loko drivers early debugcon)
    (loko drivers early ns8250)
    (loko drivers early vga)
    (only (loko runtime init) init-set!)
    (loko system unsafe)
    (loko arch amd64 pc-acpi)
    (loko arch amd64 pc-ap-boot)
    (loko arch amd64 processes)
    (loko arch amd64 pc-ustate)
    (only (loko runtime context) CPU-VECTOR:SCHEDULER-SP CPU-VECTOR:LAST-INTERRUPT-VECTOR
          CPU-VECTOR:LAST-INTERRUPT-CODE CPU-VECTOR:LAST-FAULTING-ADDRESS)
    (only (loko runtime control) print-condition)
    (loko runtime mmap)
    (loko runtime buddy)
    (loko system $x86)
    (except (loko system $host) $process-start)
    (loko system $primitives)
    (only (loko) loko-version)
    (loko system logging)
    (loko runtime time)
    (only (loko runtime utils) string-index))

;; Boot modules.
;; (("filename" (args ...) base-address length) ...)
(define *boot-modules* '())

(define (log/x severity . x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*) (apply log/x DEBUG x*))
(define (log/info . x*) (apply log/x INFO x*))
(define (log/notice . x*) (apply log/x NOTICE x*))
(define (log/warning . x*) (apply log/x WARNING x*))
(define (log/emergency . x*) (apply log/x EMERGENCY x*))
(define (wrt x)
  (call-with-string-output-port (lambda (p) (write x p))))

(define (fmt-human-readable-bytes x)
  (define k (* 1024))
  (define M (* k 1024))
  (define G (* M 1024))
  (define T (* G 1024))
  (string-append
   (cond ((> x (* 10 T)) (string-append (number->string (div x T)) "TB"))
         ((> x (* 10 G)) (string-append (number->string (div x G)) "GB"))
         ((> x (* 10 M)) (string-append (number->string (div x M)) "MB"))
         ((> x (* 10 k)) (string-append (number->string (div x k)) "kB"))
         (else (string-append (number->string x) "B")))
   "/"
   ;; A quantum is Lisp data, such as a fixnum or a pointer to
   ;; something. Defined in "Address/Memory Management For A Gigantic
   ;; Lisp Environment or, GC Considered Harmful", Jon L White, 1980.
   (let ((x (fxdiv x 8)))
     (cond ((> x (* 10 T)) (string-append (number->string (div x T)) "TQ"))
           ((> x (* 10 G)) (string-append (number->string (div x G)) "GQ"))
           ((> x (* 10 M)) (string-append (number->string (div x M)) "MQ"))
           ((> x (* 10 k)) (string-append (number->string (div x k)) "kQ"))
           (else (string-append (number->string x) "Q"))))))

;; Loko syscalls
(define (sys_hlt)
  (syscall -1))

(define (sys_read-msr reg)
  (let ((bv (make-bytevector 8)))
    (syscall -2 reg (bytevector-address bv))
    (bytevector-u64-native-ref bv 0)))

(define (sys_write-msr reg v)
  (syscall -3 reg (bitwise-and v #xffffffff)
           (bitwise-arithmetic-shift-right v 32)))

(define (sys_urun ustate)
  (syscall -4 ustate))

(define (copy-utf8z addr)
  (do ((end addr (fx+ end 1))
       (len 0 (fx+ len 1)))
      ((fxzero? (get-mem-u8 end))
       (do ((ret (make-bytevector len))
            (i (fx- end 1) (fx- i 1))
            (len (fx- len 1) (fx- len 1)))
           ((fx<=? len -1) ret)
         (bytevector-u8-set! ret len (get-mem-u8 i))))))

;; Custom ports for reading from memory
(define (open-memory-input-port fn base size)
  (define (read! bv start count)
    (do ((n (fxmin count size))
         (addr base (fx+ addr 1))
         (i start (fx+ i 1)))
        ((fx=? i n)
         (set! base (fx+ base n))
         (set! size (fx- size n))
         n)
      (bytevector-u8-set! bv i (get-mem-u8 addr))))
  (define (close)
    #f)
  (transcoded-port (make-custom-binary-input-port fn read! #f #f close)
                   (native-transcoder)))

;; Parse the multiboot command line. Environment variables are
;; key=value before "--" and anything after is parsed as command line
;; arguments. Assumes that the very first item on the command line is
;; the name of the Multiboot binary.
;; Example:
;; (pc-init-parse-command-line "./loko repl=no x -- arg1 arg2")
;; => (("repl" . "no") ("x" . ""))
;;    ("./loko" "arg1" "arg2")
(define (pc-init-parse-command-line cmdline)
  (define (get-word p)
    (call-with-string-output-port
      (lambda (outp)
        (let lp ()
          (when (memv (lookahead-char p) '(#\space #\tab))
            (get-char p)
            (lp)))
        (let lp ()
          (let ((c (get-char p)))
            (unless (or (eof-object? c) (memv c '(#\space #\tab)))
              (put-char outp c)
              (lp)))))))
  (let ((p (open-string-input-port cmdline)))
    (let* ((cmdline0 (get-word p))
           (env (let lp ((var* '()))
                  (let ((str (get-word p)))
                    (if (or (string=? str "") (string=? str "--"))
                        (reverse var*)
                        (lp (cons (let ((idx (string-index str #\=)))
                                    (if (not idx)
                                        (cons str "")
                                        (cons (substring str 0 idx)
                                              (substring str (fx+ idx 1) (string-length str)))))
                                  var*))))))
           (cmdline (let lp ((var* '()))
                      (let ((str (get-word p)))
                        (if (string=? str "")
                            (reverse var*)
                            (lp (cons str var*)))))))
      (values env (cons cmdline0 cmdline)))))

;; Check that the CPU is supported.
(define (check-cpu)
  (let-values (((_a _b _c d) (cpuid 1)))
    ;; Check for Local APIC and RDMSR/WRMSR
    (unless (and (fxbit-set? d 9)
                 (fxbit-set? d 5))
      (error 'check-cpu
             "This kernel does not run on systems without a Local APIC and MSR" d))))

;;; PIC Interrupt controller code (i8259)

(define picm #x20)
(define pics #xA0)
(define *picm-mask* #b11111111)
(define *pics-mask* #b11111111)

(define (pic-init base idt-offset)
  (define cmd base)
  (define data (fx+ base 1))
  (define icw3 (if (fx=? base picm)
                   #b100                ;IRQ2 has a slave
                   2))                  ;slave ID
  (put-i/o-u8 cmd #b10001)              ;ICW1: ICW4 needed, edge-triggered
  (put-i/o-u8 data idt-offset)          ;ICW2: vector address
  (put-i/o-u8 data icw3)                ;ICW3
  (put-i/o-u8 data #b1)                 ;ICW4: Intel Architecture
  (OCW3 base 'set #f #f))               ;OCW3: special mask mode

(define (OCW1 base m)
  ;; Used to mask IRQs
  (put-i/o-u8 (fx+ base 1) m))

(define (OCW2 base rotate? specific? eoi? level)
  ;; Used to acknowledge IRQs
  (put-i/o-u8 base (fxior (if rotate?   #b10000000 0)
                          (if specific? #b1000000 0)
                          (if eoi?      #b100000 0)
                          (fxand level #b111))))

(define (OCW3 base special-mask-mode poll? reg)
  ;; Used to read IRR/ISR
  (put-i/o-u8 base (fxior (if poll?   #b100 0)
                          (case reg
                            ((ir) #b10)
                            ((is) #b11)
                            (else 0))
                          (case special-mask-mode
                            ((set)   #b1100000)
                            ((reset) #b1000000)
                            (else 0))
                          #b1000)))

(define (pic-mask base mask)
  ;; Bit 0 in the mask is IRQ 0 (or IRQ 8).
  (if (fx=? base picm)
      (OCW1 base (fxand mask #b11111011)) ;enable cascade
      (OCW1 base mask)))

(define (pic-seoi irq)
  ;; Signal End of Interrupt and set `irq' to the bottom
  ;; priority level.
  (cond ((fx<? irq 8)
         (OCW2 picm #t #t #t irq))
        (else
         ;; Slave + cascade level
         (OCW2 pics #t #t #t irq)
         (OCW2 picm #f #t #t 2))))

;; Read the In-Service Register, which says which IRQs are currently
;; being serviced, i.e. they were not acknowledged yet.
(define (pic-get-isr)
  (OCW3 picm #f #f 'is)
  (OCW3 pics #f #f 'is)
  (fxior (fxarithmetic-shift (get-i/o-u8 pics) 8)
         (get-i/o-u8 picm)))

;; Read the Interrupt Request Register, which says which IRQs are
;; pending to be sent.
(define (pic-get-irr)
  (OCW3 picm #f #f 'ir)
  (OCW3 pics #f #f 'ir)
  (fxior (fxarithmetic-shift (get-i/o-u8 pics) 8)
         (get-i/o-u8 picm)))

(define (pic-enable irq)
  (cond ((fx<? irq 8)
         (let ((bit (fxarithmetic-shift-left 1 irq)))
           (set! *picm-mask* (fxand *picm-mask* (fxnot bit)))
           (pic-mask picm *picm-mask*)))
        ((fx<? irq 16)
         (let ((bit (fxarithmetic-shift-left 1 (fx- irq 8))))
           (set! *pics-mask* (fxand *pics-mask* (fxnot bit)))
           (pic-mask pics *pics-mask*)))))

(define (pic-disable irq)
  (cond ((fx<? irq 8)
         (let ((bit (fxarithmetic-shift-left 1 irq)))
           (set! *picm-mask* (fxior *picm-mask* bit))
           (pic-mask picm *picm-mask*)))
        ((fx<? irq 16)
         (let ((bit (fxarithmetic-shift-left 1 (fx- irq 8))))
           (set! *pics-mask* (fxior *pics-mask* bit))
           (pic-mask pics *pics-mask*)))))

;;; Advanced PIC

;; The APIC base address MSR
(define APIC-MSR #x0000001B)
(define APIC-base-reg:AE 11)  ;APIC enabled
(define APIC-base-reg:BSC 8)  ;boot strap CPU core
;;; FIXME: use the MADT
(define ABA #xFEE00000)       ;default APIC base

;; APIC registers.
(define APIC:id (+ ABA #x20))
(define APIC:task-priority (+ ABA #x80))
(define APIC:logical-destination (+ ABA #xD0))
(define APIC:destination-format (+ ABA #xE0))
(define APIC:spurious-vector (+ ABA #xF0))
(define APIC:ICR-low (+ ABA #x300))
(define APIC:ICR-high (+ ABA #x310))
(define APIC:thermal-vector (+ ABA #x330))
(define APIC:performance-vector (+ ABA #x340))
(define APIC:LINT0-vector (+ ABA #x350))
(define APIC:LINT1-vector (+ ABA #x360))
(define APIC:error-vector (+ ABA #x370))

(define (check-apic-base)
  (unless (eqv? ABA (bitwise-and (sys_read-msr APIC-MSR) #x000FFFFFFFFFF000))
    (error 'check-apic-base
           "The Local APIC must not be relocated before boot")))

;; APIC register helpers
(define (write-spurious-int enable? spurious-vector)
  (define ASE (expt 2 8))     ;APIC Software Enable
  (define FCC (expt 2 9))     ;Focus CPU Core Checking
  (put-mem-u32 APIC:spurious-vector
               (fxior (fxand spurious-vector #xff)
                      (if enable? ASE 0))))

(define (write-timer-initial-count value)
  (put-mem-u32 (fx+ ABA #x380) value))

(define (read-timer-current-count)
  (get-mem-u32 (fx+ ABA #x390)))

(define (write-timer-divide divisor)
  ;; The CPU core clock divisor for the timer.
  (let ((value
         (case divisor
           ((1) #b111)
           ((2) #b000)
           ((4) #b001)
           ((8) #b010)
           ((16) #b011)
           ((32) #b100)
           ((64) #b101)
           ((128) #b110)
           (else
            (error 'write-timer-divide "Invalid divisor" divisor)))))
    (put-mem-u32 (fx+ ABA #x3E0)
                  (fxior
                   (fxarithmetic-shift-left (fxand value #b100) 1)
                   (fxand value #b11)))))

(define (write-timer-vector x)
  (put-mem-u32 (fx+ ABA #x320) x))

(define (apic-EOI)
  ;; End of interrupt. It acknowledges one interrupt.
  ;; Whichever one it was, I can't recall.
  (put-mem-u32 (fx+ ABA #xB0) 0))

(define LVT-MASK (expt 2 16))
(define LVT-TMM (expt 2 17))  ;timer mode (1 = periodic)
(define LVT-TGM (expt 2 15))  ;1 = level, 0 = edge
(define LVT-MT-FIXED    #b00000000000) ;vector field is used
(define LVT-MT-SMI      #b01000000000) ;SMI
(define LVT-MT-NMI      #b10000000000) ;NMI
(define LVT-MT-EXTERNAL #b11100000000) ;external interrupt
(define apic-divisor 32)               ;for the calibration

(define (calibrate-APIC&CPU-against-PIT)
  ;; i8253/i8254 PIT constants
  (define PIT-delay 1/100)    ;seconds to run calibration
  (define PIT-freq (+ 1193181 2/3))
  ;; (define PIT-count (round (* PIT-freq PIT-delay)))
  (define PIT-count 11932)
  ;; I/O ports for PIT
  (define COUNTER-0 #x40)     ;IRQ0
  (define COUNTER-1 #x41)
  (define COUNTER-2 #x42)     ;Speaker
  (define CONTROL #x43)
  ;; Control register bit definitions
  (define COUNT-BINARY 0)
  (define COUNT-BCD 1)
  (define MODE-INTERRUPT-ON-TERMINAL-COUNT #b000000)
  (define MODE-PROGRAMMABLE-ONESHOT        #b000010)
  (define MODE-RATE-GENERATOR              #b000100)
  (define MODE-SQUARE-WAVE-GENERATOR       #b001000)
  (define MODE-SOFTWARE-TRIGGERED-STROBE   #b010000)
  (define MODE-HARDWARE-TRIGGERED-STROBE   #b100000)
  (define LATCH-COUNTER       #o000)
  (define COUNTER-LOW         #o020)
  (define COUNTER-HIGH        #o040)
  (define COUNTER-WORD        #o060)
  (define SELECT-0            #o000)
  (define SELECT-1            #o100)
  (define SELECT-2            #o200)
  ;; NMI Status and Control Register bits
  (define NMI-S&C #x61)     ;NMI Status and Control Register
  (define NMI-MBZ      #b11110000) ;Written as zero
  (define TMR2-OUT-STS   #b100000)
  (define SPKR-DAT-EN    #b000010) ;Speaker Data Enable
  (define TIM-CNT2-EN    #b000001) ;Timer Counter 2 Enable
  ;; Disable the speaker and enable the timer counter 2
  ;; output bit. Then start the PIT timer.
  (put-i/o-u8 NMI-S&C
              (fxand (fxior (get-i/o-u8 #x61) TIM-CNT2-EN)
                     (fxnot (fxior NMI-MBZ SPKR-DAT-EN))))
  (put-i/o-u8 CONTROL
              (fxior COUNT-BINARY MODE-INTERRUPT-ON-TERMINAL-COUNT
                     COUNTER-WORD SELECT-2))
  (put-i/o-u8 COUNTER-2 (fxand PIT-count #xff))
  (put-i/o-u8 COUNTER-2 (fxarithmetic-shift-right PIT-count 8))
  (write-timer-vector APIC-vector-timer)
  (write-timer-initial-count #xffffffff) ;Start APIC counting
  (let ((initial-tsc (rdtsc)))
    ;; Wait for PIT to finish. No GC runs, please.
    (let lp ()
      (when (fxzero? (fxand (get-i/o-u8 NMI-S&C) TMR2-OUT-STS))
        (lp)))
    ;; Stop the timer
    (write-timer-vector LVT-MASK)
    ;; Finally read the APIC timer and calculate the bus frequency.

    ;; TODO: the bus frequency can vary from calibration to
    ;; calibration, but it is usually something like 200 MHz, 400 MHz,
    ;; 1 GHz, etc. Maybe it would be a good idea to try and round the
    ;; measured value if it's close to a multiple of 100 MHz?

    ;; TODO: what about running it multiple times and taking the minimums?

    ;; TODO: investigate what the divisor is for
    (let* ((current-count (read-timer-current-count))
           (current-tsc (rdtsc))) ;FIXME: can wrap

      ;; While we're messing with the PIT anyway, let's fix counter 0
      (put-i/o-u8 CONTROL (fxior COUNT-BINARY MODE-PROGRAMMABLE-ONESHOT
                                 COUNTER-WORD SELECT-0))
      (put-i/o-u8 COUNTER-0 0)
      (put-i/o-u8 COUNTER-0 0)

      (values (* (- #xffffffff current-count)
                 (* apic-divisor (/ PIT-delay)))
              (* (- current-tsc initial-tsc)
                 (/ PIT-delay))))))

;;; I/O APIC

(define IOREGSEL #x00)
(define IOWIN    #x10)

(define IOAPICID       #x00)            ;identification
(define IOAPICVER      #x01)            ;version
(define IOAPICARB      #x02)            ;arbitration
(define IOREDTBL-base  #x10)            ;start of the I/O redirection table

;; Definitions for entries in the I/O redirection table
(define IORED-DESTINATION-shift   56)
(define IORED-INTERRUPT-MASK      (expt 2 16)) ;interrupt mask
(define IORED-TRIGGER-MODE-EDGE   0)
(define IORED-TRIGGER-MODE-LEVEL  (expt 2 15))
(define IORED-REMOTE-IRR          (expt 2 14)) ;set to 1 when accepted by lapic, 0 on EOI
(define IORED-INTPOL-ACTIVE-HIGH  0)           ;input pin polarity (0=active high, 1=active low)
(define IORED-INTPOL-ACTIVE-LOW   (expt 2 13))
(define IORED-DELIVS              (expt 2 12)) ;delivery status (0=idle, 1=send pending)
(define IORED-DESTMOD-PHYSICAL    0)           ;destination mode (0=physical, 1=logical)
(define IORED-DESTMOD-LOGICAL     (expt 2 11))
(define IORED-DELMOD-shift        8)
(define IORED-DELMOD-Fixed        #b000)
(define IORED-DELMOD-LowestPrio   #b001)
(define IORED-DELMOD-SMI          #b010)
(define IORED-DELMOD-NMI          #b100)
(define IORED-DELMOD-INIT         #b101)
(define IORED-DELMOD-ExtINT       #b111)
(define IORED-INTVEC-shift        0)

(define (ioapic-read &ioapic reg)
  (assert (fx<=? 0 reg #xff))
  (put-mem-u32 &ioapic reg)
  (get-mem-u32 (fx+ &ioapic IOWIN)))

(define (ioapic-write &ioapic reg value)
  (assert (fx<=? 0 reg #xff))
  (put-mem-u32 &ioapic reg)
  (put-mem-u32 (fx+ &ioapic IOWIN) value))

(define (ioapic-ioredtbl-read &ioapic n)
  (bitwise-ior (bitwise-arithmetic-shift-left
                (ioapic-read &ioapic (fx+ (fx* n 2) (fx+ 1 IOREDTBL-base)))
                32)
               (ioapic-read &ioapic (fx+ (fx* n 2) IOREDTBL-base))))

(define (ioapic-ioredtbl-write &ioapic n v)
  (ioapic-write &ioapic (fx+ (fx* n 2) (fx+ 1 IOREDTBL-base))
                (bitwise-bit-field v 32 64))
  (ioapic-write &ioapic (fx+ (fx* n 2) IOREDTBL-base)
                (bitwise-bit-field v 0 32)))

(define (ioapic-max-redirection-entry &ioapic)
  (fxbit-field (ioapic-read &ioapic IOAPICVER) 16 24))

(define (print-madt-ioapic ioapic)
  (let* ((&ioapic (madt-ioapic-&addr ioapic))
         (num-entries (fx+ 1 (ioapic-max-redirection-entry &ioapic))))
    (log/debug "IOAPIC #" (madt-ioapic-id ioapic)
               " @ " (number->string &ioapic 16)
               ", base=" (madt-ioapic-int-base ioapic)
               ", ID=" (number->string (ioapic-read &ioapic IOAPICID) 16)
               ", VER=" (number->string (ioapic-read &ioapic IOAPICVER) 16)
               ", ARB=" (number->string (ioapic-read &ioapic IOAPICARB) 16))
    (do ((i 0 (fx+ i 1)))
        ((eqv? i num-entries))
      (log/debug "INTI" i ": " (number->string (ioapic-ioredtbl-read &ioapic i) 16)))))

;;; Interrupt setup

;; Intel reserves the first 32 interrupt vectors for exceptions
;; and such. Let's put the legacy IRQs immediately after that.
(define PIC-vector-offset 32)
(define PIC2-vector-offset (fx+ PIC-vector-offset 8))

;; Interrupt vector numbers. The spurious vector has the three lower
;; bits set to #b111.
(define APIC-vector-offset (+ PIC2-vector-offset 8))
(define APIC-vector-timer (+ APIC-vector-offset 1))
(define APIC-vector-spurious (+ APIC-vector-offset 15))

;; Start of the Global System Interrupts (GSI).
(define IOAPIC-vector-offset (+ APIC-vector-spurious 1))

(define GSI->IRQ #f)                    ;hashtable mapping a GSI to a IRQ number

(define (pc-setup-interrupts madt)
  ;; Opinion: they could have made this whole thing *much* simpler.
  (define lapic-id (fxbit-field (get-mem-u32 APIC:id) 24 32))

  (log/debug "Boot processor APIC ID: " (number->string lapic-id))

  ;; Configure the I/O APICs
  (when (madt? madt)
    (log/debug "MADT: lapic: " (number->string (madt-&lapic madt) 16)
               " flags: " (madt-flags madt))
    (for-each (lambda (entry)
                (log/debug "MADT entry: " entry))
              (madt-entries madt))

    (set! GSI->IRQ (make-eqv-hashtable))
    (let ((ioapic* (filter madt-ioapic? (madt-entries madt))))
      ;; Configure the redirection table
      (for-each
       (lambda (ioapic)
         (let* ((&ioapic (madt-ioapic-&addr ioapic))
                (num-entries (fx+ 1 (ioapic-max-redirection-entry &ioapic))))
           (do ((i 0 (fx+ i 1))
                (gsi (madt-ioapic-int-base ioapic) (fx+ gsi 1)))
               ((eqv? i num-entries))
             (hashtable-set! GSI->IRQ gsi gsi)
             (let* ((v (cond ((find (lambda (x)
                                      (and (madt-override? x)
                                           (fx=? gsi (madt-override-gsi x))))
                                    (madt-entries madt))
                              => (lambda (override)
                                   (fxior (case (madt-override-polarity override)
                                            ((bus-default) (if (fx<=? gsi 15)
                                                               IORED-INTPOL-ACTIVE-HIGH
                                                               IORED-INTPOL-ACTIVE-LOW))
                                            ((active-high) IORED-INTPOL-ACTIVE-HIGH)
                                            ((active-low) IORED-INTPOL-ACTIVE-LOW)
                                            (else 0))
                                          (case (madt-override-trigger-mode override)
                                            ((bus-default) (if (fx<=? gsi 15)
                                                               IORED-TRIGGER-MODE-EDGE
                                                               IORED-TRIGGER-MODE-LEVEL))
                                            ((edge-triggered) IORED-TRIGGER-MODE-EDGE)
                                            ((level-triggered) IORED-TRIGGER-MODE-LEVEL)
                                            (else 0)))))
                             ((fx<=? gsi 15)
                              (fxior IORED-TRIGGER-MODE-EDGE IORED-INTPOL-ACTIVE-HIGH))
                             (else
                              (fxior IORED-TRIGGER-MODE-LEVEL IORED-INTPOL-ACTIVE-LOW))))
                    (v (bitwise-ior v IORED-INTERRUPT-MASK
                                    (bitwise-arithmetic-shift-left lapic-id IORED-DESTINATION-shift)
                                    (fxarithmetic-shift-left IORED-DELMOD-Fixed IORED-DELMOD-shift)
                                    (fx+ IOAPIC-vector-offset gsi))))
               (ioapic-ioredtbl-write &ioapic i v)))))
       ioapic*)

      ;; The overrides say that an IRQ is not identity-mapped to a
      ;; GSI, and will show up as another GSI. Overrides also change
      ;; the polarity and trigger mode.
      (for-each
       (lambda (override)
         (when (madt-override? override)
           (hashtable-set! GSI->IRQ (madt-override-gsi override)
                           (madt-override-source override))))
       (madt-entries madt))

      (for-each print-madt-ioapic ioapic*))

    ;; Initialize the PICs and mask all IRQs.
    (when (madt-pc/at-compatible? madt)
      (log/debug "The system has legacy dual 8259s")
      (pic-init picm PIC-vector-offset)
      (pic-init pics PIC2-vector-offset)
      (pic-mask picm #b11111111)
      (pic-mask pics #b11111111)))

  ;; Initialize APIC
  (calibrate-APIC&CPU-against-PIT)
  (write-timer-initial-count 0) ;stop timer
  (put-mem-u32 APIC:task-priority 32)
  (put-mem-u32 APIC:logical-destination 0)
  (put-mem-u32 APIC:destination-format #xf0000000)
  (write-timer-vector LVT-MASK)
  (put-mem-u32 APIC:thermal-vector LVT-MASK)
  (put-mem-u32 APIC:performance-vector LVT-MASK)
  (put-mem-u32 APIC:LINT0-vector (fxior LVT-MT-EXTERNAL LVT-TGM))
  (put-mem-u32 APIC:LINT1-vector LVT-MT-NMI)
  (put-mem-u32 APIC:error-vector LVT-MASK)
  (put-mem-u32 APIC:ICR-high 0)
  (write-timer-divide apic-divisor)

  ;; Enable the APIC and disable anything not supported.
  (sys_write-msr APIC-MSR (bitwise-ior ABA (expt 2 APIC-base-reg:AE)))
  (write-spurious-int 'enable APIC-vector-spurious))

;;; Scheduler

(define-record-type pcb
  (sealed #t)
  (fields (immutable pid)
          (mutable sp)               ;if usermode?=#t then this is ustate.
          (mutable status)
          (mutable msg)
          (mutable wakeup)
          (mutable wait-vector)
          queue
          usermode?
          parent
          (mutable dnode))
  (protocol
   (lambda (p)
     (lambda (pid sp usermode? parent)
       (p pid sp 'runnable #f #f #f (make-queue) usermode? parent #f)))))

(define (pcb-enqueue-message! pcb msg)
  ;; (log/debug "Message to " (pcb-pid pcb) ":" (wrt msg))
  (enqueue! (pcb-queue pcb) msg))

;; The wait vector was allocated by make-wait-vector and lives in
;; pcb's heap. Take one message from the queue and place it in the
;; wait vector. Be careful with these; the wait vector mustn't contain
;; any pointers from the scheduler's heap.
(define (pcb-dequeue-message! pcb wv)
  (let ((msg (dequeue! (pcb-queue pcb))))
    (cond ((pair? msg)
           (vector-set! wv 2 (car msg))
           (vector-set! wv 3 (cdr msg)))
          (else
           (vector-set! wv 2 msg)
           (vector-set! wv 3 #f)))))

(define (nanoseconds->TSC cpu-freq t)
  ;; Converts from nanoseconds to CPU cycles.
  (round (* t (/ cpu-freq (expt 10 9)))))

(define (TSC->nanoseconds cpu-freq t)
  (div (* t (expt 10 9)) cpu-freq))

;; Pid 0 for Loko on PC
(define (pc-scheduler cpu-freq interval buddies)
  (define DEBUG #f)
  (define *runq* (make-dlist))
  (define *waitq* '())
  (define *irq-vectors* (make-vector 256 #f))
  (define *next-pid* 2)
  (define *pids* (make-eqv-hashtable))
  (define (get-saved-sp!)
    ;; After $switch-stack has returned to the scheduler
    ;; this is where the stack pointer of the other
    ;; process is stored.
    (let ((sp ($processor-data-ref CPU-VECTOR:SCHEDULER-SP)))
      ($processor-data-set! CPU-VECTOR:SCHEDULER-SP 0) ;indicate scheduler is running
      sp))
  (define (get-saved-irq!)
    ;; The returned value is the interrupt vector number
    ;; if the interrupt that happened during the time a
    ;; process was scheduled or at the time the HLT
    ;; instruction was issued.
    (let ((IRQ ($processor-data-ref CPU-VECTOR:LAST-INTERRUPT-VECTOR)))
      ($processor-data-set! CPU-VECTOR:LAST-INTERRUPT-VECTOR #f)
      IRQ))
  (define (handle-single-PIC-irq irq poll?)
    (cond
      ((and (eqv? irq 7) (not (fxbit-set? (pic-get-isr) 7)))
       ;; Spurious IRQ7
       #f)
      ((and (eqv? irq 15) (not (fxbit-set? (pic-get-isr) 15)))
       ;; Spurious IRQ15 from pics, but picm thinks IRQ 2
       ;; actually happened so it needs to be acked.
       (pic-seoi 2))
      ((vector-ref *irq-vectors* (fx+ irq PIC-vector-offset)) =>
       (lambda (pcb)
         ;; TODO: IRQ sharing
         (when DEBUG
           (log/debug "PIC IRQ: " irq))
         ;; Disable and acknowledge this IRQ. We use special mask mode.
         (pic-disable irq)
         (pic-seoi irq)
         (pcb-enqueue-message! pcb irq)))
      ((not poll?)
       ;; Unwanted IRQ. Disable it until some process enables it.
       (pic-disable irq)
       (pic-seoi irq)
       (log/debug "Spurious 8259 IRQ: " irq))))
  (define (handle-IOAPIC-irq vec gsi irq)
    (when DEBUG
      (log/debug "IOAPIC GSI: " gsi " IRQ: " irq))
    #f)
  (define (handle-irq)
    (let ((vec (get-saved-irq!)))
      ;; First make runnable any process that was waiting for this
      ;; IRQ. If a runnable process would receive an IRQ then it must
      ;; be queued as a message.
      (when (fixnum? vec)
        (cond ((fx<? vec 32))           ;trap or syscall
              ((eqv? vec APIC-vector-timer)
               (apic-EOI))
              ((fx<=? PIC-vector-offset vec (fx+ PIC-vector-offset 15))
               (handle-single-PIC-irq (fx- vec PIC-vector-offset) #f))
              ((eqv? vec APIC-vector-spurious)
               ;; XXX: if the spurious interrupts come
               ;; without end, then probably an
               ;; interrupt in the APIC has not received
               ;; its EOI. Should increment a counter.
               ;; (display #\S)
               ;; (flush-output-port (current-output-port))
               #f)
              ((and (fx>=? vec IOAPIC-vector-offset)
                    (hashtable-ref GSI->IRQ (fx- vec IOAPIC-vector-offset) #f))
               => (lambda (irq)
                    (handle-IOAPIC-irq vec (fx- vec IOAPIC-vector-offset) irq)))
              (else
               (log/debug "Spurious interrupt: #x" (number->string vec 16)))))))
  (define (handle-wait-queue current-time)
    ;; Make runnable any process where there is a message in the queue
    ;; or the wakeup time is in the past. TODO: use (pfds heaps).
    (set! *waitq*
          (filter (lambda (pcb)
                    (let ((wakeup (pcb-wakeup pcb))
                          (wv (pcb-wait-vector pcb)))
                      (unless (eq? 'wait (pcb-status pcb))
                        (error 'scheduler
                               "Non-waiting process in the wait queue"
                               pcb))
                      (unless (vector? wv)
                        (error 'scheduler
                               "A process is in the wait queue without a wait vector"
                               (pcb-pid pcb)))
                      (cond ((not (queue-empty? (pcb-queue pcb)))
                             (when wakeup
                               (vector-set! wv 1 (TSC->nanoseconds
                                                  cpu-freq
                                                  (- wakeup current-time))))
                             (pcb-dequeue-message! pcb wv)
                             (pcb-move-to-runnable! pcb 'message)
                             #f)
                            ((not wakeup) #t)
                            ((<= current-time wakeup) #t)
                            (else
                             (vector-set! wv 1 (TSC->nanoseconds
                                                cpu-freq
                                                (- wakeup current-time)))
                             (pcb-move-to-runnable! pcb 'timeout)
                             #f))))
                  *waitq*)))

  (define (pcb-move-to-waiting! pcb msg)
    #;
    (log/debug "Moving to waiting " (pcb-pid pcb))
    (assert (pcb-dnode pcb))
    (pcb-status-set! pcb 'wait)
    (pcb-wait-vector-set! pcb msg)
    (dlist-remove! *runq* (pcb-dnode pcb))
    (pcb-dnode-set! pcb #f)
    (set! *waitq* (cons pcb *waitq*)))

  (define (pcb-add-to-runnable! pcb)
    #;
    (log/debug "Adding to runnable " (pcb-pid pcb))
    (assert (not (pcb-dnode pcb)))
    (let ((dnode (dlist-prepend! *runq* pcb)))
      (pcb-dnode-set! pcb dnode))
    (hashtable-set! *pids* (pcb-pid pcb) pcb))

  (define (pcb-move-to-runnable! pcb msg)
    #;
    (log/debug "Moving to runnable " (pcb-pid pcb))
    (when (eq? (pcb-status pcb) 'wait)
      (set! *waitq* (remq pcb *waitq*)))
    (assert (not (pcb-dnode pcb)))
    (pcb-dnode-set! pcb (dlist-prepend! *runq* pcb))
    (pcb-status-set! pcb 'runnable)
    (pcb-wakeup-set! pcb #f)
    (pcb-msg-set! pcb msg))

  (define (pcb-remove! pcb)
    ;; TODO: Free the memory allocated to the target process
    #;
    (log/debug "Remove " (pcb-pid pcb))
    (when (eq? (pcb-status pcb) 'wait)
      (set! *waitq* (remq pcb *waitq*)))
    (cond ((pcb-dnode pcb) =>
           (lambda (dnode)
             (dlist-remove! *runq* dnode))))
    (pcb-dnode-set! pcb #f)
    (pcb-status-set! pcb 'dead)
    (hashtable-delete! *pids* (pcb-pid pcb)))

  (define (pcb-suspend! pcb)
    #;
    (log/debug "Suspending " (pcb-pid pcb))
    (dlist-remove! *runq* (assert (pcb-dnode pcb)))
    (pcb-dnode-set! pcb #f)
    (pcb-status-set! pcb 'suspend))

  (define (handle-message pcb msg current-time)
    ;; These messages should be cleaned up already by wrappers around
    ;; $process-yield, so no error catching is necessary here.
    (when DEBUG
      (cond ((equal? msg '(current-ticks))
             (display #\T)
             (flush-output-port (current-output-port)))
            (else
             (log/debug (fxdiv (rdtsc) (fxdiv cpu-freq 1000))
                        (pcb-pid pcb) 'isr (number->string (pic-get-isr) 2)
                        'irr (number->string (pic-get-irr) 2)
                        'msg: msg))))
    (when (vector? msg)
      (case (vector-ref msg 0)
        ((send)
         (let ((recipient (vector-ref msg 1))
               (msg (vector-ref msg 2)))
           (cond ((hashtable-ref *pids* recipient #f) =>
                  (lambda (target)
                    (pcb-enqueue-message! target msg)
                    (pcb-msg-set! pcb 'ok)))
                 (else
                  ;; There is no such process...
                  (pcb-msg-set! pcb #f)))))
        ((wait)
         (let ((timeout (vector-ref msg 1)))
           (cond ((queue-empty? (pcb-queue pcb))
                  (when timeout
                    ;; Minimum wait in nanoseconds, unless awoken by a
                    ;; message.
                    (let ((wakeup (+ current-time
                                     (nanoseconds->TSC cpu-freq timeout))))
                      (pcb-wakeup-set! pcb wakeup))
                    ;; The process is now waiting.
                    (pcb-move-to-waiting! pcb msg)))
                 (else
                  (let ((wv msg))
                    (vector-set! wv 1 timeout)
                    (pcb-dequeue-message! pcb wv)
                    (pcb-msg-set! pcb 'message))))))
        ((allocate)
         (let ((size (vector-ref msg 1))
               (mask (vector-ref msg 2)))
           ;; Allocate a consecutive memory region. TODO: jot down
           ;; this area as allocated to this process, and free it when
           ;; the process dies. If the process is a PCI device driver
           ;; then disconnect it from the memory bus first.
           (cond ((buddies-allocate! buddies size mask) =>
                  (lambda (addr)
                    (vector-set! msg 3 addr)
                    (pcb-msg-set! pcb 'ok)))
                 (else
                  (pcb-msg-set! pcb #f)))))
        ((free)
         (let ((addr (vector-ref msg 1)))
           ;; Deallocate a previously allocated region.
           (cond ((buddies-free! buddies addr) =>
                  (lambda (addr)
                    (pcb-msg-set! pcb 'ok)))
                 (else
                  (pcb-msg-set! pcb #f)))))

        ;;; IRQs
        ((enable-irq)
         (let ((irq (vector-ref msg 1)))
           (pic-enable irq)
           (when DEBUG
             (write msg)
             (flush-output-port (current-output-port)))
           ;; XXX: Only one pcb per interrupt
           (cond ((fx<=? 0 irq 15)
                  (vector-set! *irq-vectors* (fx+ PIC-vector-offset irq) pcb)
                  (pcb-msg-set! pcb 'ok))
                 (else
                  (pcb-msg-set! pcb 'error)))))
        ((acknowledge-irq)
         (let ((irq (vector-ref msg 1)))
           (when DEBUG
             (write msg)
             (flush-output-port (current-output-port)))
           (cond ((fx<=? 0 irq 15)
                  ;; We use special mask mode and already sent SEOI.
                  ;; Now unmask the IRQ so we can receive it again
                  ;; later.
                  (when (vector-ref *irq-vectors* (fx+ PIC-vector-offset irq))
                    (pic-enable irq))
                  (pcb-msg-set! pcb 'ok))
                 (else
                  (pcb-msg-set! pcb 'error)))
           (handle-message pcb (vector-ref msg 2) current-time)))

        ;;; Processes
        ((new-process)
         (let* ((pid *next-pid*)
                (sp ($process-start pid))
                (npcb (make-pcb pid sp #f pcb)))
           (pcb-add-to-runnable! npcb)
           (pcb-msg-set! pcb 'ok)
           (vector-set! msg 1 pid)
           (set! *next-pid* (+ *next-pid* 1))))

        ((new-usermode-process)
         (let ((ustate (vector-ref msg 2)))
           (let* ((pid *next-pid*)
                  (npcb (make-pcb pid ustate #t pcb)))
             (when DEBUG
               (display "new usermode process, ustate=#x")
               (display (number->string ustate 16))
               (newline))
             (pcb-add-to-runnable! npcb)
             (pcb-msg-set! pcb 'ok)
             (vector-set! msg 1 pid)
             (set! *next-pid* (+ *next-pid* 1)))))

        ((resume-process)
         ;; XXX: This is meant for resuming usermode processes
         (let ((pid (vector-ref msg 2)))
           (let ((target (hashtable-ref *pids* pid #f)))
             (when DEBUG
               (write (list 'resume pid target))
               (newline))
             (cond
               ((and target (eq? 'suspend (pcb-status target)))
                (pcb-move-to-runnable! target #f)
                (pcb-msg-set! pcb 'ok))
               (else
                (log/warning "Attempt to resume a non-suspended process: " target)
                (pcb-msg-set! pcb 'fail))))))

        ((exit-process)
         (let ((pid (vector-ref msg 2))
               (status (vector-ref msg 3)))
           (let ((target (if (not pid)
                             pcb
                             (hashtable-ref *pids* pid #f))))
             (when DEBUG
               (write (list 'exit pid target status))
               (newline))
             (cond
               ((pcb? target)
                (pcb-remove! target)
                (pcb-msg-set! pcb 'ok))
               (else
                (pcb-msg-set! pcb 'fail))))))

        (else
         (log/warning "Bad message: " (wrt msg))
         (pcb-msg-set! pcb 'error))))
    ;; Old message types (should be migrated to vectors, since the
    ;; vectors allow a response to be given with vector-set! while the
    ;; status of the call itself is set with pcb-msg-set!)
    (when (pair? msg)
      (case (car msg)
        ((get-pid)
         (pcb-msg-set! pcb (pcb-pid pcb)))
        ((boot-loader)
         (pcb-msg-set! pcb 'multiboot))
        ((command-line)
         ;; XXX: command-line, environment and boot-modules are
         ;; extremely iffy. The process must immediately copy the
         ;; variables to its own storage.
         (pcb-msg-set! pcb (command-line)))
        ((environment)
         (pcb-msg-set! pcb (get-environment-variables)))
        ((boot-modules)
         (pcb-msg-set! pcb *boot-modules*))
        ((clocks/tick)
         (pcb-msg-set! pcb (fxdiv cpu-freq 1000)))
        ((out-of-memory)
         (display "Process " (current-error-port))
         (display (pcb-pid pcb) (current-error-port))
         (display ": out of heap memory\n" (current-error-port))
         (pcb-remove! pcb))
        (else
         (log/warning "bad message: " (wrt msg))))))

  ;; Start the boot process.
  (let ((boot (make-pcb 1 ($process-start 1) #f #f)))
    (log/notice "Starting the first process, hold on to your hat!")
    (pcb-add-to-runnable! boot)
    (hashtable-set! *pids* (pcb-pid boot) boot))

  (let scheduler-loop ((dnode #f))
    (define current-time (rdtsc)) ;FIXME: can wrap
    (handle-irq)
    (handle-wait-queue current-time)

    ;; TODO: set the timeout based on the next wakeup
    (write-timer-initial-count interval)
    (write-timer-vector (fxior APIC-vector-timer LVT-TMM))
    (write-timer-divide apic-divisor)
    (cond
      ((not dnode)
       (cond
         ((dlist-head *runq*) => scheduler-loop)
         (else
          (when (null? *waitq*)
            (error 'scheduler "No runnable or waiting processes!"))
          ;; FIXME: Poll the PICs for interrupts that were asserted
          ;; but never delivered (they are set in ISR).
          (when DEBUG
            (display #\H)
            (display (list (number->string (pic-get-isr) 2)
                           (number->string (pic-get-irr) 2)))
            (display " "))
          (sys_hlt)                     ;this should make something runnable
          (scheduler-loop #f))))

      (else
       ;; Give a time slice to the next runnable process
       (when DEBUG
         (display #\R))
       (let ((pcb (dnode-data dnode)) (next-pcb (dnode-next dnode)))
         (cond
           ((pcb-usermode? pcb)
            (let* ((msg (sys_urun (pcb-sp pcb)))
                   (vec ($processor-data-ref CPU-VECTOR:LAST-INTERRUPT-VECTOR)))
              (when DEBUG
                (write (cons 'U vec)))
              (cond
                ((not (fixnum? vec))
                 (log/debug "Bad LAST-INTERRUPT-VECTOR: " (wrt vec)))
                ((fx>=? vec 32)
                 ;; The usermode process was preempted by an interrupt
                 (handle-irq))
                ((fx<=? 0 vec 31)
                 ;; The usermode process triggered a fault/trap
                 (log/warning "Pid " (pcb-pid pcb) " triggered trap " vec)
                 (put-mem-s61 (fx+ (pcb-sp pcb) USTATE:FAULT-NUMBER) vec)
                 (put-mem-s61 (fx+ (pcb-sp pcb) USTATE:FAULT-CODE)
                              ($processor-data-ref CPU-VECTOR:LAST-INTERRUPT-CODE))
                 (when (eqv? vec 14)
                   (put-mem-s61 (fx+ (pcb-sp pcb) USTATE:CR2)
                                ($processor-data-ref CPU-VECTOR:LAST-FAULTING-ADDRESS)))
                 (pcb-enqueue-message! (pcb-parent pcb)
                                       `(,(pcb-pid pcb) . trap))
                 (pcb-suspend! pcb))
                (else
                 ;; The usermode process performed a syscall
                 (pcb-enqueue-message! (pcb-parent pcb)
                                       `(,(pcb-pid pcb) . syscall))
                 (pcb-suspend! pcb)))))
           (else
            (let ((msg ($switch-stack (pcb-sp pcb) (pcb-msg pcb))))
              (pcb-sp-set! pcb (get-saved-sp!))
              ;; (write-timer-vector LVT-MASK)
              (when (and DEBUG (eq? msg 'preempted))
                (display #\P))
              (unless (eq? msg 'preempted)
                (pcb-msg-set! pcb #f))
              (handle-message pcb msg current-time))))

         (scheduler-loop next-pcb))))))

;;; Multiboot initialization

(define (pc-init)
  (define (print . x) (for-each display x) (newline))
  (define (mem64 a)
    ;; Read a u64 from memory
    (bitwise-ior (get-mem-u32 a)
                 (bitwise-arithmetic-shift-left (get-mem-u32 (fx+ a 4)) 32)))
  ;; Multiboot information structure
  (define flag-mem              #b000000000001)
  (define flag-boot-device      #b000000000010)
  (define flag-cmdline          #b000000000100)
  (define flag-mods             #b000000001000)
  (define flag-syms-a.out       #b000000010000)
  (define flag-syms-elf         #b000000100000)
  (define flag-mmap             #b000001000000)
  (define flag-drives           #b000010000000)
  (define flag-config-table     #b000100000000)
  (define flag-boot-loader-name #b001000000000)
  (define flag-apm-table        #b010000000000)
  (define flag-vbe              #b100000000000)
  (define flag-framebuffer     #b1000000000000)
  (define field-flags            (* 4 0))
  (define field-memory-lower     (* 4 1))
  (define field-memory-upper     (* 4 2))
  (define field-boot-device      (* 4 3))
  (define field-cmdline          (* 4 4))
  (define field-mods-count       (* 4 5))
  (define field-mods-addr        (* 4 6))
  (define field-elf-sec-num      (* 4 7))
  (define field-elf-sec-size     (* 4 8))
  (define field-elf-sec-addr     (* 4 9))
  (define field-elf-sec-shndx    (* 4 10))
  (define field-mmap-length      (* 4 11))
  (define field-mmap-addr        (* 4 12))
  (define field-drives-length    (* 4 13))
  (define field-drives-addr      (* 4 14))
  (define field-config-table     (* 4 15))
  (define field-boot-loader-name (* 4 16))
  (define field-apm-table        (* 4 17))
  (define field-framebuffer_addr   88)  ;u64
  (define field-framebuffer_pitch  96)
  (define field-framebuffer_width  100)
  (define field-framebuffer_height 104)
  (define field-framebuffer_bpp    108) ;u8
  (define field-framebuffer_type   109) ;u8
  (define field-framebuffer_red_field_position    110) ;u8
  (define field-framebuffer_red_mask_size         111) ;u8
  (define field-framebuffer_green_field_position  112) ;u8
  (define field-framebuffer_green_mask_size       113) ;u8
  (define field-framebuffer_blue_field_position   114) ;u8
  (define field-framebuffer_blue_mask_size        115) ;u8

  (define MULTIBOOT_FRAMEBUFFER_TYPE_INDEXED  0)
  (define MULTIBOOT_FRAMEBUFFER_TYPE_RGB      1)
  (define MULTIBOOT_FRAMEBUFFER_TYPE_EGA_TEXT 2)

  (define (mbi-ref-u32 offset)
    (assert (fx<? offset (fx- 116 3)))
    (get-mem-u32 (fx+ ($boot-loader-data) offset)))
  (define (mbi-ref-u8 offset)
    (assert (fx<? offset 116))
    (get-mem-u8 (fx+ ($boot-loader-data) offset)))
  (define (multiboot-flag-set? flag)
    (not (fxzero? (fxand flag (mbi-ref-u32 field-flags)))))

  ;; Physical memory map
  (define *physmap* (make-mmap (- (expt 2 43) 1)))
  (define-record-type meminfo
    (sealed #t)
    (fields start length type))
  (define (fmt-addr x) (and x (number->string x 16)))
  (define (print-area x)
    (let ((i (area-info x)))
      (log/info " [" (fmt-addr (area-base x)) "," (fmt-addr (area-top x)) "] "
                (fmt-human-readable-bytes (area-size x))
                ;; " {" (fmt-addr (meminfo-start i)) "+" (fmt-addr (meminfo-length i)) "}: "
                ": "(area-type x))))
  (define (page-align-down x) (fxand x -4096))
  (define (page-align-up x) (fxand (fx+ x 4095) -4096))

  ;; Usable areas are rounded up to the next page size, and the top of
  ;; the area is rounded down because half pages are not usable.
  (define (mark-usable base len type)
    (let ((base (page-align-up base))
          (top (page-align-down (+ base len))))
      (let ((len (- top base)))
        (when (>= len 4096)
          (mmap-mark! *physmap* base len 0 type #f)))))

  ;; Reserved areas are rounded down to make the whole page reserved,
  ;; and the top is rounded up for the same reason.
  (define (mark-reserved base len type)
    (let ((base (page-align-down base))
          (top (page-align-up (+ base len))))
      (let ((len (- top base)))
        (when (> len 0)
          (mmap-mark! *physmap* base len 0 type #f)))))

  (define (find-usable)
    (mmap-filter->list (lambda (area)
                         (eq? (area-type area) 'usable))
                       *physmap*))

;;; Paging

  ;; Page allocation. Long mode page tables with PAT only. All
  ;; tables are 4096 bytes and contain 64-bit entries. CR3 points to
  ;; the PML4, which point to PDPE's, which point to 4-Kbyte PDE's
  ;; or 2-Mbyte PDE's. A 4-Kbyte PDE points to a 4-Kbyte PTE. The
  ;; PML4E may also point to a 1-Gbyte PDPE. Bit 0 always means
  ;; "present".
  (define (get-cr3)
    ;; Doesn't really return CR3...
    ($linker-address 'pml4))
  (define cr3 (get-cr3))
  (define (get-pml4t)
    (fxand cr3 (fxnot (- (expt 2 12) 2))))
  (define (supports-1G-pages?)
    (let-values (((a_ b_ c_ d) (cpuid #x80000001)))
      (fxbit-set? d 26)))
  ;; Flags available in all levels of the page table hierarchy
  (define page-P   0)                 ;present
  (define page-R/W 1)                 ;read/write
  (define page-U/S 2)                 ;user/supervisor
  (define page-PWT 4)                 ;page-level writethrough(*)
  (define page-PCD 4)                 ;page-level cache disable(*)
  (define page-A   5)                 ;accessed
  (define page-NX  63)                ;no-execute
  ;; Flags available only in the lowest level of the page table
  (define page-D       6)             ;dirty
  (define pte-PAT      7)             ;page-attribute table(*)
  (define pte-G        8)             ;global
  (define pde/pdpe-PAT 12)            ;page-attribute table(*)
  ;; Flags available only at the PDE level, and the PDPE level if
  ;; support for 1GB pages is available.
  (define page-PS 7)       ;page size
  ;; Bit field offsets in virtual addresses
  (define PHYSICAL-SIGN 52)           ;XXX:
  (define VIRTUAL-SIGN 48)            ;XXX:
  (define VIRTUAL-PML4 39)
  (define VIRTUAL-PDP 30)
  (define VIRTUAL-PD 21)
  (define VIRTUAL-PT 12)
  ;; Very nice handy stuff.
  (define (entry-present? x) (bitwise-bit-set? x page-P))
  (define (entry-r/w? x) (bitwise-bit-set? x page-R/W))
  (define (entry-u/s? x) (bitwise-bit-set? x page-U/S))
  (define (entry-accessed? x) (bitwise-bit-set? x page-A))
  (define (entry-nx? x) (bitwise-bit-set? x page-NX))
  (define (pml4e-address x)
    (bitwise-and x (bitwise-and (fxnot (- (expt 2 12) 1))
                                (- (expt 2 52) 1))))
  (define (pdpe-1G? x) (bitwise-bit-set? x page-PS))
  (define (pdpe-1G-address x)
    (bitwise-and x (bitwise-and (fxnot (- (expt 2 30) 1))
                                (- (expt 2 52) 1))))
  (define pdpe-address pml4e-address)
  (define pde-2M? pdpe-1G?)
  (define pde-4K-address pml4e-address)
  (define (pde-2M-address x)
    (bitwise-and x (bitwise-and (fxnot (- (expt 2 20) 1))
                                (- (expt 2 52) 1))))
  (define (pde-2M-pat x)
    (fxior (if (bitwise-bit-set? x pde/pdpe-PAT) #b100 0)
           (if (bitwise-bit-set? x page-PCD) #b10 0)
           (if (bitwise-bit-set? x page-PWT) #b1 0)))
  (define pte-address pml4e-address)
  (define (pte-pat x)
    (fxior (if (bitwise-bit-set? x pte-PAT) #b100 0)
           (if (bitwise-bit-set? x page-PCD) #b10 0)
           (if (bitwise-bit-set? x page-PWT) #b1 0)))
  (define (pflags e . fs)
    (let lp ((fs fs))
      (if (null? fs)
          '()
          (let ((bit (car fs)) (name (cadr fs)) (fs (cddr fs)))
            (if (bitwise-bit-set? e bit)
                (cons name (lp fs))
                (lp fs))))))
  (define (pte-flags e)
    (cons (list 'PAT (pte-pat e))
          (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
                  page-D 'D pte-G 'G page-NX 'NX)))
  (define (pde-2M-flags e)
    (cons (list 'PAT (pde-2M-pat e))
          (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
                  page-D 'D pte-G 'G page-NX 'NX)))
  (define (pde-4K-flags e)
    (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
            page-NX 'NX))
  (define pdpe-flags pde-4K-flags)
  (define pml4e-flags pde-4K-flags)
  (define (vaddr-pml4 addr)
    (fxand (bitwise-arithmetic-shift-right addr VIRTUAL-PML4)
           (- (expt 2 (- 64 VIRTUAL-SIGN)) 1)))
  (define (vaddr-pdp addr)
    (fxbit-field addr VIRTUAL-PDP VIRTUAL-PML4))
  (define (vaddr-pd addr)
    (fxbit-field addr VIRTUAL-PD VIRTUAL-PDP))
  (define (vaddr-pt addr)
    (fxbit-field addr VIRTUAL-PT VIRTUAL-PD))
  (define (table-for-each proc table-base)
    ;; Run proc on each of the entries in a descriptor table.
    (do ((i 0 (fx+ i 8)))
        ((fx=? i 4096))
      (let ((e (mem64 (fx+ table-base i))))
        (when (entry-present? e)
          (proc e (fxarithmetic-shift-right i 3))))))
  (define (print-full-table)
    (print "Full page table follows.")
    (print "CR3: #x" (fmt-addr (get-cr3))
           #\tab (pflags (get-cr3) page-PWT 'PWT page-PCD 'PCD))
    (print "Page-Map Level-4 Table:")
    (table-for-each
     (lambda (pml4e i)
       ;; TODO: sign-extension
       (print-pml4e pml4e (fxarithmetic-shift-left i VIRTUAL-PML4)))
     (get-pml4t)))
  (define (print-pml4e e base)
    (print " PML4E table base #x" (fmt-addr base) " entry at #x"
           (fmt-addr (pml4e-address e)) #\tab (pml4e-flags e))
    (table-for-each
     (lambda (pdpe i)
       (print-pdpe pdpe (fxior base (fxarithmetic-shift-left i VIRTUAL-PDP))))
     (pml4e-address e)))
  (define (print-pdpe e base)
    (cond ((pdpe-1G? e)
           (print "  PDPE 1G page #x" (fmt-addr base) " => #x"
                  (fmt-addr (pdpe-1G-address e))))
          (else
           (print "  PDPE table base #x" (fmt-addr base)
                  " entry at #x" (fmt-addr (pdpe-address e))
                  #\tab (pdpe-flags e))
           (table-for-each
            (lambda (pde i)
              (print-pde pde (fxior base (fxarithmetic-shift-left i VIRTUAL-PD))))
            (pdpe-address e))
           (newline))))
  (define (print-pde e base)
    (cond ((pde-2M? e)
           (display #\.)
           #;(print "   PDE 2M page #x" (fmt-addr base) " => #x"
                    (fmt-addr (pde-2M-address e)) #\tab (pde-2M-flags e)))
          (else
           (print "   PDE 4K table base #x" (fmt-addr base) " entry at #x"
                  (fmt-addr (pde-4K-address e)) #\tab (pde-4K-flags e))
           (table-for-each
            (lambda (pte i)
              (print-pte pte (fxior base (fxarithmetic-shift-left i VIRTUAL-PT))))
            (pde-4K-address e))
           (newline))))
  (define (print-pte e base)
    (print "    PTE page #x" (fmt-addr base) " => #x"
           (fmt-addr (pte-address e)) #\tab (pte-flags e)))

  ;; Returns the physical address that the virtual address is mapped
  ;; to, or #f if it is not mapped.
  (define (virtual->physical addr)
    (define (print . _) #f)
    (assert (= VIRTUAL-SIGN 48))
    (let ((pml4 (vaddr-pml4 addr))
          (rest (bitwise-and addr (- (expt 2 VIRTUAL-SIGN) 1))))
      (let ((pdp (vaddr-pdp rest)) (pd (vaddr-pd rest)) (pt (vaddr-pt rest)))
        (print "Fields: "  (list pml4 pdp pd pt))
        (let ((pml4e (mem64 (fx+ (get-pml4t) (fx* pml4 8)))))
          (print "pml4e: " pml4e)
          (and (entry-present? pml4e)
               (let ((pdpe (mem64 (fx+ (pml4e-address pml4e) (fx* pdp 8)))))
                 (assert (not (pdpe-1G? pdpe)))
                 (print "pdpe: " pdpe)
                 (if (pdpe-1G? pdpe)
                     (fxior (fxand addr (- (expt 2 VIRTUAL-PDP) 1))
                            (pdpe-1G-address pdpe))
                     (and (entry-present? pdpe)
                          (let ((pde (mem64 (fx+ (pdpe-address pdpe) (fx* pd 8)))))
                            (print "pde: " pde)
                            (and (entry-present? pde)
                                 (if (pde-2M? pde)
                                     (fxior (fxand addr (- (expt 2 VIRTUAL-PD) 1))
                                            (pde-2M-address pde))
                                     (let ((pte (mem64 (fx+ (pde-4K-address pde)
                                                            (fx* pt 8)))))
                                       (print "pt: " pte " " (pte-address pte))
                                       (and (entry-present? pte)
                                            (fxior (fxand addr (- (expt 2 VIRTUAL-PT) 1))
                                                   (pte-address pte)))))))))))))))
  (define *buddies* '())
  (define (get-4K-zero-page)
    ;; XXX: Must return identity-mapped memory because it's used to
    ;; populate the page tables.
    (let lp ((buddies *buddies*))
      (cond ((null? buddies)
             (error 'get-4K-zero-page "Out of memory" *buddies*))
            ((buddy-allocate! (car buddies) 4096) =>
             (lambda (page)
               (clear-page page)
               page))
            (else
             (lp (cdr buddies))))))
  ;; Create virtual memory mapping for the addresses from start to
  ;; start+len-1. Does not remove/overwrite existing mappings.
  (define (longmode-mmap start* len type)
    ;; TODO: identity mappings should be 1GB or 2MB pages.
    (define (print . _) #f)
    (define (set-mem64! a v)
      (put-mem-u32 a (bitwise-and v #xffffffff))
      (put-mem-u32 (fx+ a 4) (bitwise-arithmetic-shift-right v 32)))
    (define (page-align-down x) (fxand x -4096))
    (define (page-align-up x) (fxand (fx+ x 4095) -4096))
    (define start (page-align-down start*))
    (define end (page-align-up (fx+ start len)))
    (define attributes (fxior (expt 2 page-P)
                              (expt 2 page-R/W)
                              (expt 2 page-U/S)))
    (assert (memq type '(stack heap code identity)))
    (assert (fxzero? (fxbit-field start 0 VIRTUAL-PT)))
    (assert (fxpositive? len))
    (assert (= VIRTUAL-SIGN 48))
    (when (eq? type 'identity)
      ;; This is needed because of where thread areas are placed.
      (assert (< end (* 2 512 1024 1024 1024))))
    (do ((addr start (fx+ addr 4096)))
        ((fx=? addr end))
      (let ((pml4 (vaddr-pml4 addr))
            (rest (bitwise-and addr (- (expt 2 VIRTUAL-SIGN) 1))))
        (let ((pdp (vaddr-pdp rest)) (pd (vaddr-pd rest)) (pt (vaddr-pt rest)))
          ;; Populate the table.
          (print "Populate: " (list pml4 pdp pd pt))
          (let ((pml4e@ (fx+ (get-pml4t)
                             (fx* pml4 8))))
            ;; Add a PML4E
            (when (not (entry-present? (get-mem-u32 pml4e@)))
              (print "Adding a page to PML4T")
              (set-mem64! pml4e@ (bitwise-ior (get-4K-zero-page) attributes)))
            (let ((pdpe@ (fx+ (pml4e-address (mem64 pml4e@))
                              (fx* pdp 8))))
              ;; Add a PDPE
              (when (not (entry-present? (get-mem-u32 pdpe@)))
                (print "Adding a page to PDPT")
                (set-mem64! pdpe@ (bitwise-ior (get-4K-zero-page) attributes)))
              (let ((pdpe (mem64 pdpe@)))
                (unless (pdpe-1G? pdpe)
                  (let ((pde@ (fx+ (pdpe-address pdpe)
                                   (fx* pd 8))))
                    ;; Add a PDE
                    (when (not (entry-present? (get-mem-u32 pde@)))
                      (print "Adding a page to PDT")
                      (set-mem64! pde@ (bitwise-ior (get-4K-zero-page)
                                                    attributes)))
                    (let ((pde (mem64 pde@)))
                      (unless (pde-2M? pde)
                        (let ((pte@ (fx+ (pde-4K-address pde)
                                         (fx* pt 8))))
                          ;; Add a PTE
                          (when (not (entry-present? (get-mem-u32 pte@)))
                            (print "Adding a PTE")
                            (let ((address
                                   (if (eq? type 'identity)
                                       addr
                                       (get-4K-zero-page))))
                              (set-mem64! pte@ (bitwise-ior address
                                                            attributes))))))))))))))))

  (define (longmode-unmap addr len)
    ;; TODO: unmap and invalidate tlb
    #f)


  ;; (print-full-table)

;;; The fun starts here

  (init-early-vga-driver)
  (when (multiboot-flag-set? flag-cmdline)
    (let-values ([(env cmdline) (pc-init-parse-command-line
                                 (utf8->string (copy-utf8z (mbi-ref-u32 field-cmdline))))])
      (init-set! 'environment-variables env)
      (init-set! 'command-line cmdline)))
  (let ((console (get-environment-variable "CONSOLE")))
    (cond ((equal? console "debug")
           (init-early-debugcon-driver))
          ((equal? console "com1")
           (init-early-ns8250-driver))
          (else
           'nop)))

  (time-init-set! 'current-ticks (lambda () 0))
  (current-log-callback
   (lambda (e)
     (let ((severity (cdr (assq 'SEVERITY e)))
           (message (cdr (assq 'MESSAGE e)))
           (subsystem (cond ((assq 'SUBSYSTEM e) => cdr)
                            (else #f))))
       (define p (current-error-port))
       (display (cdr (assq 'JIFFY e)) p)
       (display " " p)
       (when subsystem
         (display subsystem p)
         (display " " p))
       (display "[" p)
       (display (severity->symbol severity) p)
       (display "] " p)
       (display (cdr (assq 'MESSAGE e)) p)
       (newline p)
       (cond ((assq 'EXCEPTION e) =>
              (lambda (exn)
                (print-condition (cdr exn) p)))))))

  (check-cpu)
  (check-apic-base)
  (log/info "Loko Scheme " (loko-version) " <https://scheme.fail>")
  (log/info "Copyright © 2023 G. Weinholt")
  (log/info "Licensed under the EUPL-1.2-or-later.")
  (init-set! '$mmap longmode-mmap)
  (init-set! 'exit
             (lambda (status)
               (log/emergency "Loko has stopped running. Sorry about that. Exit status: "
                              status)
               (let lp ()
                 (sys_hlt)
                 (lp))))
  (init-set! 'machine-type '#(amd64 pc))

  ;; Check that #AC works
  (guard (exn (else #f))
    (get-mem-u32 #x200001)           ;safe way to trigger #AC
    (log/emergency "Loko can't run because the system does not support #AC")
    (exit 70))

  ;; We need a memory map. Can't guess this sort of stuff.
  (unless (multiboot-flag-set? flag-mmap)
    (error 'system "The boot-loader did not pass a memory map"))

  (when (multiboot-flag-set? flag-boot-loader-name)
    (init-set! 'environment-variables
               (cons (cons "BOOT_LOADER_NAME"
                           (utf8->string (copy-utf8z (mbi-ref-u32 field-boot-loader-name))))
                     (get-environment-variables))))

  ;; Parse the memory map.
  (let* ((len (mbi-ref-u32 field-mmap-length))
         (start (mbi-ref-u32 field-mmap-addr))
         (end (fx+ start len)))
    (define (size addr) (get-mem-u32 addr))
    (define (base addr)
      (bitwise-ior (get-mem-u32 addr)
                   (bitwise-arithmetic-shift-left (get-mem-u32 (fx+ addr 4)) 32)))
    (define (len addr)
      (bitwise-ior (get-mem-u32 (fx+ addr 8))
                   (bitwise-arithmetic-shift-left (get-mem-u32 (fx+ addr 12)) 32)))
    (define (type addr) (get-mem-u32 (fx+ addr 16)))
    (define (qemu-nonsense? entry-base entry-len)
      ;; Check for a pointless e820 entry introduced by
      ;; qemu-project/qemu@8504f129.
      (and (eqv? entry-base #xfd00000000)
           (eqv? entry-len #x300000000)))
    ;; Mark all usable memory.
    (do ((addr start (fx+ addr (fx+ (size addr) 4))))
        ((fx>=? addr end))
      (let* ((saddr (fx+ addr 4))
             (type (type saddr)))
        (when (eq? type 1)
          (mark-usable (base saddr) (len saddr) 'usable))))
    ;; And now mark all non-usable memory.
    (do ((addr start (fx+ addr (fx+ (size addr) 4))))
        ((fx>=? addr end))
      (let* ((saddr (fx+ addr 4))
             (type (type saddr)))
        (unless (or (eqv? type 1)
                    (qemu-nonsense? (base saddr) (len saddr)))
          (mark-reserved (base saddr) (len saddr)
                         (case type
                           ((2) 'reserved)
                           ((3) 'acpi-data)
                           ((4) 'acpi-nvs)
                           ((5) 'rom)
                           ((6) 'ioapic)
                           ((7) 'lapic)
                           (else (cons 'e820 type))))))))

  ;; Parse the framebuffer information
  (cond
    ((multiboot-flag-set? flag-framebuffer)
     (let ((addr (fxior (mbi-ref-u32 field-framebuffer_addr)
                        (fxarithmetic-shift-left
                         (mbi-ref-u32 (+ field-framebuffer_addr 4))
                         32)))
           (pitch (mbi-ref-u32 field-framebuffer_pitch))
           (width (mbi-ref-u32 field-framebuffer_width))
           (height (mbi-ref-u32 field-framebuffer_height))
           (bpp (mbi-ref-u8 field-framebuffer_bpp))
           (type (mbi-ref-u8 field-framebuffer_type)))
       ;; Mark the memory so it will be identity-mapped
       (mark-reserved addr (fx* height pitch) 'framebuffer)
       (cond ((eqv? type MULTIBOOT_FRAMEBUFFER_TYPE_RGB)
              (let ((color-info (list (mbi-ref-u8 field-framebuffer_red_field_position)
                                      (mbi-ref-u8 field-framebuffer_red_mask_size)
                                      (mbi-ref-u8 field-framebuffer_green_field_position)
                                      (mbi-ref-u8 field-framebuffer_green_mask_size)
                                      (mbi-ref-u8 field-framebuffer_blue_field_position)
                                      (mbi-ref-u8 field-framebuffer_blue_mask_size))))
                (log/info "Framebuffer at #x" (number->string addr 16) ", "
                          width "x" height "x" bpp ", with pitch " pitch)
                (let ((fb-info
                       (call-with-string-output-port
                         (lambda (p)
                           (write (list addr width height bpp pitch color-info) p)))))
                  (init-set! 'environment-variables
                             (cons (cons "BOOT_FRAMEBUFFER" fb-info)
                                   (get-environment-variables))))))
             (else
              (log/warning "Unsupported framebuffer type: " type)))))
    (else
     (log/info "No framebuffer information in the Multiboot structure")))

  ;; Mark the kernel's own memory
  (let ((load-addr ($linker-address 'image-address-zero))
        (bss-end-addr ($linker-address 'bss-end)))
    (mark-reserved load-addr (fx- bss-end-addr load-addr) 'kernel))

  ;; Modules contain Scheme code to run in pid 1.
  (when (multiboot-flag-set? flag-mods)
    (do ((mods (mbi-ref-u32 field-mods-count))
         (addr (mbi-ref-u32 field-mods-addr) (fx+ addr 16))
         (i 0 (fx+ i 1)))
        ((fx=? i mods))
      (let ((start (get-mem-u32 addr))
            (end (get-mem-u32 (fx+ addr 4)))
            (str (utf8->string (copy-utf8z (get-mem-u32 (fx+ addr 8))))))
        ;; Mark the code so we don't accidentally overwrite it.
        (mark-reserved start (fx- end start) (cons 'module str))
        ;; TODO: free the pages used by the modules after using them
        (let-values ([(_env cmdline) (pc-init-parse-command-line str)])
          (set! *boot-modules* (cons (list (car cmdline) (cdr cmdline)
                                           start (fx- end start))
                                     *boot-modules*))))))

  ;; BIOS data area + some extra for good measure, VGA, EBDA, ROMs.
  (mark-reserved 0 #x3000 'bios)
  (mark-reserved #x3000 #x1000 'scratch)    ;scratch area for AP boot
  (mark-reserved #xA0000 #x20000 'vga)
  (mark-reserved #xC0000 #x40000 'bios)
  (log/info "Physical memory map:")
  (mmap-for-each print-area *physmap*)

  ;; At this point all reserved memory has been marked as such. The
  ;; multiboot info will be clobbered after this and all RAM will be
  ;; identity-mapped.

  ;; Put 1MB last, 1MB-4GB second, 4GB+ first. Only the first 4GB is
  ;; identity mapped when we get here from the boot loader, so that
  ;; memory is needed for page table. And it's better to not waste the
  ;; first 1MB, since some hardware needs that memory.
  (let ((early-limit (* 4 1024 1024 1024))
        (usable (list-sort (lambda (a b)
                             (let ((a (area-top a))
                                   (b (area-top b)))
                               (cond ((fx<? a (* 1024 1024)) #f)
                                     ((fx<? b (* 1024 1024)) #t)
                                     (else
                                      (fx<? a b)))))
                           (find-usable))))
    (define (make-area-buddy area)
      (make-buddy (area-base area) (area-size area) 12))
    ;; (for-each print-area usable)
    (log/info "Free RAM: "
              (fmt-human-readable-bytes
               (fold-left (lambda (acc area) (+ acc (area-size area)))
                          0 usable)))
    ;; Make buddy allocators for low areas first, identity map the
    ;; high RAM using these, and then make buddy allocators for the
    ;; high areas.
    (let-values ([(usable-low usable-high)
                  (partition (lambda (area)
                               (and (fx<? (area-base area) early-limit)
                                    (fx<? (area-top area) early-limit)))
                             usable)])
      (set! *buddies* (map make-area-buddy usable-low))
      ;; Identity map all RAM, even that marked unusable, as it might
      ;; contain memory-mapped registers or some other useful things.
      (mmap-for-each
       (lambda (area)
         (longmode-mmap (area-base area) (area-size area) 'identity))
       *physmap*)
      (set! *buddies* (append (map make-area-buddy usable-high) *buddies*))
      (log/info "Buddy allocators:")
      (for-each (lambda (buddy)
                  (log/info " [" (number->string (buddy-start-address buddy) 16)
                            "," (number->string (+ (buddy-start-address buddy)
                                                   (buddy-capacity buddy)) 16)
                            ") " (fmt-human-readable-bytes (buddy-capacity buddy))))
                *buddies*)))

  ;; A null page might catch some bugs.
  ;; (longmode-unmap 0 #x1000)

  ;; Use the MADT table from ACPI to configure the interrupt
  ;; controllers. There is additional information in the DSDT for
  ;; interrupt routing, but it is up to pid 1 to handle all that
  ;; nonsense.
  (let ((acpi-tables (pc-acpi-enumerate-tables (pc-acpi-find-rsdp-structure))))
    (log/info "ACPI tables: " (map car acpi-tables))
    (unless (assoc DSDT-signature acpi-tables)
      (log/notice "No DSDT found in the ACPI tables"))
    (let ((madt (parse-madt (pc-acpi-open-table acpi-tables MADT-signature))))
      (pc-setup-interrupts madt)))

  ;; Calibrate the timer against the i8253/i8254 PIT and set
  ;; the local APIC timer to fire periodically at 1000 Hz.
  (let-values ([(bus-freq cpu-freq) (calibrate-APIC&CPU-against-PIT)])
    (time-init-set! 'current-ticks (lambda () (fxdiv (rdtsc) (fxdiv cpu-freq 1000))))
    (let* ((scheduler-frequency 1/1000)
           (interval (max (div bus-freq (* apic-divisor (/ scheduler-frequency)))
                          100)))
      (log/info "CPU frequency = " cpu-freq
                " Hz, APIC frequency = " bus-freq " Hz")
      (log/info "APIC timer = " interval)
      (log/info "Hardware uptime = " (div (rdtsc) cpu-freq) " seconds")
      (write-timer-initial-count interval)
      (write-timer-vector (fxior APIC-vector-timer LVT-TMM))
      (write-timer-divide apic-divisor)

      ;; FIXME: boot the non-disabled cpus individually in the order
      ;; given in the MADT
      (log/info "Booting application processors...")
      ;; XXX: The page is hardcoded to #x3000 now due to a bug in
      ;; machine-code's 16-bit mode support
      (let ([temp-page #x3000 #; (pc-dma-allocate 4096 #x000ff000)])
        (define (busywait seconds)
          (let* ((start (rdtsc))
                 (target (+ start (nanoseconds->TSC cpu-freq (* seconds #e1e9)))))
            (let lp ()
              (let ((current (rdtsc)))
                (unless (and (> current start)
                             (> current target))
                  (lp))))))
        #; (log/debug "AP boot page: #x" (and temp-page (number->string temp-page 16)))
        (when temp-page
          (boot-application-processors temp-page APIC:ICR-low busywait))
        #;(pc-dma-free temp-page)

        ;; Useful info
        (unless (null? *boot-modules*)
          (log/info "Boot modules: " (wrt (map car *boot-modules*))))
        (log/info "Boot environment: " (wrt (get-environment-variables)))
        (log/info "Boot command line: " (wrt (command-line)))

        ;; Switch over to the scheduler, which will start the first process
        (pc-scheduler cpu-freq interval *buddies*)))))

(when (eq? ($boot-loader-type) 'multiboot)
  (init-set! 'init (lambda (stage)
                     (pc-init)))))
