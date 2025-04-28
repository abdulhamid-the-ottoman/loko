;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Real-time-clock driver

;; The RTC in the IBM PC. Maybe a Motorola MC146818B. Or is it a
;; DS1387? Either way it's a product of its time and is showing its
;; age. It's embedded in the motherboard now, e.g. in the ICH chip.

;; "Real-Time Clock Plus RAM (RTC)" MC146818A/D
;; <https://www.nxp.com/docs/en/data-sheet/MC146818A.pdf>

;; "Accessing the Real Time Clock Registers and the NMI Enable Bit"
;; <ftp://download.intel.com/design/intarch/PAPERS/321088.pdf>

;; "IBM PC Real Time Clock should run in UT"
;; <https://www.cl.cam.ac.uk/~mgk25/mswish/ut-rtc.html>

(library (loko drivers rtc mc146818)
  (export
    driver·rtc·mc146818)
  (import
    (rnrs (6))
    (srfi :19 time)
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko system time)
    (loko system unsafe)
    (only (loko system $host) enable-irq wait-irq-operation)
    (loko drivers rtc))

;; TODO: Figure out what API is needed and add channels
(define (driver·rtc·mc146818 dev)
  (define irq 8)

  ;; I/O ports
  (define reg-index #x70)
  (define reg-data #x71)
  (define reg-ext-index #x72)
  (define reg-ext-data #x73)
  (define bit7 #b10000000)

  ;; Read the RTC RAM
  (define (rtc-read i)
    ;; If bit7 = 0 then NMI is disabled.
    (put-i/o-u8 reg-index (fxior bit7 i))
    (get-i/o-u8 reg-data))

  ;; Write the RTC RAM
  (define (rtc-write i x)
    (put-i/o-u8 reg-index (fxior bit7 i))
    (put-i/o-u8 reg-data x))

  ;; RTC RAM indices
  ;; - clock/calendar
  (define RTC_Seconds           #x00)
  (define RTC_Seconds_Alarm     #x01)
  (define RTC_Minutes           #x02)
  (define RTC_Minutes_Alarm     #x03)
  (define RTC_Hours             #x04)
  (define RTC_Hours_Alarm       #x05)
  (define RTC_Day_of_Week       #x06)
  (define RTC_Day_of_Month      #x07)
  (define RTC_Month             #x08)
  (define RTC_Year              #x09)
  ;; - status registers
  (define RTC_RegA              #x0A)
  (define RTC_RegB              #x0B)
  (define RTC_RegC              #x0C)
  (define RTC_RegD              #x0D)
  ;; - configuration
  (define RTC_POST_Diagnostics  #x0E)
  (define RTC_ShutdownReason    #x0F)
  (define RTC_Century           #x32)   ;maybe

  ;; Register contents
  (define RegA_UIP   #b10000000)        ;update in progress
  (define RegA_DV2   #b01000000)        ;divider
  (define RegA_DV1   #b00100000)
  (define RegA_DV0   #b00010000)
  (define RegA_RS3   #b00001000)        ;rate selection
  (define RegA_RS2   #b00000100)
  (define RegA_RS1   #b00000010)
  (define RegA_RS0   #b00000001)
  (define RegB_SET   #b10000000)        ;when set, stops the clock
  (define RegB_PIE   #b01000000)        ;periodic interrupt enable
  (define RegB_AIE   #b00100000)        ;alarm interrupt enable
  (define RegB_UIE   #b00010000)        ;update-ended interrupt enable
  (define RegB_SQWE  #b00001000)        ;square-wave enable
  (define RegB_DM    #b00000100)        ;data mode (0=BCD/1=binary)
  (define RegB_24/12 #b00000010)        ;0=bizarro mode, 1=24-hour mode
  (define RegB_DSE   #b00000001)        ;bizarro daylight savings mode
  (define RegC_IRQF  #b10000000)        ;interrupt request
  (define RegC_PF    #b01000000)        ;periodic interrupt
  (define RegC_AF    #b00100000)        ;alarm interrupt
  (define RegC_UF    #b00010000)        ;update-ended interrupt
  (define RegD_VRT   #b10000000)        ;valid RAM and time

  ;; Cached values
  (define B (rtc-read RTC_RegB))
  (define D (rtc-read RTC_RegD))

  ;; Decode a number which based on bit 2 of RegB can be BCD or binary.
  (define (decode-byte x)
    (if (fxzero? (fxand B #b100))
        (fx+ (fx* (fxarithmetic-shift-right x 4) 10)
             (fxand x #xf))
        x))

  (define century
    (let ((c (decode-byte (rtc-read RTC_Century))))
      (if (fx<? c 19) 2000 (fx* 100 c))))
  (define valid-time?
    (not (fxzero? (fxand D RegD_VRT))))

  ;; The chip can be made to report the hours in 24-hour or 12-hour
  ;; format.
  (define (decode-hour x)
    (let ((h (decode-byte (fxbit-field x 0 7))))
      (if (fxzero? (fxand B RegB_24/12))
          ;; In 12-hour mode the chip divides the day into the four
          ;; parts shown here.
          (if (fxbit-set? x 7)          ;later part of the day?
              (if (eqv? h 12)
                  h
                  (fx+ h 12))
              (if (eqv? h 12)
                  0
                  h))
          h)))                          ;already in 24-hour time

  ;; The day of the week is stored off-by-two.
  (define (decode-day-of-week x)
    (fxmod (fx- (decode-byte x) 2) 7))

  (define (wait-while-update-in-progress)
    (let lp ()
      (unless (fxzero? (fxand (rtc-read #xA) #b10000000))
        (sleep 0.001)
        (lp))))

  (define (read-date)
    ;; Nanoseconds is zero because we read it from the RTC directly
    ;; after an update. So it's as close as we can get. The timezone
    ;; offset is zero because only DOS sets the RTC to local time.
    (define nanoseconds 0)
    (define tzoffset 0)
    (make-date nanoseconds
               (decode-byte (rtc-read RTC_Seconds))
               (decode-byte (rtc-read RTC_Minutes))
               (decode-hour (rtc-read RTC_Hours))
               (decode-byte (rtc-read RTC_Day_of_Month))
               (decode-byte (rtc-read RTC_Month))
               (fx+ century (decode-byte (rtc-read RTC_Year)))
               tzoffset))

  (define irq-token (enable-irq irq))

  (when (not valid-time?)
    (send-log WARNING "The mc146818 RTC time is not valid"))

  ;; Enable and acknowledge IRQs
  (rtc-read RTC_RegC)
  ;; Enable update-ended IRQs (1 Hz).
  (rtc-write RTC_RegB (fxior B RegB_UIE))
  (let lp ((current-date #f) (pending '()))
    (match (perform-operation
            (choice-operation
             (if valid-time?
                 (wrap-operation (wait-irq-operation irq-token) (lambda _ 'int))
                 (choice-operation))
             (if (null? pending)
                 (choice-operation)
                 (wrap-operation (sleep-operation 2) (lambda _ 'timeout)))
             (wrap-operation (get-operation (rtc-device-request-channel dev))
                             (lambda (x) (cons 'req x)))))
      ['int
       ;; Check the IRQ reason
       (let ((status (rtc-read RTC_RegC)))
         (cond ((not (fxzero? (fxand status RegC_UF)))
                ;; The RTC just updated the time.
                (let ((ticks (current-ticks))
                      (new-date (read-date)))
                  (for-each (lambda (ch) (put-message ch (cons new-date ticks))) pending)
                  (lp new-date '())))
               (else
                (lp current-date pending))))]

      [('req . ('get-date (? channel? resp-ch)))
       (lp current-date (cons resp-ch pending))]

      [('req . ('set-date (? channel? resp-ch) (? date? time)))
       ;; XXX: must be set at least two seconds before the end of
       ;; the 29th or 30th date of the month
       (rtc-write RTC_RegB (fxior B (fxior RegB_UIE RegB_SET)))
       (send-log ERROR "set-date is not implemented")
       (rtc-write RTC_RegB (fxior B RegB_UIE))
       (put-message resp-ch 'not-implemented)
       (lp current-date pending)]

      [('req . x)
       (send-log ERROR (call-with-string-output-port
                         (lambda (p)
                           (display "Invalid RTC command: " p)
                           (write x p))))
       (lp current-date pending)]

      ['timeout
       ;; We've timed out while waiting for a new second from the
       ;; hardware. Maybe it's broken.
       (for-each (lambda (ch) (put-message ch #f)) pending)
       (lp current-date '())]))))
