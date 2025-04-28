;; SRFI-19: Time Data Types and Procedures.
;;
;; Copyright (C) I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: MIT
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; SRFI 19: Time Data Types and Procedures

;; This is based on the reference implementation from
;; <https://github.com/scheme-requests-for-implementation/srfi-19>.
;; Commit errata-5-11-g5a616a3. It has been converted to an R6RS
;; library, updated for Loko and given a good scrub behind the ears.

;; XXX: A more portable copy of this library exists in chez-srfi.

(library (srfi :19 time)
  (export
    ;; Constants
    time-duration
    time-monotonic
    time-process
    time-tai
    time-thread
    time-utc

    ;; Current time and clock resolution
    current-date
    current-julian-day
    current-modified-julian-day
    current-time
    time-resolution

    ;; Time objects and accessors
    make-time time?
    time-type
    time-nanosecond
    time-second
    set-time-type!
    set-time-nanosecond!
    set-time-second!
    copy-time

    ;; Time comparison procedures
    time<=? time<? time=? time>=? time>?

    ;; Time arithmetic procedures
    time-difference
    time-difference!
    add-duration
    add-duration!
    subtract-duration
    subtract-duration!

    ;; Date objects and accessors
    make-date date?
    date-nanosecond
    date-second
    date-minute
    date-hour
    date-day
    date-month
    date-year
    date-zone-offset
    date-year-day
    date-week-day
    date-week-number

    ;; Time/Date/Julian Day/Modified Julian Day Converters
    date->julian-day
    date->modified-julian-day
    date->time-monotonic
    date->time-tai
    date->time-utc
    julian-day->date
    julian-day->time-monotonic
    julian-day->time-tai
    julian-day->time-utc
    modified-julian-day->date
    modified-julian-day->time-monotonic
    modified-julian-day->time-tai
    modified-julian-day->time-utc
    time-monotonic->date
    time-monotonic->julian-day
    time-monotonic->modified-julian-day
    time-monotonic->time-tai
    time-monotonic->time-tai!
    time-monotonic->time-utc
    time-monotonic->time-utc!
    time-tai->date
    time-tai->julian-day
    time-tai->modified-julian-day
    time-tai->time-monotonic
    time-tai->time-monotonic!
    time-tai->time-utc
    time-tai->time-utc!
    time-utc->date
    time-utc->julian-day
    time-utc->modified-julian-day
    time-utc->time-monotonic
    time-utc->time-monotonic!
    time-utc->time-tai
    time-utc->time-tai!

    ;; Date to String/String to Date Converters
    date->string
    string->date)
  (import
    (rnrs)
    (rnrs r5rs)
    (rnrs mutable-strings)
    (loko system time)
    (only (loko) get-output-string open-output-string))

(define-syntax define-optional
  (lambda (x)
    (define (opt-clauses name args* opt*)
      (syntax-case opt* ()
        [() '()]
        [((lhs rhs) (lhs* rhs*) ...)
         (with-syntax ([(args ...) args*])
           #`([(args ...) (#,name args ... rhs)]
              #,@(opt-clauses name #'(args ... lhs) #'((lhs* rhs*) ...))))]))
    (syntax-case x ()
      [(_ (name args ... [(lhs* rhs*) ...])
          . body)
       #`(define name
           (case-lambda
             #,@(opt-clauses #'name #'(args ...) #'((lhs* rhs*) ...))
             [(args ... lhs* ...) . body]))])))

(define time-tai 'time-tai)
(define time-utc 'time-utc)
(define time-monotonic 'time-monotonic)
(define time-thread 'time-thread)
(define time-process 'time-process)
(define time-duration 'time-duration)

;;-- LOCALE dependent constants

(define tm:locale-number-separator ".")

(define tm:locale-abbr-weekday-vector '#("Sun" "Mon" "Tue" "Wed"
                                         "Thu" "Fri" "Sat"))
(define tm:locale-long-weekday-vector '#("Sunday" "Monday"
                                         "Tuesday" "Wednesday"
                                         "Thursday" "Friday"
                                         "Saturday"))
;; note empty string in 0th place.
(define tm:locale-abbr-month-vector   '#("" "Jan" "Feb" "Mar"
                                         "Apr" "May" "Jun" "Jul"
                                         "Aug" "Sep" "Oct" "Nov"
                                         "Dec"))
(define tm:locale-long-month-vector   '#("" "January" "February"
                                         "March" "April" "May"
                                         "June" "July" "August"
                                         "September" "October"
                                         "November" "December"))

(define tm:locale-pm "PM")
(define tm:locale-am "AM")

;; See date->string
(define tm:locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
(define tm:locale-short-date-format "~m/~d/~y")
(define tm:locale-time-format "~H:~M:~S")
(define tm:iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")

;;-- Miscellaneous Constants.
;;-- only the tm:tai-epoch-in-jd might need changing if
;;   a different epoch is used.

(define tm:nano (expt 10 9))
(define tm:sid  86400)    ; seconds in a day
(define tm:sihd 43200)    ; seconds in a half day
(define tm:tai-epoch-in-jd 4881175/2) ; julian day number for 'the epoch'

;; A very simple error system for the time procedures
(define tm:time-error-types
  '(invalid-clock-type
    unsupported-clock-type
    incompatible-time-types
    not-duration
    dates-are-immutable
    bad-date-format-string
    bad-date-template-string
    invalid-month-specification))

(define (tm:time-error caller type value)
  (assertion-violation caller "Time error" type value))

;; Reads leap-seconds.list from <https://www.iana.org/time-zones>
(define (tm:read-leap-seconds-list filename)
  (call-with-input-file filename
    (lambda (port)
      (let lp ((table '()))
        (let ((line (get-line port)))
          (cond ((eof-object? line)
                 table)
                ((or (eqv? (string-length line) 0)
                     (char=? (string-ref line 0) #\#))
                 (lp table))
                (else
                 (let* ((data (open-string-input-port line))
                        (ntp-timestamp (read data))
                        (secs (read data)))
                   (lp (cons (cons (time-second (modified-julian-day->time-utc
                                                 (+ (/ ntp-timestamp 86400)
                                                    15020)))
                                   secs)
                             table))))))))))

;; each entry is ( utc seconds since epoch . # seconds to add for tai )
;; note they go higher to lower, and end in 1972.
(define tm:leap-second-table
  '((1483228800 . 37)
    (1435708800 . 36)
    (1341100800 . 35)
    (1230768000 . 34)
    (1136073600 . 33)
    (915148800 . 32)
    (867715200 . 31)
    (820454400 . 30)
    (773020800 . 29)
    (741484800 . 28)
    (709948800 . 27)
    (662688000 . 26)
    (631152000 . 25)
    (567993600 . 24)
    (489024000 . 23)
    (425865600 . 22)
    (394329600 . 21)
    (362793600 . 20)
    (315532800 . 19)
    (283996800 . 18)
    (252460800 . 17)
    (220924800 . 16)
    (189302400 . 15)
    (157766400 . 14)
    (126230400 . 13)
    (94694400 . 12)
    (78796800 . 11)
    (63072000 . 10)))

;; Updated through IERS Bulletin C60
;; File expires on:  28 June 2021

;; TODO: Automatically read the leap seconds table, with error
;; handling, when the above table is out of date.

(define (read-leap-second-table filename)
  (set! tm:leap-second-table (tm:read-leap-seconds-list filename)))

;; (read-leap-second-table "/usr/share/zoneinfo/leap-seconds.list")

(define (tm:leap-second-delta utc-seconds)
  (letrec ((lsd (lambda (table)
                  (cond
                    ((fx>=? utc-seconds (caar table))
                     (cdar table))
                    (else (lsd (cdr table)))))))
    (if (fx<? utc-seconds (fx* (fx* (fx- 1972 1970) 365) tm:sid))
        0
        (lsd tm:leap-second-table))))

;; Going from TAI seconds to UTC seconds
(define (tm:leap-second-neg-delta tai-seconds)
  (letrec ((lsd (lambda (table)
                  (cond ((null? table) 0)
                        ((fx<=? (cdar table) (fx- tai-seconds (caar table)))
                         (cdar table))
                        (else (lsd (cdr table)))))))
    (if (fx<? tai-seconds (fx* (fx* (fx- 1972 1970) 365) tm:sid))
        0
        (lsd tm:leap-second-table))))

;; Defined in (loko runtime time)
;; (define-record-type time
;;   (sealed #t)
;;   (fields (mutable type       time-type       set-time-type!)
;;           (mutable nanosecond time-nanosecond set-time-nanosecond!)
;;           (mutable second     time-second     set-time-second!)))

(define (copy-time time)
  (make-time (time-type time)
             (time-nanosecond time)
             (time-second time)))

;;; current-time

(define (tm:current-time-utc)
  (current-time/utc))

(define (tm:current-time-tai)
  (let* ((t (current-second))
         (s (floor t))
         (ns (floor (* (- t s) #e1e9))))
    (make-time time-tai (exact ns) (exact s))))

(define (tm:current-time-monotonic)
  (current-time/monotonic))

(define (tm:current-time-thread)
  (current-time/process))

(define (tm:current-time-process)
  (current-time/process))

(define-optional (current-time [(clock-type time-utc)])
  (cond
    ((eq? clock-type time-tai) (tm:current-time-tai))
    ((eq? clock-type time-utc) (tm:current-time-utc))
    ((eq? clock-type time-monotonic) (tm:current-time-monotonic))
    ((eq? clock-type time-thread) (tm:current-time-thread))
    ((eq? clock-type time-process) (tm:current-time-process))
    (else (tm:time-error 'current-time 'invalid-clock-type clock-type))))

;;; -- Time resolution
;; This is the resolution of the clock in nanoseconds.

(define-optional (time-resolution [(clock-type time-utc)])
  (cond
    ((eq? clock-type time-tai) 1)
    ((eq? clock-type time-utc) 1)
    ((eq? clock-type time-monotonic) 1)
    ((eq? clock-type time-thread) 1)
    ((eq? clock-type time-process) 1)
    (else (tm:time-error 'time-resolution 'invalid-clock-type clock-type))))

;;; -- Time comparisons

(define (tm:time-compare-check time1 time2 caller)
  (if (or (not (and (time? time1) (time? time2)))
          (not (eq? (time-type time1) (time-type time2))))
      (tm:time-error caller 'incompatible-time-types (list time1 time2))
      #t))

(define (time=? time1 time2)
  (tm:time-compare-check time1 time2 'time=?)
  (and (= (time-second time1) (time-second time2))
       (= (time-nanosecond time1) (time-nanosecond time2))))

(define (time>? time1 time2)
  (tm:time-compare-check time1 time2 'time>?)
  (or (> (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (> (time-nanosecond time1) (time-nanosecond time2)))))

(define (time<? time1 time2)
  (tm:time-compare-check time1 time2 'time<?)
  (or (< (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (< (time-nanosecond time1) (time-nanosecond time2)))))

(define (time>=? time1 time2)
  (tm:time-compare-check time1 time2 'time>=?)
  (or (> (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (>= (time-nanosecond time1) (time-nanosecond time2)))))

(define (time<=? time1 time2)
  (tm:time-compare-check time1 time2 'time<=?)
  (or (< (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (<= (time-nanosecond time1) (time-nanosecond time2)))))

;;; -- time arithmetic

(define (tm:time->nanoseconds time)
  (define (sign1 n)
    (if (negative? n) -1 1))
  (+ (* (time-second time) tm:nano)
     (time-nanosecond time)))

(define (tm:nanoseconds->time time-type nanoseconds)
  (make-time time-type
             (remainder nanoseconds tm:nano)
             (quotient nanoseconds tm:nano)))

(define (tm:nanoseconds->values nanoseconds)
  (values (abs (remainder nanoseconds tm:nano))
          (quotient nanoseconds tm:nano)))

(define (tm:time-difference time1 time2 time3)
  (if (or (not (and (time? time1) (time? time2)))
          (not (eq? (time-type time1) (time-type time2))))
      (tm:time-error 'time-difference 'incompatible-time-types #f))
  (if (time=? time1 time2)
      (begin
        (set-time-type! time3 time-duration)
        (set-time-second! time3 0)
        (set-time-nanosecond! time3 0))
      (let-values ([(nanos secs)
                    (tm:nanoseconds->values (- (tm:time->nanoseconds time1)
                                               (tm:time->nanoseconds time2)))])
        (set-time-type! time3 time-duration)
        (set-time-second! time3 secs)
        (set-time-nanosecond! time3 nanos)))
  time3)

(define (time-difference time1 time2)
  (tm:time-difference time1 time2 (make-time #f #f #f)))

(define (time-difference! time1 time2)
  (tm:time-difference time1 time2 time1))

(define (tm:add-duration time1 duration time3)
  (if (not (and (time? time1) (time? duration)))
      (tm:time-error 'add-duration 'incompatible-time-types #f))
  (if (not (eq? (time-type duration) time-duration))
      (tm:time-error 'add-duration 'not-duration duration)
      (let ((sec-plus (+ (time-second time1) (time-second duration)))
            (nsec-plus (+ (time-nanosecond time1) (time-nanosecond duration))))
        (let ((r (remainder nsec-plus tm:nano))
              (q (quotient nsec-plus tm:nano)))
          ;; (set-time-type! time3 (time-type time1))
          (if (negative? r)
              (begin
                (set-time-second! time3 (+ sec-plus q -1))
                (set-time-nanosecond! time3 (+ tm:nano r)))
              (begin
                (set-time-second! time3 (+ sec-plus q))
                (set-time-nanosecond! time3 r)))
          time3))))

(define (add-duration time1 duration)
  (tm:add-duration time1 duration (make-time (time-type time1) #f #f)))

(define (add-duration! time1 duration)
  (tm:add-duration time1 duration time1))

(define (tm:subtract-duration time1 duration time3)
  (if (not (and (time? time1) (time? duration)))
      (tm:time-error 'add-duration 'incompatible-time-types #f))
  (if (not (eq? (time-type duration) time-duration))
      (tm:time-error 'tm:subtract-duration 'not-duration duration)
      (let ((sec-minus  (- (time-second time1) (time-second duration)))
            (nsec-minus (- (time-nanosecond time1) (time-nanosecond duration))))
        (let ((r (remainder nsec-minus tm:nano))
              (q (quotient nsec-minus tm:nano)))
          (if (negative? r)
              (begin
                (set-time-second! time3 (- sec-minus q 1))
                (set-time-nanosecond! time3 (+ tm:nano r)))
              (begin
                (set-time-second! time3 (- sec-minus q))
                (set-time-nanosecond! time3 r)))
          time3))))

(define (subtract-duration time1 duration)
  (tm:subtract-duration time1 duration (make-time (time-type time1) #f #f)))

(define (subtract-duration! time1 duration)
  (tm:subtract-duration time1 duration time1))

;;; -- converters between types.

(define (tm:time-tai->time-utc! time-in time-out caller)
  (if (not (eq? (time-type time-in) time-tai))
      (tm:time-error caller 'incompatible-time-types time-in))
  (set-time-type! time-out time-utc)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (- (time-second time-in)
                                    (tm:leap-second-neg-delta
                                     (time-second time-in))))
  time-out)

(define (time-tai->time-utc time-in)
  (tm:time-tai->time-utc! time-in (make-time #f #f #f) 'time-tai->time-utc))

(define (time-tai->time-utc! time-in)
  (tm:time-tai->time-utc! time-in time-in 'time-tai->time-utc!))

(define (tm:time-utc->time-tai! time-in time-out caller)
  (if (not (eq? (time-type time-in) time-utc))
      (tm:time-error caller 'incompatible-time-types time-in))
  (set-time-type! time-out time-tai)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (+ (time-second time-in)
                                    (tm:leap-second-delta
                                     (time-second time-in))))
  time-out)

(define (time-utc->time-tai time-in)
  (tm:time-utc->time-tai! time-in (make-time #f #f #f) 'time-utc->time-tai))

(define (time-utc->time-tai! time-in)
  (tm:time-utc->time-tai! time-in time-in 'time-utc->time-tai!))

;; SRFI 170 says that time-monotonic is the same as CLOCK_MONOTONIC,
;; so this code is faced with the absurd situation of converting from
;; a suspendable clock to a real clock. Let's assume the monotonic
;; clock ticks from boot, ignore suspend, and accept the inaccurate
;; conversion caused by the two current-time/xxx calls.
(define (monotonic-epoch)
  (let* ((rt (current-time/utc))
         (mt (current-time/monotonic)))
    (set-time-type! mt time-utc)
    (time-difference! rt mt)))

(define (time-monotonic->time-utc time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (tm:time-error 'time-monotonic->time-utc 'incompatible-time-types time-in))
  (let ((t (copy-time time-in)))
    (set-time-type! t time-utc)
    (add-duration t (monotonic-epoch))))

(define time-monotonic->time-utc! time-monotonic->time-utc)

(define (time-monotonic->time-tai time-in)
  (time-utc->time-tai (time-monotonic->time-utc time-in)))

(define time-monotonic->time-tai! time-monotonic->time-tai)

(define (time-utc->time-monotonic time-in)
  (unless (eq? (time-type time-in) time-utc)
    (tm:time-error 'time-utc->time-monotonic 'incompatible-time-types time-in))
  (let ((t (copy-time time-in))
        (e (monotonic-epoch)))
    (set-time-type! t time-monotonic)
    (set-time-type! e time-duration)
    (subtract-duration t e)))

(define time-utc->time-monotonic! time-utc->time-monotonic)

(define (time-tai->time-monotonic time-in)
  (time-utc->time-monotonic (time-tai->time-utc time-in)))

(define time-tai->time-monotonic! time-tai->time-monotonic)

;;; -- date structures

(define-record-type date
  (sealed #t)
  (fields (mutable nanosecond date-nanosecond tm:set-date-nanosecond!)
          (mutable second date-second tm:set-date-second!)
          (mutable minute date-minute tm:set-date-minute!)
          (mutable hour date-hour tm:set-date-hour!)
          (mutable day date-day tm:set-date-day!)
          (mutable month date-month tm:set-date-month!)
          (mutable year date-year tm:set-date-year!)
          (mutable zone-offset date-zone-offset tm:set-date-zone-offset!)))

;; gives the julian day which starts at noon.
(define (tm:encode-julian-day-number day month year)
  (let* ((a (quotient (fx- 14 month) 12))
         (y (- (- (fx+ year 4800) a) (if (fxnegative? year) -1 0)))
         (m (- (+ month (fx* 12 a)) 3)))
    (+ day
       (quotient (+ (fx* 153 m) 2) 5)
       (fx* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

(define (tm:char-pos char str index len)
  (cond
    ((>= index len) #f)
    ((char=? (string-ref str index) char)
     index)
    (else
     (tm:char-pos char str (+ index 1) len))))

(define (tm:fractional-part r)
  (if (integer? r)
      "0"
      (let ((str (number->string (exact->inexact r))))
        (let ((ppos (tm:char-pos #\. str 0 (string-length str))))
          (substring str (+ ppos 1) (string-length str))))))

;; gives the seconds/date/month/year
(define (tm:decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
         (a (+ days 32044))
         (b (quotient (+ (* 4 a) 3) 146097))
         (c (- a (quotient (* 146097 b) 4)))
         (d (quotient (+ (* 4 c) 3) 1461))
         (e (- c (quotient (* 1461 d) 4)))
         (m (quotient (+ (* 5 e) 2) 153))
         (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
      (* (- jdn days) tm:sid)
      (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
      (+ m 3 (* -12 (quotient m 10)))
      (if (>= 0 y) (- y 1) y))))

(define (tm:local-tz-offset)
  ;; TODO: Local time zones.
  0)

;; special thing -- ignores nanos
(define (tm:time->julian-day-number seconds tz-offset)
  (+ (/ (+ seconds
           tz-offset
           tm:sihd)
        tm:sid)
     tm:tai-epoch-in-jd))

(define (tm:tai-before-leap-second? second)
  (exists (lambda (x)
            (= second (- (+ (car x) (cdr x)) 1)))
          tm:leap-second-table))

(define (tm:time->date time offset ttype)
  (unless (eq? (time-type time) ttype)
    (tm:time-error 'time->date 'incompatible-time-types time))
  (let-values ([(secs date month year)
                (tm:decode-julian-day-number
                 (tm:time->julian-day-number (time-second time) offset))])
    (let* ((hours   (quotient secs (* 60 60)))
           (rem     (remainder secs (* 60 60)))
           (minutes (quotient rem 60))
           (seconds (remainder rem 60)))
      (make-date (time-nanosecond time)
                 seconds
                 minutes
                 hours
                 date
                 month
                 year
                 offset))))

(define-optional (time-tai->date time [(tz-offset (tm:local-tz-offset))])
  (if (tm:tai-before-leap-second? (time-second time))
      ;; if it's *right* before the leap, we need to pretend to subtract a second ...
      (let ((d (tm:time->date (subtract-duration! (time-tai->time-utc time)
                                                  (make-time time-duration 0 1))
                              tz-offset time-utc)))
        (tm:set-date-second! d 60)
        d)
      (tm:time->date (time-tai->time-utc time) tz-offset time-utc)))

(define-optional (time-utc->date time [(tz-offset (tm:local-tz-offset))])
  (tm:time->date time tz-offset time-utc))

(define-optional (time-monotonic->date time [(tz-offset (tm:local-tz-offset))])
  (time-utc->date (time-monotonic->time-utc time) tz-offset))

(define (date->time-utc date)
  (let ((nanosecond (date-nanosecond date))
        (second (date-second date))
        (minute (date-minute date))
        (hour (date-hour date))
        (day (date-day date))
        (month (date-month date))
        (year (date-year date))
        (offset (date-zone-offset date)))
    (let ((jdays (- (tm:encode-julian-day-number day month year)
                    tm:tai-epoch-in-jd)))
      (make-time time-utc
                 nanosecond
                 (+ (* (- jdays 1/2) (* 24 60 60))
                    (* hour (* 60 60))
                    (* minute 60)
                    second
                    (- offset))))))

(define (date->time-tai d)
  (if (eqv? (date-second d) 60)
      (subtract-duration! (time-utc->time-tai! (date->time-utc d)) (make-time time-duration 0 1))
      (time-utc->time-tai! (date->time-utc d))))

(define (date->time-monotonic date)
  (time-utc->time-monotonic! (date->time-utc date)))

(define (tm:leap-year? year)
  (or (eqv? (fxmod year 400) 0)
      (and (eqv? (fxmod year 4) 0) (not (eqv? (fxmod year 100) 0)))))

(define (leap-year? date)
  (tm:leap-year? (date-year date)))

(define (tm:year-day day month year)
  (define tm:month '#(0 31 59 90 120 151 181 212 243 273 304 334))
  (unless (fx<=? 1 month 12)
    (tm:time-error 'date-year-day 'invalid-month-specification month))
  (let ((days-pr (vector-ref tm:month (fx- month 1))))
    (if (and (tm:leap-year? year) (fx>? month 2))
        (fx+ day (fx+ days-pr 1))
        (fx+ day days-pr))))

(define (date-year-day date)
  (tm:year-day (date-day date) (date-month date) (date-year date)))

;; From calendar FAQ
(define (tm:week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
               (quotient y 400) (quotient (* 31 m) 12))
            7)))

(define (date-week-day date)
  (tm:week-day (date-day date) (date-month date) (date-year date)))

(define (tm:days-before-first-week date day-of-week-starting-week)
  (let* ((first-day (make-date 0 0 0 0
                               1
                               1
                               (date-year date)
                               #f))
         (fdweek-day (date-week-day first-day)))
    (modulo (- day-of-week-starting-week fdweek-day)
            7)))

(define (date-week-number date day-of-week-starting-week)
  (quotient (- (date-year-day date)
               (tm:days-before-first-week date day-of-week-starting-week))
            7))

;; This procedure is written by Shiro Kawai in 2021 and was submitted
;; to the upstream SRFI-19 repository.
(define (tm:date-week-number-iso date)
  (define (floor-quotient x y)
    (exact (floor (/ x y))))
  ;; The week with the year's first Thursday is week 01.
  (let* ([first-day-of-the-week (tm:week-day 1 1 (date-year date))]
         [offset (if (> first-day-of-the-week 4) 0 1)]
         ;; -2: decrement one day to compensate 1-origin of date-year-day,
         ;; and decrement one more day for Sunday belongs to the previous week.
         [w (+ (floor-quotient (+ (date-year-day date) first-day-of-the-week -2)
                               7)
               offset)])
    (cond [(zero? w)
           ;; date belongs to the last week of the previous year
           (tm:date-week-number-iso (make-date 0 0 0 0 31 12
                                               (- (date-year date) 1) 0))]
          [(and (= w 53)
                (<= (tm:week-day 1 1 (+ (date-year date) 1)) 4))
           ;; date belongs to the first week of the next year
           1]
          [else w])))

(define-optional (current-date [(tz-offset (tm:local-tz-offset))])
  (time-utc->date (current-time time-utc) tz-offset))

;; given a 'two digit' number, find the year within 50 years +/-
(define (tm:natural-year n)
  (let* ((current-year (date-year (current-date)))
         (current-century (* (quotient current-year 100) 100)))
    (cond
      ((>= n 100) n)
      ((<  n 0) n)
      ((<=  (- (+ current-century n) current-year) 50)
       (+ current-century n))
      (else
       (+ (- current-century 100) n)))))

(define (date->julian-day date)
  (let ((nanosecond (date-nanosecond date))
        (second (date-second date))
        (minute (date-minute date))
        (hour (date-hour date))
        (day (date-day date))
        (month (date-month date))
        (year (date-year date))
        (offset (date-zone-offset date)))
    (+ (tm:encode-julian-day-number day month year)
       (- 1/2)
       (+ (/ (/ (+ (* hour 60 60)
                   (* minute 60) second (/ nanosecond tm:nano)) tm:sid)
             (- offset))))))

(define (date->modified-julian-day date)
  (- (date->julian-day date)
     4800001/2))

(define (time-utc->julian-day time)
  (if (not (eq? (time-type time) time-utc))
      (tm:time-error 'time-utc->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (time-second time) (/ (time-nanosecond time) tm:nano))
        tm:sid)
     tm:tai-epoch-in-jd))

(define (time-utc->modified-julian-day time)
  (- (time-utc->julian-day time)
     4800001/2))

(define (time-tai->julian-day time)
  (if (not (eq? (time-type time) time-tai))
      (tm:time-error 'time-tai->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (- (time-second time)
              (tm:leap-second-delta (time-second time)))
           (/ (time-nanosecond time) tm:nano))
        tm:sid)
     tm:tai-epoch-in-jd))

(define (time-tai->modified-julian-day time)
  (- (time-tai->julian-day time)
     4800001/2))

(define (time-monotonic->julian-day time)
  (time-utc->julian-day (time-monotonic->time-utc time)))

(define (time-monotonic->modified-julian-day time)
  (- (time-monotonic->julian-day time)
     4800001/2))

(define (julian-day->time-utc jdn)
  (let ((nanosecs (* tm:nano tm:sid (- jdn tm:tai-epoch-in-jd))))
    (make-time time-utc
               (remainder nanosecs tm:nano)
               (floor (/ nanosecs tm:nano)))))

(define (julian-day->time-tai jdn)
  (time-utc->time-tai! (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  (time-utc->time-monotonic! (julian-day->time-utc jdn)))

(define-optional (julian-day->date jdn [(offset (tm:local-tz-offset))])
  (time-utc->date (julian-day->time-utc jdn) offset))

(define-optional (modified-julian-day->date jdn [(offset (tm:local-tz-offset))])
  (julian-day->date (+ jdn 4800001/2) offset))

(define (modified-julian-day->time-utc jdn)
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  (time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  (time-utc->modified-julian-day (current-time time-utc)))

;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH if #f,
;; no padding is done, and it's as if number->string was used.
;; if string is longer than LENGTH, it's as if number->string was used.

(define (tm:padding n pad-with length)
  (let* ((str (number->string n))
         (str-len (string-length str)))
    (if (or (> str-len length)
            (not pad-with))
        str
        (let* ((new-str (make-string length pad-with))
               (new-str-offset (- (string-length new-str)
                                  str-len)))
          (do ((i 0 (+ i 1)))
              ((>= i (string-length str)))
            (string-set! new-str (+ new-str-offset i)
                         (string-ref str i)))
          new-str))))

(define (tm:last-n-digits i n)
  (abs (remainder i (expt 10 n))))

(define (tm:locale-abbr-weekday n)
  (vector-ref tm:locale-abbr-weekday-vector n))

(define (tm:locale-long-weekday n)
  (vector-ref tm:locale-long-weekday-vector n))

(define (tm:locale-abbr-month n)
  (vector-ref tm:locale-abbr-month-vector n))

(define (tm:locale-long-month n)
  (vector-ref tm:locale-long-month-vector n))

(define (tm:vector-find needle haystack comparator)
  (let ((len (vector-length haystack)))
    (define (tm:vector-find-int index)
      (cond
        ((>= index len) #f)
        ((comparator needle (vector-ref haystack index)) index)
        (else (tm:vector-find-int (+ index 1)))))
    (tm:vector-find-int 0)))

(define (tm:locale-abbr-weekday->index string)
  (tm:vector-find string tm:locale-abbr-weekday-vector string=?))

(define (tm:locale-long-weekday->index string)
  (tm:vector-find string tm:locale-long-weekday-vector string=?))

(define (tm:locale-abbr-month->index string)
  (tm:vector-find string tm:locale-abbr-month-vector string=?))

(define (tm:locale-long-month->index string)
  (tm:vector-find string tm:locale-long-month-vector string=?))

;; do nothing.
;; Your implementation might want to do something...
;;
(define (tm:locale-print-time-zone date port)
  (values))

;; Again, locale specific.
(define (tm:locale-am/pm hr)
  (if (fx>? hr 11) tm:locale-pm tm:locale-am))

(define (tm:tz-printer offset port)
  (cond
    ((eqv? offset 0) (display "Z" port))
    ((fxnegative? offset) (display "-" port))
    (else (display "+" port)))
  (if (not (eqv? offset 0))
      (let ((hours   (abs (quotient offset (* 60 60))))
            (minutes (abs (quotient (remainder offset (* 60 60)) 60))))
        (display (tm:padding hours #\0 2) port)
        (display (tm:padding minutes #\0 2) port))))

;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
(define (tm:get-formatter char)
  (case char
    ((#\~) (lambda (_date _pad-with port) (display #\~ port)))
    ((#\a) (lambda (date _pad-with port)
             (display (tm:locale-abbr-weekday (date-week-day date))
                      port)))
    ((#\A) (lambda (date _pad-with port)
             (display (tm:locale-long-weekday (date-week-day date))
                      port)))
    ((#\b) (lambda (date _pad-with port)
             (display (tm:locale-abbr-month (date-month date))
                      port)))
    ((#\B) (lambda (date _pad-with port)
             (display (tm:locale-long-month (date-month date))
                      port)))
    ((#\c) (lambda (date _pad-with port)
             (display (date->string date tm:locale-date-time-format) port)))
    ((#\d) (lambda (date _pad-with port)
             (display (tm:padding (date-day date)
                                  #\0 2)
                      port)))
    ((#\D) (lambda (date _pad-with port)
             (display (date->string date "~m/~d/~y") port)))
    ((#\e) (lambda (date _pad-with port)
             (display (tm:padding (date-day date)
                                  #\space 2)
                      port)))
    ((#\f) (lambda (date pad-with port)
             (if (> (date-nanosecond date)
                    tm:nano)
                 (display (tm:padding (+ (date-second date) 1)
                                      pad-with 2)
                          port)
                 (display (tm:padding (date-second date)
                                      pad-with 2)
                          port))
             (let* ((ns (tm:fractional-part (/
                                             (date-nanosecond date)
                                             tm:nano 1.0)))
                    (le (string-length ns)))
               (if (> le 2)
                   (begin
                     (display tm:locale-number-separator port)
                     (display (substring ns 2 le) port))))))
    ((#\h) (lambda (date _pad-with port)
             (display (date->string date "~b") port)))
    ((#\H) (lambda (date pad-with port)
             (display (tm:padding (date-hour date)
                                  pad-with 2)
                      port)))
    ((#\I) (lambda (date pad-with port)
             (let ((hr (date-hour date)))
               (if (> hr 12)
                   (display (tm:padding (- hr 12)
                                        pad-with 2)
                            port)
                   (display (tm:padding hr
                                        pad-with 2)
                            port)))))
    ((#\j) (lambda (date pad-with port)
             (display (tm:padding (date-year-day date)
                                  pad-with 3)
                      port)))
    ((#\k) (lambda (date _pad-with port)
             (display (tm:padding (date-hour date)
                                  #\space 2)
                      port)))
    ((#\l) (lambda (date _pad-with port)
             (let ((hr (if (> (date-hour date) 12)
                           (- (date-hour date) 12) (date-hour date))))
               (display (tm:padding hr  #\space 2)
                        port))))
    ((#\m) (lambda (date pad-with port)
             (display (tm:padding (date-month date)
                                  pad-with 2)
                      port)))
    ((#\M) (lambda (date pad-with port)
             (display (tm:padding (date-minute date)
                                  pad-with 2)
                      port)))
    ((#\n) (lambda (_date _pad-with port)
             (newline port)))
    ((#\N) (lambda (date pad-with port)
             (display (tm:padding (date-nanosecond date)
                                  pad-with 9)
                      port)))
    ((#\p) (lambda (date _pad-with port)
             (display (tm:locale-am/pm (date-hour date)) port)))
    ((#\r) (lambda (date _pad-with port)
             (display (date->string date "~I:~M:~S ~p") port)))
    ((#\s) (lambda (date _pad-with port)
             (display (time-second (date->time-utc date)) port)))
    ((#\S) (lambda (date pad-with port)
             (if (> (date-nanosecond date)
                    tm:nano)
                 (display (tm:padding (+ (date-second date) 1)
                                      pad-with 2)
                          port)
                 (display (tm:padding (date-second date)
                                      pad-with 2)
                          port))))
    ((#\t) (lambda (_date _pad-with port)
             (display (integer->char 9) port)))
    ((#\T) (lambda (date _pad-with port)
             (display (date->string date "~H:~M:~S") port)))
    ((#\U) (lambda (date _pad-with port)
             (if (> (tm:days-before-first-week date 0) 0)
                 (display (tm:padding (+ (date-week-number date 0) 1)
                                      #\0 2) port)
                 (display (tm:padding (date-week-number date 0)
                                      #\0 2) port))))
    ((#\V) (lambda (date _pad-with port)
             (display (tm:padding (tm:date-week-number-iso date)
                                  #\0 2) port)))
    ((#\w) (lambda (date _pad-with port)
             (display (date-week-day date) port)))
    ((#\x) (lambda (date _pad-with port)
             (display (date->string date tm:locale-short-date-format) port)))
    ((#\X) (lambda (date _pad-with port)
             (display (date->string date tm:locale-time-format) port)))
    ((#\W) (lambda (date _pad-with port)
             (if (> (tm:days-before-first-week date 1) 0)
                 (display (tm:padding (+ (date-week-number date 1) 1)
                                      #\0 2) port)
                 (display (tm:padding (date-week-number date 1)
                                      #\0 2) port))))
    ((#\y) (lambda (date pad-with port)
             (display (tm:padding (tm:last-n-digits
                                   (date-year date) 2)
                                  pad-with
                                  2)
                      port)))
    ((#\Y) (lambda (date pad-with port)
             (display (tm:padding (date-year date)
                                  pad-with
                                  4)
                      port)))
    ((#\z) (lambda (date _pad-with port)
             (tm:tz-printer (date-zone-offset date) port)))
    ((#\Z) (lambda (date _pad-with port)
             (tm:locale-print-time-zone date port)))
    ((#\1) (lambda (date _pad-with port)
             (display (date->string date "~Y-~m-~d") port)))
    ((#\2) (lambda (date _pad-with port)
             (display (date->string date "~H:~M:~S~z") port)))
    ((#\3) (lambda (date _pad-with port)
             (display (date->string date "~H:~M:~S") port)))
    ((#\4) (lambda (date _pad-with port)
             (display (date->string date "~Y-~m-~dT~H:~M:~S~z") port)))
    ((#\5) (lambda (date _pad-with port)
             (display (date->string date "~Y-~m-~dT~H:~M:~S") port)))
    (else #f)))

(define (tm:date-printer date index format-string str-len port)
  (if (fx>=? index str-len)
      (values)
      (let ((current-char (string-ref format-string index)))
        (if (not (char=? current-char #\~))
            (begin
              (display current-char port)
              (tm:date-printer date (fx+ index 1) format-string str-len port))

            (if (fx=? (fx+ index 1) str-len) ; bad format string.
                (tm:time-error 'tm:date-printer 'bad-date-format-string
                               format-string)
                (let ((pad-char? (string-ref format-string (fx+ index 1))))
                  (cond
                    ((char=? pad-char? #\-)
                     (if (fx=? (fx+ index 2) str-len) ; bad format string.
                         (tm:time-error 'tm:date-printer 'bad-date-format-string
                                        format-string)
                         (let ((formatter (tm:get-formatter
                                           (string-ref format-string
                                                       (fx+ index 2)))))
                           (if (not formatter)
                               (tm:time-error 'tm:date-printer 'bad-date-format-string
                                              format-string)
                               (begin
                                 (formatter date #f port)
                                 (tm:date-printer date (fx+ index 3)
                                                  format-string str-len port))))))

                    ((char=? pad-char? #\_)
                     (if (fx=? (fx+ index 2) str-len) ; bad format string.
                         (tm:time-error 'tm:date-printer 'bad-date-format-string
                                        format-string)
                         (let ((formatter (tm:get-formatter
                                           (string-ref format-string
                                                       (fx+ index 2)))))
                           (if (not formatter)
                               (tm:time-error 'tm:date-printer 'bad-date-format-string
                                              format-string)
                               (begin
                                 (formatter date #\space port)
                                 (tm:date-printer date (fx+ index 3)
                                                  format-string str-len port))))))
                    (else
                     (let ((formatter (tm:get-formatter
                                       (string-ref format-string
                                                   (fx+ index 1)))))
                       (if (not formatter)
                           (tm:time-error 'tm:date-printer 'bad-date-format-string
                                          format-string)
                           (begin
                             (formatter date #\0 port)
                             (tm:date-printer date (fx+ index 2)
                                              format-string str-len port))))))))))))

(define-optional (date->string date [(format-string "~c")])
  (call-with-string-output-port
    (lambda (str-port)
      (tm:date-printer date 0 format-string (string-length format-string) str-port))))

(define (tm:char->int ch)
  (if (char<=? #\0 ch #\9)
      (fx- (char->integer ch) (char->integer #\0))
      (tm:time-error 'string->date 'bad-date-template-string
                     (list "Non-integer character" ch))))

;; read an integer upto n characters long on port; upto -> #f if any length
(define (tm:integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars upto)))
          accum
          (accum-int port (+ (* accum 10) (tm:char->int (get-char port)))
                     (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-integer-reader upto)
  (lambda (port)
    (tm:integer-reader upto port)))

;; read an fractional integer upto n characters long on port; upto -> #f if any length
;;
;; The return value is normalized to upto decimal places. For example, if upto is 9 and
;; the string read is "123", the return value is 123000000.
(define (tm:fractional-integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars upto)))
          (* accum (expt 10 (- upto nchars)))
          (accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-fractional-integer-reader upto)
  (lambda (port)
    (tm:fractional-integer-reader upto port)))

;; read *exactly* n characters and convert to integer; could be padded
(define (tm:integer-reader-exact n port)
  (let ((padding-ok #t))
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
        (cond
          ((>= nchars n) accum)
          ((eof-object? ch)
           (tm:time-error 'string->date 'bad-date-template-string
                          "Premature ending to integer read."))
          ((char-numeric? ch)
           (set! padding-ok #f)
           (accum-int port (+ (* accum 10) (tm:char->int (read-char
                                                          port)))
                      (+ nchars 1)))
          (padding-ok
           (read-char port)             ; consume padding
           (accum-int port accum (+ nchars 1)))
          (else                        ; padding where it shouldn't be
           (tm:time-error 'string->date 'bad-date-template-string
                          "Non-numeric characters in integer read.")))))
    (accum-int port 0 0)))

(define (tm:make-integer-exact-reader n)
  (lambda (port)
    (tm:integer-reader-exact n port)))

(define (tm:zone-reader port)
  (let ((offset 0)
        (positive? #f))
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (tm:time-error 'string->date 'bad-date-template-string
                         (list "Invalid time zone +/-" ch)))
      (if (or (char=? ch #\Z) (char=? ch #\z))
          0
          (begin
            (cond
              ((char=? ch #\+) (set! positive? #t))
              ((char=? ch #\-) (set! positive? #f))
              (else
               (tm:time-error 'string->date 'bad-date-template-string
                              (list "Invalid time zone +/-" ch))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm:time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (* (tm:char->int ch)
                              10 60 60)))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm:time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (tm:char->int ch)
                                        60 60))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm:time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (tm:char->int ch)
                                        10 60))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm:time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (tm:char->int ch)
                                        60))))
            (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
(define (tm:locale-reader port indexer)
  (let ((string-port (open-output-string)))
    (define (read-char-string)
      (let ((ch (peek-char port)))
        (cond ((char-alphabetic? ch)
               (write-char (read-char port) string-port)
               (read-char-string))
              (else
               (get-output-string string-port)))))
    (let* ((str (read-char-string))
           (index (indexer str)))
      (if index index (tm:time-error 'string->date
                                     'bad-date-template-string
                                     (list "Invalid string for " indexer))))))

(define (tm:make-locale-reader indexer)
  (lambda (port)
    (tm:locale-reader port indexer)))

(define (tm:make-char-id-reader char)
  (lambda (port)
    (if (char=? char (read-char port))
        char
        (tm:time-error 'string->date
                       'bad-date-template-string
                       "Invalid character match."))))

;; A List of formatted read directives.
;; Each entry is a list.
;; 1. the character directive;
;; a procedure, which takes a character as input & returns
;; 2. #t as soon as a character on the input port is acceptable
;; for input,
;; 3. a port reader procedure that knows how to read the current port
;; for a value. Its one parameter is the port.
;; 4. a action procedure, that takes the value (from 3.) and some
;; object (here, always the date) and (probably) side-effects it.
;; In some cases (e.g., ~A) the action is to do nothing

(define tm:read-directive
  (let ((ireader4 (tm:make-integer-reader 4))
        (ireader2 (tm:make-integer-reader 2))
        (fireader9 (tm:make-fractional-integer-reader 9))
        ;; (ireaderf (tm:make-integer-reader #f))
        (eireader2 (tm:make-integer-exact-reader 2))
        ;; (eireader4 (tm:make-integer-exact-reader 4))
        (locale-reader-abbr-weekday (tm:make-locale-reader
                                     tm:locale-abbr-weekday->index))
        (locale-reader-long-weekday (tm:make-locale-reader
                                     tm:locale-long-weekday->index))
        (locale-reader-abbr-month   (tm:make-locale-reader
                                     tm:locale-abbr-month->index))
        (locale-reader-long-month   (tm:make-locale-reader
                                     tm:locale-long-month->index))
        (char-fail (lambda (_ch) #t))
        (do-nothing (lambda (_val _object) (values))))
    (lambda (format-char template-string)
      (case format-char
        ((#\~) (values char-fail (tm:make-char-id-reader #\~) do-nothing))
        ((#\a) (values char-alphabetic? locale-reader-abbr-weekday do-nothing))
        ((#\A) (values char-alphabetic? locale-reader-long-weekday do-nothing))
        ((#\b) (values char-alphabetic? locale-reader-abbr-month
                       (lambda (val object)
                         (tm:set-date-month! object val))))
        ((#\B) (values char-alphabetic? locale-reader-long-month
                       (lambda (val object)
                         (tm:set-date-month! object val))))
        ((#\d) (values char-numeric? ireader2 (lambda (val object)
                                                (tm:set-date-day!
                                                 object val))))
        ((#\e) (values char-fail eireader2 (lambda (val object)
                                             (tm:set-date-day! object val))))
        ((#\h) (values char-alphabetic? locale-reader-abbr-month
                       (lambda (val object)
                         (tm:set-date-month! object val))))
        ((#\H) (values char-numeric? ireader2 (lambda (val object)
                                                (tm:set-date-hour! object val))))
        ((#\k) (values char-fail eireader2 (lambda (val object)
                                             (tm:set-date-hour! object val))))
        ((#\m) (values char-numeric? ireader2 (lambda (val object)
                                                (tm:set-date-month! object val))))
        ((#\M) (values char-numeric? ireader2 (lambda (val object)
                                                (tm:set-date-minute!
                                                 object val))))
        ((#\N) (values char-numeric? fireader9 (lambda (val object)
                                                 (tm:set-date-nanosecond! object val))))
        ((#\S) (values char-numeric? ireader2 (lambda (val object)
                                                (tm:set-date-second! object val))))
        ((#\y) (values char-fail eireader2
                       (lambda (val object)
                         (tm:set-date-year! object (tm:natural-year val)))))
        ((#\Y) (values char-numeric? ireader4 (lambda (val object)
                                                (tm:set-date-year! object val))))
        ((#\z) (values (lambda (c)
                         (or (char=? c #\Z)
                             (char=? c #\z)
                             (char=? c #\+)
                             (char=? c #\-)))
                       tm:zone-reader (lambda (val object)
                                        (tm:set-date-zone-offset! object val))))
       (else
        (tm:time-error 'string->date 'bad-date-format-string template-string))))))

(define (tm:string->date date index format-string str-len port template-string)
  (define (skip-until port skipper)
    (let ((ch (peek-char port)))
      (if (eof-object? ch)
          (tm:time-error 'string->date 'bad-date-format-string template-string)
          (if (not (skipper ch))
              (begin (read-char port) (skip-until port skipper))))))
  (if (>= index str-len)
      (values)
      (let ((current-char (string-ref format-string index)))
        (if (not (char=? current-char #\~))
            (let ((port-char (read-char port)))
              (if (or (eof-object? port-char)
                      (not (char=? current-char port-char)))
                  (tm:time-error 'string->date 'bad-date-format-string template-string))
              (tm:string->date date (+ index 1) format-string str-len port template-string))
            ;; otherwise, it's an escape, we hope
            (if (> (+ index 1) str-len)
                (tm:time-error 'string->date 'bad-date-format-string template-string)
                (let ((format-char (string-ref format-string (+ index 1))))
                  (let-values ([(skipper reader actor)
                                (tm:read-directive format-char template-string)])
                    (skip-until port skipper)
                    (let ((val (reader port)))
                      (if (eof-object? val)
                          (tm:time-error 'string->date 'bad-date-format-string template-string)
                          (actor val date)))
                    (tm:string->date date (+ index 2) format-string
                                     str-len port template-string))))))))

(define (string->date input-string template-string)
  (define (tm:date-ok? date)
    (and (date-nanosecond date)
         (date-second date)
         (date-minute date)
         (date-hour date)
         (date-day date)
         (date-month date)
         (date-year date)
         (date-zone-offset date)))
  (let ((newdate (make-date 0 0 0 0 #f #f #f (tm:local-tz-offset))))
    (tm:string->date newdate
                     0
                     template-string
                     (string-length template-string)
                     (open-string-input-port input-string)
                     template-string)
    (if (tm:date-ok? newdate)
        newdate
        (tm:time-error 'string->date 'bad-date-format-string
                       (list "Incomplete date" newdate template-string))))))
