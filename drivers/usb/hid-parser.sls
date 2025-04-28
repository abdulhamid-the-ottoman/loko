;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020 G. Weinholt
#!r6rs

;;; USB HID report descriptor parser

;; The USB HID driver looks at the descriptors for the device. Under
;; device -> configuration -> interface -> HID descriptor, there is a
;; list of which report and physical descriptors the device has. The
;; driver needs to fetch the HID report descriptors and pass them to
;; parse-hid-report-descriptor.

;; Reference:
;; Device Class Definition for Human Interface Devices (HID) Version 1.11

;; TODO: Implement the errors and warnings described in this document:
;; HID Parser Error Codes, Version 1.0

(library (loko drivers usb hid-parser)
  (export
    parse-hid-report-descriptor
    parse-hid-unit
    print-hid-records

    make-hid-collection hid-collection?
    hid-collection-type
    hid-collection-usage
    hid-collection-collection*
    hid-collection-input*
    hid-collection-output*
    hid-collection-feature*
    hid-collection-report-id
    HID-COLLECTION-TYPE-PHYSICAL
    HID-COLLECTION-TYPE-APPLICATION
    HID-COLLECTION-TYPE-LOGICAL
    HID-COLLECTION-TYPE-REPORT
    HID-COLLECTION-TYPE-NAMED-ARRAY
    HID-COLLECTION-TYPE-USAGE-SWITCH
    HID-COLLECTION-TYPE-USAGE-MODIFIER

    make-hid-item hid-item?
    hid-item-flags
    hid-item-usage-page           ;for min and max, but not for usages
    hid-item-usage-minimum
    hid-item-usage-maximum
    hid-item-logical-minimum
    hid-item-logical-maximum
    hid-item-physical-minimum
    hid-item-physical-maximum
    hid-item-unit-exponent
    hid-item-unit
    hid-item-report-size
    hid-item-report-id
    hid-item-report-count
    hid-item-report-offset
    hid-item-usages
    hid-item-array?

    hid-input?
    hid-output?
    hid-feature?

    hid-parse-input-report)
  (import
    (rnrs)
    (loko system logging))

(define HID-COLLECTION-TYPE-PHYSICAL #x00)
(define HID-COLLECTION-TYPE-APPLICATION #x01)
(define HID-COLLECTION-TYPE-LOGICAL #x02)
(define HID-COLLECTION-TYPE-REPORT #x03)
(define HID-COLLECTION-TYPE-NAMED-ARRAY #x04)
(define HID-COLLECTION-TYPE-USAGE-SWITCH #x05)
(define HID-COLLECTION-TYPE-USAGE-MODIFIER #x06)

(define-record-type hid-collection
  (sealed #t)
  (fields type
          usage
          collection*                   ;more collections
          input*                        ;data from the device
          output*                       ;data to the device
          feature*))                    ;config to the device

;; Find the report ID associated with a top-level collection.
(define (hid-collection-report-id x)
  (or (exists hid-item-report-id (hid-collection-input* x))
      (exists hid-item-report-id (hid-collection-output* x))
      (exists hid-item-report-id (hid-collection-feature* x))
      (exists hid-collection-report-id (hid-collection-collection* x))))

;; An item is a bitfield in a report (which is a bytevector that comes
;; from a device endpoint, or goes to an endpoint if it's an output).
(define-record-type hid-item
  (fields flags
          ;; A range that compactly says what this item is used for.
          usage-page
          usage-minimum usage-maximum
          ;; Value range
          logical-minimum logical-maximum
          physical-minimum physical-maximum
          unit-exponent unit
          ;; Size in bits, report ID, number of reports
          report-size report-id report-count
          ;; Offset in bits
          report-offset
          ;; A list of (usage-page . usage-id)
          usages))

(define-record-type hid-input
  (sealed #t)
  (parent hid-item))

(define (hid-item-array? x)
  (not (bitwise-bit-set? (hid-item-flags x) 1)))

(define-record-type hid-output
  (sealed #t)
  (parent hid-item))

(define-record-type hid-feature
  (sealed #t)
  (parent hid-item))

(define-record-type hid-item/reserved
  (sealed #t)
  (parent hid-item)
  (fields tag))

;; Parse a HID unit.
;; Example: (parse-unit #xf0d121)
;;            =>  (SI-Linear (^ cm 2) (^ g 1) (^ s -3) (^ A -1))
;; The above is equal to volts scaled by 1e-07, so the unit exponent
;; is given as 7 if the values are sent as volts.
(define (parse-hid-unit v)
  (define table
    '#(#(#f SI-Linear SI-Rotation English-Linear English-Rotation
            #f #f #f #f #f #f #f #f #f #f 'vendor)
       #(#f cm         radian      inch              deg)
       #(#f g          g           slug              slug)
       #(#f s          s           s                 s)
       #(#f K          K           °F                °F)
       #(#f A          A           A                 A)
       #(#f cd         cd          cd                cd)
       #(#f #f         #f          #f                #f)))
  (let* ((system (fxbit-field v 0 4))
         (system-name (or (vector-ref (vector-ref table 0) system)
                          `(system ,system))))
    (let lp ((i 4) (n 1) (units '()))
      (if (eqv? n 8)
          (cons system-name (reverse units))
          (let* ((nibble (fxbit-field v i (fx+ i 4)))
                 (row (vector-ref table n))
                 (unit (if (fx<? system (vector-length row))
                           (vector-ref row system)
                           `(reserved ,nibble ,system)))
                 (exponent (if (fx<=? 0 nibble 7)
                               nibble
                               (fx- (fx- 16 nibble)))))
            (lp (fx+ i 4)
                (fx+ n 1)
                (if (or (not unit) (eqv? exponent 0))
                    units
                    (cons `(^ ,unit ,exponent) units))))))))

(define (bytevector->sint bv)
  (case (bytevector-length bv)
    ((0) 0)
    ((1) (bytevector-s8-ref bv 0))
    ((2) (bytevector-s16-ref bv 0 (endianness little)))
    (else (bytevector-s32-ref bv 0 (endianness little)))))

(define (bytevector->uint bv)
  (case (bytevector-length bv)
    ((0) 0)
    ((1) (bytevector-u8-ref bv 0))
    ((2) (bytevector-u16-ref bv 0 (endianness little)))
    (else (bytevector-u32-ref bv 0 (endianness little)))))

(define (assv-set key value alist)
  (cons (cons key value)
        (remp (lambda (x) (eq? (car x) key)) alist)))

(define (assq-ref alist key)
  (cond ((assq key alist) => cdr)
        (else #f)))

;; Get a report descriptor item
(define (get-item p)
  (define (err) (values (eof-object) #f #f))
  (let ((b (get-u8 p)))
    (cond
      ((eof-object? b) (err))
      ((eqv? b #xFE)                    ;long tag
       (let ((len (get-u8 p)))
         (if (eof-object? len)
             (err)
             (let ((tag (get-u8 p)))
               (if (eof-object? tag)
                   (err)
                   (let ((data (get-bytevector-n p len)))
                     (if (or (eof-object? data) (not (fx=? (bytevector-length data) len)))
                         (err)
                         (values 'reserved tag (get-bytevector-n p)))))))))
      (else                             ;short tag
       (let* ((size (fxbit-field b 0 2))
              (size (if (eqv? size 3) 4 size))
              (type (vector-ref '#(main global local reserved) (fxbit-field b 2 4)))
              (tag (fxbit-field b 4 8)))
         (let ((data (get-bytevector-n p size)))
           (if (or (eof-object? data) (not (fx=? (bytevector-length data) size)))
               (err)
               (values type tag data))))))))

(define (parse-global-data-item tag value
                                logical-minimum
                                physical-minimum)
  (case tag
    ((#b0000) (values 'usage-page        (bytevector->uint value)))
    ((#b0001) (values 'logical-minimum   (bytevector->sint value)))
    ((#b0010) (values 'logical-maximum
                      (if (negative? logical-minimum)
                          (bytevector->sint value)
                          (bytevector->uint value))))
    ((#b0011) (values 'physical-minimum  (bytevector->sint value)))
    ((#b0100) (values 'physical-maximum
                      (if (negative? physical-minimum)
                          (bytevector->sint value)
                          (bytevector->uint value))))
    ((#b0101) (values 'unit-exponent     (bytevector->sint value)))
    ((#b0110) (values 'unit              (bytevector->uint value)))
    ((#b0111) (values 'report-size       (bytevector->uint value)))
    ((#b1000) (values 'report-id         (bytevector->uint value)))
    ((#b1001) (values 'report-count      (bytevector->uint value)))
    (else     (values (list 'global tag) value))))

(define (parse-local-data-item tag value)
  (case tag
    ((#b0000)
     (let ((v (bytevector->uint value)))
       (if (= (bytevector-length value) 4)
           (values 'usage (cons (bitwise-bit-field v 16 32)
                                (bitwise-bit-field v 0 16)))
           (values 'usage v))))
    ((#b0001) (values 'usage-minimum      (bytevector->uint value)))
    ((#b0010) (values 'usage-maximum      (bytevector->uint value)))
    ((#b0011) (values 'designator-index   (bytevector->uint value)))
    ((#b0100) (values 'designator-minimum (bytevector->uint value)))
    ((#b0101) (values 'designator-maximum (bytevector->uint value)))
    ((#b0111) (values 'string-index       (bytevector->uint value)))
    ((#b1000) (values 'string-minimum     (bytevector->uint value)))
    ((#b1001) (values 'string-maximum     (bytevector->uint value)))
    ;; TODO: Handle delimiters
    ((#b1010) (values 'delimiter          (bytevector->uint value)))
    (else     (values (list 'local tag) value))))

(define (parse-main-item tag value data offset-ht)
  (let ((usage-page (assq-ref data 'usage-page)))
    (let ((flags (bytevector->uint value))
          (usages (map (lambda (usage)
                         (if (pair? (cdr usage))
                             (cdr usage)
                             (cons usage-page (cdr usage))))
                       (filter (lambda (x) (eq? (car x) 'usage)) data)))
          (report-size (assq-ref data 'report-size))
          (report-id (assq-ref data 'report-id))
          (report-count (assq-ref data 'report-count))
          (usage-minimum (assq-ref data 'usage-minimum))
          (usage-maximum (assq-ref data 'usage-maximum))
          (logical-minimum (assq-ref data 'logical-minimum))
          (logical-maximum (assq-ref data 'logical-maximum))
          (physical-minimum (assq-ref data 'physical-minimum))
          (physical-maximum (assq-ref data 'physical-maximum))
          (unit-exponent (assq-ref data 'unit-exponent))
          (unit (assq-ref data 'unit)))
      (let ((constructor
             (case tag
               ((#b1000) make-hid-input)
               ((#b1001) make-hid-output)
               ((#b1011) make-hid-feature)
               (else (lambda x (apply make-hid-item/reserved (append x (list tag))))))))
        (let* ((key (fxior (fxarithmetic-shift-left (or report-id 0) 4) tag))
               (total-size (fx* report-size report-count))
               ;; Increment the report offset of this ID/Type combination
               (report-offset
                (cond ((hashtable-ref offset-ht key #f) =>
                       (lambda (offset)
                         (hashtable-set! offset-ht key (fx+ offset total-size))
                         offset))
                      (report-id
                       (hashtable-set! offset-ht key (fx+ total-size 8))
                       8)
                      (else
                       (hashtable-set! offset-ht key total-size)
                       0))))
          (constructor flags
                       usage-page usage-minimum usage-maximum
                       logical-minimum logical-maximum
                       physical-minimum physical-maximum
                       unit-exponent unit
                       report-size report-id report-count
                       report-offset
                       usages))))))

(define (parse-collection value data items)
  (let ((usage-page (assq-ref data 'usage-page))
        (usage (assq-ref data 'usage)))
    (make-hid-collection (bytevector->uint value)
                         (if (pair? usage)
                             usage
                             (cons usage-page usage))
                         (filter hid-collection? items)
                         (filter hid-input? items)
                         (filter hid-output? items)
                         (filter hid-feature? items))))

;; FIXME: use this to verify the report descriptor
(define (hid-report-descriptor-uses-report-ids? bv)
  (define p (open-bytevector-input-port bv))
  (let lp ()
    (let-values ([(type tag _v) (get-item p)])
      (if (eof-object? type)
          #f
          (case type
            ((global)
             (if (eqv? tag #b1000)
                 #t                     ;report id
                 (lp)))
            (else (lp)))))))

;; Parse a HID report descriptor and return a list of hid-item and
;; hid-collection records.
(define (parse-hid-report-descriptor bv)
  (define p (open-bytevector-input-port bv))
  (define offset-ht (make-eqv-hashtable))
  (let lp ((global '()) (local '()) (stack '()) (report-data '()) (parents '()))
    ;; (write (list 'global: global 'local: local 'stack: stack)) (newline)
    (let-values ([(type tag v) (get-item p)])
      (cond
        ((eof-object? type)
         (unless (null? parents)
           (send-log WARNING "A0 0: Missing 'End Collection' in USB HID report descriptor"))
         (unless (null? stack)
           (send-log WARNING "A4 0: Missing 'Pop' in USB HID report descriptor"))
         report-data)
        (else
         ;; (write (list type tag v))
         ;; (newline)
         (case type
           ((main)                     ;report structure
            (case tag
              ((#b1010)                ;collection
               (lp global '() stack '() (cons (cons* v report-data (append global local)) parents)))
              ((#b1100)                ;end collection
               (cond ((null? parents)
                      (send-log WARNING "C0 0: Unexpected 'End Collection' in USB HID report descriptor")
                      (lp global local stack '() '()))
                     (else
                      (let ((value (caar parents))
                            (parent-report-data (cadar parents))
                            (data (cddar parents)))
                        (lp global local stack
                            (cons (parse-collection value data (reverse report-data))
                                  parent-report-data)
                            (cdr parents))))))
              (else
               (let ((item (parse-main-item tag v (append (reverse local) global) offset-ht)))
                 (lp global '() stack (cons item report-data) parents)))))
           ((global)                   ;affects all following main items
            (case tag
              ((#b1010)                ;push
               (lp global local (cons global stack) report-data parents))
              ((#b1011)                ;pop
               (cond ((null? parents)
                      (send-log WARNING "B4 0: Unexpected 'Pop' in USB HID report descriptor")
                      (lp global local stack report-data parents))
                     (else
                      (lp (car stack) local (cdr stack) report-data parents))))
              (else
               (let-values ([(key value)
                             (parse-global-data-item tag v
                                                     (assq-ref global 'logical-minimum)
                                                     (assq-ref global 'physical-minimum))])
                 (lp (assv-set key value global) local stack report-data parents)))))
           ;; State that affects the next main item
           ((local)                    ;affects the next main item
            ;; XXX: Usage page has the upper 16 bits for usage,
            ;; usage-minimum & usage-maximum
            (let-values ([(key value) (parse-local-data-item tag v)])
              (lp global (cons (cons key value) local) stack report-data parents)))
           (else
            ;; XXX: Reserved, should stop parsing here?
            (lp global local stack report-data parents))))))))

(define print-hid-records
  (case-lambda
    ((x* p)
     (define (print-hid x level)
       (define flag-names
         '#((Data . Constant)
            (Array . Variable)
            (Absolute . Relative)
            (No-Wrap . Wrap)
            (Linear . Nonlinear)
            (Preferred-State . No-Preferred)
            (No-Null-Position . Null-State)
            (Non-volatile . Volatile)
            (Bit-Field . Buffered-Bytes)))
       (define collection-types
         '#(PHYSICAL
            APPLICATION
            LOGICAL
            REPORT
            NAMED-ARRAY
            USAGE-SWITCH
            USAGE-MODIFIER))
       (define (indent p)
         (display (make-string (* level 1) #\space) p))
       (define (print-item x p)
         (display " Flags:" p)
         (do ((i 0 (fx+ i 1)))
             ((fx=? i (vector-length flag-names)))
           (unless (and (hid-input? x)
                        (or (hid-item-array? x)
                            (fx>? i 2))
                        (eqv? i 7))
             (let ((unset/set (vector-ref flag-names i)))
               (display " " p)
               (display (if (bitwise-bit-set? (hid-item-flags x) i)
                            (cdr unset/set)
                            (car unset/set))
                        p))))
         (newline p)
         (indent p)
         (display " " p)
         (when (hid-item-report-id x)
           (display " ID=" p)
           (display (hid-item-report-id x) p))
         (display " Count×Size=" p)
         (display (hid-item-report-count x) p)
         (display "×" p)
         (display (hid-item-report-size x) p)
         (display " Offset=" p)
         (display (hid-item-report-offset x) p)
         (when (and (hid-item-logical-minimum x) (hid-item-logical-maximum x))
           (display " Logical=[" p)
           (display (hid-item-logical-minimum x) p)
           (display "," p)
           (display (hid-item-logical-maximum x) p)
           (display "]" p))
         (when (and (hid-item-physical-minimum x) (hid-item-physical-maximum x))
           (display " Physical=[" p)
           (display (hid-item-physical-minimum x) p)
           (display "," p)
           (display (hid-item-physical-maximum x) p)
           (display "]" p))
         (when (and (hid-item-unit x) (not (eqv? 0 (hid-item-unit x))))
           (display " Unit=" p)
           (write (parse-hid-unit (hid-item-unit x)) p)
           (when (hid-item-unit-exponent x)
             (display " * 10^" p)
             (display (hid-item-unit-exponent x) p)))
         (when (and (hid-item-usage-minimum x) (hid-item-usage-maximum x))
           (newline p)
           (indent p)
           (display "  Usage page " p)
           (display (hid-item-usage-page x) p)
           (display " range=[" p)
           (display (hid-item-usage-minimum x) p)
           (display "," p)
           (display (hid-item-usage-maximum x) p)
           (display "]" p))
         (unless (null? (hid-item-usages x))
           (newline p)
           (indent p)
           (display "  Usages: " p)
           (write (hid-item-usages x) p)))
       (indent p)
       (cond ((hid-collection? x)
              (display "• Collection of type "  p)
              (cond ((fx<? (hid-collection-type x) (vector-length collection-types))
                     (display (vector-ref collection-types (hid-collection-type x)) p))
                    (else
                     (display (hid-collection-type x) p)))
              (display " and usage " p)
              (display (hid-collection-usage x) p)
              (newline p)
              (indent p) (display "  Collections:\n" p)
              (for-each (lambda (x) (print-hid x (+ level 2))) (hid-collection-collection* x))
              (indent p) (display "  Inputs:\n" p)
              (for-each (lambda (x) (print-hid x (+ level 2))) (hid-collection-input* x))
              (indent p) (display "  Outputs:\n" p)
              (for-each (lambda (x) (print-hid x (+ level 2))) (hid-collection-output* x))
              (indent p) (display "  Features:\n" p)
              (for-each (lambda (x) (print-hid x (+ level 2))) (hid-collection-feature* x)))
             ((hid-input? x)
              (display "•" p)
              (print-item x p)
              (newline p))
             ((hid-output? x)
              (display "•" p)
              (print-item x p)
              (newline p))
             ((hid-feature? x)
              (display "•" p)
              (print-item x p)
              (newline p))
             ((hid-item? x)
              (display "• Item: " p)
              (write x p)
              (newline p))
             (else
              (display "• Unrecognized: " p)
              (write x p)
              (newline p))))
     (for-each (lambda (x) (print-hid x 0)) x*))
    ((x*)
     (print-hid-records x* (current-output-port)))))

;;; For parsing the reports

(define (hid-item-usage-ref item i)
  (cond ((and (hid-item-usage-minimum item)
              (hid-item-usage-maximum item)
              (fx<? i (fx- (fx+ 1 (hid-item-usage-maximum item))
                           (hid-item-usage-minimum item))))
         (cons (hid-item-usage-page item)
               (fx+ (hid-item-usage-minimum item) i)))
        ((hid-item-usages item) =>
         (lambda (usages)
           (and (fx<? i (length usages))
                (list-ref usages i))))
        (else #f)))

;; Extract the bits corresponding to an item from the given
;; bytevector, which is a HID report from an endpoint.
(define (hid-report-extract-item-value bv item idx)
  (define (unsigned->signed n width)
    (if (fxbit-set? n (fx- width 1))
        (fx- n (fxarithmetic-shift-left 1 width))
        n))
  ;; Items can at most reference 32 bits of data.
  (define report-size (hid-item-report-size item))
  (let-values ([(byte-idx bit-idx)
                (fxdiv-and-mod (fx+ (hid-item-report-offset item)
                                    (fx* idx report-size))
                               8)])
    (let* ((x (cond ((eqv? report-size 0)
                     0)
                    ((fx<=? report-size 8)
                     (bytevector-u8-ref bv byte-idx))
                    ((fx<=? report-size 16)
                     (bytevector-u16-ref bv byte-idx (endianness little)))
                    ;; XXX: untested
                    ((fx<=? report-size 24)
                     (fxior (bytevector-u16-ref bv byte-idx (endianness little))
                            (fxarithmetic-shift-left (bytevector-u8-ref bv (fx+ byte-idx 2)) 16)))
                    (else
                     (bytevector-u32-ref bv byte-idx (endianness little)))))
           (v (bitwise-bit-field x bit-idx (fx+ bit-idx report-size)))
           (v (if (negative? (hid-item-logical-minimum item))
                  (unsigned->signed v report-size)
                  v)))
      v)))

;; Parse the HID report bv using the parsed report descriptor desc.
;; The desc can be a hid-collection or hid-item. The callbacks are:
;; (array-cb usage-page usage-id array-index)
;; (variable-cb usage-page usage-id value)
(define (hid-parse-input-report desc bv array-cb variable-cb)
  (cond
    ((hid-collection? desc)
     ;; Parse the collection recursively. Most HID drivers should
     ;; probably restrict themselves to parts of the reports.
     (for-each (lambda (x) (hid-parse-input-report x bv array-cb variable-cb))
               (append (hid-collection-collection* desc)
                       (hid-collection-input* desc))))

    ;; No usages?
    ((and (null? (hid-item-usages desc))
          (not (hid-item-usage-minimum desc))))

    ;; Wrong report id for this item?
    ((and (hid-item-report-id desc)
          (or (fx=? (bytevector-length bv) 0)
              (not (fx=? (hid-item-report-id desc)
                         (bytevector-u8-ref bv 0))))))

    ((hid-item-array? desc)             ;array of usages
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (hid-item-report-count desc)))
       (let ((v (hid-report-extract-item-value bv desc i)))
         (cond ((hid-item-usage-ref desc v) =>
                (lambda (usage)
                  (array-cb desc (car usage) (cdr usage) i)))))))

    (else                               ;variable
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (hid-item-report-count desc)))
       (cond ((hid-item-usage-ref desc i) =>
              (lambda (usage)
                (variable-cb desc (car usage) (cdr usage)
                             (hid-report-extract-item-value bv desc i))))))))))
