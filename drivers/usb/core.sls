;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; USB abstraction layer

(library (loko drivers usb core)
  (export
    ;; USB controllers
    make-usb-controller
    usb-controller-request-channel
    usb-controller-notify-channel
    usb-controller-root-hub

    ;; USB hubs
    make-usb-hub
    usb-hub-request-channel
    usb-hub-notify-channel
    usb-hub-descriptor
    usb-hub-descriptor-set!

    ;; USB devices
    make-usb-device
    usb-device-controller
    usb-device-port
    usb-device-address
    usb-device-max-packet-size-0
    usb-device-speed
    usb-get-device-descriptor
    usb-get-configuration-descriptors

    usb-device-$descriptor
    usb-device-$configurations
    usb-device-$strings

    ;; bmRequestType constants
    devreq-GET_STATUS
    devreq-CLEAR_FEATURE
    devreq-SET_FEATURE
    devreq-SET_ADDRESS
    devreq-GET_DESCRIPTOR
    devreq-SET_DESCRIPTOR
    devreq-GET_CONFIGURATION
    devreq-SET_CONFIGURATION
    devreq-GET_INTERFACE
    devreq-SET_INTERFACE
    devreq-SYNCH_FRAME

    ;; bDescriptorType constants
    desctype-DEVICE
    desctype-CONFIGURATION
    desctype-STRING
    desctype-INTERFACE
    desctype-ENDPOINT

    ;; USB device requests
    devreq-bmRequestType
    devreq-bRequest
    devreq-wValue
    devreq-wIndex
    devreq-wLength

    request-type:device->host?
    request-type:host->device?
    endpoint:device->host?
    endpoint:host->device?

    ;; Common to all descriptors
    desc-bLength
    desc-bDescriptorType

    ;; Device descriptor
    devdesc-bcdUSB
    devdesc-bDeviceClass
    devdesc-bDeviceSubClass
    devdesc-bDeviceProtocol
    devdesc-bMaxPacketSize0
    devdesc-idVendor
    devdesc-idProduct
    devdesc-bcdDevice
    devdesc-iManufacturer
    devdesc-iProduct
    devdesc-iSerialNumber
    devdesc-bNumConfigurations
    devdesc-standard-length

    ;; Configuration descriptor
    cfgdesc?
    cfgdesc-wTotalLength
    cfgdesc-bNumInterfaces
    cfgdesc-bConfigurationValue
    cfgdesc-iConfiguration
    cfgdesc-bmAttributes
    cfgdesc-MaxPower
    cfgdesc-standard-length

    ;; Interface descriptor
    intdesc?
    intdesc-bInterfaceNumber
    intdesc-bAlternateSetting
    intdesc-bNumEndpoints
    intdesc-bInterfaceClass
    intdesc-bInterfaceSubClass
    intdesc-bInterfaceProtocol
    intdesc-iInterface

    ;; Endpoint descriptor
    epdesc-bEndpointAddress
    epdesc-bmAttributes
    epdesc-wMaxPacketSize
    epdesc-bInterval
    epdesc-transfer-type

    print-usb-descriptor
    string-language-descriptor->list
    string-language-descriptor->string
    split-configuration-descriptor

    ;; Perform USB transfers (synchronous calls)
    usb-control-transfer
    usb-bulk-write
    usb-bulk-read
    ;; usb-interrupt-read usb-interrupt-write

    ;; Change a device's configuration (synchronous call)
    usb-set-configuration
    ;; Change a device's address (synchronous call)
    usb-set-address

    ;; Pipes give asynchronous access to USB endpoints.
    usb-setup-default-control-pipe
    usb-configure-device
    usb-setup-pipes
    usb-device-default-control-pipe
    usb-device-interfaces
    usb-interface?
    usb-interface-descriptor
    usb-interface-descriptor*
    usb-interface-pipes
    make-usb-pipe
    usb-pipe?
    usb-pipe-descriptor
    usb-pipe-out-operation         ;OUT interrupt/isochronous endpoints
    usb-pipe-in-operation          ;IN interrupt/isochronous endpoints
    usb-pipe-in?
    usb-pipe-out?

    ;; Fetch and cache the device's string and configuration descriptors
    usb-fetch-descriptors
    usb-fetch-device-descriptor)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system logging)
    (loko system fibers))

(define DEFAULT-TIMEOUT 100)

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'usb-core))

(define (log/error . x*)
  (log/x ERROR x*))

(define (log/warn . x*)
  (log/x WARNING x*))

(define (log/debug . x*)
  (log/x DEBUG x*))

(define-syntax define-vu8-ref
  (lambda (x)
    (syntax-case x ()
      ((_ name index)
       #'(define-vu8-ref name index 1))
      ((_ name index size)
       (eqv? (syntax->datum #'size) 1)
       #'(define (name desc) (bytevector-u8-ref desc index)))
      ((_ name index size)
       (eqv? (syntax->datum #'size) 2)
       #'(define (name desc) (bytevector-u16-ref desc index (endianness little)))))))

(define devreq-GET_STATUS         0)
(define devreq-CLEAR_FEATURE      1)
(define devreq-SET_FEATURE        3)
(define devreq-SET_ADDRESS        5)
(define devreq-GET_DESCRIPTOR     6)
(define devreq-SET_DESCRIPTOR     7)
(define devreq-GET_CONFIGURATION  8)
(define devreq-SET_CONFIGURATION  9)
(define devreq-GET_INTERFACE     10)
(define devreq-SET_INTERFACE     11)
(define devreq-SYNCH_FRAME       12)

(define desctype-DEVICE        1)
(define desctype-CONFIGURATION 2)
(define desctype-STRING        3)
(define desctype-INTERFACE     4)
(define desctype-ENDPOINT      5)

(define-vu8-ref devreq-bmRequestType 0)
(define-vu8-ref devreq-bRequest      1)
(define-vu8-ref devreq-wValue        2 2)
(define-vu8-ref devreq-wIndex        4 2)
(define-vu8-ref devreq-wLength       6 2)

(define (request-type:device->host? x) (fxbit-set? x 7))

(define (request-type:host->device? x) (not (fxbit-set? x 7)))

(define (endpoint:device->host? x) (fxbit-set? x 7))

(define (endpoint:host->device? x) (not (fxbit-set? x 7)))

;; Descriptor fields
(define-vu8-ref desc-bLength                  0)
(define-vu8-ref desc-bDescriptorType          1)

;; Device descriptor fields
(define-vu8-ref devdesc-bcdUSB                2 2)
(define-vu8-ref devdesc-bDeviceClass          4)
(define-vu8-ref devdesc-bDeviceSubClass       5)
(define-vu8-ref devdesc-bDeviceProtocol       6)
(define-vu8-ref devdesc-bMaxPacketSize0       7)
(define-vu8-ref devdesc-idVendor              8 2)
(define-vu8-ref devdesc-idProduct            10 2)
(define-vu8-ref devdesc-bcdDevice            12 2)
(define-vu8-ref devdesc-iManufacturer        14)
(define-vu8-ref devdesc-iProduct             15)
(define-vu8-ref devdesc-iSerialNumber        16)
(define-vu8-ref devdesc-bNumConfigurations   17)
(define devdesc-standard-length 18)

;; Configuration descriptor fields
(define-vu8-ref cfgdesc-wTotalLength         2 2)
(define-vu8-ref cfgdesc-bNumInterfaces       4)
(define-vu8-ref cfgdesc-bConfigurationValue  5)
(define-vu8-ref cfgdesc-iConfiguration       6) ;barely used
(define-vu8-ref cfgdesc-bmAttributes         7)
(define-vu8-ref cfgdesc-MaxPower             8)
(define cfgdesc-standard-length 9)

(define (cfgdesc? desc)
  (eqv? (desc-bDescriptorType desc) desctype-CONFIGURATION))

;; Interface descriptor fields
(define-vu8-ref intdesc-bInterfaceNumber   2)
(define-vu8-ref intdesc-bAlternateSetting  3)
(define-vu8-ref intdesc-bNumEndpoints      4)
(define-vu8-ref intdesc-bInterfaceClass    5)
(define-vu8-ref intdesc-bInterfaceSubClass 6)
(define-vu8-ref intdesc-bInterfaceProtocol 7)
(define-vu8-ref intdesc-iInterface         8)

(define (intdesc? desc)
  (eqv? (desc-bDescriptorType desc) desctype-INTERFACE))

;; Endpoint descriptor fields
(define-vu8-ref epdesc-bEndpointAddress 2)
(define-vu8-ref epdesc-bmAttributes     3)
(define-vu8-ref epdesc-wMaxPacketSize   4 2)
(define-vu8-ref epdesc-bInterval        6)

(define (epdesc? desc)
  (eqv? (desc-bDescriptorType desc) desctype-ENDPOINT))

(define (epdesc-transfer-type desc)
  (vector-ref '#(control isochronous bulk interrupt)
              (fxbit-field (epdesc-bmAttributes desc) 0 2)))

(define print-usb-descriptor
  (case-lambda
    ((desc p)
     (define (fmt x) (number->string x 16))
     (define (print . x*) (for-each (lambda (x) (display x p)) x*) (newline p))
     (let ((type (desc-bDescriptorType desc)))
       (cond
         ((eqv? type desctype-DEVICE)
          (print "• Device descriptor of length " (desc-bLength desc)
                 ", USB version " (fmt (devdesc-bcdUSB desc))
                 ", Max packet size: " (devdesc-bMaxPacketSize0 desc))
          (when (fx>=? (bytevector-length desc) devdesc-standard-length)
            (print "  Class: #x" (fmt (devdesc-bDeviceClass desc))
                   ", SubClass: #x" (fmt (devdesc-bDeviceSubClass desc))
                   ", Protocol: #x" (fmt (devdesc-bDeviceProtocol desc)))
            (print "  Vendor: #x" (fmt (devdesc-idVendor desc))
                   ", Product: #x" (fmt (devdesc-idProduct desc)))
            (print "  Number of configurations: " (devdesc-bNumConfigurations desc))))
         ((eqv? type desctype-STRING)
          (print "  • String descriptor of length " (desc-bLength desc)))
         ((eqv? type desctype-CONFIGURATION)
          (print "  • Configuration descriptor of length " (desc-bLength desc))
          (print "    Number of interfaces: " (cfgdesc-bNumInterfaces desc))
          (print "    Attributes: #b" (number->string (cfgdesc-bmAttributes desc) 2))
          (print "    Max power: " (fx* 2 (cfgdesc-MaxPower desc)) " mA"))
         ((eqv? type desctype-INTERFACE)
          (print "    • Interface descriptor of length " (desc-bLength desc))
          (print "      Interface number " (intdesc-bInterfaceNumber desc))
          (print "      Class: #x" (fmt (intdesc-bInterfaceClass desc))
                 ", SubClass: #x" (fmt (intdesc-bInterfaceSubClass desc))
                 ", Protocol: #x" (fmt (intdesc-bInterfaceProtocol desc)))
          (print "      Number of endpoints: " (intdesc-bNumEndpoints desc)))
         ((eqv? type desctype-ENDPOINT)
          (print "      • Endpoint descriptor of length " (desc-bLength desc))
          (let ((addr (epdesc-bEndpointAddress desc)))
            (print "        Address: #x" (fmt addr) "  EP " (fxbit-field addr 0 4) " "
                   (if (endpoint:device->host? addr) "IN" "OUT")))
          (print "        Attributes: #b" (number->string (epdesc-bmAttributes desc) 2) " "
                 (epdesc-transfer-type desc))
          (print "        Max packet size: " (epdesc-wMaxPacketSize desc))
          (print "        Interval: " (epdesc-bInterval desc)))
         (else
          (print "• Descriptor type: #x" (fmt type) ": " desc)))))
    ((desc)
     (print-usb-descriptor desc (current-output-port)))))

(define (string-language-descriptor->list desc)
  (assert (eqv? (desc-bDescriptorType desc) desctype-STRING))
  (do ((len (fxdiv (fx- (fxmin (bytevector-length desc) (desc-bLength desc)) 2) 2))
       (i 0 (fx+ i 1))
       (lang* '() (cons (bytevector-u16-ref desc (fx+ i 2) (endianness little)) lang*)))
      ((fx=? i len) (reverse lang*))))

(define (string-language-descriptor->string desc)
  (assert (eqv? (desc-bDescriptorType desc) desctype-STRING))
  (utf16->string (call-with-bytevector-output-port
                   (lambda (p)
                     (put-bytevector p desc 2 (fx- (fxmin (bytevector-length desc)
                                                          (desc-bLength desc))
                                                   2))))
                 (endianness little)))

;; Takes a list of descriptors for a single configuration and groups
;; them by the interface descriptors.
;; Something like: (C I H E E I H E) => ((I H E E) (I H E))
(define (group-by-interface-descriptor desc*)
  (if (null? desc*)
      '()
      (let ((intdesc (car desc*)))
        (if (not (eqv? (desc-bDescriptorType intdesc) desctype-INTERFACE))
            (group-by-interface-descriptor (cdr desc*)) ;ignore
            (let lp ((desc* (cdr desc*))
                     (keep* (list intdesc)))
              (cond ((null? desc*)
                     (list (reverse keep*)))
                    ((eqv? (desc-bDescriptorType (car desc*)) desctype-INTERFACE)
                     (cons (reverse keep*) (group-by-interface-descriptor desc*)))
                    (else
                     (lp (cdr desc*) (cons (car desc*) keep*)))))))))

;; Split a configuration descriptor
(define (split-configuration-descriptor desc)
  (assert (eqv? (desc-bDescriptorType desc) desctype-CONFIGURATION))
  (let ((p (open-bytevector-input-port desc)))
    (let lp ()
      (if (port-eof? p)
          '()
          (let ((cfgdesc (get-bytevector-n p (lookahead-u8 p))))
            (cons cfgdesc (lp)))))))

;; Each USB controller (HCI) gets one of these
(define-record-type usb-controller
  (sealed #t)
  (fields
   ;; Requests to the controller.
   request-channel
   ;; Notifications from the controller.
   notify-channel
   ;; The "root hub", which is a fake hub that handles the ports
   ;; directly on the controller.
   root-hub)
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel) (make-channel)
          (make-usb-hub))))))

;; Each device on the USB gets one of these
(define-record-type usb-device
  (sealed #t)
  (fields controller hub
          port
          address
          max-packet-size-0          ;max packet size for endpoint 0
          speed                      ;low, full, high, super, super+
          (mutable $descriptor)
          ;; TODO: it would be better to have records for these
          ;; ((cfgdesc intdesc0 desc00 ... epdesc00 ... . _) ...)
          (mutable $configurations)
          ;; ((lang-id manufacturer product serial-no) ...)
          (mutable $strings)
          (mutable $interfaces)
          (mutable $default-control-pipe))
  (protocol
   (lambda (p)
     (lambda (controller hub port address max-packet-size-0 speed desc)
       (p controller hub port address max-packet-size-0 speed desc #f #f #f #f)))))

;; Gets the USB device descriptor without generating bus traffic.
(define usb-get-device-descriptor usb-device-$descriptor)

;; Same as above for the configuration descriptors. The
;; usb-fetch-descriptors procedure must be called first.
(define usb-get-configuration-descriptors usb-device-$configurations)

(define (usb-device-default-control-pipe dev)
  (or (usb-device-$default-control-pipe dev)
      (assertion-violation 'usb-device-default-control-pipe
                           "Must call usb-setup-default-control-pipe first" dev)))

(define (usb-device-interfaces dev)
  (or (usb-device-$interfaces dev)
      (assertion-violation 'usb-device-interfaces
                           "Must call usb-fetch-descriptors first" dev)))

;; Each USB hub gets one of these and the controller also gets one
(define-record-type usb-hub
  (sealed #t)
  (fields
   ;; Control requests to the hub
   request-channel
   ;; Status change notifications from the hub
   notify-channel
   ;; Hub descriptor
   (mutable descriptor))
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel) (make-channel) #f)))))

;; An interface is a collection of pipes
(define-record-type usb-interface
  (sealed #t)
  (fields descriptor                    ;interface descriptor
          descriptor*                   ;list of other descriptors
          pipes))

;; Each endpoint on a device gets one of these. They are used to
;; communicate with the endpoints on an interface.
(define-record-type usb-pipe
  (sealed #t)
  (fields descriptor                    ;endpoint descriptor
          (mutable $ch))
  (protocol
   (lambda (p)
     (case-lambda
       ((ep-desc)
        (p ep-desc #f))
       ((ep-desc ch)
        (p ep-desc ch))))))

(define (usb-pipe-in? pipe)
  (let ((epdesc (usb-pipe-descriptor pipe)))
    (and (endpoint:device->host? (epdesc-bEndpointAddress epdesc))
         (not (eqv? 0 (fxbit-field (epdesc-bmAttributes epdesc) 0 2))))))

(define (usb-pipe-out? pipe)
  (let ((epdesc (usb-pipe-descriptor pipe)))
    (and (endpoint:host->device? (epdesc-bEndpointAddress epdesc))
         (not (eqv? 0 (fxbit-field (epdesc-bmAttributes epdesc) 0 2))))))

(define (usb-pipe-ch pipe)
  (or (usb-pipe-$ch pipe)
      (assertion-violation 'usb-pipe-ch
                           "Must call usb-setup-pipes first" pipe)))

;;; Requests

(define EndPt0 0)

(define (perform-devreq/response dev endpoint devreq timeout)
  (let ((ch (make-channel)))
    (put-message (usb-controller-request-channel (usb-device-controller dev))
                 (list 'perform-devreq/response ch dev endpoint devreq timeout))
    (let ((resp (get-message ch)))
      (values (car resp) (cdr resp)))))

;; Synchronous procedure to perform a control transfer on the device.
;; The request-type indicates the direction and whether the *data*
;; bytevector is read or written. Timeout is in milliseconds.
(define (usb-control-transfer dev bmRequestType bRequest wValue wIndex data timeout)
  (define (make-devreq bmRequestType bRequest wValue wIndex)
    (let ((bv (make-bytevector 8)))
      (bytevector-u8-set! bv 0 bmRequestType)
      (bytevector-u8-set! bv 1 bRequest)
      (bytevector-u16-set! bv 2 wValue (endianness little))
      (bytevector-u16-set! bv 4 wIndex (endianness little))
      (bytevector-u16-set! bv 6 (bytevector-length data) (endianness little))
      bv))
  (assert (fx<? (bytevector-length data) 1280))
  (unless (< 0 timeout 10000)
    (assertion-violation 'usb-control-transfer "Invalid timeout"
                         dev bmRequestType bRequest wValue wIndex data timeout))
  (let ((req (make-devreq bmRequestType bRequest wValue wIndex)))
    (let ((resp-ch (make-channel))
          (pipe (usb-device-default-control-pipe dev)))
      ;; This will update the data bytevector if it's a write request
      (put-message (usb-pipe-ch pipe) (list resp-ch (usb-device-speed dev) req data timeout))
      (match (get-message resp-ch)
        ['error
         (error 'usb-control-transfer "Transfer error"
                dev bmRequestType bRequest wValue wIndex data timeout)]
        [actual-length/stall
         ;; Returns 'stalled if the transfer stalled, otherwise the
         ;; actual length of the transfer (might not be implemented in
         ;; the HCI driver).
         actual-length/stall]))))

;; Synchronous procedure to perform a bulk write.
(define (usb-bulk-write pipe bv timeout)
  (assert (fx<? (bytevector-length bv) 1280))
  (unless (and (usb-pipe-out? pipe)
               (eq? (epdesc-transfer-type (usb-pipe-descriptor pipe)) 'bulk))
    (assertion-violation 'usb-bulk-write "Not an bulk OUT endpoint" pipe))
  (let ((resp-ch (make-channel)))
    (put-message (usb-pipe-ch pipe) (list resp-ch bv timeout))
    (match (get-message resp-ch)
      ['error (error 'usb-bulk-write "Transfer error" pipe)]
      ['stalled (error 'usb-bulk-write "Pipe stalled" pipe)]
      [actual-length actual-length])))

;; Synchronous procedure to perform a bulk read. The bytevector is
;; modified in place.
(define (usb-bulk-read pipe bv timeout)
  (assert (fx<? (bytevector-length bv) 1280))
  (unless (and (usb-pipe-in? pipe)
               (eq? (epdesc-transfer-type (usb-pipe-descriptor pipe)) 'bulk))
    (assertion-violation 'usb-bulk-read "Not a bulk IN endpoint" pipe))
  (let ((resp-ch (make-channel)))
    (put-message (usb-pipe-ch pipe) (list resp-ch bv timeout))
    (match (get-message resp-ch)
      ['error (error 'usb-bulk-read "Transfer error" pipe)]
      ['stalled (error 'usb-bulk-read "Pipe stalled" pipe)]
      [actual-length actual-length])))

;;; Standard control transfers

;; Synchronous procedure to select a device configuration. Only one
;; device configuration can be active at any time.
(define (usb-set-configuration dev configuration)
  (usb-control-transfer dev 0 devreq-SET_CONFIGURATION configuration 0 #vu8() DEFAULT-TIMEOUT))

(define (usb-set-address dev address)
  ;; TODO: Verify that the device switches to the new address
  (usb-control-transfer dev 0 devreq-SET_ADDRESS address 0 #vu8() DEFAULT-TIMEOUT))

;;;

(define (usb-fetch-descriptors dev)
  (assert (not (usb-device-$configurations dev)))
  (let ((desc (usb-get-device-descriptor dev)))
    (usb-device-$strings-set! dev '((1033 "(Unknown manufacturer)" "(Unknown product)" #f)))
    (cond
      ((and (eqv? 0 (devdesc-iManufacturer desc))
            (eqv? 0 (devdesc-iProduct desc))
            (eqv? 0 (devdesc-iSerialNumber desc)))
       ;; Don't even try to fetch the string descriptors
       #f)
      ((get-string-descriptor dev 0 0) =>
       (lambda (strdesc)
         ;; Fetch string descriptors
         (let ((strings
                (map
                 (lambda (langid)
                   (let ((manufacturer  (get-string-descriptor* dev langid (devdesc-iManufacturer desc)))
                         (product       (get-string-descriptor* dev langid (devdesc-iProduct desc)))
                         (serial-number (get-string-descriptor* dev langid (devdesc-iSerialNumber desc))))
                     (list langid manufacturer product serial-number)))
                 (string-language-descriptor->list strdesc))))
           (usb-device-$strings-set! dev strings))))
      (else
       (log/warn "Could not get device string descriptors")))

    ;; Fetch configuration descriptors
    (do ((conf 0 (fx+ conf 1))
         (cfgdesc** '()
                    (cons (split-configuration-descriptor
                           (get-configuration-descriptor dev conf))
                          cfgdesc**)))
        ((fx=? conf (devdesc-bNumConfigurations
                     (usb-get-device-descriptor dev)))
         (usb-device-$configurations-set! dev (reverse cfgdesc**))))))

(define (usb-fetch-device-descriptor dev total-length)
  (let ((resp (make-bytevector total-length)))
    (usb-control-transfer dev #x80 devreq-GET_DESCRIPTOR
                          (fxior (fxarithmetic-shift-left desctype-DEVICE 8)
                                 0)
                          0
                          resp DEFAULT-TIMEOUT)
    (usb-device-$descriptor-set! dev resp)))

(define (get-full-descriptor dev index descindex desctype)
  (let ((resp0 (make-bytevector 8)))
    (usb-control-transfer dev #x80 devreq-GET_DESCRIPTOR
                          (fxior (fxarithmetic-shift-left desctype 8)
                                 descindex)
                          index
                          resp0 DEFAULT-TIMEOUT)
    (let ((full-length (if (eqv? desctype desctype-CONFIGURATION)
                           (cfgdesc-wTotalLength resp0)
                           (desc-bLength resp0))))
      (cond ((fx=? full-length 8)
             resp0)
            ((fx>? full-length 8)
             (let ((resp1 (make-bytevector full-length)))
               (usb-control-transfer dev #x80 devreq-GET_DESCRIPTOR
                                     (fxior (fxarithmetic-shift-left desctype 8)
                                            descindex)
                                     index
                                     resp1 DEFAULT-TIMEOUT)
               resp1))
            (else
             (let ((resp (make-bytevector full-length)))
               (bytevector-copy! resp0 0 resp 0 full-length)
               resp))))))

(define (get-string-descriptor dev langid descindex)
  (get-full-descriptor dev langid descindex desctype-STRING))

(define (get-string-descriptor* dev langid descindex)
  (cond ((eqv? descindex 0) #f)
        ((get-string-descriptor dev langid descindex)
         => string-language-descriptor->string)
        (else #f)))

(define (get-configuration-descriptor dev descindex)
  (get-full-descriptor dev 0 descindex desctype-CONFIGURATION))

;; Select the nth configuration and create the interfaces.
(define (usb-configure-device dev n)
  (let ((conf (list-ref (usb-device-$configurations dev) n)))
    (usb-set-configuration dev (cfgdesc-bConfigurationValue (car conf)))
    (let ((interfaces
           (map (match-lambda
                 [(intdesc . desc*)
                  (let ((pipes (map (lambda (epdesc) (make-usb-pipe epdesc))
                                    (filter (lambda (desc)
                                              (eqv? (desc-bDescriptorType desc) desctype-ENDPOINT))
                                            desc*))))
                    (make-usb-interface intdesc desc* pipes))])
                (group-by-interface-descriptor conf))))
      (usb-device-$interfaces-set! dev interfaces))))

;;; Pipes

(define (perform-setup-pipe dev epdesc)
  (let ((ch (make-channel)))
    (put-message (usb-controller-request-channel (usb-device-controller dev))
                 (list 'setup-pipe ch dev epdesc (usb-device-speed dev)))
    (match (get-message ch)
      [('ok . pipe-ch) pipe-ch]
      [('error . reason)
       (error 'perform-setup-pipe "Failed to setup pipe" dev epdesc reason)])))

(define (usb-setup-default-control-pipe dev)
  (unless (not (usb-device-$default-control-pipe dev))
    (assertion-violation 'usb-setup-default-control-pipe
                         "Control pipe already setup" dev))
  (let* ((epdesc (pack "<CC CCSC" (format-size "<CC CCSC") desctype-ENDPOINT
                       0 0 (usb-device-max-packet-size-0 dev) 1))
         (pipe (make-usb-pipe epdesc (perform-setup-pipe dev epdesc))))
    (usb-device-$default-control-pipe-set! dev pipe)))

;; Once the bus manager has successfully used usb-set-configuration
;; and decided on a driver for an interface, it should call this
;; procedure to allocate resources for its endpoints.
(define (usb-setup-pipes dev int)
  (for-each (lambda (pipe)
              (unless (usb-pipe-$ch pipe)
                (usb-pipe-$ch-set! pipe (perform-setup-pipe dev (usb-pipe-descriptor pipe)))))
            (usb-interface-pipes int)))

;; This operation finishes when there is data on the pipe. The
;; operation return the data as a bytevector. It works with interrupt
;; and isochronous endpoints.
(define (usb-pipe-in-operation pipe)
  (unless (and (usb-pipe-in? pipe)
               (memq (epdesc-transfer-type (usb-pipe-descriptor pipe)) '(interrupt isochronous)))
    (assertion-violation 'usb-pipe-in-operation "Not an IN endpoint" pipe))
  (get-operation (usb-pipe-ch pipe)))

;; This operation finishes when the bytes are in the queue.
(define (usb-pipe-out-operation pipe bv)
  (unless (and (usb-pipe-out? pipe)
               (eq? (epdesc-transfer-type (usb-pipe-descriptor pipe)) 'interrupt))
    (assertion-violation 'usb-pipe-interrupt-out-operation "Not an interrupt OUT endpoint" pipe))
  (assert (bytevector? bv))
  (put-operation (usb-pipe-ch pipe) bv)))
