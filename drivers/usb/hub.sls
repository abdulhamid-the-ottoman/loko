;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; USB hub driver and hub abstraction

(library (loko drivers usb hub)
  (export
    probe·usb·hub?
    driver·usb·hub
    usb-enumerator)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system logging)
    (loko system fibers)
    (loko drivers usb core))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'usb-hub))

(define (log/debug . x*) '(log/x DEBUG x*))
(define (log/error . x*) (log/x ERROR x*))
(define (log/info . x*) (log/x INFO x*))

;;; USB hub class (external hubs)

;; These hubs are real hubs and the driver needs to talk with
;; usb-enumerator using the same protocol as root hubs.

(define (probe·usb·hub? _dev interface)
  (eqv? #x09 (intdesc-bInterfaceClass (usb-interface-descriptor interface))))

(define (driver·usb·hub dev interface hub)
  (define C_HUB_LOCAL_POWER     0)  ;Hub
  (define C_HUB_OVER_CURRENT    1)  ;Hub
  (define PORT_CONNECTION       0)  ;Port
  (define PORT_ENABLE           1)  ;Port
  (define PORT_SUSPEND          2)  ;Port
  (define PORT_OVER_CURRENT     3)  ;Port
  (define PORT_RESET            4)  ;Port
  (define PORT_POWER            8)  ;Port
  (define PORT_LOW_SPEED        9)  ;Port
  (define C_PORT_CONNECTION    16)  ;Port
  (define C_PORT_ENABLE        17)  ;Port
  (define C_PORT_SUSPEND       18)  ;Port
  (define C_PORT_OVER_CURRENT  19)  ;Port
  (define C_PORT_RESET         20)  ;Port
  (define DEFAULT-TIMEOUT 500)
  (define (feature-selector feature)
    (case feature
      [(PORT_RESET) PORT_RESET]
      [(PORT_SUSPEND) PORT_SUSPEND]
      [(PORT_ENABLE) PORT_ENABLE]
      [(PORT_POWER) PORT_POWER]
      [(C_PORT_ENABLE) C_PORT_ENABLE]
      [(C_PORT_CONNECTION) C_PORT_CONNECTION]
      [(C_PORT_RESET) C_PORT_RESET]
      [else #f]))

  ;; FIXME: Do any of these need to target the interface number?
  (define (handle-hub-request ch req)
    (log/debug "Hub request: " req)
    (match req
      ;; [('ClearHubFeature port) #f]
      ;; [('ClearPortFeature port) #f]
      ;; [('GetBusState port) #f]
      [#('GetHubDescriptor)
       (let ((data (make-bytevector (format-size "<uCCCSCC CC"))))
         (usb-control-transfer dev #b10100000 devreq-GET_DESCRIPTOR 0 0 data DEFAULT-TIMEOUT)
         (put-message ch (cons 'ok data)))]
      ;; [('GetHubStatus port) #f]
      [#('GetPortStatus port)
       (let ((data (make-bytevector (format-size "<SS"))))
         (usb-control-transfer dev #b10100011 devreq-GET_STATUS 0 port data DEFAULT-TIMEOUT)
         (put-message ch (cons 'ok data)))]
      ;; [('SetHubDescriptor port) #f]
      ;; [('SetHubFeature port) #f]
      [#('SetPortFeature feature port)
       (let ((wValue (feature-selector feature)))
         (cond ((not wValue)
                (put-message ch '(fail . bad-request)))
               (else
                (usb-control-transfer dev #b00100011 devreq-SET_FEATURE wValue port #vu8() DEFAULT-TIMEOUT)
                (put-message ch '(ok . #vu8())))))]
      [#('ClearPortFeature feature port)
       (let ((wValue (feature-selector feature)))
         (cond ((not wValue)
                (put-message ch '(fail . bad-request)))
               (else
                (usb-control-transfer dev #b00100011 devreq-CLEAR_FEATURE wValue port #vu8() DEFAULT-TIMEOUT)
                (put-message ch '(ok . #vu8())))))]
      [_ (put-message ch '(fail . unknown-req))]))

  (define request-ch (usb-hub-request-channel hub))
  (define notify-ch (usb-hub-notify-channel hub)) ;TODO: set up an interrupt transfer

  (usb-setup-pipes dev interface)
  (let loop ()
    (match (get-message request-ch)
      [(ch . req)
       (handle-hub-request ch req)
       (loop)])))

;;; USB device enumeration

;; This procedure is responsible for device enumeration on a single
;; USB controller. It talks with USB controllers (root hubs) and USB
;; hubs by using the USB hub protocol. It makes sure that only one
;; device at a time has the default address (0), assigns new devices
;; addresses and requests their device descriptors.

(define (usb-enumerator controller shutdown-cvar)
  (define root-hub (usb-controller-root-hub controller))
  (define (fetch-hub-descriptor hub)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch '#(GetHubDescriptor)))
      (match (get-message ch)
        [('ok . resp) (usb-hub-descriptor-set! hub resp)]
        [('fail . x) (error 'fetch-hub-descriptor "USB request failed" hub)])))
  (define (hub-get-port-status hub port)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch `#(GetPortStatus ,port)))
      (match (get-message ch)
        [('ok . resp) (unpack "<SS" resp)]
        [('fail . x) (error 'hub-get-port-status "USB request failed" hub x)])))
  (define (hub-set-port-feature hub port feature)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch `#(SetPortFeature ,feature ,port)))
      (match (get-message ch)
        [('ok . resp) resp]
        [('fail . x) (error 'hub-set-port-feature "USB request failed" hub x)])))
  (define (hub-clear-port-feature hub port feature)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch `#(ClearPortFeature ,feature ,port)))
      (match (get-message ch)
        [('ok . resp) resp]
        [('fail . x) (error 'hub-clear-port-feature "USB request failed" hub x)])))
  (define (hubdesc-bNbrPorts desc) (bytevector-u8-ref desc 2))

  ;; Keep track of the connected devices
  (define devices (make-vector 128 #f))
  (define (next-free-address)
    (let lp ((i 1))
      (cond ((fx=? i (vector-length devices)) #f)
            ((eqv? #f (vector-ref devices i)) i)
            (else (lp (fx+ i 1))))))

  (define (reset-port hub port)
    (define PORT_CONNECTION   0)
    (define PORT_ENABLE       1)
    (define PORT_SUSPEND      2)
    (define PORT_OVER_CURRENT 3)
    (define PORT_RESET        4)
    (define PORT_POWER        8)
    (define PORT_LOW_SPEED    9)
    (define C_PORT_CONNECTION   0)
    (define C_PORT_ENABLE       1)
    (define C_PORT_SUSPEND      2)
    (define C_PORT_OVER_CURRENT 3)
    (define C_PORT_RESET        4)

    (log/debug "Resetting port " port)
    (hub-set-port-feature hub port 'PORT_RESET) ;initiate a reset
    (log/debug "Waiting for reset")
    (let lp ((i 0))
      (sleep (/ 10 1000))
      (let-values ([(wPortStatus wPortChange) (hub-get-port-status hub port)])
        (log/debug "Port status: " port " = #b" (number->string wPortStatus 2)
                   " " (number->string wPortChange 2))
        (cond ((and (fxbit-set? wPortChange C_PORT_RESET)
                    (fxbit-set? wPortStatus PORT_ENABLE)))
              ((eqv? i 10)
               (error 'reset-port "Reset timed out" hub port))
              (else (lp (fx+ i 1)))))))

  (define (reset-and-create-device hub port low-speed?)
    (guard (exn
            ((serious-condition? exn)
             ;; Disable the port if anything goes wrong
             (log/error "USB: Reset and create failed: " exn)
             (hub-clear-port-feature hub port 'PORT_ENABLE)
             #f))
      (let ((address (next-free-address)))
        (unless address
          (error 'reset-and-create-device "No more free addresses" hub port))
        (reset-port hub port)
        (let ((dev0 (make-usb-device controller hub port 0 8 (if low-speed? 'low 'full) #f)))
          (usb-setup-default-control-pipe dev0)
          (usb-fetch-device-descriptor dev0 8)
          (let ((desc (usb-get-device-descriptor dev0)))
            (usb-set-address dev0 address)
            (sleep 0.002)
            (let ((dev (make-usb-device controller hub
                                        port address
                                        (devdesc-bMaxPacketSize0 desc)
                                        (usb-device-speed dev0)
                                        desc)))
              (usb-setup-default-control-pipe dev)
              (usb-fetch-device-descriptor dev (desc-bLength desc))
              (hub-clear-port-feature hub port 'C_PORT_CONNECTION)
              (vector-set! devices address dev)
              dev))))))

  (define hubs (list root-hub))

  (fetch-hub-descriptor root-hub)

  (let loop ()
    (case (perform-operation
           (choice-operation
            (wrap-operation (sleep-operation 1) (lambda _ 'poll))
            (wrap-operation (wait-operation shutdown-cvar) (lambda _ 'stop))))
      [(poll)
       (do ((hub* hubs (cdr hub*)))
           ((null? hub*))
         (let ((hub (car hub*)))
           (do ((num-ports (hubdesc-bNbrPorts (usb-hub-descriptor hub)))
                (port 1 (fx+ port 1)))
               ((fx>? port num-ports))
             (let-values ([(wPortStatus wPortChange) (hub-get-port-status hub port)])
               (when (fxbit-set? wPortChange 0)
                 (log/debug "Port " port "/" num-ports " status=#b" (number->string wPortStatus 2)
                            " change=#b" (number->string wPortChange 2))
                 (cond
                   ((fxbit-set? wPortStatus 0)
                    (let ((low-speed? (fxbit-set? wPortStatus 9)))
                      (cond
                        ((reset-and-create-device hub port low-speed?) =>
                         (lambda (dev)
                           (usb-fetch-descriptors dev)
                           (log/debug "New USB device on port " port)

                           ;; To keep things simple, always use the
                           ;; first configuration. Most devices have a
                           ;; single configuration.
                           (usb-configure-device dev 0)

                           (for-each
                            (lambda (interface)
                              (when (probe·usb·hub? dev interface)
                                (log/debug "Found a USB hub")
                                (let ((new-hub (make-usb-hub)))
                                  (spawn-fiber (lambda ()
                                                 (driver·usb·hub dev interface new-hub)))
                                  (fetch-hub-descriptor new-hub)
                                  ;; Enable power to the ports
                                  (let ((hubdesc (usb-hub-descriptor new-hub)))
                                    (do ((i 1 (fx+ i 1)))
                                        ((fx>? i (hubdesc-bNbrPorts hubdesc)))
                                      (hub-set-port-feature new-hub i 'PORT_POWER)))
                                  (set! hubs (cons new-hub hubs)))))
                            (usb-device-interfaces dev))

                           ;; Send a notification that there's a new
                           ;; device. If it's not a hub then whatever
                           ;; is listening will need to find a driver.
                           (spawn-fiber
                            (lambda ()
                              (put-message (usb-controller-notify-channel controller)
                                           (cons 'new-device dev)))))))))
                   (else
                    ;; TODO: Should kill the driver and so on
                    (log/debug "Disconnected device on " port)
                    (hub-clear-port-feature hub port 'C_PORT_CONNECTION))))))))
       (loop)]
      [(stop)
       #f]))))
