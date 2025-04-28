#!/usr/bin/env scheme-script
;; SPDX-License-Identifier: EUPL-1.2+
;; Loko Scheme PC bare-metal GUI
;; Copyright © 2019-2022 G. Weinholt
#!r6rs

;;; REPL that runs on a framebuffer.

;; Sorry for the mess. I'll clean up when I'm done. :)

(import
  (rnrs)
  (srfi :98)

  (only (loko) parameterize include library-directories)
  (loko match)

  (loko system fibers)
  (loko system logging)
  (loko system time)
  (loko system unsafe)

  (only (loko system $primitives) $void?)
  (only (loko runtime repl) banner repl)
  (only (loko system r7rs) current-output-port* current-input-port* current-error-port*)
  (only (loko runtime utils) string-split)

  (loko drivers pci)
  (loko drivers video bga)
  (loko drivers video vbe)
  (loko font font-6x13)

  (loko drivers mouse)
  (loko drivers keyboard)
  (loko drivers ps2 core)
  (loko drivers ps2 i8042)
  (loko drivers ps2 mouse)
  (loko drivers ps2 keyboard)
  (loko drivers usb hid-numbers)

  (loko drivers usb core)
  (loko drivers usb uhci)
  (loko drivers usb ehci)
  (loko drivers usb hid)
  (loko drivers usb hid-mouse)
  (loko drivers usb hid-keyboard)
  (loko drivers usb mass-storage)

  (loko drivers rtc)
  (loko drivers rtc mc146818)
  (srfi :19 time)

  (loko drivers ata ahci)
  (loko drivers ata atapi)
  (loko drivers ata core)
  (loko drivers ata drive)
  (loko drivers ata ide)
  (loko drivers ata identify)
  (loko drivers scsi block)
  (loko drivers scsi core)
  (loko drivers storage)
  (loko kernel storage)

  (loko drivers pci)

  (fs partitions common)
  (fs partitions mbr)
  (fs partitions gpt)
  (fs fatfs)
  (only (loko system $host) install-vfs)

  (loko net internet)
  (loko net dhcpv4-client)
  (loko net tcp)
  (loko drivers net)
  (loko drivers net eepro100)
  (loko drivers net rtl8139)
  (loko drivers net rtl8169)
  (loko drivers net virtio)

  (loko valand)
  (loko valand drawing)

  (loko pc-repl repl-window)
  (text-mode console events))

(define devs (pci-scan-bus #f))

(define font-h 13)
(define font-w 6)

(define (log/x severity . x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*) (apply log/x DEBUG x*))
(define (log/info . x*) (apply log/x INFO x*))
(define (log/error . x*) (apply log/x ERROR x*))

;;; VBE graphics or Bochs Graphics Array

(define the-display
  (cond

    ((get-environment-variable "BOOT_FRAMEBUFFER") =>
     (lambda (fbinfo)
       (display "Using the framebuffer from the bootloader information...\n")
       (match (read (open-string-input-port fbinfo))
         [(addr width height bpp pitch color-info)
          ;; FIXME: use bpp and color-info
          (let ((format VL_FORMAT_XRGB8888))
            (make-vl_display (make-vl_buffer width height pitch addr format)))])))

    ((find probe·pci·bga? devs) =>
     (lambda (dev)
       (display "Initializing Bochs graphics...\n")
       (let ((bga (make·pci·bga dev))
             (width 1920)
             (height 1080)
             (bpp 32)
             (format VL_FORMAT_XRGB8888))
         (bga-set-mode bga width height bpp #t #t)
         (let* ((stride (* width 4))
                (addr (bga-framebuffer-address bga)))
           (make-vl_display (make-vl_buffer width height stride addr format))))))

    ;; XXX: This needs to be commented out for this to work work with
    ;; UEFI on QEMU. It triggers some really weird bug when the
    ;; zabavno libraries are imported for eval.
    ((find probe·pci·vbe? devs) =>
     (lambda (dev)
       (display "Attempting to set a graphics mode with VBE...\n")
       (let ((VBE (make·pci·vbe dev))
             (width 1024)
             (height 768))
         (define (get-modes vbe)
           (let ((BIOS (vbe-bios vbe))
                 (info (vbe-supervga-info vbe)))
             (map (lambda (mode) (vesa-get-mode-info BIOS mode))
                  (supervga-info-VideoModes info))))
         (let ((modes (get-modes VBE))
               (supported-modes (make-hashtable equal-hash equal?)))
           ;; Record supported modes
           (for-each (lambda (info)
                       (when (and (eqv? (supervga-mode-BitsPerPixel info) 32)
                                  (eqv? (supervga-mode-MemoryModel info) 6)
                                  (not (eqv? (supervga-mode-XResolution info) 0)))
                         (hashtable-set! supported-modes
                                         (cons (supervga-mode-XResolution info)
                                               (supervga-mode-YResolution info))
                                         #t)))
                     modes)
           (cond
             ((find (lambda (info)
                      (and (eqv? (supervga-mode-XResolution info) width)
                           (eqv? (supervga-mode-YResolution info) height)
                           (eqv? (supervga-mode-BitsPerPixel info) 32)
                           (eqv? (supervga-mode-MemoryModel info) 6)))
                    modes)
              =>
              (lambda (mode)
                (log/info "Using VBE mode: " mode)
                (log/info "All supported modes: "
                          (vector-map
                           (lambda (mode)
                             (string-append (number->string (car mode)) "x" (number->string (cdr mode))))
                           (vector-sort (lambda (x y) (> (car x) (car y)))
                                        (hashtable-keys supported-modes))))
                (vesa-set-mode (vbe-bios VBE) (supervga-mode-Number mode) 'lfb #f #f)
                (let* ((modeinfo (vesa-get-mode-info (vbe-bios VBE)
                                                     (vesa-get-mode (vbe-bios VBE))))
                       ;; TODO: Support other graphics modes (at least
                       ;; 16- and 24-bit graphics)
                       (format VL_FORMAT_XRGB8888))
                  (make-vl_display
                   (make-vl_buffer (supervga-mode-XResolution modeinfo)
                                   (supervga-mode-YResolution modeinfo)
                                   (supervga-mode-BytesPerScanLine modeinfo)
                                   (supervga-mode-PhysBasePtr modeinfo)
                                   format)))))
             (else
              (display "Did not find a suitable direct color graphics mode\n")
              (display modes)
              (newline)
              #f))))))

    (else
     (error #f "Could not find a graphical framebuffer"))))

;;; Valand setup

(vl_display-initialize! the-display)

(define mouse-surface (vl_display-find/id the-display ID-CURSOR-START))
(define mouse-pointer (make-vl_pointer mouse-surface 0 0))
(define player (vl_display-add-player! the-display mouse-pointer))

(begin
  (define backsurf (vl_display-find/id the-display ID-BACKGROUND))
  (define menusurf (vl_display-find/id the-display ID-SYSTEM-MENU))

  (let ()
    (vl_buffer-draw-horizontal-gradient (vl_surface-buffer backsurf)
                                        0 0 0   0 .58 .58)
    (vl_surface-damage! backsurf))

  (let ((mouse-w 32)
        (mouse-h 32))
    ;; Mouse pointer by Paer Martinsson
    (include "pointer.scm")
    (let ((buf (vl_surface-buffer mouse-surface)))
      (vl_buffer-fill! buf (vl_buffer-color/argb buf 0 0 0 0))
      (vl_buffer-copy-from-bytevector buf 0 0 pointer
                                      mouse-w mouse-h (* mouse-w 4)
                                      VL_FORMAT_ARGB8888))
    (vl_surface-damage! mouse-surface)
    (vl_display-refill-z-buffer! the-display mouse-surface))

  (vl_buffer-draw-system-menu (vl_surface-buffer menusurf) font font-w font-h)
  (vl_surface-damage! menusurf))

;;; Integrate the mouse manager with Valand

(define mouse-manager (make-mouse-manager))

(define (mouse-driver)
  (let lp ((prev-buttons 0))
    (match (get-message (mouse-event-channel mouse-manager))
      [(and ev #(xd yd zd buttons _x))
       (let ((button-changes (bitwise-xor prev-buttons buttons)))
         (cond
           ((eqv? 0 button-changes)     ;only motion
            (vl_player-handle-mouse-input player (vector 'motion 'rel xd yd #f)))
           (else
            (let lp ((button-changes button-changes))
              (unless (eqv? button-changes 0)
                (let* ((button (bitwise-first-bit-set button-changes))
                       (action (if (bitwise-bit-set? buttons button) 'press 'release))
                       (ev (vector action 'rel xd yd (fx+ button 1) ev)))
                  (vl_player-handle-mouse-input player ev)
                  (lp (bitwise-and button-changes
                                   (bitwise-not
                                    (bitwise-arithmetic-shift-left 1 button))))))))))

       (vl_display-render the-display)  ;be zippy
       (lp buttons)])))

(spawn-fiber mouse-driver)

;;; Integrate keyboard manager with Valand

(define keyboard-manager (make-keyboard-manager))

(define (keyboard-driver)
  (let lp ()
    (match (get-message (keyboard-event-channel keyboard-manager))
      [(and ev #(make/break x page usage (leds mods key)))
       (let ((ev (vector make/break page usage key mods ev)))
         (vl_player-handle-keyboard-input player ev)
         (lp))]
      [ev
       (log/debug "Unhandled keyboard event: " ev)
       (lp)])))

(spawn-fiber keyboard-driver)

;;; Start a REPL and a log viewer

(spawn-fiber (lambda ()
               (let ((replwin (start-new-repl-window the-display 30 30 80 25 #f)))
                 (vl_player-focus-set! player (replwin-window replwin)))))

(spawn-fiber (lambda ()
               (start-new-repl-window the-display
                                      (- (vl_display-width the-display) 1)
                                      (- (vl_display-height the-display) (* 40 13) 70)
                                      160 40 #t)))

;;; Raw fun

(define (run-lines-demo surface)
  (define max-x (- (vl_surface-width surface) 5))
  (define max-y (- (vl_surface-height surface) 5))
  (define buf (vl_surface-buffer surface))
  (define (color r g b)
    (vl_buffer-color/rgb buf (/ r 255) (/ g 255) (/ b 255)))
  (define colors                       ;Pastels.gpl from gimp 2.0
    (vector (color 226 145 145)
            (color 153 221 146)
            (color 147 216 185)
            (color 148 196 211)
            (color 148 154 206)
            (color 179 148 204)
            (color 204 150 177)
            (color 204 164 153)
            (color 223 229 146)
            (color 255 165 96)
            (color 107 255 99)
            (color 101 255 204)
            (color 101 196 255)
            (color 101 107 255)
            (color 173 101 255)
            (color 255 101 244)
            (color 255 101 132)
            (color 255 101 101)))
  (define bg (vl_buffer-color/rgb buf 0 0 0))
  (define-record-type line
    (sealed #t)
    (fields (mutable color)
            (mutable x0) (mutable y0)
            (mutable x1) (mutable y1)))
  (define lines (make-vector 12))
  (do ((i 0 (fx+ i 1)))
      ((fx=? i (vector-length lines)))
    (vector-set! lines i (make-line (vector-ref colors 0) 10 12 10 12)))
  (vl_buffer-fill! (vl_surface-buffer surface) bg)
  (vl_surface-damage! surface)
  (let lp ((prev (vector-ref lines 0))
           (idx 1)
           (color-idx 0)
           (dx0 2) (dy0 3) (dx1 4) (dy1 5))
    (let ((line (vector-ref lines idx)))
      (vl_buffer-draw-line (vl_surface-buffer surface)
                           (line-x0 line) (line-y0 line)
                           (line-x1 line) (line-y1 line)
                           bg)
      (line-color-set! line (vector-ref colors color-idx))
      (line-x0-set! line (fx+ (line-x0 prev) dx0))
      (line-y0-set! line (fx+ (line-y0 prev) dy0))
      (line-x1-set! line (fx+ (line-x1 prev) dx1))
      (line-y1-set! line (fx+ (line-y1 prev) dy1))
      (vl_buffer-draw-line (vl_surface-buffer surface)
                           (line-x0 line) (line-y0 line)
                           (line-x1 line) (line-y1 line)
                           (line-color line))
      (vl_surface-damage! surface)
      (sleep 1/16)
      (letrec ((flip-x (lambda (x dx) (if (fx<=? 5 x max-x) dx (fx- dx))))
               (flip-y (lambda (y dy) (if (fx<=? 5 y max-y) dy (fx- dy)))))
        (lp line
            (fxmod (fx+ idx 1) (vector-length lines))
            (fxmod (fx+ color-idx 1) (vector-length colors))
            (flip-x (line-x0 line) dx0)
            (flip-y (line-y0 line) dy0)
            (flip-x (line-x1 line) dx1)
            (flip-y (line-y1 line) dy1))))))

;; Run the lines demo as a dockapp.
(let* ((y (- (vl_display-height the-display) 60))
       (lines-dockapp
        ;; FIXME: Valand should do automatic placing of dockapps
        (vl_display-allocate-user-surface the-display VL_FORMAT_XRGB8888
                                          61 y 60 60)))
  (vl_display-add-dockapp-surface! the-display lines-dockapp)
  (spawn-fiber (lambda ()
                 (run-lines-demo lines-dockapp))))

;;; Real-time clock dockapp

(spawn-fiber
 (lambda ()
   (let* ((y (- (vl_display-height the-display) 60))
          (clock-dockapp
           (vl_display-allocate-user-surface the-display VL_FORMAT_XRGB8888
                                             0 y 60 60)))
     (vl_display-add-dockapp-surface! the-display clock-dockapp)
     (let* ((buf (vl_surface-buffer clock-dockapp))
            (bg (vl_buffer-color/rgb buf 0 0 0))
            (fg (vl_buffer-color/rgb buf 1 1 1)))
       (let lp ((prev #f))
         (let* ((now (current-date))
                (str (if (not now)
                         "\n  ??:??\n\n????-??-??"
                         (date->string now "\n  ~H:~MZ\n\n~Y-~m-~d\n"))))
           (unless (equal? str prev)
             (vl_buffer-fill! buf bg)
             (let lp ((i 0) (x 0) (y 0))
               (unless (eqv? i (string-length str))
                 (let ((ch (string-ref str i)))
                   (case ch
                     ((#\linefeed)
                      (lp (fx+ i 1) 0 (fx+ y font-h)))
                     (else
                      (vl_buffer-draw-character buf font x y font-w font-h
                                                fg bg ch)
                      (lp (fx+ i 1) (fx+ x font-w) y))))))
             (vl_surface-damage! clock-dockapp))
           (sleep 1)
           (lp str)))))))

(let ((rtc (make-rtc-device)))
  (spawn-fiber
   (lambda ()
     (driver·rtc·mc146818 rtc)))
  (spawn-fiber
   (lambda ()
     ;; Update the system clock from the RTC once at boot. (It's
     ;; certainly possible to do much better than this, but then you
     ;; should look into how NTP works).
     (let retry ()
       (let-values ([(now ticks) (rtc-device-get-date rtc)])
         (cond (now
                (set-current-time/utc (date->time-utc now) ticks))
               (else
                (sleep 0.5)
                (retry))))))))

;;; PS/2 support

(define (manage-ps/2 controller)
  (define hotplug-channel (make-channel))
  (let lp ()
    (match (get-message (PS/2-controller-notify-channel controller))
      [('new-port . port)
       ;; At this point we should probe the port, find a driver for
       ;; the device and spawn a fiber to handle it.
       (let ((reset-id
              (guard (exn ((error? exn) #f))
                (PS/2-command port PS/2-RESET)
                (let ((result (PS/2-read port 4000)))
                  (PS/2-read port 1000)))))
         (cond
           ((probe·PS/2·mouse port)
            => (lambda (id)
                 #;(println (list 'mouse id port))
                 (spawn-fiber (lambda ()
                                (let ((mouse (make-managed-mouse mouse-manager)))
                                  (driver·PS/2·mouse port hotplug-channel id mouse))))))
           ((probe·PS/2·keyboard port)
            => (lambda (id)
                 #;(println (list 'keyboard id port))
                 (spawn-fiber (lambda ()
                                (let ((keyboard (make-managed-keyboard keyboard-manager)))
                                  (driver·PS/2·keyboard port hotplug-channel id keyboard))))))
           (else
            ;; Probing failed, we have no driver. Start the hotplug
            ;; driver, that should tell us when a new device has been
            ;; attached.
            (spawn-fiber (lambda ()
                           (driver·PS/2·hotplug port hotplug-channel))))))])
    (lp)))

(spawn-fiber
 (lambda ()
   (let ((controller (make-PS/2-controller)))
     (spawn-fiber (lambda () (manage-ps/2 controller)))
     (driver·isa·i8042 controller))))

;;; USB

(define (find-hid-driver hiddev)
  (cond
    ((probe·usb·hid·mouse? hiddev)
     (let ((mouse (make-managed-mouse mouse-manager)))
       (driver·usb·hid·mouse hiddev mouse)))
    ((probe·usb·hid·keyboard? hiddev)
     (let ((keyboard (make-managed-keyboard keyboard-manager)))
       (driver·usb·hid·keyboard hiddev keyboard)))
    (else
     (let lp ()
       (log/debug "Unknown HID data: "
                  (perform-operation (usb-pipe-in-operation (usbhid-device-in-pipe hiddev))))
       (lp)))))

(define (manage-usb-hci controller)
  (let lp ()
    (match (get-message (usb-controller-notify-channel controller))
      [('new-device . dev)
       ;; The device configuration is already set. At this point we
       ;; should find a driver for the device and spawn a fiber to
       ;; handle it.
       (let* ((desc (usb-get-device-descriptor dev))
              (ver (devdesc-bcdUSB desc))
              (bcddev (devdesc-bcdDevice desc)))
         (log/info "Found a USB " (number->string (fxbit-field ver 8 16) 16) "."
                   (number->string (fxbit-field ver 0 8) 16)
                   " device with class "
                   (number->string (devdesc-bDeviceClass desc) 16) "h/"
                   (number->string (devdesc-bDeviceSubClass desc) 16) "h/"
                   (number->string (devdesc-bDeviceProtocol desc) 16) "h"
                   ", vendor #x" (number->string (devdesc-idVendor desc) 16)
                   ", product #x" (number->string (devdesc-idProduct desc) 16)
                   ", device " (number->string (fxbit-field bcddev 8 16))
                   "." (number->string (fxbit-field bcddev 0 8) 16)
                   ", strings: " (usb-device-$strings dev)))

       (log/debug (call-with-string-output-port
                    (lambda (p)
                      (print-usb-descriptor (usb-get-device-descriptor dev) p)
                      (for-each (lambda (cfgdesc*)
                                  (for-each (lambda (desc) (print-usb-descriptor desc p))
                                            cfgdesc*))
                                (usb-device-$configurations dev)))))

       (for-each (lambda (interface)
                   (cond
                     ((probe·usb·mass-storage? dev interface)
                      (spawn-fiber
                       (lambda ()
                         (define (get-scsi-req-channel)
                           (let ((scsi-req-ch (make-channel)))
                             (put-message scsi-manager-ch (cons 'new-device scsi-req-ch))
                             scsi-req-ch))
                         (driver·usb·mass-storage dev interface get-scsi-req-channel))))

                     ((probe·usb·hid? dev interface)
                      (spawn-fiber
                       (lambda ()
                         (driver·usb·hid dev interface find-hid-driver))))))
                 (usb-device-interfaces dev))])
    (lp)))

;;; Disk stuff

(define (log-device-info device-type identify-block)
  (let-values ([(logical physical) (ata-identify:ata-sector-size identify-block)])
    (log/info "Found an " device-type " drive with model " (ata-identify:model-number identify-block)
              ", serial " (ata-identify:serial-number identify-block)
              ", firmware " (ata-identify:firmware-revision identify-block)
              ", commands: " (ata-identify:supported-command-set identify-block)
              ;; " Major-version: " (ata-identify:major-revision identify-block)
              (cond ((ata-identify:ata-device? identify-block) ", ATA: ")
                    ((ata-identify:atapi-device? identify-block) ", ATAPI: ")
                    (else ", Unknown device: "))
              (if (ata-identify:ata-device? identify-block)
                  (list "Logical sector size:" logical
                        "Physical sector size:" physical)
                  (list)))
    #;
    (log/info "Raw identify block: " identify-block)))

(define storage-manager-ch (make-channel))

(define (make-sector-reader storage lba-start lba-end)
  (lambda (lba)
    (cond
      ((or (not lba-end) (<= lba-start lba lba-end))
       (let ((resp-ch (make-channel)))
         (put-message (storage-device-request-channel storage)
                      (list resp-ch 'read (+ lba-start lba) 1))
         (match (get-message resp-ch)
           [('ok data)
            data]
           [('error . x)
            (log/error "Error reading from medium: LBA=" lba " error=" x)
            (eof-object)])))
      (else
       (log/error "Out of range read")
       (eof-object)))))

(define (storage-device->fatfs-device storage-device starting-lba ending-lba)
  (define cache (make-block-cache (* 2 1024 1024)
                                  (storage-device-logical-sector-size storage-device)))
  (define dev (make-logical-storage "todo" storage-device starting-lba ending-lba cache))
  (define logical-sector-size
    (storage-device-logical-sector-size storage-device))
  (define (read-sectors lba sectors)
    (logical-storage-read dev lba sectors))
  (define (write-sectors lba data)
    (logical-storage-write dev lba data))
  (define (flush) #f)
  (define (close) #f)
  (make-fatfs-device logical-sector-size
                     read-sectors
                     write-sectors
                     flush
                     close))

(define (fatfs-directory-files fs dirname)
  (let ((dir (fatfs-open-directory fs dirname)))
    (let lp ((ret '()))
      (let ((info (fatfs-read-directory dir)))
        (cond ((eof-object? info)
               (fatfs-close-directory dir)
               (reverse ret))
              (else
               (lp (cons (or (fatfs-file-info:long-filename info)
                             (fatfs-file-info:filename info))
                         ret))))))))

(define (manage-storage)
  (let lp ()
    (match (get-message storage-manager-ch)
      [('new-storage storage)
       (log/debug "New storage device: " storage)
       (spawn-fiber
        (lambda ()
          ;; Scan the partition table and list root directories of FAT
          ;; filesystems. Skip 2048-byte sector drives for now, they
          ;; are CD drives.
          (unless (eqv? (storage-device-logical-sector-size storage) 2048)
            (let ((read-sector (make-sector-reader storage 0 #f))
                  (sector-size (storage-device-logical-sector-size storage)))
              (let ((mbr (read-mbr read-sector)))
                ;; XXX: Real applications should probably check the MBR
                ;; to verify that it's a protective MBR. It could have
                ;; been overwritten with a valid MBR, in which case
                ;; gpt1 is likely an out of date GPT backup.
                (let-values ([(gpt0 gpt1) (read-gpt read-sector #f)])
                  (log/info "MBR: " mbr)
                  (when (parttable-valid? gpt0)
                    (log/info "GPT0: " gpt0))
                  (when (parttable-valid? gpt1)
                    (log/info "GPT1: " gpt1))
                  (let ((table (cond ((parttable-valid? gpt0) gpt0)
                                     ((parttable-valid? gpt1) gpt1)
                                     (else mbr))))
                    (for-each
                     (lambda (part)
                       (define GPT-TYPE-EFI-System
                         #vu8(#xC1 #x2A #x73 #x28
                                   #xF8 #x1F #x11 #xD2 #xBA #x4B
                                   #x00 #xA0 #xC9 #x3E #xC9 #x3B))
                       (when (or (member (part-type part)
                                         '(1 4 6 #xb #xc #xd #xe #xef))
                                 (equal? (part-type part) GPT-TYPE-EFI-System))
                         (log/info "Mounting the FAT file system on " part)
                         (let* ((dev (storage-device->fatfs-device storage
                                                                   (part-start-lba part)
                                                                   (part-end-lba part)))
                                (fs (open-fatfs dev)))
                           (define (parse-filename who filename)
                             (if (string=? filename "/")
                                 '()
                                 (let ((components (string-split filename #\/)))
                                   (cond ((and (not (string=? filename ""))
                                               (char=? (string-ref filename 0) #\/))
                                          (cdr components))
                                         (else
                                          (condition
                                           (make-i/o-filename-error filename)
                                           (make-who-condition who)
                                           (make-message-condition "Bad filename")))))))
                           (define (open-file filename file-options buffer-mode who)
                             (define no-create (enum-set-member? 'no-create file-options))
                             (define no-fail (enum-set-member? 'no-fail file-options))
                             (define no-truncate (enum-set-member? 'no-truncate file-options))
                             (define create (not no-create))
                             (define fail (not no-fail))
                             (define truncate (not no-truncate))
                             ;; (log/info (list filename file-options buffer-mode who))
                             (let ((path/err (parse-filename 'open-file filename))
                                   (flags (case who
                                            ((open-file-output-port open-file-input/output-port)
                                             (if (and fail create)
                                                 (fxior fatfs-open/create fatfs-open/exclusive)
                                                 (if truncate
                                                     (if (and no-fail create)
                                                         (fxior fatfs-open/create fatfs-open/truncate)
                                                         fatfs-open/truncate)
                                                     (if (and no-fail create)
                                                         fatfs-open/create
                                                         0))))
                                            (else 0)))
                                   (access-mode (case who
                                                  ((open-file-input-port) fatfs-open/read)
                                                  ((open-file-output-port) fatfs-open/write)
                                                  ((open-file-input/output-port) fatfs-open/read+write))))
                               (if (or (pair? path/err) (null? path/err))
                                   (guard (exn
                                           ((and (who-condition? exn)
                                                 (eq? (condition-who exn) 'fatfs-open-file))
                                            (raise
                                              (condition exn (make-i/o-filename-error filename)))))
                                     (fatfs-open-file fs path/err (fxior access-mode flags)))
                                   (raise
                                     (condition path/err
                                                (make-irritants-condition
                                                 (list filename file-options buffer-mode)))))))
                           (define (file-exists? fn)
                             (let ((path/err (parse-filename 'file-exists? fn)))
                               (if (or (pair? path/err) (null? path/err))
                                   (fatfs-file-exists? fs path/err)
                                   (raise path/err))))
                           (define (directory-files fn hidden?)
                             (let ((path/err (parse-filename 'directory-files fn)))
                               (if (or (pair? path/err) (null? path/err))
                                   (fatfs-directory-files fs path/err)
                                   (raise path/err))))
                           (define (delete-file fn)
                             (let ((path/err (parse-filename 'delete-file fn)))
                               (if (or (pair? path/err) (null? path/err))
                                   (fatfs-delete-file fs path/err)
                                   (raise path/err))))
                           (log/debug fs)
                           (log/info "FAT root directory: "
                                     (fatfs-directory-files fs '()))
                           (install-vfs 'open-file open-file
                                        'file-exists? file-exists?
                                        'directory-files directory-files
                                        'delete-file delete-file
                                        )
                           (library-directories (cons "/lib" (library-directories))))))
                     (parttable-partitions table)))))))))])
    (lp)))

(spawn-fiber (lambda ()
               (parameterize ([current-log-fields
                               (append '(SUBSYSTEM "storage")
                                       (current-log-fields))])
                 (manage-storage))))

(define scsi-manager-ch (make-channel))
(spawn-fiber
 (lambda ()
   (let lp ()
     (match (get-message scsi-manager-ch)
       [('new-device . ch)
        (spawn-fiber
         (lambda ()
           (match (probe·scsi ch)
             [((and (or 'SBC 'MMC) class) inq vpd-id)
              (log/info "Found a SCSI SBC/MMC compatible device."
                        " INQUIRY: " inq " VPD ID: " vpd-id)
              (let* ((scsidev (make-scsi-device ch inq))
                     (storage (let-values ([(max-lba sector-size)
                                            (scsi·block·read-capacity scsidev)])
                                ;; FIXME: this can't be the right way?
                                (let ((sector-size (or sector-size
                                                       (case class
                                                         ((MMC) 2048)
                                                         (else 512)))))
                                  (make-storage-device "SCSI logical unit" sector-size)))))
                (put-message storage-manager-ch (list 'new-storage storage))
                (driver·scsi·block scsidev storage))]
             [(_ inq vpd-id)
              (log/info "No driver for this SCSI device."
                        " INQUIRY: " inq " VPD ID: " vpd-id)])))])
     (lp))))

(define (ata-manager controller)
  (let lp ()
    (match (get-message (ata-controller-notify-channel controller))
      [('new-device . ch)
       ;; Probe the device and start a driver
       (spawn-fiber
        (lambda ()
          (match (probe·ata ch)
            [('ata . identify-block)
             (log-device-info 'ATA identify-block)
             (let-values ([(logical _) (ata-identify:ata-sector-size identify-block)])
               (let* ((atadev (make-ata-device controller ch identify-block))
                      (storage (make-storage-device "ATA drive" logical)))
                 (put-message storage-manager-ch (list 'new-storage storage))
                 (driver·ata·drive atadev storage)))]

            [('atapi . identify-block)
             (log-device-info 'ATAPI identify-block)
             (let ((scsi-req-ch (make-channel)))
               (put-message scsi-manager-ch (cons 'new-device scsi-req-ch))
               (driver·ata·atapi (make-ata-device controller ch identify-block)
                                 scsi-req-ch))]

            ['no-device
             '(log/info "No device")]

            [(type . x)
             (log/info "No driver for " type " devices")])))
       (lp)])))


(for-each (lambda (dev)
            (cond
              ((probe·pci·ahci? dev)
               (log/info "Found an AHCI controller: " dev)
               (let ((controller (make-ata-controller)))
                 (spawn-fiber (lambda () (ata-manager controller)))
                 (spawn-fiber (lambda () (driver·pci·ahci dev controller)))))
              ((probe·pci·ide? dev)
               (log/info "Found an IDE controller: " dev)
               (let ((controller (make-ata-controller)))
                 (spawn-fiber (lambda () (ata-manager controller)))
                 (spawn-fiber (lambda () (driver·pci·ide dev controller)))))))
          devs)

(define (start-networking iface)
  (let* ((ether (make-ether iface))
         (tcp (make-tcp ether)))
    (spawn-fiber (lambda () (net·ethernet·internet-stack ether)))
    (spawn-fiber (lambda ()
                   (parameterize ([current-log-fields
                                   (append '(SUBSYSTEM "dhcpv4")
                                           (current-log-fields))])
                     (net·ethernet·dhcpv4-client ether))))
    (spawn-fiber (lambda () (net·tcp/ip tcp)))
    (spawn-fiber
     (lambda ()
       ;; Wait for an IP address
       (let lp ()
         (sleep 1)
         (unless (ether-default-ipv4-source ether)
           (lp)))
       ;; Listen to the telnet port and start a repl when anyone
       ;; connects.
       (let* ((local-addr (ether-default-ipv4-source ether))
              (local-port 23)
              (telnet-server (tcp-open tcp local-addr local-port '* '* 'passive)))
         (log/info "You may now connect to port 23 to access the remote REPL")
         (let lp ()
           (let-values ([(conn addresses) (tcp-accept telnet-server)])
             (log/info "Accepting REPL connection from " addresses)
             (spawn-fiber
              (lambda ()
                (let* ((tc (make-transcoder (utf-8-codec) (eol-style crlf)))
                       (pi (transcoded-port (tcp-conn-input-port conn addresses) tc))
                       (po (transcoded-port (tcp-conn-output-port conn addresses #f) tc))
                       (pe (transcoded-port (tcp-conn-output-port conn addresses #t) tc)))
                  (parameterize ([current-input-port* pi]
                                 [current-output-port* po]
                                 [current-error-port* pe])
                    (banner (current-output-port))
                    (repl)))))
             (lp))))))))

(for-each (lambda (dev)
            (cond ((probe·pci·virtio-net? dev)
                   (log/info "Found a virtio network card: " dev)
                   (let ((iface (make-iface 'ethernet)))
                     (spawn-fiber (lambda () (driver·pci·virtio-net dev iface)))
                     (start-networking iface)))
                  ((probe·pci·rtl8139? dev)
                   (log/info "Found an rtl8139 network card: " dev)
                   (let ((iface (make-iface 'ethernet)))
                     (spawn-fiber (lambda () (driver·pci·rtl8139 dev iface)))
                     (start-networking iface)))
                  ((probe·pci·rtl8169? dev)
                   (log/info "Found an rtl8169 network card: " dev)
                   (let ((iface (make-iface 'ethernet)))
                     (spawn-fiber (lambda () (driver·pci·rtl8169 dev iface)))
                     (start-networking iface)))
                  ((probe·pci·eepro100? dev)
                   (log/info "Found an eepro100 network card: " dev)
                   (let ((iface (make-iface 'ethernet)))
                     (spawn-fiber (lambda () (driver·pci·eepro100 dev iface)))
                     (start-networking iface)))))
          devs)

#;
(for-each (lambda (dev)
            (when (probe·pci·ehci? dev)
              (log/info "Found an EHCI controller: " dev)
              (spawn-fiber
               (lambda ()
                 (let ((controller (make-usb-controller)))
                   ;; (spawn-fiber (lambda () (manage-usb-hci controller)))
                   (driver·pci·ehci dev controller))))))
          devs)

(for-each (lambda (dev)
            (when (probe·pci·uhci? dev)
              (log/info "Found a UHCI controller: " dev)
              (spawn-fiber
               (lambda ()
                 (let ((controller (make-usb-controller)))
                   (spawn-fiber (lambda ()
                                  (parameterize ([current-log-fields
                                                  (append '(SUBSYSTEM "usb")
                                                          (current-log-fields))])
                                    (manage-usb-hci controller))))
                   (driver·pci·uhci dev controller))))))
          devs)


(let lp ()
  (vl_display-render the-display)
  (sleep 1/60)
  (lp))
