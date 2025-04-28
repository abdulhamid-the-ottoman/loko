;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Universal Host Controller Interface (UHCI)

#|

Conventions in this driver:

 * &variable is a pointer to a virtual address for DMA:able (32-bit)
   memory
 * %variable is driver state;
 * devreq is device request
 * $variable is private
 * variable* is a list of zero or more objects
 * TD is transfer descriptor
 * QH is queue head

The driver is based on Lunt's book on USB. The queues are according to
the book, but oddly enough differ signficantly from how the design
guide says they should be set up wrt isochronous transfers and
reclamation.

|#

(library (loko drivers usb uhci)
  (export
    probe·pci·uhci?
    driver·pci·uhci
    driver·uhci)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko system unsafe)
    (loko system unsafe cache)
    (only (loko system $host) dma-allocate dma-free
          enable-irq wait-irq-operation)
    (loko drivers pci)
    (loko drivers utils)
    (loko drivers usb core)
    (loko drivers usb hub)
    (struct pack))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'usb-uhci))

(define (log/debug . x*)
  (log/x DEBUG x*))

(define (log/error . x*)
  (log/x ERROR x*))

(define (log/warn . x*)
  (log/x WARNING x*))

(define (driver·uhci regs regs-size irq controller)
  (define shutdown-cvar (make-cvar))

  (define-i/o uhci-regs (endianness little)
    (u16 USBCMD-ref USBCMD-set!)
    (u16 USBSTS-ref USBSTS-set!)
    (u16 USBINTR-ref USBINTR-set!)
    (u16 FRNUM-ref FRNUM-set!)
    (u32 FRBASEADDR-ref FRBASEADDR-set!)
    (u8 SOFMOD-ref SOFMOD-set!))

  (define (PortSCn-ref regs n)
    (get-i/o-u16 (fx+ (fx+ regs #x10) (fx* n 2))))

  (define (PortSCn-set! regs n v)
    (put-i/o-u16 (fx+ (fx+ regs #x10) (fx* n 2)) v))

  (define (reg-flush-writes)
    ;; So... should read something. Only on some busses/archs, where
    ;; writes need to be posted. And it's not needed if a write is
    ;; followed by a read.
    #f)

  (define (copy-bytevector-to-memory bv &addr)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i (bytevector-length bv)))
      (put-mem-u8 (fx+ &addr i) (bytevector-u8-ref bv i))))

  (define (copy-memory-to-bytevector &addr bv)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i (bytevector-length bv)) bv)
      (bytevector-u8-set! bv i (get-mem-u8 (fx+ &addr i)))))

;;; Register definitions

  ;; Command register
  (define CMD-MAXP     #b10000000)      ;Max Packet, 64/32 bytes
  (define CMD-CF       #b01000000)      ;Tell BIOS to go away
  (define CMD-SWDBG    #b00100000)      ;Software Debug
  (define CMD-FGR      #b00010000)      ;Force Global Resume
  (define CMD-EGSM     #b00001000)      ;Enter Global Suspend Mode
  (define CMD-GRESET   #b00000100)      ;Global Reset
  (define CMD-HCRESET  #b00000010)      ;Host Controller Reset
  (define CMD-RS       #b00000001)      ;Run/Stop

  ;; Status register
  (define STS-HCHalted #b100000)        ;Host Controller halted
  (define STS-HCPError #b010000)        ;Host Controller Process Error
  (define STS-HSError  #b001000)        ;Host System Error
  (define STS-Resume   #b000100)        ;Resume Detect
  (define STS-USBError #b000010)        ;USB Error Interrupt
  (define STS-USBINT   #b000001)        ;USB Interrupt
  (define STS-all-bits #b111111)
  (define (USBSTS-clear! regs)          ;clear all status bits
    (USBSTS-set! regs STS-all-bits))

  ;; Interrupt enable register
  (define INTR-Short       #b1000) ;Short Packet Interrupt Enable
  (define INTR-IOC         #b0100) ;Interrupt On Complete (IOC) Enable
  (define INTR-Resume      #b0010) ;Resume Interrupt Enable
  (define INTR-Timeout/CRC #b0001) ;Timeout/CRC Interrupt Enable

  ;; Frame number register
  (define FRNUM-MAX 1024)

  ;; Start of frame (SOF) modify register
  (define SOF-base 11936)               ;timing

  ;; Port status and control register
  (define PortSC-Suspend        #b1000000000000) ;Port suspend
  (define PortSC-Reset          #b0001000000000) ;Port reset
  (define PortSC-Low-Speed      #b0000100000000) ;Low speed device attached
  (define PortSC-Reserved-1     #b0000010000000) ;Always read as 1
  (define PortSC-Resume-Detect  #b0000001000000) ;Resume Detect
  (define PortSC-Line-D-        #b0000000100000) ;D- line status
  (define PortSC-Line-D+        #b0000000010000) ;D+ line status
  (define PortSC-Enable-Change  #b0000000001000) ;Enable/disable change
  (define PortSC-Enable         #b0000000000100) ;Enable/disable
  (define PortSC-Connect-Change #b0000000000010) ;Connect status change
  (define PortSC-Connect        #b0000000000001) ;Current connect status
  (define PortSC-R/WC-mask      #b0000000001010) ;Cleared when written as 1

;;; Hardware (DMA) data structures

  ;; Frame list pointer. One u32.
  (define FLP-mask #xFFFFFFF0)        ;valid address bits
  (define FLP-QH  #b10)               ;1=QH, 0=TD
  (define FLP-T   #b01)               ;1=Terminate (address not valid)
  (define (FLP-link-queue! &addr idx &queue-physaddr)
    (put-mem-u32 (fx+ &addr (fx* idx 4)) (fxior &queue-physaddr FLP-QH)))

  ;; Transfer descriptor. 16-byte alignment. Eight u32. Link, control
  ;; and status, token, buffer pointer and four u32 reserved for the
  ;; driver (as if).
  (define TD-size 32)
  (define TD-address-mask FLP-mask)
  (define TD0-LINK-LP-mask #xFFFFFFF0) ;valid address bits
  (define TD0-LINK-Vf  #b100)          ;1=depth first, 0=breadth first
  (define TD0-LINK-Q   #b010)          ;1=QH, 0=TD
  (define TD0-LINK-T   #b001)         ;1=Terminate (address not valid)
  (define (TD-link-td! &addr &other-td)
    (put-mem-u32 &addr (fxior TD0-LINK-Vf &other-td)))
  (define (TD-link-td/horiz! &addr &queue-physaddr)
    (put-mem-u32 &addr &queue-physaddr))
  (define (TD-link-terminate! &addr)
    (put-mem-u32 &addr TD0-LINK-T))
  (define (TD-link-td &addr)
    (let ((x (get-mem-u32 &addr)))
      (and (fxzero? (fxand x (fxior TD0-LINK-Q TD0-LINK-T)))
           (fxand x TD0-LINK-LP-mask))))

  (define TD1-CTRL-SPD         #b100000000000000000000000000000) ;Short Packet Detect
  (define TD1-CTRL-Errors-mask #b011000000000000000000000000000) ;Error down counter
  (define TD1-CTRL-LS          #b000100000000000000000000000000) ;Target is low speed
  (define TD1-CTRL-ISO         #b000010000000000000000000000000) ;1=Isochronous
  (define TD1-CTRL-IOC         #b000001000000000000000000000000) ;1=Interrupt on Complete
  (define TD1-STS-Active       #b000000100000000000000000000000) ;1=Active
  (define TD1-STS-Stalled      #b000000010000000000000000000000) ;1=Stalled
  (define TD1-STS-BufferErr    #b000000001000000000000000000000) ;1=Data Buffer Error
  (define TD1-STS-Babble       #b000000000100000000000000000000) ;1=Babble detected
  (define TD1-STS-NAK          #b000000000010000000000000000000) ;1=NAK Received
  (define TD1-STS-CRC/Timeout  #b000000000001000000000000000000) ;1=CRC/Time Out Error
  (define TD1-STS-BitstuffErr  #b000000000000100000000000000000) ;1=Bitstuff Error
  (define TD1-STS-ActLen-mask  #b000000000000000000011111111111) ;Actual Length
  (define (TD-set-control! &addr low-speed? isochronous? ioc? spd?)
    (put-mem-u32 (fx+ &addr 4)
                  (fxior (fxior TD1-CTRL-Errors-mask
                                TD1-STS-Active)
                         (fxior (if low-speed? TD1-CTRL-LS 0)
                                (if isochronous? TD1-CTRL-ISO 0)
                                (if ioc? TD1-CTRL-IOC 0)
                                (if spd? TD1-CTRL-SPD 0)))))
  (define (TD-set-inactive! &addr)
    (put-mem-u32 (fx+ &addr 4) 0)
    (store-fence))
  (define (TD-status-active? &addr)
    (load-fence)
    (fxtest (get-mem-u32 (fx+ &addr 4))
            TD1-STS-Active))
  (define (TD-status-error? &addr)
    (load-fence)
    (fxtest (get-mem-u32 (fx+ &addr 4))
            (fxior TD1-STS-Stalled
                   TD1-STS-BufferErr
                   TD1-STS-Babble
                   TD1-STS-NAK
                   TD1-STS-CRC/Timeout
                   TD1-STS-BitstuffErr)))
  (define (TD-stalled? &addr)
    (fxtest (get-mem-u32 (fx+ &addr 4))
            TD1-STS-Stalled))
  (define (TD-actual-length &addr)
    (fx+ 1 (fxand (get-mem-u32 (fx+ &addr 4)) TD1-STS-ActLen-mask)))

  (define TD2-TOKEN-MaxLen  #b11111111111000000000000000000000) ;Maximum Length
  (define TD2-TOKEN-D                   #b10000000000000000000) ;0=DATA0, 1=DATA1
  (define TD2-TOKEN-EndPt               #b01111000000000000000) ;Endpoint
  (define TD2-TOKEN-Address             #b00000111111100000000) ;Device Address
  (define TD2-TOKEN-PID                 #b00000000000011111111) ;Packet Identification
  (define mask-TD2-TOKEN-MaxLen #b11111111111)
  (define shift-TD2-TOKEN-MaxLen 21)
  (define shift-TD2-TOKEN-D 19)
  (define shift-TD2-TOKEN-EndPt 15)
  (define shift-TD2-TOKEN-Address 8)
  (define Length0 0)
  (define DATA1 1)
  (define DATA0 0)
  (define PID-SETUP #x2D)
  (define PID-OUT   #xE1)
  (define PID-IN    #x69)
  (define (TD-set-token! &addr MaxLen D EndPt Address PID)
    (assert (fx<=? 0 MaxLen 1280))
    (assert (fx<=? 0 D 1))
    (put-mem-u32 (fx+ &addr 8)
                  (bitwise-ior
                   (bitwise-arithmetic-shift-left
                    (fxand (fx- MaxLen 1) mask-TD2-TOKEN-MaxLen)
                    shift-TD2-TOKEN-MaxLen)
                   (fxior
                    (fxand TD2-TOKEN-D
                           (fxarithmetic-shift-left D shift-TD2-TOKEN-D))
                    (fxand TD2-TOKEN-EndPt
                           (fxarithmetic-shift-left EndPt shift-TD2-TOKEN-EndPt))
                    (fxand TD2-TOKEN-Address
                           (fxarithmetic-shift-left Address shift-TD2-TOKEN-Address))
                    (fxand TD2-TOKEN-PID
                           PID)))))
  (define (TD-D &addr)
    (let ((token (get-mem-u32 (fx+ &addr 8))))
      (fxand 1 (fxarithmetic-shift-right token shift-TD2-TOKEN-D))))
  (define (TD-maximum-length &addr)
    (let ((token (get-mem-u32 (fx+ &addr 8))))
      (fxand mask-TD2-TOKEN-MaxLen
             (fx+ 1 (bitwise-arithmetic-shift-right token shift-TD2-TOKEN-MaxLen)))))
  (define TD3-MASK #xFFFFFFFF)          ;valid address bits
  (define (TD-set-address! &addr data-physaddr)
    (put-mem-u32 (fx+ &addr 12) data-physaddr))
  (define (TD->list &addr)              ;just for debugging
    (assert &addr)
    (let* ((link (get-mem-u32 &addr))
           (sts (get-mem-u32 (fx+ &addr 4)))
           (token (get-mem-u32 (fx+ &addr 8))))
      `((Link ,(fxand link TD0-LINK-LP-mask)
              ,@(if (fxtest link TD0-LINK-Vf) '(Vf) '())
              ,@(if (fxtest link TD0-LINK-Q) '(Q) '())
              ,@(if (fxtest link TD0-LINK-T) '(T) '()))
        (CTRL ,@(if (fxtest sts TD1-CTRL-SPD) '(SPD) '())
              ,@(if (fxtest sts TD1-CTRL-LS) '(LS) '())
              ,@(if (fxtest sts TD1-CTRL-ISO) '(ISO) '())
              ,@(if (fxtest sts TD1-CTRL-IOC) '(IOC) '()))
        (STS ,@(if (fxtest sts TD1-STS-Active) '(Active) '())
             ,@(if (fxtest sts TD1-STS-Stalled) '(Stalled) '())
             ,@(if (fxtest sts TD1-STS-BufferErr) '(BufferErr) '())
             ,@(if (fxtest sts TD1-STS-Babble) '(Babble) '())
             ,@(if (fxtest sts TD1-STS-NAK) '(NAK) '())
             ,@(if (fxtest sts TD1-STS-CRC/Timeout) '(CRC/Timeout) '())
             ,@(if (fxtest sts TD1-STS-BitstuffErr) '(BitstuffErr) '())
             (ActLen ,(fx+ 1 (fxand sts TD1-STS-ActLen-mask))))
        (TOKEN (PID ,(let ((pid (fxbit-field token 0 8)))
                       (cond ((eqv? pid PID-OUT) 'OUT)
                             ((eqv? pid PID-IN) 'IN)
                             ((eqv? pid PID-SETUP) 'SETUP)
                             (else pid))))
               (Address ,(fxand #b1111111
                                (fxarithmetic-shift-right token shift-TD2-TOKEN-Address)))
               (EndPt ,(fxand #xf (fxarithmetic-shift-right token shift-TD2-TOKEN-EndPt)))
               (D ,(fxand 1 (fxarithmetic-shift-right token shift-TD2-TOKEN-D)))
               (MaxLen ,(TD-maximum-length &addr))))))

  ;; Check if a list of TDs have completed, possibly short. The TDs
  ;; must in the transfer order.
  ;;
  ;; Returns:
  ;; - active / complete / error
  ;; - the actual length of all used TDs, if complete (otherwise #f)
  ;; - the last TD used in the transfer
  (define (TD-list-status &td* &last-td &qh total-length)
    (load-fence)
    (let ((&short-td (QH-ELEMENT-td &qh)))
      (cond ((and (not &short-td) (not (TD-status-active? &last-td)))
             ;; Take a shortcut if the last TD is inactive and
             ;; there is no short packet.
             (if (TD-status-error? &last-td)
                 (values 'error #f #f)
                 (values 'complete total-length &last-td)))
            (else
             ;; There is a short packet: &short-td.
             (let lp ((&td* &td*) (sumlen 0))
               (assert (pair? &td*))
               (let* ((&td (car &td*))
                      (actlen (TD-actual-length &td))
                      (sumlen (fx+ sumlen actlen)))
                 (cond ((TD-status-active? &td)
                        (values 'active #f #f))
                       ((TD-status-error? &td)
                        (values 'error #f &td))
                       ((eqv? &td &short-td)
                        (values 'complete sumlen &short-td))
                       (else
                        (lp (cdr &td*) sumlen)))))))))

  ;; Queue Head. 16-byte alignment. Two u32 for the controller and two
  ;; reserved for the driver.
  (define QH-size 16)
  (define QH-address-mask #xFFFFFFF0)
  (define QH-HEAD-mask #xFFFFFFF0)    ;address of next object
  (define QH-HEAD-Q #b10)             ;1=QH, 0=TD
  (define QH-HEAD-T #b01)             ;1=Terminate (address not valid)
  (define (QH-HEAD-terminate! &addr)
    (put-mem-u32 &addr QH-HEAD-T))
  (define (QH-HEAD-link-queue! &addr &queue-physaddr)
    (store-fence)
    (put-mem-u32 &addr (fxior &queue-physaddr QH-HEAD-Q)))
  (define (QH-HEAD-link-queue &addr)
    (let ((x (get-mem-u32 &addr)))
      (and (not (fxzero? (fxand x QH-HEAD-Q)))
           (fxand x QH-HEAD-mask))))

  (define QH-ELEMENT-mask #xFFFFFFF0)   ;valid address bits
  (define QH-ELEMENT-Q #b10)            ;1=QH 0=TD
  (define QH-ELEMENT-T #b01)            ;1=Terminate (no valid queue entries)
  (define (QH-ELEMENT-terminate! &addr)
    (put-mem-u32 (fx+ &addr 4) QH-ELEMENT-T))
  (define (QH-ELEMENT-link-td! &addr &td-physaddr)
    (store-fence)
    (put-mem-u32 (fx+ &addr 4) &td-physaddr))
  (define (QH-ELEMENT-td &addr)
    (let ((x (get-mem-u32 (fx+ &addr 4))))
      (and (fxzero? (fxand x (fxior QH-ELEMENT-Q QH-ELEMENT-T)))
           (bitwise-and x QH-ELEMENT-mask))))
  (define (QH->list &addr)
    (let ((head (get-mem-u32 &addr))
          (element (get-mem-u32 (fx+ &addr 4))))
      `((HEAD ,(bitwise-and head QH-HEAD-mask)
              ,@(if (fxtest head QH-HEAD-Q) '(Q) '())
              ,@(if (fxtest head QH-HEAD-T) '(T) '()))
        (ELEMENT ,(bitwise-and element QH-ELEMENT-mask)
                 ,@(if (fxtest element QH-ELEMENT-Q) '(Q) '())
                 ,@(if (fxtest element QH-ELEMENT-T) '(T) '())))))

;;; Some memory management

  ;; TODO: Slab allcator. The waste here is amazing.

  (define (TD-allocate)
    (let ((&td (dma-allocate TD-size TD-address-mask)))
      (TD-link-terminate! &td)
      (TD-set-inactive! &td)
      (TD-set-address! &td 0)
      &td))

  (define (TD-free &td)
    (dma-free &td))

  (define (QH-allocate)
    (let ((&qh (dma-allocate QH-size QH-address-mask)))
      (QH-HEAD-terminate! &qh)
      (QH-ELEMENT-terminate! &qh)
      &qh))

  (define (QH-free &qh)
    (dma-free &qh))

;;; Driver state

  (define %num-ports 2)
  (define %original-SOF (SOFMOD-ref regs)) ;SOF determined by BIOS (1ms timing)
  (define %&frame-list #f)              ;address of the frame list
  (define %&queues #f)                  ;vector of skeleton queue heads

  (define &frame (dma-allocate 4096 #xFFFFF000))
  (define &queues (dma-allocate 4096 #xFFFFF000))

;;; Driver

  (define (millisleep ms)
    (sleep (/ ms 1000)))

  ;; Reset the controller and attached devices
  (define (global-reset)
    (USBCMD-set! regs CMD-GRESET)
    (millisleep 11)
    (USBCMD-set! regs 0))

  ;; True if a UHCI controller is detected.
  (define (detect-controller)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i 5))
      (global-reset))
    (and (fxzero? (USBCMD-ref regs))
         (fx=? (USBSTS-ref regs) STS-HCHalted)
         (begin
           (USBSTS-clear! regs)
           (and (fx=? (SOFMOD-ref regs) #x40) ;default SOFMOD
                (begin
                  ;; See if the controller resets bit 1.
                  (USBCMD-set! regs CMD-HCRESET)
                  (millisleep 50)
                  (fxzero? (fxand (USBCMD-ref regs) CMD-HCRESET)))))))

  (define (init-controller)
    (log/debug "Starting UHCI controller...")
    (USBINTR-set! regs (fxior INTR-Short INTR-IOC INTR-Resume INTR-Timeout/CRC))
    (FRNUM-set! regs #x0000)
    (%allocate-frame-list!)             ;set %&frame-list and %&queues
    (FRBASEADDR-set! regs %&frame-list)
    (SOFMOD-set! regs %original-SOF)
    (USBSTS-clear! regs)
    (USBCMD-set! regs (fxior CMD-CF CMD-RS CMD-MAXP))
    (reg-flush-writes))

  (define (uninit-controller)
    (log/debug "Stopping UHCI controller...")
    (global-reset)
    (millisleep 50)
    (SOFMOD-set! regs %original-SOF))

  ;; Indices into the %&queues vector
  (define idx-Q1 0)
  (define idx-Q2 1)
  (define idx-Q4 2)
  (define idx-Q8 3)
  (define idx-Q16 4)
  (define idx-Q32 5)
  (define idx-Q64 6)
  (define idx-Q128 7)
  (define idx-QLS 8)
  (define idx-QFS 9)
  (define idx-Qbug 10)

  ;; Inserts queue head between two existing queue heads.
  (define (schedule-insert-qh queue-index &new-qh)
    ;; Queue heads before are:  &A       -> &C
    ;; Afterwards they are:     &A -> &B -> &C
    (let* ((&A (vector-ref %&queues queue-index))
           (&B &new-qh)
           (&C (QH-HEAD-link-queue &A)))
      (assert &C)
      (QH-HEAD-link-queue! &B &C)
      (QH-HEAD-link-queue! &A &B)))

  ;; Deletes a queue head previously inserted with schedule-insert-qh.
  (define (schedule-delete-qh &qh)
    ;; Scan the schedule and remove &qh.
    ;; Queue heads before are:  &A -> &B -> &C
    ;; Afterwards they are:     &A       -> &C
    (assert (not (QH-ELEMENT-td &qh)))
    (let lp ((&A (vector-ref %&queues idx-Q128)))
      (cond ((QH-HEAD-link-queue &A) =>
             (lambda (&B)
               (if (fx=? &B &qh)
                   (cond ((QH-HEAD-link-queue &B) =>
                          (lambda (&C)
                            (QH-HEAD-link-queue! &A &C)))
                         (else
                          (QH-HEAD-terminate! &A)))
                   (lp &B))))
            (else
             (assertion-violation 'schedule-delete-qh
                                  "Queue head not in schedule" &qh)))))

  (define (%allocate-frame-list!)
    ;; Allocate addresses for the queue heads pointed to from the
    ;; frame list.
    (let ((Q1 (fx+ &queues (* 16 idx-Q1))) ;every 1ms
          (Q2 (fx+ &queues (* 16 idx-Q2))) ;every 2ms
          (Q4 (fx+ &queues (* 16 idx-Q4))) ;etc
          (Q8 (fx+ &queues (* 16 idx-Q8)))
          (Q16 (fx+ &queues (* 16 idx-Q16)))
          (Q32 (fx+ &queues (* 16 idx-Q32)))
          (Q64 (fx+ &queues (* 16 idx-Q64)))
          (Q128 (fx+ &queues (* 16 idx-Q128))) ;every 128ms
          ;; Control & bulk
          (QLS (fx+ &queues (* 16 idx-QLS)))
          (QFS (fx+ &queues (* 16 idx-QFS)))
          ;; Workaround for a known hardware bug
          (Qbug (fx+ &queues (* 16 idx-Qbug)))
          (&td (fx+ &queues (* 16 (+ idx-Qbug 1)))))
      (set! %&frame-list &frame)
      (set! %&queues (vector Q1 Q2 Q4 Q8 Q16 Q32 Q64 Q128 QLS QFS Qbug))

      ;; Initialize each queue head and element to T
      (vector-for-each (lambda (&queue)
                         (QH-HEAD-terminate! &queue)
                         (QH-ELEMENT-terminate! &queue))
                       %&queues)

      ;; Set up links between the queues. QFS & QLS are last and
      ;; appear in every frame. They are for control & bulk transfers.
      ;; The numbered queues are for interrupt transfers. Isochronous
      ;; TDs should be linked directly from the frame list.
      (QH-HEAD-link-queue! QLS Qbug)
      (QH-HEAD-link-queue! QFS QLS)
      (QH-HEAD-link-queue! Q1 QFS)
      (QH-HEAD-link-queue! Q2 Q1)
      (QH-HEAD-link-queue! Q4 Q2)
      (QH-HEAD-link-queue! Q8 Q4)
      (QH-HEAD-link-queue! Q16 Q8)
      (QH-HEAD-link-queue! Q32 Q16)
      (QH-HEAD-link-queue! Q64 Q32)
      (QH-HEAD-link-queue! Q128 Q64)

      ;; An inactive TD at the end of Qbug. Workaround for a hardware
      ;; bug.
      (TD-link-terminate! &td)
      (TD-set-inactive! &td)
      (TD-set-token! &td 0 0 0 0 0)
      (TD-set-address! &td 0)
      (QH-ELEMENT-link-td! Qbug &td)

      ;; Set up the frame list. This sets e.g. Q4 to be part of every
      ;; fourth frame, because it is either the first QH or linked
      ;; from there.
      (do ((i 0 (fx+ i 1)))
          ((fx=? i 1024))
        (FLP-link-queue! &frame i
                         (cond ((fx=? 0 (fxmod i 2)) Q1)
                               ((fx=? 1 (fxmod i 4)) Q2)
                               ((fx=? 3 (fxmod i 8)) Q4)
                               ((fx=? 7 (fxmod i 16)) Q8)
                               ((fx=? 15 (fxmod i 32)) Q16)
                               ((fx=? 31 (fxmod i 64)) Q32)
                               ((fx=? 63 (fxmod i 128)) Q64)
                               (else Q128))))))

  (define (reset-and-enable-port port)
    (log/debug "Resetting port " port)
    (PortSCn-set! regs port PortSC-Reset)
    (reg-flush-writes)
    (millisleep 50)
    (PortSCn-set! regs port 0)
    (reg-flush-writes)
    (let lp ((i 0))
      (millisleep 10)
      (let ((x (PortSCn-ref regs port)))
        (cond
          ((eqv? i 10)
           (log/error "Timed out trying to enable port " port)
           #f)
          ((eqv? 0 (fxand x PortSC-Connect))
           (log/debug "Nothing attached to port " port)
           #f)
          ((not (eqv? 0 (fxand x (fxior PortSC-Connect-Change PortSC-Enable-Change))))
           (PortSCn-set! regs port (fxior PortSC-Connect-Change PortSC-Enable-Change))
           (lp (fx+ i 1)))
          ((not (eqv? 0 (fxand x PortSC-Enable)))
           (log/debug "Port " port " enabled!")
           #t)
          (else
           (PortSCn-set! regs port PortSC-Enable)
           (lp (fx+ i 1)))))))

  (define (cleanup)
    (signal-cvar! shutdown-cvar)
    (uninit-controller)
    (dma-free &frame)
    (dma-free &queues))

;;; UHCI Root hub

  (define (fxtest a b) (not (eqv? 0 (fxand a b))))

  (define (fxtest-set-bit x PortSC-mask bit)
    (if (fxtest x PortSC-mask) (fxarithmetic-shift-left 1 bit) 0))

  (define (PortSCn->wPortStatus x)
    ;; Bit positions
    (define PORT_CONNECTION   0)
    (define PORT_ENABLE       1)
    (define PORT_SUSPEND      2)
    (define PORT_OVER_CURRENT 3)
    (define PORT_RESET        4)
    (define PORT_POWER        8)
    (define PORT_LOW_SPEED    9)
    (fxior (fxtest-set-bit x PortSC-Connect PORT_CONNECTION)
           (fxtest-set-bit x PortSC-Enable PORT_ENABLE)
           (fxtest-set-bit x PortSC-Suspend PORT_SUSPEND)
           (fxtest-set-bit x PortSC-Reset PORT_RESET)
           (fxtest-set-bit x PortSC-Low-Speed PORT_LOW_SPEED)
           (fxarithmetic-shift-left 1 PORT_POWER)))

  (define (PortSCn->wChangeStatus x)
    ;; Bit positions
    (define C_PORT_CONNECTION   0)
    (define C_PORT_ENABLE       1)
    (define C_PORT_SUSPEND      2)
    (define C_PORT_OVER_CURRENT 3)
    (define C_PORT_RESET        4)
    (fxior (fxtest-set-bit x PortSC-Connect-Change C_PORT_CONNECTION)
           (fxtest-set-bit x PortSC-Enable-Change C_PORT_ENABLE)
           ;; TODO: Implement these two properly
           #;C_PORT_SUSPEND
           (fxarithmetic-shift-left 1 C_PORT_RESET)))

  (define (handle-hub-request ch req)
    (match req
      ;; [('ClearHubFeature port) #f]
      ;; [('ClearPortFeature port) #f]
      ;; [('GetBusState port) #f]
      [#('GetHubDescriptor)
       ;; Create a hub descriptor for UHCI
       (let ((x (pack "<uCCCSCC CC" (format-size "<uCCCSCC CC")
                      #x29 %num-ports
                      ;; Ganged port power switching; not a compound
                      ;; device; global over-current protection
                      0
                      ;; Wait 10 ms for good power after power on
                      10/2 0 #xff #xff)))
         (put-message ch (cons 'ok x)))]
      ;; [('GetHubStatus port) #f]
      [#('GetPortStatus port)
       (if (not (fx<=? 1 port %num-ports))
           (put-message ch (cons 'fail 'bad-port))
           (let* ((port (fx- port 1))
                  (x (PortSCn-ref regs port)))
             (let ((wPortStatus (PortSCn->wPortStatus x))
                   (wChangeStatus (PortSCn->wChangeStatus x)))
               (put-message ch (cons 'ok (pack "<SS" wPortStatus wChangeStatus))))))]
      ;; [('SetHubDescriptor port) #f]
      ;; [('SetHubFeature port) #f]
      [#('SetPortFeature feature port)
       (if (not (fx<=? 1 port %num-ports))
           (put-message ch (cons 'fail 'bad-port))
           (let* ((port (fx- port 1))
                  (x (PortSCn-ref regs port))
                  (ok (case feature
                        [(PORT_RESET)
                         ;; TODO: keep track of "reset change" and do
                         ;; the reset in the background so the root
                         ;; hub is not blocked here
                         (reset-and-enable-port port)
                         #t]
                        [(PORT_SUSPEND)
                         (PortSCn-set! regs port (fxior PortSC-Suspend (fxand x (fxnot PortSC-R/WC-mask))))
                         (reg-flush-writes)
                         #t]
                        [else #f])))
             (put-message ch (if ok '(ok . #vu8()) '(fail . bad-request)))))]
      [#('ClearPortFeature feature port)
       (if (not (fx<=? 1 port %num-ports))
           (put-message ch (cons 'fail 'bad-port))
           (let* ((port (fx- port 1))
                  (x (PortSCn-ref regs port))
                  (ok (case feature
                        [(C_PORT_RESET)
                         ;; FIXME: See PORT_RESET above
                         #t]
                       [(PORT_SUSPEND)
                        (PortSCn-set! regs port (fxand x (fxnot (fxior PortSC-Suspend PortSC-R/WC-mask))))
                        #t]
                       [(PORT_ENABLE)
                        (PortSCn-set! regs port (fxand x (fxnot (fxior PortSC-Enable PortSC-R/WC-mask))))
                        #t]
                       [(C_PORT_CONNECTION)
                        (PortSCn-set! regs port (fxior PortSC-Connect-Change
                                                       (fxand x (fxnot PortSC-R/WC-mask))))
                        #t]
                       [(C_PORT_ENABLE)
                        (PortSCn-set! regs port (fxior PortSC-Enable-Change
                                                       (fxand x (fxnot PortSC-R/WC-mask))))
                        #t]
                       [else #f])))
             (reg-flush-writes)
             (put-message ch (if ok '(ok . #vu8()) '(fail . bad-request)))))]
      [_
       (put-message ch (cons 'fail 'unknown-req))]))

  (define (root-hub-task)
    (define (change-report)
      ;; Build a byte like the one sent in a hub interrupt endpoint
      (let lp ((port 0)
               (change-report 0))
        (if (fx=? port %num-ports)
            change-report
            (lp (fx+ port 1)
                (if (fxzero? (fxand (PortSCn-ref regs port) PortSC-Connect-Change))
                    change-report
                    (fxior change-report (fxarithmetic-shift-left 1 (fx+ port 1))))))))

    (define request-ch (usb-hub-request-channel root-hub))
    (define notify-ch (usb-hub-notify-channel root-hub))

    (let loop ((sleep-op (sleep-operation 1))
               (notify-op #f))
      (match (perform-operation
              (choice-operation
               (wrap-operation (wait-operation shutdown-cvar) (lambda _ 'shutdown))
               (wrap-operation (get-operation request-ch) (lambda (req) (cons 'req req)))
               (if notify-op
                   (wrap-operation notify-op (lambda _ 'notified))
                   (wrap-operation sleep-op (lambda _ 'sleep)))))
        ['sleep
         ;; Every interval we probe the ports for change indications
         ;; and prepare to send them. This is like the interrupt
         ;; endpoint on regular USB hubs.
         (let ((changes (change-report)))
           (log/debug "Changes: #b" (number->string changes 2))
           (loop (sleep-operation 1) (put-operation notify-ch changes)))]
        ['notified
         ;; Someone received our notification
         (loop sleep-op #f)]
        [('req ch . req)
         ;; (log/debug "Hub req: " req)
         (handle-hub-request ch req)
         (loop sleep-op notify-op)]
        ['shutdown #f])))

;;; Pipes

  (define *interrupt-cvar* (make-cvar))

  ;; Each pipe gets a queue head inserted in the schedule. They manage
  ;; the element pointer of the queue head and insert transfer
  ;; descriptors there.

  (define (interrupt-in-pipe-task dev epdesc speed pipe-ch &qh)
    ;; This pipe reads data from the device. This is handled with a
    ;; ring of transfer descriptors (TDs). The queue head (QH) element
    ;; points to one of the TDs, which are linked in a ring. When the
    ;; device has sent data, the head TD is set to inactive and the QH
    ;; element is set to the next TD in the ring. Our job is to try to
    ;; keep up with the TDs and ensure that the head element is active.
    (define low-speed? (eq? speed 'low))
    (define endpoint (epdesc-bEndpointAddress epdesc))
    (define address (usb-device-address dev))
    (define max-packet-size (epdesc-wMaxPacketSize epdesc))
    (define num-tds 8)
    (log/debug "Starting interrupt IN pipe task for endpoint " endpoint)
    (assert (even? num-tds))
    (let-values ([(&tdv &datav)
                  (let lp ((i 0) (&td* '()) (&data* '()))
                    (cond ((fx=? i num-tds)
                           (values (list->vector &td*) (list->vector &data*)))
                          (else
                           ;; FIXME: use a better allocator for the buffers
                           (let ((&td (TD-allocate))
                                 (&data (dma-allocate max-packet-size #xFFFFFFF0)))
                             (TD-set-control! &td low-speed? #f #t #f)
                             (TD-set-token!   &td max-packet-size (if (fxeven? i) DATA0 DATA1)
                                              endpoint address PID-IN)
                             (if (null? &td*)
                                 (TD-link-terminate! &td)
                                 (TD-link-td/horiz! &td (car &td*)))
                             (TD-set-address! &td &data)
                             (lp (fx+ i 1) (cons &td &td*) (cons &data &data*))))))])
      (define (cleanup)
        ;; Remove the queue head from the schedule and
        ;; deallocate everything.
        (QH-ELEMENT-terminate! &qh)
        (schedule-delete-qh &qh)
        (store-fence)
        (sleep 0.002)
        (QH-free &qh)
        (vector-for-each TD-free &tdv)
        (vector-for-each dma-free &datav))
      (guard (exn ((serious-condition? exn) (cleanup) (raise exn)))
        (let lp ((i 0))
          (define &td (vector-ref &tdv i))
          (define &data (vector-ref &datav i))
          (when (and (not (QH-ELEMENT-td &qh)) (TD-status-active? &td))
            (QH-ELEMENT-link-td! &qh &td))
          ;; Wait for the TD to become inactive
          (let poll ()
            (when (TD-status-active? &td)
              (wait *interrupt-cvar*)
              (poll)))
          ;; Parse the TD status and forward it to whoever is
          ;; interested.
          (let ((msg (let ((len (TD-actual-length &td)))
                       (if (or (TD-status-error? &td)
                               (fx>? len max-packet-size))
                           (begin
                             ;; FIXME: Do something better than just shutting down
                             (error 'interrupt-in-pipe-task "Transfer error" dev epdesc)
                             'error)     ;FIXME: better report
                           (let ((bv (make-bytevector len)))
                             (copy-memory-to-bytevector &data bv)
                             ;; XXX: Should include metadata about the transfer!
                             bv)))))
            ;; FIXME: also wait for the device shutdown, and
            ;; deallocate the resources.
            (put-message pipe-ch msg))
          ;; The HCI will have popped the TD from the QH. Reactivate
          ;; the TD and link it onto the end of the ring buffer.
          (when (eqv? &td (QH-ELEMENT-td &qh))
            (sleep 0.002))
          (when (eqv? &td (QH-ELEMENT-td &qh))
            (error 'interrupt-in-pipe-task "Controller stuck"))
          (TD-link-terminate! &td)
          (TD-set-control! &td low-speed? #f #t #f)
          (store-fence)
          (let ((&prev-td (vector-ref &tdv (fxmod (fx- i 1) num-tds))))
            (TD-link-td/horiz! &prev-td &td))
          (when (not (QH-ELEMENT-td &qh))
            ;; The pipe was not drained fast enough to keep up with
            ;; the device. If you run into this you should fix your
            ;; driver's or program's latency.
            (log/debug "Overflow on EP " (number->string endpoint 16)))
          (lp (fxmod (fx+ i 1) num-tds))))))

  (define (control-pipe-task dev epdesc _speed pipe-ch &qh)
    ;; Control transfers have three stages: setup, data and status.
    ;; The data stage is either in, out or absent.
    (define endpoint (epdesc-bEndpointAddress epdesc))
    (define address (usb-device-address dev))
    (define max-packet-size (epdesc-wMaxPacketSize epdesc))
    (define (cleanup)
      ;; Remove the queue head from the schedule and
      ;; deallocate everything.
      (QH-ELEMENT-terminate! &qh)
      (schedule-delete-qh &qh)
      (store-fence)
      (sleep 0.002)
      (QH-free &qh))
    (guard (exn ((serious-condition? exn) (cleanup) (raise exn)))
      ;; TODO: should also wait for device shutdown
      (let loop ()
        (match (get-message pipe-ch)
          [(resp-ch speed devreq bv timeout)
           ;; The speed is an argument because the control pipe for
           ;; device address 0 is reused over and over.
           (define low-speed? (eq? speed 'low))
           ;; A request to do a control transfer. A trusted procedure
           ;; built the device request. If there is a data stage then data
           ;; is either written to or read from the bytevector bv. The
           ;; response contains the actual length of the transfer and any
           ;; error conditions.
           ;; (log/debug "Control transfer: " (list devreq bv))
           (assert (fx=? (bytevector-length devreq) 8))
           (assert (fx>=? (bytevector-length bv) (devreq-wLength devreq)))
           (let* ((total-length (devreq-wLength devreq))
                  (&td0 (TD-allocate))
                  ;; &data holds the device request followed by the
                  ;; transfer payload.
                  (&devreq (dma-allocate (fx+ total-length 8) #xFFFFFFF0))
                  (&data (fx+ &devreq 8))
                  (PID (if (request-type:host->device? (devreq-bmRequestType devreq))
                           PID-OUT PID-IN)))
             (copy-bytevector-to-memory devreq &devreq)
             (when (eqv? PID PID-OUT)
               (copy-bytevector-to-memory bv &data))
             ;; Setup stage
             (TD-set-control! &td0 low-speed? #f #f #f)
             (TD-set-token!   &td0 (bytevector-length devreq) DATA0 endpoint address PID-SETUP)
             (TD-set-address! &td0 &devreq)
             ;; Data stage
             (let lp ((&prev &td0) (&td* (list &td0)) (i 0) (D DATA1))
               (cond ((fx<? i total-length)
                      ;; Append a transfer descriptor for part of the data
                      (let ((&td (TD-allocate))
                            (len (fxmin (fx- total-length i) max-packet-size)))
                        (TD-link-td! &prev &td)
                        (TD-set-control! &td low-speed? #f #f #f) ;FIXME: Short packet detect?
                        (TD-set-token!   &td len D endpoint address PID)
                        (TD-set-address! &td (fx+ &data i))
                        (lp &td (cons &td &td*) (fx+ i len) (fxxor D DATA1))))
                     (else
                      ;; Status stage
                      (let ((&td (TD-allocate)))
                        (TD-link-td!        &prev &td)
                        (TD-link-terminate! &td)
                        (TD-set-control!    &td low-speed? #f #t #f) ;IRQ
                        (TD-set-token!      &td 0 DATA1 endpoint address
                                            (if (or (eqv? 0 total-length)
                                                    (eqv? PID PID-OUT))
                                                PID-IN
                                                PID-OUT))
                        (TD-set-address!    &td 0)
                        (QH-ELEMENT-link-td! &qh &td0)
                        ;; Wait for the transfer to complete.
                        (let poll ()
                          (when (TD-status-active? &td)
                            (match (perform-operation
                                    (choice-operation
                                     (wrap-operation (wait-operation *interrupt-cvar*)
                                                     (lambda _ 'irq))
                                     ;; XXX: this doesn't actually count down properly
                                     (wrap-operation (sleep-operation (/ timeout 1000))
                                                     (lambda _ 'timeout))))
                              ['irq (poll)]
                              ['timeout
                               (let ((false-alarm? (not (TD-status-active? &td))))
                                 ((if false-alarm? log/warn log/error)
                                  (if false-alarm?
                                      (string-append "missing IRQ" (number->string irq))
                                      "timeout")
                                  " on control transfer to EP " (number->string endpoint 16)
                                  " request=" devreq;;  " device=" dev
                                  " status=" (reverse (map TD->list (cons &td &td*)))))])))
                        (QH-ELEMENT-terminate! &qh)
                        (store-fence)
                        (cond
                          ((TD-status-active? &td)
                           (put-message resp-ch 'error)
                           (sleep 0.001))
                          ((TD-status-error? &td)
                           (cond ((TD-stalled? &td)
                                  ;; Endpoints can stall to show they
                                  ;; don't support an operation.
                                  (put-message resp-ch 'stalled))
                                 (else
                                  (log/error "Transfer error: "
                                             (reverse (map TD->list (cons &td &td*))))
                                  (put-message resp-ch 'error))))
                          (else
                           (when (eqv? PID PID-IN)
                             (copy-memory-to-bytevector &data bv))
                           ;; FIXME: find the actual length
                           (put-message resp-ch total-length)))
                        (dma-free &devreq)
                        (dma-free &td)
                        (for-each TD-free &td*)
                        (loop))))))]))))

  (define (bulk-pipe-task dev epdesc speed pipe-ch &qh)
    ;; Bulk transfer of data from the device (IN) or from the host (OUT).
    (define endpoint (epdesc-bEndpointAddress epdesc))
    (define address (usb-device-address dev))
    (define max-packet-size (epdesc-wMaxPacketSize epdesc))
    (define low-speed? (eq? speed 'low))
    (define D DATA0) ;XXX: can be reset by ClearFeature ENDPOINT_HALT
    (define (cleanup)
      ;; Remove the queue head from the schedule and
      ;; deallocate everything.
      (QH-ELEMENT-terminate! &qh)
      (schedule-delete-qh &qh)
      (store-fence)
      (sleep 0.002)
      (QH-free &qh))
    (guard (exn ((serious-condition? exn) (cleanup) (raise exn)))
      ;; TODO: should also wait for device shutdown
      (let loop ()
        (match (get-message pipe-ch)
          [(resp-ch bv timeout)
           ;; A request to do a bulk transfer. A trusted procedure
           ;; built the request. Data is either written to or read
           ;; from the bytevector bv. The response contains the actual
           ;; length of the transfer and any error conditions.
           (assert (fx>? (bytevector-length bv) 0))
           (let* ((total-length (bytevector-length bv))
                  ;; &data holds the device request followed by the
                  ;; transfer payload. Round up to hold a multiple of
                  ;; the packet size.
                  (&data (dma-allocate (fx+ total-length max-packet-size) #xFFFFFFF0))
                  (PID (if (endpoint:host->device? endpoint) PID-OUT PID-IN)))
             (when (eqv? PID PID-OUT)
               (copy-bytevector-to-memory bv &data))
             (let lp ((&last-td #f) (&td0 #f) (&td* '()) (i 0) (D^ D))
               (cond ((fx<? i total-length)
                      ;; Append a transfer descriptor for part of the
                      ;; data. For an IN transfer, let the device send
                      ;; full packets.
                      (let ((&td (TD-allocate))
                            (len (if (eqv? PID PID-IN)
                                     max-packet-size
                                     (fxmin (fx- total-length i) max-packet-size))))
                        (when &last-td
                          (TD-link-td! &last-td &td))
                        (TD-set-control! &td low-speed? #f #t #t)
                        (TD-set-token!   &td len D^ endpoint address PID)
                        (TD-set-address! &td (fx+ &data i))
                        (lp &td (or &td0 &td) (cons &td &td*) (fx+ i len) (fxxor D^ DATA1))))
                     (else
                      (let ((&td* (reverse &td*)))
                        (QH-ELEMENT-link-td! &qh &td0)
                        ;; Wait for the transfer to complete.
                        (sleep 0.001)
                        (let poll ((timeout-passed? #f))
                          (let-values ([(status actual-length &last-used-td)
                                        (TD-list-status &td* &last-td &qh total-length)])
                            (case (if (and (eq? status 'active) timeout-passed?) 'timeout status)
                              ((active)
                               (match (perform-operation
                                       (choice-operation
                                        (wrap-operation (wait-operation *interrupt-cvar*)
                                                        (lambda _ 'irq))
                                        ;; FIXME: this doesn't actually count down properly
                                        (wrap-operation (sleep-operation (/ timeout 1000))
                                                        (lambda _ 'timeout))))
                                 ['irq (poll #f)]
                                 ['timeout (poll #t)]))
                              ((complete)
                               (when timeout-passed?
                                 (log/warn "a missing IRQ" irq
                                           " delayed a bulk transfer to EP "
                                           (number->string endpoint 16)))
                               (when (eqv? PID PID-IN)
                                 (copy-memory-to-bytevector &data bv))
                               (put-message resp-ch actual-length)
                               (set! D (fxxor (TD-D &last-used-td) DATA1)))
                              (else
                               (when (eq? status 'active)
                                 ;; The transfer is still ongoing, but
                                 ;; it timed out. Carefully remove it
                                 ;; from the queue.
                                 (QH-ELEMENT-terminate! &qh)
                                 (sleep 0.002))
                               (cond ((and &last-used-td (TD-stalled? &last-used-td))
                                      (log/debug "Stalled bulk transfer on EP "
                                                 (number->string endpoint 16)
                                                 " TDs=" (map TD->list &td*))
                                      (put-message resp-ch 'stalled))
                                     (else
                                      (log/error "Transfer error, status=" status
                                                 " TDs=" (map TD->list &td*))
                                      (put-message resp-ch 'error)))))))

                        ;; Transfer done, remove it from the queue and
                        ;; deallocate the memory.
                        (QH-ELEMENT-terminate! &qh)
                        (store-fence)
                        (dma-free &data)
                        (for-each TD-free &td*)
                        (loop))))))]))))

  (define (pipe-task dev epdesc speed pipe-ch &qh)
    (case (epdesc-transfer-type epdesc)
      ((control)
       (control-pipe-task dev epdesc speed pipe-ch &qh))
      ((interrupt)
       (if (endpoint:device->host? (epdesc-bEndpointAddress epdesc))
           (interrupt-in-pipe-task dev epdesc speed pipe-ch &qh)
           (error 'uhci "TODO: interrupt-out-pipe-task" dev epdesc speed pipe-ch &qh)))
      ((bulk)
       (bulk-pipe-task dev epdesc speed pipe-ch &qh))
      #;
      ((isochronous)
       )
      (else
       (error 'uhci "Internal error: invalid transfer type"
              (epdesc-transfer-type epdesc)))))

;;; Interrupt handling

  (define (interrupt-task irq-token)
    (let loop ()
      (match (perform-operation
              (choice-operation
               (wrap-operation (wait-operation shutdown-cvar) (lambda _ 'shutdown))
               (wrap-operation (wait-irq-operation irq-token) (lambda _ 'irq))))
        ['irq
         (let ((status (USBSTS-ref regs)))
           ;; (log/debug "IRQ: #b" (number->string status 2) "  " (FRNUM-ref regs))
           ;; Check for serious errors
           (when (fxtest status STS-HCPError)
             (signal-cvar! shutdown-cvar)
             (error 'driver·uhci "Host controller process error" controller status))
           (when (fxtest status STS-HSError)
             (signal-cvar! shutdown-cvar)
             (error 'driver·uhci "Host system error" controller status))

           (when (fxtest status STS-USBINT)
             ;; IOC or short packet detection -- wake all pipe tasks.
             (let ((old-cvar *interrupt-cvar*))
               (set! *interrupt-cvar* (make-cvar))
               (signal-cvar! old-cvar)))

           (unless (eqv? status 0)
             (USBSTS-clear! regs))

           (loop))]

        ['shutdown #f])))

;;; Main HCI loop

  (define ep0-dev0-pipe-ch #f)

  (define (handle-hci-request req)
    (match req

      [('setup-pipe ch dev epdesc speed)
       ;; Called from usb-setup-pipes. Should create a queue for the
       ;; endpoint, spawn a fiber to handle it and return the pipe.
       (let ((ep0-dev0 (and (eqv? 0 (usb-device-address dev))
                            (eqv? 0 (epdesc-bEndpointAddress epdesc)))))
         (cond
           ((and ep0-dev0 ep0-dev0-pipe-ch)
            ;; Endpoint 0 on device address 0 is used when setting up
            ;; devices. It makes sense to only have a single instance
            ;; of this task and never take it down.
            (put-message ch (cons 'ok ep0-dev0-pipe-ch)))
           (else
            (log/debug "Configuring pipe for endpoint #x"
                       (number->string (epdesc-bEndpointAddress epdesc) 16)
                       " on dev " dev)
            (let ((pipe-ch (make-channel))
                  (&qh (QH-allocate)))
              ;; Each endpoint gets its own queue head in the schedule. The
              ;; fiber manages the queue head and removes it when finished.
              (let ((Q (cond ((eq? (epdesc-transfer-type epdesc) 'interrupt) idx-Q1)
                             ((or (eq? speed 'low) ep0-dev0) idx-QLS)
                             (else idx-QFS))))
                (schedule-insert-qh Q &qh)
                (spawn-fiber (lambda () (pipe-task dev epdesc speed pipe-ch &qh))))
              (when ep0-dev0
                (set! ep0-dev0-pipe-ch pipe-ch))
              (put-message ch (cons 'ok pipe-ch))))))]

      [(req-type ch . _)
       (log/error "Unknown HCI request: " req-type)
       (put-message ch (cons 'fail 'unknown-req))]))

  (define root-hub (usb-controller-root-hub controller))
  (define request-ch (usb-controller-request-channel controller))
  (define notify-ch (usb-controller-notify-channel controller))

  (define TIMEOUT (sleep-operation 10)) ;DEBUGGING

  (unless (detect-controller)
    (error 'driver·uhci "The UHCI controller did not do its dance" regs))
  (init-controller)
  (let ((irq-token (enable-irq irq)))
    (guard (exn
            (else
             (log/error "UHCI driver crashed: " exn)
             (signal-cvar! shutdown-cvar)
             (cleanup)
             (raise exn)))
      (spawn-fiber root-hub-task)
      (spawn-fiber (lambda () (usb-enumerator controller shutdown-cvar)))
      (spawn-fiber (lambda () (interrupt-task irq-token)))
      (let loop ()
        (match (perform-operation
                (choice-operation
                 ;; (wrap-operation TIMEOUT (lambda _ '(stop)))
                 (wrap-operation (wait-operation shutdown-cvar)
                                 (lambda _ 'shutdown))
                 (wrap-operation (get-operation request-ch)
                                 (lambda (req) (cons 'req req)))))
          [('req . req)
           (handle-hci-request req)
           (loop)]
          ['shutdown #f]))

      (log/debug "UHCI driver stopping")
      (cleanup))))

;; Check that this is a device the driver supports
(define (probe·pci·uhci? dev)
  (and (eqv? (pcidev-base-class dev) #x0c)
       (eqv? (pcidev-sub-class dev) #x03)
       (eqv? (pcidev-interface dev) #x00)))

;; Main procedure for UHCI devices connected by PCI
(define (driver·pci·uhci dev controller)
  (let ((bar (vector-ref (pcidev-BARs dev) 4)))
    (assert (pcibar-i/o? bar))
    ;; Enable I/O (BAR0), bus mastering and unmask interrupts. Disable
    ;; the memory mapped registers.
    (pci-put-u8 dev PCI-CFG-COMMAND
                (fxand (fxior (fxior PCI-CMD-I/O-SPACE
                                     PCI-CMD-BUS-MASTER)
                              (pci-get-u8 dev PCI-CFG-COMMAND))
                       (fxnot (fxior PCI-CMD-INTERRUPT-DISABLE
                                     PCI-CMD-MEM-SPACE))))
    ;; Disable keyboard and mouse legacy support, enable PIRQ
    (pci-put-u16 dev #xC0 #x2000)
    (driver·uhci (pcibar-base bar)
                 (pcibar-size bar)
                 (pcidev-irq dev)
                 controller))))
