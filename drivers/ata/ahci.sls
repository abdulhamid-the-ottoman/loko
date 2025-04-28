;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; AHCI interface driver (Serial ATA host controller)

#|

This driver is missing support for port multipliers, enclosure
management, and hot plug. FPDMA is also a TODO item, along with better
error recovery.

The AHCI controller has a collection of registers called "ABAR".
There's a part for the controller and a set of registers per SATA
port (up to 32).

Each port has pointers to two structures in system memory: the command
list and the received FISes (one per port-multiplier port). The
command list is a list of up to 32 command headers, each of which
represents an ATA command and which points to a Command Table (CT).
The CT contains a command FIS, an ATAPI command and a Physical Region
Descriptor Table (PRDT). When sending a command to a device one of the
slots is used. Controllers only need to support a single command.

A "FIS" is a frame information structure. ATA commands and responses
are represented as ATA register sets in the FIS. This is for
historical reasons and goes all the way back to Cylinder, Head, Sector
and stuff like that. The IDE controller has one register set per port
exposed to the host, whereas the AHCI controller transfers these
through system memory instead, which allows the OS to do other tasks
while the controller handles the devices.

The PRDT is a table of addresses and lengths, plus a bit requesting an
interrupt on completion. Each entry must be an even number of
bytes (again for historical ATA reasons), up to 4MB.

Even though ports can have up to 32 command headers, traditional ATA
commands can only be issued one at a time. The READ/WRITE FPDMA QUEUED
commands (and UNLOAD) can be executed in parallel. These don't use the
regular FIS mechanism to give an ATA-8 ACS return, but instead send a
set-device-bits FIS to clear bits in the port's SACT register.
Charming.

|#

(library (loko drivers ata ahci)
  (export
    probe·pci·ahci?
    driver·pci·ahci)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system fibers)
    (loko system logging)
    (loko system unsafe)
    (loko system unsafe cache)
    (only (loko system $host)
          enable-irq wait-irq-operation
          dma-allocate dma-free)
    (loko drivers pci)
    (loko drivers ata core)
    (loko drivers ata sata))

(define development-mode #f)

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'ata-ahci))

(define (log/error . x*) (log/x ERROR x*))
(define (log/warn . x*) (log/x WARNING x*))
(define (log/debug . x*) (log/x DEBUG x*))
(define (log/info . x*) (log/x INFO x*))

(define (fxtest a b) (not (eqv? 0 (fxand a b))))

(define (probe·pci·ahci? dev)
  (and (eqv? (pcidev-base-class dev) #x01)
       (eqv? (pcidev-sub-class dev)  #x06)
       (eqv? (pcidev-interface dev)  #x01)
       (let ((BAR (vector-ref (pcidev-BARs dev) 5)))
         (and (pcibar-mem? BAR)
              (fx>=? (pcibar-size BAR) #x100)))))

;; HBA registers

(define HBA-CAP       #x00)
(define HBA-GHC       #x04)
(define HBA-IS        #x08)
(define HBA-PI        #x0C)
(define HBA-VS        #x10)
(define HBA-CCC_CTL   #x14)
(define HBA-CCC_PORTS #x18)
(define HBA-EM_LOC    #x1C)
(define HBA-EM_CTL    #x20)
(define HBA-CAP2      #x24)
(define HBA-BOHC      #x28)

(define CAP.S64A (expt 2 31))            ;supports 64-bit addresses
(define CAP.SNCQ (expt 2 30))            ;supports NCQ
(define CAP.NCS-shift 8)                 ;0-based, number of command slots
(define CAP.NCS-mask #b11111)
(define (CAP.NCS CAP)
  (fxand (fxarithmetic-shift-right CAP CAP.NCS-shift) CAP.NCS-mask))

(define CAP2.BOH (expt 2 0))            ;BIOS/OS Handoff

(define GHC.AE   #x80000000)            ;AHCI enable
(define GHC.MRSM #x00000004)            ;MSI revert to single message
(define GHC.IE   #x00000002)            ;interrupt enable
(define GHC.HR   #x00000001)            ;reset

;; Port registers. There are up to 32 sets of these.
(define (Px-base n) (fx+ #x100 (fx* #x80 n)))
(define PxCLB    #x00)                  ;command list base address
(define PxCLBU   #x04)                  ; --''-- upper 32 bits
(define PxFB     #x08)                  ;FIS base address
(define PxFBU    #x0C)                  ; --''-- upper 32 bits
(define PxIS     #x10)                  ;interrupt status
(define PxIE     #x14)                  ;interrupt enable
(define PxCMD    #x18)                  ;command
(define PxTFD    #x20)                  ;task file error & status (BSY, DRQ, ERR)
(define PxSIG    #x24)                  ;signature in first D2H register FIS
(define PxSSTS   #x28)                  ;power mgmt, speed negot., device detect
(define PxSCTL   #x2C)                  ;control of the above
(define PxSERR   #x30)                  ;diagnostics & error
(define PxSACT   #x34)                  ;active NCQ TAGs (RW1)
(define PxCI     #x38)                  ;command issue (RW1)
(define PxSNTF   #x3C)                  ;Set Device Bits FIS with Notification
(define PxFBS    #x40)                  ;FIS-based Switching Control
(define PxDEVSLP #x44)                  ;Device Sleep

(define PxCMD.ICC-shift  28)
(define PxCMD.ICC-mask   #b1111)
(define PxCMD.ASP   (expt 2 27))
(define PxCMD.ALPE  (expt 2 26))
(define PxCMD.DLAE  (expt 2 25))
(define PxCMD.ATAPI (expt 2 24))
(define PxCMD.APSTE (expt 2 23))
(define PxCMD.FBSCP (expt 2 22))
(define PxCMD.ESP   (expt 2 21))        ;External SATA Port
(define PxCMD.CPD   (expt 2 20)) ;Cold Presence Detection
(define PxCMD.MPSP  (expt 2 19)) ;Mechanical Presence Switch Attached to Port
(define PxCMD.HPCP  (expt 2 18)) ;Hot Plug Capable Port
(define PxCMD.PMA   (expt 2 17)) ;Port Multiplier Attached
(define PxCMD.CPS   (expt 2 16)) ;Cold Presence State
(define PxCMD.CR    (expt 2 15))        ;Command List Running
(define PxCMD.FR    (expt 2 14))        ;FIS Receive Running
(define PxCMD.MPSS  (expt 2 13))        ;Mechanical Presence Switch State
(define PxCMD.CCS-shift 8)              ;Current Command Slot
(define PxCMD.CCS-mask  #b11111)
(define PxCMD.FRE   (expt 2 4))         ;FIS Receive Enable
(define PxCMD.CLO   (expt 2 3))         ;Command List Override
(define PxCMD.POD   (expt 2 2))         ;Power On Device
(define PxCMD.SUD   (expt 2 1))         ;Spin-Up Device
(define PxCMD.ST    (expt 2 0))         ;Start

(define PxTFD.STS.BSY (expt 2 7))
(define PxTFD.STS.DRQ (expt 2 3))
(define PxTFD.STS.ERR (expt 2 0))

;; Interrupt status bits
(define PxIS.CPDS (expt 2 31))          ;Cold Port Detect Status
(define PxIS.TFES (expt 2 30))          ;Task File Error Status
(define PxIS.HBFS (expt 2 29))          ;Host Bus Fatal Error Status
(define PxIS.HBDS (expt 2 28))          ;Host Bus Data Error Status
(define PxIS.IFS  (expt 2 27))          ;Interface Fatal Error Status
(define PxIS.INFS (expt 2 26))          ;Interrupt Non-fatal Error Status
(define PxIS.OFS  (expt 2 24))          ;Overflow Status
(define PxIS.IPMS (expt 2 23))          ;Incorrect Port Multiplier Status
(define PxIS.PRCS (expt 2 22))          ;PhyRdy Change Status
(define PxIS.DMPS (expt 2 7))           ;Device Mechanical Presence Status
(define PxIS.PCS  (expt 2 6))           ;Port Connect Change Status (RO)
(define PxIS.DPS  (expt 2 5))           ;Descriptor Processed
(define PxIS.UFS  (expt 2 4))           ;Unknown FIS (RO)
(define PxIS.SDBS (expt 2 3))           ;Set Device Bits
(define PxIS.DSS  (expt 2 2))           ;DMA Setup FIS
(define PxIS.PSS  (expt 2 1))           ;PIO Setup FIS
(define PxIS.DHRS (expt 2 0))           ;Device->Host Register FIS

;; Interrupt enable bits
(define PxIE.CPDE (expt 2 31))
(define PxIE.TFEE (expt 2 30))
(define PxIE.HBFE (expt 2 29))
(define PxIE.HBDE (expt 2 28))
(define PxIE.IFE  (expt 2 27))
(define PxIE.INFE (expt 2 26))
(define PxIE.OFE  (expt 2 24))
(define PxIE.IPME (expt 2 23))
(define PxIE.PRCE (expt 2 22))
(define PxIE.DMPE (expt 2 7))
(define PxIE.PCE  (expt 2 6))
(define PxIE.DPE  (expt 2 5))
(define PxIE.UFE  (expt 2 4))
(define PxIE.SDBE (expt 2 3))
(define PxIE.DSE  (expt 2 2))
(define PxIE.PSE  (expt 2 1))
(define PxIE.DHRE (expt 2 0))

(define PxSSTS.DET-mask #b1111)
(define PxSSTS.DET-shift 0)

(define PxSSTS.DET/no-device             #x0)
(define PxSSTS.DET/presence-no-phy-comm  #x1)
(define PxSSTS.DET/presence-phy-comm     #x3)
(define PxSSTS.DET/offline               #x4)

(define PxSCTL.DET-mask #b1111)
(define PxSCTL.DET-shift 0)
(define PxSCTL.DET/normal      #x0)
(define PxSCTL.DET/initialize  #x1)
(define PxSCTL.DET/disable     #x4)

;; Offsets and lengths in the FIS descriptor.
(define FIS-DSFIS       #x00)                ;DMA Setup FIS
(define FIS-DSFIS-len   #x1C)
(define FIS-PSFIS       #x20)                ;PIO Setup FIS
(define FIS-PSFIS-len   #x14)
(define FIS-RFIS        #x40)                ;D2H Register FIS
(define FIS-RFIS-len    #x14)
(define FIS-SDBFIS      #x58)                ;Set Device Bits FIS
(define FIS-SDBFIS-len  #x8)
(define FIS-UFIS        #x60)                ;Unknown FIS
(define FIS-UFIS-len    #x40)

;; The command list structure contains command headers with the
;; following four 32-bit fields.
(define CH-flags        0)
(define CH-PRDBC        4)  ;byte count written by HBA
(define CH-CTBA         8)  ;command table base address
(define CH-CTBAU        12)  ;--''-- upper bits
(define CH-size         32)  ;size of an entry
(define CH-max-entries  32)  ;max number of entries

(define CH-flags-PRDTL-shift 16)        ;PRDT length
(define CH-flags-PRDTL-mask  #xffff)
(define CH-flags-PMP-shift   12)        ;Port Multiplier Port
(define CH-flags-PMP-mask    #b1111)
(define CH-flags-C           (expt 2 10)) ;Clear Busy upon R_OK
(define CH-flags-BIST        (expt 2 9))
(define CH-flags-R           (expt 2 8)) ;Reset
(define CH-flags-P           (expt 2 7)) ;Prefetchable
(define CH-flags-W           (expt 2 6)) ;Write
(define CH-flags-A           (expt 2 5)) ;ATAPI
(define CH-flags-CFL-shift   0)         ;Command FIS Length (u32s)
(define CH-flags-CFL-mask    #b1111)

;; Command table (128-byte alignment). Immediately followed by the PRDT.
(define CT-CFIS    #x00)               ;Command FIS
(define CT-ACMD    #x40)               ;ATAPI command (12 or 16 bytes)
(define CT-PRDT    #x80) ;Physical Region Descriptor Table (#xffff entries)
(define CT-size     128)                ;size of the table

;; Physical Region Descriptor Table entry
(define PRDT-DBA       0)
(define PRDT-DBAU      4)
(define PRDT-reserved  8)
(define PRDT-DBC      12)               ;0-based byte count
(define PRDT-size     16)


(define (driver·pci·ahci dev controller)
  (let* ((ABAR (vector-ref (pcidev-BARs dev) 5))
         (irq (pcidev-irq dev))
         (&hba-base (pcibar-base ABAR))
         (interrupt-cvars (make-vector 32 #f)))
    ;; Enable interrupts, memory access and bus mastering
    (pci-put-u16 dev PCI-CFG-COMMAND
                 (fxior (fxand (pci-get-u16 dev PCI-CFG-COMMAND)
                               (fxnot PCI-CMD-INTERRUPT-DISABLE))
                        (fxior PCI-CMD-MEM-SPACE
                               PCI-CMD-BUS-MASTER)))

    ;; Reset the controller
    (put-mem-u32 (fx+ &hba-base HBA-GHC) GHC.HR)
    (let lp ((i 10))
      (sleep 0.1)
      (when (fxtest (get-mem-u32 (fx+ &hba-base HBA-GHC)) GHC.HR)
        (when (eqv? i 0)
          (error 'driver·pci·ahci "Controller reset failed" dev controller))
        (lp (fx- i 1))))

    (put-mem-u32 (fx+ &hba-base HBA-GHC) (fxior GHC.AE))   ;AHCI Enable
    (let ((CAP (get-mem-u32 (fx+ &hba-base HBA-CAP)))
          (CAP2 (get-mem-u32 (fx+ &hba-base HBA-CAP2)))
          (PI (get-mem-u32 (fx+ &hba-base HBA-PI))))
      (log/debug "Found " (fxbit-count PI) " SATA ports with "
                 (fx+ 1 (CAP.NCS CAP)) " command slots each")
      (log/debug "CAP: " (number->string CAP 16))
      (log/debug "CAP2: " (number->string CAP2 16))
      (when (fxtest CAP2 CAP2.BOH)
        (log/warn "TODO: Perform BIOS/OS Handover"))
      ;; Each port has independent DMA engines and is a source of
      ;; interrupts, so each one gets a fiber.
      (do ((pi PI (fxand pi (fx- pi 1))))
          ((eqv? pi 0))
        (let ((n (fxfirst-bit-set pi)))
          (let ((&port-base (fx+ &hba-base (Px-base n)))
                (channel (make-channel)))
            (when (fx<=? &port-base (fx+ (pcibar-base ABAR) (pcibar-size ABAR)))
              (spawn-fiber
               (lambda ()
                 (driver·ahci-port n &port-base interrupt-cvars
                                   CAP channel controller)))))))
      ;; The HBA driver distributes the interrupts to the ports.
      (driver·ahci-hba &hba-base irq interrupt-cvars))))

;; The HBA part of the driver is mainly responsible for distributing
;; interrupts to ports. The HBA has one IRQ and the HBA-IS register
;; indicates which ports have an interrupt. Each port has its
;; interrupt status in its PxIS register, which is level triggered.
;; Here the status is buffered
(define (driver·ahci-hba &hba-base irq interrupt-cvars)
  (define (hba-set! reg v) (put-mem-u32 (fx+ &hba-base reg) v))
  (define (hba-ref reg) (get-mem-u32 (fx+ &hba-base reg)))
  (define (PxIS-ref n) (get-mem-u32 (fx+ (fx+ &hba-base (Px-base n)) PxIS)))
  (define (PxIE-ref n) (get-mem-u32 (fx+ (fx+ &hba-base (Px-base n)) PxIE)))
  (define (PxIE-set! n v) (put-mem-u32 (fx+ (fx+ &hba-base (Px-base n)) PxIE) v))
  (define irq-token (enable-irq irq))

  (hba-set! HBA-GHC (fxior GHC.AE GHC.IE))
  (hba-set! HBA-IS (hba-ref HBA-IS))
  (let lp ((n 0))
    (cond
      ((and development-mode (eqv? n 50))
       #f)
      (else
       (match (perform-operation
               (choice-operation
                (wrap-operation (wait-irq-operation irq-token) (lambda _ 'irq))
                (if development-mode
                    (wrap-operation (sleep-operation 10) (lambda _ 'timeout))
                    (choice-operation))))
         ['irq
          ;; IS is one bit per port. Signal all indicated ports and clear IS.
          (let ((IS (hba-ref HBA-IS)))
            (do ((IS IS (fxand IS (fx- IS 1))))
                ((eqv? IS 0))
              (let ((port-number (fxfirst-bit-set IS)))
                (when development-mode
                  (log/debug "AHCI IRQ port " port-number
                             " IE #x" (number->string (PxIE-ref port-number) 16)
                             " IS #x" (number->string (PxIS-ref port-number) 16)))

                ;; Mask interrupts for this port and wake the port
                ;; driver. It's up to the port driver to unmask
                ;; interrupts again. Interrupt sources are level
                ;; triggered based on PxIS and PxIE.
                (PxIE-set! port-number 0)
                (cond ((vector-ref interrupt-cvars port-number) =>
                       (lambda (cvar)
                         (vector-set! interrupt-cvars port-number #f)
                         (signal-cvar! cvar)))
                      (else
                       ;; Should never happen if the port driver does
                       ;; things in the right order.
                       (log/debug "Dropped AHCI interrupt for port " port-number)))))
            (hba-set! HBA-IS IS)
            (lp (if development-mode (+ n 1) n)))]
         ['timeout #f]))))
  (hba-set! HBA-GHC (fxand GHC.AE (fxnot GHC.IE)))
  (log/error "HBA driver exiting"))

;; Driver for a single AHCI port
(define (driver·ahci-port port-number &port-base interrupt-cvars CAP port-channel controller)
  (define (dma-alloc n mask)
    (dma-allocate n (if (fxtest CAP CAP.S64A) mask (fxbit-field mask 0 32))))

  (define (port-set! reg v) (put-mem-u32 (fx+ &port-base reg) v))
  (define (port-ref reg) (get-mem-u32 (fx+ &port-base reg)))

  (define (command-header-set! &clb CH-idx pm-port command-FIS-length flags PRDT-length &command-table)
    (let ((CFL (/ command-FIS-length 4)))
      (assert (fx<=? 0 CH-idx (fx- CH-max-entries 1)))
      (assert (fx<=? 0 PRDT-length CH-flags-PRDTL-mask))
      (assert (fx<=? 0 pm-port CH-flags-PMP-mask))
      (assert (and (fixnum? CFL) (fx<=? 2 CFL #x10)))

      (let ((&ch (fx+ &clb (fx* CH-idx CH-size))))
        (put-mem-u32 (fx+ &ch CH-flags)
                     (fxior (fxarithmetic-shift-left PRDT-length CH-flags-PRDTL-shift)
                            (fxarithmetic-shift-left pm-port CH-flags-PMP-shift)
                            flags
                            CFL))
        (put-mem-u32 (fx+ &ch CH-PRDBC) 0)
        (put-mem-u32 (fx+ &ch CH-CTBA) (fxbit-field &command-table 0 32))
        (put-mem-u32 (fx+ &ch CH-CTBAU) (fxarithmetic-shift-right &command-table 32))
        (store-fence))))

  (define (command-header-ref &clb CH-idx)
    (let ((&ch (fx+ &clb (fx* CH-idx CH-size))))
      (let ((prd-byte-count (get-mem-u32 (fx+ &ch CH-PRDBC))))
        prd-byte-count)))

  (define (command-table-FIS-copy! &command-table FIS)
    (do ((&base (fx+ &command-table CT-CFIS))
         (i 0 (fx+ i 4)))
        ((fx=? i (bytevector-length FIS)))
      (put-mem-u32 (fx+ &base i) (bytevector-u32-native-ref FIS i))))

  (define (command-table-CDB-copy! &command-table CDB) ;ATAPI, 12 or 14 bytes
    (do ((&base (fx+ &command-table CT-ACMD))
         (i 0 (fx+ i 4)))
        ((fx=? i (bytevector-length CDB)))
      (put-mem-u32 (fx+ &base i) (bytevector-u32-native-ref CDB i))))

  (define (copy-FIS->bytevector &command-table FIS-base len)
    (load-fence)
    (do ((FIS (make-bytevector len))
         (&addr (fx+ &command-table FIS-base) (fx+ &addr 4))
         (i 0 (fx+ i 4)))
        ((fx=? i len) FIS)
      (bytevector-u32-native-set! FIS i (get-mem-u32 &addr))))

  (define (command-table-PRDT-set! &command-table PRDT-idx &addr count interrupt?)
    (assert (fxeven? &addr))
    (assert (and (fxeven? count) (fx<? count (expt 2 22))))
    (let ((&prdt (fx+ &command-table (fx+ CT-PRDT (fx* PRDT-idx PRDT-size)))))
      (put-mem-u32 (fx+ &prdt PRDT-DBA) (fxbit-field &addr 0 32))
      (put-mem-u32 (fx+ &prdt PRDT-DBAU) (fxarithmetic-shift-right &addr 32))
      (put-mem-u32 (fx+ &prdt PRDT-reserved) 0)
      (put-mem-u32 (fx+ &prdt PRDT-DBC)
                   (fxior (if interrupt? (expt 2 31) 0)
                          (fx- count 1)))))

  ;; Initial port reset
  (define (ahci-port-reset)
    (when (fxtest (port-ref PxCMD) (fxior PxCMD.ST PxCMD.CR PxCMD.FRE PxCMD.FR))
      ;; Clear ST, wait for CR to clear
      (port-set! PxCMD (fxand (port-ref PxCMD) (fxnot PxCMD.ST)))
      (let lp ((n 6))
        (sleep 0.1)
        (when (fxtest (port-ref PxCMD) PxCMD.CR)
          (if (eqv? n 0)
              (error 'ahci-port-reset "The PxCMD.CR flag is not clearing"
                     port-number)
              (lp (fx- n 1)))))
      ;; FIXME: PxSCTL.DET='0h'?
      (when (fxtest (port-ref PxCMD) PxCMD.FRE)
        ;; Clear FRE, wait for FR to clear
        (port-set! PxCMD (fxand (port-ref PxCMD) (fxnot PxCMD.FRE)))
        (let lp ((n 6))
          (sleep 0.1)
          (when (fxtest (port-ref PxCMD) PxCMD.FR)
            (if (eqv? n 0)
                (error 'ahci-port-reset "The PxCMD.FR flag is not clearing"
                       port-number)
                (lp (fx- n 1))))))))

  ;; Command list & received FIS
  (define &clb (dma-alloc 1024 (fxnot 1023)))
  (define &fb (dma-alloc (* 256 16) (fxnot 4095))) ;one FIS receive spot per PM port
  (define bounce-length 8192)
  (define &buf (dma-alloc bounce-length (fxnot #b11))) ;bounce buffer
  (define PRDT-length 1)
  (define &command-table
    (dma-alloc (+ CT-size (* PRDT-length PRDT-size))
               (fxnot 127)))
  (define idle-PxIE
    (fxior   PxIE.CPDE   PxIE.TFEE   PxIE.HBFE   PxIE.HBDE   PxIE.IFE
             PxIE.INFE   PxIE.OFE    PxIE.IPME #;PxIE.PRCE   PxIE.DMPE
             PxIE.PCE  #;PxIE.DPE    PxIE.UFE    PxIE.SDBE   PxIE.DSE
           #;PxIE.PSE  #;PxIE.DHRE))

  (define (log-port)
    (let ((CMD (port-ref PxCMD)))
      (log/debug "Port " port-number
                 " CMD: #x" (number->string CMD 16)
                 "[ST=" (fxtest CMD PxCMD.ST)
                 ",CR=" (fxtest CMD PxCMD.CR)
                 ",CCS=" (fxand (fxarithmetic-shift-right CMD PxCMD.CCS-shift) PxCMD.CCS-mask)
                 ";FRE=" (fxtest CMD PxCMD.FRE)
                 ",FR=" (fxtest CMD PxCMD.FR)
                 "] IS: #x" (number->string (port-ref PxIS) 16)
                 " TFD: #x" (number->string (port-ref PxTFD) 16)
                 " SSTS: #x" (number->string (port-ref PxSSTS) 16)
                 " SERR: #x" (number->string (port-ref PxSERR) 16)
                 " SACT: #b" (number->string (port-ref PxSACT) 2)
                 " CI: #b" (number->string (port-ref PxCI) 2)
                 " SIG: #x" (number->string (port-ref PxSIG) 16))))

  (define (log-received-FIS n)
    (define (hexify &addr len)
      (call-with-string-output-port
        (lambda (p)
          (do ((i 0 (fx+ i 1)))
              ((fx=? i len))
            (let ((n (get-mem-u8 (fx+ &addr i))))
              (when (fx<? n #x10)
                (put-char p #\0))
              (put-string p (number->string n 16))
              (put-char p #\space))))))
    (let ((&addr (fx+ &fb (fx* n 256))))
      (log/debug "FIS" n " DSFIS=" (hexify (fx+ &addr FIS-DSFIS) FIS-DSFIS-len)
                 " PSFIS=" (hexify (fx+ &addr FIS-PSFIS) FIS-PSFIS-len)
                 " RFIS=" (hexify (fx+ &addr FIS-RFIS) FIS-RFIS-len)
                 " SDBFIS=" (hexify (fx+ &addr FIS-SDBFIS) FIS-SDBFIS-len)
                 " UFIS=" (hexify (fx+ &addr FIS-UFIS) FIS-UFIS-len))))

  (define (ahci-port-enable-interrupt IE)
    (unless (vector-ref interrupt-cvars port-number)
      (vector-set! interrupt-cvars port-number (make-cvar)))
    (port-set! PxIE IE))

  ;; Wait for the specified interrupts or a timeout.
  (define (wait-interrupt/timeout-op IE timeout)
    (ahci-port-enable-interrupt IE)
    (choice-operation
     (wrap-operation (wait-operation (vector-ref interrupt-cvars port-number))
                     (lambda _
                       ;; The HBA driver will have disabled all
                       ;; interrupt enable bits now. Let's clear the
                       ;; bits in PxIS that we are going to handle.
                       (let ((is (port-ref PxIS)))
                         (port-set! PxIS is) ;XXX: RWC register
                         (cons 'irq is))))
     (wrap-operation (sleep-operation timeout)
                     (lambda _ 'timeout))))

  (define (ahci-device-ready?)
    (and (eqv? (fxand (port-ref PxSSTS) PxSSTS.DET-mask)
               PxSSTS.DET/presence-phy-comm)
         (not (eqv? (port-ref PxSIG) #xFFFFFFFF))))

  (define device-notified? #f)

  (define (ahci-port-detection-stage)
    ;; Reset the port, ensuring that CR and FR are clear
    (ahci-port-reset)
    (log-port)
    (port-set! PxSERR #xFFFFFFFF)
    (port-set! PxCMD (fxior (port-ref PxCMD) PxCMD.POD)) ;power on drive
    (port-set! PxCMD (fxior (port-ref PxCMD) PxCMD.SUD)) ;spin up drive
    (port-set! PxSCTL PxSCTL.DET/initialize)
    (sleep 0.001)
    (port-set! PxSCTL PxSCTL.DET/normal)
    ;; Wait for PhyRdy change
    (port-set! PxIS (port-ref PxIS))
    (port-set! PxCMD (fxior (port-ref PxCMD) PxCMD.FRE)) ;receive FISes
    ;; (log/debug "Waiting for the device to be ready")
    (let lp ((n 30))
      ;; (log-port)
      (sleep 0.1)
      (unless (eqv? n 0)
        (unless (ahci-device-ready?)
          (match (perform-operation (wait-interrupt/timeout-op PxIE.PRCE 1))
            (('irq . is)
             ;; FIXME: check for errors
             #f)
            ('timeout 'timeout))
          (lp (- n 1)))))

    ;; If there is a device then drive the communication. Otherwise
    ;; wait for a device to appear.
    (cond ((ahci-device-ready?)
           (log/debug "SATA device ready")
           (unless device-notified?
             ;; There's a device here now, so let's notify the
             ;; controller. XXX: handle hot unplug and plugging in a
             ;; different device.
             (put-message (ata-controller-notify-channel controller)
                          (cons 'new-device port-channel))
             (set! device-notified? #t))
           (ahci-port-normal-stage))
          (else
           ;; TODO: Use the right interrupt instead
           #;
           (log/debug "No device on port " port-number)
           (sleep 30)
           (when (not development-mode)
             (ahci-port-detection-stage)))))

  (define (ahci-port-recover-stage)
    ;; TODO: Issue a reset and clear busy?
    ;; XXX: This is normal for the first ATAPI command.
    (log/debug "Starting error recovery")
    (ahci-port-detection-stage))

  ;; Create an ASC-8 style return based on the TFD. Missing the count
  ;; and lba fields, so maybe is not a good idea. The relevant data is
  ;; really in the proper FIS.
  (define (ahci-port-ASC-8-return)
    (let ((TFD (port-ref PxTFD)))
      (let ((error (fxbit-field TFD 8 16))
            (status (fxbit-field TFD 0 8))
            (count 0)
            (lba 0))
        (vector error count lba status))))

  ;; Perform any non-queued ATA/ATAPI command
  (define (ahci-port-perform-nonqueued cmd data data-len cdb command-header-flags)
    (define pm-port 0)
    ;; TODO: Used for NCQ. Must be the smaller of that supported by
    ;; the controller and the device. NCQ and non-NCQ commands can't
    ;; be in the command list at the same time.
    (define command-slot 0)

    (cond
      ((not (fx<=? 0 data-len bounce-length))
       (list 'error 'bad-request 'data-length))
      (else
       (when (fxtest command-header-flags CH-flags-W)
         ;; Copy the data into the bounce buffer
         (do ((i 0 (fx+ i 2)))
             ((fx=? i (bytevector-length data)))
           (put-mem-u16 (fx+ &buf i) (bytevector-u16-native-ref data i))))

       (let ((FIS (ata-command-FIS cmd pm-port))
             (data-len (fxand (fx+ data-len 1) -2))) ;round up to even
         (command-table-FIS-copy! &command-table FIS)
         (when (fxtest command-header-flags CH-flags-A)
           (command-table-CDB-copy! &command-table cdb))
         (unless (eqv? data-len 0)
           (command-table-PRDT-set! &command-table 0 &buf data-len 'interrupt))
         ;; TODO: set CH-flags-P when allowed
         (command-header-set! &clb command-slot pm-port (bytevector-length FIS)
                              command-header-flags
                              (if (eqv? data-len 0) 0 PRDT-length)
                              &command-table)
         (put-mem-u8 (fx+ &fb FIS-PSFIS) 0)
         (put-mem-u8 (fx+ &fb FIS-RFIS) 0)

         ;; Issue the command.
         (store-fence)
         (ahci-port-enable-interrupt (fxior idle-PxIE PxIE.PSE PxIE.DHRE))
         (port-set! PxCI (fxarithmetic-shift-left 1 command-slot))

         ;; Wait for completion
         (let ((is (let lp-irq ((i 10))
                     (cond
                       ((eqv? i 0)
                        (log/warn "Command timeout"))
                       (else
                        (match (perform-operation (wait-interrupt/timeout-op
                                                   (fxior idle-PxIE PxIE.PSE PxIE.DHRE)
                                                   1))
                          (('irq . is)
                           ;; FIXME: Handle all other PxIS bits here
                           (cond ((fxtest is (fxior PxIS.PSS PxIS.DHRS))
                                  is)
                                 (else
                                  (log-port)
                                  (lp-irq (fx- i 1)))))
                          ('timeout
                           (let ((is (port-ref PxIS)))
                             (port-set! PxIS is) ;FIXME: handle other interrupts
                             (log/debug "Timeout. IS: " (number->string is 16))
                             is))))))))

           ;; Handle the return
           (let* ((FIS (cond ((fxtest is PxIS.DHRS)
                              (bytevector->FIS (copy-FIS->bytevector &fb FIS-RFIS FIS-RFIS-len)))
                             ((fxtest is PxIS.PSS)
                              (bytevector->FIS (copy-FIS->bytevector &fb FIS-PSFIS FIS-PSFIS-len)))
                             (else #f)))
                  (resp (or (FIS->ATA8-ACS-return FIS)
                            (ahci-port-ASC-8-return))))
             (when development-mode
               (log-received-FIS pm-port)
               (log-port))
             (cond ((fxtest (port-ref PxTFD) PxTFD.STS.ERR)
                    (list 'ata-error resp))
                   ((fxtest command-header-flags CH-flags-W)
                    (list 'ok resp))
                   (else
                    (let ((resp-len (command-header-ref &clb command-slot)))
                      ;; Copy as many bytes as were received
                      (do ((ret (make-bytevector resp-len))
                           (len^ (fxand resp-len (fxnot #b11)))
                           (i 0 (fx+ i 4)))
                          ((fx=? i len^)
                           ;; Finish the trailing bytes
                           (do ((i i (fx+ i 1)))
                               ((fx=? i resp-len)
                                (list 'ok resp ret))
                             (bytevector-u8-set! ret i (get-mem-u8 (fx+ &buf i)))))
                        (bytevector-u32-native-set! ret i (get-mem-u32 (fx+ &buf i)))))))))))))

  (define (ahci-port-normal-stage)
    (log/debug "SATA port ready to go")
    (port-set! PxCMD (fxior (port-ref PxCMD) PxCMD.ST)) ;process the command list
    (port-set! PxSERR #xFFFFFFFF)

    (let lp ()
      (match (perform-operation (choice-operation
                                 (wait-interrupt/timeout-op idle-PxIE 5)
                                 (wrap-operation (get-operation port-channel)
                                                 (lambda (x) (cons 'req x)))))
        [('irq . is)
         (log/debug "TODO: Port " port-number " unexpected interrupt: #x" (number->string is 2))
         (lp)]

        ['timeout
         (if development-mode
             (log/debug "Port " port-number " timeout")
             (lp))]

        [('req resp-ch ('dma-data-in (? fixnum? data-len)) (? vector? cmd))
         (let ((response (ahci-port-perform-nonqueued cmd #vu8() data-len #f 0)))
           (put-message resp-ch response)
           (case (car response)
             ((ok) (lp))
             (else (ahci-port-recover-stage))))]

        [('req resp-ch ('dma-data-out (? bytevector? data)) (? vector? cmd))
         (let ((response (ahci-port-perform-nonqueued cmd data (bytevector-length data) #f CH-flags-W)))
           (put-message resp-ch response)
           (case (car response)
             ((ok) (lp))
             (else (ahci-port-recover-stage))))]

        [('req resp-ch ('pio-data-in (? fixnum? data-len)) (? vector? cmd))
         (let ((response (ahci-port-perform-nonqueued cmd #vu8() data-len #f 0)))
           (put-message resp-ch response)
           (case (car response)
             ((ok) (lp))
             (else (ahci-port-recover-stage))))]

        [('req resp-ch ('pio-data-out (? bytevector? data)) (? vector? cmd))
         (let ((response (ahci-port-perform-nonqueued cmd data (bytevector-length data) #f CH-flags-W)))
           (put-message resp-ch response)
           (case (car response)
             ((ok) (lp))
             (else (ahci-port-recover-stage))))]

        [('req resp-ch ('packet-in cdb data-len) (? vector? cmd))
         (let ((response (ahci-port-perform-nonqueued cmd #vu8() data-len cdb CH-flags-A)))
           (put-message resp-ch response)
           (case (car response)
             ((ok) (lp))
             (else (ahci-port-recover-stage))))]

        ;; XXX: untested
        [('req resp-ch ('packet-out-out cdb (? bytevector? data)) (? vector? cmd))
         (let ((response (ahci-port-perform-nonqueued cmd data (bytevector-length data) cdb
                                                      (fxior CH-flags-W CH-flags-A))))
           (put-message resp-ch response)
           (case (car response)
             ((ok) (lp))
             (else (ahci-port-recover-stage))))]

        [('req resp-ch ('packet-non-data cdb) (? vector? cmd))
         (match (ahci-port-perform-nonqueued cmd #vu8() 0 cdb CH-flags-A)
           [('ok resp _)
            (put-message resp-ch (list 'ok resp))
            (lp)]
           [response
            (put-message resp-ch response)
            (ahci-port-recover-stage)])]

        [('req resp-ch protocol cmd)
         (log/error "Bad ATA request: " (list 'req resp-ch protocol cmd))
         (put-message resp-ch (list 'error 'bad-request protocol cmd))
         (lp)])))

  ;; Set the pointers to the command list and the receive FIS area
  (port-set! PxCLB (fxbit-field &clb 0 32))
  (port-set! PxCLBU (bitwise-bit-field &clb 32 64))
  (port-set! PxFB (fxbit-field &fb 0 32))
  (port-set! PxFBU (bitwise-bit-field &fb 32 64))

  ;; Let's go
  (ahci-port-detection-stage)
  (ahci-port-reset)
  (dma-free &command-table)
  (dma-free &clb)
  (dma-free &fb)
  (log/error "Port " port-number " exiting")))
