#!/usr/bin/env -S loko --program
;; SPDX-License-Identifier: EUPL-1.2+
;; © 2021 G. Weinholt

;;; Hacked together PC emulator using Linux KVM.

;; Sorry about the mess!

(import
  (rnrs)
  (loko drivers pci)
  (loko system logging)
  (loko system fibers)
  (loko arch amd64 linux-syscalls)
  (loko arch amd64 linux-numbers)
  (loko system unsafe)
  (machine-code disassembler x86)

  (zabavno cpu x86)
  (prefix (zabavno hardware ich8) ich8:)

  (srfi :19)
  (yxskaft simple))

(define char-w 8)
(define char-h 15)
(define cols 80)
(define rows 25)
(define width (* cols char-w))
(define height (* rows char-h))
(define window (create-simple-window "Loko kvm-virt sample" width height))
(define &pixmap (simple-window-ptr window))
(define depth (simple-window-depth window))
(define bytes-per-line (simple-window-bytes-per-line window))

(spawn-fiber
 (lambda ()
   (let lp ()
     (render-simple-window window)
     (sleep 1/60)
     (lp))))

(define debugport
  (open-file-output-port "debug.txt" (file-options no-fail)))

(define logport
  (open-file-output-port "log.txt" (file-options no-fail)
                         (buffer-mode line)
                         (native-transcoder)))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*) (log/x DEBUG x*))
(define (log/error . x*) (log/x ERROR x*))
(define (log/info . x*) (log/x INFO x*))

(current-log-callback
 (lambda (e)
   (define p logport)
   (display "<" p)
   (display (cdr (assq 'SEVERITY e)) p)
   (display ">" p)
   (display (cdr (assq 'MESSAGE e)) p)
   (newline p)
   (flush-output-port logport)))

#;
(current-log-callback
 (lambda (e)
   (put-bytevector debugport
                   (string->utf8
                    (call-with-string-output-port
                      (lambda (p)
                        (display "<" p)
                        (display (cdr (assq 'SEVERITY e)) p)
                        (display ">" p)
                        (display (cdr (assq 'MESSAGE e)) p)
                        (newline p)))))
   (flush-output-port debugport)))


(define M (make-machine))
;(machine-debug-set! M #t)
(ich8:init M)

;; PCI configuration space
(let ()
  (define dummy-device
    (case-lambda
      ((reg size) (- (expt 2 size) 1))
      ((reg size value) #f)))

  (define %sel 0)
  (define %enable? #t)
  (define devices (make-eqv-hashtable))
  (define %selected-device dummy-device)

  (define (make-selector bus dev func)
    (fxior (fxarithmetic-shift-left bus 16)
           (fxarithmetic-shift-left dev 11)
           (fxarithmetic-shift-left func 8)))

  (define pci-addr
    (case-lambda
      ((_port _size) %sel)
      ((_port _size value)
       (set! %enable? (fxbit-set? value 31))
       (set! %sel (fxand (fxnot #b11) value))
       (set! %selected-device (hashtable-ref devices (fxand #x00ffff00 value)
                                             dummy-device)))))

  (define pci-data
    (case-lambda
      ((port size)
       (if (not %enable?)
           #f
           (let ((reg (fxior (fxand (fxnot #b11) (fxbit-field %sel 0 8))
                             (fxand port #b11))))
             (log/debug "PCI READ: " (list
                                      'reg (number->string reg 16)
                                      'size size
                                      'dev %selected-device))
             (let ((v (%selected-device reg size)))
               (cond ((fixnum? v) v)
                     (else
                      (case size
                        ((8) (bytevector-u8-ref v reg))
                        ((16) (bytevector-u16-ref v reg (endianness little)))
                        ((32) (bytevector-u32-ref v reg (endianness little))))))))))

      ((port size value)
       (when %enable?
         (let ((reg (fxior (fxand (fxnot #b11) (fxbit-field %sel 0 8))
                           (fxand port #b11))))
           (log/debug "PCI WRITE: " (list 'reg (number->string reg 16)
                                          'size size
                                          'dev %selected-device
                                          'value (number->string value 16)))
           (%selected-device reg size value))))))

  ;; Some memory controller or something
  (letrec ((memory-thing-pci
            (let ((regs (make-bytevector 260 0)))
              (bytevector-u16-set! regs PCI-CFG-VENDOR-ID #x8086 (endianness little))
              (bytevector-u16-set! regs PCI-CFG-DEVICE-ID #x1237 (endianness little))
              (bytevector-u16-set! regs PCI-CFG-00-SUBSYSTEM-VENDOR-ID #x1af4 (endianness little))
              (bytevector-u16-set! regs PCI-CFG-00-SUBSYSTEM-ID        #x1100 (endianness little))

              (case-lambda
                ((reg size) regs)
                ((reg size value) #f)))))
    (hashtable-set! devices (make-selector 0 0 0) memory-thing-pci))

  ;; VGA controller
  (letrec ((vga-pci
            (let ((regs (make-bytevector 260 0))
                  (rom-mapped? #f)
                  (bios (call-with-port (open-file-input-port "vgabios.bin")
                          (lambda (p)
                            (let ((bios (make-bytevector (* 64 1024) 0))
                                  (bv (get-bytevector-all p)))
                              (bytevector-copy! bv 0 bios 0 (bytevector-length bv))
                              bios)))))
              (define (log . x*) '(apply log/debug "VGA: " x*))
              (bytevector-u16-set! regs PCI-CFG-VENDOR-ID #x1234 (endianness little))
              (bytevector-u16-set! regs PCI-CFG-DEVICE-ID #x1111 (endianness little))
              (bytevector-u8-set! regs PCI-CFG-COMMAND (fxior PCI-CMD-I/O-SPACE
                                                              PCI-CMD-MEM-SPACE))
              (bytevector-u16-set! regs PCI-CFG-00-SUBSYSTEM-VENDOR-ID #x1af4 (endianness little))
              (bytevector-u16-set! regs PCI-CFG-00-SUBSYSTEM-ID        #x1100 (endianness little))
              (bytevector-u8-set! regs PCI-CFG-INTERFACE #x00) ;VGA controller
              (bytevector-u8-set! regs PCI-CFG-SUB-CLASS #x00)
              (bytevector-u8-set! regs PCI-CFG-BASE-CLASS #x03)  ;Display

              (let ((videomem (make-bytevector (+ 4 (* 256 1024)) 0)))
                (define map0 (fx* 0 (fx* 64 1024))) ;character map
                (define map1 (fx* 1 (fx* 64 1024))) ;attribute map
                (define map2 (fx* 2 (fx* 64 1024))) ;character font
                (define map3 (fx* 3 (fx* 64 1024)))
                (define (render-character row col)
                  (define background #x000000) ;FIXME: get from attribute
                  (define map-locations '#(0 2 4 6 1 3 5 7))
                  (define map-select (bytevector-u8-ref %sequencer-regs reg-SEQ-character-map))
                  (define map-select-a
                    (fx* (fx* 8 1024)
                         (vector-ref map-locations
                                     (fxior (fxarithmetic-shift-right (fxand map-select #b100000) 3)
                                            (fxarithmetic-shift-right (fxand map-select #b001100) 2)))))
                  (define map-select-b
                    (fx* (fx* 8 1024)
                         (vector-ref map-locations
                                     (fxior (fxarithmetic-shift-right (fxand map-select #b010000) 3)
                                            (fxand map-select #b000011)))))
                  (when (and (fx<? -1 row rows) (fx<? -1 col cols))
                    (let* ((video-idx (fx+ (fx* row cols) col))
                           (attr (bytevector-u8-ref videomem (fx+ map1 video-idx)))
                           (cc (bytevector-u8-ref videomem (fx+ map0 video-idx))))
                      ;; FIXME: use the graphics mode register, and the crtc register, etc
                      ;; FIXME: use map A if attr&0x08 and seq[4]&1
                      (let ((font-offset (fx+ (fx+ map2 map-select-b)
                                              (fx* cc 32)))
                            ;; FIXME: Bit 3 = Intensity / Font Select
                            (fg (dac-ref (fxbit-field attr 0 4)))
                            ;; FIXME: Bit 7 = Blink / Intensity
                            (bg (dac-ref (fxbit-field attr 4 8))))
                        (let ((bx (fx* char-w col))
                              (by (fx* char-h row)))
                          (do ((y 0 (fx+ y 1)))
                              ((fx=? y char-h))
                            (do ((font-line (bytevector-u8-ref videomem (fx+ font-offset y)))
                                 (x 0 (fx+ x 1)))
                                ((fx=? x char-w))
                              (put-pixel32 &pixmap
                                           (fx+ (fx* (fx+ by y) bytes-per-line)
                                                (fx* 4 (fx+ bx x)))
                                           (if (fxbit-set? font-line (fx- 7 x))
                                               fg
                                               bg)))))))))
                (define (re-render)
                  (do ((y 0 (fx+ y 1)))
                      ((fx=? y 25))
                    (do ((x 0 (fx+ x 1)))
                        ((fx=? x 80))
                      (render-character y x))))
                (define memory-hook
                  (case-lambda
                    ((address size)
                     (log "READ " address)
                     (if (fx=? #b10 (fxand %misc #b10))
                         (case size
                           ((8)
                            (let ((offset (fx- address %videomem-base))
                                  (map-mask (bytevector-u8-ref %sequencer-regs reg-SEQ-map-mask))
                                  (memory-mode (bytevector-u8-ref %sequencer-regs reg-SEQ-memory-mode)))

                              (if (fx<=? %videomem-base address (fx+ %videomem-base (fx- %videomem-size 1)))
                                  (cond ((not (fxbit-set? memory-mode 2))
                                         ;; Odd/even mode
                                         (let ((offset^ (fxarithmetic-shift-right offset 1)))
                                           (log "READ offset " offset "  MAPS #b" (number->string map-mask 2) " OFFSET " offset^
                                                " MEMORY MODE #b" (number->string memory-mode 2))
                                           (if (fxeven? offset)
                                               (bytevector-u8-ref videomem (fx+ map0 offset^))
                                               (bytevector-u8-ref videomem (fx+ map1 offset^)))))
                                        (else
                                         (log "SEQ READ offset " offset "  MAPS #b" (number->string map-mask 2)
                                              " MEMORY MODE #b" (number->string memory-mode 2))
                                         (if (fxbit-set? map-mask 0)
                                             (bytevector-u8-ref videomem (fx+ map0 offset))
                                             0)))
                                  0)))
                           ((16)
                            (fxior (memory-hook address 8)
                                   (fxarithmetic-shift-left (memory-hook (fx+ address 1) 8) 8)))
                           ((32)
                            (bitwise-ior (memory-hook address 8)
                                         (fxarithmetic-shift-left (memory-hook (fx+ address 1) 8) 8)
                                         (fxarithmetic-shift-left (memory-hook (fx+ address 2) 8) 16)
                                         (fxarithmetic-shift-left (memory-hook (fx+ address 3) 8) 24))))
                         0))
                    ((address size value)
                     ;; XXX: Accesses are realigned by Zabavno
                     (when (fx=? #b10 (fxand %misc #b10))
                       (let f ((address address) (size size) (value value))
                         ;; #xA0000-#xBFFFF
                         (case size
                           ((8)
                            (let ((offset (fx- address %videomem-base))
                                  (map-mask (bytevector-u8-ref %sequencer-regs reg-SEQ-map-mask))
                                  (memory-mode (bytevector-u8-ref %sequencer-regs reg-SEQ-memory-mode)))
                              (when (fx<=? %videomem-base address (fx+ %videomem-base (fx- %videomem-size 1)))
                                (cond ((not (fxbit-set? memory-mode 2))
                                       ;; Odd/even mode
                                       (let ((offset^ (fxarithmetic-shift-right offset 1)))
                                         (cond ((fxeven? offset)
                                                (let-values ([(row col) (fxdiv-and-mod offset^ 80)])
                                                  (when (fxbit-set? map-mask 0)
                                                    (log "WRITE " value " TO MAPS #b" (number->string map-mask 2) " OFFSET " offset^
                                                         " MEMORY MODE #b" (number->string memory-mode 2)
                                                         " CHAR " col "x" row)
                                                    (bytevector-u8-set! videomem (fx+ map0 offset^) value))
                                                  (render-character row col))
                                                (when (fxbit-set? map-mask 2)
                                                  (log "WRITE #b" (number->string value 2) " TO MAPS #b" (number->string map-mask 2) " OFFSET " offset^
                                                       " MEMORY MODE #b" (number->string memory-mode 2)
                                                       " FONT")
                                                  (bytevector-u8-set! videomem (fx+ map2 offset^) value)))
                                               (else
                                                (let-values ([(row col) (fxdiv-and-mod offset^ 80)])
                                                  (when (fxbit-set? map-mask 1)
                                                    (log "WRITE " value " TO MAPS #b" (number->string map-mask 2) " OFFSET " offset^
                                                         " MEMORY MODE #b" (number->string memory-mode 2)
                                                         " ATTR " col "x" row)
                                                    (bytevector-u8-set! videomem (fx+ map1 offset^) value))
                                                  (render-character row col))
                                                (let-values ([(row col) (fxdiv-and-mod offset^ 80)])
                                                  (when (fxbit-set? map-mask 3)
                                                    (log "WRITE " value " TO MAPS #b" (number->string map-mask 2) " OFFSET " offset^
                                                         " MEMORY MODE #b" (number->string memory-mode 2)
                                                         " MAP3")
                                                    (bytevector-u8-set! videomem (fx+ map3 offset^) value)))))))
                                      (else
                                       (log "SEQ WRITE " value " TO MAPS #b" (number->string map-mask 2) " OFFSET " offset
                                            " MEMORY MODE #b" (number->string memory-mode 2)
                                            " v: #b" (number->string value 2))
                                       (when (fxbit-set? map-mask 0)
                                         (bytevector-u8-set! videomem (fx+ map0 offset) value))
                                       (when (fxbit-set? map-mask 1)
                                         (bytevector-u8-set! videomem (fx+ map1 offset) value))
                                       (when (fxbit-set? map-mask 2)
                                         (bytevector-u8-set! videomem (fx+ map2 offset) value))
                                       (when (fxbit-set? map-mask 3)
                                         (bytevector-u8-set! videomem (fx+ map3 offset) value)))))))
                           ((16)
                            (f address 8 (fxand value #xff))
                            (f (fx+ address 1) 8 (fxarithmetic-shift-right value 8)))
                           ((32)
                            (f address 8 (bitwise-bit-field value 0 8))
                            (f (fx+ address 1) 8 (bitwise-bit-field value 8 16))
                            (f (fx+ address 2) 8 (bitwise-bit-field value 16 24))
                            (f (fx+ address 3) 8 (bitwise-bit-field value 24 32)))))))))
                ;; Miscellaneous output register
                (define %misc #b00000011)
                (define misc-output
                  (case-lambda
                    ((_addr _size)
                     (log "MISC READ #b" (number->string %misc 2))
                     %misc)
                    ((_addr _size v)
                     (set! %misc (fxand v #b11001111))
                     (log "MISC WRITE #b" (number->string %misc 2)))))

                ;; Sequencer
                (define reg-SEQ-map-mask #x02)
                (define reg-SEQ-character-map #x03)
                (define reg-SEQ-memory-mode #x04)
                (define %sequencer-addr 0)
                (define %sequencer-regs (make-bytevector 8 0))
                (define sequencer-addr
                  (case-lambda
                    ((_addr _size) %sequencer-addr)
                    ((_addr _size v) (set! %sequencer-addr (fxand v #b111)))))
                (define sequencer-data
                  (case-lambda
                    ((_addr _size)
                     (log "SEQ(" %sequencer-addr ") read " (bytevector-u8-ref %sequencer-regs %sequencer-addr))
                     (bytevector-u8-ref %sequencer-regs %sequencer-addr))
                    ((_addr _size v)
                     (log "SEQ(" %sequencer-addr ") write #b" (number->string v 2))
                     (bytevector-u8-set! %sequencer-regs %sequencer-addr (fxand v #xff)))))

                ;; CRT controller
                (define %crtc-addr 0)
                (define %crtc-regs (make-bytevector 32 0))
                (define crtc-addr
                  (case-lambda
                    ((_addr _size) %crtc-addr)
                    ((_addr _size v) (set! %crtc-addr (fxand v #b11111)))))
                (define crtc-data
                  (case-lambda
                    ((_addr _size)
                     (log "CRTC(" %crtc-addr ") read " (bytevector-u8-ref %crtc-regs %crtc-addr))
                     (bytevector-u8-ref %crtc-regs %crtc-addr))
                    ((_addr _size v)
                     (log "CRTC(" %crtc-addr ") write " (fxand v #xff))
                     (bytevector-u8-set! %crtc-regs %crtc-addr (fxand v #xff)))))

                ;; Graphics controller
                (define %videomem-base #xB8000)
                (define %videomem-size (* 32 1024))
                (define %graphics-addr 0)
                (define %graphics-regs (make-bytevector 16 0))
                (define graphics-addr
                  (case-lambda
                    ((_addr _size) %graphics-addr)
                    ((_addr _size v) (set! %graphics-addr (fxand v #b1111)))))
                (define graphics-data
                  (case-lambda
                    ((_addr _size)
                     (log "GRAPHICS(" %graphics-addr ") read " (bytevector-u8-ref %graphics-regs %graphics-addr))
                     (bytevector-u8-ref %graphics-regs %graphics-addr))
                    ((_addr _size v)
                     (log "GRAPHICS(" %graphics-addr ") write #x" (number->string v 16))
                     (case %graphics-addr
                       ((#x06)
                        (case (fxbit-field v 2 4)
                          ((#b00)
                           (set! %videomem-base #xA0000)
                           (set! %videomem-size (* 128 1024)))
                          ((#b01)
                           (set! %videomem-base #xA0000)
                           (set! %videomem-size (* 64 1024)))
                          ((#b10)
                           (set! %videomem-base #xB0000)
                           (set! %videomem-size (* 32 1024)))
                          (else
                           (set! %videomem-base #xB8000)
                           (set! %videomem-size (* 32 1024))))
                        (log "MM #b" (number->string (fxbit-field v 2 4) 2)
                             " OE " (fxbit-set? v 1)
                             " Graphics? " (fxbit-set? v 0)
                             " base #x" (number->string %videomem-base 16)
                             " size " (/ %videomem-size 1024) " KB")))
                     (bytevector-u8-set! %graphics-regs %graphics-addr v))))

                ;; Attribute controller
                (define %attribute-addr 0)
                (define %attribute-regs (make-bytevector 16 0))
                (define %attribute-address-selected? #t) ;flip-flop
                (define attribute-addr/data
                  (case-lambda
                    ((_addr _size) %attribute-addr)
                    ((_addr _size v)
                     (cond (%attribute-address-selected?   ;address
                            (set! %attribute-address-selected? #f)
                            (set! %attribute-addr (fxand v #b1111))
                            (log "Attribute address => #x" (number->string %attribute-addr 16)))
                           (else                           ;data
                            (set! %attribute-address-selected? #t)
                            (bytevector-u8-set! %attribute-regs %attribute-addr (fxand v #xff))
                            (log "Attribute(" (number->string %attribute-addr 16) ") => "
                                 (fxand v #xff)))))))
                (define attribute-data
                  (case-lambda
                    ((_addr _size)
                     (log "ATTRIBUTE(" %attribute-addr ") read "
                          (bytevector-u8-ref %attribute-regs %attribute-addr))
                     (bytevector-u8-ref %attribute-regs %attribute-addr))
                    ((_addr _size v)
                     'FIXME)))

                ;; Video DAC Palette
                (define (dac-ref c)
                  ;; FIXME: optimize
                  (let ((i (fx* c 3)))
                    (fxior (fx* 4 (fxarithmetic-shift-left
                                   (bytevector-u8-ref %dac-palette i)
                                   16))
                           (fx* 4 (fxarithmetic-shift-left
                                 (bytevector-u8-ref %dac-palette (fx+ i 1))
                                 8))
                           (fx* 4 (bytevector-u8-ref %dac-palette (fx+ i 2))))))
                (define %dac-palette (make-bytevector (* 3 256) 0))
                (define %dac-write-mode? #f)
                (define %dac-addr 0)
                (define %dac-pel-mask #xFF)
                (define dac-addr/write
                  (case-lambda
                    ((_addr _size) %dac-addr)
                    ((_addr _size value)
                     (log "DAC WRITE " value)
                     (set! %dac-write-mode? #t)
                     (set! %dac-addr (fx* value 3)))))
                (define dac-addr/read
                  (case-lambda
                    ((_addr _size)
                     (if %dac-write-mode? 0 #b11))
                    ((_addr _size value)
                     (log "DAC READ " value)
                     (set! %dac-write-mode? #f)
                     (set! %dac-addr (fx* value 3)))))
                (define dac-data
                  (case-lambda
                    ((_addr _size)
                     (let ((v (bytevector-u8-ref %dac-palette %dac-addr)))
                       (log "DAC(" %dac-addr ") read " (number->string v 16))
                       (set! %dac-addr (fxmod (fx+ %dac-addr 1) (* 3 256)))
                       v))
                    ((_addr _size v)
                     (log "DAC(" %dac-addr ") write #x" (number->string v 16))
                     (bytevector-u8-set! %dac-palette %dac-addr (fxand v #x3f))
                     (set! %dac-addr (fxmod (fx+ %dac-addr 1) (* 3 256))))))
                (define dac-pel-mask
                  (case-lambda
                    ((_addr _size)
                     %dac-pel-mask)
                    ((_addr _size value)
                     (set! %dac-pel-mask value))))

                ;; Input status register 1
                (define input-status-register-1
                  (case-lambda
                    ((_addr _size)
                     (set! %attribute-address-selected? #t)
                     0)
                    ((_addr _size value)
                     'FIXME)))

                (define (hook-i/o port proc)
                  ;; XXX: Old ISA systems only care about AD[9::0], so
                  ;; some old software might expect the I/O registers
                  ;; to be mirrored.
                  (machine-hook-i/o-port! M port 8 proc)
                  ;; Split 16-bit writes into two 8-bit writes at
                  ;; successive addresses.
                  (machine-hook-i/o-port! M port 16
                                          (case-lambda
                                            ((addr size)
                                             (proc addr size))
                                            ((addr size value)
                                             (proc addr 8 (fxand value #xff))
                                             (port-write M (fx+ addr 1) 8
                                                         (fxarithmetic-shift-right value 8))))))

                ;; #xA0000h to #xBFFFF
                (do ((addr #xA0000 (fx+ addr 4096)))
                    ((fx=? addr #xC0000))
                  (machine-hook-4k-page! M addr memory-hook))

                (hook-i/o #x3C4 sequencer-addr)
                (hook-i/o #x3C5 sequencer-data)
                ;; 3Bn when misc[0]=0, 3Dn otherwise
                (hook-i/o #x3B4 crtc-addr)
                (hook-i/o #x3B5 crtc-data)
                (hook-i/o #x3D4 crtc-addr)
                (hook-i/o #x3D5 crtc-data)
                (hook-i/o #x3CE graphics-addr)
                (hook-i/o #x3CF graphics-data)
                (hook-i/o #x3C0 attribute-addr/data)
                (hook-i/o #x3C1 attribute-data)
                (hook-i/o #x3C8 dac-addr/write)
                (hook-i/o #x3C7 dac-addr/read)
                (hook-i/o #x3C9 dac-data)
                (hook-i/o #x3C6 dac-pel-mask)

                (hook-i/o #x3CC misc-output) ;read
                (hook-i/o #x3C2 misc-output) ;write, read = input status register 0
                ;; 3Bn when misc[0]=0, 3Dn otherwise
                (hook-i/o #x3BA input-status-register-1)
                (hook-i/o #x3DA input-status-register-1)
                ;; Feature control: 3CA(read), 3BA/3DA(write)
                ;; Video subsystem enable: 3C3(read/write)

                )

              ;; Configuration space access
              (case-lambda
                ((reg size) regs)
                ((reg size value)
                 (cond
                   ((and (eqv? reg PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS)
                         (eqv? size 32))
                    ;; Request 64K for the VGA BIOS
                    (let ((value (fxand value (fxior 1 (fxxor (fx- (* 64 1024) 1) #xffffffff)))))
                      (bytevector-u32-set! regs reg value (endianness little))
                      (cond ((and (fxbit-set? value 0) (not rom-mapped?))
                             (set! rom-mapped? #t)
                             (let ((base-address (fxand value (fxnot 1))))
                               (log/debug "Mapping option ROM to #x" (number->string base-address 16))
                               ;; XXX: hardcoded 4
                               (kvm-set-user-memory-region 4 KVM_MEM_READONLY
                                                           base-address
                                                           vgabiosmem-size
                                                           &vgabiosmem)))
                            ((and (not (fxbit-set? value 0)) rom-mapped?)
                             (log/debug "Unmapping the VGA option ROM")
                             ;; XXX: hardcoded 4
                             (kvm-set-user-memory-region 4 0 0 0 0)))))))))))

    (hashtable-set! devices (make-selector 0 1 0) vga-pci))



  ;; IDE controller
  (let ()
    (define (make-identify-block)
      (define max-lba48 (+ 1 (div 32505856 512))) ;TODO: do not hard code
      (define c 503)
      (define h 2)
      (define s 63)
      (let ((block (make-bytevector 512)))
        (define (word-set! block word v)
          (bytevector-u16-set! block (fx* word 2) v (endianness little)))
        (define serial   (string->utf8 " 0                  "))
        (define revision (string->utf8 " 0      "))
        (define model    (string->utf8 "aZabnv o            "))
        ;; ATA device
        (word-set! block 0 (fxarithmetic-shift-left 1 15))
        ;; CHS
        (word-set! block 1 c)           ;cylinders
        (word-set! block 3 h)           ;headers
        (word-set! block 6 s)           ;sectors
        (bytevector-copy! serial 0 block (* 10 2) (bytevector-length serial))
        (bytevector-copy! revision 0 block (* 23 2) (bytevector-length revision))
        (bytevector-copy! model 0 block (* 27 2) (bytevector-length model))
        ;; Number of sectors (lba48)
        (bytevector-u64-set! block (fx* 100 2) max-lba48 (endianness little))
        (word-set! block 49 (fxarithmetic-shift-left 1 9)) ;lba
        (word-set! block 64 #x3)        ;PIO3-4
        (word-set! block 80 #b100000)   ;ATA/ATAPI-5
        (word-set! block 83 (fxarithmetic-shift-left 1 10)) ;lba48
        block))

    (define (ide-controller name reg-command-blk reg-control-blk reg-bus-master-blk irq)
      (define drive0-port (open-file-input/output-port "FD12LITE.img" (file-options no-truncate no-create)))

      (define (log . x*) '(apply log/debug name ": " x*))
      (define ata-device-LBA   #b01000000) ;use LBA
      (define ata-device-DRV   #b00010000) ;drive select
      (define ata-status-BSY   #b10000000) ;busy
      (define ata-status-DRDY  #b01000000) ;drive ready
      (define ata-status-DF    #b00100000) ;device fault
      (define ata-status-SERV  #b00010000) ;service
      (define ata-status-DRQ   #b00001000) ;data request
      (define ata-status-CORR  #b00000100) ;corrected data
      (define ata-status-IDX   #b00000010) ;index bit
      (define ata-status-ERR   #b00000001) ;error
      (define device-control-HOB   #b10000000)
      (define device-control-SRST  #b00000100)   ;software reset
      (define device-control-nIEN  #b00000010)   ;disable interrupts
      (define ata-cmd-execute-device-diagnostics #x90)
      (define ata-cmd-flush-cache                #xE7)
      (define ata-cmd-identify-device            #xEC)
      (define ata-cmd-identify-packet-device     #xA1)
      (define ata-cmd-packet                     #xA0)
      (define ata-cmd-read-dma                   #xC8)
      (define ata-cmd-read-dma-ext               #x25)
      (define ata-cmd-read-sectors               #x20)
      (define ata-cmd-set-features               #xEF)
      (define ata-cmd-set-multiple               #xC6)
      (define ata-cmd-write-dma                  #xCA)
      (define ata-cmd-write-dma-ext              #x35)
      (define ata-cmd-write-multiple             #xC3)
      (define ata-cmd-write-sectors              #x30)

      (define (interrupt)
        (when (eqv? 0 (fxand device-control-nIEN %device-control))
          (kvm-irq-line irq 1)
          (kvm-irq-line irq 0)))

      (define reg-cmd-data
        (case-lambda
          ((_addr size)
           ;; (log "read " size)
           (case size
             ((16)
              (let* ((b0 (get-u8 %data-to-guest))
                     (b1 (get-u8 %data-to-guest)))
                (if (or (eof-object? b0) (eof-object? b1))
                    0
                    (fxior (fxarithmetic-shift-left b1 8) b0))))
             ((32)
              (let* ((b0 (get-u8 %data-to-guest))
                     (b1 (get-u8 %data-to-guest))
                     (b2 (get-u8 %data-to-guest))
                     (b3 (get-u8 %data-to-guest)))
                (if (or (eof-object? b0) (eof-object? b1) (eof-object? b2) (eof-object? b3))
                    0
                    (fxior (fxarithmetic-shift-left b3 24)
                           (fxarithmetic-shift-left b2 16)
                           (fxarithmetic-shift-left b1 8)
                           b0))))
             (else 0)))
          ((_addr size value)
           (log "write " size)
           #f)))

      (define reg-cmd-error/feature
        (case-lambda
          ((_addr size)
           0)
          ((_addr size value)
           #f)))

      (define %blockp (make-bytevector 8 0))
      (define %block (make-bytevector 8 0))
      (define (drive-select)
        (if (eqv? 0 (fxand ata-device-DRV (bytevector-u8-ref %block 6)))
            0
            1))
      (define (lba?)
        (not (eqv? 0 (fxand ata-device-LBA (bytevector-u8-ref %block 6)))))
      (define reg-cmd
        (case-lambda
          ((addr size)
           (log "block read " (number->string (fx- addr reg-command-blk) 16) " => "
                (bytevector-u8-ref %block (fx- addr reg-command-blk)))
           (bytevector-u8-ref %block (fx- addr reg-command-blk)))
          ((addr size value)
           (log "block write " (number->string (fx- addr reg-command-blk) 16)
                " " (number->string value 16))
           (let ((reg (fx- addr reg-command-blk)))
             (bytevector-u8-set! %blockp reg (bytevector-u8-ref %block reg))
             (bytevector-u8-set! %block reg value)))))

      (define %data-to-guest (open-bytevector-input-port #vu8()))
      (define %error0 #f)
      (define reg-cmd-status/command
        (case-lambda
          ((_addr _size)                ;status
           (log "status "
                (if (eqv? 0 (drive-select))
                    (if (port-eof? %data-to-guest)
                        'no-data-ready
                        'ata-status-DRQ)
                    'wrong-drive))
           (if (eqv? 0 (drive-select))
               (fxior ata-status-DRDY
                      (if %error0 ata-status-ERR 0)
                      (if (port-eof? %data-to-guest) 0 ata-status-DRQ))
               0))
          ((_addr _size value)          ;command
           (log "COMMAND: " (number->string value 16))
           (when (eqv? 0 (drive-select))
             (cond ((eqv? value ata-cmd-read-sectors)
                    (log "read sectors: " %block " " %blockp)
                    (cond ((lba?)
                           (set! %error0 #f)
                           (let ((feature
                                  (fxior (fxarithmetic-shift-left (bytevector-u8-ref %blockp 1) 8)
                                         (bytevector-u8-ref %block 1)))
                                 (count
                                  (fxior (fxarithmetic-shift-left (bytevector-u8-ref %blockp 2) 8)
                                         (bytevector-u8-ref %block 2)))
                                 (lba
                                  (fxior (bytevector-u8-ref %block 3)
                                         (fxarithmetic-shift-left (bytevector-u8-ref %block 4) 8)
                                         (fxarithmetic-shift-left (bytevector-u8-ref %block 5) 16)
                                         (fxarithmetic-shift-left (bytevector-u8-ref %blockp 3) 24)
                                         (fxarithmetic-shift-left (bytevector-u8-ref %blockp 4) 32)
                                         (fxarithmetic-shift-left (bytevector-u8-ref %blockp 5) 40))))
                             (log "Read LBA: " lba " count: " count " feature: " feature)
                             ;; FIXME: This sure could be better!
                             (set-port-position! drive0-port (fx* lba 512))
                             (bytevector-fill! %block 0)
                             (bytevector-fill! %blockp 0)
                             (set! %data-to-guest (open-bytevector-input-port
                                                   (get-bytevector-n drive0-port (* count 512))))))
                          (else
                           ;; TODO: Handle CHS
                           (set! %error0 #t)))
                    (interrupt))
                   ((eqv? value ata-cmd-identify-device)
                    (log "identify-device")
                    (set! %error0 #f)
                    (set! %data-to-guest (open-bytevector-input-port
                                          (make-identify-block)))
                    (interrupt))
                   ((eqv? value ata-cmd-flush-cache))
                   (else
                    (set! %error0 #t)
                    (log "Unknown ATA command: " (number->string value 16)
                         " " %block %blockp))))
           #f)))

      (define %device-control device-control-nIEN)
      (define reg-ctl-alt/device-control
        (case-lambda
          ((_addr _size)                ;status
           (log "alt status")
           0)
          ((_addr _size value)          ;command
           (log "control " (number->string value 16))
           (set! %device-control value)
           (when (not (eqv? 0 (fxand value device-control-SRST)))
             ;; Reset
             (set! %error0 #f)
             (bytevector-fill! %block 0)
             (bytevector-fill! %blockp 0)
             (set! %data-to-guest (open-bytevector-input-port #vu8())))
           #f)))

      ;; Command block registers
      (machine-hook-i/o-port! M (fx+ reg-command-blk 0) 16 reg-cmd-data)
      (machine-hook-i/o-port! M (fx+ reg-command-blk 0) 32 reg-cmd-data)
      (machine-hook-i/o-port! M (fx+ reg-command-blk 1) 8 reg-cmd-error/feature)
      (do ((i 2 (fx+ i 1)))
          ((fx>? i 6))
        (machine-hook-i/o-port! M (fx+ reg-command-blk i) 8 reg-cmd))
      (machine-hook-i/o-port! M (fx+ reg-command-blk 7) 8 reg-cmd-status/command)
      ;; Control block registers
      (machine-hook-i/o-port! M (fx+ reg-control-blk 0) 8 reg-ctl-alt/device-control))

    (letrec ((ide-pci
              (let ((regs (make-bytevector 260 0)))
                (bytevector-u16-set! regs PCI-CFG-VENDOR-ID #x8086 (endianness little))
                (bytevector-u16-set! regs PCI-CFG-DEVICE-ID #x2820 (endianness little))
                (bytevector-u8-set! regs PCI-CFG-REVISION-ID #x02)
                (bytevector-u16-set! regs PCI-CFG-STATUS #x02B0 (endianness little))
                (bytevector-u8-set! regs PCI-CFG-INTERFACE  #b1010) ;FIXME: see datasheet
                (bytevector-u8-set! regs PCI-CFG-SUB-CLASS  #x01) ;IDE
                (bytevector-u8-set! regs PCI-CFG-BASE-CLASS #x01) ;mass storage
                ;; Configuration space
                (case-lambda
                  ((reg size) regs)
                  ((reg size value) #f)))))

      (ide-controller "IDE0" #x1f0 #x3f6 #f 14)
      ;; (ide-controller "IDE1" #x170 #x376 #f 15)

      (hashtable-set! devices (make-selector 0 2 0) ide-pci)))

  (machine-hook-i/o-port! M #xCF8 32 pci-addr)
  (for-each (lambda (size)
              (for-each (lambda (offset)
                          (machine-hook-i/o-port! M (fx+ #xCFC offset) size pci-data))
                        '(0 1 2 3)))
            '(8 16 32)))


(let ()
  (define %nmi-disable 0)
  (define %idx 0)
  (define %bank0 (make-bytevector 128 0))

  (define rtc-index
    (case-lambda
      ((_port _size) %idx)
      ((_port _size value)
       (set! %nmi-disable (fxand #x80 value))
       (set! %idx (fxand #x7f value)))))
  (define prev-sec 0)
  (define rtc-data
    (case-lambda
      ((_port _size)
       (define date (current-date))
       ;; TODO: Should should do this with irqfd/eventfd instead, with
       ;; resampler?
       (unless (or (fx=? prev-sec (date-second date))
                   (eqv? 0 (fxand (bytevector-u8-ref %bank0 #xb)
                                  #b00010000)))
         (log/debug "RTC interrupt")
         (kvm-irq-line 8 1)
         (kvm-irq-line 8 0)
         (bytevector-u8-set! %bank0 #x0C #b10010000)
         (set! prev-sec (date-second date)))
       (let ((v (bytevector-u8-ref %bank0 %idx))
             (encode-byte (lambda (b)
                            (if (eqv? 0 (fxand (bytevector-u8-ref %bank0 #xC) #b100))
                                (let-values ([(h l) (fxdiv-and-mod b 10)])
                                  (fxior (fx* h 16) l))
                                b))))
         (case %idx
           ((#x00) (encode-byte (date-second date)))
           ((#x02) (encode-byte (date-minute date)))
           ((#x04) (encode-byte (date-hour date)))
           ((#x08) (encode-byte (date-month date)))
           ((#x0C)
            (bytevector-u8-set! %bank0 #x0C 0)
            v)
           ((#x0D) (fxior (fxand v #b10111111) #x80))
           ((#x32) (encode-byte (div (date-year date) 100)))
           (else v))))
      ((_port _size value)
       (case %idx
         ((#x0B) (bytevector-u8-set! %bank0 %idx value))
         ((#x0C) #f)
         ((#x0F) (bytevector-u8-set! %bank0 %idx value))
         (else
          (bytevector-u8-set! %bank0 %idx value))))))

  ;; https://git.seabios.org/cgit/seabios.git/tree/src/hw/rtc.h
  ;; http://www.bioscentral.com/misc/cmosmap.htm
  ;; Intel's documentation in e.g. the ICH8 datasheet is good.
  (let* ((total-memory (* 32 1024 1024))
         (base-mem 640)
         (ext-mem (max 0 (min 65535 (div (- total-memory (* 1 1024 1024)) 1024))))
         (ext-mem2 (max 0 (min 65535 (fxarithmetic-shift-right
                                      (- total-memory (* 16 1024 1024))
                                      16))))
         (high-mem 0))
    (bytevector-u16-set! %bank0 #x15 base-mem (endianness little))
    (bytevector-u16-set! %bank0 #x17 ext-mem (endianness little))
    (bytevector-u16-set! %bank0 #x30 ext-mem (endianness little))
    (bytevector-u16-set! %bank0 #x34 ext-mem2 (endianness little))
    (bytevector-u32-set! %bank0 #x5b high-mem (endianness little))
    )
  (bytevector-u8-set! %bank0 #x0B #b010) ;bcd, 24 hour
  (bytevector-u8-set! %bank0 #x14 #b111) ;FPU & PS/2 mouse

  (machine-hook-i/o-port! M #x70 8 rtc-index)
  (machine-hook-i/o-port! M #x71 8 rtc-data))


(let ()
  (define debugcon
    (case-lambda
      ((_port _size) #xE9)
      ((_port _size value)
       (put-u8 debugport value)
       (flush-output-port debugport))))
  (machine-hook-i/o-port! M #xE9 8 debugcon)
  (machine-hook-i/o-port! M #x402 8 debugcon))

(define PS/2-data '())
(define (PS/2-enqueue-byte byte)
  (set! PS/2-data (reverse (cons byte (reverse PS/2-data))))
  ;; FIXME: Only if enabled
  (kvm-irq-line 1 1))
(let ()
  (define %status 0)
  (define %data 0)
  (define STS-OUTPUT-BUFFER-FULL #b00000001)
  (define STS-INPUT-BUFFER-FULL  #b00000010)
  (define STS-SYSTEM-FLAG        #b00000100)
  (define STS-COMMAND/DATA       #b00001000)
  (define STS-AUX                #b00100000)
  (define STS-TIMEOUT            #b01000000)
  (define STS-PARITY             #b10000000)
  (define i8042-data
    (case-lambda
      ((_port _size)
       (if (null? PS/2-data)
           0
           (let ((data (car PS/2-data)))
             (set! PS/2-data (cdr PS/2-data))
             (cond ((null? PS/2-data)
                    (kvm-irq-line 1 0))
                   (else
                    (kvm-irq-line 1 0)
                    (kvm-irq-line 1 1)))
             data)))
      ((_port _size value)
       #f)))
  (define i8042-status/command
    (case-lambda
      ((_port _size)
       (if (null? PS/2-data)
           (fxand %status (fxnot #b00000001))
           (fxior %status STS-OUTPUT-BUFFER-FULL)))
      ((_port _size value)
       (log/debug "i8042 command: " (number->string value 16))
       (case value
         ((#xAA)
          (set! PS/2-data '(#x55)))
         ((#xAB)
          (set! PS/2-data '(#x00)))))))
  (machine-hook-i/o-port! M #x60 8 i8042-data)
  (machine-hook-i/o-port! M #x64 8 i8042-status/command))

;;; KVM setup and main loop

(define NULL 0)

(define regions '())

(define (kvm-set-user-memory-region slot flags guest_phys_addr memory_size userspace_addr)
  (let ((reg (make-bytevector sizeof-kvm_userspace_memory_region)))
    (bytevector-u32-native-set! reg offsetof-kvm_userspace_memory_region-slot slot)
    (bytevector-u32-native-set! reg offsetof-kvm_userspace_memory_region-flags flags)
    (bytevector-u64-native-set! reg offsetof-kvm_userspace_memory_region-guest_phys_addr guest_phys_addr)
    (bytevector-u64-native-set! reg offsetof-kvm_userspace_memory_region-memory_size memory_size)
    (bytevector-u64-native-set! reg offsetof-kvm_userspace_memory_region-userspace_addr userspace_addr)
    (sys_ioctl vm-fd KVM_SET_USER_MEMORY_REGION (bytevector-address reg))
    (set! regions (cons (list guest_phys_addr memory_size userspace_addr)
                        regions))))

(define (kvm-enable-cap cap)
  (let ((bv (make-bytevector sizeof-kvm_enable_cap 0)))
    (bytevector-u32-native-set! bv offsetof-kvm_enable_cap-cap cap)
    (sys_ioctl kvm-fd KVM_ENABLE_CAP (bytevector-address bv))))

;; XXX: Not used if the pic is in the kernel
(define (kvm-interrupt irq)
  (let ((bv (make-bytevector sizeof-kvm_interrupt 0)))
    (bytevector-u32-native-set! bv offsetof-kvm_interrupt-irq irq)
    (sys_ioctl vm-fd KVM_INTERRUPT (bytevector-address bv))))

(define (kvm-irq-line irq level)
  (let ((bv (make-bytevector sizeof-kvm_irq_level 0)))
    (bytevector-u32-native-set! bv offsetof-kvm_irq_level-irq irq)
    (bytevector-u32-native-set! bv offsetof-kvm_irq_level-level level)
    (sys_ioctl vm-fd KVM_IRQ_LINE (bytevector-address bv))))

(define (kvm-get-irqchip chip-id)
  (let ((bv (make-bytevector sizeof-kvm_irqchip 0)))
    (bytevector-u32-native-set! bv offsetof-kvm_irqchip-chip_id chip-id)
    (sys_ioctl vm-fd KVM_GET_IRQCHIP (bytevector-address bv))
    bv))

(define (kvm-set-irqchip bv)
  (assert (fx=? (bytevector-length bv) sizeof-kvm_irqchip))
  (sys_ioctl vm-fd KVM_SET_IRQCHIP (bytevector-address bv)))

(define kvm-fd
  (let ((fname-bv (string->utf8 "/dev/kvm\x0;")))
    (sys_openat AT_FDCWD (bytevector-address fname-bv)
                (fxior O_RDWR O_NONBLOCK O_CLOEXEC)
                0)))

(let ((kvm-version (sys_ioctl kvm-fd KVM_GET_API_VERSION 0)))
  (unless (eqv? kvm-version KVM_API_VERSION)
    (error 'kvm "Wrong KVM version" kvm-version)))

(define vm-fd (sys_ioctl kvm-fd KVM_CREATE_VM 0))

(sys_ioctl vm-fd KVM_SET_TSS_ADDR #xfffbd000)

;; FIXME: sys_mmap on Loko should really use specific addresses

(define lowmem-addr 0)
(define lowmem-size #xA0000)
(define &lowmem                          ;0-9FFFF
  (sys_mmap NULL lowmem-size
            (fxior PROT_READ PROT_WRITE)
            (fxior MAP_PRIVATE MAP_ANONYMOUS MAP_NORESERVE)
            -1
            0))
(sys_madvise &lowmem lowmem-size MADV_MERGEABLE)

(define shadowmem-addr #xC0000)
(define shadowmem-size (- #x100000 #xC0000))
(define &shadowmem                       ;C0000-100000
  (sys_mmap NULL shadowmem-size
            (fxior PROT_READ PROT_WRITE)
            (fxior MAP_PRIVATE MAP_ANONYMOUS MAP_NORESERVE)
            -1
            0))
(sys_madvise &shadowmem shadowmem-size MADV_MERGEABLE)

(define highmem-addr (* 1024 1024))
(define highmem-size (* 32 1024 1024))
(define &highmem                         ;1M-32M
  (sys_mmap NULL highmem-size
            (fxior PROT_READ PROT_WRITE)
            (fxior MAP_PRIVATE MAP_ANONYMOUS MAP_NORESERVE)
            -1
            0))
(sys_madvise &highmem highmem-size MADV_MERGEABLE)

(define bios-fd
  (let ((fname-bv (string->utf8 "bios.bin\x0;")))
    (sys_openat AT_FDCWD (bytevector-address fname-bv) (fxior O_RDONLY O_CLOEXEC) 0)))
(define biosmem-addr #xFFFE0000)
(define biosmem-size (* 128 1024))
(define &biosmem
  (sys_mmap NULL biosmem-size
            PROT_READ (fxior MAP_SHARED)
            bios-fd
            0))

(define vgabios-fd
  (let ((fname-bv (string->utf8 "vgabios.bin\x0;")))
    (sys_openat AT_FDCWD (bytevector-address fname-bv) (fxior O_RDONLY O_CLOEXEC) 0)))
(define vgabiosmem-addr #f)             ;Option ROM address selected via PCI
(define vgabiosmem-size (* 64 1024))
(define &vgabiosmem
  (sys_mmap NULL vgabiosmem-size
            PROT_READ (fxior MAP_SHARED)
            vgabios-fd
            0))

(call-with-port (open-file-input-port "bios.bin")
  (lambda (p)
    (let ((bv (get-bytevector-all p)))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i (bytevector-length bv)))
        (put-mem-u8 (fx+ (fx+ &shadowmem #x20000) i) (bytevector-u8-ref bv i))))))

(kvm-set-user-memory-region 0 0                lowmem-addr    lowmem-size     &lowmem)
(kvm-set-user-memory-region 1 0                shadowmem-addr shadowmem-size  &shadowmem)
(kvm-set-user-memory-region 2 0                highmem-addr   highmem-size    &highmem)
(kvm-set-user-memory-region 3 KVM_MEM_READONLY biosmem-addr   biosmem-size    &biosmem)
;; 4 is for the VGA option ROM

;; XXX: These override our own emulation!
(sys_ioctl vm-fd KVM_CREATE_IRQCHIP 0)
(let ((bv (make-bytevector sizeof-kvm_pit_config 0)))
  (sys_ioctl vm-fd KVM_CREATE_PIT2 (bytevector-address bv)))


(define cpu-fd (sys_ioctl vm-fd KVM_CREATE_VCPU 0))

(define vcpu-mmap-size (sys_ioctl kvm-fd KVM_GET_VCPU_MMAP_SIZE 0))

(define vcpu-kvm-run                    ;dma-allocate ?
  (sys_mmap NULL vcpu-mmap-size (fxior PROT_READ PROT_WRITE) MAP_SHARED cpu-fd 0))

;; Processor state after reset, according to AMD, "14.1.3 Processor
;; Initialization State". Hmm.
(define sregs (make-bytevector sizeof-kvm_sregs 0))
(sys_ioctl cpu-fd KVM_GET_SREGS (bytevector-address sregs))
(bytevector-u16-native-set! sregs (fx+ offsetof-kvm_sregs-cs offsetof-kvm_segment-selector) #xF000)
(bytevector-u64-native-set! sregs (fx+ offsetof-kvm_sregs-cs offsetof-kvm_segment-base) #xFFFF0000)
(bytevector-u32-native-set! sregs (fx+ offsetof-kvm_sregs-cs offsetof-kvm_segment-limit) #x0000FFFF)
(sys_ioctl cpu-fd KVM_SET_SREGS (bytevector-address sregs))

(define regs (make-bytevector sizeof-kvm_regs 0))
(bytevector-u64-native-set! regs offsetof-kvm_regs-rip #x0000FFF0)
(bytevector-u64-native-set! regs offsetof-kvm_regs-rflags #x2)
(sys_ioctl cpu-fd KVM_SET_REGS (bytevector-address regs))

;;;

(define (set-guest-debug control)
  (let ((bv (make-bytevector sizeof-kvm_guest_debug)))
    (bytevector-u32-native-set! bv offsetof-kvm_guest_debug-control control)
    (sys_ioctl cpu-fd KVM_SET_GUEST_DEBUG (bytevector-address bv))))


(define trace #f)


(define (run)
  (let lp ((i 0))
    (flush-output-port (current-output-port))

    (sys_ioctl cpu-fd KVM_GET_SREGS (bytevector-address sregs))
    (sys_ioctl cpu-fd KVM_GET_REGS (bytevector-address regs))

    (when (and trace (> i 5000))
      (set-guest-debug (fxior KVM_GUESTDBG_ENABLE KVM_GUESTDBG_SINGLESTEP)))

    (when (and trace (> i 5000))
      (let* ((rAX (bytevector-u64-native-ref regs offsetof-kvm_regs-rax))
             (rCX (bytevector-u64-native-ref regs offsetof-kvm_regs-rcx))
             (rFLAGS (bytevector-u64-native-ref regs offsetof-kvm_regs-rflags))
             (cs.selector (bytevector-u16-native-ref sregs (fx+ offsetof-kvm_sregs-cs
                                                                offsetof-kvm_segment-selector)))
             (cs.base (bytevector-u64-native-ref sregs (fx+ offsetof-kvm_sregs-cs
                                                            offsetof-kvm_segment-base)))
             (cs.d/b (bytevector-u8-ref sregs (fx+ offsetof-kvm_sregs-cs offsetof-kvm_segment-db)))
             (cs.l (bytevector-u8-ref sregs (fx+ offsetof-kvm_sregs-cs offsetof-kvm_segment-l)))
             (rip (bytevector-u64-native-ref regs offsetof-kvm_regs-rip))
             (linear-rip (fx+ cs.base rip)))
        (define (print-instr/sexpr i)
          (cond ((pair? i)
                 (display "(")
                 (let lp ((i i))
                   (unless (null? i)
                     (print-instr/sexpr (car i))
                     (unless (null? (cdr i))
                       (display #\space)
                       (lp (cdr i)))))
                 (display ")"))
                ((and (number? i) (exact? i) (integer? i))
                 (display "#x")
                 (display (number->string i 16)))
                (else
                 (display i))))
        (display i)
        (display " rAX: #x" )
        (display (number->string rAX 16))
        (display "\trCX: #x" )
        (display (number->string rCX 16))
        (display "\trFLAGS: #x" )
        (display (number->string rFLAGS 16))
        (display "\tRIP: #x" )
        (display (number->string linear-rip 16))
        (display " ")
        (let* ((mode (if (eqv? cs.l 1) 64 (if (eqv? cs.d/b 0) 16 32)))
               (instr
                (let lp ((regions regions))
                  (unless (null? regions)
                    (let* ((region (car regions))
                           (base (car region))
                           (len (cadr region))
                           (end (fx+ base (fx- len 1)))
                           ;; FIXME: If paging is on then this has to
                           ;; be translated with KVM_TRANSLATE
                           (userspace-addr (caddr region)))
                      (cond ((fx<=? base linear-rip end)
                             ;; FIXME: this might read outside the region
                             (let ((bv (make-bytevector 15))
                                   (rip-addr (fx+ userspace-addr (fx- linear-rip base))))
                               (do ((i 0 (fx+ i 1)))
                                   ((fx=? i (bytevector-length bv)))
                                 (let ((b (get-mem-u8 (fx+ rip-addr i))))
                                   (when (fx<? b #x10)
                                     (display #\0))
                                   (display (number->string b 16)) (display #\space)
                                   (bytevector-u8-set! bv i b)))
                               (guard (exn ((invalid-opcode? exn) exn))
                                 (get-instruction (open-bytevector-input-port bv)
                                                  mode #f rip))))
                            (else (lp (cdr regions)))))))))
          (display #\tab)
          (print-instr/sexpr instr)
          (newline))))

    (let retry ()
      (sys_ioctl cpu-fd KVM_RUN 0
                 (lambda (errno)
                   (cond ((eqv? errno EINTR)
                          #f)
                         (else
                          (raise
                            (condition
                             (make-who-condition 'kvm-run)
                             (make-irritants-condition (list cpu-fd))
                             (make-syscall-error 'ioctl errno))))))))

    (let lp ()
      (let ((e (simple-window-poll-for-event window)))
        (when e
          (log/debug "Event: " e)
          (cond ((or (keyboard-event? e))
                 (let ((char (keyboard-event-char e))
                       (keysym (keyboard-event-bare-keysym e)))
                   (log/debug "key: " char)
                   (cond ((eqv? keysym XKB_KEY_BackSpace)
                          (PS/2-enqueue-byte (if (keyboard-event-press? e) #x0e #x8e)))
                         ((eqv? keysym XKB_KEY_Up)
                          (cond ((keyboard-event-press? e) (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #x48))
                                (else (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #xC8))))
                         ((eqv? keysym XKB_KEY_Down)
                          (cond ((keyboard-event-press? e) (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #x50))
                                (else (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #xD0))))
                         ((eqv? keysym XKB_KEY_Left)
                          (cond ((keyboard-event-press? e) (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #x4b))
                                (else                      (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #xcb))))
                         ((eqv? keysym XKB_KEY_Right)
                          (cond ((keyboard-event-press? e) (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #x4d))
                                (else                      (PS/2-enqueue-byte #xE0) (PS/2-enqueue-byte #xcd))))
                         ((eqv? keysym XKB_KEY_KP_Begin)
                          ;; XXX: F9...
                          (PS/2-enqueue-byte (if (keyboard-event-press? e) #x43 #xc3)))
                         ((eqv? keysym XKB_KEY_Tab)
                          (PS/2-enqueue-byte (if (keyboard-event-press? e) #x0f #x8f)))
                         (else
                          (case (keyboard-event-bare-keysym e)
                            ((#xff0d) (PS/2-enqueue-byte (if (keyboard-event-press? e) #x1c #x9c)))
                            ((#xff1b) (PS/2-enqueue-byte (if (keyboard-event-press? e) #x01 #x81)))
                            (else
                             (case char
                               ((#\d)       (PS/2-enqueue-byte (if (keyboard-event-press? e) #x20 #xA0)))
                               ((#\i)       (PS/2-enqueue-byte (if (keyboard-event-press? e) #x17 #x97)))
                               ((#\r)       (PS/2-enqueue-byte (if (keyboard-event-press? e) #x13 #x93)))
                               ((#\n)       (PS/2-enqueue-byte (if (keyboard-event-press? e) #x31 #xb1)))
                               ((#\c)       (PS/2-enqueue-byte (if (keyboard-event-press? e) #x2e #xae)))
                               ((#\a)       (PS/2-enqueue-byte (if (keyboard-event-press? e) #x1e #x9e)))
                               ((#\m)       (PS/2-enqueue-byte (if (keyboard-event-press? e) #x32 #xb2)))
                               ((#\space)   (PS/2-enqueue-byte (if (keyboard-event-press? e) #x39 #xb9)))))))))))
          (lp))))

    (let ((exit-reason (get-mem-u32 (fx+ vcpu-kvm-run offsetof-kvm_run-exit_reason))))
      (cond

        ((eqv? exit-reason KVM_EXIT_INTR)
         (lp (fx+ i 1)))

        ((eqv? exit-reason KVM_EXIT_HLT)
         (log/error "KVM_EXIT_HLT")
         (sleep 1/60)                   ;TODO: what to do?
         (lp (fx+ i 1)))

        ((eqv? exit-reason KVM_EXIT_DEBUG)
         (lp (fx+ i 1)))

        ((eqv? exit-reason KVM_EXIT_IO)
         (let ((direction (get-mem-u8 (fx+ vcpu-kvm-run offsetof-kvm_run-io.direction)))
               (size (get-mem-u8 (fx+ vcpu-kvm-run offsetof-kvm_run-io.size)))
               (port (get-mem-u16 (fx+ vcpu-kvm-run offsetof-kvm_run-io.port)))
               (count (get-mem-u32 (fx+ vcpu-kvm-run offsetof-kvm_run-io.count)))
               (offset (get-mem-s61 (fx+ vcpu-kvm-run offsetof-kvm_run-io.data_offset))))
           (do ((i 0 (fx+ i 1))
                (offset offset (fx+ offset size)))
               ((fx=? i count))
             (assert (fx<=? 0 offset vcpu-mmap-size))
             (let ((&data (fx+ vcpu-kvm-run offset)))
               (cond
                 ((eqv? direction KVM_EXIT_IO_OUT)
                  (let ((value (case size
                                 ((1) (get-mem-u8 &data))
                                 ((2) (get-mem-u16 &data))
                                 (else (get-mem-u32 &data)))))
                    (unless (eqv? port #x402)
                      '(log/debug "IO OUT: " (number->string port 16) " value: " (number->string value 16)
                                 " " (fx+ 1 i) '/ count " size: " size))
                    (port-write M port (fx* size 8) value)))
                 (else
                  (let ((value (port-read M port (fx* size 8))))
                    '(log/debug "IO IN: " (number->string port 16) " value: " (number->string value 16)
                                " " (fx+ 1 i) '/ count  " size: " size)
                    (case size
                      ((1) (put-mem-u8 &data value))
                      ((2) (put-mem-u16 &data value))
                      (else (put-mem-u32 &data value))))))))
           (lp (fx+ i 1))))

        ;; TODO: Investigate batched MMIO
        ((eqv? exit-reason KVM_EXIT_MMIO)
         (let ((phys_addr (get-mem-s61 (fx+ vcpu-kvm-run offsetof-kvm_run-mmio.phys_addr)))
               (&data (fx+ vcpu-kvm-run offsetof-kvm_run-mmio.data))
               (len (get-mem-u32 (fx+ vcpu-kvm-run offsetof-kvm_run-mmio.len)))
               (write? (not (eqv? 0 (get-mem-u8 (fx+ vcpu-kvm-run offsetof-kvm_run-mmio.is_write))))))

           (cond (write?
                  (case len
                    ((1) (memory-u8-set! (machine-RAM M) phys_addr (get-mem-u8 &data)))
                    ((2) (memory-u16-set! (machine-RAM M) phys_addr (get-mem-u16 &data)))
                    ((4) (memory-u32-set! (machine-RAM M) phys_addr (get-mem-u32 &data)))
                    (else (assert #f)))

                  '(log/debug "MMIO OUT: " (number->string phys_addr 16)
                              " v: " (number->string (get-mem-u32 &data) 16)
                              " len: " len)
                  #f)
                 (else                  ;read
                  '(log/debug "MMIO IN: " (number->string phys_addr 16) " len: " len)
                  (case len
                    ((1) (put-mem-u8 &data (memory-u8-ref (machine-RAM M) phys_addr)))
                    ((2) (put-mem-u16 &data (memory-u16-ref (machine-RAM M) phys_addr)))
                    ((4) (put-mem-u32 &data (memory-u32-ref (machine-RAM M) phys_addr)))
                    (else (assert #f)))))

           (lp (fx+ i 1))))

        ((eqv? exit-reason KVM_EXIT_SHUTDOWN)
         (log/error "KVM_EXIT_SHUTDOWN"))

        (else
         (let ((p (current-error-port)))
           (log/error "Unhandled exit reason: " exit-reason)))))))

(run)
