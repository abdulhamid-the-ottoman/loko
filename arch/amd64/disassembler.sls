;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Disassembler for the Intel x86-16/32/64 instruction set.
;; Copyright © 2008-2019 G. Weinholt

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;;; Disassembler based on the one in the machine-code project.

(library (loko arch amd64 disassembler)
  (export disassemble1)
  (import
    (machine-code disassembler x86-opcodes)
    (except (rnrs) get-u8))

  (define (raise-UD msg . irritants)
    (raise (condition
            (make-who-condition 'get-instruction)
            (make-message-condition msg)
            (make-irritants-condition irritants))))

  (define (disassemble1 bv collect)
    (define debug #f)

    (define (print . x) (for-each display x) (newline))

;;; Lightweight enumerations

    ;; This is not the same as the enumerations in R6RS. The syntax is
    ;; compatible, but everything just becomes constants and bit
    ;; operations.

    (define-syntax define-fxenumeration
      (lambda (x)
        (syntax-case x ()
          ((_ const-name (f ...) set-name)
           #'(begin
               (define-syntax set-name
                 (lambda (y)
                   (syntax-case y (%hack)
                     ((_) 0)
                     ((_ %hack) #''(f ...))
                     ((_ x rest (... ...))
                      #'(fxior (const-name x) (set-name rest (... ...)))))))
               ;; This assumes that there's constant folding going on,
               ;; like with cp0.
               (define-syntax const-name
                 (lambda (y)
                   (syntax-case y (f ...)
                     ((_ f)
                      #'(expt 2 (- (length '(f ...))
                                   (length (memq 'f '(f ...))))))
                     ...))))))))

    (define (enum-set-member? x set)
      (not (fxzero? (fxand x set))))
    (define enum-set-union fxior)
    (define enum-set-intersection fxand)
    (define enum-set=? fx=?)
    (define (enum-set-difference x y)
      (fxand x (fxnot y)))

    ;; Only used in the debugging printout.
    (define (enum-set->list x) (number->string x 2))

    (define-syntax enum-set-constructor
      (lambda (x)
        (syntax-case x ()
          ((_ (set))
           #'(lambda (symbols)
               (fold-left (lambda (acc f)
                            (fxior acc
                                   (expt 2 (- (length (set %hack))
                                              (length (memq f (set %hack)))))))
                          0
                          symbols))))))

    (define-syntax define-enumeration
      (lambda (x)
        (syntax-case x ()
          ((_ const-name (f ...) set-name)
           #'(define-syntax const-name
               (lambda (y)
                 (syntax-case y ()
                   ((_ x) #''x))))))))

;;;

    (define-fxenumeration prefix
      (operand address cs ds es fs gs ss lock repz repnz rex rex.w rex.r
               rex.x rex.b vex vex.l)
      prefix-set)

    (define-enumeration tag
      ;; Just used for the `tag' syntax. So here is a list of all tags
      ;; that can be given to the "collect" function.
      (modr/m sib disp immediate /is4 prefix opcode)
      tag-set)

    (define (has-modr/m? instr)
      ;; Not the prettiest function ever, but it works.
      (and (pair? instr)
           (exists (lambda (op)
                     (memv (string-ref (symbol->string op) 0)
                           '(#\C #\D #\E #\G #\M #\N #\P #\Q #\R #\S #\U #\V #\W)))
                   (cdr instr))))

    (define (has-/is4? instr)
      (and (pair? instr)
           (exists (lambda (op) (memq op '(In Kpd Kps Kss Ksd Lo Lx)))
                   (cdr instr))))

    
;;; Simple byte decoding
    (define (ModR/M-mod byte)
      (fxbit-field byte 6 8))

    (define ModR/M-reg
      (case-lambda
        ((byte)
         (fxbit-field byte 3 6))
        ((byte prefixes)
         (if (enum-set-member? (prefix rex.r) prefixes)
             (fxior #b1000 (fxbit-field byte 3 6))
             (fxbit-field byte 3 6)))))

    (define ModR/M-r/m
      (case-lambda
        ((byte)
         (fxbit-field byte 0 3))
        ((byte prefixes)
         (if (enum-set-member? (prefix rex.b) prefixes)
             (bitwise-ior #b1000 (fxbit-field byte 0 3))
             (fxbit-field byte 0 3)))))

    (define (print-modr/m byte prefixes)
      (print "ModR/M=#x" (number->string byte 16)
             " Mod=#b" (number->string (ModR/M-mod byte) 2)
             " Reg=#b" (number->string (ModR/M-reg byte prefixes) 2)
             " R/M=#b" (number->string (ModR/M-r/m byte prefixes) 2)))

    (define (print-sib byte prefixes)
      (print "SIB=#x" (number->string byte 16)
             " Scale=#b" (number->string (SIB-scale byte) 2)
             " Index=#b" (number->string (SIB-index byte prefixes) 2)
             " Base=#b" (number->string (SIB-base byte prefixes) 2)))

    (define (SIB-scale byte)
      (bitwise-arithmetic-shift-left 1 (fxbit-field byte 6 8)))

    (define (SIB-index byte prefixes)
      (if (enum-set-member? (prefix rex.x) prefixes)
          (bitwise-ior #b1000 (fxbit-field byte 3 6))
          (fxbit-field byte 3 6)))

    (define (SIB-base byte prefixes)
      (if (enum-set-member? (prefix rex.b) prefixes)
          (bitwise-ior #b1000 (fxbit-field byte 0 3))
          (fxbit-field byte 0 3)))

    (define (VEX-vvvv byte mode)
      ;; Encodes another XMM operand.
      (define mode 64)
      (bitwise-and (if (= mode 64) #b1111 #b111) ;VEX.vvvv
                   (bitwise-xor #b1111 (fxbit-field byte 3 7))))

    (define (VEX-m-mmmm->table byte)
      (case (fxbit-field byte 0 5) ;VEX.m-mmmm
        ((#b00001) (vector-ref opcodes #x0F))
        ((#b00010) (vector-ref (vector-ref opcodes #x0F) #x38))
        ((#b00011) (vector-ref (vector-ref opcodes #x0F) #x3A))
        (else (raise-UD "Reserved VEX.m-mmmm encoding"
                        (fxbit-field byte 0 5)))))

    (define (VEX3->prefixes prefixes mode byte1 byte2)
      (define mode 64)
      (let ((byte1 (if (= mode 64) byte1 (bitwise-ior byte1 #b1110000))))
        (fold-left enum-set-union
                   prefixes
                   (list
                    (if (bitwise-bit-set? byte1 7) ;VEX.R
                        (prefix-set) (prefix-set rex.r))
                    (if (bitwise-bit-set? byte1 6) ;VEX.X
                        (prefix-set) (prefix-set rex.x))
                    (if (bitwise-bit-set? byte1 5) ;VEX.B
                        (prefix-set) (prefix-set rex.b))
                    (if (bitwise-bit-set? byte2 7) ;VEX.W
                        (prefix-set rex.w) (prefix-set))
                    (if (bitwise-bit-set? byte2 2) ;VEX.L
                        (prefix-set vex.l) (prefix-set))
                    (case (fxbit-field byte2 0 2) ;VEX.pp
                      ((#b00) (prefix-set vex))
                      ((#b01) (prefix-set vex operand))
                      ((#b10) (prefix-set vex repz))
                      ((#b11) (prefix-set vex repnz)))
                    (if (= mode 64) (prefix-set rex) (prefix-set))))))

    (define (VEX2->prefixes prefixes mode byte1)
      (define mode 64)
      (let ((byte (if (= mode 64) byte1 (bitwise-ior byte1 #b11000000))))
        (fold-left enum-set-union
                   prefixes
                   (list
                    (if (bitwise-bit-set? byte 7) ;VEX.R
                        (prefix-set) (prefix-set rex.r))
                    (if (bitwise-bit-set? byte 2) ;VEX.L
                        (prefix-set vex.l) (prefix-set))
                    (case (fxbit-field byte 0 2) ;VEX.pp
                      ((#b00) (prefix-set vex))
                      ((#b01) (prefix-set vex operand))
                      ((#b10) (prefix-set vex repz))
                      ((#b11) (prefix-set vex repnz)))
                    (if (= mode 64) (prefix-set rex) (prefix-set))))))

    (define (lookahead-is-valid-VEX? port)
      "In legacy mode, the upper two bits following a C4 or C5 byte must
be #b11 for it to be considered a valid VEX prefix (so there will be
no conflict with LES/LDS)."
      (let ((byte (lookahead-u8 port)))
        (and (not (eof-object? byte))
             (= (fxbit-field byte 6 8) #b11))))

    (define (lookahead-is-valid-XOP? port)
      "The instruction is probably a POP if map_select < #b111."
      (let ((byte (lookahead-u8 port)))
        (and (fixnum? byte)
             (fx>=? (fxbit-field byte 0 5) #b00111))))

;;;
    (define (VEX-prefix-check prefixes mode)
      (when (eqv? mode 16)
        (raise-UD "The VEX/XOP prefix is not valid in 16-bit modes"))
      (unless (enum-set=? (prefix-set)
                          (enum-set-intersection
                           prefixes (prefix-set vex rex lock operand repz repnz)))
        (raise-UD "Conflicting prefixes together with VEX/XOP")))

    (define (needs-VEX prefixes)
      (unless (enum-set-member? (prefix vex) prefixes)
        (raise-UD "This instruction requires a VEX prefix")))
    
;;; Port input

    (define (really-get-bytevector-n port n collect tag)
      (let ((bv (get-bytevector-n port n)))
        (unless (eof-object? bv)
          (if collect (apply collect tag (bytevector->u8-list bv))))
        (when (or (eof-object? bv) (< (bytevector-length bv) n))
          (raise-UD "End of file inside instruction"))
        bv))

    (define (get-u8 port)
      (bytevector-u8-ref (really-get-bytevector-n port 1 #f #f)
                         0))

    (define (get-u8/collect port collect tag)
      (bytevector-u8-ref (really-get-bytevector-n port 1 collect tag)
                         0))

    (define (get-s8/collect port collect tag)
      (bytevector-s8-ref (really-get-bytevector-n port 1 collect tag)
                         0))

    (define (get-s16/collect port collect tag)
      (bytevector-s16-ref (really-get-bytevector-n port 2 collect tag)
                          0 (endianness little)))

    (define (get-u16/collect port collect tag)
      (bytevector-u16-ref (really-get-bytevector-n port 2 collect tag)
                          0 (endianness little)))

    (define (get-s32/collect port collect tag)
      (bytevector-s32-ref (really-get-bytevector-n port 4 collect tag)
                          0 (endianness little)))

    (define (get-u32/collect port collect tag)
      (bytevector-u32-ref (really-get-bytevector-n port 4 collect tag)
                          0 (endianness little)))

    (define (get-u64/collect port collect tag)
      (bytevector-u64-ref (really-get-bytevector-n port 8 collect tag)
                          0 (endianness little)))

;;; Register names
    (define reg-names8 '#(al cl dl bl ah ch dh bh))

    ;; Intel calls these r8l, r9l, and so on, but since AMD invented
    ;; them, use AMD's names.
    (define reg-names8rex '#(al cl dl bl spl bpl sil dil
                                r8b r9b r10b r11b r12b r13b r14b r15b))

    (define reg-names16 '#(ax cx dx bx sp bp si di
                              r8w r9w r10w r11w r12w r13w r14w r15w))

    (define reg-names32 '#(eax ecx edx ebx esp ebp esi edi
                               r8d r9d r10d r11d r12d r13d r14d r15d))

    (define reg-names64 '#(rax rcx rdx rbx rsp rbp rsi rdi
                               r8 r9 r10 r11 r12 r13 r14 r15))

    ;; These are sometimes called mmx0, mmx1, etc for no apparent
    ;; reason.
    (define reg-names-mmx '#(mm0 mm1 mm2 mm3 mm4 mm5 mm6 mm7
                                 mm0 mm1 mm2 mm3 mm4 mm5 mm6 mm7))

    (define reg-names-xmm '#(xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7
                                  xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15))

    (define reg-names-ymm '#(ymm0 ymm1 ymm2 ymm3 ymm4 ymm5 ymm6 ymm7
                                  ymm8 ymm9 ymm10 ymm11 ymm12 ymm13 ymm14 ymm15))

    (define reg-names-sreg '#(es cs ss ds fs gs #f #f
                                 es cs ss ds fs gs #f #f))

    (define reg-names-creg '#(cr0 cr1 cr2 cr3 cr4 cr5 cr6 cr7 cr8 cr9
                                  cr10 cr11 cr12 cr13 cr14 cr15))

    (define reg-names-dreg '#(dr0 dr1 dr2 dr3 dr4 dr5 dr6 dr7 dr8 dr9
                                  dr10 dr11 dr12 dr13 dr14 dr15))

    (define reg-names-x87 '#(st0 st1 st2 st3 st4 st5 st6 st7
                                 st0 st1 st2 st3 st4 st5 st6 st7))
    
;;; Special cases
    (define (fix-lock instruction prefixes)
      (cond ((not (enum-set-member? (prefix lock) prefixes))
             instruction)
            ((or (null? (cdr instruction))
                 (not (pair? (cadr instruction))))
             (raise-UD "LOCK prefix requires a memory destination operand"))
            ((memq (car instruction) lock-instructions) =>
             (lambda (name)
               (cons (string->symbol
                      (string-append "lock." (symbol->string (car instruction))))
                     (cdr instruction))))
            (else
             (raise-UD "LOCK prefix invalid for this instruction"))))

    (define (fix-rep instruction prefixes)
      (cond ((enum-set-member? (prefix repz) prefixes)
             (cond ((memq (car instruction) rep-instructions)
                    (cons (string->symbol
                           (string-append "rep."
                                          (symbol->string (car instruction))))
                          (cdr instruction)))
                   ((memq (car instruction) repz-instructions)
                    (cons (string->symbol
                           (string-append "repz."
                                          (symbol->string (car instruction))))
                          (cdr instruction)))
                   (else instruction)))
            ((enum-set-member? (prefix repnz) prefixes)
             (cond ((memq (car instruction) repz-instructions)
                    (cons (string->symbol
                           (string-append "repnz."
                                          (symbol->string (car instruction))))
                          (cdr instruction)))
                   (else instruction)))
            (else instruction)))

    (define (fix-nop instruction prefixes mode operand-size)
      (define mode 64)
      (define (nop)
        (if (enum-set-member? (prefix repz) prefixes) '(pause) '(nop)))
      (if (eq? (car instruction) '*nop*)
          (case mode
            ((32 64)
             (case operand-size
               ((16) (if (enum-set-member? (prefix rex.b) prefixes)
                         '(xchg r8w ax)
                         '(xchg ax ax)))
               ((32) (if (enum-set-member? (prefix rex.b) prefixes)
                         '(xchg r8d eax)
                         (nop)))
               ((64) (if (enum-set-member? (prefix rex.b) prefixes)
                         '(xchg r8 rax)
                         '(xchg rax rax)))))
            ((16)
             (case operand-size
               ((16) (nop))
               ((32) '(xchg eax eax)))))
          instruction))
    
;;; Instruction stream decoding
    (define (get-displacement port mode collect prefixes modr/m address-size)
      (define mode 64)
      (let ((mod (ModR/M-mod modr/m))
            (r/m (ModR/M-r/m modr/m prefixes)))
        (define (mem32/64 register regs sib?)
          (case mod
            ((#b00) (cond
                      ((fx=? (fxand register #b111) #b101)
                       ;; FIXME: sign-extend these instead of giving
                       ;; them as negative numbers? At least if they are
                       ;; large enough...
                       (if (or (fx=? mode 32) sib?)
                           (list (get-s32/collect port collect (tag disp)))
                           (list (if (fx=? address-size 64) 'rip 'eip)
                                 (get-s32/collect port collect (tag disp)))))
                      (else
                       (list (vector-ref regs register)))))
            ((#b01) (list (vector-ref regs register)
                          (get-s8/collect port collect (tag disp))))
            ((#b10) (list (vector-ref regs register)
                          (get-s32/collect port collect (tag disp))))))
        (if (fx=? mod #b11)
            r/m                           ;register operand
            (if #f ;;(fx=? address-size 16)
                (let ((addr16 '#((bx si) (bx di) (bp si) (bp di)
                                 (si) (di) (bp) (bx))))
                  (case mod
                    ((#b00) (if (fx=? r/m #b110)
                                (list (get-s16/collect port collect (tag disp)))
                                (vector-ref addr16 r/m)))
                    ((#b01) (append (vector-ref addr16 r/m)
                                    (list (get-s8/collect port collect (tag disp)))))
                    ((#b10) (append (vector-ref addr16 r/m)
                                    (list (get-s16/collect port collect (tag disp)))))))
                (let ((regs (if (fx=? address-size 64) reg-names64 reg-names32))
                      (sib (and (fx=? (ModR/M-r/m modr/m) #b100)
                                (get-u8/collect port collect (tag sib)))))
                  (if sib
                      (append (mem32/64 (SIB-base sib prefixes) regs #t)
                              (if (fx=? (SIB-index sib prefixes) #b100)
                                  '()
                                  `((* ,(vector-ref regs (SIB-index sib prefixes))
                                       ,(SIB-scale sib)))))
                      (mem32/64 r/m regs #f)))))))

    (define (translate-displacement prefixes mode disp operand-size . memsize)
      (define mode 64)
      (cond ((integer? disp)
             (vector-ref (case operand-size
                           ((8) (if (enum-set-member? (prefix rex) prefixes)
                                    reg-names8rex
                                    reg-names8))
                           ((16) reg-names16)
                           ((32) reg-names32)
                           ((64) reg-names64)
                           ((mmx) reg-names-mmx)
                           ((xmm) (if (enum-set-member? (prefix vex.l) prefixes)
                                      reg-names-ymm
                                      reg-names-xmm))
                           ((x87) reg-names-x87)
                           ((notreg)
                            (raise-UD "ModR/M encoded a register but memory is required"))
                           (else
                            (error 'translate-displacement
                                   "Unimplemented register operand size" operand-size)))
                         disp))
            ((pair? disp)
             (cons (case (if (null? memsize) operand-size (car memsize))
                     ((8) 'mem8+)
                     ((16) 'mem16+)
                     ((32) 'mem32+)
                     ((64 mmx) 'mem64+)
                     ((x87 80) 'mem80+)
                     ((xmm) (if (enum-set-member? (prefix vex.l) prefixes)
                                'mem256+
                                'mem128+))
                     ((128) 'mem128+)
                     ((ptr16) 'mem16:16+)
                     ((ptr32) 'mem16:32+)
                     ((ptr64) 'mem16:64+)
                     ((generic) 'mem+)
                     ((notmem)
                      (raise-UD "ModR/M byte encoded memory but a register is required"))
                     (else
                      (error 'translate-displacement
                             "Unimplemented memory operand size"
                             (if (null? memsize) operand-size (car memsize)))))
                   (cond ((prefixes->segment-override prefixes mode #f) =>
                          (lambda (seg) (cons seg disp)))
                         (else disp))))
            (else
             ;; This happens if ModR/M should've been read, but wasn't.
             (error 'translate-displacement
                    "Bad displacement" disp))))

    (define (prefixes->segment-override prefixes mode default)
      ;; TODO: What if multiple segment overrides are given?
      (define mode 64)
      (cond ((enum-set-member? (prefix fs) prefixes) 'fs)
            ((enum-set-member? (prefix gs) prefixes) 'gs)
            (else
             (if (= mode 64)
                 #f
                 (cond ((enum-set-member? (prefix cs) prefixes) 'cs)
                       ((enum-set-member? (prefix ds) prefixes) 'ds)
                       ((enum-set-member? (prefix es) prefixes) 'es)
                       ((enum-set-member? (prefix ss) prefixes) 'ss)
                       (else default))))))

    (define (get-operand port mode collect op prefixes opcode vex.v
                         operand-size address-size modr/m
                         disp /is4)
      (define mode 64)
      (let get-operand ((op op))
        (case op
          ((Jb) (list '+ (case mode
                           ((16) 'ip)
                           ((32) 'eip)
                           ((64) 'rip))
                      (get-s8/collect port collect (tag disp))))
          ((Jz)
           (let ((rip (case mode
                        ((16) 'ip)
                        ((32) 'eip)
                        ((64) 'rip))))
             (case operand-size
               ((16) (list '+ rip (get-s16/collect port collect (tag disp))))
               ((32 64) (list '+ rip (get-s32/collect port collect (tag disp)))))))

          ((Md/q)                         ;FIXME: verify
           (translate-displacement prefixes mode disp
                                   (if (= operand-size 16) 32 operand-size)))

          ((Gd/q)                         ;FIXME: verify
           (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes)
                                   (if (= operand-size 16) 32 operand-size)))

          ((Gq) (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes)
                                        64))
          ((Gv) (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes)
                                        operand-size))
          ((Gz) (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes)
                                        (if (= operand-size 16) 16 32)))
          ((Gd) (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes)
                                        32))
          ((Gb) (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes)
                                        8))
          ((Gw) (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes)
                                        16))

          ((Ev) (translate-displacement prefixes mode disp operand-size))
          ((Eb) (translate-displacement prefixes mode disp 8))
          ((Ew) (translate-displacement prefixes mode disp 16))
          ((Ed) (translate-displacement prefixes mode disp 32))
          ((Eq) (translate-displacement prefixes mode disp 64))
          ((Ed/q)
           (translate-displacement prefixes mode disp
                                   (if (= operand-size 16) 32 operand-size)))

          ((Ib) (get-u8/collect port collect (tag immediate)))
          ((IbS)
           ;; Sign extended immediate byte (not official opsyntax)
           (let ((byte (get-u8/collect port collect (tag immediate))))
             (if (bitwise-bit-set? byte 7)
                 (case operand-size
                   ((16) (bitwise-ior #xff00 byte))
                   ((32) (bitwise-ior #xffffff00 byte))
                   ((64) (bitwise-ior #xffffffffffffff00 byte)))
                 byte)))
          ((Iw) (get-u16/collect port collect (tag immediate)))
          ((Id) (get-u32/collect port collect (tag immediate)))
          ((Iv)
           ((case operand-size
              ((16) get-u16/collect)
              ((32) get-u32/collect)
              ((64) get-u64/collect))
            port collect (tag immediate)))
          ((Iz)
           (case operand-size
             ((16) (get-u16/collect port collect (tag immediate)))
             ((32) (get-u32/collect port collect (tag immediate)))
             ((64)
              (let ((imm (get-u32/collect port collect (tag immediate))))
                (if (bitwise-bit-set? imm 31)
                    (bitwise-ior #xffffffff00000000 imm)
                    imm)))))

          ((Ob)
           (list 'mem8+
                 ((case address-size
                    ((16) get-u16/collect)
                    ((32) get-u32/collect)
                    ((64) get-u64/collect))
                  port collect (tag disp))))
          ((Ov)
           ;; FIXME: is this correct?
           (list (case operand-size
                   ((16) 'mem16+)
                   ((32) 'mem32+)
                   ((64) 'mem64+))
                 ((case address-size
                    ((16) get-u16/collect)
                    ((32) get-u32/collect)
                    ((64) get-u64/collect))
                  port collect (tag disp))))
          ;; Far pointer
          ((Ap)
           (let* ((off (if (= operand-size 32)
                           (get-u32/collect port collect (tag disp))
                           (get-u16/collect port collect (tag disp))))
                  (ss (get-u16/collect port collect (tag disp))))
             (list 'far ss off)))

          ;; String operation operands
          ((Xb)
           (let ((seg (prefixes->segment-override prefixes mode 'ds)))
             (case address-size
               ((16) `(mem8+ ,seg si))
               ((32) (if seg `(mem8+ ,seg esi) '(mem8+ esi)))
               ((64) (if seg `(mem8+ ,seg rsi) '(mem8+ rsi))))))
          ((Xv)
           (let ((seg (prefixes->segment-override prefixes mode 'ds))
                 (size (case operand-size
                         ((16) 'mem16+)
                         ((32) 'mem32+)
                         ((64) 'mem64+))))
             (case address-size
               ((16) `(,size ,seg si))
               ((32) (if seg `(,size ,seg esi) `(,size esi)))
               ((64) (if seg `(,size ,seg rsi) `(,size rsi))))))
          ((Xz)
           (let ((seg (prefixes->segment-override prefixes mode 'ds))
                 (size (case operand-size
                         ((16) 'mem16+)
                         ((32 64) 'mem32+))))
             (case address-size
               ((16) `(,size ,seg si))
               ((32) (if seg `(,size ,seg esi) `(,size esi)))
               ((64) (if seg `(,size ,seg rsi) `(,size rsi))))))

          ((Yb)
           (case address-size
             ((16) '(mem8+ es di))
             ((32) (if (= mode 64) '(mem8+ edi) '(mem8+ es edi)))
             ((64) '(mem8+ rdi))))
          ((Yv)
           (let ((size (case operand-size
                         ((16) 'mem16+)
                         ((32) 'mem32+)
                         ((64) 'mem64+))))
             (case address-size
               ((16) `(,size es di))
               ((32) (if (= mode 64) `(,size edi) `(,size es edi)))
               ((64) `(,size rdi)))))
          ((Yz)
           (let ((size (case operand-size
                         ((16) 'mem16+)
                         ((32 64) 'mem32+))))
             (case address-size
               ((16) `(,size es di))
               ((32) (if (= mode 64) `(,size edi) `(,size es edi)))
               ((64) `(,size rdi)))))

          ;; Special registers
          ((Cd/q) (vector-ref reg-names-creg (ModR/M-reg modr/m prefixes)))
          ((Dd/q) (vector-ref reg-names-dreg (ModR/M-reg modr/m prefixes)))
          ((Sw) (or (vector-ref reg-names-sreg (ModR/M-reg modr/m))
                    (raise-UD "Invalid segment register encoded")))

          ;; SSE. "Packed" is also "vector" in some documentation. It
          ;; means that the register is packed with more than one
          ;; number. For example, ps means four 32-bit floats packed
          ;; together in a 128-bit xmm register. "Scalar" is when
          ;; there is just one number in a register, at the lowest
          ;; bits.

          ;; ps = packed single-precision floating point
          ;; pd = packed double-precision floating point
          ;; ss = scalar single-precision floating point
          ;; sd = scalar double-precision floating point
          ;; x = 128/256 bit vector
          ((Vps Vdq Vpd Vq Vd Vsd Vss Vx)
           (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes) 'xmm))
          ;; Called VRdq by AMD:
          ((Ups Udq Upd Uq)
           (translate-displacement prefixes mode disp 'xmm 'notmem))

          ((Wps Wdq Wpd Wx)
           (translate-displacement prefixes mode disp 'xmm))
          ((Wsd Udq/Mq Wq)
           (translate-displacement prefixes mode disp 'xmm 64))
          ((Wss Udq/Md)
           (translate-displacement prefixes mode disp 'xmm 32))
          ((Udq/Mw)
           (translate-displacement prefixes mode disp 'xmm 16))

          ((Pq Pd)
           (translate-displacement prefixes mode (ModR/M-reg modr/m prefixes) 'mmx))
          ((Qq)
           (translate-displacement prefixes mode disp 'mmx))
          ((Qd)
           (translate-displacement prefixes mode disp 'mmx 32))
          ;; Called PRq by AMD:
          ((Nq)
           (translate-displacement prefixes mode disp 'mmx 'notmem))

          ((Wps/128 Wo)                   ;Forced to 128-bit xmm
           (translate-displacement (enum-set-difference prefixes (prefix-set vex.l))
                                   mode disp 'xmm))
          ((Wq/128)                       ;Forced to 128-bit xmm
           (translate-displacement (enum-set-difference prefixes (prefix-set vex.l))
                                   mode disp 'xmm 64))
          ((Vq/128)                       ;Forced to 128-bit xmm
           (translate-displacement (enum-set-difference prefixes (prefix-set vex.l))
                                   mode (ModR/M-reg modr/m prefixes) 'xmm 64))
          ((Vo)                           ;Forced to 128-bit xmm
           (translate-displacement (enum-set-difference prefixes (prefix-set vex.l))
                                   mode (ModR/M-reg modr/m prefixes) 'xmm))

          ;; Intel AVX. K, KW, WK, B, BW, WB is not official opsyntax.
          ((Kpd Kps Kss Ksd)
           (needs-VEX prefixes)
           (translate-displacement prefixes mode
                                   (fxbit-field /is4 4 (if (= mode 64) 8 7))
                                   'xmm))
          ((Lo)
           (needs-VEX prefixes)
           (translate-displacement (enum-set-difference prefixes (prefix-set vex.l))
                                   mode
                                   (fxbit-field /is4 4 (if (= mode 64) 8 7))
                                   'xmm))
          ((Lx)
           (needs-VEX prefixes)
           (translate-displacement prefixes mode
                                   (fxbit-field /is4 4 (if (= mode 64) 8 7))
                                   'xmm))

          ((KWpd) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Wpd) (get-operand 'Kpd)))
          ((KWps) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Wps) (get-operand 'Kps)))

          ((WKpd) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Kpd) (get-operand 'Wpd)))
          ((WKps) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Kps) (get-operand 'Wps)))

          ((Bpd Bps Bss Bsd Bdq Hx)
           (needs-VEX prefixes)
           (translate-displacement prefixes mode vex.v 'xmm))
          ((Ho)
           (needs-VEX prefixes)
           (translate-displacement (enum-set-difference prefixes (prefix-set vex.l))
                                   mode vex.v 'xmm))

          ;; TODO: These things shouldn't be used. Replace them with #(W ... ...)
          ((BWpd) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Wpd) (get-operand 'Bpd)))
          ((BWps) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Wps) (get-operand 'Bps)))
          ((BWsd) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Wsd) (get-operand 'Bsd)))
          ((BWss) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Wss) (get-operand 'Bss)))

          ((WBpd) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Bpd) (get-operand 'Wpd)))
          ((WBps) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Bps) (get-operand 'Wps)))
          ((WBss) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Bss) (get-operand 'Wss)))
          ((WBsd) (if (enum-set-member? (prefix rex.w) prefixes)
                      (get-operand 'Bsd) (get-operand 'Wsd)))

          ((In) (fxbit-field /is4 0 4))

          ((By)
           (needs-VEX prefixes)
           (translate-displacement prefixes mode vex.v (max 32 operand-size)))

          ;; These must be memory references
          ((M Ms) (translate-displacement prefixes mode disp 'notreg 'generic))
          ((Mb) (translate-displacement prefixes mode disp 'notreg 8))
          ((Mw) (translate-displacement prefixes mode disp 'notreg 16))
          ((Md) (translate-displacement prefixes mode disp 'notreg 32))
          ((Mq) (translate-displacement prefixes mode disp 'notreg 64))
          ((Mdq) (translate-displacement prefixes mode disp 'notreg 128))
          ((Mpd Mps) (translate-displacement prefixes mode disp 'notreg 'xmm))
          ((Mv) (translate-displacement prefixes mode disp 'notreg operand-size))
          ((Mem80)
           ;; Used for x87 memory operands, so it could really be 80,
           ;; 64 or 32 bits depending on how the x87 is configured. In
           ;; Linux on amd64 it's used for "long double", which is in
           ;; fact 80 bits wide.
           (translate-displacement prefixes mode disp 'notreg 80))
          ((Mp)
           (translate-displacement prefixes mode disp 'notreg
                                   (case operand-size
                                     ((16) 'ptr16)
                                     ((32) 'ptr32)
                                     ((64) 'ptr64))))
          ((Ma)
           (translate-displacement prefixes mode disp
                                   'notreg
                                   (case operand-size
                                     ((16) 32)
                                     ((32) 64))))

          ((Rd/q)
           ;; 64-bit general register in long mode, 32-bit in legacy.
           (translate-displacement prefixes mode disp
                                   (if (= mode 16) 32 mode) 'notmem))
          ((Rv/Mw) (translate-displacement prefixes mode disp operand-size 16))
          ((Rd/Mw) (translate-displacement prefixes mode disp 32 16))
          ((Rd/Mb) (translate-displacement prefixes mode disp 32 8))

          ((*rAX/r8 *rCX/r9 *rDX/r10 *rBX/r11 *rSP/r12 *rBP/r13 *rSI/r14 *rDI/r15)
           (translate-displacement prefixes mode (ModR/M-r/m opcode prefixes)
                                   operand-size))
          ((*AL/R8L *CL/R9L *DL/R10L *BL/R11L *AH/R12L *CH/R13L *DH/R14L *BH/R15L)
           (translate-displacement prefixes mode (ModR/M-r/m opcode prefixes) 8))
          ((*eCX *eDX *eBX *eSP *eBP *eSI *eDI)
           (translate-displacement prefixes mode (ModR/M-r/m opcode prefixes)
                                   (if (= operand-size 16) 16 32)))

          ;; x87
          ((*st0) 'st0)
          ((*st) (translate-displacement prefixes mode disp 'x87))

          ((*unity) 1)
          ((*CS) 'cs)
          ((*ES) 'es)
          ((*DS) 'ds)
          ((*FS) 'fs)
          ((*GS) 'gs)
          ((*SS) 'ss)
          ((*DX) 'dx)
          ((*CL) 'cl)
          ((*eAX) (if (= operand-size 16) 'ax 'eax))
          ((*AX) 'ax)
          ((*AL) 'al)
          ((*rAX)
           (case operand-size
             ((16) 'ax)
             ((32) 'eax)
             ((64) 'rax)))
          (else
           (error 'get-operand "Unimplemented opsyntax" op)))))

    (define (get-operands port mode collect prefixes instr modr/m opcode vex.v d64)
      (define mode 64)
      (let* ((operand-size (case mode
                             ((64) (cond ((enum-set-member? (prefix rex.w) prefixes) 64)
                                         ((enum-set-member? (prefix operand) prefixes) 16)
                                         (d64 64)
                                         (else 32)))
                             ((32) (cond ((enum-set-member? (prefix operand) prefixes) 16)
                                         (else 32)))
                             ((16) (cond ((enum-set-member? (prefix operand) prefixes) 32)
                                         (else 16)))))
             (address-size (case mode
                             ((64) (cond ((enum-set-member? (prefix address) prefixes) 32)
                                         (else 64)))
                             ((32) (cond ((enum-set-member? (prefix address) prefixes) 16)
                                         (else 32)))
                             ((16) (cond ((enum-set-member? (prefix address) prefixes) 32)
                                         (else 16)))))
             (modr/m (or modr/m (and (has-modr/m? instr)
                                     (get-u8/collect port collect (tag modr/m)))))
             (disp (and (number? modr/m)
                        (get-displacement port mode collect prefixes modr/m address-size)))
             (/is4 (and (has-/is4? instr) (get-u8/collect port collect (tag /is4)))))
        ;; At this point in the instruction stream, the only things left
        ;; are I, J and O (immediate, jump offset, offset) values.
        (when debug
          (print "Instruction=" instr
                 " prefixes=" (enum-set->list prefixes)
                 " opcode=" (number->string opcode 16)
                 " vex.v=" vex.v
                 " displacement=" disp
                 " /is4=" (and /is4 (number->string /is4 2)))
          (if (number? modr/m) (print-modr/m modr/m prefixes)))

        (let* ((x (cons (car instr)
                        (let lp ((op* (cdr instr)))
                          (if (null? op*)
                              '()
                              (cons (get-operand port mode collect (car op*) prefixes
                                                 opcode vex.v
                                                 operand-size address-size modr/m
                                                 disp /is4)
                                    (lp (cdr op*)))))))
               (x (fix-nop x prefixes mode operand-size))
               (x (fix-lock x prefixes))
               (x (fix-rep x prefixes)))
          x)))

    (define (get-instruction* port mode collect)
      (define mode 64)
      (let more-opcode ((opcode-table opcodes)
                        (vex.v #f)
                        (prefixes (prefix-set)))
        (let ((opcode (get-u8 port)))
          (let lp ((instr (vector-ref opcode-table opcode))
                   (modr/m #f)
                   (opcode opcode)
                   (prefixes prefixes)
                   (opcode-collected #f)
                   (vex-traversed #f)
                   (d64 #f))
            (cond
              ((and (= opcode #xC4) (or (= mode 64) (lookahead-is-valid-VEX? port))
                    (not (enum-set-member? (prefix vex) prefixes)))
               ;; Three-byte VEX prefix
               (let* ((byte1 (get-u8 port))
                      (byte2 (get-u8 port)))
                 (collect (tag prefix) opcode byte1 byte2)
                 (VEX-prefix-check prefixes mode)
                 (more-opcode (VEX-m-mmmm->table byte1)
                              (VEX-vvvv byte2 mode)
                              (VEX3->prefixes prefixes mode byte1 byte2))))

              ((and (= opcode #xC5) (or (= mode 64) (lookahead-is-valid-VEX? port))
                    (not (enum-set-member? (prefix vex) prefixes)))
               ;; Two-byte VEX prefix
               (let ((byte1 (get-u8 port)))
                 (collect (tag prefix) opcode byte1)
                 (VEX-prefix-check prefixes mode)
                 (more-opcode (vector-ref opcodes #x0F)
                              (VEX-vvvv byte1 mode)
                              (VEX2->prefixes prefixes mode byte1))))

              ;; ((and (eqv? opcode #x8F) (eq? opcode-table opcodes)
              ;;       (lookahead-is-valid-XOP? port))
              ;;  ;; Three-byte XOP prefix
              ;;  (let* ((byte1 (get-u8 port))
              ;;         (byte2 (get-u8 port)))
              ;;    (collect (tag prefix) opcode byte1 byte2)
              ;;    (VEX-prefix-check prefixes mode)
              ;;    (more-opcode (XOP-map-select->table byte1)
              ;;                 (VEX-vvvv byte2 mode)
              ;;                 (VEX3->prefixes prefixes mode byte1 byte2))))

              ((not instr)
               (unless opcode-collected
                 (collect 'opcode opcode))
               (raise-UD "Invalid or reserved opcode"))

              ((and (pair? instr) (eq? (car instr) '*prefix*)) ;Prefix
               (collect (tag prefix) opcode)
               (when (enum-set-member? (prefix rex) prefixes)
                 (raise-UD "Other prefixes can not follow the REX prefix"))
               (more-opcode opcode-table
                            vex.v
                            (enum-set-union
                             prefixes
                             ((enum-set-constructor (prefix-set)) (cdr instr)))))

              ((pair? instr)
               ;; An instruction has finally been found
               (unless opcode-collected
                 (collect (tag opcode) opcode))
               (when (and (enum-set-member? (prefix vex) prefixes)
                          (not vex-traversed))
                 (raise-UD "VEX was used but a legacy instruction was found"))
               (get-operands port mode collect prefixes instr modr/m opcode vex.v d64))

              ;; Traverse the instruction table

              ((eq? (vector-ref instr 0) 'Group)
               ;; Read a ModR/M byte and use the fields as opcode
               ;; extension.
               (collect (tag opcode) opcode)
               (let* ((modr/m (get-u8/collect port collect (tag modr/m)))
                      (v (vector-ref instr (if (and (> (vector-length instr) 3)
                                                    (= (ModR/M-mod modr/m) #b11))
                                               3 2)))
                      (instr (vector-ref v (ModR/M-reg modr/m))))
                 (cond ((and (vector? instr) (= (vector-length instr) 8))
                        (when debug (print-modr/m modr/m prefixes))
                        (lp (vector-ref instr (ModR/M-r/m modr/m))
                            'ModR/M-invalid opcode prefixes
                            #t vex-traversed d64))
                       (else
                        (lp instr modr/m opcode prefixes
                            #t vex-traversed d64)))))

              ((eq? (vector-ref instr 0) 'Prefix)
               ;; SSE instructions, e.g., where one of these prefixes
               ;; is considered part of the opcode. "Vanligt
               ;; REP-prefix kan vara DÖDLIG SSE--vi har hela listan".
               (lp (vector-ref instr
                               (cond ((enum-set-member? (prefix repz) prefixes) 2)
                                     ((enum-set-member? (prefix repnz) prefixes) 4)
                                     ((enum-set-member? (prefix operand) prefixes) 3)
                                     (else 1)))
                   modr/m opcode
                   (enum-set-difference prefixes (prefix-set repz repnz operand))
                   opcode-collected vex-traversed d64))

              ((eq? (vector-ref instr 0) 'Datasize)
               ;; Pick different instructions depending on
               ;; effective operand size.
               (lp (vector-ref instr
                               (case mode
                                 ((64)
                                  (cond ((enum-set-member? (prefix rex.w) prefixes) 3)
                                        ((enum-set-member? (prefix operand) prefixes) 1)
                                        (else 2)))
                                 ((32)
                                  (cond ((enum-set-member? (prefix operand) prefixes) 1)
                                        (else 2)))
                                 ((16)
                                  (cond ((enum-set-member? (prefix operand) prefixes) 2)
                                        (else 1)))))
                   modr/m opcode
                   prefixes
                   opcode-collected vex-traversed d64))

              ((eq? (vector-ref instr 0) 'Addrsize)
               (lp (vector-ref instr
                               (case mode
                                 ((64) (if (enum-set-member? (prefix address) prefixes) 2 3))
                                 ((32) (if (enum-set-member? (prefix address) prefixes) 1 2))
                                 ((16) (if (enum-set-member? (prefix address) prefixes) 2 1))))
                   modr/m opcode
                   prefixes
                   opcode-collected vex-traversed d64))

              ((eq? (vector-ref instr 0) 'Mode)
               ;; Choose between compatibility/legacy mode and
               ;; long mode.
               (lp (vector-ref instr (if (= mode 64) 2 1))
                   modr/m opcode
                   prefixes
                   opcode-collected vex-traversed d64))

              ((eq? (vector-ref instr 0) 'VEX)
               (lp (vector-ref instr
                               (cond ((enum-set-member? (prefix vex.l) prefixes)
                                      (if (> (vector-length instr) 3) 3 2)) ;256-bit
                                     ((enum-set-member? (prefix vex) prefixes) 2) ;128-bit
                                     (else 1)))
                   modr/m opcode
                   prefixes
                   opcode-collected #t d64))

              ((eq? (vector-ref instr 0) 'Mem/reg)
               ;; Read ModR/M and see if it encodes memory or a
               ;; register. Used for the MOVLPS/MOVHLPS and
               ;; MOVHPS/MOVLHPS instructions (mnemonics differ) and
               ;; VMOVSD (operands differ).
               (let ((modr/m (get-u8 port)))
                 (collect (tag opcode) opcode)
                 (collect (tag modr/m) modr/m)
                 (lp (vector-ref instr
                                 (cond ((= (ModR/M-mod modr/m) #b11) 2) ;register
                                       (else 1)))
                     modr/m opcode
                     prefixes
                     #t vex-traversed d64)))

              ((eq? (vector-ref instr 0) 'f64)
               ;; Operand size is forced to 64 bits in 64-bit mode.
               (lp (vector-ref instr 1)
                   modr/m opcode
                   (if (= mode 64)
                       (enum-set-difference prefixes (prefix-set operand rex.w))
                       prefixes)
                   opcode-collected vex-traversed #t))

              ((eq? (vector-ref instr 0) 'd64)
               ;; In 64-bit mode, the default operand size is 64
               ;; bits. The only other possible operand size is then
               ;; 16 bits.
               (lp (vector-ref instr 1)
                   modr/m opcode
                   prefixes
                   opcode-collected vex-traversed #t))

              ((eq? (vector-ref instr 0) 'Prefix/eos)
               ;; This is for 0F 38 F0/F1 (MOVBE/CRC32). These opcodes
               ;; look a lot like SSE, but they use 66 to change the
               ;; effective operand size.
               (lp (vector-ref instr
                               (cond ((enum-set-member? (prefix repnz) prefixes) 2)
                                     (else 1)))
                   modr/m opcode
                   (enum-set-difference prefixes (prefix-set repz repnz))
                   opcode-collected vex-traversed d64))

              ((eq? (vector-ref instr 0) 'W)
               ;; This handles the case where two of the operands change
               ;; order based on REX.W. This is used in VEX/XOP to
               ;; enable the memory operand to be either one of two
               ;; operands.
               (lp (vector-ref instr
                               (cond ((enum-set-member? (prefix rex.w) prefixes) 2)
                                     (else 1)))
                   modr/m opcode
                   (enum-set-difference prefixes (prefix-set rex.w))
                   opcode-collected vex-traversed d64))

              (else
               (collect (tag opcode) opcode)
               (let ((opcode (get-u8 port)))
                 ;; A new opcode table (two-byte or three-byte opcode)
                 (lp (vector-ref instr opcode)
                     modr/m opcode
                     prefixes
                     #f vex-traversed d64))))))))

    ;; Read the next instruction from the given port, using the given
    ;; bit mode (16, 32 or 64). The `collect' argument is either #f, or
    ;; a function which accepts any number of arguments: the first
    ;; argument is a type tag, and the following arguments are bytes.
    ;; All bytes read from the port will be passed to the collector.
    (define (get-instruction port mode collect)
      (let ((collect (or collect (lambda x #f))))
        (if (eof-object? (lookahead-u8 port))
            (eof-object)
            (get-instruction* port mode collect))))

    (get-instruction (if (bytevector? bv)
                         (open-bytevector-input-port bv)
                         bv)
                     64 collect)))
