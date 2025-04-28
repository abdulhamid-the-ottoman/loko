;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; AMD64 code generator

;; XXX: Registers mustn't contain invalid bit patterns (e.g. unshifted
;; fixnums) and be live while calling anything that can invoke the GC.
;; Generally this means that primitives completely evaluate their
;; arguments before doing anything to the values.

(library (loko arch amd64 codegen)
  (export
    codegen)
  (import
    (loko arch amd64 config)
    (loko arch amd64 objects)
    (loko runtime context)
    (loko compiler recordize)
    (loko compiler utils)
    (rename (loko runtime utils) (map-in-order map))
    (loko match)
    (except (rnrs) map)
    (only (rnrs mutable-pairs) set-car!))

;; codes is a list of records. Returns a list of assembler text and
;; a list of assembler data.
(define (codegen codes primlocs make-init-code? codegen-options)
  (define who 'simple-cg)
  (define debug #f)
  (define code '())
  (define data '())
  (define bss-globals '())
  (define globals (make-eq-hashtable))
  (define use-branch-instrumentation
    (cond ((assq 'coverage codegen-options) => (lambda (x) (eq? (cdr x) 'afl++)))
          (else #f)))
  ;; This works for all supported targets. Linux doesn't support the
  ;; 7th argument. NetBSD's src/sys/arch/amd64/include/frame_regs.h
  ;; claims support for ten arguments, but src/sys/syscallargs.h
  ;; claims support for eight arguments, but seven arguments are the
  ;; most arguments actually used in src/sys/sys/syscallargs.h.
  (define syscall-args '(rax rdi rsi rdx r10 r8 r9 (mem64+ rsp -8)))

  (define (pvec i) (+ (* 8 i) (- (tag 'vector))))

  (define (get-global name)
    (or (hashtable-ref globals name #f)
        (let ((offset (* 8 (+ PROCESS-VECTOR-OFFSET (hashtable-size globals)))))
          (hashtable-set! globals name offset)
          offset)))

  (define (init-globals!)
    (let-values (((k* v*) (hashtable-entries globals)))
      (vector-for-each
       (lambda (k v)
         (let ((gensym-idx (hashtable-ref gensym-idx-locations k #f)))
           ;; (write (list k v gensym-idx))
           ;; (newline)
           ;; The key is #{standard-input-port |U35CrPcjZ=PvgdY9|}
           ;; and the value is an offset into the process vector.
           ;; gensym-idx is initially (%u64 31). The library manager
           ;; stuff has the gensym in some table, so to reach the
           ;; value stored in the global environment the gensym
           ;; contains an index into the global environment.
           (when gensym-idx
             (set-car! (cdr gensym-idx) v))))
       k* v*))
    ;; Used in scheme-init when allocating the pcb
    (emit-data `(%equiv initial-pcb-length
                        ,(immediate (+ (+ PROCESS-VECTOR-OFFSET 1)
                                       (hashtable-size globals))))))

  ;; Tables for encode-object.
  (define (encode-const const)
    (encode-object const objs strings symbols gensyms bytevectors rtds
                   gensym-idx-locations emit-data))
  (define objs (make-hashtable equal-hash equal?))
  (define symbols (make-eq-hashtable)) ;symbol interning
  (define gensyms (make-eq-hashtable))
  (define strings (make-hashtable string-hash string=?))
  (define bytevectors (make-hashtable equal-hash bytevector=?))
  (define rtds (make-hashtable equal-hash equal?))  ;collecting for the bootstrap rtd table
  (define gensym-idx-locations (make-eq-hashtable))

  ;; Pseudo-registers.
  (define *pcounter* -1)
  (define (reg)
    (set! *pcounter* (fx+ *pcounter* 1))
    `(reg ,*pcounter*))
  (define (reset-pseudo-register-counter!)
    (set! *pcounter* -1))
  (define *lcounter* -1)
  (define (mklabel name)
    (set! *lcounter* (fx+ *lcounter* 1))
    (vector name *lcounter*))
  ;; Various explicit type checks (not complete!)
  (define explicit-checks #f)
  (define explicit-vector-checks #t)  ;for debugability

  ;; This takes a stack index, an environment and a variable. The
  ;; variable is given a location in a pseudo register. The golden
  ;; rule for this concept is that only proper objects (that the GC
  ;; can't choke on) are allowed in these locations. This is because
  ;; they might be saved on the stack or in a callee-save register.
  (define-syntax with-loc
    (lambda (x)
      (syntax-case x ()
        ((with-loc ((env loc) (old-env var))
           . body)
         (syntax->datum #'var)
         #'(let ((r (reg)))
             (let ((env (extend-env var r old-env))
                   (loc r))
               . body))))))

  (define-syntax with-new-frame
    (lambda (x)
      (syntax-case x (emit call)
        ;; Previously this was used to adjust rsp across calls. Now
        ;; it is used to tell pass-optimize where to insert liveness
        ;; information. It must be safe to spill registers across the
        ;; body.
        ((_ () expr* ... (emit (q (call target))))
         #'(begin
             (emit '(%comment save-live-registers))
             expr* ...
             (emit (q (call target)))
             ;; The optimizer will do liveness analysis and replace
             ;; this comment with a note to the GC. If there is no
             ;; note then the whole frame is live.
             (emit '(%comment liveness-information))
             (emit '(%comment restore-live-registers)))))))

  (define-syntax with-restart-handler
    (lambda (x)
      (syntax-case x (index overflow fxwidth impress fixnum? char? flonum?)
        ((with-restart-handler (type restart raise proceed)
           body ...)
         #'(let ((raise (vector 'raise))
                 (proceed (vector 'proceed)))
             ;; The body expression must return at most one value
             ;; and it should emit a branch to one of the labels.
             (let ((ret (let ()
                          body ...)))
               (emit `(%label ,raise)
                     '(%comment unlikely)
                     ;; It would be better to use int3, but that
                     ;; generates SIGTRAP, which confuses gdb.
                     '(ud2)
                     `(%label ,proceed))
               ret)))
        ((with-restart-handler (type proceed)
           body ...)
         (with-syntax (((restart raise) (generate-temporaries '(restart raise))))
           #'(with-restart-handler (type restart raise proceed)
               body ...))))))

  (define (rflags->SETcc rflags.x)
    (case rflags.x
      ((rflags.z) 'sete)
      ((rflags.s) 'sets)
      ((rflags.l) 'setl)
      ((rflags.g) 'setg)
      ((rflags.o) 'seto)
      ((rflags.ge) 'setge)
      ((rflags.le) 'setle)
      ((rflags.be) 'setbe)
      ((rflags.nz) 'setne)
      ((rflags.ns) 'setns)
      ((rflags.nl) 'setnl)
      ((rflags.ng) 'setng)
      ((rflags.no) 'setno)
      ((rflags.nge) 'setnge)
      ((rflags.nle) 'setnle)
      ((rflags.nbe) 'setnbe)
      (else #f)))

  (define (rflags->Jcc rflags.x)
    ;; XXX: these are inverses, silly enough
    (case rflags.x
      ((rflags.z) 'jne)
      ((rflags.s) 'jns)
      ((rflags.l) 'jnl)
      ((rflags.g) 'jng)
      ((rflags.o) 'jno)
      ((rflags.ge) 'jnge)
      ((rflags.le) 'jnle)
      ((rflags.be) 'jnbe)
      ((rflags.nz) 'jz)
      ((rflags.ns) 'js)
      ((rflags.nl) 'jl)
      ((rflags.ng) 'jg)
      ((rflags.no) 'jo)
      ((rflags.nge) 'jge)
      ((rflags.nle) 'jle)
      ((rflags.nbe) 'jbe)
      (else #f)))

  ;; Emit code
  (define (emit . x*)
    (let lp ((x* x*))
      (unless (null? x*)
        (when debug
          (display "  ")
          (write (car x*))
          (newline))
        (set! code (cons (car x*) code))
        (lp (cdr x*)))))

  (define (emit-data x)
    (when debug
      (display "DATA: ")
      (write x)
      (newline))
    (set! data (cons x data)))

  (define (combinator? x)
    (and (closure? x)
         (null? (closure-free* x))))

  (define (constant? x)
    (or (const? x)
        (and (infer? x)
             (const? (infer-expr x)))))

  (define (constant-value x)
    (if (const? x)
        (const-value x)
        (const-value (infer-expr x))))

  (define (constant-fixnum? x)
    ;; TODO: use target-fixnum?.
    (or (and (const? x)
             (fixnum? (const-value x)))
        (and (infer? x)
             (const? (infer-expr x))
             (fixnum? (const-value (infer-expr x))))))

  (define (inferred-positive-fixnum? x)
    (or (and (const? x)
             (let ((v (const-value x)))
               ;; TODO: use target-fixnum?.
               (and (fixnum? v) (positive? v))))
        (and (infer? x)
             (let ((facts (infer-facts x)))
               (and (memq 'positive facts)
                 (memq 'fixnum facts))))))

  (define (constant-char? x)
    (or (and (const? x)
             (char? (const-value x)))
        (and (infer? x)
             (const? (infer-expr x))
             (char? (const-value (infer-expr x))))))

  (define (constant-s31? op)
    (and (constant? op)
         (s31? (constant-value op))))

  (define (s31? v)
    (and (fixnum? v)
         (fx<=? (- (expt 2 31)) v (- (expt 2 31) 1))))

  (define compile-time-random-u16
    (and use-branch-instrumentation (make-random-u16-maker)))

  (define (cg-branch-instrumentation)
    ;; AFL instrumentation.
    (when use-branch-instrumentation
      (let (#;(r (reg))
            (current-location (compile-time-random-u16)))
        (emit `(mov rcx (mem64+ afl-location))
              `(xor cx ,current-location)
              `(inc (mem8+ afl-map rcx))
              `(shr cx 1)
              `(mov (mem64+ afl-location) rcx)))))

  (define (cg-get-i/o eax operand* ctxt env)
    (let ((port (car operand*)))
      (if (constant? port)
          (emit `(mov edx ,(constant-value port)))
          (emit `(mov rdx ,(cg port 'value env #f))
                `(sar edx ,(shift 'fixnum))))
      (unless (eq? eax 'eax)
        (emit `(xor eax eax)))
      (emit `(in ,eax dx)
            `(shl rax ,(shift 'fixnum)))
      'rax))

  (define (cg-get-i/o-n! mem+ operand* ctxt env)
    ;; get-i/o-uXX-n! port address count.
    (let ((port (car operand*))
          (address (cadr operand*))
          (count (caddr operand*)))
      (let ((Tcount (reg)) (Taddress (reg)))
        (emit `(mov ,Tcount ,(cg count 'value env #f)))
        (emit `(mov ,Taddress ,(cg address 'value env #f)))
        (emit `(mov rdx ,(cg port 'value env #f))
              `(sar edx ,(shift 'fixnum)))
        ;; Load rcx with the number of uXX units to be read.
        (emit `(mov rcx ,Tcount)
              `(sar rcx ,(fx+ (shift 'fixnum)
                              (case mem+
                                ((mem8+) 0)
                                ((mem16+) 1)
                                ((mem32+) 2))))
              `(mov rdi ,Taddress)
              `(sar rdi ,(shift 'fixnum)) ;address, in bytes
              `(rep.ins (,mem+ rdi) dx))
        (cg-void ctxt))))

  (define (cg-put-i/o eax operand* ctxt env)
    (let ((port (car operand*))
          (value (cadr operand*)))
      (cond ((and (constant? port)
                  (integer? (constant-value port))
                  (< (constant-value port) 256))
             (let ((port (exact (constant-value port))))
               (emit `(mov rax ,(cg value 'value env #f))
                     `(sar eax ,(shift 'fixnum))
                     `(out ,port ,eax))))
            (else
             (let ((reg-port (reg)) (reg-value (reg)))
               (emit `(mov ,reg-port ,(cg port 'value env #f)))
               (emit `(mov ,reg-value ,(cg value 'value env #f)))
               (emit `(mov rdx ,reg-port)
                     `(sar edx ,(shift 'fixnum))
                     `(mov rax ,reg-value)
                     `(sar rax ,(shift 'fixnum))
                     `(out dx ,eax)))))
      (cg-void ctxt)))

  (define (cg-put-i/o-n mem+ operand* ctxt env)
    ;; put-i/o-uXX-n port address count.
    (match operand*
      [(port address count)
       (let ((Tcount (reg)) (Taddress (reg)))
         (emit `(mov ,Tcount ,(cg count 'value env #f)))
         (emit `(mov ,Taddress ,(cg address 'value env #f)))
         (emit `(mov rdx ,(cg port 'value env #f))
               `(sar edx ,(shift 'fixnum)))
         ;; Load rcx with the number of uXX units to be written.
         (emit `(mov rcx ,Tcount)
               `(sar rcx ,(fx+ (shift 'fixnum)
                               (case mem+
                                 ((mem8+) 0)
                                 ((mem16+) 1)
                                 ((mem32+) 2))))
               `(mov rsi ,Taddress)
               `(sar rsi ,(shift 'fixnum)) ;address, in bytes
               `(rep.outs dx (,mem+ rsi)))
         (cg-void ctxt))]))

  (define (cg-get-mem mem+ movzx/sx reg operand* ctxt env)
    ;; (get-mem-uNN addr). addr must be aligned. XXX: make sure
    ;; that the optimizer never removes these loads.
    (let ((addr (car operand*)))
      (cond ((and (constant? addr) (< (constant-value addr) #x7fffffff))
             (emit `(,movzx/sx ,reg (,mem+ ,(constant-value addr)))))
            (else
             (emit `(mov rax ,(cg addr 'value env #f)))
             (emit `(sar rax ,(shift 'fixnum)))
             (emit `(,movzx/sx ,reg (,mem+ rax)))))
      (emit `(sal rax ,(shift 'fixnum)))
      'rax))

  (define (cg-put-mem mem+ rax-part operand* ctxt env)
    (let ((addr (car operand*)) (value (cadr operand*)))
      (let ((loc (reg)) (data (reg)))
        (emit `(mov ,loc ,(cg value 'value env #f)))
        (emit `(mov ,data ,(cg addr 'value env #f)))
        (emit `(sar ,data ,(shift 'fixnum)))
        (emit `(mov rax ,loc))
        (emit `(sar rax ,(shift 'fixnum)))
        (emit `(mov (,mem+ ,data) ,rax-part))
        (cg-void ctxt))))

  (define (%cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op)
    ;; Partially range check the index. Case analysis: if n is near
    ;; greatest-fixnum, then n+1 or n+3 wraps around to a negative
    ;; index. If n>=0 and near zero, the comparison works as
    ;; expected. If n<-3 the comparison also works (because it is an
    ;; unsigned comparison). If n+1>=0 or n+3>=0, then it does not
    ;; work as expected, but the index is misaligned, so this will
    ;; be caught anyway.
    (let ((r (if (eq? mem+ 'mem8+)
                 idx-reg
                 (let ((tmp (reg)))
                   (emit `(lea ,tmp (mem+ ,idx-reg
                                          ,(immediate (case mem+
                                                        ((mem16+) 1)
                                                        ((mem32+) 3)
                                                        (else 7))))))
                   tmp))))
      ;; Type check bv, verify index in bounds.
      (with-restart-handler (index proceed)
        (emit `(cmp ,r (mem64+ ,bv-reg ,(fx- (tag 'bytevector))))
              `(jb ,proceed)))))

  (define (cg-helper-bv-idx-check/u8 mem+ bv-reg idx-reg idx-op)
    ;; Check for a non-negative fixnum.
    (if (inferred-as? idx-op 'fixnum)
        (with-restart-handler (index proceed)
          (emit `(test ,idx-reg ,idx-reg)
                `(jns ,proceed)))
        (let ((m (reg)))
          (with-restart-handler (index proceed)
            (emit `(mov ,m ,(bitwise-ior (mask 'fixnum)
                                         (expt 2 63)))
                  `(test ,idx-reg ,m)
                  `(jz ,proceed)))))
    (%cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op))

  (define (cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op)
    ;; Type check index.
    (cg-check-fixnum? idx-reg idx-op)
    (%cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op))

  (define (cg-bytevector-native-set! mem+ mvreg class bits
                                     operand* ctxt env)
    ;; Verify these evil things: idx is a fixnum, idx is in range of
    ;; the bytevector length, the value is in the proper range.
    (assert (null? (cdddr operand*)))
    (let ((bv-reg (reg)) (idx-reg (reg)) (n-reg 'rdx) (n-reg/32 'edx))
      (emit `(mov ,bv-reg ,(cg (car operand*) 'value env #f)))
      (emit `(mov ,idx-reg ,(cg (cadr operand*) 'value env #f)))
      (emit `(mov ,n-reg ,(cg (caddr operand*) 'value env #f)))
      (cg-helper-bv-idx-check mem+ bv-reg idx-reg (cadr operand*))
      ;; Check that the number is ok.
      (case class
        ((ieee)
         ;; Type check.
         (with-restart-handler (bytevector-value proceed)
           (emit `(cmp ,n-reg/32 ,(tag 'flonum))
                 `(jz ,proceed))))
        ((u)
         ;; Combined type and range check.
         (let ((m (reg)))
           (emit `(mov ,m ,(bitwise-not (immediate (- (expt 2 bits) 1)))))
           (with-restart-handler (bytevector-value proceed)
             (emit `(test ,n-reg ,m)
                   `(jz ,proceed)))))
        (else
         (let ((tmp (reg)))
           (cg-check-fixnum? n-reg (caddr operand*))
           (cond ((fx<=? bits 16)
                  ;; Standard range check trick.
                  (emit `(lea ,tmp (mem+ ,n-reg ,(immediate (expt 2 (- bits 1))))))
                  (with-restart-handler (bytevector-value proceed)
                    (emit `(cmp ,tmp ,(immediate (- (expt 2 bits) 1)))
                          `(jbe ,proceed))))
                 (else
                  (emit `(mov ,tmp ,n-reg))
                  ;; Shift off everything except the sign, and
                  ;; see if it's 0 or 1.
                  (emit `(sar ,tmp ,(+ (shift 'fixnum) bits -1))
                        `(add ,tmp 1))
                  (with-restart-handler (bytevector-value proceed)
                    (emit `(cmp ,tmp 1)
                          `(jbe ,proceed))))))))
      ;; Do the write.
      (case class
        ((ieee)
         (emit `(shr ,n-reg ,(shift 'flonum))))
        (else
         (emit `(sar ,n-reg ,(shift 'fixnum))))) ;as mvreg
      ;; XXX: Checks alignment of idx, and is also part of the range
      ;; check.
      (emit `(sar ,idx-reg ,(shift 'fixnum))
            `(mov (,mem+ ,bv-reg ,idx-reg 8 8 ,(fx- (tag 'bytevector))) ,mvreg))
      (cg-void ctxt)))

  ;; TODO: do more testing of this code (in particular the range check).
  (define (cg-bv-known-ref name bv idx endian ctxt env)
    ;; The endianness is known to be little or big, and the number
    ;; of operands is correct (endianness always present).
    (define r32 'eax)                 ;TODO: classed pseudo regs
    (define r64 'rax)
    (let-values
        (((native? size mask class mov mvreg mem+)
          (case name
            ((bytevector-u8-ref)         (values #t 1 #b000 'u 'movzx  r32 'mem8+))
            ((bytevector-s8-ref)         (values #t 1 #b000 's 'movsx  r64 'mem8+))
            ((bytevector-u16-ref)        (values #f 2 #b001 'u 'movzx  r32 'mem16+))
            ((bytevector-s16-ref)        (values #f 2 #b001 's 'movsx  r64 'mem16+))
            ((bytevector-u32-ref)        (values #f 4 #b011 'u 'mov    r32 'mem32+))
            ((bytevector-s32-ref)        (values #f 4 #b011 's 'movsxd r64 'mem32+))
            ((bytevector-u16-native-ref) (values #t 2 #b001 'u 'movzx  r32 'mem16+))
            ((bytevector-s16-native-ref) (values #t 2 #b001 's 'movsx  r64 'mem16+))
            ((bytevector-u32-native-ref) (values #t 4 #b011 'u 'mov    r32 'mem32+))
            ((bytevector-s32-native-ref) (values #t 4 #b011 's 'movsxd r64 'mem32+))
            ((bytevector-ieee-single-native-ref) (values #t 4 #b011 'ieee 'mov r32 'mem32+))
            (else
             (error 'cg-bv-known-ref "Not prepared to handle this" name)))))
      (define (do-static-index-unaligned bv-reg idx bswap?)
        ;; idx is a register or a fixnum.
        (assert (memq class '(s u)))
        (case size
          ((2)
           ;; Two u8 reads
           (let-values ([(i0 i1) (if bswap? (values 0 1) (values 1 0))])
             (let ((addr0 `(mem8+ ,bv-reg ,idx ,i0 8 8 ,(fx- (tag 'bytevector))))
                   (addr1 `(mem8+ ,bv-reg ,idx ,i1 8 8 ,(fx- (tag 'bytevector)))))
               (emit `(,mov ,mvreg ,addr0)
                     `(sal rax 8)
                     `(mov al ,addr1))
               (emit `(sal rax ,(shift 'fixnum)))
               'rax)))
          ((4)
           (cond
             ((fixnum? idx)
              ;; Two aligned u32 reads, shifting the result.
              (let* ((idx^ (fxand idx (fxnot mask)))
                     (a (fx* 8 (fx- idx idx^))))  ;8, 16, 24
                ;; Load the requested bytes into eax:edx,
                ;; then shift them into eax.
                (emit `(mov eax (mem32+ ,bv-reg ,idx^ 0 8 8 ,(fx- (tag 'bytevector))))
                      `(mov edx (mem32+ ,bv-reg ,idx^ 4 8 8 ,(fx- (tag 'bytevector)))))
                (emit `(shrd eax edx ,a))
                (assert (eq? r32 'eax))
                ;; Now do sign extension and endianness swapping.
                (when bswap?
                  (emit '(bswap eax)))))
             (else
              ;; Four u8 reads.
              (let-values ([(i0 i1 i2 i3) (if bswap? (values 0 1 2 3) (values 3 2 1 0))])
                (let ((addr0 `(mem8+ ,bv-reg ,idx ,i0 8 8 ,(fx- (tag 'bytevector))))
                      (addr1 `(mem8+ ,bv-reg ,idx ,i1 8 8 ,(fx- (tag 'bytevector))))
                      (addr2 `(mem8+ ,bv-reg ,idx ,i2 8 8 ,(fx- (tag 'bytevector))))
                      (addr3 `(mem8+ ,bv-reg ,idx ,i3 8 8 ,(fx- (tag 'bytevector)))))
                  (emit `(movzx eax ,addr0)
                        `(sal eax 8)
                        `(mov al ,addr1)
                        `(sal eax 8)
                        `(mov al ,addr2)
                        `(sal eax 8)
                        `(mov al ,addr3))))))
           ;; Still u32/s32
           (when (eq? class 's)
             (emit '(movsxd rax eax)))
           (emit `(sal rax ,(shift 'fixnum)))
           'rax)))
      (define (do-static-index bv-reg idx bswap?)
        ;; The index is static.
        (let ((idx (constant-value idx)))
          (with-restart-handler (index proceed)
            ;; Verify the index and implicitly check that it's a bytevector.
            (emit `(cmp (mem64+ ,bv-reg ,(fx- (tag 'bytevector)))
                        ,(immediate (+ idx size -1)))
                  `(jnbe ,proceed)))
          (cond
            ((or native? (zero? (fxand idx mask)))
             ;; The index is aligned or can be assumed to be.
             (let ((mref `(,mem+ ,bv-reg ,idx 8 8 ,(fx- (tag 'bytevector)))))
               ;; Do the memory reference, possibly followed
               ;; by endianness swapping. Return a register.
               (cond
                 ((or (eqv? size 1) (not bswap?))
                  (emit `(,mov ,mvreg ,mref))
                  (emit `(sal ,r64 ,(shift 'fixnum)))
                  r64)
                 ((eq? class 's)
                  ;; The sign bit is only in the right place
                  ;; after the xchg/bswap.
                  (case size
                    ((2)
                     (emit `(movzx ax ,mref)
                           `(xchg al ah)
                           `(,mov ,mvreg ax)))
                    ((4)
                     (emit `(mov eax ,mref)
                           `(bswap eax)
                           `(,mov ,mvreg eax))))
                  (emit `(sal ,r64 ,(shift 'fixnum)))
                  r64)
                 (else
                  ;; Unsigned reference.
                  (emit `(,mov ,mvreg ,mref))
                  (case size
                    ((2)
                     (assert (eq? r32 'eax))
                     (emit `(xchg al ah)))
                    ((4)
                     (emit `(bswap ,r32))))
                  (emit `(sal ,r64 ,(shift 'fixnum)))
                  r64))))
            (else
             ;; The index is not aligned.
             (do-static-index-unaligned bv-reg idx bswap?)))))
      (define (do-dynamic-index bv-reg idx bswap?)
        ;; The index is dynamic.
        (let ((idx-reg (reg)))
          (emit `(mov ,idx-reg ,(cg idx 'value env #f)))
          (cond
            (native?
             ;; The index can be assumed to be aligned. Check the
             ;; index and also that it's a bytevector.
             (cg-helper-bv-idx-check mem+ bv-reg idx-reg idx)
             (emit `(sar ,idx-reg ,(shift 'fixnum)))
             ;; XXX: Checks alignment of idx, and is also part
             ;; of the range check.
             (emit `(,mov ,mvreg (,mem+ ,bv-reg ,idx-reg 8 8 ,(fx- (tag 'bytevector)))))
             ;; FIXME: what of the bswap?
             (case class
               ((ieee)
                (emit `(sal ,r64 ,(shift 'flonum))
                      `(or ,r64 ,(tag 'flonum))))
               (else
                (emit `(sal ,r64 ,(shift 'fixnum)))))
             r64)
            (else
             ;; The index might not be aligned. The memory
             ;; references generated here will not help trap -1
             ;; and -3.
             (cg-helper-bv-idx-check/u8 mem+ bv-reg idx-reg idx)
             (emit `(sar ,idx-reg ,(shift 'fixnum)))
             (do-static-index-unaligned bv-reg idx-reg bswap?)))))
      ;; Static or dynamic index argument.
      (let ((bv-reg (reg))
            (bswap? (not (eq? endian 'little))))
        (emit `(mov ,bv-reg ,(cg bv 'value env #f)))
        (cond ((and (constant? idx) (fixnum? (constant-value idx))
                    (< -1 (constant-value idx) (expt 2 30))
                    (memq class '(s u)))
               (do-static-index bv-reg idx bswap?))
              (else
               (do-dynamic-index bv-reg idx bswap?))))))

  (define (cg-syscall return-pair operand* ctxt env)
    ;; The syscall instruction.
    (define reg* syscall-args)
    (assert (fx<=? 1 (length operand*) (length reg*)))
    (let ((acc (reg))
          (ret-reg (and return-pair (reg)))
          (preg* (let lp ((operand* operand*) (preg* '()))
                   (if (null? operand*)
                       (reverse preg*)
                       (let ((preg (reg)))
                         (emit `(mov ,preg ,(cg (car operand*) 'value env #f)))
                         (lp (cdr operand*) (cons preg preg*)))))))
      (when return-pair
        (emit `(mov ,ret-reg ,(cg return-pair 'value env #f))))
      (emit `(mov ,acc 0))          ;for mass typechecking of the operands
      (let lp ((operand* operand*) (reg* reg*) (preg* preg*))
        (when (pair? operand*)
          (let ((operand (car operand*)) (hreg (car reg*)) (preg (car preg*)))
            (cond
              ((constant-fixnum? operand)
               (emit `(mov ,hreg ,(constant-value operand))))
              (else
               (emit `(mov ,hreg ,preg))
               (unless (inferred-as? operand 'fixnum)
                 (emit `(or ,acc ,preg)))
               (emit `(sar ,hreg ,(shift 'fixnum)))))
            (lp (cdr operand*) (cdr reg*) (cdr preg*)))))
      (with-restart-handler (fixnum? proceed)
        (emit `(test ,acc ,(mask 'fixnum))
              `(jz ,proceed)))
      (let* ((num-stack-args (fxmax 0 (fx- (length operand*)
                                           (length (filter symbol? reg*)))))
             (stack-disp (if (eqv? num-stack-args 0)
                             0
                             (fx* 8 (fx+ num-stack-args 1)))))
        (unless (eqv? stack-disp 0)
          (emit `(sub rsp ,stack-disp)))
        (emit '(%comment likely)
              `(%comment call syscall ,(length operand*))
              ;; rcx and r11 are clobbered by syscall; *BSD sets carry
              `(syscall))
        (cond (return-pair
               ;; TODO: This is a temporary solution for returning
               ;; multiple values without actually doing so. See the
               ;; wip/values branch for ongoing work to fix this. An
               ;; alternative would be to let syscall/carry be like if.
               (emit `(mov edx ,(immediate #f))
                     `(setc dh)
                     `(sal rax ,(shift 'fixnum)))
               (unless (eqv? 0 stack-disp)
                 (emit `(add rsp ,stack-disp)))
               (emit `(mov (mem64+ ,ret-reg ,(fx- (car-offset) (tag 'pair))) rdx)
                     `(mov (mem64+ ,ret-reg ,(fx- (cdr-offset) (tag 'pair))) rax))
               (cg-void ctxt))
              (else
               ;; TODO: might need to construct a bignum. should maybe
               ;; return two values instead?
               (emit `(sal rax ,(shift 'fixnum)))
               (unless (eqv? 0 stack-disp)
                 (emit `(add rsp ,stack-disp)))
               'rax)))))

  (define (cg-test ctxt rflags.x)
    ;; Used after rflags has been updated. When not called from
    ;; inside an if expression it constructs a boolean.
    (match ctxt
      [('test consequence alternative)
       (let ((Jcc (rflags->Jcc rflags.x))) ;XXX: inverse
         (emit `(,Jcc ,alternative))
         (emit `(jmp ,consequence))
         '*branched*)]
      [_
       ;; Turn the flags test into a boolean object
       (let ((setcc (or (rflags->SETcc rflags.x)
                        (error 'cg-test "Unknown flag" rflags.x))))
         ;; Can only use ah, bh, ch or dh.
         (emit `(mov rax ,(immediate #f)))
         (emit `(,setcc ah)))
       'rax]))

  (define (cg-fxdiv-and-mod n-reg d euclidean?)
    ;; TODO: Can the magic constants do the Eucliedan adjustments?
    (let-values ([(M s) (division-magic d 64)])
      (let* ((M-reg (reg)) (r (reg)))
        (emit `(sar ,n-reg ,(shift 'fixnum)))
        (emit `(mov ,r ,n-reg))
        (when euclidean?
          ;; if    d<0 && n<0: n = n + d + 1
          ;; elif: d>0 && n<0: n = n - d + 1
          (cond ((negative? d)
                 ;; if n<0: n = n + d + 1
                 ;; n+=(n>>w)&(d+1)
                 (let ((skip (vector 'div-skip))
                       (c-reg (reg)))
                   (emit `(test ,n-reg ,n-reg)
                         `(jns ,skip))
                   (emit `(mov ,c-reg ,(+ d 1))
                         `(add ,n-reg ,c-reg))
                   (emit `(%label ,skip))))
                (else
                 ;; if n<0: n = n - d + 1
                 (let ((skip (vector 'div-skip))
                       (c-reg (reg)))
                   (emit `(test ,n-reg ,n-reg)
                         `(jns ,skip))
                   (emit `(mov ,c-reg ,(+ (- d) 1))
                         `(add ,n-reg ,c-reg))
                   (emit `(%label ,skip))))))
        ;; TODO: would it make sense to swap around the args to IMUL?
        (emit `(mov ,M-reg ,M))
        (emit `(mov rax ,n-reg))
        (emit `(imul ,M-reg))       ;rdx:rax <- rax * M-reg
        (cond ((and (> d 0) (< M 0))
               (emit `(add rdx ,n-reg)))
              ((and (< d 0) (> M 0))
               (emit `(sub rdx ,n-reg))))
        (unless (eqv? s 0)
          (emit `(sar rdx ,s)))
        ;; The quotient is now in rdx. Add one to rdx if rdx < 0.
        (let ((t (reg)))
          (emit `(mov ,t rdx)
                `(shr ,t ,(- 64 1))
                `(add rdx ,t)))
        ;; The remainder ends up in r.
        (let ((t (reg)))
          ;; TODO: adjust rdx before? maybe copy unadjusted n into r, too?
          (cond ((s31? d)
                 (emit `(imul ,t rdx ,d)))
                (else
                 (let ((d-reg (reg)))
                   (emit `(mov ,t rdx)
                         `(mov ,d-reg ,d)
                         `(imul ,t ,d-reg)))))
          (emit `(sub ,r ,t)))
        (emit `(sal rdx ,(shift 'fixnum))
              `(sal ,r ,(shift 'fixnum)))
        (values 'rdx r))))

  (define (cg-fxcmp-predicate label name operand* ctxt env tail?)
    ;; Generates code for fx=?, fx<?, etc.
    (match operand*
      [(a b)
       (let ((condition (case name
                          ((fx=?) 'rflags.z)
                          ((fx<?) 'rflags.l)
                          ((fx>?) 'rflags.g)
                          ((fx>=?) 'rflags.ge)
                          ((fx<=?) 'rflags.le)
                          (else
                           (error 'cg-fxcmp-predicate "Internal error"))))
             (op1 (reg)) (op2 (reg)))
         (emit `(mov ,op1 ,(cg a 'value env #f)))
         (emit `(mov ,op2 ,(cg b 'value env #f)))
         (cg-check-fixnum? op1 a
                           op2 b)
         (emit `(cmp ,op1 ,op2))
         (cg-test ctxt condition))]
      [(a x b)
       (if (and (eq? name 'fx<=?)
                (constant-fixnum? a)
                (constant-s31? b)
                (s31? (- (constant-value a)))
                (fx<=? (constant-value a) (constant-value b)))
           ;; Hacker's Delight 4-1. a ≤ x ≤ b  ==>  x-a ≤u b-a
           (let ((a^ (constant-value a))
                 (b^ (constant-value b))
                 (t0 (reg))
                 (t1 (reg))
                 (t2 (reg)))
             (emit `(mov ,t0 ,(cg x 'value env #f)))
             (cg-check-fixnum? t0 x)
             (cond ((eqv? a^ 0)
                    (emit `(mov ,t1 ,t0)))
                   (else
                    (emit `(lea ,t1 (mem+ ,t0 ,(immediate (- a^)))))))
             (emit `(mov ,t2 ,(immediate (fx- b^ a^)))
                   `(cmp ,t1 ,t2))
             (cg-test ctxt 'rflags.be))
           (cg-primcall-proc label name operand* ctxt env tail?))]
      [_
       (cg-primcall-proc label name operand* ctxt env tail?)]))

  (define (cg-chcmp-predicate label name operand* ctxt env tail?)
    ;; Generates code for char=?, char<?, etc.
    (match operand*
      [(a b)
       (let ((condition (case name
                          ((char=?) 'rflags.z)
                          ((char<?) 'rflags.l)
                          ((char>?) 'rflags.g)
                          ((char>=?) 'rflags.ge)
                          ((char<=?) 'rflags.le)
                          (else
                           (error 'cg-chcmp-predicate "Internal error"))))
             (op1 (reg)) (op2 (reg)))
         (emit `(mov ,op1 ,(cg a 'value env #f)))
         (emit `(mov ,op2 ,(cg b 'value env #f)))
         (cg-check-char? op1 a
                         op2 b)
         (emit `(cmp ,op1 ,op2))
         (cg-test ctxt condition))]
      [(a x b)
       (if (and (eq? name 'char<=?)
                (constant-char? a)
                (constant-char? b)
                (char<=? (constant-value a) (constant-value b)))
           ;; Hacker's Delight 4-1. a ≤ x ≤ b  ==>  x-a ≤u b-a
           (let ((a^ (char->integer (constant-value a)))
                 (b^ (char->integer (constant-value b)))
                 (t0 (reg))
                 (t1 (reg))
                 (t2 (reg)))
             (emit `(mov ,t0 ,(cg x 'value env #f)))
             (cg-check-char? t0 x)
             (emit `(shr ,t0 ,(fx- (shift 'char) (shift 'fixnum))))
             (cond ((eqv? a^ 0)
                    (emit `(mov ,t1 ,t0)))
                   (else
                    (emit `(lea ,t1 (mem+ ,t0 ,(immediate (- a^)))))))
             (emit `(mov ,t2 ,(immediate (fx- b^ a^)))
                   `(cmp ,t1 ,t2))
             (cg-test ctxt 'rflags.be))
           (cg-primcall-proc label name operand* ctxt env tail?))]
      [_
       (cg-primcall-proc label name operand* ctxt env tail?)]))

  (define (cg-type-predicate operand* type-mask type-tag ctxt env)
    ;; TODO: partial access of pseudoregisters
    (assert (and (fixnum? type-mask) (fixnum? type-tag)))
    (emit `(mov rax ,(cg (car operand*) 'value env #f)))
    (let ((M type-mask) (T type-tag))
      (cond ((fxzero? T)
             (emit `(test eax ,M)))
            ((fx=? M #xff)
             (emit `(cmp al ,T)))
            ((fx<=? M #xff)
             (emit `(and al ,M)
                   `(cmp al ,T)))
            (else
             (emit `(and eax ,M)
                   `(cmp eax ,T)))))
    (cg-test ctxt 'rflags.z))

  (define (cg-const-predicate operand* v ctxt env)
    (let ((tmp (reg)))
      (emit `(mov ,tmp ,(cg (car operand*) 'value env #f)))
      (emit `(cmp ,tmp ,(immediate v)))
      (cg-test ctxt 'rflags.z)))

  (define (cg-length operand* type-tag env)
    ;; boxes do not really have lengths, but the first field is the
    ;; type object.
    (assert (memq type-tag `(,(tag 'bytevector) ,(tag 'string)
                             ,(tag 'vector) ,(tag 'box))))
    (let ((r0 (reg)) (r1 (reg)))
      (emit `(mov ,r0 ,(cg (car operand*) 'value env #f))
            `(mov ,r1 (mem64+ ,r0 ,(fx- type-tag))))
      r1))

  (define (cg-void ctxt)
    (if (eq? ctxt 'value)
        (let ((r (reg)))
          (emit `(mov ,r (+ (<< $ ,(shift 'void)) ,(tag 'void))))
          r)
        'bug:void))

  (define (cg-source src)
    (when (const? src)
      (let ((source (const-value src)))
        (when source
          (emit `(%comment source ,source))))))

  (define (unlikely-procedure? x)
    (or (memq x '(raise-continuable))
        (never-returns? x)))

  (define (never-returns? x)
    (memq x '(error
              assertion-violation
              assertion-error
              undefined-variable
              raise
              raise-accessor-error
              raise-mutator-error
              implementation-restriction)))

  (define (cg-primcall-proc label name operand* ctxt env tail?)
    ;; TODO: do this in a separate pass! also see the primref
    ;; code! this fallthrough case is what should happen if the
    ;; primitive can't be open-coded, too (e.g. bad arity).
    (cond ((primlocs name) =>
           (lambda (location)
             (when (unlikely-procedure? name)
               (emit '(%comment unlikely)))
             (cg-funcall label
                         (make-funcall (make-primref '$global-ref)
                                       (list (make-const location #f))
                                       #f #f)
                         operand* ctxt env (or tail? (never-returns? name)))))
          (else
           (error who "Missing primitive" name))))

  (define (cg-primcall-proc* label name loc* ctxt env tail?)
    (cond ((primlocs name) =>
           (lambda (location)
             (when (unlikely-procedure? name)
               (emit '(%comment unlikely)))
             (cg-funcall* label
                          (make-funcall (make-primref '$global-ref)
                                        (list (make-const location #f))
                                        #f #f)
                          loc* ctxt env (or tail? (never-returns? name)))))
          (else
           (error who "Missing primitive" name))))

  (define (cg-explicit-check-pair? reg)
    (when explicit-checks
      (let ((t (reg)))
        (with-restart-handler (pair? proceed)
          (emit `(mov ,t ,reg)
                `(and ,t ,(mask 'pair))
                `(cmp ,t ,(tag 'pair))
                `(je ,proceed))))))

  (define (cg-explicit-check-vector? reg)
    (when explicit-checks
      (let ((t (reg)))
        (with-restart-handler (vector? proceed)
          (emit `(mov ,t ,reg)
                `(and ,t ,(mask 'vector))
                `(cmp ,t ,(tag 'vector))
                `(je ,proceed))))))

  ;; Was the type of the operand inferred as `type'?
  (define (inferred-as? op type)
    (and (infer? op)
         (eq? type (car (infer-facts op)))))

  (define cg-check-fixnum*
    (case-lambda
      ((reg1 op failure)
       (unless (inferred-as? op 'fixnum)
         (emit `(test ,reg1 ,(mask 'fixnum))
               `(jnz ,failure))))
      ((reg1 op1 reg2 op2 failure)
       (cond
         ((inferred-as? op1 'fixnum)
          (cg-check-fixnum* reg2 op2 failure))
         ((inferred-as? op2 'fixnum)
          (cg-check-fixnum* reg1 op1 failure))
         (else
          (let ((tmp (reg)))
            (emit `(mov ,tmp ,reg1))
            (emit `(or ,tmp ,reg2)
                  `(test ,tmp ,(mask 'fixnum))
                  `(jnz ,failure))))))))

  (define cg-check-fixnum?
    (case-lambda
      ((reg1 op)
       (unless (inferred-as? op 'fixnum)
         (with-restart-handler (fixnum? proceed)
           (emit `(test ,reg1 ,(mask 'fixnum))
                 `(jz ,proceed)))))
      ((reg1 op1 reg2 op2)
       (cond
         ((inferred-as? op1 'fixnum)
          (cg-check-fixnum? reg2 op2))
         ((inferred-as? op2 'fixnum)
          (cg-check-fixnum? reg1 op1))
         (else
          (let ((tmp (reg)))
            (with-restart-handler (fixnum? proceed)
              (emit `(mov ,tmp ,reg1))
              (emit `(or ,tmp ,reg2)
                    `(test ,tmp ,(mask 'fixnum))
                    `(jz ,proceed)))))))))

  (define cg-check-char?
    (case-lambda
      ((reg1 op)
       (unless (inferred-as? op 'char)
         (with-restart-handler (char? proceed)
           (let ((tmp (reg)))
             ;; FIXME: verify that this is optimized
             (emit `(mov ,tmp ,reg1)
                   `(and ,tmp ,(mask 'char))
                   `(cmp ,tmp ,(tag 'char))
                   `(je ,proceed))))))
      ((reg1 op1 reg2 op2)
       (cond
         ((inferred-as? op1 'char)
          (cg-check-char? reg2 op2))
         ((inferred-as? op2 'char)
          (cg-check-char? reg1 op1))
         (else
          (let ((tmp (reg)))
            (with-restart-handler (char? proceed)
              (emit `(mov ,tmp ,reg1))
              (emit `(or ,tmp ,reg2)
                    `(and ,tmp ,(mask 'char))
                    `(cmp ,tmp ,(tag 'char))
                    `(je ,proceed)))))))))

  (define cg-check-flonum?
    (case-lambda
      ((reg1 op)
       (unless (inferred-as? op 'flonum)
         (let ((tmp (reg)))
           (with-restart-handler (flonum? proceed)
             (emit `(mov ,tmp ,reg1)
                   `(and ,tmp ,(mask 'flonum))
                   `(cmp ,tmp ,(tag 'flonum))
                   `(je ,proceed))))))
      ((reg1 op1 reg2 op2)
       (cond
         ((inferred-as? op1 'flonum)
          (cg-check-flonum? reg2 op2))
         ((inferred-as? op2 'flonum)
          (cg-check-flonum? reg1 op1))
         (else
          (let ((tmp (reg)))
            (with-restart-handler (flonum? proceed)
              (emit `(mov ,tmp ,reg1))
              (emit `(and ,tmp ,reg2)
                    `(and ,tmp ,(mask 'flonum))
                    `(cmp ,tmp ,(tag 'flonum))
                    `(je ,proceed)))))))))

  (define cg-check-bytevector?
    (case-lambda
      ((reg1 op)
       (unless (inferred-as? op 'bytevector)
         (with-restart-handler (bytevector? proceed)
           (let ((tmp (reg)))
             ;; This sequence should get peephole optimized to a byte
             ;; register compare.
             (emit `(mov ,tmp ,reg1)
                   `(and ,tmp ,(mask 'bytevector))
                   `(cmp ,tmp ,(tag 'bytevector))
                   `(je ,proceed))))))))

  (define (cg-allocation size)
    ;; Generate a heap overflow check. After this code it is ok to
    ;; use the memory in [allocptr,allocptr+size[.
    (unless (eqv? size 0)
      (let ((alloc-ok (vector 'alloc-ok)))
        (emit `(sub (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)) ,size)
              `(jns ,alloc-ok))
        (with-new-frame ()
          (emit '(%comment unlikely))
          (emit '(mov rdi rsp))         ;tell the gc the stack start
          (emit '(call stop-and-copy)))
        (emit `(%label ,alloc-ok)))))

  ;; Inlined versions of car, cdr, c...r
  (define (cg-cxr label name operand* ctxt env tail?)
    (match operand*
      [(x)
       (let ((operations (cdr (reverse (cdr (string->list (symbol->string name)))))))
         (let ((op (reg)))
           (emit `(mov ,op ,(cg x 'value env #f)))
           (let lp ((op op) (operations operations))
             (if (null? operations)
                 op
                 (let ((op^ (reg)))
                   (cg-explicit-check-pair? op^)
                   (case (car operations)
                     [(#\a)
                      (emit `(mov ,op^ (mem64+ ,op ,(fx- (car-offset) (tag 'pair)))))]
                     [else
                      (emit `(mov ,op^ (mem64+ ,op ,(fx- (cdr-offset) (tag 'pair)))))])
                   (lp op^ (cdr operations)))))))]
      [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

  ;; Create a procedure that dispatches on the its first argument.
  ;; This is mostly about about making a procedure that's small
  ;; enough that the optimizer doesn't choke on it.
  (define-syntax lambda-case-dispatch
    (lambda (x)
      (syntax-case x (else)
        ((_ (label name formal* ...)
            ((symlist* ...) . body*) ...
            (else else-expr))
         (with-syntax ([(t* ...) (generate-temporaries #'((symlist* ...) ...))])
           #'(letrec ([t* (lambda (label name formal* ...) . body*)] ...)
               (lambda (label name formal* ...)
                 (case name
                   ((symlist* ...) (t* label name formal* ...))
                   ...
                   (else else-expr)))))))))

  #;
  (define-syntax lambda-case-dispatch
    (lambda (x)
      (syntax-case x (else)
        ((_ (label name formal* ...)
            ((symlist* ...) . body*) ...
            (else else-expr))
         (with-syntax ([(t* ...) (generate-temporaries #'((symlist* ...) ...))])
           #'(letrec ([t* (lambda (label name formal* ...) . body*)] ...)
               (let ((lookup (make-eq-hashtable)))
                 (for-each
                  (lambda (sym)
                    (hashtable-set! lookup sym t*))
                  '(symlist* ...))
                 ...
                 (lambda (label name formal* ...)
                   (cond
                     ((hashtable-ref lookup name #f) =>
                      (lambda (proc) (proc label name formal* ...)))
                     (else else-expr))))))))))

  ;; FIXME: these _really_ should check the arity.
  (define cg-primcall
    (lambda-case-dispatch (label name operand* ctxt env tail?)
      ((null?) (cg-const-predicate operand* '() ctxt env))
      ((eof-object?) (cg-const-predicate operand* (eof-object) ctxt env))
      ((boolean?) (cg-type-predicate operand* (mask 'boolean) (tag 'boolean) ctxt env))
      ((pair?) (cg-type-predicate operand* (mask 'pair) (tag 'pair) ctxt env))
      ((vector?) (cg-type-predicate operand* (mask 'vector) (tag 'vector) ctxt env))
      ((char?) (cg-type-predicate operand* (mask 'char) (tag 'char) ctxt env))
      ((fixnum?) (cg-type-predicate operand* (mask 'fixnum) (tag 'fixnum) ctxt env))
      ((string?) (cg-type-predicate operand* (mask 'string) (tag 'string) ctxt env))
      ((bytevector?) (cg-type-predicate operand* (mask 'bytevector) (tag 'bytevector) ctxt env))
      ((procedure?) (cg-type-predicate operand* (mask 'procedure) (tag 'procedure) ctxt env))
      ((flonum?) (cg-type-predicate operand* (mask 'flonum) (tag 'flonum) ctxt env))
      (($immsym?) (cg-type-predicate operand* (mask 'immsym) (tag 'immsym) ctxt env))
      (($box?) (cg-type-predicate operand* (mask 'box) (tag 'box) ctxt env))
      (($box-header?) (cg-type-predicate operand* (mask 'box-header) (tag 'box-header) ctxt env))

      ((void) (cg-void ctxt))
      (($void?) (cg-type-predicate operand* (mask 'void) (tag 'void) ctxt env))
      ((eof-object)
       (let ((tmp (reg)))
         (emit `(mov ,tmp ,(immediate (eof-object))))
         tmp))

      ((bytevector-length) (cg-length operand* (tag 'bytevector) env))
      ((vector-length) (cg-length operand* (tag 'vector) env))
      ((string-length) (cg-length operand* (tag 'string) env))
      (($box-type) (cg-length operand* (tag 'box) env))

      ((not)
       (match operand*
         [(op)
          (match ctxt
            [('test consequence alternative)
             (let ((result (cg op `(test ,alternative ,consequence) env #f)))
               (cond ((eq? result '*branched*)
                      result)
                     (else
                      (let ((t (reg)))
                        (emit `(mov ,t ,result))
                        (emit `(cmp ,t ,(immediate #f)))
                        (cg-test ctxt 'rflags.z)))))]
            [_
             (let* ((result (cg op 'value env #f))
                    (flag (let ((t (reg)))
                            (emit `(mov ,t ,result))
                            (emit `(cmp ,t ,(immediate #f)))
                            'rflags.z)))
               (cg-test ctxt flag))])]
         [_
          (cg-primcall-proc label name operand* ctxt env tail?)]))
      ((eq?)
       (assert (eqv? (length operand*) 2))
       (let ((operand* (if (constant? (car operand*))
                           (reverse operand*)
                           operand*)))
         (let ((op1 (reg)) (op2 (reg)))
           (emit `(mov ,op1 ,(cg (car operand*) 'value env #f)))
           (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
           (emit `(cmp ,op1 ,op2))
           (cg-test ctxt 'rflags.z))))
      ((cons)
       (let* ((CAR (reg)) (CDR (reg)) (PAIR (reg)) (alloc (reg)))
         (emit `(mov ,CAR ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,CDR ,(cg (cadr operand*) 'value env #f)))
         (cg-allocation 16)
         (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
               `(mov (mem64+ ,alloc ,(car-offset)) ,CAR)
               `(mov (mem64+ ,alloc ,(cdr-offset)) ,CDR)
               `(lea ,PAIR (mem+ ,alloc ,(tag 'pair)))
               `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) 16))
         PAIR))
      ((car caar caaar caaaar cdaaar cdaar cadaar cddaar cdar cadar caadar cdadar
            cddar caddar cdddar cdr cadr caadr caaadr cdaadr cdadr cadadr cddadr cddr
            caddr caaddr cdaddr cdddr cadddr cddddr)
       (cg-cxr label name operand* ctxt env tail?))
      ((set-car!)
       (let ((preg (reg)) (vreg (reg)))
         (emit `(mov ,vreg ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,preg ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-pair? preg)
         (emit `(mov (mem64+ ,preg ,(fx- (car-offset) (tag 'pair))) ,vreg)))
       (cg-void ctxt))
      ((set-cdr!)
       (let ((preg (reg)) (vreg (reg)))
         (emit `(mov ,vreg ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,preg ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-pair? preg)
         (emit `(mov (mem64+ ,preg ,(fx- (cdr-offset) (tag 'pair))) ,vreg)))
       (cg-void ctxt))

      ((fixnum-width)
       (let ((r (reg)))
         (emit `(mov ,r ,(immediate (amd64-fixnum-width))))
         r))
      ((fxzero? fxnegative? fxpositive? fxodd? fxeven?)
       (let ((value (reg)) (t (reg)))
         (emit `(mov ,value ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? value (car operand*))
         (emit `(test ,value ,(case name
                                ((fxodd? fxeven?) (immediate 1))
                                (else value))))
         (cg-test ctxt
                  (case name
                    ((fxzero?) 'rflags.z)
                    ((fxnegative?) 'rflags.s)
                    ((fxodd?) 'rflags.nz)
                    ((fxeven?) 'rflags.z)
                    ((fxpositive?) 'rflags.nle)))))
      ((fx+)
       (unless (and (pair? operand*)
                    (pair? (cdr operand*))
                    (null? (cddr operand*)))
         (assertion-violation 'cg "Bad fx+ args" operand*))
       (let ((tmp1 (reg)) (tmp2 (reg)))
         (let f ((op1 (car operand*))
                 (op2 (cadr operand*)))
           (cond ((constant? op2)
                  (cond ((fixnum? (constant-value op2))
                         (emit `(mov ,tmp1 ,(cg op1 'value env #f)))
                         (cg-check-fixnum? tmp1 op1)
                         (with-restart-handler (overflow proceed)
                           (emit `(add ,tmp1 ,(cg op2 'value env #f)))
                           (emit `(jno ,proceed)))
                         tmp1)
                        (else
                         (cg-primcall-proc label name operand* ctxt env tail?))))
                 ((and (constant? op1) (not (constant? op2)))
                  (f op2 op1))
                 (else
                  (emit `(mov ,tmp1 ,(cg op2 'value env #f)))
                  (emit `(mov ,tmp2 ,(cg op1 'value env #f)))
                  (cg-check-fixnum? tmp1 op2
                                    tmp2 op1)
                  (with-restart-handler (overflow proceed)
                    (emit `(add ,tmp1 ,tmp2))
                    (emit `(jno ,proceed)))
                  tmp1)))))
      ((fx-)
       (match operand*
         [(op)
          (let ((r (reg)))
            (emit `(mov ,r ,(cg op 'value env #f)))
            (cg-check-fixnum? r op)
            (with-restart-handler (overflow proceed)
              (emit `(neg ,r))
              (emit `(jno ,proceed)))
            r)]
         [(op1 op2)
          (let ((op1 (reg)) (op2 (reg)))
            (emit `(mov ,op1 ,(cg (car operand*) 'value env #f)))
            (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
            (cg-check-fixnum? op1 (car operand*)
                              op2 (cadr operand*))
            (with-restart-handler (overflow proceed)
              (emit `(sub ,op1 ,op2))
              (emit `(jno ,proceed)))
            op1)]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))
      ((fxnot)
       (assert (eqv? (length operand*) 1))
       (let ((r (reg)))
         (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? r (car operand*))
         (emit `(not ,r)
               `(and ,r ,(fxnot (mask 'fixnum))))
         r))
      ((fx=? fx<? fx>? fx>=? fx<=?)
       (cg-fxcmp-predicate label name operand* ctxt env tail?))
      ((char=? char<? char>? char>=? char<=?)
       (cg-chcmp-predicate label name operand* ctxt env tail?))
      ((fxand)
       (if (eqv? (length operand*) 2)
           (let f ((op1 (car operand*))
                   (op2 (cadr operand*)))
             (cond ((constant? op2)
                    (cond ((fixnum? (constant-value op2))
                           (let* ((tmp (reg)))
                             (emit `(mov ,tmp ,(cg op1 'value env #f)))
                             (cg-check-fixnum? tmp op1)
                             (emit `(and ,tmp ,(cg op2 'value env #f)))
                             tmp))
                          (else
                           (cg-primcall-proc label name operand* ctxt env tail?))))
                   ((and (constant? op1) (not (constant? op2)))
                    (f op2 op1))
                   (else
                    (let* ((tmp1 (reg)) (tmp2 (reg)))
                      (emit `(mov ,tmp1 ,(cg op2 'value env #f)))
                      (emit `(mov ,tmp2 ,(cg op1 'value env #f)))
                      (cg-check-fixnum? tmp1 op2
                                        tmp2 op1)
                      (emit `(and ,tmp1 ,tmp2))
                      tmp1))))
           (cg-primcall-proc label name operand* ctxt env tail?)))
      ((fxxor fxior)
       (cond ((eqv? (length operand*) 2)
              (let ((ret (reg)) (op2 (reg)))
                (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
                (emit `(mov ,ret ,(cg (car operand*) 'value env #f)))
                (cg-check-fixnum? op2 (cadr operand*)
                                  ret (car operand*))
                (case name
                  ((fxxor) (emit `(xor ,ret ,op2)))
                  ((fxior) (emit `(or ,ret ,op2))))
                ret))
             (else (cg-primcall-proc label name operand* ctxt env tail?))))
      ((fx*)
       (assert (fx=? (length operand*) 2))
       (let f ((op1 (car operand*))
               (op2 (cadr operand*)))
         (cond
           ((and (constant? op1) (not (constant? op2)))
            (f op2 op1))
           ((constant-s31? op2)
            (let ((ret (reg)) (tmp (reg)))
              (emit `(mov ,tmp ,(cg op1 'value env #f)))
              (cg-check-fixnum? tmp op1)
              (with-restart-handler (overflow proceed)
                (emit `(imul ,ret ,tmp ,(constant-value op2))
                      `(jno ,proceed)))
              ret))
           (else
            (let ((r1 (reg)) (r2 (reg)))
              (emit `(mov ,r1 ,(cg op1 'value env #f)))
              (emit `(mov ,r2 ,(cg op2 'value env #f)))
              (cg-check-fixnum? r1 op1
                                r2 op2)
              (emit `(sar ,r2 ,(shift 'fixnum)))
              (with-restart-handler (overflow proceed)
                (emit `(imul ,r1 ,r2)      ;R1 <- R1 * R2
                      `(jno ,proceed)))
              r1)))))
      (($fx*/false)                   ;UNSAFE
       (let ((r0 (reg)) (r1 (reg)))
         (emit `(mov ,r0 ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,r1 ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov rax ,r1))
         (emit `(sar rax ,(shift 'fixnum)))
         (emit `(mov rdi ,r0))
         (emit `(imul rax rdi))       ;RAX <- RAX * RDI
         (emit `(mov rdx ,(immediate #f))
               `(cmovo rax rdx))
         'rax))
      (($fxquo+rem)
       ;; XXX: can trigger #DE
       (let* ((n (reg)) (d (reg)) (tmp1 (reg)) (tmp2 (reg)))
         (emit `(mov ,d ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,n ,(cg (car operand*) 'value env #f)))
         (emit `(mov rax ,n)
               `(mov rdi ,d))
         (emit `(cqo))                ;sign-extend RAX into RDX
         (emit `(idiv #;rdx:rax rdi)) ;RAX, RDX <- RDX:RAX / RDI
         (emit `(sal rax ,(shift 'fixnum))
               `(mov ,tmp1 rax)
               `(mov ,tmp2 rdx)
               `(mov ,(car %ret-reg*) ,tmp1)
               `(mov ,(cadr %ret-reg*) ,tmp2))
         (cg-mv-return 2 ctxt env tail?)))
      ((fxdiv)
       (cond
         ((and (eqv? (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               (let ((n (constant-value (cadr operand*))))
                 (or (eqv? 1 (bitwise-bit-count n))
                     (and (fx<? n -1)
                          (eqv? 1 (bitwise-bit-count (- n)))))))
          ;; fxdiv with known power-of-two divisor.
          (let ((n (constant-value (cadr operand*)))
                (r (reg)))
            (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? r (car operand*))
            (let ((a (bitwise-first-bit-set (abs n))))
              (emit `(sar ,r ,a)
                    `(and ,r ,(fxnot (mask 'fixnum))))
              (when (negative? n)
                (emit `(neg ,r)))
              r)))
         ((and (eqv? (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               ;; Forbidden: (least-fixnum), -1, 0, 1
               (<= 2 (abs (constant-value (cadr operand*))) (amd64-greatest-fixnum)))
          ;; fxdiv with a known good divisor.
          (let ((n-reg (reg)))
            (emit `(mov ,n-reg ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? n-reg (car operand*))
            (let-values ([(d m) (cg-fxdiv-and-mod n-reg (constant-value (cadr operand*)) #t)])
              d)))
         (else
          (cg-primcall-proc label name operand* ctxt env tail?))))
      ((fxmod)
       (cond
         ((and (eqv? (length operand*) 2)
               (inferred-positive-fixnum? (car operand*))
               (constant-fixnum? (cadr operand*))
               (positive? (constant-value (cadr operand*)))
               (eqv? 1 (bitwise-bit-count (constant-value (cadr operand*)))))
          ;; fxmod of a positive number with known power-of-two divisor.
          (let ((n (constant-value (cadr operand*)))
                (r (reg)))
            (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? r (car operand*))
            (emit `(and ,r ,(immediate (- n 1))))
            r))
         ((and (= (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               ;; Forbidden: (least-fixnum), -1, 0, 1
               (<= 2 (abs (constant-value (cadr operand*))) (amd64-greatest-fixnum)))
          ;; fxmod with a known good divisor.
          (let ((n-reg (reg)))
            (emit `(mov ,n-reg ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? n-reg (car operand*))
            (let-values ([(d m) (cg-fxdiv-and-mod n-reg (constant-value (cadr operand*)) #t)])
              m)))
         (else
          (cg-primcall-proc label name operand* ctxt env tail?))))
      ((fxdiv-and-mod)
       (cond
         ((and (= (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               ;; Forbidden: (least-fixnum), -1, 0, 1
               (<= 2 (abs (constant-value (cadr operand*))) (amd64-greatest-fixnum)))
          ;; fxdiv with a known good divisor.
          (let ((n-reg (reg)))
            (emit `(mov ,n-reg ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? n-reg (car operand*))
            (let-values ([(d m) (cg-fxdiv-and-mod n-reg (constant-value (cadr operand*)) #t)])
              (emit `(mov ,(car %ret-reg*) ,d)
                    `(mov ,(cadr %ret-reg*) ,m))
              (cg-mv-return 2 ctxt env tail?))))
         (else
          (cg-primcall-proc label name operand* ctxt env tail?))))

      ((fxarithmetic-shift-right)
       (cond
         ((and (fx=? (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               (fx<=? 0 (constant-value (cadr operand*)) 60))
          ;; fxasr with a known good shift amount
          (let ((r (reg))
                (a (constant-value (cadr operand*))))
            (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? r (car operand*))
            (unless (eqv? a 0)
              (emit `(sar ,r ,a)
                    `(and ,r ,(fxnot (mask 'fixnum)))))
            r))
         (else
          (match operand*
            [(a b)
             (let* ((ret (reg)) (op0 (reg)) (op1 (reg)))
               (emit `(mov ,op0 ,(cg a 'value env #f)))
               (emit `(mov ,op1 ,(cg b 'value env #f)))
               (cg-check-fixnum? op0 a op1 b)
               (emit `(mov rcx ,op1)
                     `(sar ecx ,(shift 'fixnum)))
               (with-restart-handler (fxwidth proceed)
                 (emit `(cmp cl ,(amd64-fixnum-width))
                       `(jb ,proceed)))
               (emit `(mov ,ret ,op0)
                     `(sar ,ret cl)
                     `(and ,ret ,(fxnot (mask 'fixnum))))
               ret)]
            [_ (cg-primcall-proc label name operand* ctxt env tail?)]))))
      ((fxarithmetic-shift-left)
       (cond
         ((and (fx=? (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               (eqv? (constant-value (cadr operand*)) 1))
          ;; (fxasl x 1).
          (let ((r (reg))
                (a (constant-value (cadr operand*))))
            (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? r (car operand*))
            (with-restart-handler (overflow proceed)
              (emit `(sal ,r 1))
              (emit `(jno ,proceed)))
            r))
         (else
          (match operand*
            [(a b)
             (let* ((ret (reg)) (op0 (reg)) (op1 (reg)) (tmp (reg))
                    (amount (and (constant-fixnum? b)
                                 (fx<=? 0 (constant-value b)
                                        (fx- (amd64-fixnum-width) 1))
                                 (constant-value b))))
               (emit `(mov ,op0 ,(cg a 'value env #f)))
               (emit `(mov ,op1 ,(cg b 'value env #f)))
               (cg-check-fixnum? op0 a op1 b)
               (emit `(mov rcx ,op1)
                     `(sar ecx ,(shift 'fixnum)))
               (unless amount
                 (with-restart-handler (fxwidth proceed)
                   (emit `(cmp cl ,(amd64-fixnum-width))
                         `(jb ,proceed))))
               ;; Transform the shift into a multiplication, which
               ;; can be checked for overflow with rFLAGS.OF.
               (cond (amount
                      (let ((scale (fxarithmetic-shift-left 1 amount)))
                        (cond
                          ((fx<? scale 128)
                           (with-restart-handler (impres proceed)
                             (emit `(imul ,ret ,op0 ,scale)
                                   `(jno ,proceed))))
                          (else
                           (emit `(mov ,ret ,op0))
                           (emit `(mov ,tmp ,scale))
                           (with-restart-handler (impres proceed)
                             (emit `(imul ,ret ,tmp)
                                   `(jno ,proceed)))))))
                     (else
                      (emit `(mov ,ret ,op0))
                      (emit `(mov ,tmp 1)
                            `(shl ,tmp cl))
                      (with-restart-handler (impres proceed)
                        (emit `(imul ,ret ,tmp)
                              `(jno ,proceed)))))
               ret)]
            [_ (cg-primcall-proc label name operand* ctxt env tail?)]))))
      (($fxasl/false)                 ;UNSAFE
       (let* ((ret (reg)) (op0 (reg)) (op1 (reg)) (false (reg)) (tmp (reg)))
         (emit `(mov ,op0 ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,op1 ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,false ,(immediate #f)))
         (emit `(mov rcx ,op1)
               `(sar rcx #x3)
               `(mov ,ret ,op0)
               `(shl ,ret cl)
               `(mov ,tmp ,ret)
               `(sar ,tmp cl)
               `(cmp ,op0 ,tmp)
               `(cmovnz ,ret ,false)
               `(cmp rcx ,(amd64-fixnum-width))
               `(cmovnb ,ret ,false))
         ret))
      (($fx+/false $fx-/false)                   ;UNSAFE
       (let* ((op0 (reg)) (op1 (reg)) (false (reg)) (ret (reg)))
         (emit `(mov ,op1 ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,op0 ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,false ,(immediate #f))
               `(mov ,ret ,op0))
         (case name
           (($fx+/false) (emit `(add ,ret ,op1)))
           (else         (emit `(sub ,ret ,op1))))
         (emit `(cmovo ,ret ,false))
         ret))
      ((fxlength)
       (let* ((op (reg)) (tmp (reg)) (zero (reg)) (ret (reg)))
         (emit `(mov ,op ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? op (car operand*))
         (emit `(mov ,zero 0)
               `(sar ,op ,(fx- (shift 'fixnum) 1)) ;n shifted 1 left
               `(mov ,tmp ,op)
               `(not ,op)
               `(cmovl ,tmp ,op)    ;tmp = (if (negative? n) (fxnot n) n)
               `(bsr ,ret ,tmp)     ;position of first set bit
               `(cmovz ,ret ,zero)  ;special case for n=0
               `(sal ,ret ,(shift 'fixnum)))
         ret))
      ((fxfirst-bit-set)
       (let ((op (reg)) (ret (reg)) (tmp (reg)))
         (emit `(mov ,op ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? op (car operand*))
         (emit `(mov ,tmp 2)
               `(bsf ,ret ,op)
               `(cmovz ,ret ,tmp)
               `(lea ,ret (mem+ (* ,ret 8) ,(* 8 -3))))
         ret))
      ((fxbit-count)
       (if use-popcnt
           (match operand*
             [(a)
              (let* ((ret (reg)) (op (reg)) (inv (reg)))
                (emit `(mov ,op ,(cg a 'value env #f)))
                (cg-check-fixnum? op a)
                (emit `(mov ,inv ,op)
                      `(sar ,inv ,63)
                      `(sar ,op ,(shift 'fixnum))
                      `(xor ,op ,inv)
                      `(popcnt ,ret ,op)
                      `(xor ,ret ,inv)
                      `(sal ,ret ,(shift 'fixnum)))
                ret)]
             [_ (cg-primcall-proc label name operand* ctxt env tail?)])
           (cg-primcall-proc label name operand* ctxt env tail?)))

      ((integer->char)
       (assert (= (length operand*) 1))
       (let ((r (reg))
             (t (reg)))
         (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? r (car operand*))
         (emit `(lea ,t (mem+ ,r ,(immediate #x-D800))))
         (with-restart-handler (sv? proceed)
           (emit `(cmp ,r ,(immediate #x10FFFF))
                 `(jbe ,proceed)))
         (with-restart-handler (sv? proceed)
           (emit `(cmp ,t ,(immediate (- #xDFFF #xD800)))
                 `(jnbe ,proceed)))
         (assert (eqv? (tag 'fixnum) (fxarithmetic-shift-right
                                      (tag 'char)
                                      (fx- (shift 'char) (shift 'fixnum)))))
         (emit `(shl ,r ,(fx- (shift 'char) (shift 'fixnum)))
               `(or ,r ,(tag 'char)))
         r))
      ((char->integer)
       (let ((r (reg)))
         (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
         (unless (inferred-as? (car operand*) 'char)
           (with-restart-handler (char? proceed)
             (let ((tmp (reg)))
               (emit `(mov ,tmp ,r)
                     `(and ,tmp ,(mask 'char))
                     `(cmp ,tmp ,(tag 'char))
                     `(je ,proceed)))))
         (emit `(shr ,r ,(fx- (shift 'char) (shift 'fixnum))))
         r))
      (($immsym->fixnum)              ;UNSAFE
       (let ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(shr ,ret ,(fx- (shift 'immsym) (shift 'fixnum)))
               `(and ,ret ,(fxnot (mask 'fixnum))))
         ret))
      (($fixnum->immsym)              ;UNSAFE
       (let ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(shl ,ret ,(fx- (shift 'immsym) (shift 'fixnum)))
               `(or ,ret ,(tag 'immsym)))
         ret))
      (($void->fixnum)                ;UNSAFE
       (let ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(shr ,ret ,(fx- (shift 'void) (shift 'fixnum)))
               `(and ,ret ,(fxnot (mask 'fixnum))))
         ret))
      (($object->fixnum)
       ;; Bits may be lost if the object is an immediate. The caller
       ;; must not care.
       (let ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(sal ,ret ,(shift 'fixnum)))
         ret))

      (($make-bytevector)             ;UNSAFE
       (let ((ret (reg)) (len (reg)) (req (reg)) (alloc (reg)))
         (emit `(mov ,len ,(cg (car operand*) 'value env #f))
               `(mov ,req ,len)
               `(shr ,req ,(shift 'fixnum))
               `(add ,req ,(+ 8 8 7))
               `(and ,req -8))
         (cg-allocation req)
         (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
               `(mov (mem64+ ,alloc) ,len)
               `(lea ,ret (mem+ ,alloc ,(tag 'bytevector)))
               `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) ,req))
         (let ((seek-tag (reg)))
           ;; Create the seek mark
           (emit `(sub ,req 8)        ;length field not included
                 `(mov ,seek-tag ,(tag 'seek-mark))
                 `(shl ,req ,(shift 'seek-mark))
                 `(or ,req ,seek-tag)
                 `(mov (mem64+ ,ret 8 ,(fx- (tag 'bytevector))) ,req)))
         ret))
      (($bytevector-truncate!)              ;UNSAFE
       (match operand*
         [(bv^ n^ seek-offset^ seek^)
          (let ((bv (reg)) (len (reg)) (req (reg)) (seek-offset (reg)) (seek (reg)))
            (emit `(mov ,bv ,(cg bv^ 'value env #f)))
            (emit `(mov ,seek ,(cg seek^ 'value env #f)))
            (emit `(mov ,seek-offset ,(cg seek-offset^ 'value env #f)))
            (emit `(mov ,len ,(cg n^ 'value env #f))
                  `(mov ,req ,len)
                  `(shr ,req ,(shift 'fixnum))
                  `(add ,req ,(+ 8 8 7))
                  `(and ,req -8))
            (emit `(mov (mem64+ ,bv ,(fx- (tag 'bytevector))) ,len))
            (let ((seek-tag (reg)))
              ;; Update the seek mark
              (emit `(sub ,req 8)        ;length field not included
                    `(mov ,seek-tag ,(tag 'seek-mark))
                    `(shl ,req ,(shift 'seek-mark))
                    `(or ,req ,seek-tag)
                    `(mov (mem64+ ,bv 8 ,(fx- (tag 'bytevector))) ,req)))
            (unless (and (const? seek^) (eqv? #f (const-value seek^)))
              (let ((seek-tag (reg)))
                ;; Tell the GC to not read the discarded part of the
                ;; bytevector.
                (emit `(mov ,seek-tag ,(tag 'seek-mark))
                      `(shl ,seek ,(shift 'seek-mark))
                      `(or ,seek ,seek-tag)
                      `(mov (mem64+ ,bv 8 8 ,seek-offset ,(fx- (tag 'bytevector))) ,seek))))
            bv)]))
      ((bytevector-u8-set!)
       (cg-bytevector-native-set! 'mem8+ 'dl 'u 8 operand* ctxt env))
      ((bytevector-s8-set!)
       (cg-bytevector-native-set! 'mem8+ 'dl 's 8 operand* ctxt env))
      ((bytevector-u16-native-set!)
       (cg-bytevector-native-set! 'mem16+ 'dx 'u 16 operand* ctxt env))
      ((bytevector-s16-native-set!)
       (cg-bytevector-native-set! 'mem16+ 'dx 's 16 operand* ctxt env))
      ((bytevector-u32-native-set!)
       (cg-bytevector-native-set! 'mem32+ 'edx 'u 32 operand* ctxt env))
      ((bytevector-s32-native-set!)
       (cg-bytevector-native-set! 'mem32+ 'edx 's 32 operand* ctxt env))
      ((bytevector-ieee-single-native-set!)
       (cg-bytevector-native-set! 'mem32+ 'edx 'ieee 32 operand* ctxt env))

      ((bytevector-ieee-double-native-set!)
       (match operand*
         [(bv idx v)
          (let ((bv-reg (reg)) (idx-reg (reg)) (v-reg (reg)))
            (emit `(mov ,bv-reg ,(cg bv 'value env #f)))
            (emit `(mov ,idx-reg ,(cg idx 'value env #f)))
            (emit `(mov ,v-reg ,(cg v 'value env #f)))
            (cg-helper-bv-idx-check 'mem64+ bv-reg idx-reg idx)
            (cg-check-flonum? v-reg v)
            (emit `(shr ,v-reg ,(shift 'flonum))
                  `(sar ,idx-reg ,(shift 'fixnum))
                  `(movq xmm1 ,v-reg)
                  `(cvtss2sd xmm0 xmm1)
                  `(movq (mem64+ ,bv-reg 8 8 ,idx-reg ,(- (tag 'bytevector))) xmm0))
            (cg-void ctxt))]
         [else
          (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((bytevector-s8-ref bytevector-u8-ref
                          bytevector-s16-ref bytevector-u16-ref
                          bytevector-s32-ref bytevector-u32-ref
                          bytevector-s16-native-ref bytevector-u16-native-ref
                          bytevector-s32-native-ref bytevector-u32-native-ref
                          bytevector-ieee-single-native-ref)
       (match (cons name operand*)
         (((or 'bytevector-s16-ref 'bytevector-s32-ref
               'bytevector-u16-ref 'bytevector-u32-ref)
           bv idx (? (lambda (x)
                       (and (constant? x)
                            (memq (constant-value x) '(little big))))
                     endian))
          ;; Known endianness.
          (cg-bv-known-ref name bv idx (constant-value endian) ctxt env))
         (((or 'bytevector-s8-ref 'bytevector-u8-ref
               'bytevector-s16-native-ref 'bytevector-u16-native-ref
               'bytevector-s32-native-ref 'bytevector-u32-native-ref
               'bytevector-ieee-single-native-ref)
           bv idx)
          (cg-bv-known-ref name bv idx (endianness little) ctxt env))
         (_
          (cg-primcall-proc label name operand* ctxt env tail?))))

      ((bytevector-ieee-double-native-ref)
       (match operand*
         [(bv idx)
          (let ((bv-reg (reg)) (idx-reg (reg)))
            (emit `(mov ,bv-reg ,(cg bv 'value env #f)))
            (emit `(mov ,idx-reg ,(cg idx 'value env #f)))
            (cg-helper-bv-idx-check 'mem64+ bv-reg idx-reg idx)
            (emit `(sar ,idx-reg ,(shift 'fixnum))
                  `(movq xmm0 (mem64+ ,bv-reg 8 8 ,idx-reg ,(- (tag 'bytevector))))
                  `(cvtsd2ss xmm0 xmm0)
                  `(movd eax xmm0)
                  `(shl rax ,(shift 'flonum))
                  `(or rax ,(tag 'flonum)))
            'rax)]
         [else
          (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((bytevector-address)
       (let ((bv-reg (reg)) (ret (reg)))
         (emit `(mov ,bv-reg ,(cg (car operand*) 'value env #f)))
         (cg-check-bytevector? bv-reg (car operand*))
         (emit `(lea ,ret (mem64+ ,bv-reg 8 8 ,(fx- (tag 'bytevector)))))
         (emit `(sal ,ret ,(shift 'fixnum)))
         ret))

      (($make-vector)                 ;UNSAFE
       (let ((len (reg)) (req (reg)) (fill (reg)))
         (emit `(mov ,len ,(cg (car operand*) 'value env #f))
               `(lea ,req (mem64+ 8 ,len)))
         (when (pair? (cdr operand*))   ;optional fill value
           (emit `(mov ,fill ,(cg (cadr operand*) 'value env #f))))
         (cg-allocation req)
         (let ((ret (reg)) (alloc (reg)))
           (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
                 `(mov (mem64+ ,alloc) ,len)
                 `(lea ,ret (mem+ ,alloc ,(tag 'vector)))
                 `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) ,req))
           (when (pair? (cdr operand*))
             (emit `(lea rdi (mem+ ,ret ,(- (tag 'vector)) 8))
                   `(lea rcx (mem+ ,req -8))
                   `(sar rcx 3)           ;quads
                   `(mov rax ,fill)
                   `(rep.stos (mem64+ rdi) rax)))
           ret)))
      ((vector-ref)
       (let* ((vec (reg)) (idx (reg)) (ret (reg)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,vec ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-vector? vec)
         (if explicit-vector-checks
             (with-restart-handler (index proceed)
               (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                     `(jb ,proceed)))
             (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                   `(cmovnb ,idx ,vec)))
         (emit `(mov ,ret (mem64+ ,vec ,idx 8 ,(fx- (tag 'vector)))))
         ret))
      ((vector-set!)
       (let* ((vec (reg)) (idx (reg)) (value (reg)))
         (emit `(mov ,value ,(cg (caddr operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,vec ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-vector? vec)
         (when explicit-checks
           (cg-check-fixnum? idx (cadr operand*)))
         (if explicit-vector-checks
             (with-restart-handler (index proceed)
               (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                     `(jb ,proceed)))
             (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                   `(cmovnb ,idx ,vec)))
         (emit `(mov (mem64+ ,vec ,idx 8 ,(fx- (tag 'vector))) ,value))
         (cg-void ctxt)))

      (($make-box)                    ;VERY UNSAFE
       ;; ($make-box type length). Slots will be filled with zero. The
       ;; length refers to the number of slots.
       (let ((req (reg)) (type (reg)) (len (reg)))
         (emit `(mov ,type ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,len ,(cg (cadr operand*) 'value env #f))
               `(lea ,req (mem64+ 8 ,len)))
         (cg-allocation req)
         (let ((ret (reg)) (alloc (reg)))
           (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
                 `(mov (mem64+ ,alloc) ,type)
                 `(lea ,ret (mem+ ,alloc ,(tag 'box)))
                 `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) ,req))
           ;; Clear the memory.
           (emit `(lea rdi (mem+ ,ret ,(- (tag 'box)) 8))
                 `(lea rcx (mem+ ,req -8))
                 `(sar rcx 3)           ;quads
                 `(xor eax eax)
                 `(rep.stos (mem64+ rdi) rax))
           ret)))
      (($box-type-set!)               ;VERY UNSAFE
       ;; ($box-type-set! box value)
       (match operand*
         [(box value)
          (let* ((rbox (reg)) (rvalue (reg)))
            (emit `(mov ,rvalue ,(cg value 'value env #f)))
            (emit `(mov ,rbox ,(cg box 'value env #f)))
            (emit `(mov (mem64+ ,rbox ,(fx- (tag 'box))) ,rvalue))
            (cg-void ctxt))]))
      (($box-ref)                     ;VERY UNSAFE
       ;; ($box-ref box index)
       (let* ((box (reg)) (idx (reg)) (ret (reg)))
         (emit `(mov ,box ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,ret (mem64+ ,box ,idx 8 ,(fx- (tag 'box)))))
         ret))
      (($box-set!)                    ;VERY UNSAFE
       ;; ($box-set! box index value)
       (let* ((box (reg)) (idx (reg)) (value (reg)))
         (emit `(mov ,value ,(cg (caddr operand*) 'value env #f)))
         (emit `(mov ,box ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov (mem64+ ,box ,idx 8 ,(fx- (tag 'box))) ,value))
         (cg-void ctxt)))
      (($box-header-type-eq?)
       ;; ($box-header-type-eq? obj type)
       ;; ($box-header-type-eq? obj type mask test)
       (let lp ((operand* operand*))
         (match operand*
           [(obj type)
            (lp (list obj type (make-const 0 #f) (make-const 0 #f)))]
           [(obj (? constant? type) (? constant? value-mask) (? constant? value-test))
            (let-values ([(test-mask test-tag)
                          (box-header-type-eq-mask (constant-value type)
                                                   (constant-value value-mask)
                                                   (constant-value value-test))])
              (let ((tmp (reg)))
                (emit `(mov ,tmp ,(cg obj 'value env #f))
                      `(and ,tmp ,test-mask)
                      `(cmp ,tmp ,test-tag))
                (cg-test ctxt 'rflags.z)))])))
      (($box-header-length)
       ;; ($box-header-length box)
       (let* ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(shr ,ret ,(- (shift 'box-header:length) (shift 'fixnum)))
               `(and ,ret ,(fxnot (mask 'fixnum))))
         ret))
      (($box-header-value)
       ;; ($box-header-value box)
       (let* ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(and ,ret ,(bitwise-arithmetic-shift-left (mask 'box-header:value)
                                                          (shift 'box-header:value)))
               `(shr ,ret ,(- (shift 'box-header:value) (shift 'fixnum))))
         ret))
      ;; TODO: $box-header-refs?
      (($make-box-header)
       ;; ($make-box-header type refs? value length)
       (match operand*
         [((? const? type) refs? value length)
          (let* ((rrefs? (reg)) (rvalue (reg)) (rlength (reg)) (ret (reg)))
            (emit `(mov ,ret ,(immediate (btag (const-value type))))
                  `(shl ,ret ,(- (shift 'box-header:type) (shift 'fixnum)))
                  `(or ,ret ,(tag 'box-header)))
            (emit `(mov ,rrefs? ,(cg refs? 'value env #f))
                  `(and ,rrefs? ,(fxnot (mask 'boolean)))
                  `(shl ,rrefs? ,(- (shift 'box-header:refs?) (shift 'boolean)))
                  `(or ,ret ,rrefs?))
            (emit `(mov ,rvalue ,(cg value 'value env #f))
                  `(shl ,rvalue ,(- (shift 'box-header:value) (shift 'fixnum)))
                  `(or ,ret ,rvalue))
            (emit `(mov ,rlength ,(cg length 'value env #f))
                  `(shl ,rlength ,(- (shift 'box-header:length) (shift 'fixnum)))
                  `(or ,ret ,rlength))
            ret)]))

      (($record $box)
       (match operand*
         [(rtd . field-op*)
          (let* ((rtd-reg (reg))
                 (field* (map (lambda (op)
                                (let ((r (reg)))
                                  (emit `(mov ,r ,(cg op 'value env #f)))
                                  r))
                              field-op*)))
            (emit `(mov ,rtd-reg ,(cg rtd 'value env #f)))
            (let ((req (fx* 8 (fx+ 1 (length field*))))
                  (alloc (reg)))
              (cg-allocation req)
              (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
                    `(mov (mem64+ ,alloc) ,rtd-reg))
              (do ((i 8 (fx+ i 8))
                   (field* field* (cdr field*)))
                  ((null? field*))
                (emit `(mov (mem64+ ,alloc ,i) ,(car field*))))
              (let ((ret (reg)))
                (emit `(lea ,ret (mem+ ,alloc ,(tag 'box)))
                      `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) ,req))
                ret)))]))
      ((vector)
       (let* ((num-args (length operand*))
              (field* (map (lambda (op)
                             (let ((r (reg)))
                               (emit `(mov ,r ,(cg op 'value env #f)))
                               r))
                           operand*)))
         (let ((req (fx* 8 (fx+ 1 num-args)))
               (alloc (reg)))
           (cg-allocation req)
           (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
                 `(mov (mem64+ ,alloc) ,(immediate num-args)))
           (do ((i 8 (fx+ i 8))
                (field* field* (cdr field*)))
               ((null? field*))
             (emit `(mov (mem64+ ,alloc ,i) ,(car field*))))
           (let ((ret (reg)))
             (emit `(lea ,ret (mem+ ,alloc ,(tag 'vector)))
                   `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) ,req))
             ret))))

      (($make-string)                 ;UNSAFE
       (let ((size (reg)) (req (reg)) (ret (reg)) (alloc (reg)) (fill (reg)))
         (when (pair? (cdr operand*))   ;optional fill value
           (emit `(mov ,fill ,(cg (cadr operand*) 'value env #f))))
         (emit `(mov ,size ,(cg (car operand*) 'value env #f))
               `(mov ,req ,size)
               `(shr ,req ,(fx- (shift 'fixnum) 2)) ;32-bit slots
               `(lea ,req (mem64+ 8 ,req 7))
               `(and ,req -8))
         (cg-allocation req)
         (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
               `(mov (mem64+ ,alloc) ,size)
               `(lea ,ret (mem+ ,alloc ,(tag 'string)))
               `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) ,req))
         (when (pair? (cdr operand*))
           (let ((tmp (reg)))
             (emit `(mov ,tmp ,fill)
                   `(shl ,tmp 32)
                   `(or ,fill ,tmp))
             (emit `(lea rdi (mem+ ,ret ,(- (tag 'string)) 8))
                   `(lea rcx (mem+ ,req -8))
                   `(sar rcx 3)
                   `(mov rax ,fill)
                   `(rep.stos (mem64+ rdi) rax))))
         ret))
      (($string-truncate!)              ;UNSAFE
       (match operand*
         [(str^ n^)
          (let ((str (reg)) (n (reg)))
            (emit `(mov ,str ,(cg str^ 'value env #f)))
            (emit `(mov ,n ,(cg n^ 'value env #f)))
            (emit `(mov (mem64+ ,str ,(fx- (tag 'string))) ,n))
            str)]))
      ((string-ref)
       (let ((str (reg)) (idx (reg)))
         (emit `(mov ,str ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (cg-check-fixnum? idx (cadr operand*))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ ,str ,(fx- (tag 'string))))
                 `(jb ,proceed)))
         (assert (eqv? (shift 'fixnum) 3))
         (emit `(shr ,idx 1))
         (emit `(mov eax (mem32+ ,str ,idx 8 ,(fx- (tag 'string)))))
         ;; (emit `(shr ,idx ,(shift 'fixnum)))
         ;; (emit `(mov eax (mem32+ ,str (* ,idx 4) 8 ,(fx- (tag 'string)))))
         (emit `(or eax ,(tag 'char)))
         'rax))
      ((string-set!)
       (let ((str (reg)) (idx (reg)) (chtmp (reg)) (char64 'rcx) (char32 'ecx) (char8 'cl))
         (emit `(mov ,str ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,chtmp ,(cg (caddr operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         ;; char is given rcx, because there is no way to make an 8
         ;; or 32-bit pseudo register.
         (emit `(mov ,char64 ,chtmp))
         (cg-check-fixnum? idx (cadr operand*))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ ,str ,(fx- (tag 'string)))) ;type checks str
                 `(jb ,proceed)))
         (unless (inferred-as? (caddr operand*) 'char)
           (with-restart-handler (char? proceed)
             (assert (eqv? (mask 'char) #xff))
             (emit `(cmp ,char8 ,(tag 'char))
                   `(je ,proceed))))
         (emit `(sar ,idx 1))
         (emit `(mov (mem32+ ,str ,idx 8 ,(fx- (tag 'string))) ,char32))
         (cg-void ctxt)))

      (($debug-display)
       (emit `(mov rdi ,(cg (car operand*) 'value env #f)))
       (with-new-frame ()
         (emit '(call debug-display)))
       (cg-void ctxt))
      (($debug-put-u8)
       (emit `(mov rdi ,(cg (car operand*) 'value env #f))
             `(sar rdi ,(shift 'fixnum)))
       (with-new-frame ()
         (emit '(call (mem64+ *debug-put-u8))))
       (cg-void ctxt))
      (($pcb-set! $processor-data-set!) ;UNSAFE
       (let ((idx (reg)) (data (reg))
             (vec (if (eq? name '$pcb-set!) %pcb %cpu)))
         (emit `(mov ,idx ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,data ,(cg (cadr operand*) 'value env #f)))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ ,vec ,(- (tag 'vector))))
                 `(jb ,proceed)))
         (emit `(mov (mem64+ ,vec ,idx ,(- (tag 'vector))) ,data))
         (cg-void ctxt)))
      (($pcb-ref $processor-data-ref)   ;UNSAFE
       (let ((idx (reg)) (ret (reg))
             (vec (if (eq? name '$pcb-ref) %pcb %cpu)))
         (emit `(mov ,idx ,(cg (car operand*) 'value env #f)))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ ,vec ,(- (tag 'vector))))
                 `(jb ,proceed)))
         (emit `(mov ,ret (mem64+ ,vec ,(- (tag 'vector)) ,idx)))
         ret))
      (($boot-loader-type)
       ;; TODO: remove
       (emit `(mov rax (mem64+ boot-loader-type)))
       'rax)
      (($boot-loader-data)
       ;; TODO: remove
       (emit `(mov rax (mem64+ boot-loader-data)))
       'rax)

      (($procedure-info)
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(mov rax (mem64+ rax ,(fx- 8 (tag 'procedure)))))
       'rax)
      (($procedure-info-set!)
       (let* ((proc (reg)) (info (reg)))
         (emit `(mov ,proc ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,info ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov (mem64+ ,proc ,(fx- 8 (tag 'procedure))) ,info))
         (cg-void ctxt)))
      (($procedure-ref)
       (let* ((proc (reg)) (info (reg)) (idx (reg)) (ret (reg)))
         (emit `(mov ,proc ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (cg-check-fixnum? idx (cadr operand*))
         (emit `(mov ,info (mem64+ ,proc ,(fx- 8 (tag 'procedure)))))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ ,info ,(fx- 16 (tag 'box))))
                 `(jb ,proceed)))
         (emit `(mov ,ret (mem64+ ,proc ,idx ,(fx- 16 (tag 'procedure)))))
         ret))

      ;; for call/cc
      (($copy-stack)
       ;; This is somewhat weird, because the call from copy-stack
       ;; might be a second return.
       (with-new-frame ()
         (emit '(call copy-stack)))
       (emit `(mov ,%closure ,(lookup %closure env)))
       'rax)
      (($restore-stack)
       (let* ((stack (reg)) (v* (reg)))
         (emit `(mov ,stack ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,v* ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov rdx ,stack)
               `(mov rax ,v*)
               `(jmp restore-stack)))
       'rax)
      (($values)
       (let ((v* (reg)))
         (emit `(mov rdi ,(cg (car operand*) 'value env #f)))
         (cg-restore-callee-save env)
         (cg-deallocate-frame)
         (emit `(jmp values))           ;tail call
         'rax))
      (($bootstrap-symbols)
       (let ((r (reg)))
         (emit `(mov ,r (+ bootstrap-symbols ,(tag 'pair))))
         r))
      (($bootstrap-rtds)
       (let ((r (reg)))
         (emit `(mov ,r (+ bootstrap-rtds ,(tag 'vector))))
         r))
      (($global-set!)
       (let ((v (reg)))
         (emit `(mov ,v ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov (mem64+ ,%pcb ,(get-global (constant-value (car operand*)))
                             ,(- (tag 'vector)))
                     ,v)))
       (cg-void ctxt))
      (($global-ref)
       (let ((ret (reg)))
         (emit `(mov ,ret (mem64+ ,%pcb ,(get-global (constant-value (car operand*)))
                                  ,(- (tag 'vector)))))
         ret))

      (($switch-stack)
       (assert (null? (cddr operand*)))
       (let ((tmp (reg)))
         (emit `(mov ,tmp ,(cg (car operand*) 'value env #f)))
         (emit `(mov rax ,(cg (cadr operand*) 'value env #f))
               `(mov rdi ,tmp)
               `(sar rdi ,(shift 'fixnum)))
         (let ((t1 (reg)))
           (emit `(mov ,t1 ,%pcb))
           (with-new-frame ()
             (emit '(call switch-stack))) ;rdi, rax
           (emit `(mov ,%pcb ,t1)))
         (emit `(mov ,%closure ,(lookup %closure env)))
         'rax))
      (($linker-address)
       ;; There can be no closure to represent this, because the
       ;; symbol table for the boot image is not available at
       ;; runtime.
       (assert (null? (cdr operand*)))
       (assert (symbol? (constant-value (car operand*))))
       (emit `(mov rax (<< ,(constant-value (car operand*))
                           ,(shift 'fixnum))))
       'rax)
      (($heap-remaining)
       ;; This is for the time-it procedure.
       (let ((tmp (reg)))
         (emit `(mov ,tmp (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)))
               `(sal ,tmp ,(shift 'fixnum)))
         tmp))

      ((apply)
       (match operand*
         [(op opnd . opnd*)
          (cg-apply op (cons opnd opnd*) ctxt env tail?)]
         [_
          (cg-primcall-proc label name operand* ctxt env tail?)]))

      ;; (loko system unsafe)
      ((syscall) (cg-syscall #f operand* ctxt env))
      (($syscall/carry!) (cg-syscall (car operand*) (cdr operand*) ctxt env))
      ((get-i/o-u8) (cg-get-i/o 'al operand* ctxt env))
      ((put-i/o-u8) (cg-put-i/o 'al operand* ctxt env))
      ((get-i/o-u8-n!) (cg-get-i/o-n! 'mem8+ operand* ctxt env))
      ((put-i/o-u8-n) (cg-put-i/o-n 'mem8+ operand* ctxt env))
      ((get-i/o-u16) (cg-get-i/o 'ax operand* ctxt env))
      ((put-i/o-u16) (cg-put-i/o 'ax operand* ctxt env))
      ((get-i/o-u16-n!) (cg-get-i/o-n! 'mem16+ operand* ctxt env))
      ((put-i/o-u16-n) (cg-put-i/o-n 'mem16+ operand* ctxt env))
      ((get-i/o-u32) (cg-get-i/o 'eax operand* ctxt env))
      ((put-i/o-u32) (cg-put-i/o 'eax operand* ctxt env))
      ((get-i/o-u32-n!) (cg-get-i/o-n! 'mem32+ operand* ctxt env))
      ((put-i/o-u32-n) (cg-put-i/o-n 'mem32+ operand* ctxt env))
      ((get-mem-u8) (cg-get-mem 'mem8+ 'movzx 'eax operand* ctxt env))
      ((get-mem-u16) (cg-get-mem 'mem16+ 'movzx 'eax operand* ctxt env))
      ((get-mem-u32) (cg-get-mem 'mem32+ 'mov 'eax operand* ctxt env))
      ((get-mem-s61) (cg-get-mem 'mem64+ 'mov 'rax operand* ctxt env))
      ((put-mem-u8) (cg-put-mem 'mem8+ 'al operand* ctxt env))
      ((put-mem-u16) (cg-put-mem 'mem16+ 'ax operand* ctxt env))
      ((put-mem-u32) (cg-put-mem 'mem32+ 'eax operand* ctxt env))
      ((put-mem-s61) (cg-put-mem 'mem64+ 'rax operand* ctxt env))

      ;; loko system $amd64. These are ALL unsafe.
      (($disable-interrupts)
       ;; TODO: it would be nice to be able to actually use CLI/STI, since
       ;; they are permitted at CPL=3 (but not under Linux).
       ;; (emit '(cli))
       (with-new-frame ()
         (emit '(call $tmp-cli)))
       (cg-void ctxt))
      (($enable-interrupts)
       ;; (emit '(sti))
       (with-new-frame ()
         (emit '(call $tmp-sti)))
       (cg-void ctxt))
      ((cpuid)
       ;; (cpuid eax [ecx])
       (let* ((op1 (reg)) (op2 (reg))
              (ra (reg)) (rb (reg)) (rc (reg)) (rd (reg)))
         (emit `(mov ,op1 ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,op2 ,(if (null? (cdr operand*))
                               0
                               (cg (cadr operand*) 'value env #f))))
         (emit `(mov rax ,op1)
               `(sar rax ,(shift 'fixnum))
               `(mov rcx ,op2)
               `(sar rcx ,(shift 'fixnum))
               `(cpuid)
               `(lea ,ra (mem+ (* rax 8)))
               `(lea ,rb (mem+ (* rbx 8)))
               `(lea ,rc (mem+ (* rcx 8)))
               `(lea ,rd (mem+ (* rdx 8)))
               `(mov ,(list-ref %ret-reg* 0) ,ra)
               `(mov ,(list-ref %ret-reg* 1) ,rb)
               `(mov ,(list-ref %ret-reg* 2) ,rc)
               `(mov ,(list-ref %ret-reg* 3) ,rd))
         (cg-mv-return 4 ctxt env tail?)))
      ((rdtsc)
       ;; TODO: double-check that this is true: Equivalent to (mod
       ;; (full-rdtsc) (greatest-fixnum)). TODO: see what happens at
       ;; wraparound by adding some large base to this value. It
       ;; wraps around after ((2^w)/(f*10^9)) seconds.
       (let ((ret (reg)))
         (let ((barrier
                (and (eqv? (length operand*) 1)
                     (constant? (car operand*))
                     (constant-value (car operand*)))))
           (case barrier
             ((start)
              ;; CPUID stops instructions before RDTSC from being
              ;; counted towards the time. The AND is to keep the
              ;; fixnum positive.
              (emit '(xor eax eax)
                    '(xor ecx ecx)
                    '(cpuid)
                    '(rdtsc)))        ;edx:eax <- tsc
             ((stop)
              ;; With RDTSCP instructions before it are all included
              ;; in the time.
              (emit '(rdtscp)))       ;edx:eax <- tsc, ecx <- cpu
             (else
              (emit '(rdtsc))))
           ;; Dependency on edx:eax here stops these
           ;; instructions from running before rdtscp/rdtsc.
           (emit `(and edx ,(- (expt 2 (- 32 (shift 'fixnum) 1))
                               1))
                 `(shl rdx ,(+ 32 (shift 'fixnum)))
                 `(lea ,ret (mem+ rdx (* rax 8))))
           (when (eq? barrier 'stop)
             ;; Stop instructions after CPUID from running before
             ;; RDTSCP. Input to CPUID will be EAX and ECX coming
             ;; from RDTSCP.
             (emit '(cpuid))))
         ret))
      ((rdrand rdseed)
       ;; Random numbers. First value is #t if the value is valid;
       ;; second value is the random value.
       (let ((ret (reg)))
         (emit `(mov rax ,(immediate #f))
               `(mov ,ret 0)
               `(,name ,ret)            ;rdrand/rdseed
               `(setc ah)
               `(shl ,ret ,(shift 'fixnum))
               `(mov ,(car %ret-reg*) rax)
               `(mov ,(cadr %ret-reg*) ,ret))
         (cg-mv-return 2 ctxt env tail?)))
      ((cache-line-flush)
       (let ((addr (reg)))
         (emit `(mov ,addr ,(cg (car operand*) 'value env #f))
               `(sar ,addr ,(shift 'fixnum))
               `(clflush (mem8+ ,addr)))
         (cg-void ctxt)))
      ((memory-fence) (emit '(mfence)) (cg-void ctxt))
      ((load-fence) (emit '(lfence)) (cg-void ctxt))
      ((store-fence) (emit '(sfence)) (cg-void ctxt))
      (($valgrind)
       ;; ($valgrind req-location)
       (assert (eqv? (length operand*) 1))
       (cg-funcall '$valgrind (make-const #f #f) operand* ctxt env tail?))

      ;; Floating point
      ((fixnum->flonum)
       (match operand*
         [(fx)
          (let ((op0 (reg)) (ret (reg)))
            (emit `(mov ,op0 ,(cg fx 'value env #f)))
            (cg-check-fixnum? op0 fx)
            ;; TODO: check the rounding mode
            (emit `(sar ,op0 ,(shift 'fixnum))
                  `(cvtsi2ss xmm1 ,op0)
                  `(movq ,ret xmm1)   ;TODO: should be movd
                  `(shl ,ret ,(shift 'flonum))
                  `(or ,ret ,(tag 'flonum)))
            ret)]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))
      ((fl=? fl>?)
       (match operand*
         [(fl0 fl1)
          (let ((op0 (reg)) (op1 (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (emit `(mov ,op1 ,(cg fl1 'value env #f)))
            (cg-check-flonum? op0 fl0 op1 fl1)
            (emit `(shr ,op0 ,(shift 'flonum))
                  `(shr ,op1 ,(shift 'flonum))
                  `(movq xmm1 ,op0)   ;TODO: should be movd
                  `(movq xmm2 ,op1)
                  `(ucomiss xmm1 xmm2))
            (case name
              ((fl=?)
               ;; TODO: would be nice to use this in cg-test. CMPSS
               ;; is somehow better, but doesn't use flags.
               (emit `(mov eax ,(immediate #t))
                     `(mov edx ,(immediate #f))
                     `(setnp ah)         ;eax=#f if NaN
                     `(cmovne eax edx))  ;eax=#f is not equal
               'rax)
              ((fl>?)
               (cg-test ctxt 'rflags.nbe))))]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))
      ((fl+ fl- fl* fl/ flmin flmax)
       (match operand*
         [(fl0 fl1)
          (let ((op0 (reg)) (op1 (reg)) (ret (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (emit `(mov ,op1 ,(cg fl1 'value env #f)))
            (cg-check-flonum? op0 fl0 op1 fl1)
            (emit `(shr ,op0 ,(shift 'flonum))
                  `(shr ,op1 ,(shift 'flonum))
                  `(movq xmm1 ,op0)   ;TODO: should be movd
                  `(movq xmm2 ,op1))
            (case name
              ((fl+) (emit `(addss xmm1 xmm2)))
              ((fl-) (emit `(subss xmm1 xmm2)))
              ((fl*) (emit `(mulss xmm1 xmm2)))
              ((fl/) (emit `(divss xmm1 xmm2)))
              ((flmin) (emit `(minss xmm1 xmm2)))
              ((flmax) (emit `(maxss xmm1 xmm2))))
            (emit `(movq ,ret xmm1)   ;TODO: should be movd
                  `(shl ,ret ,(shift 'flonum))
                  `(or ,ret ,(tag 'flonum)))
            ret)]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))
      ((flsqrt flfloor flceiling fltruncate flround)
       (match operand*
         [(fl0)
          (let ((op0 (reg)) (ret (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (cg-check-flonum? op0 fl0)
            (emit `(shr ,op0 ,(shift 'flonum))
                  `(movq xmm1 ,op0))  ;TODO: should be movd
            (case name
              ((flsqrt) (emit `(sqrtss xmm1 xmm1)))
              (else
               (cond
                 (use-sse4.1
                  ;; These require SSE 4.1
                  (case name
                    ((flfloor)    (emit `(roundss xmm1 xmm1 #b0001)))
                    ((flceiling)  (emit `(roundss xmm1 xmm1 #b0010)))
                    ((fltruncate) (emit `(roundss xmm1 xmm1 #b0011)))
                    ((flround)    (emit `(roundss xmm1 xmm1 #b0000)))))
                 (else
                  ;; Hacker's Delight, 17-2. Changing the rounding
                  ;; mode is slower, but works with just SSE2. The
                  ;; sign bit also needs to be copied manually.
                  (case name
                    ((flfloor)
                     (emit `(movd xmm2 (mem32+ c_231))
                           `(ldmxcsr (mem32+ mxcsr-floor))
                           `(addss xmm1 xmm2)
                           `(subss xmm1 xmm2)))
                    ((flceiling)
                     (emit `(movd xmm2 (mem32+ c_231))
                           `(ldmxcsr (mem32+ mxcsr-ceiling))
                           `(addss xmm1 xmm2)
                           `(subss xmm1 xmm2)))
                    ((flround)
                     (emit `(movd xmm2 (mem32+ c_231))
                           `(ldmxcsr (mem32+ mxcsr-round))
                           `(addss xmm1 xmm2)
                           `(subss xmm1 xmm2)))
                    ((fltruncate)
                     (let ((negative (vector 'fltruncate 'negative))
                           (done (vector 'fltruncate 'done)))
                       (emit `(ldmxcsr (mem32+ mxcsr-truncate))
                             `(xorps xmm0 xmm0)
                             `(ucomiss xmm1 xmm0)
                             `(jb ,negative))
                       ;; if x>=0.0 then (x+c_23) - c_23
                       (emit `(movd xmm2 (mem32+ c_23))
                             `(addss xmm1 xmm2)
                             `(subss xmm1 xmm2))
                       (emit `(jmp ,done))
                       (emit `(%label ,negative))
                       ;; if x<0.0 then c_23 - (c_23-x)
                       (emit `(movss xmm2 xmm1)
                             `(movd xmm1 (mem32+ c_23))
                             `(movss xmm0 xmm1)
                             `(subss xmm0 xmm2)
                             `(subss xmm1 xmm0))
                       (emit `(%label ,done)))))
                  ;; Now copy the sign, to handle things like
                  ;; (fltruncate -0.0).
                  (unless (eq? name 'flround)
                    (emit `(and ,op0 #x80000000) ;keep the sign bit
                          `(movq xmm0 ,op0)
                          `(orps xmm1 xmm0)))
                  (emit `(ldmxcsr (mem32+ mxcsr-round)))))))
            (emit `(movq ,ret xmm1)   ;TODO: should be movd
                  `(shl ,ret ,(shift 'flonum))
                  `(or ,ret ,(tag 'flonum)))
            ret)]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))
      ((flabs)
       (match operand*
         [(fl0)
          (let ((op0 (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (cg-check-flonum? op0 fl0)
            ;; Clear the sign bit. BZHI can also do it.
            (emit `(shl ,op0 1)
                  `(shr ,op0 1))
            op0)]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      ;; Generic arithmetic specialized for fixnums
      ((+ - * quotient remainder div mod)
       (match operand*
         [(a b)
          (if (or (inferred-as? a 'flonum) (inferred-as? b 'flonum))
              (cg-primcall-proc label name operand* ctxt env tail?)
              (let ((fallback (mklabel 'fallback))
                    (proceed (mklabel 'proceed)))
                (let* ((op0 (reg)) (op1 (reg)) (ret (reg)))
                  (emit `(mov ,op0 ,(cg a 'value env #f)))
                  (emit `(mov ,op1 ,(cg b 'value env #f)))
                  (cg-check-fixnum* op0 a op1 b fallback)
                  (emit `(mov ,ret ,op0))
                  (case name
                    ((+) (emit `(add ,ret ,op1) `(jo ,fallback)))
                    ((-) (emit `(sub ,ret ,op1) `(jo ,fallback)))
                    ((*)
                     (let lp ((a a) (b b) (op0 op0) (op1 op1))
                       (cond
                         ((and (constant? a) (not (constant? b)))
                          (lp b a op1 op0))
                         ((constant-s31? b)
                          (emit `(imul ,ret ,op0 ,(constant-value b))
                                `(jo ,fallback)))
                         (else
                          (emit `(mov ,ret ,op0) ;do not clobber op0
                                `(sar ,ret ,(shift 'fixnum))
                                `(imul ,ret ,op1)
                                `(jo ,fallback))))))
                    ((quotient remainder div mod)
                     (cond
                       ((and (memq name '(quotient div))
                             (constant-fixnum? b)
                             (let ((d (constant-value b)))
                               (or (eqv? 1 (bitwise-bit-count d))
                                   (and (fx<? d -1)
                                        (eqv? 1 (bitwise-bit-count (- d)))))))
                        (let* ((n (constant-value b))
                               (k (bitwise-first-bit-set (abs n)))
                               (skip (mklabel 'quotient)))
                          (emit `(mov ,ret ,op0))
                          (when (eq? name 'quotient)
                            (emit `(test ,ret ,ret)
                                  `(jge ,skip))
                            (let ((t (reg)))
                              (emit `(mov ,t ,(immediate (- (expt 2 k) 1)))
                                    `(add ,ret ,t)))
                            (emit `(%label ,skip)))
                          (emit `(sar ,ret ,k)
                                `(and ,ret ,(fxnot (mask 'fixnum))))
                          (when (negative? n)
                            (emit `(neg ,ret)))))

                       ((and (constant-fixnum? b)
                             (<= 2 (abs (constant-value b)) (greatest-fixnum)))
                        (let-values ([(q r) (cg-fxdiv-and-mod op0 (constant-value b)
                                                              (memq name '(div mod)))])
                          (emit `(mov ,ret ,(if (memq name '(quotient div)) q r)))))

                       ((memq name '(div mod)) ;TODO: do the adjustment
                        (emit `(jmp ,fallback)))

                       (else
                        (let ((r (if (eq? name 'quotient) 'rax 'rdx))
                              (tmp (reg)))
                          (emit `(mov rax ,op0)
                                `(sar rax ,(shift 'fixnum))
                                `(mov ,tmp ,op1)
                                `(sar ,tmp ,(shift 'fixnum))
                                `(cqo)
                                `(idiv #;rdx:rax ,tmp)
                                `(mov ,ret ,r)
                                `(sal ,ret ,(fx- (shift 'fixnum) 1))
                                `(sal ,ret 1)
                                `(jo ,fallback)))))))
                  (emit `(jmp ,proceed))
                  (emit `(%label ,fallback))
                  (emit `(%comment unlikely))
                  (emit `(mov ,ret ,(cg-primcall-proc* label name (list op0 op1) ctxt env tail?)))
                  (emit `(%label ,proceed))
                  ret)))]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((bitwise-xor bitwise-ior bitwise-and)
       (match operand*
         [(a b)
          (let ((fallback (mklabel 'fallback))
                (proceed (mklabel 'proceed)))
            (let* ((op0 (reg)) (op1 (reg)) (ret (reg)))
              (emit `(mov ,op0 ,(cg a 'value env #f)))
              (emit `(mov ,op1 ,(cg b 'value env #f)))
              (cg-check-fixnum* op0 a op1 b fallback)
              (emit `(mov ,ret ,op0))
              (case name
                ((bitwise-xor) (emit `(xor ,ret ,op1)))
                ((bitwise-ior) (emit `(or ,ret ,op1)))
                ((bitwise-and) (emit `(and ,ret ,op1))))
              (emit `(jmp ,proceed))
              (emit `(%label ,fallback))
              (emit `(%comment unlikely))
              (emit `(mov ,ret ,(cg-primcall-proc* label name (list op0 op1) ctxt env tail?)))
              (emit `(%label ,proceed))
              ret))]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((bitwise-not)
       (match operand*
         [(a)
          (let ((fallback (mklabel 'fallback))
                (proceed (mklabel 'proceed)))
            (let* ((op0 (reg)) (ret (reg)))
              (emit `(mov ,op0 ,(cg a 'value env #f)))
              (cg-check-fixnum* op0 a fallback)
              (emit `(mov ,ret ,op0))
              (emit `(not ,ret)
                    `(and ,ret ,(fxnot (mask 'fixnum))))
              (emit `(jmp ,proceed))
              (emit `(%label ,fallback))
              (emit `(%comment unlikely))
              (emit `(mov ,ret ,(cg-primcall-proc* label name (list op0) ctxt env tail?)))
              (emit `(%label ,proceed))
              ret))]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((zero? negative? positive? odd? even?)
       (match operand*
         [(a)
          (if (inferred-as? a 'flonum)
              (cg-primcall-proc label name operand* ctxt env tail?)
              (let ((fallback (mklabel 'fallback))
                    (proceed (mklabel 'proceed)))
                (let* ((op0 (reg)) (ret (reg)))
                  (emit `(mov ,op0 ,(cg a 'value env #f)))
                  (cg-check-fixnum* op0 a fallback)
                  (emit `(test ,op0 ,(case name
                                       ((odd? even?) (immediate 1))
                                       (else op0))))
                  (let ((rfix (cg-test ctxt (case name
                                              ((zero?) 'rflags.z)
                                              ((negative?) 'rflags.s)
                                              ((odd?) 'rflags.nz)
                                              ((even?) 'rflags.z)
                                              ((positive?) 'rflags.nle)))))
                    (unless (eq? rfix '*branched*)
                      (emit `(mov ,ret ,rfix)))
                    (emit `(jmp ,proceed))
                    (emit `(%label ,fallback))
                    (emit `(%comment unlikely))
                    (let ((rgen (cg-primcall-proc* label name (list op0) ctxt env tail?)))
                      (match ctxt
                        [('test . _)
                         (emit `(cmp ,rgen ,(immediate #f)))
                         (cg-test ctxt 'rflags.nz)
                         (emit `(%label ,proceed))
                         '*branched*]
                        [_
                         (emit `(mov ,ret ,rgen))
                         (emit `(%label ,proceed))
                         ret]))))))]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((= < > <= >=)
       (match operand*
         [(a b)
          (if (or (inferred-as? a 'flonum) (inferred-as? b 'flonum))
              (cg-primcall-proc label name operand* ctxt env tail?)
              (let ((fallback (mklabel 'fallback))
                    (proceed (mklabel 'proceed)))
                (let* ((op0 (reg)) (op1 (reg)) (ret (reg)))
                  (emit `(mov ,op0 ,(cg a 'value env #f)))
                  (emit `(mov ,op1 ,(cg b 'value env #f)))
                  (cg-check-fixnum* op0 a op1 b fallback)
                  (emit `(cmp ,op0 ,op1))
                  (let ((rfix (cg-test ctxt (case name
                                              ((=) 'rflags.z)
                                              ((<) 'rflags.l)
                                              ((>) 'rflags.g)
                                              ((>=) 'rflags.ge)
                                              ((<=) 'rflags.le)))))
                    (unless (eq? rfix '*branched*)
                      (emit `(mov ,ret ,rfix)))
                    (emit `(jmp ,proceed))
                    (emit `(%label ,fallback))
                    (emit `(%comment unlikely))
                    (let ((rgen (cg-primcall-proc* label name (list op0 op1) ctxt env tail?)))
                      (match ctxt
                        [('test . _)
                         (emit `(cmp ,rgen ,(immediate #f)))
                         (cg-test ctxt 'rflags.nz)
                         (emit `(%label ,proceed))
                         '*branched*]
                        [_
                         (emit `(mov ,ret ,rgen))
                         (emit `(%label ,proceed))
                         ret]))))))]
         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((put-char put-u8)
       (match operand*
         [(port ch/byte)
          (let* ((fallback (mklabel 'fallback))
                 (proceed (mklabel 'proceed))
                 (preg (reg))
                 (chreg 'rcx) (chreg32 'ecx) (chreg8 'cl)
                 (buf (reg))
                 (wpos (reg))
                 (port:buffer 1)
                 (port:buffer-wpos 4)
                 (port:buffer-wend 5)
                 (port:nl 6))
            (emit `(mov ,preg ,(cg port 'value env #f)))
            (emit `(mov ,chreg ,(cg ch/byte 'value env #f)))
            (case name
              ((put-char)
               (unless (inferred-as? ch/byte 'char)
                 (let ((tmp (reg)))
                   (emit `(mov ,tmp ,chreg)
                         `(and ,tmp ,(mask 'char))
                         `(cmp ,tmp ,(tag 'char))
                         `(jne ,fallback)))))
              ((put-u8)
               (cg-check-fixnum* chreg ch/byte fallback)))
            (let-values ([(test-mask test-tag)
                          (case name
                            ((put-char) (box-header-type-eq-mask 'port #b101 #b101))
                            (else       (box-header-type-eq-mask 'port #b101 #b001)))])
              (let ((tmp (reg)))
                (emit `(mov ,tmp (mem64+ ,preg ,(fx- (tag 'box)))) ;test: box
                      `(and ,tmp ,test-mask)
                      `(cmp ,tmp ,test-tag)
                      `(jne ,fallback))))       ;test the port
            (emit `(mov ,buf (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer)))
                  `(mov ,wpos (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer-wpos)))
                  `(cmp ,wpos (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer-wend)))
                  `(je ,fallback))        ;test: wpos != wend
            (when (eq? name 'put-char)
              (emit `(cmp ,chreg (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:nl)))
                    `(je ,fallback)))      ;newline?
            (emit `(add (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer-wpos)) ,(immediate 1)))
            (emit `(sar ,wpos ,(shift 'fixnum)))
            (case name
              ((put-u8)
               (emit `(sar ,chreg ,(shift 'fixnum))
                     `(mov (mem8+ ,buf ,wpos 8 8 ,(fx- (tag 'bytevector))) ,chreg8)))
              (else
               (emit `(mov (mem32+ ,buf (* 4 ,wpos) 8 ,(fx- (tag 'string))) ,chreg32))))
            (emit `(jmp ,proceed)
                  `(%label ,fallback))
            (emit `(%comment unlikely))
            (cg-primcall-proc* label name (list preg chreg) ctxt env tail?)
            (emit `(%label ,proceed))
            (cg-void ctxt))]

         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      ((get-u8 lookahead-u8 get-char lookahead-char)
       (match operand*
         [(port)
          (let* ((fallback (mklabel 'fallback))
                 (proceed (mklabel 'proceed))
                 (preg (reg))
                 (buf (reg))
                 (rpos (reg))
                 (ret (reg))
                 (port:buffer 1)
                 (port:buffer-rpos 2)
                 (port:buffer-rend 3))
            (let-values ([(test-mask test-tag)
                          (case name
                            ((get-char lookahead-char)
                             (box-header-type-eq-mask 'port #b110 #b110))
                            (else
                             (box-header-type-eq-mask 'port #b110 #b010)))])
              (emit `(mov ,preg ,(cg port 'value env #f)))
              (let ((tmp (reg)))
                (emit `(mov ,tmp (mem64+ ,preg ,(fx- (tag 'box)))) ;test: box
                      `(and ,tmp ,test-mask)
                      `(cmp ,tmp ,test-tag)
                      `(jne ,fallback)))       ;test the port
              (emit `(mov ,buf (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer)))
                    `(mov ,rpos (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer-rpos)))
                    `(cmp ,rpos (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer-rend)))
                    `(je ,fallback))        ;test: rpos != rend
              (case name
                ((get-u8 get-char)
                 (emit `(add (mem64+ ,preg ,(fx- (tag 'box)) ,(* 8 port:buffer-rpos)) ,(immediate 1)))))
              (case name
                ((get-u8 lookahead-u8)
                 (emit `(sar ,rpos ,(shift 'fixnum))
                       `(movzx ,ret (mem8+ ,buf ,rpos 8 8 ,(fx- (tag 'bytevector))))
                       `(sal ,ret ,(shift 'fixnum))))
                (else
                 (emit `(shr ,rpos 1)
                       `(mov eax (mem32+ ,buf ,rpos 8 ,(fx- (tag 'string))))
                       `(or eax ,(tag 'char))
                       `(mov ,ret rax))))
              (emit `(jmp ,proceed)
                    `(%label ,fallback))
              (emit `(%comment unlikely))
              (emit `(mov ,ret ,(cg-primcall-proc* label name (list preg) ctxt env tail?)))
              (emit `(%label ,proceed))
              ret))]

         [_ (cg-primcall-proc label name operand* ctxt env tail?)]))

      (else
       (cg-primcall-proc label name operand* ctxt env tail?))))

  (define (lookup var env)
    (let ((loc (assq var env)))
      (when (not loc)
        (error 'lookup "Variable not bound" (variable-name var)))
      (let ((loc (cdr loc)))
        (cond ((pair? loc)
               (cond ((eq? (car loc) 'reg)
                      loc)
                     (else
                      (assert (eq? (car loc) 'free))
                      `(mem64+ ,%closure ,(- (cdr loc) (tag 'procedure))))))
              (else
               `(mem64+ rsp ,loc LOCALS))))))

  (define (extend-env var reg env)
    (emit `(%comment frame
                     ,(if (variable? var) (variable-name var) var)
                     ,reg))
    (cons (cons var reg) env))

  (define (extend-formals lhs* formals-proper?)
    ;; The first arguments go into registers.
    (do ((lhs* lhs* (cdr lhs*))
         (reg* %arg-reg* (cdr reg*))
         (env '() (let ((r (reg)))
                    (emit `(%comment frame ,(variable-name (car lhs*)) ,r))
                    (cons (cons (car lhs*) r) env))))
        ((or (null? lhs*) (null? reg*))
         ;; Now arguments go onto the stack. The first argument has
         ;; the highest address and optional arguments are at the
         ;; lowest addresses, because we need to statically allocate
         ;; space in the frame to hold all arguments.
         (do ((si -8 (fx- si 8))
              (lhs* lhs* (cdr lhs*))
              (env env
                   (cond ((and (not formals-proper?) (null? (cdr lhs*)))
                          ;; The restargs are always in a register
                          ;; (handled by cg-consargs).
                          (let ((r (reg)))
                            (emit `(%comment frame ,(variable-name (car lhs*)) ,r))
                            (cons (cons (car lhs*) r) env)))
                         (else
                          ;; Otherwise the argument is at the given
                          ;; stack index.
                          (emit `(%comment frame ,(variable-name (car lhs*)) ,si))
                          (cons (cons (car lhs*) si) env)))))
             ((null? lhs*)
              env)))))

  (define (extend-free free* env)
    ;; Free variables start at offset 16 in the closure.
    (let lp ((i 16) (free* free*) (env env))
      (if (null? free*) env
          (lp (fx+ i 8) (cdr free*)
              (begin
                (emit `(%comment closure ,(variable-name (car free*)) ,i))
                (cons (cons (car free*) (cons 'free i)) env))))))

  (define (cg-allocate-frame number-of-formals)
    (emit `(%comment allocate-frame ,number-of-formals)
          `(%comment frame-size)))

  (define (cg-restore-callee-save env)
    (emit `(mov rbx ,(lookup 'rbx env))
          `(mov rbp ,(lookup 'rbp env))))

  (define (cg-deallocate-frame)
    (emit `(%comment deallocate-frame)))

  (define (cg-mv-return num-values ctxt env tail?)
    (cond
      ((eqv? num-values 0)
       (emit `(mov ,%value-count ,(immediate 0)))
       (when tail?
         (emit `(%comment mv-values 0)))
       (emit `(mov rax ,(cg-void ctxt))))
      (else
       (emit `(mov ,%value-count ,(immediate (fx- num-values))))
       (emit `(%comment reserve-frame-space
                        ,(fxmax 0 (fx- num-values (length %ret-reg*)))))
       (when tail?
         (emit `(%comment mv-values ,num-values))) ;liveness
       (emit `(mov rax ,(car %ret-reg*)))))
    ;; Either return to the caller (when in tail position) or to later
    ;; in the same function and same frame
    (cond (tail?
           (cg-restore-callee-save env)
           (cg-deallocate-frame)
           (emit '(stc)
                 '(ret)))
          (else
           (emit '(stc))))
    'rax)

  (define (cg-consargs formals env)
    ;; Conses rest arguments.
    (emit `(mov r10 ,(immediate (fx- (length formals) 1))) ;fixed
          ;; Make room on the stack for all arguments (including those
          ;; actually passed in registers). This potentially creates a
          ;; stack slot with junk (up to as many as there are
          ;; arguments passed in registers), but consargs cleans them
          ;; up before calling the GC.
          '(cdqe)
          '(add rsp rax)
          ;; Cons the rest args.
          '(call consargs)
          '(sub rsp rax))
    'r12)

  (define (cg-apply operator operand* ctxt env tail?)
    (define (unconsargs rest-loc reg* locals-offset li fixed-args)
      ;; expands rest-args. It takes a list of objects and places
      ;; them on the stack. TODO: catch circular lists? should be
      ;; cheap enough and maybe an error message is nicer than a
      ;; stack overflow.
      (define :loop (vector 'unconsargs 1))
      (define :done (vector 'unconsargs 2))
      (define tmp (reg))
      (define rest (reg))
      (define regargcount (length %arg-reg*))
      (define fixsize (fxmax 0 (fx* 8 (fx- fixed-args regargcount))))
      ;; rest-loc is the rest list. rax is %arg-count.
      (for-each (lambda (reg) (emit `(mov ,reg ,(immediate 0)))) reg*)
      (emit `(mov ,rest ,rest-loc))
      ;; Setup the initial argument count
      (emit `(mov ,%arg-count ,(immediate (fx- fixed-args))))
      (unless (eqv? fixed-args 0)
        (emit '(cdqe)))
      ;; Uncons into registers
      (do ((reg* reg* (cdr reg*)))
          ((null? reg*))
        (emit `(cmp ,rest ,(immediate '()))
              `(je ,:done)
              `(mov ,(car reg*) (mem64+ ,rest ,(fx- (car-offset) (tag 'pair))))
              `(mov ,rest (mem64+ ,rest ,(fx- (cdr-offset) (tag 'pair))))
              `(sub rax 8)))
      (emit `(%label ,:loop)
            `(cmp ,rest ,(immediate '()))
            `(je ,:done)
            `(mov ,tmp (mem64+ ,rest ,(fx- (car-offset) (tag 'pair))))
            `(mov ,rest (mem64+ ,rest ,(fx- (cdr-offset) (tag 'pair))))
            `(mov (mem64+ rsp rax ,(* 8 regargcount) ,(+ li fixsize) ,locals-offset) ,tmp)
            `(sub rax 8)
            `(jmp ,:loop)
            `(%label ,:done)))
    ;; The operands are: arg0 ... rest-args. This evaluates all
    ;; operands then unconses the rest-args list.
    (let ((fixed-args (fx- (length operand*) 1)))
      (let lp ((operand* operand*) (loc* '()))
        (cond
          ((pair? operand*)
           ;; Evalate the arguments
           (let ((loc (reg)))
             (emit `(mov ,loc ,(cg (car operand*) 'value env #f)))
             (lp (cdr operand*) (cons loc loc*))))
          (else
           (let ((locals-offset (if tail? 'LOCALS 0))
                 (stack-offset (if tail? 0 -8))  ;room for the return address
                 (arg-loc* (reverse (cdr loc*)))
                 (rest-loc (car loc*))
                 (closure-loc (reg)))
             (emit `(mov ,closure-loc ,(cg operator 'value env #f)))
             (when tail? (cg-restore-callee-save env))
             ;; Move the arguments into place
             (do ((reg* %arg-reg* (cdr reg*))
                  (loc* arg-loc* (cdr loc*)))
                 ((or (null? reg*) (null? loc*))
                  (do ((i (fx+ stack-offset -8) (fx- i 8))
                       (loc* loc* (cdr loc*)))
                      ((null? loc*)
                       ;; Uncons the last argument to apply. This
                       ;; will overwrite local variables.
                       (unconsargs rest-loc reg* locals-offset i fixed-args))
                    (emit `(mov (mem64+ rsp ,i ,locals-offset) ,(car loc*)))))
               (emit `(mov ,(car reg*) ,(car loc*))))
             ;; Do the call
             (emit `(mov ,%closure ,closure-loc))
             (cond
               ((not tail?)             ;apply call
                ;; Move the arguments into place (making room
                ;; for the return address written by call).
                (emit `(%comment call apply ,(length %arg-reg*)))
                (with-new-frame ()
                  (emit `(call (mem64+ ,%closure ,(fx- (tag 'procedure))))))
                (emit `(mov ,%closure ,(lookup %closure env)))
                'rax)
               (else                  ; apply tail call.
                ;; Move the arguments into place (making room for
                ;; the return address written by call). Overwrites
                ;; the incoming arguments.
                (emit `(%comment reserve-frame-space
                                 ,(fxmax 0 (fx- fixed-args (length %arg-reg*)))))
                (cg-deallocate-frame)
                (emit `(%comment call apply-tail ,(length %arg-reg*)))
                (emit `(jmp (mem64+ ,%closure ,(fx- (tag 'procedure)))))
                (cg-void ctxt)))))))))

  ;; Call a function using arguments already in pseudo registers.
  (define (cg-funcall* label operator loc* ctxt env tail?)
    (define operand-length (length loc*))
    (cond
      ((not tail?)                  ;not a tail call
       (emit `(mov ,%closure ,(cg operator 'value env #f)))
       ;; Move the arguments into place (making room for the
       ;; return address written by call).
       (do ((reg* %arg-reg* (cdr reg*))
            (loc* loc* (cdr loc*)))
           ((or (null? reg*) (null? loc*))
            (do ((i -16 (fx- i 8))
                 (loc* loc* (cdr loc*)))
                ((null? loc*))
              (emit `(mov (mem64+ rsp ,i) ,(car loc*)))))
         (emit `(mov ,(car reg*) ,(car loc*))))
       ;; Pass the number of arguments.
       (emit `(mov ,%arg-count ,(immediate (fx- operand-length))))
       (emit `(%comment call normal ,operand-length))
       (if label
           (with-new-frame ()
             (emit `(call ,label)))
           (with-new-frame ()
             (emit `(call (mem64+ ,%closure ,(fx- (tag 'procedure)))))))
       (emit `(mov ,%closure ,(lookup %closure env)))
       'rax)
      (else                         ;tail call
       ;; TODO: evaluating the operator might create a closure.
       ;; that shouldn't be necessary if this is a self-call. just
       ;; reload %closure?
       (emit `(mov ,%closure ,(cg operator 'value env #f)))
       (cg-restore-callee-save env)
       ;; The arguments have been stored in pseudo registers
       ;; (which might be spilled, but not into the reserved part
       ;; of the frame). The part of the frame storing the
       ;; incoming arguments is no longer needed.
       (do ((reg* %arg-reg* (cdr reg*))
            (loc* loc* (cdr loc*)))
           ((or (null? reg*) (null? loc*))
            (do ((i -8 (fx- i 8))
                 (loc* loc* (cdr loc*)))
                ((null? loc*))
              (emit `(mov (mem64+ rsp ,i LOCALS) ,(car loc*)))))
         (emit `(mov ,(car reg*) ,(car loc*))))
       (emit `(mov ,%arg-count ,(immediate (fx- operand-length))))
       ;; Get space for outgoing arguments.
       (emit `(%comment reserve-frame-space
                        ,(fxmax 0 (fx- operand-length (length %arg-reg*)))))
       (cg-deallocate-frame)
       (emit `(%comment call tail ,operand-length))
       (if label
           (emit `(jmp ,label))
           (emit `(jmp (mem64+ ,%closure ,(fx- (tag 'procedure))))))
       (cg-void ctxt))))

  (define (cg-funcall label operator operand* ctxt env tail?)
    (let lp ((operand* operand*) (loc* '()))
      (cond
        ;; Store operands in pseudo registers (loc*).
        ((pair? operand*)
         (let ((loc (reg)))
           (emit `(mov ,loc ,(cg (car operand*) 'value env #f)))
           (lp (cdr operand*) (cons loc loc*))))
        (else
         (cg-funcall* label operator (reverse loc*) ctxt env tail?)))))

  (define (cg-mv-call x ctxt env tail?)
    (define num-arg-regs (length %arg-reg*))
    (define label #f)                   ;TODO: where did this go?
    ;; XXX: The consumer is evaluated and saved here because there
    ;; will be an unknown number of registers live immediately after
    ;; the call to the producer, so arbitrary code should not be
    ;; emitted there.
    (let ((reg-cons (cg (mv-call-consumer x) 'value env #f)))
      (let ((reg-prod (cg (mv-call-producer x) 'value env #f)))
        ;; A multi-value producer will not have set %value-count,
        ;; RFLAGS.CF=1 and set all arguments for the consumer.
        ;;
        ;; Single-value producers clear RFLAGS.CF and set rax to their
        ;; single return value.
        (let ((tmp (reg)))
          (emit `(mov ,tmp ,(bitwise-bit-field (immediate -1) 0 32))
                `(cmovnc ,%value-count/64 ,tmp)
                `(cmovnc ,(car %arg-reg*) ,reg-prod)))
        (cond
          ((not tail?)
           ;; The producer left the arguments in just the right
           ;; locations for a call to the consumer.
           (emit `(mov ,%arg-count ,%value-count)) ;argument count
           (emit `(mov ,%closure ,reg-cons))
           (emit `(%comment call mv 0))
           (if label
               (with-new-frame ()
                 (emit `(call ,label)))
               (with-new-frame ()
                 (emit `(call (mem64+ ,%closure ,(fx- (tag 'procedure)))))))
           (emit `(mov ,%closure ,(lookup %closure env)))
           'rax)
          (else
           ;; The producer left the arguments in their own frame, which
           ;; is perfect in a non-tail call. Unfortunately we are doing
           ;; a tail call with a dynamic argument count and the
           ;; arguments are in the wrong stack frame. This needs a
           ;; dynamic loop to move them into the current frame.
           (emit `(mov ,%closure ,reg-cons))
           (cg-restore-callee-save env)
           (let ((loop (vector 'mv-call 'loop))
                 (exit (vector 'mv-call 'exit))
                 (tmp (reg))
                 (i (reg)))
             ;; XXX: %value-count is negative
             (emit `(movsxd ,%value-count/64 ,%value-count)
                   `(mov ,i ,(- (* 8 num-arg-regs)))
                   `(%label ,loop)
                   `(cmp ,i ,%value-count/64)
                   `(jle ,exit)
                   `(mov ,tmp (mem64+ rsp -16 ,i ,(* 8 num-arg-regs)))
                   `(mov (mem64+ rsp -8 LOCALS ,i ,(* 8 num-arg-regs)) ,tmp)
                   `(sub ,i 8)
                   `(jmp ,loop)
                   `(%label ,exit)))
           (emit `(mov ,%arg-count ,%value-count))
           (cg-deallocate-frame)
           (emit `(%comment call mv-tail ,num-arg-regs))
           (if label
               (emit `(jmp ,label))
               (emit `(jmp (mem64+ ,%closure ,(fx- (tag 'procedure))))))
           'rax)))))

  (define (cg-mv-let x ctxt env tail?)
    (assert (funcall? (mv-let-expr x))) ;affects the offset of the return values
    (let ((reg-expr (cg (mv-let-expr x) 'value env #f)))
      ;; The expression will now have produced some values. Same
      ;; return convention as in mv-call.
      (let ((err (vector 'badret))
            (tmp (reg)))
        (emit `(mov ,tmp ,(bitwise-bit-field (immediate -1) 0 32))
              `(cmovnc ,%value-count/64 ,tmp)  ;single value
              `(cmovnc ,(car %ret-reg*) ,reg-expr)
              `(cmp ,%value-count ,(immediate (fx- (length (mv-let-lhs* x)))))
              `(jne bad-mv-let))
        (let lp ((lhs* (mv-let-lhs* x)) (reg* %ret-reg*) (n 0) (env env))
          (cond ((pair? lhs*)
                 (with-loc ((env loc) (env (car lhs*)))
                   (cond ((pair? reg*)
                          (emit `(mov ,loc ,(car reg*)))
                          (lp (cdr lhs*) (cdr reg*) n env))
                         (else
                          (emit `(mov ,loc (mem64+ rsp -16 ,n)))
                          (lp (cdr lhs*) reg* (fx+ n -8) env)))))
                (else
                 (cg (mv-let-body x) ctxt env tail?)))))))

  (define (cg-mv-values x ctxt env tail?)
    ;; This implement a multiple-value return, never returns just a
    ;; single value and is always in a tail context. The job is to
    ;; evaluate the expressions and move the values into registers
    ;; (and the current stack frame, if there are many values).
    (let* ((expr* (mv-values-expr* x))
           (values-length (length expr*)))
      (cond
        ((null? expr*)
         (cg-mv-return 0 ctxt env tail?))
        (else
         (let ((reg* (map (lambda (_) (reg)) expr*)))
           (do ((reg* reg* (cdr reg*))
                (expr* expr* (cdr expr*)))
               ((null? expr*))
             (emit `(mov ,(car reg*) ,(cg (car expr*) 'value env #f))))
           ;; Move values into place
           (do ((reg* reg* (cdr reg*))
                (ret* %ret-reg* (cdr ret*))
                (si 0 (fx+ si 1)))
               ((or (null? ret*) (null? reg*))
                ;; This is exactly the same as during a tail call, but
                ;; we then just do a return instead. A tail return.
                (do ((i -8 (fx- i 8))
                     (reg* reg* (cdr reg*)))
                    ((null? reg*))
                  (emit `(mov (mem64+ rsp ,i LOCALS) ,(car reg*)))))
             (emit `(mov ,(car ret*) ,(car reg*))))
           (cg-mv-return (length expr*) ctxt env tail?))))))

  (define (cg-labels x label env tail?)
    (unless (labels? x)
      (error 'cg-labels "Not a label" x env tail?))
    ;; First compile the body of the labels.
    (let ((end-label (vector 'end)))
      (emit '(%comment procedure)
            '(%align 8)
            `(%label ,label ,end-label))
      (reset-pseudo-register-counter!)
      (emit '(%comment allocate-frame 0)
            '(%comment frame-size))
      (with-loc ((env save-closure) (env %closure))
        (emit `(mov ,save-closure ,%closure))
        (with-loc ((env save-rbx) (env 'rbx))
          (emit `(mov ,save-rbx rbx))
          (with-loc ((env save-rbp) (env 'rbp))
            (emit `(mov ,save-rbp rbp))
            (emit `(mov rax ,(cg (labels-body x) 'value env tail?))
                  `(mov ,%closure ,save-closure))
            (cg-restore-callee-save env)
            (cg-deallocate-frame)
            (emit '(clc)
                  '(ret)
                  `(%label ,end-label)
                  '(%comment procedure-end))))))
    ;; Compile the procedures in the labels.
    (do ((proc* (labels-proc* x) (cdr proc*)))
        ((null? proc*))
      (let* ((x (car proc*))
             (end-label (proc-end-label x)))
        ;; The general entry label of a case-lambda can be used
        ;; as the label field in procedure objects. Entry points
        ;; are therefore aligned so that they may be treated as
        ;; fixnums.
        (reset-pseudo-register-counter!)
        (emit '(%comment procedure)
              `(%align 8)
              `(%label ,(proc-label x) ,end-label))
        (cond ((const-value (proc-source x))
               => (lambda (s) (emit `(%comment source ,s)))))
        (do ((cases (proc-cases x) (cdr cases)))
            ((null? cases))
          (let* ((c (car cases))
                 (info (proccase-info c))
                 (formals (caseinfo-formals info)))
            (cond ((caseinfo-proper? info)
                   (if (null? formals)
                       (emit `(test ,%arg-count ,%arg-count))
                       (emit `(cmp ,%arg-count ,(immediate (fx- (length formals))))))
                   (emit `(je ,(caseinfo-label info))))
                  ((null? (cdr formals))
                   (emit `(jmp ,(caseinfo-label info))))
                  (else
                   (emit `(cmp ,%arg-count ,(immediate (fx- (fx- (length formals) 1)))))
                   (emit `(jle ,(caseinfo-label info)))))))
        ;; XXX: This can be a jump because r15 already contains
        ;; information about the callee.
        (emit `(%comment unlikely))
        (emit `(jmp formals-mismatch))
        ;; Emit the bodies. By emitting these in reverse order it
        ;; becomes possible to always remove the unconditional
        ;; jump above by doing branch reversal.
        (do ((cases (reverse (proc-cases x)) (cdr cases)))
            ((null? cases))
          (let* ((c (car cases))
                 (info (proccase-info c))
                 (formals (caseinfo-formals info))
                 (label (caseinfo-label info)))
            (let ((env (extend-free (proc-free x)
                                    (extend-formals formals (caseinfo-proper? info))))
                  (arg-space (fxmax 0 (fx- (length formals) (length %arg-reg*)))))
              (emit `(%label ,(caseinfo-label info)))
              (cond ((and (not (caseinfo-proper? info))
                          ;; FIXME: should be residual-referenced?,
                          ;; at least if cp0 was used
                          (variable-referenced? (last formals)))
                     (let ((list (cg-consargs formals env)))
                       (cg-allocate-frame (fxmax 0 (fx- arg-space 1)))
                       (emit `(mov ,(lookup (last formals) env) ,list))))
                    (else
                     ;; We want space in the frame for holding the
                     ;; arguments that were passed on the stack.
                     ;; This is part of the space made available
                     ;; after consargs has run.
                     (cg-allocate-frame arg-space)))
              ;; Move the argument registers into pseudo registers.
              (do ((reg* %arg-reg* (cdr reg*))
                   (formals formals (cdr formals)))
                  ((or (null? reg*) (null? formals)
                       (and (not (caseinfo-proper? info))
                            (null? (cdr formals)))))
                (emit `(mov ,(lookup (car formals) env) ,(car reg*))))
              ;; Pseudo registers for the callee-save registers.
              (with-loc ((env save-closure) (env %closure))
                (emit `(mov ,save-closure ,%closure))
                (with-loc ((env save-rbx) (env 'rbx))
                  (emit `(mov ,save-rbx rbx))
                  (with-loc ((env save-rbp) (env 'rbp))
                    (emit `(mov ,save-rbp rbp))
                    ;; Evaluate the body and return.
                    (emit `(mov rax ,(cg (proccase-body c) 'value env 'tail)))
                    (cg-restore-callee-save env)
                    (cg-deallocate-frame)
                    (emit '(clc)
                          '(ret))))))))
        (emit `(%label ,end-label)
              '(%comment procedure-end))))
    ;; For the return value of the labels body.
    'rax)

  (define (closure-size c)
    ;; Combinators do not need to be allocated at runtime, so they
    ;; take no memory.
    (if (combinator? c)
        0
        (fx+ (fx* 8 (length (closure-free* c)))
             16)))

  (define (cg-closure x ctxt env)
    ;; Closures: label, info, free*.
    (define-record-type info
      (sealed #t)
      (nongenerative loko-procinfo-555bfd14-d155-420f-a058-8ccd8ab0301e)
      (fields name (mutable free-length) source label end-label))
    (define info-rtd (record-type-descriptor info))
    (when debug
      (display (if (combinator? x) "A COMBINATOR: " "A CLOSURE: "))
      (write (record->sexpr x)) (newline)
      (newline))
    (let* ((c (closure-code x))
           (info (vector 'info (proc-label c)))
           (name (encode-const (proc-name c)))
           (source (encode-const (proc-source c))))
      (emit-data '(%align 8 0))
      (emit-data `(%label ,info))
      (emit-data `(%u64 ,(encode-const (make-const info-rtd #f))
                        ,name
                        ,(immediate (length (closure-free* x)))
                        ,source
                        (<< ,(proc-label c) ,(shift 'fixnum))
                        (<< ,(proc-end-label c) ,(shift 'fixnum))))
      (cond ((combinator? x)
             ;; There are no free variables, so all the necessary
             ;; information is available at compile time.
             ;; TODO: (eq? (closure foo) (closure foo)) => #t
             (let ((ret (vector 'closure (const-value (proc-name c)))))
               (emit-data '(%align 8 0))
               (emit-data `(%label ,ret))
               (emit-data `(%u64 ,(proc-label c)
                                 (+ ,info ,(tag 'box))))
               `(+ ,ret ,(tag 'procedure))))
            (else
             (unless (eq? ctxt 'fix)
               ;; The overflow check was already done by fix.
               (cg-allocation (closure-size x)))
             (let ((alloc (reg)))
               (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER))))
               (emit `(mov (mem64+ ,alloc) ,(proc-label c)))
               (emit `(mov (mem64+ ,alloc 8) (+ ,info ,(tag 'box))))
               ;; Store the free variables.
               (do ((free* (closure-free* x) (cdr free*))
                    (offset (* 2 8) (fx+ offset 8)))
                   ((null? free*))
                 (let ((t (reg)))
                   (emit `(mov ,t ,(lookup (car free*) env)))
                   (emit `(mov (mem64+ ,alloc ,offset) ,t))))
               ;; Construct the closure pointer.
               (let ((t (reg)))
                 (emit `(lea ,t (mem+ ,alloc ,(tag 'procedure))))
                 (emit `(add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER))
                             ,(closure-size x)))
                 t))))))

  (define (cg-fix x ctxt env tail?)
    ;; rhs* is always closures (or combinators). tmp points to the
    ;; first closure (which gets filled in later by cg-closure).
    (cg-allocation (fold-left + 0 (map closure-size (fix-rhs* x))))
    (let ((tmp (reg)) (alloc (reg)))
      (emit `(mov ,alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
            `(lea ,tmp (mem+ ,alloc ,(tag 'procedure))))
      (let lp ((lhs* (fix-lhs* x)) (rhs* (fix-rhs* x)) (env env))
        (cond ((pair? lhs*)
               (with-loc ((env loc) (env (car lhs*)))
                 (cond ((combinator? (car rhs*))
                        ;; This one gets stored in .data.
                        (emit `(mov ,loc ,(cg-closure (car rhs*) ctxt env))))
                       (else
                        ;; Construct an empty closure.
                        (let ((size (closure-size (car rhs*))))
                          (emit `(mov ,loc ,tmp)
                                `(add ,tmp ,size)))))
                 (lp (cdr lhs*) (cdr rhs*) env)))
              (else
               ;; Emit the right-hand sides. Cleverly arranged so
               ;; that cg-closure will use the same addresses as
               ;; were calculated above.
               (do ((rhs* (fix-rhs* x) (cdr rhs*)))
                   ((null? rhs*))
                 (assert (closure? (car rhs*)))
                 (unless (combinator? (car rhs*))
                   (cg-closure (car rhs*) 'fix env)))
               ;; And finally emit the body.
               (cg (fix-body x) 'value env tail?))))))

  (define (cg x ctxt env tail?)
    (when debug
      (display #\:)
      (write (record->sexpr x)) (newline))
    (cond
      ((ref? x)
       (lookup (ref-name x) env))
      ((fix? x)
       (cg-fix x ctxt env tail?))
      ((closure? x)
       (cg-closure x ctxt env))
      ((bind? x)
       (let ((old-env env))
         (let lp ((lhs* (bind-lhs* x)) (rhs* (bind-rhs* x)) (env env))
           (cond ((pair? lhs*)
                  (with-loc ((env loc) (env (car lhs*)))
                    (emit `(mov ,loc ,(cg (car rhs*) 'value old-env #f)))
                    (lp (cdr lhs*) (cdr rhs*) env)))
                 (else
                  (cg (bind-body x) ctxt env tail?))))))
      ((infer? x)
       (cg (infer-expr x) ctxt env tail?))
      ((const? x)
       (match ctxt
         [('test consequence alternative)
          (if (const-value x)
              (emit `(jmp ,consequence))
              (emit `(jmp ,alternative)))
          '*branched*]
         [_
          (cond ((encode-const x) =>
                 (lambda (v)
                   (let ((t (reg)))
                     (emit `(mov ,t ,v))
                     t)))
                (else
                 (error who "Internal error: constant not encoded" (const-value x))))]))
      ((seq? x)
       (cg (seq-e0 x) 'effect env #f)
       (cg (seq-e1 x) ctxt env tail?))
      ((test? x)
       (let* ((consequence (mklabel 'if-conseq))
              (alternative (mklabel 'if-alt))
              (exit (mklabel 'if-exit)))
         (let ((result (cg (test-expr x) `(test ,consequence ,alternative) env #f)))
           (unless (eq? result '*branched*)
             (emit `(cmp ,result ,(immediate #f)))
             (emit `(je ,alternative)))
           (emit `(%label ,consequence))
           (cg-branch-instrumentation)
           (let* ((consequence0 (mklabel 'chain-conseq0))
                  (alternative0 (mklabel 'chain-altern0))
                  (ctxt0 (match ctxt
                           [('test . _) (list 'test consequence0 alternative0)]
                           [_ 'value])))
             (let* ((t (reg))
                    (r0 (cg (test-then x) ctxt0 env tail?)))
               (unless (eq? r0 '*branched*)
                 (emit `(mov ,t ,r0)))
               (emit `(jmp ,exit))
               (emit `(%label ,alternative))
               (cg-branch-instrumentation)
               (let* ((consequence1 (mklabel 'chain-conseq1))
                      (alternative1 (mklabel 'chain-altern1))
                      (ctxt1 (match ctxt0
                               [('test . _) (list 'test consequence1 alternative1)]
                               [_ 'value])))
                 (let ((r1 (cg (test-else x) ctxt1 env tail?)))
                   (unless (eq? r1 '*branched*)
                     (emit `(mov ,t ,r1)))
                   (emit `(jmp ,exit))
                   (match ctxt
                     [('test consequence^ alternative^)
                      (letrec ((f (lambda (r lbl const exit)
                                    ;; If the other expr of the test did
                                    ;; not branch then a value needs to
                                    ;; be created for this branch also.
                                    (cond ((eq? r '*branched*)
                                           (emit `(%comment chain ,r0 ,(record->sexpr (test-then x))
                                                            ,r1 ,(record->sexpr (test-else x))))
                                           (emit `(jmp ,lbl)))
                                          (else
                                           (emit `(%comment materialize ,r0 ,(record->sexpr (test-then x))
                                                            ,r1 ,(record->sexpr (test-else x))))
                                           (emit `(mov ,t ,const)
                                                 `(jmp ,exit)))))))
                        (emit `(%label ,consequence0))
                        (f r1 consequence^ (immediate #t) exit)
                        (emit `(%label ,alternative0))
                        (f r1 alternative^ (immediate #f) exit)
                        (emit `(%label ,consequence1))
                        (f r0 consequence^ (immediate #t) exit)
                        (emit `(%label ,alternative1))
                        (f r0 alternative^ (immediate #f) exit)
                        (emit `(%label ,exit))
                        (cond ((and (eq? r0 '*branched*) (eq? r1 '*branched*))
                               '*branched*)
                              (else
                               t)))]
                     [_
                      (emit `(%label ,exit))
                      t]))))))))

      ((funcall? x)
       (let* ((operator (funcall-operator x))
              (operand* (funcall-operand* x)))
         (cond
           ((primref? operator)
            (cg-primcall (funcall-label x) (primref-name operator) operand* ctxt env tail?))
           (else
            (cg-funcall (funcall-label x) operator operand* ctxt env tail?)))))

      ;; These three are introduced by pass-loops:
      ((mutate? x)
       ;; This is used by pass-loops and can never update closure
       ;; variables.
       (let ((t (reg)))
         (emit `(mov ,t ,(cg (mutate-expr x) 'value env #f)))
         (let ((loc (lookup (mutate-name x) env)))
           (assert (not (memq %closure loc)))
           (emit `(mov ,loc ,t)))))
      ((goto? x)
       (cg-source (goto-source x))
       (emit `(jmp ,(goto-label x)))
       (cg-void ctxt))
      ((tagbody? x)
       (cg-source (tagbody-source x))
       (emit `(%label ,(tagbody-label x)))
       (cg (tagbody-body x) 'value env tail?))

      ;; Introduced by pass-values:
      ((mv-call? x)
       (cg-mv-call x ctxt env tail?))
      ((mv-let? x)
       (cg-mv-let x ctxt env tail?))
      ((mv-values? x)
       (cond
         (tail?
          (cg-mv-values x ctxt env tail?))
         ((null? (mv-values-expr* x))
          (cg-void ctxt))
         (else
          (let lp ((x* (cdr (mv-values-expr* x))))
            (unless (null? x*)
              (cg (car x*) 'effect env #f)
              (lp (cdr x*))))
          (cg (car (mv-values-expr* x)) ctxt env tail?))))

      (else
       (error who "Internal error: unknown code type" x))))

  (let ((labels
         (map (lambda (code)
                (string->symbol
                 (call-with-string-output-port
                   (lambda (p)
                     (display "lib" p)
                     (for-each (lambda (sym)
                                 (put-char p #\_)
                                 (display sym p))
                               (labels-top-level-name code))))))
              codes)))
    ;; First emit a procedure that calls each of the codes.
    (let ((end (vector 'end-start)))
      (emit '(%comment procedure)
            '(%align 8)
            `(%label scheme-start ,end))
      (let lp ((labels labels))
        (cond ((null? labels))
              ((null? (cdr labels))
               (emit `(mov ,%closure ,(immediate #f)))
               (emit `(jmp ,(car labels)))
               (emit `(%label ,end)
                     '(%comment procedure-end)))
              (else
               (emit `(mov ,%closure ,(immediate #f)))
               (with-new-frame ()
                 (emit `(call ,(car labels))))
               (lp (cdr labels))))))
    ;; Now emit the codes.
    (for-each (lambda (label code)
                (assert (labels? code))
                (cg-labels code label '() #t))
              labels codes))

  (when make-init-code?
    (init-globals!)

    ;; RTDs that become part of the image
    (let-values ([(_ vs) (hashtable-entries rtds)])
      (let ((bootstrap-rtds (vector->list vs)))
        (emit-data '(%align 16 0))
        (emit-data '(%label bootstrap-rtds))
        (emit-data `(%u64 ,(immediate (length bootstrap-rtds))
                          ,@bootstrap-rtds))))
    ;; Make a minimal perfect hashtable of the bootstrap symbol
    ;; interning table.
    (emit-data '(%align 16 0))
    (emit-data `(%label bootstrap-symbols))
    ;; k* is a vector of host symbols and v* is a vector of assembler
    ;; labels for those symbols. Things like (+ #(const ...) 1).
    (let ((str->sym (make-hashtable string-hash string=?)))
      (let-values (((k* v*) (hashtable-entries symbols)))
        (vector-for-each
         (lambda (k v)
           (hashtable-set! str->sym (symbol->string k) v))
         k* v*)
        ;; The perfect hashtable can take any string and find the
        ;; symbol that matches it, if there is such a symbol. You
        ;; always gets some symbol, but you must verify that it's the
        ;; right one.
        (let-values (((G K V) (hashtable->minimal-perfect-hashtable str->sym)))
          ;; TODO: G could be a vector of integers that can fit the
          ;; length of the vector * 2, maybe an s16.
          (let ((G-label (vector 'G)) (V-label (vector 'V)))
            (if (< (car-offset) (cdr-offset))
                (emit-data `(%u64 (+ ,G-label ,(tag 'vector))
                                  (+ ,V-label ,(tag 'vector))))
                (emit-data `(%u64 (+ ,V-label ,(tag 'vector))
                                  (+ ,G-label ,(tag 'vector)))))
            (emit-data '(%align 16 0))
            (emit-data `(%label ,G-label))
            (emit-data `(%u64 ,(immediate (vector-length G))
                              ,@(vector->list (vector-map immediate G))))
            (emit-data '(%align 16 0))
            (emit-data `(%label ,V-label))
            (emit-data `(%u64 ,(immediate (vector-length V))
                              ,@(vector->list V))))))))

  (values (reverse code)
          (reverse data))))
