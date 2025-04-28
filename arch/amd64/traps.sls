;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Convert traps to conditions

(library (loko arch amd64 traps)
  (export)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (loko match)
    (only (loko system unsafe) get-mem-u8)
    (only (loko runtime control) register-error-invoker)
    (only (loko runtime conditions) make-program-counter-condition
          make-continuation-condition)
    (only (loko runtime conditions) make-program-counter-condition)
    (only (loko runtime scheduler) out-of-memory)
    (only (loko arch amd64 memory) HEAP-MARGIN)
    (loko arch amd64 disassembler)
    (loko system $primitives))

(define (recover-explicit-condition rip live-data code-window)
  ;; Look at a few of the AMD64 instructions at the restart point of
  ;; an explicit trap. The code generator uses with-restart-handler
  ;; to get the program here via UD2. Nothing else uses UD2. No
  ;; clash with other generated code is therefore possible.
  (define (OVERFLOW? x) (memq x '(jo jno)))
  (define (BOUND? x) (memq x '(jb jnb jbe jnbe)))
  (define (EQ? x) (memq x '(jz jnz)))
  (define (assvio msg)
    (condition (make-assertion-violation)
               (make-message-condition msg)
               (make-program-counter-condition rip)))
  (define (impvio msg)
    (condition (make-implementation-restriction-violation)
               (make-message-condition msg)
               (make-program-counter-condition rip)))
  (define (irr c . irritants)
    (condition c (make-irritants-condition irritants)))
  (define (not-spill-code? x)
    (match x
      [('mov ('mem64+ 'rsp . _) _)
       #f]
      [('mov _ ('mem64+ 'rsp . _))
       #f]
      [_ #t]))
  ;; TODO: recover irritants
  (let lp ((code-window (filter not-spill-code? code-window)))
    (match code-window
      ;; Various overflows
      ([('neg . _) ((? OVERFLOW?) . _) . _]
       (irr (impvio "The result of (fx- (least-fixnum)) is not a fixnum")
            (least-fixnum)))
      ([('add . _) ((? OVERFLOW?) . _) . _]
       (impvio "Overflow in fx+"))
      ([('sub . _) ((? OVERFLOW?) . _) . _]
       (impvio "Overflow in fx-"))
      ([('imul . _) ((? OVERFLOW?) . _) . _]
       (impvio "Overflow in fx* or fxarithmetic-shift-left"))
      ([('shl . _) ((? OVERFLOW?) . _) . _]
       (impvio "Overflow in fxarithmetic-shift-left or (fx* 2 ...)"))
      ;; Type checks
      ([('test _ #b111) ((? EQ?) . _)]
       (assvio "Type error: expected a fixnum"))
      ([('cmp _ #b11111) ((? EQ?) . _)]
       (assvio "Type error: expected a character"))
      ([('cmp _ #x4f) ((? EQ?) . _)]
       (assvio "Type error: expected a flonum"))
      ;; Index checks
      ([('cmp reg ('mem64+ _ -6)) ((? BOUND?) . _) . _]
       ;; TODO: here reg should be inspected, because it might not
       ;; even be a fixnum.
       (assvio "The given vector index is out of range (or not a fixnum)"))
      ([('cmp reg ('mem64+ _ -4)) ((? BOUND?) . _) . _]
       (assvio "The given string index is out of range"))
      ([('cmp reg ('mem64+ _ -5)) ((? BOUND?) . _) . _]
       (assvio "The given bytevector index is out of range"))
      ;; Unicode scalar value.
      ;; #x3ff8 = immediate (- #xDFFF #xD800)
      ([('cmp reg #x3ff8) ((? BOUND?) . _) . _]
       (assvio "The given integer is in the Unicode High Surrogate Area"))
      ;; #x87FFF8 = immediate #x10FFFF
      ([('cmp reg #x87FFF8) ((? BOUND?) . _) . _]
       (assvio "The given integer is not a Unicode scalar value"))
      ;; Fixnum width
      ([('cmp cl 61) . _]
       (assvio "The given integer is not a valid shift count"))
      ([_ . rest] (lp rest))
      ([] #f))))

(define (recover-memory-condition live-data inst rip category closure)
  ;; The given instruction has caused #AC/#GP/#SS. Try to find out
  ;; what it was doing. TODO: use live-data to
  ;; to find the irritants
  ;; TODO: locate the source of the call
  (define (disp->type disp)
    (case (fx- 8 (bitwise-and disp #b111))
      ((#b001) "boxed object")
      ((#b010) "pair")
      ((#b011) "procedure")
      ((#b100) "string")
      ((#b110) "vector")
      ((#b101) "bytevector")
      ;; XXX: this is one of those impossible things
      (else "bad pointer")))
  (define (ref disp)
    (let ((type (disp->type disp)))
      (and type
           (condition
            (make-assertion-violation)
            (make-message-condition
             (string-append "Type error: expected a " type))
            (make-program-counter-condition rip)))))
  (define (set disp)
    (let ((type (disp->type disp)))
      (and type
           (condition
            (make-assertion-violation)
            (make-message-condition
             (if (eq? category 'page-fault)
                 (string-append "Attempted to mutate an immutable " type)
                 (string-append "Type error: expected to mutate a " type)))
            (make-program-counter-condition rip)))))
  (match inst
    (((or 'mov 'cmp) dst ('mem64+ r disp))
     (ref disp))
    (('cmp ('mem64+ r disp) op2)
     (ref disp))
    (('mov ('mem64+ r disp) src)
     (set disp))
    (('mov ('mem32+ r disp _) src)
     (set disp))
    (((or 'call 'jmp) ('mem64+ 'r15 disp))
     (and (equal? (disp->type disp) "procedure")
          (condition
           (make-assertion-violation)
           (make-who-condition 'apply)
           (make-message-condition "Tried to call a non-procedural object")
           (make-irritants-condition (list closure))
           (make-program-counter-condition rip))))
    (_ #f)))

;; See invoke-error in (loko arch amd64 lib).
(define (raise-trap* category closure rip irritants)
  (define-record-type info
    (sealed #t)
    (nongenerative loko-procinfo-555bfd14-d155-420f-a058-8ccd8ab0301e)
    (fields name (mutable free-length) source label end-label))
  (define disasm-rip rip)
  (define (copy-inst addr)
    (and (fx<=? (* 2 1024 1024) addr (expt 2 32))
         (do ((ret (make-bytevector 15))
              (i 0 (fx+ i 1)))
             ((fx=? i (bytevector-length ret))
              ret)
           (bytevector-u8-set! ret i (get-mem-u8 (fx+ addr i))))))
  (define (get-instruction)
    ;; Disassembles the instruction at disasm-rip and increments
    ;; disasm-rip to point past that instruction.
    (let ((bytes (and (procedure? disassemble1)
                      (copy-inst disasm-rip))))
      (and bytes
           (disassemble1 bytes (lambda (tag . b*)
                                 (set! disasm-rip (fx+ disasm-rip (length b*))))))))
  (define (get-instructions-up-to-branch)
    ;; Start reading instructions from disasm-rip up to the branch
    ;; that jumped to rip.
    (let lp ((window '()) (i 6))
      (case i
        ((0) (reverse window))        ;stop early
        (else
         (let ((inst (get-instruction)))
           (match inst
             ((Jcc ('+ 'rip disp))
              (reverse (cons inst window)))
             (else
              (lp (cons inst window) (fx- i 1)))))))))
  (define (closure->who closure)
    ;; Returns a condition with zero, one or two &who conditions.
    (if (not (procedure? closure))
        (condition)
        (let ((info ($procedure-info closure)))
          (if (not (info? info))
              (condition)
              (let ((name (info-name info))
                    (source (info-source info)))
                (condition
                 (if (symbol? name)
                     (make-who-condition (string->symbol (symbol->string name)))
                     (condition))
                 (if (vector? source)
                     (make-who-condition
                      (string-append (vector-ref source 0) ":"
                                     (number->string (vector-ref source 1)) ":"
                                     (number->string (vector-ref source 2))))
                     (condition))))))))
  (define (return c)
    (let ((who (closure->who closure)))
      (let ((c* (simple-conditions c)))
        (apply condition (car c*) who (cdr c*)))))
  (define (generic type message . irritants)
    (return
     (condition type
                (make-message-condition message)
                (make-irritants-condition irritants)
                (make-program-counter-condition rip))))
  (define (translate-DE inst)
    (match inst
      (('idiv . _) "Division by zero")
      (_ #f)))
  (define (translate-explicit-trap)
    ;; An explicitly inserted trap instruction has been found
    ;; (currently UD2, maybe INT1 or INT3 in the future). Look up the
    ;; address of the current closure and find the branch to the trap
    ;; instruction.
    (cond
      ((procedure? closure)
       (let ((info ($procedure-info closure)))
         (set! disasm-rip (info-label info))
         (unless (fx<=? disasm-rip rip (info-end-label info))
           (generic (make-error) "Wrong closure register value" closure))
         (let lp ((window '()) (len 0))
           (when (fx>? len 6)
             (set-cdr! (list-tail window 5) '()))
           (when (fx>? disasm-rip (info-end-label info))
             (generic (make-error) "It's off the rails" closure))
           (let ((instr (get-instruction)))
             (match instr
               ((_Jcc ('+ 'rip disp))
                (cond ((fx=? (fx+ disasm-rip disp) rip)
                       (return (or (recover-explicit-condition rip #f (reverse (cons instr window)))
                                   (condition
                                    (make-assertion-violation)
                                    (make-message-condition "An unrecognized error was trapped")
                                    (make-irritants-condition window)
                                    (make-program-counter-condition rip)))))
                      (else
                       (lp '() 0))))    ;reset the window at other branches
               (_ (lp (cons instr window) (fx+ len 1))))))))
      (else
       ;; If there is no closure there's something's very wrong.
       (generic (make-error) "It has all gone terribly wrong" closure))))
  (define (find-condition category closure rip irritants)
    (case category
      ;; Software-triggered.
      ((formals)
       (return (condition
                (make-assertion-violation)
                (make-message-condition "Expected a different number of arguments")
                (make-irritants-condition irritants))))
      ((undefined)
       ;; XXX: disassemble could result in an &invalid-opcode
       ;; condition if the instruction *really* is invalid
       (let ((inst (get-instruction)))
         (cond ((equal? inst '(ud2))
                (translate-explicit-trap))
               (else
                (generic (make-error)
                         "The program has used an unsupported CPU instruction" inst)))))
      ;; Hardware triggered
      ((alignment noncanonical page-fault)
       (let ((inst (get-instruction)))
         (return (or (and inst (recover-memory-condition #f inst rip category closure))
                     (condition (make-assertion-violation)
                                (make-message-condition
                                 "An unrecognized condition was detected by the hardware")
                                (make-irritants-condition (list inst))
                                (make-program-counter-condition rip))))))
      ((accvio)
       ;; TODO
       (generic (make-error) "The hardware has detected an error" category))
      ((divide)
       ;; This is #DE.
       (let* ((inst (get-instruction))
              (msg (and inst (translate-DE inst))))
         (if msg
             (generic (make-assertion-violation) msg)
             (generic (make-assertion-violation)
                      "The program performed an undefined mathematical operation"
                      inst))))
      ((memory)
       ;; Out of memory, but not completely.
       (return
        (condition (make-implementation-restriction-violation)
                   (make-message-condition "Out of memory")
                   (make-program-counter-condition rip))))
      (else
       (generic (make-error)
                "There was an error detected at the low level" category))))
  (cond ((eq? category 'memory)
         ;; Out of memory errors don't capture the continuation and
         ;; get no stack traces. Allocating a variable amount of
         ;; memory when memory is tight is difficult.
         (raise (find-condition category closure rip irritants)))
        (else
         (call/cc
           (lambda (k)
             (raise (condition (find-condition category closure rip irritants)
                               (make-continuation-condition k))))))))

(define (raise-trap category closure rip irritants)
  (case category
    ((memory)
     (when (fx<? ($heap-remaining) (fxdiv HEAP-MARGIN 10))
       (out-of-memory))
     (raise-trap* category closure rip irritants))
    (else
     (raise-trap* category closure rip irritants))))

(register-error-invoker raise-trap))
