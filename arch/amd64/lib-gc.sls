;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Stop-and-copy garbage collector

(library (loko arch amd64 lib-gc)
  (export
    lib-gc:text lib-gc:data)
  (import
    (loko arch amd64 config)
    (loko arch amd64 objects)
    (only (loko arch amd64 memory) HEAP-MARGIN)
    (loko runtime context)
    (rnrs (6)))

(define (pvec i) (+ (* 8 i) (- (tag 'vector))))
(define %scan 'r12)                     ;scan pointer
(define %heap-rem 'r13)                 ;heap remaining
(define %alloc 'r11)                    ;allocation pointer

(define (lib-gc-stop-and-copy)
  (define gc-stack '#(gc stack))
  (define gc-raise '#(gc raise))
  (define gc-margin '#(gc margin))
  `((%align 8)
    (%label stop-and-copy relocate)

    ;; Must preserve rsp and relocate %closure, rbx and rbp. Must
    ;; update %pcb. The stack is split in one area that only contains
    ;; live references and another area where the NOPs after the
    ;; return addresses determine what parts of the stack frame are
    ;; live. Everything else which may be of interest must have been
    ;; saved on the stack. The stuff which is preserved here must
    ;; match what analyzer.scm has.
    (push ,%closure)
    (push rbx)
    (push rbp)

    ;; Remove anything on the stack that isn't live. The caller sets
    ;; rdi to point right after the first return address where the
    ;; stack tracing mechanism will work.
    (lea rbx (mem+ rdi -8))
    (mov rbp (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:STACK-TOP)))
    (call cleanup-stack)

    ;; (mov rdi ,(char->integer #\G))
    ;; (call (mem64+ *debug-put-u8))

    ;; Load the working versions of these values
    (mov ,%heap-rem (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)))
    (mov ,%alloc (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))

    ;; Swap the heaps.
    (mov rax (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-CURRENT-HEAP)))
    (mov rbx (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)))
    (mov rcx (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (mov rdx (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-CURRENT-HEAP)) rcx)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)) rdx)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-OTHER-HEAP)) rax)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)) rbx)

    ;; Count the number of GCs, so that it's possible to tell if
    ;; object have moved (used in hashtables and some other places).
    (add (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT))
         ,(immediate 1))

    ;; The scan pointer moves from the start of the new heap and
    ;; tries to catch up with the free pointer.
    (mov ,%scan (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-CURRENT-HEAP)))

    ;; Set the remaining heap space to be the top of the heap
    ;; minus the requested memory amount. Later on this is
    ;; adjusted to be exactly the amount of space remaining in
    ;; the new heap.
    (mov rdx (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (add rdx (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (sub rdx ,%alloc)               ;rdx = space remaining in old heap
    (add ,%heap-rem ,%scan)
    (add ,%heap-rem (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)))
    (sub ,%heap-rem rdx)

    ;; Allocate stuff at the start of the new heap.
    (mov ,%alloc ,%scan)

    ;; Relocate the processor vector.
    (mov rax ,%pcb)
    (call relocate)
    (mov ,%pcb rax)

    ;; Relocate all locals on the stack.
    (mov r9 rsp)
    (mov r10 (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:STACK-TOP)))
    (%label ,gc-stack)
    (mov rax (mem+ r9))
    (call relocate)                     ;relocate a local
    (mov (mem+ r9) rax)
    (add r9 8)
    (call gc-loop)
    (cmp r9 r10)
    (jb ,gc-stack)

    (call gc-loop)

    ;; Clear the old heap.
    ;; (mov rdi (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    ;; (mov rcx (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    ;; (sar rcx 3)                         ;quads
    ;; (xor eax eax)
    ;; (rep.stos (mem64+ rdi) rax)

    ;; If the heap margin was temporarily lifted, i.e. it was possible
    ;; to briefly allocate things in that memory, then %heap-rem
    ;; should have the margin deducted.
    (cmp (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-MARGIN-LIFTED?)) ,(immediate #t))
    (jne ,gc-margin)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-MARGIN-LIFTED?)) ,(immediate #f))
    (sub ,%heap-rem ,HEAP-MARGIN)
    (%label ,gc-margin)

    ;; Tell the program how much is left and check that there's enough
    ;; space in the new heap for the object that was being allocated.
    (sub ,%heap-rem ,%alloc)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-REMAINING)) ,%heap-rem)
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)) ,%alloc)
    (js ,gc-raise)

    (pop rbp)
    (pop rbx)
    (pop ,%closure)
    (ret)

    ;; The amount of free memory is less than HEAP-MARGIN.
    (%label ,gc-raise)
    (mov rdi ,%heap-rem)
    (sal rdi ,(shift 'fixnum))
    (mov ,%heap-rem (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-CURRENT-HEAP)))
    (add ,%heap-rem (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)))
    (sub ,%heap-rem (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:ALLOCATION-POINTER)))
    (mov (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:HEAP-MARGIN-LIFTED?)) ,(immediate #t))
    ;; Invoke heap-overflow as if the program invoked it directly.
    (pop rbp)
    (pop rbx)
    (pop ,%closure)
    (jmp heap-overflow)))

(define (lib-gc-relocate)
  (define relocate-copy '#(gc relocate copy))
  (define relocate-ret '#(gc relocate ret))
  `((%align 8)
    (%label relocate gc-loop)
    ;; rax is an object which will be relocated and returned
    ;; in rax. If it was found on the stack it might also be a
    ;; return address. No return address can be located inside
    ;; the stop-and-copy areas, so these addresses are never
    ;; dereferenced here.
    (mov ebx eax)
    (and ebx #b111)
    (dec ebx)                           ;rbx = tag minus one
    (cmp ebx 5)
    (ja ,relocate-ret)                  ;return if tag is outside the
                                        ;range #b001-#b110
    (mov rsi rax)
    (sub rsi (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (cmp rsi (mem64+ ,%pcb ,(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (ja ,relocate-ret)                  ;return if object outside heap
    (mov rsi rax)                       ;source = old object
    (and rsi ,(fxnot #b111))            ;unmask tag
    (cmp (mem16+ rsi) ,(tag 'move))     ;object already moved?
    (jne ,relocate-copy)
    (mov rax (mem64+ rsi))
    (sar rax ,(shift 'move))            ;return the new location
    (ret)
    (%label ,relocate-copy)             ;move the object
    (call gc-object-size)               ;rcx is aligned size of rax
    (mov rdi ,%alloc)                   ;copy into the free memory
    (rep.movs (mem64+ rdi) (mem64+ rsi))
    (mov rcx rax)
    (and rcx ,(fxnot #b111))
    (lea rax (mem+ ,%alloc rbx 1))      ;add back the tag
    (mov rbx rax)
    (sal rbx ,(shift 'move))
    (or rbx ,(tag 'move))
    (mov (mem64+ rcx) rbx)              ;store a forwarding pointer
    (mov ,%alloc rdi)
    (%label ,relocate-ret)
    (ret)))

(define (lib-gc-gc-loop)
  (define seek '#(gc loop seek))
  (define boxseek '#(gc loop boxseek))
  (define exit '#(gc loop exit))
  (define ignore '#(gc loop ignore))
  (define panic '#(gc loop panic))
  (assert (eqv? (bitwise-bit-field (mask 'seek-mark) 0 60) #xffff))
  (assert (eqv? (mask 'box-header) #xffff))
  (assert (eqv? (shift 'box-header:refs?) 31))
  (assert (eqv? (mask 'box-header:length) #xffffffff))
  `((%align 16)
    (%label gc-loop gc-object-size)
    ;; This routine treats the new heap as a queue. scan points to the
    ;; start of the queue. Its job is to relocate each cell in every
    ;; object which has already been relocated.
    (cmp ,%scan ,%alloc)
    (je ,exit)
    (mov rax (mem+ ,%scan))
    (cmp ax ,(bitwise-and (tag 'seek-mark) #xffffffff))
    (je ,seek)                          ;seek mark?
    ;; FIXME: Enable, after making these distinguishable from return addresses
    ;;(cmp ax ,(tag 'box-header))
    ;;(je ,boxseek)                       ;box header?
    (call relocate)
    (mov (mem+ ,%scan) rax)
    (add ,%scan 8)
    (jmp gc-loop)

    (%label ,seek)
    ;; The object under the scan pointer looks like a seek mark. It
    ;; could be a return address from a stack copied to the heap, so
    ;; seek marks are also tagged with high bits so that they are
    ;; non-canonical.
    (shl rax 1)
    (jnc ,ignore)
    (shr rax ,(+ (shift 'seek-mark) 1))

    (add ,%scan rax)                    ;seek over non-references
    (cmp ,%scan ,%alloc)
    (ja ,panic)                         ;scan > free?
    (jne gc-loop)

    (%label ,exit)
    (ret)

    ;; Box headers might have refs?=0, which lets the object contain
    ;; random bits.
    ;; FIXME: what about return addresses that look like this?                  move refs? bit
    (%label ,boxseek)
    (test eax eax)
    (js ,ignore)                        ;box-header:refs?
    (shr rax ,(shift 'box-header:length))
    (lea ,%scan (mem+ ,%scan (* 8 rax) 8))
    (cmp ,%scan ,%alloc)
    (ja ,panic)                         ;scan > free?
    (jmp gc-loop)

    (%label ,ignore)
    ;; It can't be a real seek mark, so ignore it.
    (add ,%scan 8)
    (jmp gc-loop)

    (%label ,panic)
    ;; The scan pointer is above the free pointer. Caused by an
    ;; invalid seek mark.
    (mov rdi ,(immediate 'bad-seek))
    (call (mem64+ *panic))))

;; These functions set rcx to the number of 8-byte words occupied by
;; the object in rax.

(define (lib-gc-object-sizer)
  (define high-tag '#(gc sizer high-tag))
  (define boxhdr '#(gc sizer boxhdr))
  (define bignum '#(gc sizer bignum))
  (define record '#(gc sizer record))
  (define fixup '#(gc sizer fixup))
  (define fixup-done '#(gc sizer fixup-done))
  ;; These are in the order of the type tags.
  (define sizers
    '#(box-sizer
       pair-sizer
       procedure-sizer
       string-sizer
       bytevector-sizer
       vector-sizer))
  `((%label gc-object-size box-sizer)
    (cmp ebx 3)
    (jge ,high-tag)
    (cmp ebx 1)
    (je ,(vector-ref sizers 1))
    (jl ,(vector-ref sizers 0))
    (jmp ,(vector-ref sizers 2))
    (%align 16)
    (%label ,high-tag)
    (cmp ebx 4)
    (je ,(vector-ref sizers 4))
    (jl ,(vector-ref sizers 3))
    (jmp ,(vector-ref sizers 5))

    (%align 8)
    (%label box-sizer pair-sizer)
    ;; Boxes are either built-in types (the first field is an
    ;; immediate symbol), in which case the second field is a length
    ;; field (except for bignums which have a fixed size), or record
    ;; types. Record types have an rtd (boxed value) as their first
    ;; field. The rtd contains a field that gives the number of fields
    ;; in the record itself. Most recently a new box header type was
    ;; created that contains an explicit length field, counted in
    ;; multiples of eight.
    (mov rcx (mem+ rax ,(fx- (tag 'box))))
    (cmp cx ,(tag 'box-header))
    (je ,boxhdr)                        ;box header

    (mov rcx (mem+ rax ,(fx- (tag 'box))))
    (and ecx ,(mask 'box))
    (cmp ecx ,(tag 'box))
    (je ,record)                       ;records have the length in rtd

    ;; TODO: Remove all need for this case. $box-type returns
    ;; something other then a box.
    (mov rcx (mem+ rax 8 ,(fx- (tag 'box))))
    (shr rcx ,(shift 'fixnum))
    (add rcx 2)                         ;+type+length
    (ret)

    (%label ,record)
    (mov rcx (mem+ rax ,(fx- (tag 'box)))) ;get rtd
    (mov rcx (mem64+ rcx 8 8 ,(fx- (tag 'box))))   ;rtd:record-size
    (shr rcx ,(shift 'fixnum))
    (add rcx 1)                           ;+type
    (ret)

    ;; Box header
    (%label ,boxhdr)
    (shr rcx ,(shift 'box-header:length))
    (add rcx 1)                           ;+header
    (ret)))

(define (lib-gc-pair-sizer)
  '((%align 8)
    (%label pair-sizer procedure-sizer)
    (mov ecx 2)
    (ret)))

(define (lib-gc-procedure-sizer)
  `((%align 8)
    (%label procedure-sizer string-sizer)
    ;; info contains the closure size
    (mov rcx (mem+ rax 8 ,(fx- (tag 'procedure)))) ;get "info"
    (mov rcx (mem+ rcx 16 ,(fx- (tag 'box))))      ;free vars
    (shr rcx ,(shift 'fixnum))
    (add rcx 2)                         ;label and info field
    (ret)))

(define (lib-gc-string-sizer)
  `((%align 8)
    (%label string-sizer bytevector-sizer)
    (mov rcx (mem+ rax ,(fx- (tag 'string))))
    (shr rcx ,(shift 'fixnum))          ;rcx = number of u32s
    (add rcx 1)                         ;even number of u32s
    ;; (and rcx -2)                     ;rcx = even number of u32s (not needed)
    (shr rcx 1)                         ;number of u64s
    (add rcx 1)                         ;+ length field
    (ret)))

(define (lib-gc-bytevector-sizer)
  `((%align 8)
    (%label bytevector-sizer vector-sizer)
    (mov rcx (mem+ rax ,(fx- (tag 'bytevector))))
    (shr rcx ,(shift 'fixnum))          ;rcx = number of u8s
    (add rcx 7)
    ;; (and rcx -8)                        ;rcx aligned to u64s (not needed)
    (shr rcx 3)                         ;number of u64s
    (add rcx 2)                         ;length field + seek mark
    (ret)))

(define (lib-gc-vector-sizer)
  `((%align 8)
    (%label vector-sizer invoke-trap)
    (mov rcx (mem+ rax ,(fx- (tag 'vector))))
    (shr rcx ,(shift 'fixnum))
    (add rcx 1)                         ;length field
    (ret)))

(define (lib-gc:text)
  `(,@(lib-gc-stop-and-copy)
    ,@(lib-gc-relocate)
    ,@(lib-gc-gc-loop)
    ,@(lib-gc-object-sizer)
    ,@(lib-gc-pair-sizer)
    ,@(lib-gc-procedure-sizer)
    ,@(lib-gc-string-sizer)
    ,@(lib-gc-bytevector-sizer)
    ,@(lib-gc-vector-sizer)))

(define (lib-gc:data)
  '()))
