;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Kernel-independent amd64 debugging printer.

(library (loko arch amd64 lib-printer)
  (export
    lib-printer:text lib-printer:data)
  (import
    (loko arch amd64 objects)
    (rnrs (6)))

;; Register r11 in this file is 0 for display and 1 for write.

;; This file starts with a type dispatch, which might be useful in
;; more places.

(define (lib-printer:text)
  (define (mem-car base)
    `(mem64+ ,base ,(- (car-offset) (tag 'pair))))
  (define (mem-cdr base)
    `(mem64+ ,base ,(- (cdr-offset) (tag 'pair))))
  (define (mem-vector-ref base index . x)
    `(mem64+ ,base 8 ,(- (tag 'vector)) ,index ,@x))
  (define (mem-vector-length base)
    `(mem64+ ,base ,(- (tag 'vector))))
  (define start 'debug-writer)
  (define positive '#(printer positive))
  (define boolean '#(printer boolean))
  (define char '#(printer char))
  (define fixnum '#(printer fixnum))
  (define immsym '#(printer immsym))
  (define pair '#(printer pair))
  (define vector* '#(printer vector))
  (define nopar '#(printer nopar))
  (define null '#(printer null))
  (define end '#(printer end))
  `((%label debug-display debug-write)
    (xor r11d r11d)
    (jmp ,start)
    (%label debug-write ,start)
    (mov r11d 1)
    (jmp ,start)

    (%label ,start ,end)
    ;; This will display an object on the architecture's debug port.
    ;; rdi contains the object and r11 indicates whether to use
    ;; display or write syntax.
    (push rdi)                        ;save the object
    (mov eax edi)
    (and eax ,(mask 'immsym))
    (cmp eax ,(tag 'immsym))
    (je ,immsym)

    (and eax ,(mask 'fixnum))         ;same for all of fixnum--vector
    (jz ,fixnum)
    (jnp ,nopar)
    (cmp eax ,(tag 'vector)) (je ,vector*)
    ;; (cmp eax ,(tag 'procedure)) (je ,procedure)
    ;; (cmp eax ,(tag 'bytevector)) (je ,bytevector)
    (%label ,nopar)
    (cmp eax ,(tag 'pair)) (je ,pair)
    ;; (cmp eax ,(tag 'box)) (je ,box)
    ;; (cmp eax ,(tag 'string)) (je ,string)

    (cmp dil ,(tag 'char)) (je ,char)
    (cmp dil ,(tag 'boolean)) (je ,boolean)
    (cmp rdi ,(tag 'null)) (je ,null)

    (mov edi ,(char->integer #\#))
    (call (mem64+ *debug-put-u8))
    (mov edi ,(char->integer #\<))
    (call (mem64+ *debug-put-u8))
    (mov edi ,(char->integer #\?))
    (call (mem64+ *debug-put-u8))
    (mov edi ,(char->integer #\>))
    (call (mem64+ *debug-put-u8))
    (jmp ,end)

    ;;; Boolean
    (%label ,boolean)
    (mov edi ,(char->integer #\#))
    (call (mem64+ *debug-put-u8))
    (mov rdi (mem64+ rsp))
    (sar rdi ,(shift 'boolean))
    (mov edi ,(char->integer #\t))
    (mov rax ,(char->integer #\f))
    (cmovz rdi rax)
    (call (mem64+ *debug-put-u8))
    (jmp ,end)

    ;;; Chars (utf-8 is not done)
    (%label ,char)
    ,@(let ((l '#(printer char loop)))
        `((test r11d r11d)
          (jz ,l)
          (mov edi ,(char->integer #\#))
          (call (mem64+ *debug-put-u8))
          (mov edi ,(char->integer #\\))
          (call (mem64+ *debug-put-u8))
          (%label ,l)
          (mov rdi (mem64+ rsp))
          (shr rdi ,(shift 'char))
          (call (mem64+ *debug-put-u8))
          (jmp ,end)))

    ;;; Fixmnums
    (%label ,fixnum)
    (test rdi rdi)
    (sar rdi ,(shift 'fixnum))
    (jns ,positive)
    ;; handle negative numbers
    (mov edi ,(char->integer #\-))
    (call (mem64+ *debug-put-u8))
    (mov rdi (mem+ rsp))
    (sar rdi ,(shift 'fixnum))
    (neg rdi)
    (%label ,positive)
    (mov rax rdi)                       ;rax = positive number
    (xor r10d r10d)
    (mov rcx 1000000000000000000)
    ,@(let ((loop '#(printer fixnum loop))
            (skip '#(printer fixnum skip))
            (end  '#(printer fixnum end)))
        `((%label ,loop)
          (xor edx edx)
          (div rcx)                     ;rdx = (rdx:rax)/rcx, rax = (rdx:rax)%rcx
          (or r10 rax)                  ;r10 = bitwise or of all digits
          (push rdx)                    ;save the rest of the digits
          (test r10 r10)
          (jz ,skip)                    ;skip leading zeroes
          (push rcx)
          (lea rdi (mem+ rax ,(char->integer #\0)))
          (call (mem64+ *debug-put-u8)) ;print the digit
          (pop rcx)
          (%label ,skip)

          (mov r11d 10)
          (xor edx edx)
          (mov rax rcx)
          (div r11)
          (mov rcx rax)                 ;rcx = rcx / 10
          (pop rax)                     ;rax = rest of digits

          (cmp rcx 1)
          (jne ,loop)))                 ;loop until the last digit

    (lea rdi (mem+ rax ,(char->integer #\0)))
    (call (mem64+ *debug-put-u8))
    (jmp ,end)

    ;;; Immediate symbols
    (%label ,immsym)
    ,@(let ((loop '#(printer immsym loop)))
        `((mov rax (mem64+ rsp))
          (shr rax ,(shift 'immsym))  ;rax:immsym
          (mov (mem64+ rsp) rax)      ;save untagged symbol
          (%label ,loop)
          (mov rax (mem64+ rsp))
          (mov rsi rax)
          (and rsi #b11111)           ;rdi:next char
          (jz ,end)
          (shr rax 5)                 ;rax:rest of symbol
          (mov (mem64+ rsp) rax)      ;save rest of the symbol
          (mov rdi ,alphs)            ;alphabet
          (mov rax ,alphe)            ;end-alphabet
          (cmovz rdi rax)             ;set! rdi (if (eq? rest 0) end-alphabet alphabet)
          (movzx rdi (mem8+ rdi rsi -1))
          (call (mem64+ *debug-put-u8))
          (jmp ,loop)))

    ;;; Null
    (%label ,null)
    (mov edi ,(char->integer #\())
    (call (mem64+ *debug-put-u8))
    (mov edi ,(char->integer #\)))
    (call (mem64+ *debug-put-u8))
    (jmp ,end)

    ;;; Pairs
    (%label ,pair)
    ,@(let ((loop '#(printer pair loop))
            (pair '#(printer pair pair))
            (pend '#(printer pair end)))
        `((mov edi ,(char->integer #\())
          (call (mem64+ *debug-put-u8))
          (%label ,loop)
          (mov rdi (mem64+ rsp))
          (cmp rdi ,(immediate '()))
          (je ,pend)
          (mov rdi ,(mem-car 'rdi))
          (call ,start)               ;display (car rdi)
          (mov rdi (mem64+ rsp))
          (mov rdi ,(mem-cdr 'rdi))   ;set! rdi (cdr rdi)
          (mov (mem64+ rsp) rdi)
          (cmp rdi ,(immediate '()))  ;null? rdi
          (je ,loop)
          (mov rsi rdi)
          (and rsi ,(mask 'pair))
          (cmp rsi ,(tag 'pair))      ;pair? rdi
          (je ,pair)
          (mov edi ,(char->integer #\space))
          (call (mem64+ *debug-put-u8))
          (mov edi ,(char->integer #\.))
          (call (mem64+ *debug-put-u8))
          (mov edi ,(char->integer #\space))
          (call (mem64+ *debug-put-u8))
          (mov rdi (mem64+ rsp))
          (call ,start)
          (jmp ,pend)
          (%label ,pair)
          (mov edi ,(char->integer #\space))
          (call (mem64+ *debug-put-u8))
          (jmp ,loop)
          (%label ,pend)
          (mov edi ,(char->integer #\)))
          (call (mem64+ *debug-put-u8))
          (jmp ,end)))

    ;;; Vectors
    ,@(let ((loop       '#(printer vector loop))
            (vector-end '#(printer vector end)))
        `((%label ,vector*)
          (mov edi ,(char->integer #\#))
          (call (mem64+ *debug-put-u8))
          (mov edi ,(char->integer #\())
          (call (mem64+ *debug-put-u8))
          (xor rcx rcx)
          (mov rdi (mem64+ rsp))
          (cmp rcx ,(mem-vector-length 'rdi))
          (jae ,vector-end)
          (%label ,loop)
          (mov rdi (mem64+ rsp))
          (mov rdi ,(mem-vector-ref 'rdi 'rcx))
          (push rcx)
          (call ,start)
          (pop rcx)
          (add rcx ,(immediate 1))
          (mov rdi (mem64+ rsp))
          (cmp rcx ,(mem-vector-length 'rdi))
          (je ,vector-end)
          (mov edi ,(char->integer #\space))
          (call (mem64+ *debug-put-u8))
          (jmp ,loop)
          (%label ,vector-end)
          (mov edi ,(char->integer #\)))
          (call (mem64+ *debug-put-u8))

          (jmp ,end)))

    (%label ,end)
    (pop rdi)
    (ret)))

(define alphs '#(printer alphabet-start))
(define alphe '#(printer alphabet-end))

(define (lib-printer:data)
  `((%label ,alphs)
    (%utf8z "abcdefghijklmnopqrstuvwxyz-/<=>")
    (%label ,alphe)
    (%utf8z "acdefghklmnopqrstvxy!*+-/08<=>?"))))
