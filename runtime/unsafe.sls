;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Unsafe procedures, an escape hatch

(library (loko runtime unsafe)
  (export
    syscall
    get-i/o-u8 get-i/o-u16 get-i/o-u32
    put-i/o-u8 put-i/o-u16 put-i/o-u32
    get-i/o-u8-n! get-i/o-u16-n! get-i/o-u32-n!
    get-mem-u8 get-mem-u16 get-mem-u32 get-mem-s61
    put-mem-u8 put-mem-u16 put-mem-u32 put-mem-s61
    memory-fence load-fence store-fence)
  (import
    (only (rnrs) define case-lambda assert fx<=? not fxnegative?)
    (prefix (loko system unsafe) sys:)
    (prefix (loko system unsafe cache) sys:))

;; Btw: doing syscalls directly from the REPL stands a good chance of
;; not working every now and then if the GC runs and moves your
;; bytevectors around.
(define syscall
  (case-lambda
    ((n) (sys:syscall n))
    ((n a) (sys:syscall n a))
    ((n a b) (sys:syscall n a b))
    ((n a b c) (sys:syscall n a b c))
    ((n a b c d) (sys:syscall n a b c d))
    ((n a b c d e) (sys:syscall n a b c d e))
    ((n a b c d e f) (sys:syscall n a b c d e f))))

(define (get-i/o-u8 port)
  (assert (fx<=? 0 port #xffff))
  (sys:get-i/o-u8 port))

(define (put-i/o-u8 port v)
  (assert (fx<=? 0 port #xffff))
  (assert (fx<=? 0 v #xff))
  (sys:put-i/o-u8 port v))

(define (get-i/o-u16 port)
  (assert (fx<=? 0 port #xffff))
  (sys:get-i/o-u16 port))

(define (put-i/o-u16 port v)
  (assert (fx<=? 0 port #xffff))
  (assert (fx<=? 0 v #xffff))
  (sys:put-i/o-u16 port v))

(define (get-i/o-u32 port)
  (assert (fx<=? 0 port #xffff))
  (sys:get-i/o-u32 port))

(define (put-i/o-u32 port v)
  (assert (fx<=? 0 port #xffff))
  (assert (fx<=? 0 v #xffffffff))
  (sys:put-i/o-u32 port v))

(define (get-i/o-u8-n! port address count)
  (assert (fx<=? 0 port #xffff))
  (assert (not (fxnegative? count)))
  (sys:get-i/o-u8-n! port address count))

(define (get-i/o-u16-n! port address count)
  (assert (fx<=? 0 port #xffff))
  (assert (not (fxnegative? count)))
  (sys:get-i/o-u16-n! port address count))

(define (get-i/o-u32-n! port address count)
  (assert (fx<=? 0 port #xffff))
  (assert (not (fxnegative? count)))
  (sys:get-i/o-u32-n! port address count))

(define (get-mem-u8 addr)
  (sys:get-mem-u8 addr))

(define (get-mem-u16 addr)
  (sys:get-mem-u16 addr))

(define (get-mem-u32 addr)
  (sys:get-mem-u32 addr))

(define (get-mem-s61 addr)
  (sys:get-mem-s61 addr))

(define (put-mem-u8 addr v)
  (sys:put-mem-u8 addr v))

(define (put-mem-u16 addr v)
  (sys:put-mem-u16 addr v))

(define (put-mem-u32 addr v)
  (sys:put-mem-u32 addr v))

(define (put-mem-s61 addr v)
  (sys:put-mem-s61 addr v))

(define (memory-fence)
  (sys:memory-fence))

(define (load-fence)
  (sys:load-fence))

(define (store-fence)
  (sys:store-fence)))
