;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; Initialization helpers common to several amd64 targets

(library (loko arch amd64 common-init)
  (export
    amd64-init-memory-map)
  (import
    (rnrs (6))
    (loko runtime mmap)
    (loko arch amd64 memory)
    (loko system $primitives))

;; Add all memory addresses that are part of the initial process
;; image.
(define (amd64-init-memory-map memory-map)
  ;; .text
  (mmap-mark! memory-map ($linker-address 'image-address-zero)
              (- ($linker-address 'text-end) ($linker-address 'image-address-zero))
              (fxior prot-read prot-exec)
              'text #f)
  ;; .data
  (mmap-mark! memory-map ($linker-address 'data)
              (- ($linker-address 'data-end) ($linker-address 'data))
              (fxior prot-read)
              'data #f)
  ;; .bss
  (mmap-mark! memory-map ($linker-address 'bss)
              (- ($linker-address 'bss-end) ($linker-address 'bss))
              (fxior prot-read prot-write)
              'bss #f)
  ;; stack 0
  (mmap-mark! memory-map (+ STACK-0-START PAGE-SIZE) (- STACK-0-SIZE PAGE-SIZE)
              (fxior prot-read prot-write)
              'stack-0 #f)
  ;; heap 0
  (mmap-mark! memory-map HEAP-0-START HEAP-0-SIZE
              (fxior prot-read prot-write)
              'heap-0 #f)))
