;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Register the target for Loko on Linux on AMD64

(library (loko arch amd64 linux-asm)
  (export visit)
  (import
    (rnrs (6))
    (loko arch asm)
    (prefix (loko arch amd64 elf-start) amd64-elf-start:)
    (prefix (loko arch amd64 linux-start) amd64-linux-start:)
    (prefix (loko arch amd64 lib) amd64:)
    (prefix (loko arch amd64 codegen) amd64:)
    (prefix (loko arch amd64 analyzer) amd64:)
    (prefix (loko arch amd64 tables) amd64:))

(define (visit) #f)

(define (linux-assembler-library target-cpu target-kernel notice text data)
  (append '((%origin #x200000)
            (%label image-address-zero))
          (amd64-elf-start:image-header)
          notice
          (amd64:text-start)
          (amd64-elf-start:text-start)
          (amd64:text)
          (amd64-elf-start:text)
          (amd64-linux-start:text)
          text
          (amd64:text-end)
          (amd64:data-start)
          (amd64:data)
          (amd64-elf-start:data)
          (amd64-linux-start:data)
          data
          (amd64:data-end)
          (amd64-elf-start:image-footer)))

(register-target 'amd64 'linux
                 amd64:assemble
                 amd64:codegen
                 amd64:instruction-analyzer
                 amd64:target-convention
                 amd64:table-generator
                 linux-assembler-library))
