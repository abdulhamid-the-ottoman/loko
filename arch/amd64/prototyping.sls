;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Stuff for prototyping on the amd64 port (in the REPL).

;; This is maybe temporary until the compiler is up and running.

(library (loko arch amd64 prototyping)
  (export
    cpuid rdtsc rdrand rdseed

    $processor-data-ref $processor-data-set!
    $object->fixnum

    $box?
    $make-box
    $make-box-header
    $box-ref
    $box-set!
    $box-type
    $box-type-set!
    $box-header-type-eq?
    $box-header-value
    $box-header-length

    $procedure-ref
    $procedure-length)
  (import
    (rnrs)
    (prefix (loko system $x86) sys:)
    (prefix (loko system $primitives) sys:)
    (prefix (loko system $host) sys:))

(define cpuid
  (case-lambda
    ((eax)
     (cpuid eax 0))
    ((eax ecx)
     (sys:cpuid eax ecx))))

(define (rdtsc)
  (sys:rdtsc))

(define (rdrand)
  (sys:rdrand))

(define (rdseed)
  (sys:rdseed))

(define ($processor-data-ref idx)
  (sys:$processor-data-ref idx))

(define ($processor-data-set! idx v)
  (sys:$processor-data-set! idx v))

(define ($object->fixnum x)
  (sys:$object->fixnum x))

;; The boxes!! Should probably not be here either.
(define ($box? x) (sys:$box? x))

(define ($make-box type len) (sys:$make-box type len))

(define ($make-box-header type refs? value length)
  (case type
    ((bignum) (sys:$make-box-header 'bignum refs? value length))
    ((ratnum) (sys:$make-box-header 'ratnum refs? value length))
    ((symbol) (sys:$make-box-header 'symbol refs? value length))
    ((port) (sys:$make-box-header 'port refs? value length))
    ((rtd) (sys:$make-box-header 'rtd refs? value length))
    (else
     (error '$make-box-header "Not implemented" type refs? value length))))

(define ($box-ref v i) (sys:$box-ref v i))

(define ($box-set! v i x) (sys:$box-set! v i x))

(define ($box-type x) (sys:$box-type x))

(define ($box-type-set! x t) (sys:$box-type-set! x t))

(define ($box-header-value x) (assert (sys:$box-header? x)) (sys:$box-header-value x))

(define ($box-header-length x) (assert (sys:$box-header? x)) (sys:$box-header-length x))

(define $box-header-type-eq?
  (case-lambda
    ((obj type)
     (case type
       ((port) (sys:$box-header-type-eq? obj 'port))
       ((rtd) (sys:$box-header-type-eq? obj 'rtd))
       (else
        (error '$box-header-type-eq? "Not implemented" obj type))))
    ((obj type mask test)
     (and ($box-header-type-eq? obj type)
          (let ((t ($box-header-value obj)))
            (fx=? test (fxand t mask)))))))

(define ($procedure-ref proc idx)
  (sys:$procedure-ref proc idx))

(define ($procedure-length proc)
  ($box-ref (sys:$procedure-info proc) 1)))
