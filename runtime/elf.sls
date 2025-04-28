;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020, 2021 G. Weinholt
#!r6rs

;;; ELF helpers

(library (loko runtime elf)
  (export
    elf-parse-stack-data)
  (import
    (rnrs (6))
    (only (loko system unsafe) get-mem-s61 get-mem-u8)
    (only (loko runtime utils) string-index))

;; Handle data placed on the stack by execve. The initial stack
;; pointer set by the kernel is passed as stk-loc.
(define (elf-parse-stack-data stk-loc)
  (define command-line)
  (define environment-variables)
  (define auxiliary-vector)
  (define AT_EXECPATH 15)               ;FreeBSD (conflicts with AT_PLATFORM)
  (define AT_EXECFN 31)                 ;Linux
  (define AT_SUN_EXECNAME 2014)         ;NetBSD
  (define AT_RANDOM 25)
  (define utf8z-types '(15 24 31 2014))
  (define field-argc 0)
  (define field-argv 1)
  (define stk-maxaddr stk-loc)
  (define (get-random addr)
    (do ((ret (make-bytevector 16))
         (i 0 (fx+ i 1)))
        ((fx=? i 16)
         (set! stk-maxaddr (fxmax stk-maxaddr (fx+ addr (fx- i 1))))
         ret)
      (bytevector-u8-set! ret i (get-mem-u8 (fx+ addr i)))))
  (define (copy-utf8z addr)
    (do ((end addr (fx+ end 1))
         (len 0 (fx+ len 1)))
        ((fxzero? (get-mem-u8 end))
         (set! stk-maxaddr (fxmax stk-maxaddr end))
         (do ((ret (make-bytevector len))
              (i (fx- end 1) (fx- i 1))
              (len (fx- len 1) (fx- len 1)))
             ((fx<=? len -1) ret)
           (bytevector-u8-set! ret len (get-mem-u8 i))))))
  (define (stk-ref i)
    (set! stk-maxaddr (fxmax stk-maxaddr (fx+ (fx+ stk-loc (fx* i 8)) 7)))
    (get-mem-s61 (fx+ stk-loc (fx* i 8))))
  (let ((argc (stk-ref field-argc)))
    ;; Parse command line
    (do ((i (fx- argc 1) (fx- i 1))
         (cmdline '() (cons (utf8->string
                             (copy-utf8z (stk-ref (fx+ field-argv i))))
                            cmdline)))
        ((fx=? i -1)
         (set! command-line cmdline)))
    ;; Parse environment
    (do ((envp (fx+ argc 2) (fx+ envp 1))
         (env '() (cons (let ((str (utf8->string (copy-utf8z (stk-ref envp)))))
                          (let ((idx (string-index str #\=)))
                            (if (not idx)
                                (cons str "") ;can happen
                                (cons (substring str 0 idx)
                                      (substring str (fx+ idx 1) (string-length str))))))
                        env)))
        ((fxzero? (stk-ref envp))
         (set! environment-variables env)
         ;; Parse ELF auxiliary vector. XXX: AT_RANDOM is interesting.
         ;; Maybe the UID/GID stuff, too. AT_SYSINFO* points to the
         ;; syscall page!
         (do ((auxp (fx+ envp 1) (fx+ auxp 2))
              (auxv '()
                    (let ((type (stk-ref auxp))
                          (value (stk-ref (fx+ auxp 1))))
                      (cons (cons type
                                  (cond ((memq type utf8z-types)
                                         (copy-utf8z value))
                                        ((eqv? type AT_RANDOM)
                                         (get-random value))
                                        (else value))
                                  value)
                            auxv))))
             ((fxzero? (stk-ref auxp))
              (set! auxiliary-vector auxv)
              ;; (command-line) must not return empty, so use the
              ;; executable's filename if the parent process didn't
              ;; supply any arguments.
              (cond
                ((and (eqv? argc 0)
                      (or (assv AT_EXECFN auxv) (assv AT_SUN_EXECNAME auxv) (assv AT_EXECPATH auxv)))
                 => (lambda (execfn)
                      (set! command-line
                            (list (utf8->string (cdr execfn)))))))))))
    (values command-line environment-variables auxiliary-vector stk-maxaddr))))
