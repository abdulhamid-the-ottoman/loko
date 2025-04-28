;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; Main function for pid 1, which is the first process started by the
;;; scheduler (pid 0). In SRFI-18 terms, this is the primordial
;;; thread.

(library (loko runtime main)
  (export
    load-program
    main)
  (import
    (rnrs (6))
    (rnrs mutable-strings (6))
    (only (loko) compile-program print-dialects load
          interaction-environment)
    (only (loko compiler cp0) cp0-size-limit cp0-effort-limit)
    (loko config)
    (only (loko runtime init) init-set! set-file-mode)
    (only (loko runtime repl) banner repl)
    (only (psyntax expander) compile-r6rs-top-level new-interaction-environment)
    (only (psyntax compat) top-level-language)
    (only (loko runtime reader) read-annotated)
    (loko runtime fibers)
    (loko match))

(define (skip-shebang p)
  (let* ((pos0 (port-position p))
         (shebang (get-string-n p 3)))
    (if (and (string? shebang)
             (or (string=? shebang "#!/")
                 (string=? shebang "#! ")))
        (begin (get-line p) #f)
        (set-port-position! p pos0))))

(define (read-code fn)
  (call-with-input-file fn
    (lambda (p)
      (skip-shebang p)
      (let lp ((codes '()))
        (let ((datum (read-annotated p fn)))
          (if (eof-object? datum)
              (reverse codes)
              (lp (cons datum codes))))))))

(define (load-program fn)
  (let* ((top-level (read-code fn))
         (proc (compile-r6rs-top-level top-level)))
    (proc)
    (if #f #f)))

(define (string-suffix? sfx str)
  (let ((strlen (string-length str))
        (sfxlen (string-length sfx)))
    (and (fx>=? strlen sfxlen)
         (string=? sfx (substring str (fx- strlen sfxlen) strlen)))))

(define (string-prefix? pfx str)
  (let ((strlen (string-length str))
        (pfxlen (string-length pfx)))
    (and (fx>=? strlen pfxlen)
         (string=? pfx (substring str 0 pfxlen)))))

;; Takes a filename and returns the default executable filename.
;; "foo/bar.sps" => "foo/bar", "foo/bar" => "foo/bar.exe"
(define (executable-name sps-fn)
  (cond ((memv #\. (reverse (string->list sps-fn)))
         => (lambda (c*)
              (list->string (reverse (cdr c*)))))
        (else
         (string-append sps-fn ".exec"))))

(define (main)
  ;; Sanity check
  (guard (exn (else #f))
    (let ((msg "warning: Read-only data is not being protected from writes.\n"))
      (string-set! msg 0 #\W)
      (display msg (current-error-port))))

  ;; Parse the command line. TODO: Get a real command line parser
  ;; (maybe srfi-37)
  (let loop-args ((args (command-line))
                  (options '()))
    (match args
      [((? (lambda (x) (string-suffix? "scheme-script" x)) exec-name) . rest)
       (match rest
         [(fn . rest)
          (init-set! 'command-line (cons fn rest))
          (load-program fn)
          (flush-output-port (current-output-port))
          (exit 0)]
         [()
          (display "Fatal: the Loko scheme-script program expects a filename.\n"
                   (current-error-port))
          (exit 1)])]
      [(exec-name "--program" fn . rest)
       (init-set! 'command-line (cons fn rest))
       (load-program fn)
       (flush-output-port (current-output-port))
       (exit 0)]
      [(exec-name "--script" fn . rest)
       (init-set! 'command-line (cons fn rest))
       (interaction-environment (new-interaction-environment))
       (load fn (interaction-environment))
       (flush-output-port (current-output-port))
       (exit 0)]

      ;; Compiler options
      [(exec-name (? (lambda (x) (string-prefix? "-ftarget=" x)) target) . rest)
       (let ((targetsym (string->symbol
                         (substring target (string-length "-ftarget=")
                                    (string-length target)))))
         (config-target-kernel targetsym))
       (loop-args (cons exec-name rest) options)]
      [(exec-name (? (lambda (x) (string-prefix? "-fcp0-size-limit=" x)) target) . rest)
       (let ((n (string->number (substring target (string-length "-fcp0-size-limit=")
                                           (string-length target)))))
         (cp0-size-limit n))
       (loop-args (cons exec-name rest) options)]
      [(exec-name (? (lambda (x) (string-prefix? "-fcp0-effort-limit=" x)) target) . rest)
       (let ((n (string->number (substring target (string-length "-fcp0-effort-limit=")
                                           (string-length target)))))
         (cp0-effort-limit n))
       (loop-args (cons exec-name rest) options)]
      [(exec-name "-fcoverage=afl++" . rest)
       (loop-args (cons exec-name rest) (cons '(coverage . afl++) options))]
      [(exec-name "-feval" . rest)
       (loop-args (cons exec-name rest) (append '(eval use-primlocs) options))]
      [(exec-name "-ffreestanding" . rest)
       (loop-args (cons exec-name rest) (append '(freestanding) options))]
      [(exec-name (? (lambda (x) (string-prefix? "-std=" x)) std) . rest)
       (let ((std (string->symbol (substring std (string-length "-std=") (string-length std)))))
         (unless (memq std '(r6rs r7rs))
           (display "Unknown language standard: " (current-error-port))
           (write std (current-error-port))
           (newline (current-error-port))
           (exit 1))
         (top-level-language std)
         (loop-args (cons exec-name rest) options))]

      [(exec-name "--verbose" . rest)
       (loop-args (cons exec-name rest) (append '(verbose) options))]
      [(exec-name "--compile" sps-fn "--output" out-fn)
       (compile-program out-fn sps-fn options)
       (set-file-mode out-fn #o755)]
      [(exec-name "--compile" sps-fn)
       (let ((out-fn (executable-name sps-fn)))
         (assert (not (equal? sps-fn out-fn)))
         (compile-program out-fn sps-fn options)
         (set-file-mode out-fn #o755))]
      [(exec-name)
       (banner (current-output-port))
       (repl)
       (flush-output-port (current-output-port))
       ;; All polite Schemes say good bye
       (display (if (eq? 'r7rs (top-level-language))
                    "Upanīyati loko. Addhuvo.\n"
                    "Sabbaṁ pahāya gamanīyaṁ.\n"))
       (exit 0)]
      [args
       (display "Fatal: unrecognized Loko command line:\n" (current-error-port))
       (write args (current-error-port))
       (newline (current-error-port))
       (exit 1)]))))
