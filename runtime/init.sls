;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Initialization calls and procedures for the execution environment.

;; The linux-init, pc-init and process-init libraries call init-set!.

(library (loko runtime init)
  (export
    command-line
    get-environment-variables get-environment-variable
    putenv
    exit
    emergency-exit
    file-exists? delete-file
    open-file-input-port
    open-file-output-port
    open-file-input/output-port
    open-i/o-poller
    $mmap
    machine-type
    get-random-seed
    elf-auxiliary-vector

    set-file-mode
    directory-files

    init
    init-set!
    install-vfs make-vfs

    features)
  (import
    (except (rnrs) command-line exit
            file-exists? delete-file
            open-file-input-port
            open-file-output-port
            open-file-input/output-port)
    (rnrs mutable-pairs))

(define *command-line* '("loko"))
(define (command-line)
  *command-line*)

;; SRFI-98
(define *environment-variables* '())
(define (get-environment-variables)
  *environment-variables*)
(define (get-environment-variable name)
  (cond ((assoc name *environment-variables*) => cdr)
        (else #f)))

;; Putenv
(define (putenv name value)
  (assert (string? name))
  (assert (or (string? value) (not value)))
  (cond
    ((not value)
     (set! *environment-variables*
           (remp (lambda (x) (equal? name (car x))) *environment-variables*)))
    ((assoc name *environment-variables*) =>
     (lambda (pair)
       (set-cdr! pair value)))
    (else
     (set! *environment-variables*
           (cons (cons name value) *environment-variables*)))))

;; ELF auxiliary vector, auxv
(define *auxiliary-vector* '())
(define (elf-auxiliary-vector)
  *auxiliary-vector*)

(define *exit*
  (lambda status
    (apply error 'exit "No exit procedure has been installed" status)))

(define exit
  (case-lambda
    (()
     (guard (exn (else #f))
       (flush-output-port (current-output-port)))
     ;; FIXME: run winders, in all fibers...
     (*exit* #t))
    ((status)
     (guard (exn (else #f))
       (flush-output-port (current-output-port)))
     ;; FIXME: run winders, in all fibers...
     (*exit* status))))

(define *emergency-exit*
  (lambda status
    (apply error 'emergency-exit
           "No emergency-exit procedure has been installed" status)))

(define emergency-exit
  (case-lambda
    (()
     (*emergency-exit* #t))
    ((status)
     (*emergency-exit* status))))

(define *open-i/o-poller*
  (lambda x
    (apply error 'open-i/o-poller
           "No open-i/o-poller has been installed" x)))

(define (open-i/o-poller)
  (*open-i/o-poller*))

(define *mmap* (lambda _ (error 'mmap "No mmap procedure installed")))

(define ($mmap start len type)
  (*mmap* start len type))

(define *init*
  (lambda _ (error 'start "No init procedure installed")))
(define (init stage)
  (*init* stage))

(define *machine-type* '#(unknown unknown))
(define (machine-type)
  *machine-type*)

;; Returns a bytevector with n bytes usable as a random seed.
(define *get-random-seed*
  (lambda (n)
    (error 'get-random-seed "No get-random-seed! procedure installed" n)))
(define (get-random-seed n)
  (*get-random-seed* n))

(define (init-set! what value)
  (case what
    (($mmap) (set! *mmap* value))
    ((auxiliary-vector) (set! *auxiliary-vector* value))
    ((command-line) (set! *command-line* value))
    ((environment-variables) (set! *environment-variables* value))
    ((exit) (set! *exit* value))
    ((emergency-exit) (set! *emergency-exit* value))
    ((init) (set! *init* value))
    ((machine-type) (set! *machine-type* value))
    ((get-random-seed) (set! *get-random-seed* value))
    ((open-i/o-poller) (set! *open-i/o-poller* value))
    (else
     (error 'init-set! "Unrecognized key" what value))))

;;; Virtual filesystem abstraction

;; The idea here is to make all file system operations switchable at
;; runtime. Open files should not be affected. It can be used to
;; implement a file system on targets that don't have a natural file
;; system.

(define-record-type vfs
  (fields delete-file
          file-exists?
          open-file
          set-file-mode
          all-ops))

(define *vfs* #f)

(define (delete-file filename)
  (let ((proc (and (vfs? *vfs*) (vfs-delete-file *vfs*))))
    (unless (procedure? proc)
      (raise
        (condition
         (make-i/o-error)
         (make-who-condition 'delete-file)
         (make-message-condition "No delete-file procedure has been installed")
         (make-irritants-condition filename))))
    (proc filename)))

(define (file-exists? filename)
  (let ((proc (and (vfs? *vfs*) (vfs-file-exists? *vfs*))))
    (and (procedure? proc)
         (proc filename))))

(define (open-file filename file-options buffer-mode who)
  (let ((proc (and (vfs? *vfs*) (vfs-open-file *vfs*))))
    (unless (procedure? proc)
      (raise
        (condition
         (make-i/o-filename-error filename)
         (make-who-condition who)
         (make-message-condition "No file opener has been installed")
         (make-irritants-condition (list filename file-options buffer-mode)))))
    (proc filename file-options buffer-mode who)))

(define open-file-input-port
  (case-lambda
    ((fn)
     (open-file-input-port fn (file-options)))
    ((fn fo)
     (open-file-input-port fn fo (buffer-mode block)))
    ((fn fo bm)
     (open-file-input-port fn fo bm #f))
    ((filename file-options buffer-mode maybe-transcoder)
     (define who 'open-file-input-port)
     (assert (buffer-mode? buffer-mode))
     (let ((p (open-file filename file-options buffer-mode who)))
       (if maybe-transcoder
           (transcoded-port p maybe-transcoder)
           p)))))

(define open-file-output-port
  (case-lambda
    ((fn)
     (open-file-output-port fn (file-options)))
    ((fn fo)
     (open-file-output-port fn fo (buffer-mode block)))
    ((fn fo bm)
     (open-file-output-port fn fo bm #f))
    ((filename file-options buffer-mode maybe-transcoder)
     (define who 'open-file-output-port)
     (assert (buffer-mode? buffer-mode))
     (let ((p (open-file filename file-options buffer-mode who)))
       (if maybe-transcoder
           (transcoded-port p maybe-transcoder)
           p)))))

(define open-file-input/output-port
  (case-lambda
    ((fn)
     (open-file-input/output-port fn (file-options)))
    ((fn fo)
     (open-file-input/output-port fn fo (buffer-mode block)))
    ((fn fo bm)
     (open-file-input/output-port fn fo bm #f))
    ((filename file-options buffer-mode maybe-transcoder)
     (define who 'open-file-input/output-port)
     (assert (buffer-mode? buffer-mode))
     (let ((p (open-file filename file-options buffer-mode who)))
       (if maybe-transcoder
           (transcoded-port p maybe-transcoder)
           p)))))

;; chmod
(define (set-file-mode filename mask)
  (let ((proc (and (vfs? *vfs*) (vfs-set-file-mode *vfs*))))
    (unless (procedure? proc)
      (raise
        (condition
         (make-i/o-filename-error filename)
         (make-who-condition 'set-file-mode)
         (make-message-condition "No procedure has been installed")
         (make-irritants-condition (list filename mask)))))
    (proc filename mask)))

(define directory-files
  (case-lambda
    ((dir)
     (directory-files dir #f))
    ((dir dotfiles?)
     (let ((proc (and (vfs? *vfs*) (hashtable-ref (vfs-all-ops *vfs*) 'directory-files #f))))
       (unless (procedure? proc)
         (raise
           (condition
            (make-i/o-filename-error dir)
            (make-who-condition 'directory-files)
            (make-message-condition "No procedure has been installed")
            (make-irritants-condition (list dir dotfiles?)))))
       (proc dir dotfiles?)))))

;; Some kind of virtual filesystem
(define (install-vfs . op*)
  (let ((extra (make-eq-hashtable)))
    (let lp ((op* op*) (delete-file #f) (file-exists? #f)
             (open-file #f) (set-file-mode #f))
      (if (null? op*)
          (set! *vfs* (make-vfs delete-file file-exists?
                                open-file set-file-mode
                                extra))
          (let ((op (car op*))
                (proc (cadr op*)))
            (hashtable-set! extra op proc)
            (case op
              ((delete-file) (lp (cddr op*) proc file-exists? open-file set-file-mode))
              ((file-exists?) (lp (cddr op*) delete-file proc open-file set-file-mode))
              ((open-file) (lp (cddr op*) delete-file file-exists? proc set-file-mode))
              ((set-file-mode) (lp (cddr op*) delete-file file-exists? open-file proc))
              (else (lp (cddr op*) delete-file file-exists?
                        open-file set-file-mode))))))))

;; TODO: SRFI-138
(define (features)
  '(r6rs
    syntax-case r7rs exact-closed exact-complex ieee-float full-unicode ratios
    loko
    x86-64 little-endian)))
