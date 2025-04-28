;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; SRFI-170 (POSIX)

(library (srfi :170 posix)
  (export
    posix-error? posix-error-name posix-error-message

    binary-input textual-input
    binary-output textual-output
    binary-input/output

    buffer-none buffer-block buffer-line

    open-file open/read open/write open/read+write open/append
    open/create open/exclusive open/nofollow open/truncate
    fd->port

    create-directory
    create-fifo
    create-hard-link
    create-symlink
    read-symlink
    rename-file
    delete-directory
    set-file-mode
    set-file-owner owner/unchanged group/unchanged
    set-file-times time/now time/omit
    truncate-file

    file-info
    file-info?
    file-info:device file-info:inode file-info:mode file-info:nlinks file-info:uid
    file-info:gid file-info:rdev file-info:size file-info:blksize file-info:blocks
    file-info:atime file-info:mtime file-info:ctime
    file-info-directory? file-info-fifo? file-info-regular? file-info-symlink?
    file-info-socket? file-info-device?

    directory-files
    make-directory-files-generator
    open-directory read-directory close-directory

    real-path
    file-space
    temp-file-prefix
    create-temp-file
    call-with-temporary-filename

    umask
    set-umask!
    current-directory
    set-current-directory!
    pid
    nice
    user-uid
    user-gid
    user-effective-uid
    user-effective-gid
    user-supplementary-gids

    user-info
    user-info?
    user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell
    user-info:full-name user-info:parsed-full-name
    group-info
    group-info?
    group-info:name group-info:gid

    get-environment-variables get-environment-variable
    set-environment-variable! delete-environment-variable!

    posix-time monotonic-time

    terminal?)

  (import
    (rnrs (6))
    (rename (srfi :170 compat)
            (read-directory loko:read-directory)
            (open-directory loko:open-directory)
            (directory-files loko:directory-files))
    (srfi :198 private))

(define posix-error? syscall-errno-condition?)
(define posix-error-name condition-syscall-symbol)
(define posix-error-message condition-syscall-message)

;; Removes the Loko extensions

(define (open-directory dirname)
  (loko:open-directory dirname))

(define (read-directory dir)
  (loko:read-directory dir))

(define directory-files
  (case-lambda
    ((dirname)
     (loko:directory-files dirname))
    ((dirname dotfiles?)
     (loko:directory-files dirname dotfiles?)))))
