;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2019, 2020 G. Weinholt
#!r6rs

;;; SRFI-170 (POSIX) support library for Linux

#|

Known non-conformance and extensions:

 * temp-file-prefix is a Chez-style parameter, but should be a
   SRFI-39/R7RS parameter

 * read-directory has an optional flag that causes it to return
   directory-entry objects instead of just filenames

 * uname is kept from SRFI 170 draft #6 as an extension

 * The user-info and group-info procedures do not try to access the
   databases via NSS and therefore will not see some users and groups.
   If this is a problem for you then please open an issue.

 * Certain file system operations can block all fibers. This is a
   general issue with Loko on Linux or *BSD and should be fixed.

 * Binary output ports do not have a line buffering mode, so writing
   #xA does not trigger a flush.

|#

(library (srfi :170 compat)
  (export
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
    directory-entry-inode directory-entry-offset
    directory-entry-type directory-entry-filename

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

    uname
    uname?
    uname:os-name uname:node-name uname:release-name uname:version uname:machine

    get-environment-variables get-environment-variable
    set-environment-variable! delete-environment-variable!

    posix-time monotonic-time

    terminal?)
  (import
    (rnrs (6))
    (rnrs mutable-strings (6))
    (srfi :19 time)
    (srfi :98 os-environment-variables)
    (srfi :198 private)                 ;TODO: this was withdrawn
    (only (loko) make-parameter port-file-descriptor port-file-descriptor-set!
          port-buffer-mode-set! putenv)
    (loko match)
    (loko system fibers)
    (loko system random)
    (loko system time)
    (loko system unsafe)
    (loko arch amd64 linux-syscalls)
    (loko arch amd64 linux-numbers))

(define binary-input 'binary-input)
(define textual-input 'textual-input)
(define binary-output 'binary-output)
(define textual-output 'textual-output)
(define binary-input/output 'binary-input/output)
(define buffer-none 'buffer-none)
(define buffer-block 'buffer-block)
(define buffer-line 'buffer-line)

;; Convert a filename to a NUL terminated bytevector.
(define (filename->c-string who fn)
  (unless (string? fn)
    (assertion-violation who "Expected a string" fn))
  (string-for-each
   (lambda (c)
     (when (eqv? c #\nul)
       ;; The filename is not representable on Linux
       (raise (condition
               (make-who-condition who)
               (make-i/o-filename-error fn)
               (make-message-condition "Unrepresentable filename")
               (make-irritants-condition (list fn))))))
   fn)
  (string->utf8 (string-append fn "\x0;")))

;; Convert a NUL terminated bytevector to a string.
(define (utf8z->string src offset)
  (do ((len 0 (fx+ len 1)))
      ((if (fx=? (fx+ offset len) (bytevector-length src))
           (assertion-violation 'utf8z->string "No NUL terminator found" src offset)
           (eqv? (bytevector-u8-ref src (fx+ offset len)) 0))
       (let ((tmp (make-bytevector len)))
         (bytevector-copy! src offset
                           tmp 0 (bytevector-length tmp))
         (utf8->string tmp)))))

(define & bytevector-address)

(define-syntax define-optional
  (lambda (x)
    (define (opt-clauses name args* opt*)
      (syntax-case opt* ()
        [() '()]
        [((lhs rhs) (lhs* rhs*) ...)
         (with-syntax ([(args ...) args*])
           #`([(args ...) (#,name args ... rhs)]
              #,@(opt-clauses name #'(args ... lhs) #'((lhs* rhs*) ...))))]))
    (syntax-case x ()
      [(_ (name args ... [(lhs* rhs*) ...])
          . body)
       #`(define name
           (case-lambda
             #,@(opt-clauses #'name #'(args ...) #'((lhs* rhs*) ...))
             [(args ... lhs* ...) . body]))])))

(define (string-split str c)
  (let lp ((start 0) (end 0))
    (cond ((fx=? end (string-length str))
           (list (substring str start end)))
          ((char=? c (string-ref str end))
           (cons (substring str start end)
                 (lp (fx+ end 1) (fx+ end 1))))
          (else
           (lp start (fx+ end 1))))))

(define-syntax with-restart
  (syntax-rules ()
    ((_ (proc args ... handler))
     (let retry ()
       (proc args ... (lambda (errno)
                        (cond ((eqv? errno EINTR)
                               ;; The syscall was interrupted.
                               (yield-current-task)
                               (retry))
                              (else
                               (handler errno)))))))))

;;; 3.2 I/O

;; (open-file fname port-type flags [permission-bits [buffer-mode] ])

(define-optional (open-file fname port-type flags [(permission-bits #o666) (buffer-mode buffer-block)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'open-file)
       (make-message-condition "Failed to open the file")
       (make-irritants-condition (list fname flags permission-bits))
       (filename-condition syswho errno fname)
       (make-syscall-error syswho errno))))
  (assert (memq port-type '(binary-input textual-input binary-output textual-output binary-input/output)))
  (let ((fname-bv (filename->c-string 'open-file fname)))
    (let ((fd (with-restart
               (sys_openat AT_FDCWD (bytevector-address fname-bv)
                           (fxior flags (fxior O_NOCTTY O_LARGEFILE O_NONBLOCK O_CLOEXEC))
                           permission-bits
                           (lambda (errno) (raise-error 'openat errno))))))
      (fd->port fd port-type buffer-mode fname))))

(define open/read O_RDONLY)
(define open/write O_WRONLY)
(define open/read+write O_RDWR)
(define open/append O_APPEND)
(define open/create O_CREAT)
(define open/exclusive O_EXCL)
(define open/nofollow O_NOFOLLOW)
(define open/truncate O_TRUNC)

(define-optional (fd->port fd port-type [(buffer-mode 'block)
                                         (id (string-append "fd " (number->string fd)))])
  (define position (sys_lseek fd 0 SEEK_CUR (lambda _ #f)))
  (define (handle-read-error errno)
    (raise
      (condition
       (make-syscall-error 'read errno))))
  (define (read! bv start count)
    (let ((status (let retry ()
                    (sys_read fd (fx+ (& bv) start) count
                              (lambda (errno)
                                (cond ((eqv? errno EAGAIN)
                                       (wait-for-readable fd)
                                       (retry))
                                      ((eqv? errno EINTR)
                                       (yield-current-task)
                                       (retry))
                                      (else
                                       (handle-read-error errno))))))))
      (when position
        (set! position (+ status)))
      status))
  (define (handle-write-error errno)
    (raise
      (condition
       (make-syscall-error 'write errno))))
  (define (write! bv start count)
    (let ((status
           (let retry ()
             (sys_write fd (fx+ (bytevector-address bv) start) count
                        (lambda (errno)
                          (cond ((eqv? errno EAGAIN)
                                 (wait-for-writable fd)
                                 (retry))
                                ((eqv? errno EINTR)
                                 (retry))
                                (else
                                 (handle-write-error errno))))))))
      (when position
        (set! position (+ position status)))
      status))
  (define get-position
    (and position
         (lambda () position)))
  (define set-position!
    (and position
         (lambda (off)
           (sys_lseek fd off SEEK_SET)
           (set! position off))))
  (define (close)
    (sys_close fd))
  (let ((p (case port-type
             ((binary-input) (make-custom-binary-input-port id read! get-position set-position! close))
             ((binary-output) (make-custom-binary-output-port id write! get-position set-position! close))
             ((binary-input/output) (make-custom-binary-input/output-port
                                     id write! get-position set-position! close))
             ((textual-input) (transcoded-port
                               (make-custom-binary-input-port id read! get-position set-position! close)
                               (native-transcoder)))
             ((textual-output) (transcoded-port
                                (make-custom-binary-output-port id write! get-position set-position! close)
                                (native-transcoder)))
             (else
              (assertion-violation 'open-file "Invalid port-type" fd port-type buffer-mode)))))
    (port-file-descriptor-set! p fd)
    (port-buffer-mode-set! p buffer-mode)
    p))

;;; 3.3 File system

;; As per R6RS, these are also used when appropriate and feasible:

;; make-i/o-filename-error
;; make-i/o-file-protection-error
;; make-i/o-file-is-read-only-error
;; make-i/o-file-already-exists-error
;; make-i/o-file-does-not-exist-error

(define (filename-condition syswho errno fname)
  (cond ((eqv? errno EACCES)
         (make-i/o-file-protection-error fname))
        ((eqv? errno EROFS)
         ;; XXX: There are actually more errors that should be covered
         ;; by this, but it requires more syscalls to find the actual
         ;; cause of the error.
         (make-i/o-file-is-read-only-error fname))
        ((eqv? errno EEXIST)
         (make-i/o-file-already-exists-error fname))
        ((eqv? errno ENOENT)
         (make-i/o-file-does-not-exist-error fname))
        (else
         (make-i/o-filename-error fname))))

(define-optional (create-directory fname [(permission-bits #o775)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-directory)
       (make-message-condition "Failed to create the directory")
       (make-irritants-condition (list fname permission-bits))
       (filename-condition syswho errno fname)
       (make-syscall-error syswho errno))))
  (unless (fixnum? permission-bits)
    (assertion-violation 'create-directory "Expected an exact integer"
                         fname permission-bits))
  (let ((fname-bv (filename->c-string 'create-directory fname)))
    (with-restart
     (sys_mkdirat AT_FDCWD (& fname-bv) permission-bits
                  (lambda (errno) (raise-error 'mkdirat errno))))
    (values)))

(define-optional (create-fifo fname [(permission-bits #o664)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-fifo)
       (make-message-condition "Failed to create the FIFO")
       (make-irritants-condition (list fname permission-bits))
       (filename-condition syswho errno fname)
       (make-syscall-error syswho errno))))
  (unless (fixnum? permission-bits)
    (assertion-violation 'create-fifo "Expected an exact integer"
                         fname permission-bits))
  (let ((fname-bv (filename->c-string 'create-fifo fname))
        (mode (fxior (fxand permission-bits (fxnot S_IFMT)) S_IFSOCK)))
    (with-restart
     (sys_mknodat AT_FDCWD (& fname-bv) mode 0
                  (lambda (errno) (raise-error 'mknodat errno))))
    (values)))

(define (create-hard-link oldname newname)
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-hard-link)
       (make-message-condition "Failed to create the link")
       (make-irritants-condition (list oldname newname))
       ;; XXX: This error reporting could be improved
       (if (eqv? errno EEXIST)
           (filename-condition syswho errno newname)
           (filename-condition syswho errno oldname))
       (make-syscall-error syswho errno))))
  (let ((oldname-bv (filename->c-string 'create-hard-link oldname))
        (newname-bv (filename->c-string 'create-hard-link newname)))
    (with-restart
     (sys_linkat AT_FDCWD (& oldname-bv) AT_FDCWD (& newname-bv) 0
                 (lambda (errno) (raise-error 'linkat errno))))
    (values)))

(define (create-symlink oldname newname)
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-symlink)
       (make-message-condition "Failed to create the symlink")
       (make-irritants-condition (list oldname newname))
       (filename-condition syswho errno newname)
       (make-syscall-error syswho errno))))
  (let ((oldname-bv (filename->c-string 'create-symlink oldname))
        (newname-bv (filename->c-string 'create-symlink newname)))
    (with-restart
     (sys_symlinkat (& oldname-bv) AT_FDCWD (& newname-bv)
                    (lambda (errno) (raise-error 'symlinkat errno))))
    (values)))

(define (read-symlink filename)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'read-symlink)
       (make-message-condition "Failed to read symlink")
       (make-irritants-condition (list filename))
       (filename-condition 'readlinkat errno filename)
       (make-syscall-error 'readlinkat errno))))
  (let ((fn (filename->c-string 'read-symlink filename)))
    (let lp ((bufsize 128))
      (let ((buf (make-bytevector bufsize)))
        (let ((len (sys_readlinkat AT_FDCWD (& fn) (& buf) bufsize raise-error)))
          (cond ((fx=? len bufsize)
                 ;; The filename was truncated
                 (lp (fx* bufsize 2)))
                (else
                 (utf8z->string buf 0))))))))

(define (rename-file oldname newname)
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'rename-file)
       (make-message-condition "Failed to rename the file")
       (make-irritants-condition (list oldname newname))
       (filename-condition syswho errno newname)
       (make-syscall-error syswho errno))))
  (let ((oldname-bv (filename->c-string 'rename-file oldname))
        (newname-bv (filename->c-string 'rename-file newname)))
    (with-restart
     (sys_renameat2 AT_FDCWD (& oldname-bv) AT_FDCWD (& newname-bv) 0
                    (lambda (errno) (raise-error 'renameat2 errno))))
    (values)))

(define (delete-directory fname)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'delete-directory)
       (make-message-condition "Failed to delete the directory")
       (make-irritants-condition (list fname))
       (filename-condition 'unlinkat errno fname)
       (make-syscall-error 'unlinkat errno))))
  (let ((newname-bv (filename->c-string 'delete-directory fname)))
    (with-restart
     (sys_unlinkat AT_FDCWD (& newname-bv) AT_REMOVEDIR raise-error))
    (values)))

(define (set-file-mode fname permission-bits)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-file-mode)
       (make-message-condition "Could not set file mode")
       (make-irritants-condition (list fname))
       (make-i/o-filename-error fname)
       (make-syscall-error 'fchmodat errno))))
  (let ((fname-bv (filename->c-string 'set-file-mode fname)))
    (with-restart
     (sys_fchmodat AT_FDCWD (& fname-bv) permission-bits 0 raise-error)))
  (values))

(define owner/unchanged -1)
(define group/unchanged -1)

(define (set-file-owner fname uid gid)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-file-mode)
       (make-message-condition "Could not set file owner")
       (make-irritants-condition (list fname uid))
       (make-i/o-filename-error fname)
       (make-syscall-error 'fchownat errno))))
  (let ((fname-bv (filename->c-string 'set-file-mode fname)))
    (with-restart
     (sys_fchownat AT_FDCWD (& fname-bv) uid gid 0 raise-error)))
  (values))

(define time/now (make-time 'srfi-170 UTIME_NOW 0))
(define time/omit (make-time 'srfi-170 UTIME_OMIT 0))

(define-optional (set-file-times fname [(access-timespec time/now) (mod-timespec time/now)])
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-file-times)
       (make-message-condition "Could not set file times")
       (make-irritants-condition (list fname access-timespec mod-timespec))
       (make-i/o-filename-error fname)
       (make-syscall-error 'utimensat errno))))
  (let ((utimes (make-bytevector 32)))
    (bytevector-u64-native-set! utimes 24 (time-nanosecond mod-timespec))
    (bytevector-u64-native-set! utimes 16 (time-second mod-timespec))
    (bytevector-u64-native-set! utimes 8 (time-nanosecond access-timespec))
    (bytevector-u64-native-set! utimes 0 (time-second access-timespec))
    (let ((fname-bv (filename->c-string 'set-file-times fname)))
      (with-restart
       (sys_utimensat AT_FDCWD (& fname-bv) (& utimes) 0 raise-error))))
  (values))

(define (truncate-file fname/port len)
  (define (raise-error errno)
    (let ((syswho (if (string? fname/port) 'truncate 'ftruncate)))
      (raise
        (condition
         (make-who-condition 'truncate-file)
         (make-message-condition "Could not truncate file")
         (make-irritants-condition (list fname/port len))
         (if (string? fname/port)
             (make-i/o-filename-error fname/port)
             (condition))
         (make-syscall-error syswho errno)))))
  (unless (fixnum? len)
    (assertion-violation 'truncate-file "Expected a fixnum" fname/port len))
  (if (string? fname/port)
      (let ((fname-bv (filename->c-string 'truncate-file fname/port)))
        (with-restart
         (sys_truncate (& fname-bv) len raise-error)))
      (cond ((port-file-descriptor fname/port) =>
             (lambda (fd)
               (with-restart
                (sys_ftruncate fd len raise-error))))
            (else
             (assertion-violation 'truncate-file
                                  "Expected a filename or a port with a file descriptor"
                                  fname/port len))))
  (values))

(define-record-type (&file-info make-file-info file-info?)
  (sealed #t)
  (fields (immutable device file-info:device)
          (immutable inode file-info:inode)
          (immutable mode file-info:mode)
          (immutable nlinks file-info:nlinks)
          (immutable uid file-info:uid)
          (immutable gid file-info:gid)
          (immutable rdev file-info:rdev)
          (immutable size file-info:size)
          (immutable blksize file-info:blksize)
          (immutable blocks file-info:blocks)
          (immutable atime file-info:atime)
          (immutable mtime file-info:mtime)
          (immutable ctime file-info:ctime)))

;; One of stat, lstat or fstat
(define-optional (file-info filename/port [(follow? #t)])
  (define (bytevector-time-native-ref stat offset)
    (make-time 'time-utc (bytevector-u64-native-ref stat (fx+ offset 8))
               (bytevector-s64-native-ref stat offset)))
  (define (raise-error errno)
    (let ((syswho (if (string? filename/port) (if follow? 'stat 'lstat) 'fstat)))
      (raise (condition
              (make-who-condition 'file-info)
              (make-message-condition "Could not stat file")
              (make-irritants-condition (list filename/port))
              (if (string? filename/port)
                  (make-i/o-filename-error filename/port)
                  (condition))
              (make-syscall-error syswho errno)))))
  (let ((statbuf (make-bytevector sizeof-stat)))
    ;; Call the right stat syscall
    (if (string? filename/port)
        (let ((fn (filename->c-string 'file-info filename/port)))
          (if follow?
              (with-restart (sys_stat (& fn) (& statbuf) raise-error))
              (with-restart (sys_lstat (& fn) (& statbuf) raise-error))))
        (cond ((port-file-descriptor filename/port) =>
               (lambda (fd) (with-restart (sys_fstat fd (& statbuf) raise-error))))
              (else
               (assertion-violation 'file-info
                                    "Expected a filename or a port with a file descriptor"
                                    filename/port))))
    ;; Decode the buffer
    (let ((device (bytevector-u64-native-ref statbuf offsetof-stat-st_dev))
          (inode (bytevector-u64-native-ref statbuf offsetof-stat-st_ino))
          (mode (bytevector-u32-native-ref statbuf offsetof-stat-st_mode))
          (nlinks (bytevector-u32-native-ref statbuf offsetof-stat-st_nlink))
          (uid (bytevector-u32-native-ref statbuf offsetof-stat-st_uid))
          (gid (bytevector-u32-native-ref statbuf offsetof-stat-st_gid))
          (rdev (bytevector-u64-native-ref statbuf offsetof-stat-st_rdev))
          (size (bytevector-s64-native-ref statbuf offsetof-stat-st_size))
          (blksize (bytevector-s32-native-ref statbuf offsetof-stat-st_blksize))
          (blocks (bytevector-s64-native-ref statbuf offsetof-stat-st_blocks))
          (atime (bytevector-time-native-ref statbuf offsetof-stat-st_atime))
          (mtime (bytevector-time-native-ref statbuf offsetof-stat-st_mtime))
          (ctime (bytevector-time-native-ref statbuf offsetof-stat-st_ctime)))
      (make-file-info device inode mode nlinks uid gid rdev
                      size blksize blocks atime mtime ctime))))

(define (file-info-directory? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFDIR))

(define (file-info-fifo? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFIFO))

(define (file-info-regular? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFREG))

(define (file-info-symlink? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFLNK))

(define (file-info-socket? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFIFO) S_IFIFO))

(define (file-info-device? file-info)
  (not (eqv? 0 (fxand (file-info:mode file-info) (fxior S_IFCHR S_IFBLK)))))

(define-optional (open-directory dirname [(dot-files? #f)])
  (let* ((fn (filename->c-string 'open-directory dirname))
         (fd (with-restart
              (sys_openat AT_FDCWD (& fn)
                          (bitwise-ior O_CLOEXEC O_DIRECTORY O_NONBLOCK)
                          0
                          (lambda (errno)
                            (raise
                              (condition
                               (make-who-condition 'open-directory)
                               (make-message-condition "Could not open directory")
                               (make-irritants-condition (list dirname))
                               (if (eqv? errno ENOENT)
                                   (make-i/o-file-does-not-exist-error dirname)
                                   (make-i/o-filename-error dirname))
                               (make-syscall-error 'open errno))))))))
    (define (read! bv start count)
      (let retry ()
        (with-restart
         (sys_getdents64 fd (fx+ (& bv) start) count
                         (lambda (errno)
                           (cond ((eqv? errno EAGAIN)
                                  (wait-for-readable fd)
                                  (retry))
                                 (else
                                  (raise
                                    (condition
                                     (make-who-condition 'read-directory)
                                     (make-i/o-filename-error dirname)
                                     (make-syscall-error 'getdents64 errno))))))))))
    (define get-position #f)
    (define set-position! #f)
    (define (close)
      (sys_close fd (lambda (errno)
                      (raise
                        (make-who-condition 'close-directory)
                        (make-message-condition "Error while closing the directory")
                        (make-i/o-filename-error dirname)
                        (make-syscall-error 'close errno)))))
    (let ((p (make-custom-binary-input-port
              dirname read! get-position set-position! close)))
      (port-file-descriptor-set! p fd)
      (make-dir p dot-files?))))

(define-record-type dir
  (fields port dot-files?))

(define-record-type directory-entry
  (fields inode offset type filename))

(define-optional (read-directory dir [(full-info? #f)])
  ;; Read and parse a "struct linux_dirent64" (which is apparently not
  ;; part of the UAPI headers). The full-info? argument is an
  ;; extension.
  (define sizeof-linux_dirent64 19)
  (let ((port (dir-port dir)))
    (let lp ()
      (if (port-eof? port)
          (eof-object)
          (let ((bv (get-bytevector-n port sizeof-linux_dirent64)))
            (let ((d_ino (bytevector-u64-native-ref bv 0))
                  (d_off (bytevector-u64-native-ref bv 8))
                  (d_reclen (bytevector-u16-native-ref bv 16))
                  (d_type (bytevector-u8-ref bv 18)))
              (let ((fn (utf8z->string (get-bytevector-n port (fx- d_reclen sizeof-linux_dirent64))
                                       0)))
                (if (or (and (not (dir-dot-files? dir)) (char=? (string-ref fn 0) #\.))
                        (member fn '("." "..")))
                    (lp)
                    (if full-info?
                        (make-directory-entry d_ino d_off d_type fn)
                        fn)))))))))

(define (close-directory dir)
  (close-port (dir-port dir)))

(define-optional (directory-files dirname [(dotfiles? #f) (full-info? #f)])
  (define (unwind-protectish thunk on-exit)
    (with-exception-handler
      (lambda (exn)
        (on-exit)
        (raise exn))
      (lambda ()
        (let-values ([x (thunk)])
          (on-exit)
          (apply values x)))))
  (let ((dir (open-directory dirname dotfiles?)))
    (unwind-protectish
     (lambda ()
       (let lp ((ret '()))
         (let ((fn (read-directory dir full-info?)))
           (if (eof-object? fn)
               ret
               (lp (cons fn ret))))))
     (lambda ()
       (close-directory dir)))))

(define-optional (make-directory-files-generator dirname [(dotfiles? #f)])
  (let ((dir (open-directory dirname dotfiles?))
        (closed #f))
    (lambda ()
      (if closed
          (eof-object)
          (let ((fn (read-directory dir)))
            (cond ((eof-object? fn)
                   (close-directory dir)
                   (set! closed #t)
                   (eof-object))
                  (else fn)))))))

;; FIXME: Must be a SRFI-39/R7RS parameter
(define temp-file-prefix
  (make-parameter
   (let ((tmpdir (or (get-environment-variable "TMPDIR") "/tmp")))
     (string-append tmpdir "/" (number->string (sys_getpid))))))

(define (string-index-right str c)
  (do ((i (fx- (string-length str) 1) (fx- i 1)))
      ((or (fx=? i -1) (eqv? (string-ref str i) c))
       i)))

(define (make-temp-name)
  (let* ((len (fx+ 8 (fxand (get-random-u8) 7)))
         (ret (make-string len)))
    (do ((i 0 (fx+ i 1)))
        ((fx=? i len) ret)
      (string-set! ret i (integer->char
                          (fx+ (char->integer #\A)
                               (fxmod (get-random-u8) 26)))))))

(define-optional (create-temp-file [(prefix (temp-file-prefix))])
  (define who 'create-temp-file)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition who)
       (make-message-condition "Failed to create temporary file")
       (make-irritants-condition (list prefix))
       (make-syscall-error 'linkat errno))))
  (let* ((slash (string-index-right prefix #\/))
         (dir (if (eqv? slash -1)
                  "."
                  (substring prefix 0 slash)))
         (dir-bv (filename->c-string who dir)))
    ;; Create an anonymous file
    (let* ((fd (sys_openat AT_FDCWD (& dir-bv) (fxior O_TMPFILE O_WRONLY) #o600))
           (procname (string-append "/proc/self/fd/" (number->string fd)))
           (procname-bv (filename->c-string who procname)))
      (let retry ((tries 100))
        (let* ((tempname (string-append prefix (make-temp-name)))
               (tempname-bv (filename->c-string who tempname)))
          ;; Try to link the file to a new name
          (case (with-restart
                 (sys_linkat AT_FDCWD (& procname-bv) AT_FDCWD (& tempname-bv) AT_SYMLINK_FOLLOW
                             (lambda (errno)
                               (cond ((and (eqv? errno EEXIST) (not (eqv? tries 0)))
                                      'exists)
                                     (else
                                      (sys_close fd)
                                      (raise-error errno))))))
            ((exists)
             (retry (fx- tries 1)))
            (else
             (sys_close fd)
             tempname)))))))

;; XXX: Please don't use this to create temporary files; use the
;; procedure above instead. It is hopefully secure against symlink
;; attacks.
(define-optional (call-with-temporary-filename maker [(prefix (temp-file-prefix))])
  (let retry ((tries 100))
    (let ((tempname (string-append prefix (make-temp-name))))
      (let-values ([(x . rest)
                    (guard (exn
                            ((and (syscall-errno-condition? exn)
                                  (not (eqv? 0 tries)))
                             #f))
                      (maker tempname))])
        (if (not x)
            (retry (fx- tries 1))
            (apply values x rest))))))

(define (real-path path^)
  (define error-msg "Failed to get the canonicalized absolute pathname")
  (define (path-is-symlink? filename enoent-ok)
    (let* ((statbuf (make-bytevector sizeof-stat))
           (fn (filename->c-string 'real-path filename)))
      (and
        (with-restart
         (sys_lstat (& fn) (& statbuf)
                    (lambda (errno)
                      (cond ((and (eqv? errno ENOENT) enoent-ok)
                             #f)
                            (else
                             (raise (condition
                                     (make-who-condition 'real-path)
                                     (make-message-condition error-msg)
                                     (make-irritants-condition (list path^))
                                     (make-i/o-file-protection-error filename)
                                     (make-syscall-error 'lstat errno))))))))
        (eqv? (fxand (bytevector-u32-native-ref statbuf offsetof-stat-st_mode)
                     S_IFMT)
              S_IFLNK))))
  (define (join-components components)
    (if (null? components)
        "/"
        (call-with-string-output-port
          (lambda (p)
            (for-each
             (lambda (component)
               (put-char p #\/)
               (put-string p component))
             components)))))
  (let loop ((path path^) (seen #f) (limit 500))
    (when (eqv? limit 0)
      ;; This is mostly in case the hashtable approach isn't foolproof
      (raise
        (condition
         (make-who-condition 'real-path)
         (make-message-condition error-msg)
         (make-irritants-condition (list path^))
         (make-i/o-filename-error path^))))
    (let ((path (if (or (eqv? 0 (string-length path))
                        (not (eqv? #\/ (string-ref path 0))))
                    (string-append (current-directory) "/" path)
                    path)))
      (let lp ((x '())
               (components (string-split path #\/)))
        (match components
          [("" . c*) (lp x c*)]
          [("." . c*) (lp x c*)]
          [(".." . c*)
           (if (null? x)
               (lp x c*)
               (lp (cdr x) c*))]
          [(c . c*)
           (let ((tmppath (join-components (reverse (cons c x)))))
             (cond ((path-is-symlink? tmppath (null? c*))
                    (let ((target (read-symlink tmppath))
                          (seen (or seen (make-hashtable string-hash string=?))))
                      (hashtable-update! seen tmppath
                                         (lambda (old)
                                           (if old
                                               (raise
                                                 (condition
                                                  (make-who-condition 'real-path)
                                                  (make-message-condition error-msg)
                                                  (make-irritants-condition (list path tmppath))
                                                  (make-i/o-filename-error path^)))
                                               #t))
                                         #f)
                      (if (eqv? #\/ (string-ref target 0))
                          (loop (string-append target (join-components c*)) seen (fx- limit 1))
                          (loop (string-append (join-components (reverse x)) "/" target "/"
                                               (join-components c*))
                                seen (fx- limit 1)))))
                   (else
                    (lp (cons c x) c*))))]
          [()
           (join-components (reverse x))])))))

(define (file-space path-or-port)
  (let ((buf (make-bytevector sizeof-statfs)))
    (cond ((string? path-or-port)
           (let ((fn (filename->c-string 'file-space path-or-port)))
             (sys_statfs (& fn) (& buf))))
          (else
           (sys_fstatfs (port-file-descriptor path-or-port) (& buf))))
    (* (bytevector-u64-native-ref buf offsetof-statfs-f_bavail)
       (bytevector-u64-native-ref buf offsetof-statfs-f_bsize))))

;;; 3.5 Process state

(define (umask)
  (let ((mask (sys_umask #o22)))
    (sys_umask mask)
    mask))

;; TODO: Maintain this in the fiber
(define (set-umask! mask)
  (sys_umask mask))

(define (current-directory)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'current-directory)
       (make-message-condition "Could not get the current directory")
       (make-irritants-condition '())
       (make-syscall-error 'getcwd errno))))
  (let lp ((size 128))
    (let* ((buf (make-bytevector size))
           (status
            (with-restart
             (sys_getcwd (& buf) (bytevector-length buf)
                         (lambda (errno)
                           (cond ((eqv? errno ERANGE)
                                  'resize)
                                 (else
                                  (raise-error errno))))))))
      (case status
        ((resize)
         (lp (fx* size 2)))
        (else
         (utf8z->string buf 0))))))

;; TODO: Maintain this in the fiber
(define-optional (set-current-directory! [(fname #f)])
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-current-directory!)
       (make-message-condition "Could not set the current directory")
       (make-irritants-condition (list fname))
       (make-syscall-error 'chdir errno))))
  (let ((fname (or fname (get-environment-variable "HOME"))))
    (unless fname
      (assertion-violation 'set-current-directory!
                           "Failed to get the home directory"))
    (let ((fname-bv (filename->c-string 'set-current-directory fname)))
      (with-restart
       (sys_chdir (& fname-bv) raise-error))
      (values))))

(define (pid)
  (sys_getpid))

(define-optional (nice [(delta 1)])
  (let ((current (fx- 20 (sys_getpriority PRIO_PROCESS 0))))
    (sys_setpriority PRIO_PROCESS 0 (fx+ current delta))))

(define (user-uid)
  (sys_getuid))

(define (user-gid)
  (sys_getgid))

(define (user-effective-uid)
  (sys_geteuid))

(define (user-effective-gid)
  (sys_getegid))

(define (user-supplementary-gids)
  (let* ((groups (sys_getgroups 0 0))
         (buf (make-bytevector (fx* groups sizeof-gid_t))))
    (sys_getgroups (bytevector-length buf) (& buf))
    (bytevector->uint-list buf (native-endianness) sizeof-gid_t)))

;;; 3.6 User and group database access

;; This chapter is a pain to implement fully without access to libc on
;; a system where everything else uses libc. It would be nice, in a
;; way, if /etc/passwd was a virtual file. Perhaps a file that you
;; could send queries to. One can dream...

;; If you're reading this because you found out about the limitation
;; the hard way, then please open an issue. There's a way to solve
;; this with a daemon that relays NSS info.

(define (user-info uid/name)
  (when (not (file-exists? "/etc/passwd"))
    (error 'user-info "No /etc/passwd file" uid/name))
  (call-with-input-file "/etc/passwd"
    (lambda (p)
      (let lp ()
        (let ((line (get-line p)))
          (if (eof-object? line)
              #f
              (let ((parts (string-split line #\:)))
                (if (not (fx=? (length parts) 7))
                    (lp)
                    (let ((name (car parts))
                          (uid (string->number (list-ref parts 2) 10)))
                      (if (or (equal? name uid/name) (equal? uid uid/name))
                          (apply make-user-info parts)
                          (lp)))))))))))

(define-record-type (&user-info make-user-info user-info?)
  (fields (immutable name user-info:name)
          passwd
          (immutable uid user-info:uid)
          (immutable gid user-info:gid)
          (immutable full-name user-info:full-name) ;gecos
          (immutable home-dir user-info:home-dir)
          (immutable shell user-info:shell)))

(define (user-info:parsed-full-name user-info)
  (let ((parts (string-split (user-info:full-name user-info) #\,)))
    (if (null? parts)
        '()
        (let ((part0
               (call-with-string-output-port
                 (lambda (p)
                   (string-for-each
                    (lambda (c)
                      (if (eqv? c #\&)
                          (let ((name (user-info:name user-info)))
                            (let ((c0 (string-ref name 0)))
                              (put-char p
                                        (if (char<? c0 #\delete)
                                            (char-upcase c0)
                                            c0)))
                            (put-string p (substring name 1 (string-length name))))
                          (put-char p c)))
                    (car parts))))))
          (cons part0 (cdr parts))))))

(define (group-info gid/name)
  (when (not (file-exists? "/etc/group"))
    (error 'group-info "No /etc/group file" gid/name))
  (call-with-input-file "/etc/group"
    (lambda (p)
      (let lp ()
        (let ((line (get-line p)))
          (if (eof-object? line)
              #f
              (let ((parts (string-split line #\:)))
                (if (not (fx=? (length parts) 4))
                    (lp)
                    (let ((name (car parts))
                          (uid (string->number (list-ref parts 2) 10)))
                      (if (or (equal? name gid/name) (equal? uid gid/name))
                          (apply make-group-info parts)
                          (lp)))))))))))

(define-record-type (&group-info make-group-info group-info?)
  (fields (immutable name group-info:name)
          passwd
          (immutable gid group-info:gid)
          members))

;;; 3.8 System parameters

(define-record-type (&uname make-uname uname?)
  (fields (immutable os-name uname:os-name)
          (immutable node-name uname:node-name)
          (immutable release-name uname:release-name)
          (immutable version uname:version)
          (immutable machine uname:machine)
          (immutable domain-name uname:domain-name)))

(define (uname)
  (let ((buf (make-bytevector sizeof-new_utsname #xff)))
    (sys_uname (& buf))
    (make-uname (utf8z->string buf offsetof-new_utsname-sysname)
                (utf8z->string buf offsetof-new_utsname-nodename)
                (utf8z->string buf offsetof-new_utsname-release)
                (utf8z->string buf offsetof-new_utsname-version)
                (utf8z->string buf offsetof-new_utsname-machine)
                (utf8z->string buf offsetof-new_utsname-domainname))))

;;; 3.10 Time

(define (gettime type clock)
  (let* ((x (make-bytevector sizeof-timespec))
         (_ (sys_clock_gettime clock (& x)))
         (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
         (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
    (make-time type nanoseconds seconds)))

(define (posix-time)
  (gettime 'time-utc CLOCK_REALTIME))

(define (monotonic-time)
  (gettime 'time-monotonic CLOCK_MONOTONIC))

;;; 3.11

(define (set-environment-variable! name value)
  (putenv name value))

(define (delete-environment-variable! name)
  (putenv name #f))

;;; 3.12 Terminal device control

(define (terminal? port)
  (cond
    ((port-file-descriptor port) =>
     (lambda (fd)
       (let ((buf (make-bytevector sizeof-termios)))
         (and (with-restart
               (sys_ioctl fd TCGETS (& buf)
                          (lambda (errno)
                            (if (eqv? errno ENOTTY)
                                #f
                                (raise
                                  (condition
                                   (make-who-condition 'terminal?)
                                   (make-irritants-condition (list port))
                                   (make-syscall-error 'ioctl fd 'TCGETS 'buf)))))))
              #t))))
    (else #f))))
