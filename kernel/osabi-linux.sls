;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2021 G. Weinholt
#!r6rs

;;; (Very limited) Linux ABI compatibility

;; This is a proof of concept and a way to port some simpler programs.
;; It has been tested with: statically linked assembler programs,
;; statically linked musl programs, Free Pascal programs and Doom.

;; It is intended for Loko on bare metal. For running Linux program on
;; the Linux kernel, see (pre-srfi processes).

;; It has been extended with support for Valand to get simple
;; graphical programs working, but it's kind of dumb to do it that
;; way in the Linux support.

;; I have no intention of implementing complete support for the Linux
;; syscall interface here; it's completely wrong for the way Loko is
;; designed. Over half a century of cruft.

(library (loko kernel osabi-linux)
  (export
    spawn-linux-process)
  (import
    (rnrs)
    (loko system $host)
    (loko system fibers)
    (loko system logging)
    (loko system random)
    (loko system time)
    (loko system unsafe)
    (loko match)
    (loko queues)
    (loko kernel binfmt-elf)
    (only (loko) port-buffer-mode-set! loko-version parameterize)
    (only (loko runtime scheduler) pid-id)

    (loko arch amd64 pc-ustate)
    (loko arch amd64 linux-numbers)
    (only (machine-code format elf) PF-R PF-W PF-X)

    (struct pack)

    (loko valand)
    (loko valand drawing))

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))))

(define (log/debug . x*) (log/x DEBUG x*))
(define (log/warning . x*) (log/x WARNING x*))
(define (log/error . x*) (log/x ERROR x*))
(define (log/critical . x*) (log/x CRITICAL x*))

(define (fxtest a b) (not (eqv? 0 (fxand a b))))

;; TODO: Is this in the Linux headers anywhere?
(define SIG_DFL 0)
(define SS_DISABLE 2)

(define-record-type *nix-process
  (sealed #t)
  (fields pid
          page-table
          ustate
          (mutable brk)
          initial-brk
          fds
          ;; FIXME: this is obviously not a proper allocator
          (mutable mmap-start)
          valand-display
          sigactions
          (mutable sigaltstack)))

(define-record-type fdesc
  (sealed #t)
  (fields read
          write
          ioctl
          mmap-helper))

(define (*nix-process-next-free-fd process)
  (let ((fds (*nix-process-fds process)))
    (do ((i 0 (fx+ i 1)))
        ((not (hashtable-contains? fds i))
         i))))

(define (allocate-page)
  (dma-allocate 4096 (fxnot 4095)))

(define (open-user-memory-input-port pt)
  (define vaddr 0)
  (define &paddr #f)
  (define psize 0)
  (define (read! bv start count)
    (cond ((not (<= 0 vaddr (- (expt 2 47) 1)))
           0)
          (else
           (when (eqv? psize 0)
             (let-values ([(&paddr^ psize^) (page-table-lookup pt vaddr #f)])
               (set! &paddr &paddr^)
               (set! psize (or psize^ 0))))
           (if (eqv? psize 0)
               0
               (do ((n (fxmin count psize))
                    (i 0 (fx+ i 1)))
                   ((fx=? i n)
                    (set! &paddr (fx+ &paddr n))
                    (set! psize (fx- psize n))
                    (set! vaddr (fx+ vaddr n))
                    n)
                 (bytevector-u8-set! bv (fx+ start i)
                                     (get-mem-u8 (fx+ &paddr i))))))))
  (define (getpos)
    vaddr)
  (define (setpos pos)
    (set! vaddr pos)
    (set! psize 0))
  (let ((p (make-custom-binary-input-port "usermem" read! getpos setpos #f)))
    (port-buffer-mode-set! p (buffer-mode none))
    p))

(define (open-user-memory-output-port pt)
  (define vaddr 0)
  (define &paddr #f)
  (define psize 0)
  (define (write! bv start count)
    (cond ((not (<= 0 vaddr (- (expt 2 47) 1)))
           0)
          (else
           (when (eqv? psize 0)
             (let-values ([(&paddr^ psize^) (page-table-lookup pt vaddr #t)])
               (when (not &paddr^)
                 (error #f "User memory not accessible for writes" vaddr))
               (set! &paddr &paddr^)
               (set! psize (or psize^ 0))))
           (do ((n (fxmin count psize))
                (i 0 (fx+ i 1)))
               ((fx=? i n)
                (set! &paddr (fx+ &paddr n))
                (set! psize (fx- psize n))
                (set! vaddr (fx+ vaddr n))
                n)
             (put-mem-u8 (fx+ &paddr i)
                         (bytevector-u8-ref bv (fx+ start i)))))))
  (define (getpos)
    vaddr)
  (define (setpos pos)
    (set! vaddr pos)
    (set! psize 0))
  (let ((p (make-custom-binary-output-port "usermem" write! getpos setpos #f)))
    (port-buffer-mode-set! p (buffer-mode none))
    p))

(define (linux-read pt ustate fdesc vaddr size)
  (let ((size (fxmin #x7ffff000 size)))
    (cond
      ((not fdesc)
       (- EBADF))
      ((fx<? size 0)
       (- EINVAL))
      ((eqv? size 0)
       0)
      (else
       (let ((umem (open-user-memory-output-port pt)))
         (set-port-position! umem vaddr)
         (cond ((channel? fdesc)
                (let ((resp-ch (make-channel)))
                  (put-message fdesc (list resp-ch size))
                  (match (get-message resp-ch)
                    [bv
                     (log/warning "FOO: " (list 'bv bv 'size size))
                     (guard (exn
                             ((serious-condition? exn)
                              (log/warning "Hmm, EFAULT " exn)
                              ;; XXX: data loss
                              (- EFAULT)))
                       (cond ((eof-object? bv)
                              0)
                             (else
                              (put-bytevector umem bv)
                              (bytevector-length bv))))])))
               ((and (fdesc? fdesc) (fdesc-read fdesc))
                => (lambda (read) (read pt vaddr size)))
               (else
                (let ((bv (get-bytevector-n fdesc (fxmin (* 128 1024) size))))
                  (cond ((eof-object? bv)
                         0)
                        (else
                         (put-bytevector umem bv)
                         (flush-output-port umem)
                         (bytevector-length bv)))))))))))

(define (linux-write pt ustate fdesc vaddr size)
  (let ((size (fxmin #x7ffff000 size)))
    (cond
      ((not fdesc)
       (- EBADF))
      ((fx<? size 0)
       (- EINVAL))
      ((eqv? size 0)
       0)
      (else
       (let ((umem (open-user-memory-input-port pt)))
         (set-port-position! umem vaddr)
         (cond ((channel? fdesc)
                (match (get-message fdesc)
                  [(resp-ch bv start count)
                   (let ((n (get-bytevector-n! umem bv start (fxmin count size))))
                     (cond ((eof-object? n)
                            (put-message resp-ch -1)
                            (- EFAULT))
                           (else
                            (put-message resp-ch n)
                            n)))]))
               (else
                (let* ((size (fxmin (* 128 1024) size))
                       (bv (get-bytevector-n umem size)))
                  (cond ((bytevector? bv)
                         (put-bytevector fdesc bv)
                         (bytevector-length bv))
                        (else
                         0))))))))))

(define (linux-writev pt ustate fdesc *iov iovcnt)
  (let ((umem (open-user-memory-input-port pt)))
    (set-port-position! umem *iov)
    (cond
      ((not fdesc)
       (- EBADF))
      ((fx<? iovcnt 0)
       (- EINVAL))
      (else
       (let lp ((written 0) (iovcnt iovcnt))
         (cond
           ((eqv? iovcnt 0)
            written)
           (else
            (let ((bv (get-bytevector-n umem 16)))
              (if (or (eof-object? bv) (not (eqv? (bytevector-length bv) 16)))
                  (if (eqv? written 0)
                      (- EFAULT)
                      written)
                  (let ((iov_base (bytevector-u64-native-ref bv 0))
                        (iov_len (bytevector-u64-native-ref bv 8)))
                    (let ((n (linux-write pt ustate fdesc iov_base iov_len)))
                      (if (fx<? n 0)
                          (if (eqv? written 0)
                              n
                              written)
                          (lp (fx+ written n) (fx- iovcnt 1))))))))))))))

(define (linux-nanosleep pt ustate *req *rem)
  (let ((umem (open-user-memory-input-port pt)))
    (set-port-position! umem *req)
    (let ((bv (get-bytevector-n umem 16)))
      (if (or (eof-object? bv) (not (eqv? (bytevector-length bv) 16)))
          (- EFAULT)
          (let ((tv_sec (bytevector-u64-native-ref bv 0))
                (tv_nsec (bytevector-u64-native-ref bv 8)))
            (sleep (+ tv_sec (/ tv_nsec (expt 10 9))))
            (when (not (eqv? *rem 0))
              (let ((umem (open-user-memory-output-port pt)))
                (set-port-position! umem *rem)
                (bytevector-fill! bv 0)
                (put-bytevector umem bv)))
            0)))))

(define (linux-clock_nanosleep pt ustate clockid flags *req *rem)
  ;; TODO: handle clockid and flags
  (let ((umem (open-user-memory-input-port pt)))
    (set-port-position! umem *req)
    (let ((bv (get-bytevector-n umem 16)))
      (if (or (eof-object? bv) (not (eqv? (bytevector-length bv) 16)))
          (- EFAULT)
          (let ((tv_sec (bytevector-u64-native-ref bv 0))
                (tv_nsec (bytevector-u64-native-ref bv 8)))
            ;; (log/warning "sleep " (+ tv_sec (/ tv_nsec (expt 10 9))))
            (sleep (+ tv_sec (/ tv_nsec (expt 10 9))))
            (when (not (eqv? *rem 0))
              (let ((umem (open-user-memory-output-port pt)))
                (set-port-position! umem *rem)
                (bytevector-fill! bv 0)
                (put-bytevector umem bv)))
            0)))))

(define (linux-clock_gettime pt ustate process clk-id *tp)
  ;; TODO: handle clk-id
  (let ((bv (make-bytevector 16 0))
        (time (current-time/utc)))
    (bytevector-u64-native-set! bv 0 (time-second time))
    (bytevector-u64-native-set! bv 8 (time-nanosecond time))
    (when (not (eqv? *tp 0))
      (let ((umem (open-user-memory-output-port pt)))
        (set-port-position! umem *tp)
        (put-bytevector umem bv)))
    0))

(define (linux-brk pt ustate process brk)
  (define (align-up x n)
    (fxand (fx+ x (fx- n 1))
           (fxnot (fx- n 1))))
  (let ((old-brk (*nix-process-brk process)))
    (log/debug "brk: " (number->string brk 16) " old: " (number->string old-brk 16))
    (cond ((fx<? brk (*nix-process-initial-brk process))
           old-brk)
          ((fx=? brk old-brk)
           ;; Do nothing.
           old-brk)
          ((fx<? brk old-brk)
           ;; TODO: Release memory.
           (log/warning "TODO: release memory")
           ;; (*nix-process-brk-set! process brk)
           old-brk)
          ((fx>? (fx- brk (*nix-process-initial-brk process))
                 (* 2 1024 1024))
           old-brk)
          (else
           ;; Allocate memory.
           (*nix-process-brk-set! process brk)
           (do ((vaddr (align-up old-brk 4096) (fx+ vaddr 4096)))
               ((fx>=? vaddr brk)
                brk)
             (let ((paddr (allocate-page)))
               (page-table-map! pt vaddr paddr #b110 #t allocate-page)))))))

(define (linux-arch_prctl pt ustate code addr)
  (cond ((eqv? code ARCH_SET_FS)
         (cond ((fx<? -1 addr (expt 2 47))
                (let ((fsbase (get-mem-s61 (fx+ ustate USTATE:FSBASE))))
                  (log/debug "FSBASE is now #x" (number->string addr 16))
                  (put-mem-s61 (fx+ ustate USTATE:FSBASE) addr)
                  fsbase))
               (else (- EFAULT))))
        (else (- ENOSYS))))

(define (linux-uname pt ustate *buf)
  (define (putstr port string len)
    (let ((bv (string->utf8 (string-append string "\x0;"))))
      (put-bytevector port bv 0 (fxmin len (bytevector-length bv)))
      (let ((padlen (fx- len (bytevector-length bv))))
        (when (fx>? padlen 0)
          (put-bytevector port (make-bytevector padlen 0))))))
  (let ((umem (open-user-memory-output-port pt)))
    (guard (exn
            ((serious-condition? exn)
             (log/warning "uname is no good: " exn)
             (- EFAULT)))
      (set-port-position! umem *buf)
      (putstr umem "Linux" sizeof-new_utsname-sysname)
      (putstr umem "darkstar" sizeof-new_utsname-nodename)
      (putstr umem "5.10.0" sizeof-new_utsname-release)
      (putstr umem (string-append "Loko Scheme " (loko-version))
              sizeof-new_utsname-version)
      (putstr umem "x86_64" sizeof-new_utsname-machine)
      (putstr umem "(none)" sizeof-new_utsname-domainname)
      0)))

(define (linux-mmap pt ustate process addr length prot flags fd offset)
  (define fdesc (hashtable-ref (*nix-process-fds process) fd #f))
  (log/debug "mmap " (list 'addr (number->string addr 16)
                           'length length
                           'prot (number->string prot 16)
                           'flags (number->string flags 16)
                           'fd fd
                           'offset offset))
  (cond
    ((or (not (eqv? 0 (fxand offset 4095))) (fx<? offset 0))
     (log/warning "Invalid mmap offset: " offset)
     (- EINVAL))
    ((or (not (eqv? 0 (fxand addr 4095))) (fx<? addr 0))
     (log/warning "Invalid mmap address: " addr)
     (- EINVAL))
    ((or (not (eqv? 0 (fxand length 4095))) (fx<? length 0))
     (log/warning "Invalid mmap address: " addr)
     (- EINVAL))
    ((not (or (eqv? 0 (fxand prot MAP_ANONYMOUS))
              fdesc))
     (log/warning "mmap that is neither anonymous nor backed by a file")
     (- EBADFD))
    ((and (fxtest flags MAP_FIXED) (fx<? addr 8192))
     (- EINVAL))
    (else
     (let ((addr (if (fxtest flags MAP_FIXED)
                     addr
                     ;; TODO: use (runtime mmap)
                     (*nix-process-mmap-start process)))
           (flags (fxior (if (fxtest prot PROT_READ) PF-R 0)
                         (if (fxtest prot PROT_WRITE) PF-W 0)
                         (if (fxtest prot PROT_EXEC) PF-X 0))))
       (do ((vaddr addr (fx+ vaddr 4096))
            (offset offset (fx+ offset 4096))
            (end (fx+ addr length)))
           ((fx>=? vaddr end)
            (*nix-process-mmap-start-set! process (+ vaddr 4096)) ;guard page!
            (log/debug "mmap " (number->string addr 16) "-"
                       (number->string vaddr 16))
            addr)
         (let-values ([(&paddr^ _) (page-table-lookup pt vaddr #t)])
           (unless &paddr^
             (if fdesc
                 ((fdesc-mmap-helper fdesc) pt vaddr flags offset allocate-page)
                 (let ((paddr (allocate-page)))
                   (page-table-map! pt vaddr paddr flags #t allocate-page))))))))))

(define (linux-munmap pt ustate process *addr len)
  (log/warning "TODO: implement munmap properly")
  0)

(define (linux-openat pt ustate process dfd *pathname flags mode)
  (cond
    ((not (eqv? dfd AT_FDCWD))
     (- EINVAL))
    (else
     ;; FIXME: This should be relative to dfd.
     (let* ((fn (copy-pathname-from-user pt *pathname))
            (fn (if (and (>= (string-length fn) 1)
                         (not (eqv? (string-ref fn 0) #\/)))
                    (string-append "/" fn)
                    fn)))
       (guard (exn
               ((i/o-file-does-not-exist-error? exn) (- ENOENT))
               ((i/o-filename-error? exn) (- EINVAL)))
         (let ((fds (*nix-process-fds process))
               (fd (*nix-process-next-free-fd process))
               (port
                ;; TODO: Use the other file modes
                (cond ((equal? fn "/dev/valand")
                       (open-valand-fdesc (*nix-process-valand-display process)))
                      ((eqv? O_RDONLY (fxand flags O_ACCMODE))
                       (open-file-input-port fn))
                      ((eqv? O_WRONLY (fxand flags O_ACCMODE))
                       (open-file-output-port fn))
                      (else
                       (open-file-input/output-port fn)))))
           (hashtable-set! fds fd port)
           fd))))))

(define (linux-close pt ustate process fd)
  (let ((fdesc (hashtable-ref (*nix-process-fds process) fd #f)))
    (hashtable-delete! (*nix-process-fds process) fd)
    (cond ((not fdesc)
           (- EBADFD))
          ((port? fdesc)
           (close-port fdesc)
           0)
          (else
           (log/warning "TODO: Close wrapped ports")
           0))))

(define (linux-faccessat pt ustate process dfd *filename mode flags)
  (cond
    ((not (eqv? dfd AT_FDCWD))
     (- EINVAL))
    (else
     ;; FIXME: This should be relative to dfd.
     (let* ((fn (copy-pathname-from-user pt *filename))
            (fn (if (and (>= (string-length fn) 1)
                         (not (eqv? (string-ref fn 0) #\/)))
                    (string-append "/" fn)
                    fn)))
       (guard (exn ((i/o-filename-error? exn) (- EINVAL)))
         (cond ((file-exists? fn) 0)
               (else (- ENOENT))))))))

(define (linux-lseek pt ustate process fd offset whence)
  ;;(log/warning "seek " fd " " offset " " whence)
  (let ((fdesc (hashtable-ref (*nix-process-fds process) fd #f)))
    (cond ((not fdesc) (- EBADFD))
          ((channel? fdesc) (- ESPIPE))
          ((not (and (port-has-set-port-position!? fdesc)
                     (port-has-port-position? fdesc)))
           (- ESPIPE))
          (else
           (cond ((eqv? whence SEEK_SET)
                  (set-port-position! fdesc offset)
                  offset)
                 ((eqv? whence SEEK_CUR)
                  (let ((pos (+ offset (port-position fdesc))))
                    (set-port-position! fdesc pos)
                    pos))
                 (else
                  (log/warning "TODO: more seek modes: " whence)
                  (- EINVAL)))))))

(define (wrap-output-port output-port ch)
  (spawn-fiber (lambda ()
                 (define (read! bv start count)
                   (let ((resp-ch (make-channel)))
                     (put-message ch (list resp-ch bv start count))
                     (let ((n (get-message resp-ch)))
                       (if (eq? n 'fail)
                           (read! bv start count)
                           n))))
                 (let* ((pb (make-custom-binary-input-port "fd" read! #f #f #f))
                        (p (transcoded-port pb (native-transcoder))))
                   ;; FIXME: Not quite efficient, is it, and it
                   ;; doesn't work as an implementation of pipes. But
                   ;; this is used to get transcoding of UTF-8 when
                   ;; printing to the console.
                   (let lp ()
                     (put-char output-port (get-char p))
                     (lp))))))

;; XXX: This is converting from a textual input port to a binary input
;; port... slowly!
(define (wrap-input-port input-port ch)
  (spawn-fiber
   (lambda ()
     (let lp ((buf #f))
       (if (not buf)
           (let ((c (get-char input-port)))
             (if (eof-object? c)
                 (lp c)
                 (lp (string->utf8 (string c)))))
           (match (get-message ch)
             [(resp-ch count)
              (log/warning "req get " count " with buf" buf)
              (cond ((or (eof-object? buf)
                         (>= count (bytevector-length buf)))
                     (put-message resp-ch buf)
                     (lp #f))
                    (else
                     (let ((consumed (make-bytevector count))
                           (kept (make-bytevector (- (bytevector-length buf) buf))))
                       (bytevector-copy! buf 0 consumed 0 (bytevector-length consumed))
                       (bytevector-copy! buf count kept 0 (bytevector-length kept))
                       (put-message resp-ch buf)
                       (lp kept))))]))))))

(define (copy-from-user pt addr len)
  (let ((umem (open-user-memory-input-port pt)))
    (set-port-position! umem addr)
    (let ((bv (get-bytevector-n umem len)))
      (cond ((and (bytevector? bv) (eqv? (bytevector-length bv) len))
             bv)
            (else #f)))))

(define (copy-to-user pt addr bv)
  (let ((umem (open-user-memory-output-port pt)))
    (set-port-position! umem addr)
    (put-bytevector umem bv)
    (bytevector-length bv)))

(define (copy-pathname-from-user pt addr)
  (define PATH_MAX 4096)
  (let ((umem (open-user-memory-input-port pt)))
    (set-port-position! umem addr)
    (utf8->string
     (call-with-bytevector-output-port
       (lambda (p)
         (let lp ((c 0))
           (unless (eqv? c PATH_MAX)
             (let ((b (get-u8 umem)))
               (unless (eqv? b 0)
                 (put-u8 p b)
                 (lp (fx+ c 1)))))))))))

(define (open-valand-fdesc disp)
  (define VALAND_SIMPLE_SURFACE #x40087601)
  (define VALAND_DAMAGE_FULL_SURFACE #x40047602)
  (define app-surface #f)
  (define queue (make-queue))
  (define (valand-fdesc-kbd ev)
    (guard (exn
            (else (log/warning "Error when handling keyboard event: " exn)))
      (match ev
        [#(make/break page usage key mods _)
         (enqueue! queue
                   (pack "QSSlL 12x"
                         0 page usage
                         (if (char? key) (char->integer key) -1)
                         (if (eq? make/break 'make) 1 0)))]
        [x (log/warning "valand-fdesc: Unhandled keyboard event: " x)])))
  (define (valand-ioctl process req *arg)
    (cond ((eqv? req VALAND_SIMPLE_SURFACE)
           (cond (app-surface
                  (log/warning "The process already has a window")
                  (- EINVAL))
                 ((copy-from-user (*nix-process-page-table process)
                                  *arg (format-size "SSL"))
                  =>
                  (lambda (arg)
                    (let-values ([(resx resy format) (unpack "SSL" arg)])
                      (log/debug "New " resx "x" resy " window with format #x"
                                 (number->string format 16))
                      ;; FIXME: use the dimensions from the request
                      (let ((window (vl_display-allocate-user-surface disp format
                                                                      30 500
                                                                      resx resy)))
                        (vl_display-add-window-surface! disp window)
                        (vl_surface-draw-simple-decorations window)
                        (vl_surface-input-callbacks-set! window
                                                         #f
                                                         valand-fdesc-kbd)
                        (set! app-surface window)
                        0))))
                 (else
                  (log/warning "Bad VALAND_SIMPLE_SURFACE argument")
                  (- EINVAL))))
          ((eqv? req VALAND_DAMAGE_FULL_SURFACE)
           (cond (app-surface
                  (vl_surface-damage! app-surface)
                  0)
                 (else
                  (log/warning "The process does not have a window")
                  (- ENODEV))))
          (else
           (log/warning "Bad ioctl request: " (number->string req 16))
           (- EINVAL))))
  (define (valand-mmap-helper pt vaddr flags offset allocate-page)
    (cond ((not app-surface)
           (- ENOMEM))
          (else
           (let* ((buf (vl_surface-buffer app-surface))
                  (&data (vl_buffer-&data buf))
                  (size (vl_buffer-size buf)))
             (cond ((<= (+ offset 4096) size)
                    (page-table-map! pt vaddr (fx+ &data offset) flags #f allocate-page)
                    0)
                   (else (- ENOMEM)))))))
  (define (valand-read pt vaddr size)
    (cond ((not app-surface)
           0)
          ((not (eqv? size 32))
           (- EINVAL))
          ((not (queue-empty? queue))
           (copy-to-user pt vaddr (dequeue! queue)))
          (else
           (- EAGAIN))))
  (make-fdesc valand-read
              #f
              valand-ioctl
              valand-mmap-helper))

(define (linux-ioctl pt ustate process fd request &arg)
  (let ((fdesc (hashtable-ref (*nix-process-fds process) fd #f)))
    (cond ((and (fdesc? fdesc) (fdesc-ioctl fdesc))
           => (lambda (ioctl) (ioctl process request &arg)))
          ((channel? fdesc)
           ;; TODO: do everything
           (log/warning "TODO: ioctl")
           0)
          (else
           (- EBADFD)))))

(define (linux-newfstatat pt ustate process dirfd *pathname *statbuf flag)
  (cond
    ((fxtest flag AT_EMPTY_PATH)
     (cond
       ((hashtable-ref (*nix-process-fds process) dirfd #f) =>
        (lambda (fdesc)
          (let ((stat (make-bytevector sizeof-stat)))
            (let ((mode (cond ((channel? fdesc) (fxior S_IFCHR #o620))
                              ((fdesc? fdesc) (fxior S_IFCHR #o620))
                              (else S_IFREG)))
                  (rdev (cond ((channel? fdesc) #x8800)
                              ((fdesc? fdesc) #x8800)
                              (else 0)))
                  (nlink 1))
              (bytevector-u16-native-set! stat offsetof-stat-st_nlink nlink)
              (bytevector-u32-native-set! stat offsetof-stat-st_mode mode)
              (bytevector-u16-native-set! stat offsetof-stat-st_rdev rdev))
            (copy-to-user pt *statbuf stat)
            0)))
       (else (- EBADFD))))
    (else
     ;; TODO: more stuff
     (- ENOSYS))))

(define (u32->s32 x)
  (if (fx>? x (- (expt 2 31) 1))
      (fx- x (expt 2 32))
      x))

(define (linux-sigaltstack pt ustate process *uss *uoss)
  (unless (eqv? *uoss 0)
    (copy-to-user pt *uoss (pack "qQl" 0 0 0)))
  (cond
    ((eqv? *uss 0)
     0)
    (else
     (let ((uss (copy-from-user (*nix-process-page-table process)
                                *uss sizeof-sigaltstack)))
       (let-values ([(ss_sp ss_flags ss_size) (unpack "qQl" uss)])
         (cond ((fxtest SS_DISABLE ss_flags)
                (*nix-process-sigaltstack-set! process #f))
               ((not (eqv? 0 ss_flags))
                (- EINVAL))
               (else
                (*nix-process-sigaltstack-set! process (cons ss_sp ss_size))
                0)))))))

(define (linux-rt_sigprocmask pt ustate process how *set *oldset sigsetsize)
  (log/warning "TODO: rt_sigprocmask" (list how *set *oldset sigsetsize))
  0)

(define (linux-rt_sigaction pt ustate process signal *act *oact sigsetsize)
  (let ((sigactions (*nix-process-sigactions process)))
    (cond ((or (not (fx<? -1 signal (vector-length sigactions)))
               (eqv? signal SIGKILL)
               (eqv? signal SIGSTOP))
           (- EINVAL))
          ((fx<? sigsetsize 8)
           (- EINVAL))                  ;???
          ((eqv? *act 0)
           0)
          (else
           (unless (eqv? *oact 0)
             (copy-to-user pt *oact (vector-ref sigactions signal)))
           (let ((act (copy-from-user pt *act sizeof-sigaction)))
             (vector-set! sigactions signal act))
           0))))

(define (linux-timer_create pt ustate process clock *event-spec created-id)
  (log/warning "TODO: timer_create" (list clock *event-spec created-id))
  0)

(define (linux-timer_settime pt ustate process timer flags *new-setting *old-setting)
  (log/warning "TODO: timer_settime" (list timer flags *new-setting *old-setting))
  0)

(define (linux-fcntl pt ustate process fd cmd maybe-arg)
  (cond ((eqv? cmd F_GETFL)
         (fxior O_RDWR O_APPEND O_LARGEFILE))
        ((eqv? cmd F_SETFL)
         0)
        (else
         (- EINVAL))))

(define (linux-epoll_create1 pt ustate process flags)
  (let ((fds (*nix-process-fds process))
        (fd (*nix-process-next-free-fd process)))
    (log/warning "TODO: epoll_create1" (list flags))
    (hashtable-set! fds fd 'epoll-todo)
    fd))

(define (send-linux-signal process signo si_trapno si_code)
  (let* ((pt (*nix-process-page-table process))
         (ustate (*nix-process-ustate process))
         (ucontext (make-bytevector sizeof-ucontext 0))
         (info (make-bytevector sizeof-siginfo_t 0)))
    ;; TODO: use sa_mask
    (let-values ([(sa_handler sa_flags _sa_restorer sa_mask)
                  (unpack "qqqq" (vector-ref (*nix-process-sigactions process) signo))])
      (when (fxtest sa_flags SA_RESETHAND)
        (vector-set! (*nix-process-sigactions process) signo
                     (pack "qqqq" SIG_DFL 0 0 0)))
      ;; Allocate space on the stack for siginfo_t and ucontext.
      (let* ((sp (cond ((and (fxtest sa_flags SA_ONSTACK)
                             (*nix-process-sigaltstack process))
                        => (match-lambda
                            [(sp . size)
                             (bytevector-u64-native-set! ucontext offsetof-ucontext-uc_stack sp)
                             (fx+ sp size)]))
                       (else
                        ;; Current stack pointer, minus the "red zone"
                        (fxand (fx- (get-mem-s61 (fx+ ustate USTATE:RSP)) 128)
                               -8))))
             (sp (fx- sp sizeof-siginfo_t)) (*info sp)
             (sp (fx- sp sizeof-ucontext)) (*ucontext sp)
             (sp (fxand sp -16)))
        (bytevector-u32-native-set! info offsetof-siginfo_t-si_signo signo)
        (bytevector-u32-native-set! info offsetof-siginfo_t-si_trapno si_trapno)
        (bytevector-u32-native-set! info offsetof-siginfo_t-si_code si_code)

        (bytevector-u64-native-set! ucontext offsetof-ucontext-uc_flags 0)
        #;
        (bytevector-u64-native-set! ucontext offsetof-ucontext-uc_sigmask sigmask)

        ;; TODO: what about SSE state?
        (letrec ((copy!
                  (lambda (mcontext-offset ustate-offset)
                    (bytevector-s64-native-set! ucontext
                                                (fx+ offsetof-ucontext-uc_mcontext mcontext-offset)
                                                (get-mem-s61 (fx+ ustate ustate-offset))))))
          (copy! offsetof-sigcontext_64-r8 USTATE:R8)
          (copy! offsetof-sigcontext_64-r9 USTATE:R9)
          (copy! offsetof-sigcontext_64-r10 USTATE:R10)
          (copy! offsetof-sigcontext_64-r11 USTATE:R11)
          (copy! offsetof-sigcontext_64-r12 USTATE:R12)
          (copy! offsetof-sigcontext_64-r13 USTATE:R13)
          (copy! offsetof-sigcontext_64-r14 USTATE:R14)
          (copy! offsetof-sigcontext_64-r15 USTATE:R15)
          (copy! offsetof-sigcontext_64-di USTATE:RDI)
          (copy! offsetof-sigcontext_64-si USTATE:RSI)
          (copy! offsetof-sigcontext_64-bp USTATE:RBP)
          (copy! offsetof-sigcontext_64-bx USTATE:RBX)
          (copy! offsetof-sigcontext_64-dx USTATE:RDX)
          (copy! offsetof-sigcontext_64-ax USTATE:RAX)
          (copy! offsetof-sigcontext_64-cx USTATE:RCX)
          (copy! offsetof-sigcontext_64-sp USTATE:RSP)
          (copy! offsetof-sigcontext_64-ip USTATE:RIP)
          (copy! offsetof-sigcontext_64-flags USTATE:RFLAGS)
          #;
          offsetof-sigcontext_64-err
          (copy! offsetof-sigcontext_64-trapno USTATE:FAULT-NUMBER)
          (when (eqv? si_trapno 14)
            (copy! offsetof-sigcontext_64-cr2 USTATE:CR2)))

        (copy-to-user pt *info info)
        (copy-to-user pt *ucontext ucontext)

        ;; Invoke the signal handler
        (put-mem-s61 (fx+ ustate USTATE:RDI) signo)
        (put-mem-s61 (fx+ ustate USTATE:RSI) *info)
        (put-mem-s61 (fx+ ustate USTATE:RDX) *ucontext)
        (put-mem-s61 (fx+ ustate USTATE:RSP) sp)
        (put-mem-s61 (fx+ ustate USTATE:RIP) sa_handler)

        #f))))

(define (handle-linux-process pid pt ustate fds valand-display)
  ;; FIXME: Find the program break automatically. On the first brk()
  ;; call, scan below the stack. Or something. It's not well defined
  ;; and even Linux gives weird results when Loko calls brk().
  (define process
    (let ((brk #x200000000)
          (mmap-start #x3000000000)
          (sigactions (make-vector 32 (pack "qqqq" SIG_DFL 0 0 0)))
          (sigaltstack #f))
      (make-*nix-process pid pt ustate brk brk fds mmap-start valand-display
                         sigactions sigaltstack)))
  (define (loop)
    (match (perform-operation (wait-process-operation pid))
      ('syscall
       ;; Syscall arguments are: rdi rsi rdx r10 r8 r9
       (let ((rax (get-mem-s61 (fx+ ustate USTATE:RAX)))
             (arg0 (get-mem-s61 (fx+ ustate USTATE:RDI)))
             (arg1 (get-mem-s61 (fx+ ustate USTATE:RSI)))
             (arg2 (get-mem-s61 (fx+ ustate USTATE:RDX)))
             (arg3 (get-mem-s61 (fx+ ustate USTATE:R10))))
         (define __NR_brk 12)
         (define __NR_writev 20)
         (define __NR_nanosleep 35)
         (define __NR_clock_nanosleep 230)
         (log/debug (list 'syscall rax
                          (number->string arg0 16)
                          (number->string arg1 16)
                          (number->string arg2 16)
                          (number->string arg3 16)))
         (cond
           ((eqv? rax __NR_clock_gettime)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-clock_gettime pt ustate process arg0 arg1))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_read)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-read pt ustate (hashtable-ref fds arg0 #f)
                                     arg1 arg2))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_write)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-write pt ustate (hashtable-ref fds arg0 #f)
                                      arg1 arg2))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_writev)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-writev pt ustate (hashtable-ref fds arg0 #f)
                                       arg1 arg2))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_brk)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-brk pt ustate process arg0))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_mmap)
            (let ((addr arg0)
                  (length arg1)
                  (prot arg2)
                  (flags arg3)
                  (fd (u32->s32 (get-mem-s61 (fx+ ustate USTATE:R8))))
                  (offset (get-mem-s61 (fx+ ustate USTATE:R9))))
              (put-mem-s61 (fx+ ustate USTATE:RAX)
                           (linux-mmap pt ustate process addr length prot flags fd offset))
              (process-resume pid)
              (loop)))

           ((eqv? rax __NR_munmap)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-munmap pt ustate process arg0 arg1))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_nanosleep)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-nanosleep pt ustate arg0 arg1))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_clock_nanosleep)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-clock_nanosleep pt ustate arg0 arg1 arg2 arg3))
            (process-resume pid)
            (loop))

           ((or (eqv? rax __NR_exit)
                (eqv? rax __NR_exit_group))
            arg0)

           ((eqv? rax __NR_arch_prctl)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-arch_prctl pt ustate arg0 arg1))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_uname)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-uname pt ustate arg0))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_mprotect)
            (log/warning "Pretending to implement mprotect")
            (put-mem-s61 (fx+ ustate USTATE:RAX) 0)
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_openat)
            (let ((dfd (u32->s32 arg0)))
              (put-mem-s61 (fx+ ustate USTATE:RAX)
                           (linux-openat pt ustate process dfd arg1 arg2 arg3)))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_close)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-close pt ustate process arg0))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_lseek)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-lseek pt ustate process arg0 arg1 arg2))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_ioctl)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-ioctl pt ustate process arg0 arg1 arg2))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_newfstatat)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-newfstatat pt ustate process arg0 arg1 arg2 arg3))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_sigaltstack)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-sigaltstack pt ustate process arg0 arg1))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_rt_sigprocmask)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-rt_sigprocmask pt ustate process arg0 arg1 arg2 arg3))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_rt_sigaction)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-rt_sigaction pt ustate process arg0 arg1 arg2 arg3))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_rt_sigreturn)
            (log/error "FIXME: rt_sigreturn")
            #f
            ;; (put-mem-s61 (fx+ ustate USTATE:RAX) (linux-rt_sigreturn pt ustate process))
            ;; (process-resume pid)
            ;; (loop)
            )

           ((eqv? rax __NR_timer_create)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-timer_create pt ustate process arg0 arg1 arg2))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_timer_settime)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-timer_settime pt ustate process arg0 arg1 arg2 arg3))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_fcntl)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-fcntl pt ustate process arg0 arg1 arg2))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_epoll_create1)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-epoll_create1 pt ustate process arg0))
            (process-resume pid)
            (loop))

           ((eqv? rax __NR_faccessat)
            (put-mem-s61 (fx+ ustate USTATE:RAX)
                         (linux-faccessat pt ustate process arg0 arg1 arg2 arg3))
            (process-resume pid)
            (loop))

           (else
            (log/warning "Unimplemented Linux syscall " rax)
            (put-mem-s61 (fx+ ustate USTATE:RAX) (- ENOSYS))
            (process-resume pid)
            (loop)))))

      ('trap
       (let ((fault (get-mem-s61 (fx+ ustate USTATE:FAULT-NUMBER))))
         (cond ((eqv? fault 6)          ;#UD
                (send-linux-signal process SIGILL fault (fxior SI_KERNEL ILL_ILLOPN))
                (process-resume pid)
                (loop))
               ((eqv? fault 14)         ;#PF
                (let ((cr2 (get-mem-s61 (fx+ ustate USTATE:CR2))))
                  (log/warning "Page fault at"
                               " RIP=#x" (number->string
                                          (get-mem-s61 (fx+ ustate USTATE:RIP)) 16)
                               " code=#b" (number->string
                                           (get-mem-s61 (fx+ ustate USTATE:FAULT-CODE))
                                           2)
                               " CR2=#x" (number->string cr2 16)))
                (send-linux-signal process SIGSEGV fault (fxior SI_KERNEL SEGV_ACCERR))
                (process-resume pid)
                (loop))
               ((eqv? fault 17)         ;#AC
                (send-linux-signal process SIGBUS fault (fxior SI_KERNEL BUS_ADRALN))
                (process-resume pid)
                (loop))
               (else
                (log/critical "Traps are not implemented yet. "
                              (list 'rip (number->string
                                          (get-mem-s61 (fx+ ustate USTATE:RIP)) 16)
                                    'fault fault
                                    'code (get-mem-s61 (fx+ ustate USTATE:FAULT-CODE))))
                #f))))

      (x
       (log/critical "Unknown process message: " x)
       #f)))

  (let ((status (guard (exn
                        ((serious-condition? exn)
                         (send-log CRITICAL "Exception while handling process"
                                   'EXCEPTION exn)
                         #f))
                  (loop))))
    ;; FIXME: mark the window as dead
    (log/debug "exit status: " status)
    (process-exit pid status)
    (page-table-free! pt)
    ($free-ustate ustate)))

(define (spawn-linux-process path command-line environment valand-display)
  (define fds (make-eqv-hashtable))
  (define auxv
    `((,AT_PAGESZ . 4096)
      (,AT_PLATFORM . ,(string->utf8 "x86_64"))
      (,AT_EXECFN . ,(string->utf8 path))
      (,AT_FLAGS . 0)
      (,AT_UID . 0)
      (,AT_EUID . 0)
      (,AT_GID . 0)
      (,AT_EGID . 0)
      (,AT_RANDOM . ,(let ((bv (make-bytevector 16)))
                       (get-random-bytevector-n! bv 0 16)
                       bv))))
  (let ((stdin (make-channel))
        (stdout (make-channel))
        (stderr (make-channel)))
    (hashtable-set! fds 0 stdin)
    (hashtable-set! fds 1 stdout)
    (hashtable-set! fds 2 stderr)
    (wrap-input-port (current-input-port) stdin)
    (wrap-output-port (current-output-port) stdout)
    (wrap-output-port (current-error-port) stderr))
  (let-values ([(pt ustate) (binfmt-elf-load path command-line environment auxv)])
    (define pid (new-usermode-process ustate))
    (spawn-fiber
     (lambda ()
       (parameterize ([current-log-fields
                       (append `(_PID ,(pid-id pid) _EXE ,path)
                               (current-log-fields))])
         (handle-linux-process pid pt ustate fds valand-display))))
    pid)))
