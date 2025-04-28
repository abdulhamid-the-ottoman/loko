;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2020 G. Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Utilities

;; Make a NULL-terminated array of pointers to bytevectors. This must
;; be wrapped in a loop that ensures that (collections) has the same
;; value before this call as immediately before the pointers are used.
(define (bytevector-addresses bv*)
  (sint-list->bytevector (append (map bytevector-address bv*) '(0))
                         (native-endianness)
                         sizeof-void*))

;; Convert an alist to a list of NUL-terminated name=value bytevectors.
(define (env-alist->bytevectors alist)
  (map (lambda (entry)
         (call-with-bytevector-output-port
           (lambda (p)
             (if (bytevector? (car entry))
                 (put-bytevector p (car entry))
                 (put-bytevector p (string->utf8 (car entry))))
             (put-u8 p (char->integer #\=))
             (put-bytevector p (string->utf8z (cdr entry))))))
       alist))

;; Split a string on a character
(define (string-split str c)
  (let lp ((start 0) (end 0))
    (cond ((fx=? end (string-length str))
           (list (substring str start end)))
          ((char=? c (string-ref str end))
           (cons (substring str start end)
                 (lp (fx+ end 1) (fx+ end 1))))
          (else
           (lp start (fx+ end 1))))))

;; Convert a string to a NUL-terminated bytevector
(define (string->utf8z val)
  (let lp ((x val))
    (cond
      ((string? x)
       (lp (string->utf8 x)))
      ((bytevector? x)
       (call-with-bytevector-output-port
         (lambda (p)
           (do ((i 0 (fx+ i 1)))
               ((fx=? i (bytevector-length x)))
             (when (eqv? 0 (bytevector-u8-ref x i))
               (assertion-violation 'string->utf8z "Cannot represent NUL" val)))
           (put-bytevector p x)
           (put-u8 p (char->integer #\nul)))))
      (else
       (assertion-violation 'string->utf8z "Expected a string or bytevector" val)))))

;;; Constructors

(define-record-type (process mkprocess process?)
  (fields child-id wait-ch (mutable code) (mutable status))
  (protocol
   (lambda (mk)
     (lambda (pid wait-ch)
       (mk pid wait-ch 0 0)))))

;; Open a pipe, returning the read and write ends.
(define (make-pipe)
  (let ((buf (make-bytevector (* 2 sizeof-int))))
    (sys_pipe2 (bytevector-address buf) (fxior O_CLOEXEC O_NONBLOCK))
    (values (fd->port (bytevector-u32-native-ref buf 0) binary-input)
            (fd->port (bytevector-u32-native-ref buf 4) binary-output))))

(define (make-textual-pipe)
  (let-values (((in out) (make-pipe)))
    (let ((tc (native-transcoder)))
      (values (transcoded-port in tc) (transcoded-port out tc)))))

(define (child-handle-fds setup ports)
  ;; FDs to keep open
  (for-each
   (lambda (fd)
     (let ((prev (sys_fcntl fd F_GETFL 0)))
       (unless (eqv? 0 (fxand O_CLOEXEC prev))
         (sys_fcntl fd F_SETFL (fxand (fxnot O_CLOEXEC) prev)))))
   (setup-ref setup 'open-fds '()))
  ;; Arrange the ports on their assigned fds
  ;; TODO: dup pre-pass so fds don't step on each other
  (for-each
   (match-lambda
    ((child-fd . '#t)
     ;; Should be the same as in the parent process
     #f)
    ((child-fd . '#f)
     (sys_close child-fd))
    ((child-fd . 'null)
     (let* ((null (open-file-input/output-port "/dev/null" (file-options no-fail)))
            (fd (port-file-descriptor null)))
       (sys_dup3 fd child-fd 0)))
    ((child-fd . port)
     (let ((parent-fd (port-file-descriptor port)))
       (unless (= child-fd parent-fd)
         (sys_dup3 parent-fd child-fd 0)))))
   ports))

(define (make-process setup cmd . args)
  (let-values (((errin errout) (make-textual-pipe)))
    (let ((ports (append (list (cons 0 (setup-ref setup 'stdin #t))
                               (cons 1 (setup-ref setup 'stdout #t))
                               (cons 2 (setup-ref setup 'stderr #t)))
                         (cond ((setup-ref setup 'stdout+stderr #f) =>
                                (lambda (port)
                                  (list (cons 1 (setup-ref setup 'stdout port))
                                        (cons 2 (setup-ref setup 'stderr port)))))
                               (else '()))
                         (setup-ports setup))))
      (setup-validate setup ports cmd args)
      (let ((proc (fork)))
        (cond
          ((not proc)                   ;child process
           (guard (exn
                   ((syscall-errno-condition? exn)
                    ;; Propagate the error to the parent
                    (write (list (condition-syscall-function exn)
                                 (condition-syscall-errno exn))
                           errout)
                    (close-port errout)
                    (sys_exit EX_OSERR))
                   (else
                    (sys_exit EX_SOFTWARE)))
             ;; Put all fds in their right places
             (child-handle-fds setup ports)
             ;; Process group + session
             (match (setup-ref setup 'group #f)
               ('new             (sys_setpgid 0 (sys_getpid)))
               ('new-session     (sys_setsid))
               ((? fixnum? pgid) (sys_setpgid 0 pgid))
               (#f               #f))
             ;; Exec + propagate errno
             (apply process-exec* setup cmd args))
           (sys_exit EX_SOFTWARE))
          (else                         ;parent process
           (close-port errout)
           (let ((err (read errin)))
             (close-port errin)
             ;; XXX: is this permitted by the spec or should the
             ;; process crash only be seen through wait?
             (match err
               ((function errno)
                (raise           ;error smuggled in from child
                  (condition
                   (make-process-exception)
                   (make-who-condition 'make-process)
                   (make-message-condition "Failed to exec program")
                   (make-irritants-condition (cons* setup cmd args))
                   (make-syscall-error function errno))))
               (else
                (if (setup-ref setup 'wait #f)
                    (process-wait proc)
                    proc))))))))))

;;; The setup dictionary

(define (setup-ref plist key default)
  (let lp ((plist plist))
    (cond ((null? plist) default)
          ((eq? key (car plist)) (cadr plist))
          (else (lp (cddr plist))))))

(define (setup-ports plist)
  (let lp ((plist plist))
    (cond ((null? plist) '())
          ((fixnum? (car plist))
           (cons (cons (car plist) (cadr plist))
                 (lp (cddr plist))))
          (else (lp (cddr plist))))))

(define (setup-validate setup ports cmd args)
  (for-each
   (match-lambda
    ((or (child-fd . '#t) (child-fd . '#f) (child-fd . 'null))
     #f)
    ((child-fd . port)
     (unless (fixnum? (port-file-descriptor port))
       (assertion-violation 'make-process "No fd connected with this port"
                            port)))
    (_ (assertion-violation 'make-process "Invalid port in setup" setup cmd args)))
   ports))

;;; Synthetic process objects

(define (pid->proc pid)
  (mkprocess pid #f))

;;; Process object predicates

(define (synthetic-process? obj)
  (and (process? obj)
       (not (channel? (process-wait-ch obj)))))

;;; Process object accessors

;; process-child-id

(define (process-child-group proc)
  (sys_getpgid (process-child-id proc)))

(define (process-child-session proc)
  (sys_getsid (process-child-id proc)))

;; XXX: process-exited? is missing

(define (process-terminated? proc)      ;WIFSIGNALED
  (when (synthetic-process? proc)
    (error 'process-terminated? "Not implemented for synthetic processes" proc))
  (let ((code (process-code proc)))
    (or (eqv? code CLD_KILLED)
        (eqv? code CLD_DUMPED))))

(define (process-stopped? proc)         ;WIFSTOPPED
  (when (synthetic-process? proc)
    (error 'process-stopped? "Not implemented for synthetic processes" proc))
  (let ((code (process-code proc)))
    (eqv? code CLD_STOPPED)))

(define (process-exit-code proc)        ;WEXITSTATUS
  (when (synthetic-process? proc)
    (error 'process-exit-code "Not implemented for synthetic processes" proc))
  (let ((code (process-code proc)))
    (and (eqv? code CLD_EXITED)
         (process-status proc))))

(define (process-stop-signal proc)      ;WSTOPSIG
  (when (synthetic-process? proc)
    (error 'process-stop-signal "Not implemented for synthetic processes" proc))
  (let ((code (process-code proc)))
    (and (eqv? code CLD_STOPPED)
         (process-status proc))))

(define (process-terminate-signal proc) ;WTERMSIG
  (when (synthetic-process? proc)
    (error 'process-terminate-signal "Not implemented for synthetic processes" proc))
  (let ((code (process-code proc)))
    (and (or (eqv? code CLD_KILLED)
             (eqv? code CLD_DUMPED))
         (process-status proc))))

;;; Process termination procedures

(define chld-sub-ch (make-channel))

(define (sigchld-subscribe pid)
  (let ((ch (make-channel)))
    (put-message chld-sub-ch (cons pid ch))
    ch))

(define (sys-wait idtype id)
  (let* ((si (make-bytevector sizeof-siginfo_t 0))
         (ru (make-bytevector sizeof-rusage 0))
         (options (fxior WNOHANG WEXITED WUNTRACED WCONTINUED __WALL __WNOTHREAD)))
    (and (let retry ()
           (sys_waitid idtype id (bytevector-address si) options
                       (bytevector-address ru)
                       (lambda (errno)
                         (cond ((eqv? errno EINTR) (retry))
                               ((eqv? errno ECHILD) #f)
                               (else
                                (raise
                                  (condition
                                   (make-process-exception)
                                   (make-who-condition 'linux-listen-signals)
                                   (make-syscall-error 'waitid errno))))))))
         (let ((pid (bytevector-u32-native-ref si offsetof-siginfo_t-si_pid))
               (code (bytevector-s32-native-ref si offsetof-siginfo_t-si_code))
               (uid (bytevector-u32-native-ref si offsetof-siginfo_t-si_uid))
               (status (bytevector-s32-native-ref si offsetof-siginfo_t-si_status)))
           (list (cons 'status status) (cons 'pid pid)
                 (cons 'code code) (cons 'uid uid))))))

(define (sigchld-fiber)
  (define children (make-eqv-hashtable))
  (enable-signal SIGCHLD)
  (do () (#f)
    (match (perform-operation
            (choice-operation (wrap-operation (wait-signal-operation SIGCHLD)
                                              (lambda _ 'SIGCHLD))
                              (wrap-operation (get-operation chld-sub-ch)
                                              (lambda (x) (cons 'sub x)))))
      ('SIGCHLD
       (let loop ()
         (cond
           ((sys-wait P_ALL -1) =>
            (lambda (info)
              (let ((pid (cdr (assq 'pid info)))
                    (code (cdr (assq 'code info))))
                ;; Notify everyone who was interested in this child
                (for-each (lambda (ch) (spawn-fiber (lambda () (put-message ch info))))
                          (hashtable-ref children pid '()))
                (when (or (eqv? code CLD_EXITED) (eqv? code CLD_KILLED) (eqv? code CLD_DUMPED))
                  ;; The child is no more
                  (hashtable-delete! children pid)))
              (loop)))))
       (acknowledge-signal SIGCHLD))
      (('sub . (pid . ch))
       (hashtable-update! children pid (lambda (ch*) (cons ch ch*)) '())))))

(define process-wait
  (case-lambda
    ((proc)
     (process-wait proc #f))
    ((proc stopped?)
     (let lp ()
       (let ((msg (get-message (process-wait-ch proc))))
         (let ((code (cdr (assq 'code msg)))
               (status (cdr (assq 'status msg))))
           (process-code-set! proc code)
           (process-status-set! proc status)
           (cond
             ((or (eqv? code CLD_EXITED) (eqv? code CLD_KILLED) (eqv? code CLD_DUMPED))
              proc)
             ((and stopped? (eqv? code CLD_STOPPED)))
             (else
              ;; The child is still alive, but it stopped or continued
              (lp)))))))))

(define process-wait-any
  (case-lambda
    ((proc)
     (process-wait-any proc #f))
    ((proc stopped?)
     (error 'process-wait-any "Not yet implemented"))))

(define process-wait-group
  (case-lambda
    ((proc)
     (process-wait-any proc #f))
    ((proc stopped?)
     (error 'process-wait-group "Not yet implemented"))))

;;; Sending signals to processes

(define (process-terminate proc)
  (process-send-signal proc SIGTERM))

(define (process-send-signal proc signo)
  (sys_kill (process-child-id proc) signo))

(define (process-send-group-signal proc/pgid signo)
  (let ((pid (if (fixnum? proc/pgid) proc/pgid (process-child-id proc/pgid))))
    (sys_kill (fx- pid) signo)))

;;; Fork and exec

(define process-fork
  (case-lambda
    (()
     (fork))
    ((thunk)
     ;; XXX: What should happen if thunk raises an exception?
     (or (fork)
         (guard (exn (else (sys_exit 70)))
           (sys_exit (thunk)))))))

;; Replace the current program image with the one from prog. The arg*
;; and env* arguments are lists of bytevectors.
(define (execve cmd arg* env*)
  (let retry ()
    (let ((gc0 (collections)))
      (let* ((%arg (bytevector-addresses arg*))
             (%env (bytevector-addresses env*)))
        (cond ((not (= gc0 (collections)))
               (retry))
              (else
               (sys_execve (bytevector-address cmd)
                           (bytevector-address %arg)
                           (bytevector-address %env))))))))

(define (fork)
  (let ((pid (sys_fork)))
    (if (eqv? pid 0)
        #f                              ;child process
        (mkprocess pid (sigchld-subscribe pid)))))

(define (path-join dir fn)
  (cond ((path-absolute? fn)
         fn)
        ((string=? dir "")
         fn)
        ((char=? (string-ref dir (fx- (string-length dir) 1)) #\/)
         (string-append dir fn))
        (else
         (string-append dir "/" fn))))

(define (path-absolute? str)
  (and (fx>? (string-length str) 0)
       (char=? (string-ref str 0) #\/)))

(define (process-exec* setup cmd . args)
  ;; XXX: document says to use closed-fds
  (let ((arg0 (setup-ref setup 'arg0 cmd))
        (env (or (setup-ref setup 'env #f) (get-environment-variables))))
    (let ((args^ (map string->utf8z (cons arg0 args)))
          (env^ (env-alist->bytevectors env)))
      (if (and (setup-ref setup 'path '#f) (not (path-absolute? cmd)))
          (let ((dirs (cond ((get-environment-variable "PATH")
                             => (lambda (s) (string-split s #\:)))
                            (else '("")))))
            (let lp ((dirs dirs))
              (guard (exn
                      ((and (not (null? (cdr dirs)))
                            (syscall-errno-condition? exn))
                       (lp (cdr dirs))))
                (let ((dir (car dirs)))
                  (execve (string->utf8z (path-join dir cmd)) args^ env^)))))
          (execve (string->utf8z cmd) args^ env^)))))

(define (process-exec setup cmd . args)
  (let ((errno (apply process-exec* setup cmd args)))
    (raise
      (condition
       (make-process-exception)
       (make-syscall-error 'execve errno)
       (make-irritants-condition (list setup cmd args))))))

;;; Exceptions

;; TODO: actually use this everywhere
(define-condition-type &process-exception &serious
  make-process-exception process-exception?)

(define process-exception-errno condition-syscall-errno)

(define process-exception-message condition-message)

;;; XXX: not in the document

(define (get-process-id)
  (sys_getpid))
