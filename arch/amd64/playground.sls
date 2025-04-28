;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Playground for the standard library.

;; This is stuff that should be improved and moved elsewhere.

(library (loko arch amd64 playground)
  (export
    time-it time-it*
    collections
    disassemble
    stack-trace
    valgrind
    cache-line-size)
  (import
    (rnrs (6))
    (rnrs mutable-strings (6))
    (loko system time)
    (loko system unsafe)
    (only (loko runtime pretty) pretty-print-simple)
    (only (loko system $x86) rdtsc cpuid)
    (only (loko system $primitives) $box? $box-ref $box-type
          $object->fixnum
          $procedure-info $procedure-length $procedure-ref
          $pcb-ref
          $heap-remaining
          $valgrind)
    (loko match)
    (loko arch amd64 disassembler)
    (only (loko runtime context)
          PROCESS-VECTOR:STACK-TOP
          PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT))

(define (collections)
  ($pcb-ref PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT))

(define (time-it what thunk)
  (define (print . x) (for-each display x) (newline))
  (define (time->µs t)
    (+ (div (time-nanosecond t) (expt 10 3))
       (* (time-second t) (expt 10 (- 9 3)))))
  ;; Number of garbage collections, elapsed CPU and real time, time
  ;; spent in the garbage collector, bytes allocated.
  (let* ((t0 (current-time/process))
         (t1 (current-time/process))
         (t2 (current-time/process))
         (t3 (current-time/process))
         (_ (collections))
         (_ ($heap-remaining))
         ;; TODO: check if this is right by averaging even more
         ;; numbers and see if this is a good approximation
         (overhead (div (- (time->µs t3) (time->µs t0)) 4)))
    (let* ((gc0 (collections))
           (time0 (current-time/process))
           (hr0 ($heap-remaining))
           (tsc0 (rdtsc 'start)))
      (let ((ret (thunk)))
        (let* ((tsc1 (rdtsc 'stop))
               (hr1 ($heap-remaining))
               (time1 (current-time/process))
               (gc1 (collections)))
          (print "Timings for " what ":")
          (print "  " (- gc1 gc0) " garbage collection runs")
          (print "  " (- (time->µs time1) (time->µs time0) overhead)
                 " µs elapsed process time")
          (print "  " (- tsc1 tsc0) " elapsed processor cycles")
          ;; hr0-hr1 is accurate if there were no collections
          ;; If there were collections then take ...
          (when (= gc0 gc1)
            (print "  " (div (- hr0 hr1) 8) " Q allocated")))
        ret))))

(define (time-it* what iterations thunk)
  ;; A good reference on this is "How to Benchmark Code Execution
  ;; Times on Intel® IA-32 and IA-64 Instruction Set Architectures
  ;; (324264-001)." This should preferably run without interrupts.
  ;;
  ;; Under Linux the thread should be pinned to a CPU (see taskset).
  ;;
  ;; Any sort of power optimizations and core boosting should really
  ;; be turned off.
  ;;
  ;; Recent CPUs have a lower-resolution TSC. Increments are either 1
  ;; or 20—36. See Appendix A of "Take A Way: Exploring the Security
  ;; Implications of AMD's Cache Way Predictors" (Lipp, et al.) 2020.
  ;; 10.1145/3320269.3384746.
  ;; <https://hal.inria.fr/hal-02866777/document>.
  ;;
  ;; TODO: add a complexity count, as in the paper?
  ;; TODO: read the cycle count with RDPMC.
  (define-syntax tsc-resolution (identifier-syntax 100))
  (define (print . x) (for-each display x) (newline))
  (define (fmt v)
    (let* ((s (number->string (exact (round (* v 100)))))
           (n (string-length s)))
      (if (< n 2)
          (string-append "." s)
          (string-append (substring s 0 (fx- n 2)) "."
                         (substring s (fx- n 2) n)))))
  (assert (fxpositive? iterations))
  (print "Timing " what " to find the minimum cycle time:")

  (do ((i 0 (fx+ i 1)))
      ((fx=? i 3))
    ;; Warm up the code.
    (thunk))
  (let ((iterations (fxdiv (fx+ iterations (fx- tsc-resolution 1)) tsc-resolution)))
    (let lp ((least (greatest-fixnum))
             (greatest 0)
             (i iterations)
             (n 0)
             (sum 0)
             (sumsq 0))
      (cond ((fxzero? i)
             (let* ((µ (/ sum n))
                    (σ² (/ (- sumsq (/ (* sum sum) n))
                           n))          ;For s² use (/ ... (- n 1))
                    (σ (sqrt σ²)))
               ;; We use the population variance, because the whole
               ;; population was sampled. Outliers were discarded, but
               ;; they are not wanted anyway.
               (print "\n  The cycle count varied between " least " and " greatest)
               (print "  (Arithmetic mean)      µ  = " (fmt µ))
               (print "  (Standard deviation)   σ  = " (fmt σ))
               (print "  (Population variance)  σ² = " (fmt σ²))
               (display "                    min x_i = µ")
               (unless (eqv? σ 0)
                 (let ((devs (/ (- least µ) σ)))
                   (cond ((positive? devs) (print "+" (fmt devs) "σ"))
                         ((negative? devs) (print (fmt devs) "σ")))
                   (print "  Used " n " samples ("
                          (- iterations n) " outliers discarded).")
                   (when (> (abs devs) 1)
                     (print "\n  You might have a bad test setup. Disable frequency scaling, etc.\n"
                            "  Do not simply rerun the test until this message disappears.\n"))))
               least))
            (else
             (let* ((gc0 (collections))
                    (tsc0 (rdtsc 'start)))
               (let-syntax ((repeat
                             (lambda (x)
                               (syntax-case x ()
                                 ((_ expr)
                                  (with-syntax (((r ...) (vector->list (make-vector tsc-resolution #'expr))))
                                    #'(begin r ...)))))))
                 (repeat (thunk)))
               (let* ((tsc1 (rdtsc 'stop))
                      (cycles (fxdiv (fx- tsc1 tsc0) tsc-resolution))
                      (gc1 (collections)))
                 (assert (fx>=? cycles 0))
                 (when (fx<? cycles least)
                   (print "New minimum is " cycles " cycles with " i " iterations to go."))
                 (let ((least (fxmin cycles least))
                       (greatest (fxmax cycles greatest)))
                   ;; Discard outliers. Doing this seems questionable.
                   (if (or (not (= gc0 gc1))
                           (fx>? (fxarithmetic-shift-right cycles 1) least))
                       (lp least greatest (fx- i 1) n sum sumsq)
                       (lp least greatest (fx- i 1)
                           (fx+ n 1)
                           (+ sum cycles)
                           (+ sumsq (* cycles cycles))))))))))))

;; XXX: this should basically be like fcdisasm, but better. It
;; should be able to mark the "current" instruction. It should
;; insert labels for local branches. It should look up branch
;; destinations in the symbol table. It should translate
;; immediates into objects. It should translate comparisons into
;; predicates. It should translate memory references.
(define (disassemble proc)
  (define (print . x) (for-each display x) (newline))
  (define-record-type info
    (sealed #t)
    (nongenerative loko-procinfo-555bfd14-d155-420f-a058-8ccd8ab0301e)
    (fields name (mutable free-length) source label end-label))
  (define (copy-code x)
    (let* ((info ($procedure-info x))
           (label (info-label info))
           (size (fx- (info-end-label info) label)))
      (do ((bv (make-bytevector size))
           (addr label (fx+ addr 1))
           (i 0 (fx+ i 1)))
          ((fx=? i size) bv)
        (bytevector-u8-set! bv i (get-mem-u8 addr)))))
  (define (get-instructions p)
    (let lp ((rip- 0))
      (let ((bytes '()))
        (let* ((ins (disassemble1 p (lambda x (set! bytes (cons x bytes)))))
               (rip+ (port-position p)))
          (if (eof-object? ins)
              '()
              (cons (cons* rip- rip+ (reverse bytes) ins)
                    (lp rip+)))))))
  (define (recover-local-labels ins*)
    (define last-rip (caar (reverse ins*)))
    (define addrs (make-eqv-hashtable))
    ;; Build a hashtable of branch targets.
    (let lp ((ins* ins*) (i 0))
      (match ins*
        ;; XXX: not very clever about what is a branch.
        [((rip- rip+ bytes . (J ('+ 'rip disp))) . ins*)
         (let ((target (+ rip+ disp)))
           (cond ((fx<=? 0 target last-rip)
                  (let ((l (vector i)))
                    ;; TODO: have the label numbers incrementing
                    (hashtable-set! addrs target l)
                    (lp ins* (fx+ i 1))))
                 (else (lp ins* i))))]
        [(_ . ins*) (lp ins* i)]
        [() #f]))
    ;; Now insert label declarations and find the maximum size (w)
    ;; of an instruction (used later when printing).
    (let lp ((ins* ins*) (w 0) (ret* '()))
      (match ins*
        [((rip- rip+ bytes . ins) . ins*)
         (let ((ret* (cond ((hashtable-ref addrs rip- #f)
                            => (lambda (l) `((%label ,l) ,@ret*)))
                           (else ret*)))
               (w (fxmax w (fx- rip+ rip-)))
               (ins^
                (match ins
                  [(J ('+ 'rip disp))
                   (let ((target (+ rip+ disp)))
                     (cond ((hashtable-ref addrs target #f)
                            => (lambda (l) `(,J ,l)))
                           (else ins)))]
                  [_ ins])))
           (lp ins* w `((,rip- ,rip+ ,bytes . ,ins^) ,@ret*)))]
        [() (values (reverse ret*) w)])))
  (define print-instr/sexpr
    (match-lambda
     [('* reg 1)
      (display reg)]
     [(and (_ . _) i)
      (display #\()
      (let lp ((i i))
        (unless (null? i)
          (print-instr/sexpr (car i))
          (unless (null? (cdr i))
            (display #\space)
            (lp (cdr i)))))
      (display #\))]
     [(? number? i)
      (display "#x")
      (display (number->string i 16))]
     [(? vector? L)
      ;; Label hack. Might be nice to have error-handling labels
      ;; named assertion0, restart0, etc.
      (display (label-name L))]
     [x (display x)]))
  (define (print1 entry maxsz rip- rip+ bytes ins)
    (display "   ")
    (display (number->string (fx+ entry rip-) 16))
    (display #\space)
    (for-each (match-lambda
               [(tag . byte*)
                (case tag
                  ((modr/m sib) (display "\x1b;[1;34m"))
                  ((opcode) (display "\x1b;[1;32m"))
                  ((prefix) (display "\x1b;[1;33m"))
                  ((immediate) (display "\x1b;[1;37m"))
                  ((disp offset) (display "\x1b;[1;35m"))
                  (else (display "\x1b;[0m")))
                (for-each (lambda (byte)
                            (when (fx<? byte #x10)
                              (display #\0))
                            (display (number->string byte 16)))
                          byte*)])
              bytes)
    (display "\x1b;[0m")
    ;; Align the hexdump
    (display (make-string (fx+ 1 (fx* 2 (fx- maxsz (fx- rip+ rip-)))) #\space))
    (print-instr/sexpr ins)
    (newline))
  (define (comment x)
    (print " ; " x))
  (define (label-name x)
    (match x
      [#(L) (string-append "L" (number->string L))]))
  (define (instruction*-comment ins*)
    ;; Two instructions are available. Can't see past labels.
    (match ins*
      [(('mov r1 ('mem64+ r2 -2)) . _)
       `(set! ,r1 (car ,r2))]
      [(('mov r1 ('mem64+ r2 +6)) . _)
       `(set! ,r1 (cdr ,r2))]
      [(('mov r1 ('mem64+ r2 (? fixnum? disp) ('* r3 '1))) . _)
       (let ((idx (fxand disp (fxnot #b111))))
         (case (fx- 8 (fxand disp #b111))
           ((#b001) `(set! ,r1 ($box-ref ,r2 ,r3)))
           ;; ((#b011) "procedure.")
           ;; ((#b100) "string.")
           ((#b110) `(set! ,r1 (vector-ref ,r2 (+ ,r3 ,idx))))
           ;; ((#b101) "bytevector.")
           (else #f)))]
      [(('cmp r1 ('mem64+ r2 (? fixnum? disp)))
        ('jnb L) . _)
       (let ((idx (fxand disp (fxnot #b111))))
         (and (zero? idx)
              (case (fx- 8 (fxand disp #b111))
                ((#b100) `(unless (fx<? -1 ,r1 (string-length ,r2))
                            (goto ,(label-name L))))
                ((#b110) `(unless (fx<? -1 ,r1 (vector-length ,r2))
                            (goto ,(label-name L))))
                ((#b101) `(unless (fx<? -1 ,r1 (bytevector-length ,r2))
                            (goto ,(label-name L))))
                (else #f))))]
      [(('test r1 '7) ('jnz L) . _)
       `(unless (fixnum? ,r1) (goto ,(label-name L)))]
      [(('ud2 . _) . _)
       `(raise (make-assertion-violation))]
      [_ #f]))
  (define (print-code/amd64 ins* maxsz entry)
    (print "  entry:")
    (let lp ((ins* ins*))
      (match ins*
        [(('%label L) . ins*)
         (print "  " (label-name L) ":")
         (lp ins*)]
        [((rip- rip+ bytes . ins) . ins*)
         (cond ((instruction*-comment
                 (cons ins (match ins*
                             [((_ _ _ . ins) . _) (list ins)]
                             [((%label . _) . _) '()]
                             [() '()])))
                => comment))
         (print1 entry maxsz rip- rip+ bytes ins)
         (lp ins*)]
        [() (if #f #f)])))
  (assert (procedure? proc))
  (cond
    ((info-label ($procedure-info proc)) =>
     (lambda (entry)
       (print "Disassembly for " proc #\newline)
       (let ((p (open-bytevector-input-port (copy-code proc))))
         ;; TODO: might be interesting to use pass-optimize to analyze
         ;; the code.
         (let-values (((ins* w) (recover-local-labels (get-instructions p))))
           (print-code/amd64 ins* w entry)))))
    (else
     (print "The procedure " proc " is interpreted\n"))))

(define (call-with-limited-string-output-port limit proc)
  (define buf (make-string limit))
  (define pos 0)
  (define exit-k)
  (define (limit-write! str start count)
    (let ((count^ (fxmin (fx- limit pos) count)))
      (cond ((eqv? count^ 0)
             (string-set! buf (fx- (string-length buf) 1) #\…)
             (set! pos limit)
             (exit-k))
            (else
             (do ((i 0 (fx+ i 1)))
                 ((fx=? i count^)
                  (set! pos (fx+ pos count^))
                  count^)
               (string-set! buf (fx+ pos i) (string-ref str (fx+ start i))))))))
  (define port
    (make-custom-textual-output-port "*limited string*" limit-write! #f #f #f))
  (call/cc
    (lambda (k)
      (set! exit-k k)
      (proc port)
      (flush-output-port port)))
  (substring buf 0 pos))

(define (pretty-print/limit/fix-indent v max-len indent-spaces)
  (let ((s (call-with-limited-string-output-port max-len (lambda (p) (pretty-print-simple v p)))))
    (call-with-string-output-port
      (lambda (p)
        (do ((i 0 (fx+ i 1)))
            ((fx=? i (string-length s)))
          (let ((c (string-ref s i)))
            (put-char p c)
            (when (and (eqv? c #\linefeed)
                       (not (fx=? i (fx- (string-length s) 1))))
              (display (make-string indent-spaces #\space) p))))))))

(define (stack-trace k p)
  ;; This code is currently specific to amd64. This code will work
  ;; *very* unreliably if interpreted by a tree code interpreter.
  (define who 'stack-trace)
  (define (print . x) (for-each (lambda (x) (display x p)) x) (newline p))
  (define (continuation-stack k)
    ;; Continuations are returned from call/cc as closures where one
    ;; of the free variables is a boxed stack.
    (if (not (procedure? k))
        (error who "Not a continuation" k)
        (let lp ((i 0))
          (if (fx=? i ($procedure-length k))
              (error who "Not a continuation" k)
              (let ((v ($procedure-ref k i)))
                (if (and ($box? v) (eq? ($box-type v) 'stack))
                    v
                    (lp (fx+ i 1))))))))
  (define (get-mem-uint addr size)
    ;; XXX: this clearly shows that the encoding is too complex for
    ;; an assembler decoder. Always using two bytes for the size
    ;; should solve this.
    (let lp ((addr addr) (size size) (ret 0))
      (if (fxzero? size)
          ret
          (lp (fx+ addr 1)
              (fx- size 1)
              (fxior (fxarithmetic-shift-left ret 8)
                     (get-mem-u8 addr))))))
  (define (get-livemask addr size1)
    ;; The first nop instruction has size1 bytes of live mask. The
    ;; following ones (if they exist) have five bytes each.
    (let lp ((mask (get-mem-uint addr size1))
             (addr (fx+ addr size1)))
      (let ((op0 (get-mem-u8 addr))
            (op1 (get-mem-u8 (fx+ addr 1)))
            (modr/m (get-mem-u8 (fx+ addr 2)))
            (addr (fx+ addr 3)))
        (if (and (fx=? op0 #x0F)
                 (fx=? op1 #x1F)
                 (fx=? modr/m #b10100100))
            (lp (bitwise-ior (bitwise-arithmetic-shift-left
                              (get-mem-uint addr 4 #;what?))
                             mask)
                (fx+ addr 5))
            mask))))
  (let* ((stk (continuation-stack k))
         (len ($box-ref stk 0)))
    (print "Stack trace (most recent frame first):")
    (let lp ((frame 0) (idx 1))
      (cond ((fx=? idx len)
             (print "End of stack trace."))
            ((fx>? idx len)
             (print "End of stack trace.")
             (print "WARNING: Overrun in stack decoding."))
            (else
             (let ((rip ($object->fixnum ($box-ref stk idx)))
                   (idx (fx+ idx 1)))
               ;; TODO: look up where the code of rip comes from
               (print " Frame " frame " has return address #x" (number->string rip 16) ".")
               (when (<= #x200000 rip #xffffffff) ;XXX: should probably just catch #PF
                 (let ((frame (fx+ frame 1))
                       (op0 (get-mem-u8 rip))
                       (op1 (get-mem-u8 (fx+ rip 1)))
                       (modr/m (get-mem-u8 (fx+ rip 2))))
                   (let ((size-bytes (fxbit-field modr/m 3 7)))
                     (cond ((or (not (fx=? op0 #x0F))
                                (not (fx=? op1 #x1F))
                                (fxzero? size-bytes))
                            ;; This is not a liveness NOP, skip it.
                            (print "  (no live locals)")
                            (lp frame idx))
                           (else
                            (let ((locals (fx+ (get-mem-uint (fx+ rip 3) size-bytes) 1))
                                  (livemask (get-livemask (fx+ (fx+ rip 3) size-bytes)
                                                          size-bytes)))
                              (do ((i 0 (fx+ i 1))
                                   (idx idx (fx+ idx 1)))
                                  ((fx=? i locals))
                                (cond ((bitwise-bit-set? livemask i)
                                       ;; XXX: ONLY if the local is in
                                       ;; the livemask is it ever safe
                                       ;; to do $box-ref on it from a
                                       ;; boxed stack.
                                       (display "  Local " p)
                                       (display i p)
                                       (display ": " p)
                                       (let* ((v ($box-ref stk idx))
                                              (s (pretty-print/limit/fix-indent v 200 (+ (string-length "  Local : ")
                                                                                         (string-length (number->string i)))))
                                              (len (string-length s)))
                                         (display s p)
                                         (when (or (eqv? len 0)
                                                   (not (char=? #\linefeed
                                                                (string-ref s (fx- len 1)))))
                                           (newline p))))))
                              (lp frame (fx+ idx (fx* locals 1)))))))))))))))

;; Valgrind requests (valgrind.h)
(define RUNNING_ON_VALGRIND #x1001)

(define valgrind
  (case-lambda
    ((request)
     (assert (fx=? request RUNNING_ON_VALGRIND))
     (let ((req (make-bytevector (* 7 8) 0)))
       (bytevector-u64-native-set! req 0 request)
       ($valgrind (bytevector-address req))))
    ((request arg0)
     (assert (fx=? request RUNNING_ON_VALGRIND))
     (let ((req (make-bytevector (* 7 8) 0)))
       (bytevector-u64-native-set! req 0 request)
       (bytevector-u64-native-set! req 8 arg0)
       ($valgrind (bytevector-address req))))))

;; The size and alignment (in bytes) of cache-line-flush. Usually 64.
(define cache-line-size
  (let ((line-size
         (let-values ([(_a b _c _d) (cpuid #x00000001)])
           (fx* (fxbit-field b 8 16) 8))))
    (lambda () line-size))))
