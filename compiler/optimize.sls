;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Low-level optimizations

;; This is planned:

;; [x] global register allocation
;; [x] remove branch chains
;; [x] reverse branches
;; [x] remove unreachable code
;; [x] loop analysis
;; [x] live/dead variable analysis
;; [x] dead assignment elimination
;; [x] local copy propagation
;; [ ] local common subexpression elimination
;; [x] peephole optimization
;; [ ] loop-invariant code motion
;; [x] eliminate useless jumps
;; [x] reorder basic blocks
;; [ ] merge basic blocks (improves local analysis)
;; [x] alignment of branch targets
;; [ ] evaluation order determination

;; The input to the optimizer must follow a specific format.
;; Procedures are delimited by (%comment procedure) (%align 8) (%label
;; <label>) and (%comment procedure-end). There are more rules, but
;; who has the time to write them all out...

(library (loko compiler optimize)
  (export
    pass-optimize)
  (import
    (psyntax compat)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map)
    (rnrs mutable-pairs)
    (loko match))

;;; Record types and stuff

(define-record bblk (label
                     align
                     number loopnest
                     ;; Gap buffer with lines.
                     line*
                     ;; pred* and succ* are lists of bblk records.
                     ;; The first succ is always the fallthrough,
                     ;; if there is one.
                     pred* succ*
                     ;; Dominators is a bvector.
                     doms loop
                     ;; varstates
                     uses defs ins outs
                     ;; Integer flags
                     status))

(define-record line (inst info sets uses deads live-out))

(define-record loop (header pre-header blocks invariant sets writes?))

;; XXX: This was going to be gap buffers, but doubly-linked lists
;; would probably have been better.
(define-record gap (buffer))

(define (list->gap l)
  (make-gap (list->vector l)))

(define (gap->list gap)
  (vector->list (gap-buffer gap)))

(define (gap-cursor-index gap c) c)

(define (gap-top gap) 0)

(define (gap-next gap c)
  (fx+ c 1))

(define (gap-prev gap c)
  (fx- c 1))

(define (gap-bottom gap)
  (fx- (vector-length (gap-buffer gap)) 1))

;; (define (gap-bottom? gap c)
;;   (fx=? c (fx- (vector-length (gap-buffer gap)) 1)))

(define (gap-over-top? gap c)
  (fxnegative? c))

(define (gap-end? gap c)
  (fx=? c (vector-length (gap-buffer gap))))

;; (define (gap-empty? gap)
;;   (fx=? (vector-length (gap-buffer gap))
;;         (gap-size gap)))

(define (gap-ref gap c)
  (vector-ref (gap-buffer gap) c))

(define-syntax gap-for-each
  (syntax-rules ()
    ((_ proc gap)
     (let ((g gap) (p proc))
       (do ((c (gap-top g) (gap-next g c)))
           ((gap-end? g c))
         (p (gap-ref g c)))))))

(define (gap-filter! match? gap)
  (set-gap-buffer! gap (list->vector
                        (filter match?
                                (vector->list
                                 (gap-buffer gap))))))

(define (gap-exists match? gap)
  (let lp ((i (gap-top gap)))
    (cond ((gap-end? gap i) #f)
          ((match? (gap-ref gap i)) #t)
          (else (lp (gap-next gap i))))))

(define (new-bblk label line*)
  (define (bvector-none) 0)
  (make-bblk label 0
             0 0
             line* '() '()
             (bvector-none) #f
             (varstate-none) (varstate-none)
             (varstate-none) (varstate-none)
             0))

(define (new-line inst info sets uses)
  (make-line inst info sets uses 0 0))

(define (new-loop head)
  (make-loop head #f '() (varstate-none) (varstate-none) #f))

(define (fallthrough cblk)
  (let ((succ* (bblk-succ* cblk)))
    (and (pair? succ*) (car succ*))))

(define (bblk-pred*-add! blk pred)
  (unless (memq pred (bblk-pred* blk))
    (set-bblk-pred*! blk (cons pred (bblk-pred* blk)))))

(define (bblk-succ*-add! blk succ)
  (unless (memq succ (bblk-succ* blk))
    (set-bblk-succ*! blk (cons succ (bblk-succ* blk)))))

(define (varstate-none) 0)

(define varempty? (lambda (x) (eqv? x 0)))

(define var-union bitwise-ior)

(define var-inter bitwise-and)

(define (var-set-minus v1 v2)
  (bitwise-and v1 (bitwise-not v2)))

(define var=? =)

(define (varstate-for-each proc x)
  (let lp ((x x))
    (let ((idx (bitwise-first-bit-set x)))
      (unless (eqv? idx -1)
        (proc idx)
        (lp (bitwise-and x (- x 1)))))))

(define (varstate-map proc x)
  (let lp ((x x) (ret '()))
    (let ((idx (bitwise-first-bit-set x)))
      (cond ((eqv? idx -1)
             (reverse ret))
            (else
             (lp (bitwise-and x (- x 1))
                 (cons (proc idx) ret)))))))

;; Only for pretty-printing for basic block vectors.
(define (bvector->list x)
  (let lp ((ret '()) (x x))
    (let ((idx (bitwise-first-bit-set x)))
      (if (fx=? idx -1)
          (reverse ret)
          (lp (cons idx ret)
              (bitwise-and x (- x 1)))))))

;;; Fixnum sets

(define-record-type set
  (sealed #t)
  (fields (mutable members)
          (immutable array)
          (immutable stack))
  (protocol
   (lambda (p)
     (lambda (u)
       (p 0 (make-vector u 0) (make-vector u 0))))))

;; Arrays might be better done as bytevectors later
(define array-ref vector-ref)

(define array-set! vector-set!)

(define stack-ref vector-ref)

(define stack-set! vector-set!)

;; Constant time operations

(define (set-clear! s)
  (set-members-set! s 0))

(define (set-empty? s)
  (fxzero? (set-members s)))

(define (set-add! s n)
  (define A (set-array s))
  (define S (set-stack s))
  (let ((a (array-ref A n))
        (m (set-members s)))
    (unless (and (fx<? a m) (fx=? (stack-ref S a) n))
      ;; Extend the stack to point at the new member. The entry in
      ;; array is made to point at the new stack entry.
      (set-members-set! s (fx+ m 1))
      (stack-set! S m n)
      (array-set! A n m))))

(define (set-delete! s n)
  (define A (set-array s))
  (define S (set-stack s))
  (let ((a (array-ref A n))
        (m (set-members s)))
    (when (and (fx<? a m) (fx=? (stack-ref S a) n))
      (let* ((m (fx- m 1))
             (e (stack-ref S m)))
        (set-members-set! s m)
        (stack-set! S a e)
        (array-set! A e a)))))

(define (set-member? s n)
  (define A (set-array s))
  (define S (set-stack s))
  (let ((a (array-ref A n))
        (m (set-members s)))
    (and (fx<? a m) (fx=? (stack-ref S a) n))))

(define (set-choose-one s)
  (define S (set-stack s))
  (let ((m (set-members s)))
    ;; XXX: selects the last in the stack so that sets can also be
    ;; used as stacks
    (assert (fxpositive? m))
    (stack-ref S (fx- m 1))))

(define set-cardinality set-members)

;; Linear time (in the cardinality of the set) operations

(define (set-union! s0 s1)
  ;; Adds the elements from s1 to s0.
  (define A0 (set-array s0))
  (define S0 (set-stack s0))
  (define S1 (set-stack s1))
  (define m1 (set-members s1))
  (let lp ((i 0) (m0 (set-members s0)))
    (cond ((fx=? i m1)
           (set-members-set! s0 m0))
          (else
           (let* ((n (stack-ref S1 i))
                  (a (array-ref A0 n)))
             (cond ((and (fx<? a m0) (fx=? (stack-ref S0 a) n))
                    (lp (fx+ i 1) m0))
                   (else
                    ;; n is not in s0, so add it.
                    (stack-set! S0 m0 n)
                    (array-set! A0 n m0)
                    (lp (fx+ i 1) (fx+ m0 1)))))))))

(define (set-intersection! s0 s1)
  ;; Afterwards s0 only contains those elements which are in both
  ;; s0 and s1.
  (define A0 (set-array s0))
  (define A1 (set-array s1))
  (define S0 (set-stack s0))
  (define S1 (set-stack s1))
  (define m1 (set-members s1))
  (let lp ((i 0) (m0 (set-members s0)))
    (cond ((fx=? i m0)
           (set-members-set! s0 m0))
          (else
           (let* ((n (stack-ref S0 i))
                  (a (array-ref A1 n)))
             (cond ((and (fx<? a m1) (fx=? (stack-ref S1 a) n))
                    (lp (fx+ i 1) m0))
                   (else
                    ;; n is not in s1, so remove it from s0.
                    (let* ((m0 (fx- m0 1))
                           (e (stack-ref S0 m0)))
                      (stack-set! S0 i e)
                      (array-set! A0 e i)
                      (lp i m0)))))))))

(define (set-difference! s0 s1)
  ;; Afterwards s0 no longer contains the elements from s1.
  (define A0 (set-array s0))
  (define A1 (set-array s1))
  (define S0 (set-stack s0))
  (define S1 (set-stack s1))
  (define m1 (set-members s1))
  (let lp ((i 0) (m0 (set-members s0)))
    (cond ((fx=? i m0)
           (set-members-set! s0 m0))
          (else
           (let* ((n (stack-ref S0 i))
                  (a (array-ref A1 n)))
             (cond ((not (and (fx<? a m1) (fx=? (stack-ref S1 a) n)))
                    (lp (fx+ i 1) m0))
                   (else
                    ;; n is in s1, so remove it from s0.
                    (let* ((m0 (fx- m0 1))
                           (e (stack-ref S0 m0)))
                      (stack-set! S0 i e)
                      (array-set! A0 e i)
                      (lp i m0)))))))))

(define (set-copy! s0 s1)
  (set-clear! s0)
  (set-union! s0 s1))

(define (set=? s0 s1)
  ;; Checks if the sets are of the same cardinality and every
  ;; element in s1 exists in s0.
  (define A0 (set-array s0))
  (define S0 (set-stack s0))
  (define S1 (set-stack s1))
  (let ((m0 (set-members s0))
        (m1 (set-members s1)))
    (and (fx=? m0 m1)
         (let lp ((i 0))
           (or (fx=? i m1)
               (let* ((n (stack-ref S1 i))
                      (a (array-ref A0 n)))
                 (and (fx<? a m0) (fx=? (stack-ref S0 a) n)
                      (lp (fx+ i 1)))))))))

(define (set->list s)
  (define S (set-stack s))
  (let ((m (set-members s)))
    (let lp ((i 0) (ret '()))
      (if (fx=? i m)
          ret
          (lp (fx+ i 1)
              (cons (stack-ref S i) ret))))))

(define (set-for-each proc s)
  (define S (set-stack s))
  (let ((m (set-members s)))
    (let lp ((i 0))
      (unless (fx=? i m)
        (proc (stack-ref S i))
        (lp (fx+ i 1))))))

;; Linear in the time of the universe

(define (set-complement! s0 s1)
  ;; Sets s0 to those elements which do not exist in s1.
  (define A0 (set-array s0))
  (define A1 (set-array s1))
  (define S0 (set-stack s0))
  (define S1 (set-stack s1))
  (define m1 (set-members s1))
  (define u (vector-length A0))
  (let lp ((n 0) (m0 0))
    (cond ((fx=? n u)
           (set-members-set! s0 m0))
          (else
           (let ((a (array-ref A1 n)))
             (cond ((and (fx<? a m1) (fx=? (stack-ref S1 a) n))
                    (lp (fx+ n 1) m0))
                   (else
                    ;; n is not in s1, so add it.
                    (stack-set! S0 m0 n)
                    (array-set! A0 n m0)
                    (lp (fx+ n 1) (fx+ m0 1)))))))))

;;; The optimizations themselves

(define (pass-optimize text data instruction-analyzer host-convention)
  (define debug #f)
  (define (strip-gensyms x)
    (cond ((not debug) x)
          ((symbol? x)
           (string->symbol (symbol->string x)))
          ((pair? x)
           (cons (strip-gensyms (car x))
                 (strip-gensyms (cdr x))))
          ((vector? x)
           (vector-map strip-gensyms x))
          (else x)))

  (define-syntax print
    (lambda (x)
      (syntax-case x ()
        [(_ x* ...)
         #'(when debug
             (display x*) ...
             (newline))])))

  (define *register-names* (host-convention 'register-names))
  (define *scratch* (host-convention 'scratch))
  (define *unavailable* (host-convention 'unavailable))
  (define *number-of-blocks*)
  (define *loops* '())
  (define *all-regs* (- (expt 2 (vector-length *register-names*)) 1))
  (define *stack-liveness* #f)
  (define *branch-targets* (make-eq-hashtable))

  (define *pseudoregs* (make-eqv-hashtable))
  (define *pcounter* -1)
  (define (new-pseudo-reg)
    (set! *pcounter* (fx+ *pcounter* 1))
    (let ((reg `(reg ,*pcounter*)))
      (hashtable-set! *pseudoregs* *pcounter* reg)
      reg))

  ;; Status fields
  (define DONE #b1)
  (define IMMORTAL #b10)

  (define (bblk-immortal? cblk)
    (not (zero? (bitwise-and (bblk-status cblk) IMMORTAL))))

  (define (bblk-done? bblk)
    (not (fxzero? (fxand (bblk-status bblk) DONE))))

  (define (set-bblk-done! bblk)
    (set-bblk-status! bblk (fxior (bblk-status bblk) DONE)))

  (define (set-bblk-not-done! bblk)
    (set-bblk-status! bblk (fxand (bblk-status bblk) (fxnot DONE))))

  ;; Only for pretty-printing of basic blocks.
  (define (register-index->name idx)
    (let ((names *register-names*))
      (if (< idx (vector-length names))
          (vector-ref names idx)
          (string-append "P" (number->string
                              (- idx (vector-length names)))))))

  (define (varstate->list x)
    (define (set->list x regs)
      (let lp ((regs regs) (x x))
        (let ((idx (bitwise-first-bit-set x)))
          (if (fx=? idx -1)
              regs
              (let ((name (register-index->name idx)))
                (lp (cons name regs)
                    (bitwise-and x (- x 1))))))))
    (if debug
        (if (var=? x *scratch*)
            'scratch
            (set->list x '()))
        'no-debugging))

  ;; Pseudo registers come out of the code generator looking like
  ;; (reg 0). They have object identity.
  (define (pseudo-register? x)
    (and (pair? x) (eq? (car x) 'reg) (pair? (cdr x))))

  (define (pseudo-register-index x)
    (+ (vector-length *register-names*) (cadr x)))

  ;; Recognizes the assembler operands created by make-stack-load
  ;; and make-stack-store.
  (define (spill-place-holder? x)
    (and (pair? x) (eq? (car x) 'spill-location)))

  (define (spill-identifier x)
    (cadr x))

;;; Instructions!

  (define (last-instruction cblk)
    (let ((line* (bblk-line* cblk)))
      (let lp ((i (gap-bottom line*)))
        (if (gap-over-top? line* i)
            #f
            (let ((line (gap-ref line* i)))
              (if (directive? line)
                  (lp (gap-prev line* i))
                  line))))))

  (define (first-instruction cblk)
    (let ((line* (bblk-line* cblk)))
      (let lp ((i (gap-top line*)))
        (if (gap-end? line* i)
            #f
            (let ((line (gap-ref line* i)))
              (if (directive? line)
                  (lp (gap-next line* i))
                  line))))))

  ;; Recover the label that the instruction branches to. Also works
  ;; for jumps.
  (define (branch-target line)
    (cadr (line-inst line)))

  ;; Make a new assembler instruction that is a branch with the
  ;; condition reversed. Return #f is not possible.
  (define (reverse-branch inst target)
    (let ((mnemonic (host-convention 'reverse-branch-condition (car inst))))
      (and mnemonic (list mnemonic target))))

  (define (normal-branch inst target)
    (let* ((mnemonic (car inst))
           (new-inst (list mnemonic target)))
      (and mnemonic
           (host-convention 'valid-branch? new-inst)
           new-inst)))

  (define (make-return)
    (host-convention 'return-instruction))

  (define (find-pseudo-in-inst reg old-inst)
    ;; This is needed because the pseudo registers (reg N) have
    ;; identity (are compared with eq?).
    ;; TODO: might be better to keep them in an hashtable
    (let ((op (let lp ((x old-inst))
                (cond ((and (pseudo-register? x)
                            (= reg (pseudo-register-index x)))
                       x)
                      ((pair? x)
                       (or (lp (car x)) (lp (cdr x))))
                      (else #f)))))
      (or op
          (error 'find-pseudo-in-inst
                 "Not found" reg old-inst))))

  (define (make-stack-store reg old-inst)
    ;; TODO: host-convention
    (let ((name (find-pseudo-in-inst reg old-inst)))
      (let ((l (new-line `(mov (spill-location ,reg) ,name)
                         0 0 0)))
        (setup-line! l)
        l)))

  (define (make-stack-load reg old-inst)
    ;; TODO: host-convention
    (let ((name (find-pseudo-in-inst reg old-inst)))
      (let ((l (new-line `(mov ,name (spill-location ,reg))
                         0 0 0)))
        (setup-line! l)
        l)))

  (define (branch? line)
    (eq? (line-info line) 'branch))

  (define (label->target lbl)
    ;; The labels of blocks are #f or lists where the car is an
    ;; assembler label and the rest is for use by the compiler.
    (and lbl (car lbl)))

  (define (jump? line)
    (eq? (line-info line) 'jump))

  (define (return? line)
    (eq? (line-info line) 'return))

  (define (trap? line)
    (eq? (line-info line) 'trap))

  (define (control-transfer? line)
    (memq (line-info line) '(branch jump #;call #;fastcall return #;trap)))

  (define (call? line) (eq? (line-info line) 'call))

  (define (arithmetic? line) (eq? (line-info line) 'arithmetic))

  ;; Line is an unconditional mov instruction that does not
  ;; reference memory. It does not convert between data types. The
  ;; sets and uses reflect the destination and source of the move.
  ;; The source may be a constant.
  (define (move? line) (eq? (line-info line) 'move))

  (define (load? line) (eq? (line-info line) 'load))

  (define (cond-move? line) (eq? (line-info line) 'cond-move))

  (define (safe-load? line)
    ;; Is this load always without side-effect (other than setting a
    ;; register)?
    (and (eq? (line-info line) 'load)
         (host-convention 'safe-load? (line-inst line))))

  (define (directive? line) (eq? (line-info line) 'directive))

  (define (copy? line)
    (and (move? line)
         (let ((sets (line-sets line))
               (uses (line-uses line)))
           (and (not (varempty? uses))
                (not (varempty? sets))))))

  ;; Find the basic block that the instruction branches to. The car
  ;; of the block label must be eq? to the target label.
  (define (find-branch-target bblk* line)
    (hashtable-ref *branch-targets* (branch-target line) #f))

;;; Main

  (define (setup-line! line)
    (let-values (((info sets uses) (instruction-analyzer (line-inst line)
                                                         *stack-liveness*)))
      (set-line-info! line info)
      (set-line-sets! line sets)
      (set-line-uses! line uses)
      (set-line-deads! line (varstate-none))
      (set-line-live-out! line (varstate-none))))

  (define (code->basic-blocks code*)
    ;; Split the code into basic blocks.
    (let lp ((label (cdar code*))
             (bblk* '())
             (code* (cdr code*)))
      (let lp* ((line* '())
                (code* code*))
        (cond ((equal? (car code*) '(%comment procedure-end))
               ;; Last block.
               (if (and (null? line*) (not label))
                   (list->gap (reverse bblk*))
                   (list->gap
                    (reverse (cons (new-bblk label (list->gap (reverse line*)))
                                   bblk*)))))
              ((eq? (caar code*) '%label)
               (lp (cdar code*)
                   (if (and (null? line*) (not label))
                       bblk*
                       (cons (new-bblk label (list->gap (reverse line*)))
                             bblk*))
                   (cdr code*)))
              (else
               (let-values (((info sets uses)
                             (instruction-analyzer (car code*) *stack-liveness*)))
                 (let* ((line (new-line (car code*) info sets uses))
                        (line* (cons line line*)))
                   (if (control-transfer? line)
                       ;; transfers of control (TODO: not error
                       ;; handlers)
                       (lp #f
                           (cons (new-bblk label (list->gap (reverse line*)))
                                 bblk*)
                           (cdr code*))
                       (lp* line* (cdr code*))))))))))

  ;; (print-blocks
  ;;  (code->basic-blocks
  ;;   '((%label scheme-start)
  ;;     (call init-global-environment)
  ;;     (nop)
  ;;     (jmp #(top))
  ;;     (%label hello)
  ;;     (nop)
  ;;     (ret)
  ;;     (%label end)
  ;;     (%comment procedure-end))))

  (define (print-line line)
    (define (len x)
      (string-length
       (call-with-string-output-port
         (lambda (p)
           (write x p)))))
    (display "  ")
    (let ((inst (strip-gensyms (line-inst line))))
      (write inst)
      (print (make-string (max 0 (- 40 (len inst)))
                          #\space)
             " ;sets: " (varstate->list (line-sets line))
             " uses: " (varstate->list (line-uses line))
             " deads: " (varstate->list (line-deads line))
             ;; " live-out: " (varstate->list (line-live-out line))
             )))

  (define (print-block cblk)
    (print "\n; Block " (bblk-number cblk) " / " (- *number-of-blocks* 1)
           "\n; Predecessors: "
           (map bblk-number (bblk-pred* cblk))
           "\n;   Successors: "
           (map bblk-number (bblk-succ* cblk))
           "\n;   Dominators: " (bvector->list (bblk-doms cblk))

           "\n;         Uses: " (varstate->list (bblk-uses cblk))
           "\n;         Defs: " (varstate->list (bblk-defs cblk))

           "\n;          Ins: " (varstate->list (bblk-ins cblk))
           "\n;         Outs: " (varstate->list (bblk-outs cblk)))
    (when (positive? (bblk-align cblk))
      (write `(%align ,(bblk-align cblk)))
      (newline))
    (when (bblk-label cblk)
      (write (strip-gensyms `(%label ,@(bblk-label cblk))))
      (newline))
    (gap-for-each print-line (bblk-line* cblk)))

  (define (print-blocks bblk*)
    (gap-for-each print-block bblk*))

  (define (renumber-blocks! bblk*)
    (print ";; Renumber blocks")
    (do ((i 0 (fx+ i 1))
         (c (gap-top bblk*) (gap-next bblk* c)))
        ((gap-end? bblk* c)
         (set! *number-of-blocks* i))
      (let ((cblk (gap-ref bblk* c)))
        (set-bblk-number! cblk i))))

  (define (setup-flow! bblk*)
    (define (fallthrough c . x)
      (let ((ft (gap-ref bblk* (gap-next bblk* c))))
        (cons ft (remq ft x))))
    (hashtable-clear! *branch-targets*)
    (gap-for-each (lambda (cblk)
                    (set-bblk-pred*! cblk '())
                    (cond ((bblk-label cblk) =>
                           (lambda (label)
                             (hashtable-set! *branch-targets* (car label) cblk)))))
                  bblk*)
    (do ((i 0 (fx+ i 1))
         (c (gap-top bblk*) (gap-next bblk* c)))
        ((gap-end? bblk* c))
      (let* ((cblk (gap-ref bblk* c)))
        ;; Setup successors.
        (cond ((last-instruction cblk) =>
               (lambda (last)
                 (cond
                   ((branch? last)
                    (cond ((find-branch-target bblk* last)
                           => (lambda (target)
                                (set-bblk-succ*! cblk (fallthrough c target))))
                          (else
                           ;; Branch to extern.
                           (set-bblk-succ*! cblk (fallthrough c)))))
                   ((jump? last)
                    (cond ((find-branch-target bblk* last)
                           => (lambda (target)
                                (set-bblk-succ*! cblk (list target))))
                          (else
                           ;; Indirect or other procedure. TODO:
                           ;; recover jump tables?
                           (set-bblk-succ*! cblk '()))))
                   ((or (return? last) (trap? last))
                    ;; No successor.
                    (set-bblk-succ*! cblk '()))
                   (else
                    ;; Fall through.
                    (set-bblk-succ*! cblk (fallthrough c))))))
              ((gap-end? bblk* (gap-next bblk* c))
               ;; Last block has no successor.
               (set-bblk-succ*! cblk '()))
              (else
               ;; Empty block falls through.
               (set-bblk-succ*! cblk (fallthrough c))))
        ;; Setup predeccessors.
        (for-each (lambda (blk)
                    (set-bblk-pred*! blk (cons cblk (bblk-pred* blk))))
                  (bblk-succ* cblk)))))

  (define (basic-blocks->code bblk* new-text*)
    (define (cons-align cblk new-text*)
      (if (positive? (bblk-align cblk))
          (cons `(%align ,(bblk-align cblk))
                new-text*)
          new-text*))
    (define (cons-label cblk new-text*)
      (if (bblk-label cblk)
          (cons `(%label ,@(bblk-label cblk))
                new-text*)
          new-text*))
    (let lp ((new-text* new-text*)
             (i (gap-top bblk*)))
      (cond ((gap-end? bblk* i)
             (cons '(%comment procedure-end) new-text*))
            (else
             (let* ((cblk (gap-ref bblk* i))
                    (line* (bblk-line* cblk)))
               (let lp* ((new-text*
                          (cons-label cblk
                                      (cons-align cblk new-text*)))
                         (j (gap-top line*)))
                 (cond ((gap-end? line* j)
                        (lp new-text* (gap-next bblk* i)))
                       (else
                        (lp* (cons (line-inst (gap-ref line* j))
                                   new-text*)
                             (gap-next line* j))))))))))

  (define (setup-lines! bblk*)
    (gap-for-each (lambda (cblk)
                    (gap-for-each setup-line! (bblk-line* cblk)))
                  bblk*))

  (define (gather-pseudo-registers bblk*)
    (hashtable-clear! *pseudoregs*)
    (gap-for-each
     (lambda (cblk)
       (gap-for-each
        (lambda (line)
          (unless (directive? line)
            (let f ((x (cdr (line-inst line))))
              (cond ((pseudo-register? x)
                     (set! *pcounter* (max *pcounter* (pseudo-register-index x)))
                     (hashtable-set! *pseudoregs* (pseudo-register-index x) x))
                    ((pair? x)
                     (f (car x))
                     (f (cdr x)))))))
        (bblk-line* cblk)))
     bblk*))

  (define (optimize-procedure code* new-text*)
    (define OPTIMIZE? #t)
    (define REORDER-BLOCKS? #t)
    (let ((comment (car code*))
          (align (cadr code*))
          (label (caddr code*)))
      (assert (equal? comment '(%comment procedure)))
      (assert (eq? (car align) '%align))
      (assert (eq? (car label) '%label))
      (print "\n;;; Procedure started by " label)
      ;; (when debug
      ;;   (write code*)
      ;;   (newline))
      (set! *stack-liveness* #f)
      (let ((bblk* (code->basic-blocks (cddr code*))))
        (set-bblk-status! (gap-ref bblk* (gap-top bblk*)) IMMORTAL)
        (set-bblk-status! (gap-ref bblk* (gap-bottom bblk*)) IMMORTAL)
        (setup-flow! bblk*)
        (renumber-blocks! bblk*)
        (when debug
          (print-blocks bblk*))

        ;; An early pass to remove unreachable code saves some time
        ;; later.
        (let loop ()
          (when (remove-unreachable-code! bblk*)
            (loop)))

        (find-loops! bblk*)

        ;; Save and restore live pseudo registers around calls. XXX:
        ;; Only used when coalesce is working, which is a limitation
        ;; created by the current register allocator being slightly
        ;; broken.
        (when (leafy-procedure? bblk*)
          (gather-pseudo-registers bblk*)
          (live-variables! bblk*)
          (live/dead-variables! bblk*)
          (save/restore-live-registers bblk*))

        (global-register-allocation! bblk*)
        (assign-stack-locations! bblk*)
        ;; All pseudo registers are now gone. The sets and uses of
        ;; each line must be recalculated to take stack locations
        ;; into consideration.
        (set! *stack-liveness* #t)
        (setup-lines! bblk*)

        (when OPTIMIZE?
          (remove-branch-chains! bblk*)
          (reverse-branches! bblk*)
          (remove-unreachable-code! bblk*)
          (remove-unnecessary-jumps! bblk*)
          (renumber-blocks! bblk*)

          (local-copy-propagation! bblk*)
          (live-variables! bblk*)
          (dead-assignment-elimination! bblk* (lambda _ #f))

          (remove-unnecessary-jumps! bblk*)
          (remove-unnecessary-jumps! bblk*)

          (live-variables! bblk*)
          (live/dead-variables! bblk*)

          (when #t
            (local-copy-propagation! bblk*)
            (live-variables! bblk*)
            (dead-assignment-elimination! bblk* (lambda _ #f))

            (remove-trash bblk*)
            (let loop ()
              (letrec ((changes #f)
                       (opt++ (lambda _ (set! changes #t))))
                (local-copy-propagation! bblk*)
                (live-variables! bblk*)
                (live/dead-variables! bblk*)
                (dead-assignment-elimination! bblk* opt++)

                (live-variables! bblk*)
                (live/dead-variables! bblk*)
                (peephole-optimization! bblk* opt++)

                (when changes
                  (remove-branch-chains! bblk*)
                  (reverse-branches! bblk*)
                  (remove-unreachable-code! bblk*)
                  (remove-unnecessary-jumps! bblk*)
                  (renumber-blocks! bblk*)
                  (loop)))))

          ;; Reorder basic blocks
          (when REORDER-BLOCKS?
            (remove-unreachable-code! bblk*)
            (renumber-blocks! bblk*)
            (find-loops! bblk*)
            (reorder-blocks! bblk*)))

        ;; Insert information about what stack positions are live.
        (live-variables! bblk*)
        (live/dead-variables! bblk*)
        (liveness-annotations! bblk*)

        (when OPTIMIZE?
          ;; Insert padding to align branch target labels
          (renumber-blocks! bblk*)
          (find-loops! bblk*)
          (align-branch-targets! bblk*))

        (remove-trash bblk*)

        (when debug
          (print "\n;; pass-optimize is finished with the procedure:")
          (for-each (lambda (loop)
                      (print ";; Loop header: " (bblk-number (loop-header loop)))
                      (print ";;      Blocks: " (map bblk-number (loop-blocks loop))))
                    *loops*)
          (print-blocks bblk*))
        (basic-blocks->code bblk* `(,align ,comment ,@new-text*)))))

  (define (main text data)
    (define (get-proc text*)
      (let lp ((ret '())
               (text* text*))
        (if (equal? (car text*) '(%comment procedure-end))
            (values (reverse (cons (car text*) ret))
                    (cdr text*))
            (lp (cons (car text*) ret) (cdr text*)))))
    (let lp ((text* text) (new-text* '()))
      (let-values (((code* text*) (get-proc text*)))
        ;; The optimizer output is prepended to new-text* in reverse
        ;; order.
        (let ((new-text* (optimize-procedure code* new-text*)))
          (if (null? text*)
              (values (reverse new-text*) data)
              (lp text* new-text*))))))

;;; Unreachable code removal

  (define (bblk-has-no-effect? cblk)
    (not (gap-exists (lambda (line)
                       (not (directive? line)))
                     (bblk-line* cblk))))

  (define (remove-unreachable-code! bblk*)
    ;; (requires setup-flow!)
    (define changes #f)
    (when debug
      (print ";; Remove unreachable code"))
    (gap-filter! (lambda (cblk)
                   (cond ((and (not (bblk-label cblk))
                               (bblk-has-no-effect? cblk))
                          (set! changes #t)
                          (when debug
                            (print ";;; OPT: empty basic block"))
                          #f)
                         ((or (bblk-immortal? cblk)
                              (not (null? (bblk-pred* cblk))))
                          #t)
                         (else
                          (set! changes #t)
                          (when debug
                            (print ";;; OPT: unreachable code"))
                          #f)))
                 bblk*)
    (when changes
      (setup-flow! bblk*))
    changes)

;;; Loop analysis

  ;; FIXME: there is some non-determinism from something here. important!

  (define (find-loops! bblk*)
    ;; bitvectors
    (define (bvector-add vec blk)
      (bitwise-ior vec (expt 2 (bblk-number blk))))
    (define bvector-intersection bitwise-and)
    (define (bvector-in? vec blk)
      (bitwise-bit-set? vec (bblk-number blk)))
    (define bvector=? =)
    (define (bvector-all)
      #;(- (expt 2 *number-of-blocks*) 1)
      -1)
    (define (bvector-none) 0)
    ;;
    (define (add-to-loop! loop bblk)
      (set-bblk-loopnest! bblk (fx+ (bblk-loopnest bblk) 1))
      (set-loop-blocks! loop (cons bblk (loop-blocks loop))))
    (print ";; Find loops")
    (let* ((i (gap-top bblk*))
           (top (gap-ref bblk* i)))
      ;; Calculate dominators
      (gap-for-each (lambda (cblk)
                      (set-bblk-doms! cblk
                                      (if (eq? cblk top)
                                          (bvector-add (bvector-none)
                                                       top)
                                          (bvector-all))))
                    bblk*)
      (let loop ()
        (let lp ((i (gap-next bblk* (gap-top bblk*)))
                 (changes 0 #;#f))
          (if (gap-end? bblk* i)
              (unless (eqv? changes 0)
                (loop))
              #;
              (when changes
                (loop))
              (let* ((cblk (gap-ref bblk* i))
                     (pred*-doms (map bblk-doms (bblk-pred* cblk)))
                     (doms (bvector-add
                            (if (null? pred*-doms)
                                0
                                (apply bvector-intersection pred*-doms))
                            cblk))
                     (old (bblk-doms cblk)))
                ;; The new doms is the intersection of the
                ;; dominators of the predecessors of cblk.
                (set-bblk-doms! cblk doms)
                (lp (gap-next bblk* i)
                    (bitwise-ior (bitwise-xor doms old))
                    #;
                    (or changes (not (bvector=? doms old))))))))
      ;; Find natural loops
      (set! *loops* '())
      (gap-for-each (lambda (cblk)
                      (set-bblk-loop! cblk #f)
                      (set-bblk-loopnest! cblk 0))
                    bblk*)
      (gap-for-each
       (lambda (cblk)
         ;; See if this is a backedge to a loop header.
         (let ((head (find (lambda (blk)
                             ;; Head dominates current block?
                             (bvector-in? (bblk-doms cblk) blk))
                           (bblk-succ* cblk))))
           (when head
             ;; Construct the loop record (if none exists) and add
             ;; all blocks to the loop.
             (print "Backedge " (bblk-number cblk)
                    " -> " (bblk-number head))
             (unless (bblk-loop head)
               (let ((loop (new-loop head)))
                 (set-bblk-loop! head loop)
                 (set! *loops* (cons loop *loops*))))
             (gap-for-each set-bblk-not-done! bblk*)
             (set-bblk-done! head)
             (let ((loop (bblk-loop head)))
               (let lp ((cblk cblk))
                 (unless (bblk-done? cblk)
                   (set-bblk-done! cblk)
                   (unless (memq cblk (loop-blocks loop))
                     (add-to-loop! loop cblk))
                   (for-each lp (bblk-pred* cblk))))
               (add-to-loop! loop head)))))
       bblk*)
      ;; Sort by loop nesting level (deepest first)
      (set! *loops* (list-sort (lambda (x y)
                                 (> (bblk-loopnest (loop-header x))
                                    (bblk-loopnest (loop-header y))))
                               *loops*))))

  ;; TODO: locate (or create) pre-headers

;;; Liveness

  ;; Per block.
  (define (live-variables! bblk*)
    (gap-for-each (lambda (cblk)
                    (set-bblk-defs! cblk (varstate-none))
                    (set-bblk-uses! cblk (varstate-none))
                    (set-bblk-ins! cblk (varstate-none))
                    (set-bblk-outs! cblk (varstate-none)))
                  bblk*)
    ;; Calculate uses/defs
    (do ((i (gap-top bblk*) (gap-next bblk* i)))
        ((gap-end? bblk* i))
      (let* ((cblk (gap-ref bblk* i))
             (line* (bblk-line* cblk)))
        (let lp ((i (gap-top line*))
                 (defs (varstate-none))
                 (uses (varstate-none)))
          (cond ((gap-end? line* i)
                 (set-bblk-defs! cblk defs)
                 (set-bblk-uses! cblk uses))
                (else
                 (let* ((line (gap-ref line* i))
                        (uses* (var-union uses (var-set-minus (line-uses line) defs)))
                        (defs* (var-union defs (var-set-minus (line-sets line) uses*))))
                   (lp (gap-next line* i) defs* uses*)))))))
    ;; Calculate ins/outs. XXX: do this in reverse order so that
    ;; there will be fewer iterations?
    (print ";; Live variables")
    (let loop ()
      (let lp ((i (gap-top bblk*)) (changes #f))
        (if (gap-end? bblk* i)
            (when changes
              (loop))
            (let* ((cblk (gap-ref bblk* i))
                   (uses (bblk-uses cblk))
                   (defs (bblk-defs cblk))
                   (ins (bblk-ins cblk))
                   (outs* (var-set-minus
                           (fold-left var-union
                                      (varstate-none)
                                      (map bblk-ins
                                           (bblk-succ* cblk)))
                           *unavailable*))
                   (ins* (var-set-minus
                          (var-union uses (var-set-minus outs* defs))
                          *unavailable*)))
              (set-bblk-ins! cblk ins*)
              (set-bblk-outs! cblk outs*)
              (lp (gap-next bblk* i)
                  (or changes (not (var=? ins* ins)))))))))

  ;; Calculate live and dead variables/regs per line.
  (define (live/dead-variables! bblk*)
    (print ";; Live/dead variables")
    (do ((i (gap-bottom bblk*) (gap-prev bblk* i)))
        ((gap-over-top? bblk* i))
      (let* ((cblk (gap-ref bblk* i))
             (line* (bblk-line* cblk)))
        (let lp ((i (gap-bottom line*))
                 (live (bblk-outs cblk)))
          (unless (gap-over-top? line* i)
            (let* ((line (gap-ref line* i))
                   (live* (var-set-minus live (line-sets line))))
              (set-line-deads! line (var-set-minus
                                     (var-set-minus (line-uses line) live*)
                                     *unavailable*))
              (set-line-live-out! line live)
              (lp (gap-prev line* i)
                  (var-union live* (line-uses line)))))))))

;;; Save/restore live pseudo registers at marked locations

  (define (varstate-pseudos x)
    (let lp ((regs '())
             (x (bitwise-and x (bitwise-not (- (expt 2 (vector-length *register-names*)) 1)))))
      (let ((idx (bitwise-first-bit-set x)))
        (if (eqv? idx -1)
            regs
            (lp (cons (hashtable-ref *pseudoregs* idx #f) regs)
                (bitwise-and x (- x 1)))))))

  (define (save/restore-live-registers bblk*)
    (define save-regs (make-eqv-hashtable))
    (define (save-pseudo reg)
      (let* ((save-reg (new-pseudo-reg))
             (l (new-line `(mov ,save-reg ,reg) 0 0 0)))
        (hashtable-set! save-regs (pseudo-register-index reg) save-reg)
        (setup-line! l)
        l))
    (define (restore-pseudo reg)
      (let* ((save-reg (hashtable-ref save-regs (pseudo-register-index reg) #f))
             (l (new-line `(mov ,reg ,save-reg) 0 0 0)))
        (assert save-reg)
        (setup-line! l)
        l))
    (print ";; Insert saves/restore")
    (gap-for-each
     (lambda (cblk)
       (hashtable-clear! save-regs)
       (let lp ((line* (gap->list (bblk-line* cblk)))
                (ret '()))
         (if (null? line*)
             (set-bblk-line*! cblk (list->gap (reverse ret)))
             (let ((line (car line*)))
               (cond
                 ((equal? (line-inst line) '(%comment save-live-registers))
                  (lp (cdr line*)
                      (append (map save-pseudo (varstate-pseudos (line-live-out line)))
                              ret)))
                 ((equal? (line-inst line) '(%comment restore-live-registers))
                  (lp (cdr line*)
                      (append (map restore-pseudo (varstate-pseudos (line-live-out line)))
                              ret)))
                 (else
                  (lp (cdr line*)
                      (cons line ret))))))))
     bblk*))

;;; Dead assignment elimination

  ;; TODO: the code generator must probably be able to tell this
  ;; procedure about I/O. Or perhaps loads and stores can never be
  ;; removed, because they can trap. must at least loads from the
  ;; environment, stack and closures can be removed.
  (define (dead-assignment-elimination! bblk* opt++)
    (print ";; Dead assignment elimination")
    (gap-for-each
     (lambda (cblk)
       (let ((line* (bblk-line* cblk)))
         (let lp ((i (gap-bottom line*))
                  (live (bblk-outs cblk)))
           (unless (gap-over-top? line* i)
             (let* ((line (gap-ref line* i))
                    (uses (line-uses line))
                    (sets (line-sets line)))
               (let ((set-and-live
                      (var-inter sets (var-union live *unavailable*))))
                 ;; If the instruction has sets, but none of those
                 ;; sets are live, then this is a dead assignment.
                 (cond ((and (varempty? set-and-live)
                             (not (varempty? sets))
                             (or (arithmetic? line)
                                 (move? line)
                                 (cond-move? line)
                                 (safe-load? line)))
                        (print ";;; OPT: dead assignment")
                        (opt++)
                        (set-line-inst! line `(%comment dead ,(line-inst line)))
                        (setup-line! line)
                        (lp (gap-prev line* i) live))
                       (else
                        (lp (gap-prev line* i)
                            (var-union uses (var-set-minus live sets)))))))))))
     bblk*))

;;; Remove branch chains

  (define (remove-branch-chains! bblk*)
    (define (follow start branch)
      ;; Follow an acyclic branch chain until it reaches a non-branch
      ;; or a return.
      (let lp ((cblk start)
               (branch branch)
               (chain (cons start '()))
               (inst #f))
        (cond
          ((null? (bblk-succ* cblk))
           (print "no successors")
           inst)
          ((not (or (branch? branch) (jump? branch)))
           (print "not a branch/jump")
           inst)
          (else
           (when debug
             (print " #;cblk: " (bblk-number cblk)
                    " #;branch: " (line-inst branch)
                    " #;chain: " (map bblk-number chain)
                    " #;inst: " inst))
           (let* ((tblk (last (bblk-succ* cblk)))
                  (target-line (first-instruction tblk)))
             (cond ((or (memq tblk chain)
                        (not (eq? (label->target (bblk-label tblk))
                                  (branch-target branch))))
                    ;; Found a cycle or some other strange block.
                    (print ";; cycle")
                    inst)

                   ((and target-line (or (jump? target-line)
                                         (and (return? target-line) (jump? branch))))
                    ;; Follow a jump/return
                    (let ((inst^ (if (jump? target-line)
                                     (normal-branch (line-inst branch)
                                                    (branch-target target-line))
                                     (make-return))))
                      (when debug
                        (print ";; follow " (bblk-number tblk) ", " (line-inst branch) " => " inst^)
                        (print-line target-line))
                      (if (not inst^)
                          (begin (print ";; giving up") inst)
                          (lp tblk target-line (cons tblk chain) inst^))))

                   ((and (not target-line)
                         (pair? (bblk-succ* tblk)))
                    ;; Follow an empty block's fallthrough. Pretend
                    ;; that it contains a jump to its fallthrough
                    ;; block.
                    (let ((fblk (car (bblk-succ* tblk))))
                      (cond
                        ((and (bblk-label fblk)
                              (normal-branch (line-inst branch) (car (bblk-label fblk))))
                         =>
                         (lambda (new-branch)
                           (print ";; Trying branch chaining to fallthrough block "
                                    (bblk-number fblk) " => " new-branch)
                           (lp tblk branch (cons tblk chain) new-branch)))
                        (else inst))))
                   (else
                    (print ";; done")
                    inst)))))))
    ;; (requires setup-flow!)
    (when debug
      (renumber-blocks! bblk*))
    (gap-for-each
     (lambda (cblk)
       (let ((branch (last-instruction cblk)))
         (when branch
           (let ((inst (follow cblk branch)))
             (when inst
               (when debug
                 (print ";;; OPT: possible to remove a branch chain in block "
                        (bblk-number cblk) " => " inst)
                 (print-line branch))

               ;; (print-block cblk)
               ;; (print-block tblk)
               ;; Replace the branch target
               (set-line-inst! branch inst)
               (setup-line! branch)
               ;; Lazy
               (setup-flow! bblk*))))))
     bblk*))

;;; Avoid jumps by branch reversal

  (define (reverse-branches! bblk*)
    ;; (requires setup-flow!)
    (do ((i (gap-top bblk*) (gap-next bblk* i)))
        ((gap-end? bblk* i))
      (let ((cblk (gap-ref bblk* i)))
        (cond
          ((last-instruction cblk) =>
           (lambda (branch)
             ;; See if cblk ends with a branch, the fallthrough
             ;; block is merely a jump with no label before it, and
             ;; the branch target is the block after the
             ;; fallthrough.
             (when (branch? branch)
               (let* ((i+1 (gap-next bblk* i))
                      (i+2 (gap-next bblk* i+1)))
                 (unless (or (gap-end? bblk* i+1)
                             (gap-end? bblk* i+2))
                   (let* ((jblk (gap-ref bblk* i+1))
                          (tblk (gap-ref bblk* i+2))
                          (jump (first-instruction jblk)))
                     (when (and jump (jump? jump) (not (bblk-label jblk))
                                (eq? (label->target (bblk-label tblk))
                                     (branch-target branch))
                                ;; Try to not introduce branches to
                                ;; error handlers, but instead keep
                                ;; them as jumps.
                                (not (null? (bblk-succ* jblk))))
                       (let ((sblk (find-branch-target bblk* jump))
                             (inst (reverse-branch (line-inst branch)
                                                   (branch-target jump))))
                         (when inst
                           (print ";;; OPT: branch reversal possible.")
                           ;; (print-line branch)
                           ;; (print-line jump)
                           ;; (print inst)
                           ;; (print-block cblk)
                           ;; (print-block jblk)
                           ;; (print-block tblk)
                           ;; (when sblk
                           ;;   (print-block sblk))
                           ;; (print ";;;")
                           ;; Reverse the branch
                           ;; (invalidates find-loops!)
                           (set-line-inst! branch inst)
                           (setup-line! branch)
                           (set-bblk-succ*! cblk
                                            (if sblk
                                                (list jblk sblk)
                                                (list jblk)))
                           ;; Clear out the jump
                           (set-line-inst! jump `(%comment rev ,(line-inst jump)))
                           (setup-line! jump)
                           (set-bblk-succ*! jblk (list tblk))
                           ;; Fixup the predecessors
                           (when sblk
                             (set-bblk-pred*! sblk (remq jblk (bblk-pred* sblk))))
                           (set-bblk-pred*! tblk (remq cblk (bblk-pred* tblk)))
                           (bblk-pred*-add! tblk jblk)
                           (when sblk
                             (bblk-pred*-add! sblk cblk)))))))))))))))

;;; Avoid unnecessary jumps

  (define (remove-unnecessary-jumps! bblk*)
    (define (branches-to? branch bblk)
      (eq? (label->target (bblk-label bblk))
           (branch-target branch)))
    (define (find-fallthrough match? bblk* i)
      ;; Find a fallthrough block that matches the condition.
      (let ((i (gap-next bblk* i)))
        (if (gap-end? bblk* i)
            #f
            (let ((cblk (gap-ref bblk* i)))
              (cond ((match? cblk)
                     cblk)
                    ((bblk-has-no-effect? cblk)
                     (find-fallthrough match? bblk* i))
                    (else #f))))))
    (define (remove-pred-from-succ*! cblk)
      (for-each (lambda (pblk)
                  (set-bblk-pred*! pblk (remq cblk (bblk-pred* pblk))))
                (bblk-succ* cblk)))
    ;; (requires setup-flow!)
    (do ((i (gap-top bblk*) (gap-next bblk* i)))
        ((gap-end? bblk* i))
      (let ((cblk (gap-ref bblk* i)))
        (cond
          ((last-instruction cblk) =>
           (lambda (branch)
             ;; See if cblk ends with a branch or jump to the
             ;; fallthrough block.
             (when (or (branch? branch) (jump? branch))
               (let ((tblk (find-fallthrough (lambda (blk)
                                               (branches-to? branch blk))
                                             bblk* i)))
                 (when tblk
                   ;; The branch is to one of the fallthrough blocks.
                   (let ((fblk (find-fallthrough (lambda x #t) bblk* i)))
                     (when fblk
                       ;; (invalidates find-loops!)
                       (print ";;; OPT: unnecessary jump")
                       (set-line-inst! branch `(%comment unnecessary
                                                         ,(line-inst branch)))
                       (setup-line! branch)
                       (remove-pred-from-succ*! cblk)
                       (set-bblk-succ*! cblk (list fblk))
                       (bblk-pred*-add! fblk cblk))))))))))))

;;; Local copy propagation

  ;; This records register to register moves (copies). When a
  ;; register is later used as a source (but not destination) that
  ;; reference is replaced by the copy.

  (define *copy-propagator* (host-convention 'copy-propagator))

  (define (local-copy-propagation! bblk*)
    (define debug #f)
    (define (copyprop! cblk)
      (let ((line* (bblk-line* cblk)))
        (let lp ((i (gap-top line*))
                 (valid-dst (varstate-none))
                 (valid-src (varstate-none))
                 (sources '()))
          (unless (gap-end? line* i)
            (let* ((line (gap-ref line* i))
                   (potentials (var-inter (line-uses line) valid-dst)))
              ;; See if it's possible to do copy propagation.
              (when debug
                (print-line line)
                (print ";; valid-dst: " (varstate->list valid-dst)
                       " valid-src: " (varstate->list valid-src)
                       " sources: " sources))
              (unless (or (directive? line) (varempty? potentials)
                          (null? sources))
                (when debug
                  (print ";;; might do copy propagation. potentials: "
                         (varstate->list potentials))
                  (print ";; =>"))
                ;; The sources alist maps a line-sets to a
                ;; line-uses. This is needed to support stuff like
                ;; SPARC's FMOVD, which moves two registers at once.
                ;; The sources alist also includes the actual copy
                ;; instruction, which is needed to support the x86,
                ;; e.g. CMP AL, 7 where RAX is a copy of RDX. This
                ;; should become CMP DL, 7 and not CMP RDX, 7.

                ;; TODO: why did not r15 get propagated to the last line here?
                ;; (mov rcx (mem64+ rsp 0))                 ;sets: (rcx) uses: (P0 rsp) deads: ()
                ;; (mov r15 rcx)                            ;sets: (r15) uses: (rcx) deads: ()
                ;; (mov r15 (mem64+ rcx 133))               ;sets: (r15) uses: (rcx) deads: (rcx)
                (let ((new-inst (*copy-propagator* (line-inst line)
                                                   valid-dst valid-src
                                                   sources
                                                   (lambda ()
                                                     (print ";;; OPT: copy propagation")))))
                  (set-line-inst! line new-inst)
                  (setup-line! line)
                  (when debug
                    (print-line line))))
              ;; The sets no longer contain good copies.
              (let* ((sets (line-sets line))
                     (uses (line-uses line))
                     (valid-dst* (var-set-minus valid-dst sets))
                     (valid-src* (var-set-minus valid-src sets))
                     (sources* (remp (lambda (x)
                                       (or (not (varempty? (var-inter (car x) sets)))
                                           (not (varempty? (var-inter (cadr x) sets)))))
                                     sources)))
                (cond ((and (move? line)
                            (not (varempty? uses))
                            (not (varempty? sets)))
                       ;; Moves are where copies come from.
                       (cond ((var=? sets uses)
                              ;; Redundant move.
                              (print ";;; OPT: redundant move")
                              (set-line-inst! line `(%comment redundant
                                                              ,(line-inst line)))
                              (setup-line! line)
                              (lp (gap-next line* i) valid-dst valid-src sources))
                             (else
                              (when debug
                                (print ";; this makes a copy."))
                              (lp (gap-next line* i)
                                  (var-union valid-dst* sets)
                                  (var-union valid-src* uses)
                                  (cons (list sets uses (line-inst line)) sources*)))))
                      (else
                       (lp (gap-next line* i) valid-dst* valid-src* sources*)))))))))
    (print ";; Local copy propagation")
    (gap-for-each copyprop! bblk*))

;;; Peephole optimization

  ;; XXX: should this run until fixpoint here, or should the loop be
  ;; in main?
  (define (peephole-optimization! bblk* opt++)
    (define *branch-changes* #f)
    (define peephole-optimizer
      (host-convention 'peephole-optimizer))
    (define dummy (make-line #f #f 0 0 0 0))
    (define (replace! line inst)
      (when (or (jump? line) (branch? line)) (set! *branch-changes* #t))
      (if inst
          (set-line-inst! line inst)
          (set-line-inst! line `(%comment peephole ,(line-inst line))))
      (setup-line! line)
      (when (or (jump? line) (branch? line)) (set! *branch-changes* #t)))
    (define (peephole! cblk)
      (define line* (bblk-line* cblk))
      (define (next i)
        ;; (print (list 'next i))
        (if (gap-end? line* i)
            (values dummy i)
            (let ((line (gap-ref line* i)))
              ;; Directives never go into the window.
              (if (directive? line)
                  (next (gap-next line* i))
                  (values line (gap-next line* i))))))
      (let*-values (((a i) (next (gap-top line*)))
                    ((b i) (next i))
                    ((c i) (next i))
                    ((d i) (next i)))
        ;; Loop until there are no instructions in the window.
        (let loop ((i i) (a a) (b b) (c c) (d d))
          (unless (eq? a dummy)
            (let-values (((e i+1) (next i)))
              ;; (print (list i a b c d e i+1))
              (cond ((peephole-optimizer line-inst line-deads a b c d) =>
                     (lambda (inst*)
                       ;; The first argument in the list is a
                       ;; bitmask where each set bit indicates that
                       ;; the line was part of the match. Those
                       ;; lines will be removed from the basic
                       ;; block.
                       (let ((mask (car inst*))
                             (inst* (cdr inst*)))
                         (when debug
                           (print ";;; OPT: peephole optimization")
                           (print "#;Peephole: " (map (lambda (l)
                                                        (and l (line-inst l)))
                                                      (list a b c d))
                                  " => " inst* #\space
                                  (number->string mask 2)))
                         (opt++)
                         ;; TODO: make this general
                         (assert (pair? inst*))
                         (case mask
                           ((#b1)
                            (assert (null? (cdr inst*)))
                            (replace! a (car inst*)))
                           ((#b11)
                            (assert (null? (cddr inst*)))
                            (replace! a (car inst*))
                            (replace! b (cadr inst*)))
                           ((#b111)
                            (assert (null? (cdddr inst*)))
                            (replace! a (car inst*))
                            (replace! b (cadr inst*))
                            (replace! c (caddr inst*)))
                           (else
                            (error 'peephole! "TODO" mask)))
                         (loop i+1 b c d e))))
                    (else
                     ;; No match. Just slide the window.
                     (loop i+1 b c d e))))))))
    (when debug (print ";; Peephole optimizer"))
    (gap-for-each peephole! bblk*)
    (when *branch-changes*
      (setup-flow! bblk*)))

;;; Global register allocation

  ;; This is an attempt at a Chaitin-Briggs register allocator.

  ;; Briggs, P. (1992). Register allocation via graph coloring
  ;; (Doctoral dissertation).

  ;; Briggs, P., Cooper, K. D., & Torczon, L. (1994). Improvements to
  ;; graph coloring register allocation. ACM Transactions on
  ;; Programming Languages and Systems (TOPLAS), 16(3), 428-455.

  (define (leafy-procedure? bblk*)
    (not (gap-exists (lambda (bblk)
                       (gap-exists (lambda (line)
                                     (and (call? line)
                                          (not (equal? '(call stop-and-copy)
                                                       (line-inst line)))))
                                   (bblk-line* bblk)))
                     bblk*)))

  (define (global-register-allocation! bblk*)
    (define debug #f)
    (define highest-color 15)         ;FIXME: host convention
    (define k (- (+ highest-color 1) (bitwise-bit-count *unavailable*)))
    (define compatible-classes? (host-convention 'register-compatible-classes?))
    ;; The interference graphs have a triangular bit matrix, a
    ;; vector of degrees and a set of neighbors.
    (define (make-graph n)
      (vector (make-bytevector (fx+ (fxdiv (fx* n n)
                                           (fx* 2 8))
                                    1)
                               0)
              (make-vector n 0)
              (make-vector n '())))
    (define (graph-adj g) (vector-ref g 0))
    (define (graph-deg g) (vector-ref g 1))
    (define (graph-nei g) (vector-ref g 2))
    (define (adj-index i j)
      (if (fx<? i j)
          (fx+ i (fxarithmetic-shift-right (fx* j j) 1))
          (fx+ j (fxarithmetic-shift-right (fx* i i) 1))))
    (define (add-edge! g x y)
      (unless (fx=? x y)
        (let ((adj (graph-adj g))
              (k (adj-index x y)))
          (let ((byte (fxarithmetic-shift-right k 3))
                (bit (fxarithmetic-shift-left 1 (fxand k #b111))))
            (let ((bits (bytevector-u8-ref adj byte)))
              (when (fxzero? (fxand bits bit))
                (bytevector-u8-set! adj byte (fxior bits bit))
                (let ((deg (graph-deg g))
                      (nei (graph-nei g)))
                  (vector-set! deg x (fx+ (vector-ref deg x) 1))
                  (vector-set! deg y (fx+ (vector-ref deg y) 1))
                  (vector-set! nei x (cons y (vector-ref nei x)))
                  (vector-set! nei y (cons x (vector-ref nei y))))))))))
    (define (interfere? g x y)
      (or (fx=? x y)
          (let ((adj (graph-adj g))
                (k (adj-index x y)))
            (let ((byte (fxarithmetic-shift-right k 3))
                  (bit (fxarithmetic-shift-left 1 (fxand k #b111))))
              (let ((bits (bytevector-u8-ref adj byte)))
                (not (fxzero? (fxand bits bit))))))))
    (define (degree g x)
      (vector-ref (graph-deg g) x))
    (define (print-edges g reg)
      (let ((edges (vector-ref (graph-nei g) reg)))
        (print (register-index->name reg) " -- "
               (map register-index->name edges))))
    (define (print-graph g)
      (when debug
        (print ";; Interference graph:")
        (do ((i 0 (+ i 1)))
            ((= i (vector-length (graph-nei g))))
          (print-edges g i))))
    (define (build-graph! G cblk)
      #;
      (when debug
        (print ";; Building graph for this block:")
        (print-block cblk)
        (print ";;"))
      (let ((line* (bblk-line* cblk)))
        (let lp ((i (gap-bottom line*))
                 (live (bblk-outs cblk)))
          (unless (gap-over-top? line* i)
            (let* ((line (gap-ref line* i))
                   (live (if (copy? line)
                             (var-set-minus live (line-uses line))
                             live)))
              ;; Update graph
              #;
              (when debug
                (display "G: ")
                (print-line line))
              (varstate-for-each
               (lambda (reg1)
                 (varstate-for-each
                  (lambda (reg2)
                    (when (compatible-classes? reg1 reg2)
                      #;
                      (when debug
                        (print "Adding the edge " (register-index->name reg1)
                               " -- " (register-index->name reg2)))
                      (add-edge! G reg1 reg2)))
                  live))
               (line-sets line))
              (lp (gap-prev line* i)
                  (var-union (var-set-minus live (line-sets line))
                             (line-uses line))))))))
    (define pseudos (make-eqv-hashtable))
    (define (coalesce! bblk* graph)
      (define changes? #f)
      (define assignments (make-vector (vector-length (graph-nei graph))
                                       #f))
      (define (path-compress reg-idx)
        (let lp ((reg-idx reg-idx))
          (cond ((vector-ref assignments reg-idx) => lp)
                (else reg-idx))))
      (define (coalesce-line! line)
        ;; This looks at copy instructions (MOV reg,reg). If the
        ;; source and destination do not interfere with each other
        ;; then the instruction can be removed and the live ranges
        ;; can be coalesced.
        (when (copy? line)
          (let ((sets (line-sets line))
                (uses (line-uses line)))
            (when (and (= 1 (bitwise-bit-count sets))
                       (= 1 (bitwise-bit-count uses)))
              ;; XXX: normally this code would know which operand is
              ;; the source and which is the destination
              (let ((src (path-compress (bitwise-first-bit-set uses)))
                    (dst (path-compress (bitwise-first-bit-set sets))))
                (cond
                  ((var=? sets uses)
                   (print ";;; OPT: redundant move (coalesce)")
                   (when debug (print-line line))
                   (set-line-inst! line `(%comment redundant
                                                   ,(line-inst line)))
                   (setup-line! line))

                  ((not (interfere? graph src dst))
                   (let ((father (fxmin src dst))
                         (son (fxmax src dst)))
                     (when (and (fx>=? son (vector-length *register-names*))
                                (not (bitwise-bit-set? *unavailable* father)))
                       ;; Any mention of son should now be replaced by father.
                       (when debug
                         (print ";;; OPT: coalesce " (register-index->name father) " with "
                                (register-index->name son))
                         (print-line line))
                       ;; Save the pseudos involved so that
                       ;; rewrite-line! can find them. Hacky.
                       (let f ((x (cdr (line-inst line))))
                         (cond ((pseudo-register? x)
                                (hashtable-set! pseudos (pseudo-register-index x) x))
                               ((pair? x)
                                (f (car x))
                                (f (cdr x)))))
                       (set! changes? #t)
                       (set-line-inst! line `(%comment coalesced ,(line-inst line)))
                       (setup-line! line)
                       (vector-set! assignments son father)
                       (for-each (lambda (son-edge-reg)
                                   (add-edge! graph son-edge-reg father))
                                 (vector-ref (graph-nei graph) son))
                       ;; TODO: This is the slow way of doing it. The
                       ;; Briggs dissertation suggests a merge-find
                       ;; set.
                       (rewrite-blocks! bblk* assignments))))))))))
      (define (coalesce-block! cblk)
        (gap-for-each coalesce-line! (bblk-line* cblk)))

      ;; XXX: Currently only done for leaf procedures because
      ;; due to registers not being saved across calls, non-leaf
      ;; procedures are made worse than before.
      ;; FIXME: Run coalesce in the deepest loop first.
      (when (leafy-procedure? bblk*)
        (gap-for-each coalesce-block! bblk*)
        (when changes?
          (print ";; Assignments after coalescing:")
          (print assignments)
          (when debug
            (print ";;;;;;;;;;;;;;;;:")
            (gap-for-each print-block bblk*)
            (print ";;;;;;;;;;;;;;;;.")))
        (hashtable-clear! pseudos))
      changes?)
    (define (rewrite-line! line colors)
      (define changes #f)
      (unless (directive? line)
        (let* ((inst (line-inst line))
               (new-inst
                (cons (car inst)
                      (let lp ((op (cdr inst)))
                        (cond ((pseudo-register? op)
                               (let ((color (vector-ref colors (pseudo-register-index op))))
                                 (cond ((not color) ;for coalesce
                                        op)
                                       ((fx<? color (vector-length *register-names*))
                                        (unless (fx<=? color highest-color)
                                          (error 'rewrite-lines "Funny register assigned"
                                                 line color))
                                        (set! changes #t)
                                        (vector-ref *register-names* color))
                                       ((hashtable-ref pseudos color #f) =>
                                        (lambda (pseudo) ;for coalesce
                                          (set! changes #t)
                                          pseudo))
                                       (else
                                        ;; Also for the benefit of coalesce
                                        (error 'rewrite-lines
                                               "Unexpectedly assigned a pseudo register"
                                               color (hashtable-keys pseudos))))))
                              ((pair? op)
                               (cons (lp (car op))
                                     (lp (cdr op))))
                              (else op))))))
          (when changes
            (when debug
              (print (if (eqv? 0 (hashtable-size pseudos)) ";; Rewrite:" ";; Coalesce:"))
              (print-line line))
            (set-line-inst! line new-inst)
            (setup-line! line)
            (when debug
              (print-line line))))))
    (define (rewrite-blocks! bblk* colors)
      (gap-for-each (lambda (cblk)
                      (gap-for-each (lambda (line)
                                      (rewrite-line! line colors))
                                    (bblk-line* cblk)))
                    bblk*))
    (define (partition-graph g k spill-costs)
      ;; low: live ranges with degree < k
      ;; high: live ranges with degree >= k and finite spill cost
      (let* ((deg (graph-deg g))
             (low (make-set (vector-length deg)))
             (high (make-set (vector-length deg))))
        (do ((i (vector-length *register-names*) (fx+ i 1)))
            ((fx=? i (vector-length deg))
             (values low high))
          (cond ((fx<? (vector-ref deg i) k)
                 ;; Less than k edges means we can easily assign
                 ;; this a color.
                 (set-add! low i))
                ((not (eq? 'inf (vector-ref spill-costs i)))
                 (set-add! high i))))))
    (define (allocate! G regs spill-costs)
      ;; "Simplify". This colors the interference graph with an
      ;; approximation ratio of two. Remove nodes of degree < k and
      ;; put them on the stack. If there are only(?) nodes of degree
      ;; => k, then insert spill code. TODO: k=16 is the number of
      ;; registers in the integer class, in this case. Other classes
      ;; should also be colorable.
      (define stack (make-set (vector-length (graph-deg G))))
      (define stack-optimism (make-set (vector-length (graph-deg G))))
      (define push! set-add!)
      (define (pop! s)
        (let ((ret (set-choose-one s)))
          (set-delete! s ret)
          ret))
      (define stack->list set->list)
      (define stack-empty? set-empty?)
      (let-values ([(low high) (partition-graph G k spill-costs)])
        (define (simplify hopeful?)
          (when debug
            (print "stack: " (map register-index->name (stack->list stack))
                   " stack-optimism: " (map register-index->name (stack->list stack-optimism))
                   " low: " (map register-index->name (set->list low))
                   " high: " (map register-index->name (set->list high))))
          (cond ((not (set-empty? low))
                 (let ((m (set-choose-one low))
                       (nei (graph-nei G))
                       (deg (graph-deg G)))
                   ;; Remove m from G, updating low and high.
                   (set-delete! low m)
                   (if hopeful?
                       (push! stack-optimism m)
                       (push! stack m))
                   (let lp* ((n* (vector-ref nei m)))
                     (cond ((pair? n*)
                            (let* ((j (car n*))
                                   (degree (fx- (vector-ref deg j) 1)))
                              (vector-set! deg j degree)
                              ;; Pseudo registers pass from high to
                              ;; low if the new degree is less than k.
                              (when (and (set-member? high j)
                                         (fx=? degree (fx- k 1)))
                                (when debug
                                  (print "degree of " (register-index->name j)
                                         " is now " degree "≤" k))
                                (set-add! low j)
                                (set-delete! high j))
                              (lp* (cdr n*))))
                           (else
                            (simplify hopeful?))))))
                ((not (set-empty? high))
                 ;; One of the registers in high will probably need
                 ;; to be spilled. Optimistically move a register
                 ;; from high to low.
                 (let ((r* (set->list high))
                       (deg (graph-deg G)))
                   (let lp ((r* (cdr r*))
                            (cheapest (car r*))
                            (lowest-cost (/ (vector-ref spill-costs (car r*))
                                            (vector-ref deg (car r*)))))
                     (cond ((pair? r*)
                            (let ((r (car r*)))
                              (let ((cost (/ (vector-ref spill-costs r)
                                             (vector-ref deg r))))
                                (if (< cost lowest-cost)
                                    (lp (cdr r*) r cost)
                                    (lp (cdr r*) cheapest lowest-cost)))))
                           (else
                            (when debug
                              (print "Lowest spill cost: "
                                     (register-index->name cheapest)))
                            (set-add! low cheapest)
                            (set-delete! high cheapest)
                            (simplify 'hopeful))))))
                (else
                 (select))))
        (define (select)
          ;; Select colors.
          (let ((colors (make-vector regs highest-color))
                ;;FIXME: used only needs to be as large as the index
                ;;of the highest assignable hardware register + 1
                (used (make-vector (fx+ highest-color 1)))
                (nei (graph-nei G)))
            ;; Hardware registers are assigned themselves.
            (do ((i 0 (fx+ i 1)))
                ((fx=? i (vector-length *register-names*)))
              (vector-set! colors i i))
            (let lp ((spill* '()))
              (when debug
                (print "Select: " (map register-index->name (stack->list stack))))
              (cond ((not (stack-empty? stack))
                     (let ((m (pop! stack)))
                       (cond ((let ((cost (vector-ref spill-costs m)))
                                (and (not (eq? cost 'inf)) (negative? cost)))
                              (when debug
                                (print "\nWill always spill " (register-index->name m)))
                              (lp (cons m spill*)))
                             (else
                              (when debug
                                (print "\nFind a color for " (register-index->name m)))
                              (vector-fill! used #f)
                              (for-each (lambda (n)
                                          (when debug
                                            (let ((assigned (vector-ref colors n)))
                                              (display (cons (register-index->name n)
                                                             (if (= assigned highest-color)
                                                                 'none
                                                                 (register-index->name
                                                                  assigned))))))
                                          (vector-set! used (vector-ref colors n) #t))
                                        (vector-ref nei m))
                              ;; XXX: Looping over 0..highest works
                              ;; because the integer registers are
                              ;; first.
                              (let ((all (var-set-minus *all-regs* *unavailable*)))
                                (let lp* ((c 0)
                                          (must-use (var-set-minus *scratch*
                                                                   *unavailable*)))
                                  ;; Try callee-save first.
                                  (cond ((fx=? c highest-color)
                                         (cond ((= must-use all)
                                                (when debug
                                                  (print "\n;; No color for " (register-index->name m)))
                                                (lp (cons m spill*)))
                                               (else
                                                (lp* 0 all))))
                                        ((and (not (vector-ref used c))
                                              (bitwise-bit-set? must-use c))
                                         (when debug
                                           (print "\nRegister " (register-index->name m)
                                                  " gets color " c " ("
                                                  (vector-ref *register-names* c) ")"))
                                         (vector-set! colors m c)
                                         (lp spill*))
                                        (else
                                         (lp* (fx+ c 1) must-use)))))))))
                    ((not (stack-empty? stack-optimism))
                     (when debug
                       (print "Turning optimistic!"))
                     (set-copy! stack stack-optimism)
                     (set-clear! stack-optimism)
                     (lp spill*))
                    ((pair? spill*)
                     ;; After spilling there must be renumbering etc
                     (spill! bblk* spill*)
                     'spills)
                    (else
                     (when debug
                       (print "Register allocation resulted in these colors:\n" colors))
                     ;; Verify that no register was forgotten
                     (do ((i (vector-length *register-names*) (fx+ i 1)))
                         ((fx=? i (vector-length colors)))
                       (when (= highest-color (vector-ref colors i))
                         (error 'global-register-allocation!
                                "Wrong colors allocated" colors)))
                     (rewrite-blocks! bblk* colors)
                     'no-spills)))))
        (simplify #f)))

    (define (spill! bblk* spill*)
      (define spill-set
        (fold-left var-union (varstate-none)
                   (map (lambda (x) (expt 2 x))
                        spill*)))
      (define (spill-block! cblk)
        (when debug
          (print "\n;; Spilling in a block."))
        (let ((line* (bblk-line* cblk)))
          (let lp ((i (gap-bottom line*))
                   (live (var-inter (bblk-outs cblk) spill-set))
                   (ret '()))
            (cond ((not (gap-over-top? line* i))
                   (let* ((line (gap-ref line* i))
                          (live* (var-set-minus live (line-sets line))))
                     (when debug
                       (display "S: ")
                       (print-line line)
                       (print "live: " (varstate->list live)
                              " live*: " (varstate->list live*)))
                     (let* ((to-store (var-inter (line-sets line) spill-set))
                            (to-load (var-set-minus (var-inter (line-uses line)
                                                               spill-set)
                                                    0 #;live*))
                            (stores
                             (varstate-map
                              (lambda (r)
                                ;; Insert stores after definitions
                                (when debug
                                  (print ";; inserting store: " (register-index->name r)))
                                (make-stack-store r (line-inst line)))
                              to-store))
                            (loads
                             (varstate-map
                              (lambda (r)
                                ;; Insert loads before uses
                                (when debug
                                  (print ";; inserting load: " (register-index->name r)))
                                (make-stack-load r (line-inst line)))
                              to-load)))
                       ;; FIXME: insert the loads that are taken
                       ;; into account by need-load in compute-spill-costs
                       (lp (gap-prev line* i)
                           (var-inter (var-union live* (line-uses line))
                                      spill-set)
                           (append loads (cons line (append stores ret)))))))
                  (else
                   ;; ret contains the original code + spills.
                   (let ((new-line* (list->gap ret)))
                     (when debug
                       (print ";; After spills were inserted: ")
                       (gap-for-each print-line new-line*))
                     (set-bblk-line*! cblk new-line*))
                   #f)))))
      (when debug
        (print "\n;; Spilling registers: " (varstate->list spill-set)))
      (gap-for-each spill-block! bblk*))

    ;; Returns a vector of spill costs. The cost is a real number or
    ;; 'inf if the register must not be spilled. Based on § 8.7 in
    ;; Briggs' thesis.
    (define (compute-spill-costs bblk* regs)
      ;; "Live ranges that are live and have been used since the last
      ;; death."
      (define need-load (make-set regs))
      ;; "Live ranges that are live across a death …"
      (define must-spill (make-set regs))
      (define live (make-set regs))     ;current live range
      (define loads (make-vector regs 0)) ;spill cost
      (define stores (make-vector regs 0)) ;spill cost
      (define copies (make-vector regs 0)) ;spill cost
      ;; "At a definition … is not a member of mustSpill, then the
      ;; definition must be close to all uses …."
      (define infinite-costs (make-vector regs #f))
      (define (costs-block! cblk)
        (define line* (bblk-line* cblk))
        (define depth (+ (expt 10 (bblk-loopnest cblk)) 1))
        (set-clear! need-load)
        (set-clear! live)
        (varstate-for-each (lambda (reg) (set-add! live reg))
                           (bblk-outs cblk))
        (set-copy! must-spill live)
        ;; Loop backwards over the lines
        (do ((i (gap-bottom line*) (gap-prev line* i)))
            ((gap-over-top? line* i))
          ;; TODO: look for copies (if a register in a copy is
          ;; spilled then the copy can be removed)
          (let ((line (gap-ref line* i)))
            ;; Sets
            (varstate-for-each
             (lambda (r)
               (when (set-member? need-load r)
                 (set-delete! need-load r)
                 ;; FIXME: Disabled, broken, nothing acts on this.
                 #;
                 (when (not (set-member? must-spill r))
                   ;; The definition is close to all uses of the live
                   ;; range.
                   (vector-set! infinite-costs r #t)))
               (vector-set! stores r (+ depth (vector-ref stores r)))
               (set-delete! live r))
             (line-sets line))
            ;; Deaths
            (varstate-for-each
             (lambda (r)
               (when (not (set-member? live r))
                 (for-each (lambda (m)
                             (vector-set! loads r (+ depth (vector-ref loads r)))
                             (set-add! must-spill m))
                           (set->list need-load))
                 (set-clear! need-load)))
             (line-uses line))
            ;; Uses
            (varstate-for-each
             (lambda (r)
               (set-add! live r)
               (set-add! need-load r))
             (line-uses line))))
        ;; Weigh by the loop nesting depth.
        (set-for-each (lambda (r)
                        (vector-set! loads r (+ depth (vector-ref loads r))))
                      need-load))
      ;; Compute the costs and then fill in spill-costs
      (gap-for-each costs-block! bblk*)
      (when debug
        (print " ;; Computed costs:"))
      (do ((spill-costs (make-vector regs 'bug))
           (i (vector-length *register-names*) (fx+ i 1)))
          ((fx=? i regs)
           spill-costs)
        (let ((cost (if (vector-ref infinite-costs i)
                        'inf
                        (- (+ (vector-ref loads i)
                              (vector-ref stores i))
                           (vector-ref copies i)))))
          (when debug
            (print (register-index->name i) ": " cost))
          (vector-set! spill-costs i cost))))

    (define (renumber-pseudo-registers! bblk*)
      (define max-regs 0)
      (define regs (make-eq-hashtable))
      (define (renumber-line! line)
        (unless (directive? line)
          (let lp ((x (line-inst line)))
            (cond ((pseudo-register? x)
                   (cond ((hashtable-ref regs x #f))
                         (else
                          ;; This register has not been seen yet,
                          ;; so give it a new number and remember
                          ;; that it has been seen.
                          (set-car! (cdr x) max-regs)
                          (set! max-regs (fx+ max-regs 1))
                          (hashtable-set! regs x #t))))
                  ((pair? x)
                   (lp (car x))
                   (lp (cdr x)))))
          (setup-line! line)))
      (gap-for-each (lambda (cblk)
                      (gap-for-each renumber-line! (bblk-line* cblk)))
                    bblk*)
      (+ (vector-length *register-names*)
         max-regs))

    (define (register-alloc-main)
      (when debug
        (print ";;; Global register allocation."))

      (let ((regs (renumber-pseudo-registers! bblk*)))
        (when debug
          (print ";; There are " regs " registers"))
        (let lp ((pass 0))
          (live-variables! bblk*)
          (live/dead-variables! bblk*)
          (when debug
            (print ";; Building interference graph for pass " pass))
          (let ((G (make-graph regs)))
            (gap-for-each (lambda (cblk) (build-graph! G cblk)) bblk*)
            (print-graph G)
            (cond ((and (eqv? pass 0) (coalesce! bblk* G))
                   (print ";; Coalesced some registers")
                   (lp (+ pass 1)))
                  (else
                   (let ((spill-costs (compute-spill-costs bblk* regs)))
                     (if (eq? (allocate! G regs spill-costs) 'spills)
                         (lp (+ pass 1))))))))))
    (register-alloc-main))

;;; Assign locations to local variables, fix entry and exit.

  ;; TODO: it is possible to build an interference graph for spill
  ;; place holders and color it with an unlimited number of colors
  ;; (representing stack locations).

  ;; XXX: the whole case-lambda has only one frame size because of
  ;; what (loko arch amd64 tables) does.

  (define (assign-stack-locations! bblk*)
    (define si 0)
    (define ids (make-eqv-hashtable))
    (define LOCALS #f)
    (define RESERVE 0) ;reserved for incoming/outgoing (tail) arguments
    (define (make-stack-reference offset)
      ;; TODO: host-convention
      `(mem64+ rsp ,(* offset 8)))
    (define (decrease-sp amount)
      ;; TODO: host-convention
      (if (zero? amount)
          '(%comment no-locals)
          `(sub rsp ,(* amount 8))))
    (define (increase-sp amount)
      ;; TODO: host-convention
      (if (zero? amount)
          '(%comment no-locals)
          `(add rsp ,(* amount 8))))
    (define (assign id)
      (or (hashtable-ref ids id #f)
          (let ((loc (make-stack-reference si)))
            (hashtable-set! ids id loc)
            (set! si (+ si 1))
            loc)))
    (define (assign-line! line)
      (let ((inst (line-inst line)))
        (when (exists spill-place-holder? inst)
          (let ((new-inst (cons (car inst)
                                (map (lambda (op)
                                       (if (spill-place-holder? op)
                                           (assign (spill-identifier op))
                                           op))
                                     (cdr inst)))))
            (set-line-inst! line new-inst)
            (setup-line! line)))
        (when (and (eq? (car inst) '%comment)
                   (eq? (cadr inst) 'reserve-frame-space))
          ;; In this first pass of the blocks, also look for how
          ;; much space must be reserved for outgoing arguments.
          (let ((args-space (caddr inst)))
            (set! RESERVE (max RESERVE args-space))
            (print ";; Reserving space for " RESERVE " arguments.")))))
    (define (fixup-sp! line)
      (let ((inst (line-inst line)))
        (cond ((and (eq? (car inst) '%comment)
                    (eq? (cadr inst) 'allocate-frame))
               ;; si is now one more than the number of slots needed. The
               ;; last slot is used for the return address.
               (let* ((args-space (max RESERVE (caddr inst)))
                      (new-inst (decrease-sp (+ si args-space))))
                 (set! LOCALS (+ si args-space))
                 ;; TODO: align the frame size?
                 (print ";; Stack frame contains " LOCALS " slots.")
                 (set-line-inst! line new-inst)
                 (setup-line! line)))
              ((equal? inst '(%comment deallocate-frame))
               (unless LOCALS
                 (error 'assign-stack-locations!
                        "Internal error in the code generator"))
               (let ((new-inst (increase-sp LOCALS)))
                 (set-line-inst! line new-inst)
                 (setup-line! line)))
              ((equal? inst '(%comment liveness-information))
               ;; The liveness annotations need to know the size of
               ;; the stack frames.
               (set-line-inst! line `(%comment liveness ,(or LOCALS 0))))
              ((equal? inst '(%comment frame-size))
               ;; For (loko arch amd64 tables).
               (set-line-inst! line `(%comment frame-size ,(or LOCALS 0))))
              (LOCALS
               (let ((new-inst (let lp ((x inst))
                                 (cond ((pair? x)
                                        (cons (lp (car x))
                                              (lp (cdr x))))
                                       ((eq? x 'LOCALS)
                                        ;; XXX: host-convention
                                        (* 8 LOCALS))
                                       (else x)))))
                 (set-line-inst! line new-inst))))))
    ;; Find the number of stack frame slots needed
    (gap-for-each (lambda (cblk)
                    (gap-for-each assign-line! (bblk-line* cblk)))
                  bblk*)
    (gap-for-each (lambda (cblk)
                    (gap-for-each fixup-sp! (bblk-line* cblk)))
                  bblk*))

;;; Stack location liveness annotation for the GC

  ;; The liveness annotator translates the liveness information into
  ;; an instruction sequence that the GC can use to see which stack
  ;; positions are live.
  (define *liveness-annotator* (host-convention 'liveness-annotator))

  (define (liveness-annotations! bblk*)
    (gap-for-each
     (lambda (cblk)
       (let ((line* (bblk-line* cblk)))
         (let lp ((i (gap-bottom line*))
                  (live (bblk-outs cblk)))
           (unless (gap-over-top? line* i)
             (let* ((line (gap-ref line* i))
                    (uses (line-uses line))
                    (sets (line-sets line))
                    (inst (line-inst line)))
               (when (and (directive? line)
                          (eq? (car inst) '%comment)
                          (eq? (cadr inst) 'liveness))
                 (let ((frame-size (caddr inst)))
                   ;; The loop here has calculated liveness based on
                   ;; live out. The code generator now wants this
                   ;; comment (previously modified by
                   ;; assign-stack-locations!) to be replaced with
                   ;; an annotation of some sort that the GC can use
                   ;; to see which stack locations contain valid
                   ;; Scheme objects.
                   (let ((stack-live (var-set-minus live *all-regs*)))
                     (set-line-inst! line (*liveness-annotator* stack-live
                                                                frame-size)))))
               (lp (gap-prev line* i)
                   (var-union uses (var-set-minus live sets))))))))
     bblk*))

;;; Aligning branch targets

  ;; This finds all branch targets and aligns them to 16 bytes. It
  ;; very likely aligns too many targets. This is done as an attempt
  ;; to get smaller variations in performance due to code alignment.

  (define (align-branch-targets! bblk*)
    (gap-for-each
     (lambda (cblk)
       (let ((pred* (bblk-pred* cblk)))
         (cond ((eqv? (bblk-number cblk) 0)
                ;; The first block must be aligned so that the
                ;; general entry point address is fixnum.
                (set-bblk-align! cblk 8))
               ((or (null? pred*)
                    (and (pair? pred*) (null? (cdr pred*))
                         (= (bblk-number (car pred*))
                            (- (bblk-number cblk) 1))))
                ;; The block has zero preds or one predecessor which
                ;; is the immediate predecessor.
                (set-bblk-align! cblk 0))
               ((bblk-loop cblk) =>
                (lambda (loop)
                  (set-bblk-align! cblk (if (eq? cblk (loop-header loop))
                                            16 0))))
               (else
                (set-bblk-align! cblk 0)))))
     bblk*))

;;; Basic block reordering

  ;; This reorders basic blocks to increase the probability that the
  ;; branches will be not-taken. Branches to error handlers and the
  ;; GC are unlikely to be taken. Basic blocks in loops are made
  ;; consecutive if possible.

  (define (print-blocks-graph bblk* fn)
    (when #f
      (if (file-exists? fn)
          (delete-file fn))
      (with-output-to-file fn
        (lambda ()
          (print "digraph G {")
          (gap-for-each (lambda (cblk)
                          (unless (and (null? (bblk-succ* cblk))
                                       (null? (bblk-pred* cblk)))
                            (print #\" (bblk-number cblk) #\"
                                   (cond
                                     ((bblk-loop cblk) =>
                                      (lambda (loop)
                                        (if (eq? cblk (loop-header loop))
                                            " [shape=diamond,color=green]"
                                            " [color=blue]"))) ;XXX: why isn't this used?
                                     ((positive? (bblk-loopnest cblk))
                                      " [color=green]")
                                     ((bblk-immortal? cblk)
                                      " [color=blue]")
                                     (else
                                      ""))
                                   #\;)
                            (for-each (lambda (s)
                                        (let ((last (last-instruction cblk)))
                                          (print #\" (bblk-number cblk) #\"
                                                 " -> " #\" (bblk-number s) #\"
                                                 " [label=" #\"
                                                 (if (and (line? last)
                                                          (branch? last)
                                                          (eq? s (find-branch-target bblk* last)))
                                                     (car (line-inst last))
                                                     "")
                                                 #\" "];")))
                                      (bblk-succ* cblk))))
                        bblk*)
          (print "}")))))

  (define (block-unlikely? cblk)
    (gap-exists (lambda (x)
                  (let ((i (line-inst x)))
                    (or (equal? i '(%comment unlikely))
                        #;(equal? (car i) 'call))))
                (bblk-line* cblk)))

  (define (block-likely? cblk)
    (gap-exists (lambda (x)
                  (let ((i (line-inst x)))
                    (equal? i '(%comment likely))))
                (bblk-line* cblk)))

  (define (best-successor cblk succ*)
    ;; There is at least one successor and the best one should be
    ;; returned. Guesses must be made.
    (let ((loop (bblk-loop cblk)))
      (cond ((null? (cdr succ*))
             (print "No choice, using " (bblk-number (car succ*)))
             (car succ*))
            ;; If one of the blocks has already been emitted, then the
            ;; other block should be used.
            ((bblk-done? (cadr succ*)) (car succ*))
            ((bblk-done? (car succ*)) (cadr succ*))
            ;; Blocks that call the GC should not be fallthrough.
            ((block-unlikely? (car succ*))
             (print "Block " (bblk-number (car succ*)) " is unlikely")
             (cadr succ*))
            ((block-unlikely? (cadr succ*))
             (print "Block " (bblk-number (cadr succ*)) " is unlikely")
             (car succ*))
            ((block-likely? (car succ*))
             (print "Block " (bblk-number (car succ*)) " is likely")
             (car succ*))
            ;; Falling through to your loop header is not very nice
            ((and loop (eq? (cadr succ*) (loop-header loop)))
             (print "Not the loop header")
             (car succ*))
            ;; XXX: maybe always pick the one with the deepest loop nesting level?
            ((fx>=? (bblk-loopnest cblk) (bblk-loopnest (cadr succ*)))
             ;; There was a branch to a block that's in the same loop
             ;; nesting level, pick that one as the new fallthrough.
             (print "Using the deeper loop")
             (cadr succ*))
            ((fx>? (gap-bottom (bblk-line* (car succ*)))
                   (gap-bottom (bblk-line* (cadr succ*))))
             ;; The block with the fewer instructions might be a
             ;; nice fallthrough.
             (print "Using the shorter block")
             (cadr succ*))
            (else
             ;; Pick whatever was the fallthrough before.
             (print "Using the default")
             (car succ*)))))

  (define (setup-branch! bblk* cblk fallthrough target alternative*)
    ;; Updates the last line so that the block falls through or
    ;; jumps to fallthrough and branches to branch*. If there's an
    ;; unconditional jump to the fallthrough then it can be removed.
    ;; If there was a conditional branch to fallthrough, then it
    ;; should be reversed and jump to branch* instead. The blocks in
    ;; branch* might need to have labels created for them.
    (assert (or (null? alternative*) (null? (cdr alternative*))))
    (let ((last (last-instruction cblk)))
      (cond ((and (eq? fallthrough target)
                  (null? alternative*))
             (when (and (line? last) (or (branch? last) (jump? last))
                        (find-branch-target bblk* last))
               ;; The fallthrough is the only successor, so this
               ;; branch or jump can be removed.
               (set-line-inst! last `(%comment reordered ,(line-inst last)))
               (setup-line! last)))
            ((or (not (line? last))
                 (not (or (branch? last)
                          (jump? last))))
             ;; There is no branch/jump at the end of this block, so
             ;; insert one. It is not possible for this block to
             ;; require a branch, because it did not previously
             ;; contain one (what condition would it branch on?).
             ;; TODO: host convention
             (let ((new (new-line `(jmp ,(label->target (bblk-label target)))
                                  0 0 0))
                   (line* (bblk-line* cblk)))
               ;; The gap buffers were supposed to be better than
               ;; this...
               (when debug
                 (print "block " (bblk-number cblk) " gets a new jump"))
               (assert (null? alternative*))
               (setup-line! new)
               (set-gap-buffer! line*
                                (list->vector
                                 (append (vector->list (gap-buffer line*))
                                         (list new))))))
            ((jump? last)
             ;; Same reasoning as above.
             (when debug
               (print "block " (bblk-number cblk) " gets a rewritten jump"))
             (assert (null? alternative*))
             (set-line-inst! last (normal-branch (line-inst last)
                                                 (label->target (bblk-label target)))))

            ((not fallthrough)
             (assertion-violation 'setup-branch! "Branch with no fallthrough"
                                  (bblk-number cblk) last))

            ;; There is a branch at the end of the block. Now the
            ;; job is to rewrite it if the target of the old branch
            ;; was to the new fallthrough.
            ((eq? (label->target (bblk-label fallthrough))
                  (branch-target last))
             (when debug
               (print "block " (bblk-number cblk) " needs reversal"))
             (let ((reversed (reverse-branch (line-inst last)
                                             (label->target (bblk-label (car alternative*))))))
               (unless reversed
                 ;; TODO: it would be nice if this wasn't necessary,
                 ;; if this could just abort the optimization
                 ;; attempt...
                 (assertion-violation 'setup-branch! "Could not reverse a new branch"
                                      (line-inst last)))
               (set-line-inst! last reversed)
               (setup-line! last)))
            (else #f))))

  (define (reorder-blocks! bblk*)
    (define (walk-successors cblk wait* ret*)
      ;; Walk the call graph by starting at the entry block and
      ;; following preferred successors. The call graph is not
      ;; updated during this walk, that must be done by setup-flow!.
      (when debug
        (print ";;; Walking from block: " (bblk-number cblk) " succ*: "
               (map bblk-number (bblk-succ* cblk)))
        (print ";; wait*: " (map bblk-number wait*)
               " ret*: " (map bblk-number ret*)))
      (unless (bblk-done? cblk)
        (let ((succ* (bblk-succ* cblk))
              (ret* (cons cblk ret*)))
          (set-bblk-done! cblk)
          (cond
            ((null? succ*)
             ;; This block has no successors, so one of the blocks
             ;; in the wait list must be visited now.
             (pop-a-block wait* ret*))
            (else
             (let* ((fallthrough (best-successor cblk succ*))
                    (branch* (remq fallthrough succ*))
                    (wait* (append-blocks (remp bblk-done? branch*)
                                          (remq fallthrough wait*))))
               (when debug
                 (print ";;  best fallthrough of " (bblk-number cblk) " is "
                        (bblk-number fallthrough) ", other branches: "
                        (map bblk-number branch*)))
               (cond
                 ((and (bblk-done? fallthrough)
                       (null? branch*))
                  ;; The preferred fallthrough is already in ret*, but
                  ;; there aren't any branches.
                  (setup-branch! bblk* cblk #f fallthrough branch*)
                  (pop-a-block wait* ret*))
                 ((bblk-done? fallthrough)
                  ;; The preferred fallthrough is already in ret* and
                  ;; can't be placed there again (unless it's copied).
                  ;; Make a new fallthrough. TODO: host convention
                  (let* ((jump (new-line `(jmp ,(label->target (bblk-label fallthrough)))
                                         0 0 0))
                         (jblk (new-bblk (list (vector 'reorder-jump))
                                         (list->gap (list jump)))))
                    (hashtable-set! *branch-targets* (car (bblk-label jblk)) jblk)
                    (setup-line! jump)
                    (setup-branch! bblk* cblk fallthrough jblk branch*)
                    (pop-a-block wait* (cons jblk ret*))))
                 (else
                  (setup-branch! bblk* cblk fallthrough fallthrough branch*)
                  (walk-successors fallthrough wait* ret*)))))))))
    (define (append-blocks x* wait*)
      ;; TODO: Use a double-ended queue instead.
      (if (null? x*)
          wait*
          (cond ((bblk-done? (car x*))
                 (append-blocks (cdr x*) wait*))
                ((block-unlikely? (car x*))
                 ;; Process unlikely blocks last, so that they
                 ;; hopefully go to the end of the procedure.
                 (append-blocks (cdr x*)
                                (append wait* (list (car x*)))))
                (else
                 (append-blocks (cdr x*)
                                (cons (car x*) wait*))))))
    (define (pop-a-block wait* ret*)
      (if (pair? wait*)
          (if (bblk-done? (car wait*))
              (pop-a-block (cdr wait*) ret*)
              (walk-successors (car wait*) (cdr wait*) ret*))
          (finish-up ret*)))
    (define (finish-up ret*)
      ;; Last add all blocks that are IMMORTAL but not DONE
      (let lp ((i (gap-top bblk*))
               (ret* ret*))
        (cond ((not (gap-end? bblk* i))
               (let ((cblk (gap-ref bblk* i))
                     (i (gap-next bblk* i)))
                 (cond ((and (bblk-immortal? cblk)
                             (not (bblk-done? cblk)))
                        (set-bblk-done! cblk)
                        (lp i (cons cblk ret*)))
                       (else
                        ;; Also removes unreachable blocks.
                        (lp i ret*)))))
              (else
               (set-gap-buffer! bblk* (list->vector
                                       (reverse ret*)))))))
    (when debug
      (print ";;; Block reordering"))
    (unless (< *number-of-blocks* 3)
      (print-blocks-graph bblk* "before.dot")
      (let ((l 0))
        (gap-for-each (lambda (cblk)
                        (set-bblk-not-done! cblk)
                        (unless (bblk-label cblk)
                          (set-bblk-label! cblk (list (vector 'label l)))
                          (set! l (+ l 1))))
                      bblk*))
      (walk-successors (gap-ref bblk* (gap-top bblk*))
                       '() '())
      (renumber-blocks! bblk*)
      (setup-flow! bblk*)
      (print-blocks-graph bblk* "after.dot")))

;;; Remove trash left from some optimization passes

  (define (remove-trash bblk*)
    (print ";; Remove trash")
    (gap-for-each
     (lambda (cblk)
       (gap-filter!
        (lambda (line)
          (match (line-inst line)
            [('%comment (or 'peephole 'redundant 'coalesced
                            'dead 'rev 'unnecessary 'reordered) . _)
             #f]
            [_ #t]))
        (bblk-line* cblk)))
     bblk*))

;;;

  (main text data)))
