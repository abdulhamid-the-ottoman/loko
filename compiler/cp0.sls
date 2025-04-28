;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Compiler pass 0

;; Dead code elimination, constant propagation, constant folding, copy
;; propagation and procedure inlining.

;; TODO: the singly-referenced stuff, and the recursive stuff

;; TODO: important: unique labels when duplicating case-lambdas (for
;; the whole lambda and for each case, too).

(library (loko compiler cp0)
  (export
    pass-cp0 cp0-size-limit cp0-effort-limit
    ;; These two for pass-loops:
    operand? operand-value)
  (import
    (rename (loko runtime utils) (map-in-order map))
    (loko compiler recordize)
    (loko match)
    (psyntax compat)              ;for define-record
    (except (rnrs) map)
    (rnrs eval))

(define who 'pass-cp0)

;; TODO: do this properly (from config?)
(define (target-fixnum? x)
  (fixnum? x))

(define (target-fixnum-width)
  (fixnum-width))

(define (target-greatest-fixnum)
  (greatest-fixnum))

(define (target-endianness)
  'little)

(define target-environment
  (let ((e #f))
    (lambda ()
      ;; Work around some import ordering dependencies.
      (unless e
        (set! e (environment '(rnrs))))
      e)))

(define (lookup-primitive p)
  (case p
    (($fx-/false)
     (lambda (a b)
       (let ((x (fx- a b)))
         (and (target-fixnum? x) x))))
    (($fx+/false)
     (lambda (a b)
       (let ((x (fx+ a b)))
         (and (target-fixnum? x) x))))
    (($fx*/false)
     (lambda (a b)
       (let ((x (fx* a b)))
         (and (target-fixnum? x) x))))
    (($fxasl/false)
     (lambda (a b)
       (let ((x (fxarithmetic-shift-left a b)))
         (and (target-fixnum? x) x))))
    (($box?)
     (lambda (x)
       (cond ((target-fixnum? x) #f)
             ((char? x) #f)
             (else
              (assertion-violation 'lookup-primitive
                                   "Being cautious" x)))))
    (else
     (eval p (target-environment)))))

(define cp0-size-limit (make-parameter 16))

(define cp0-effort-limit (make-parameter 50))

;; ctxt is either an app or one of the symbols value, test, effect
(define-record app (operand* ctxt inlined convention))

(define-record operand (exp env value residualize-for-effect
                            ec size inner-pending outer-pending))

(define-syntax context-case-test
  (lambda (x)
    (syntax-case x (app value test effect)
      ((_ tmp app) #'(app? tmp))
      ((_ tmp value) #'(eq? tmp 'value))
      ((_ tmp test) #'(eq? tmp 'test))
      ((_ tmp effect) #'(eq? tmp 'effect)))))

(define-syntax context-case-tests
  (lambda (x)
    (syntax-case x (app value test effect else)
      ((_ tmp (t1 t2 trest ...))
       #'(or (context-case-test tmp t1)
             (context-case-tests tmp (t2 trest ...))))
      ((_ tmp (t1))
       #'(context-case-test tmp t1))
      ((_ tmp else) #'#t))))

(define-syntax context-case
  (lambda (x)
    (syntax-case x ()
      ((_ val (lhs rhs ...) ...)
       #'(let ((tmp val))
           (cond ((context-case-tests tmp lhs) rhs ...)
                 ...))))))

;; This syntax reads like let-values
(define-syntax with-extended-env
  (lambda (x)
    (syntax-case x ()
      ((_ ((env variable*) (old-env old-variable* operand*)) body ...)
       ;; XXX: this is stupidly enough broken when variable* = old-variable*
       #'(let* ((variable* (fresh-variables old-variable* operand*))
                (env (append (map cons old-variable* variable*) old-env)))
           (let ((v (let () body ...)))
             (residual-to-source variable*)
             v))))))

(define (fresh-variables old-variable* operand*)
  (let ((variable* (map fresh-residual-variable old-variable*)))
    (when operand*
      (for-each set-variable-operand!
                variable* operand*))
    variable*))

(define (fresh-residual-variable x)
  (let ((exported (if (variable-export-name x) #t #f)))
    ;; If the variable is exported (via library-letrec*) then there
    ;; might be references to it, but from other libraries. Even
    ;; exports that aren't explicitly exported can be implicitly
    ;; exported via macros (thanks Aziz!).
    (make-variable (gensym (variable-name x)) #f
                   (or (variable-referenced? x) exported)
                   (variable-mutated? x)
                   exported #f
                   #f #f
                   (variable-export-name x))))

(define (residual-to-source x*)
  (for-each (lambda (x)
              (set-variable-referenced?! x (variable-residual-referenced? x))
              (set-variable-mutated?! x (variable-residual-mutated? x)))
            x*))

(define make-voidcall
  (let ((c (make-funcall (make-primref 'void) '() #f #f)))
    (lambda ()
      c)))

(define (make-not expr src)
  (if (const? expr)
      (make-const (not (const-value expr)) #f)
      (make-funcall (make-primref 'not) (list expr) #f src)))

(define (last-operand-null? operand*)
  (and (pair? operand*)
       (let ((x (last operand*)))
         (and (const? x) (null? (const-value x))))))

;;; Procedures for creating and manipulating counters

(define-record counter (value ctxt k))

(define (passive-counter)
  (make-counter (greatest-fixnum) #f
                (lambda x
                  (error who
                         "Internal error: attempt to abort from passive counter"))))

(define (passive-counter-value counter)
  (fx- (greatest-fixnum) (counter-value counter)))

(define (active-counter? x)
  (and (counter? x) (counter-ctxt x)))

(define (decrement counter amount)
  (let ((n (fx- (counter-value counter) amount)))
    (set-counter-value! counter n)
    (when (fx<? n 0)
      (reset-integrated! (counter-ctxt counter))
      ((counter-k counter) #f))))

(define (reset-integrated! ctxt)
  (set-app-inlined! ctxt #f)
  (let ((ctxt (app-ctxt ctxt)))
    (when (app? ctxt)
      (reset-integrated! ctxt))))

;;; Procedures for constructing and accessing residual sequences

;; TODO: this might introduce a strict order where previously the
;; order was unspecified.

(define (make-sequence e0 e1)
  (if (simple? e0)
      e1
      (let ((e0 (if (seq? e0)
                    (if (simple? (seq-e1 e0))
                        (seq-e0 e0)
                        e0)
                    e0)))
        (if (seq? e1)
            (make-seq (make-seq e0 (seq-e0 e1)) (seq-e1 e1))
            (make-seq e0 e1)))))

(define (simple? x)
  (cond ((const? x) #t)
        ((ref? x) #t)
        ((proc? x) #t)
        ((primref? x) #t)
        ((eq? x (make-voidcall)))
        (else #f)))

(define (result-exp e)
  (if (seq? e) (seq-e1 e) e))

;;; Procedures for processing calls and operands

(define (record-type-immutable? rtd)
  (let ((names (record-type-field-names rtd)))
    (let lp ((i 0))
      (cond ((eqv? i (vector-length names))
             (cond ((record-type-parent rtd)
                    => record-type-immutable?)
                   (else #t)))
            ((record-field-mutable? rtd i)
             #f)
            (else (lp (fx+ i 1)))))))

(define (record-type-parent-fields-length rtd)
  (let lp ((rtd (record-type-parent rtd)) (len 0))
    (if (not rtd)
        len
        (lp (record-type-parent rtd)
            (fx+ len (vector-length (record-type-field-names rtd)))))))

(define (reasonable-make-rtd-operands? operand*)
  (match operand*
    [(name parent uid (? const? sealed?) (? const? opaque?) (? const? fields))
     (and
       (const? name) (symbol? (const-value name))
       (const? uid)
       (boolean? (const-value sealed?))
       (boolean? (const-value opaque?))
       (vector? (const-value fields))
       (for-all (match-lambda
                 [((or 'mutable 'immutable) (? symbol? _)) #t]
                 [_ #f])
                (vector->list (const-value fields))))]
    [_ #f]))

(define-record-type rcd
  (sealed #t)
  (nongenerative loko-rcd-3ca6464b-5f42-47e1-87e6-19a970f345aa)
  (fields rtd parent-rcd protocol))

;; Folding handles the case when all arguments to a procedure are
;; constant. This handles the cases when only some of them are.
;; XXX: some of this is probably only better on amd64.
(define (maybe-handle-primitive e operand* source ctxt env ec sc)
  (define (not-const? x) (not (const? x)))
  (define (F f . x*)
    (if (symbol? f)
        (make-funcall (make-primref f) x* #f source)
        (make-funcall f x* #f source)))
  (define (C x) (make-const x #f))
  (define (IF test then else)
    (make-test* test then else ctxt env ec sc))
  (define (eq-able? x*)
    ;; True if two objects with the same type as any of the objects
    ;; in x can be compared with eq? rather than eqv?.
    (and (list? x*)
         (for-all (lambda (c)
                    (or (symbol? c)
                        (target-fixnum? c)
                        (char? c)
                        (boolean? c)
                        (null? c)))
                  x*)))
  (let f ((operand* operand*))
    (case (primref-name e)
      ((memv member memq)
       (match operand*
         ((val (? const? lst))
          (cond ((eq-able? (const-value lst))
                 (match (const-value lst)
                   (() (C '#f))
                   ((x) (make-test* (F 'eq? val (C x))
                                    lst
                                    (C '#f)
                                    ctxt env ec sc))
                   (y*
                    (if (match y* [(_a _b _c _d . _) #t] [_ #f])
                        (F 'fast-memq val lst)
                        (let* ((var (if (ref? val)
                                        (ref-name val)
                                        (fresh-residual-variable
                                         (symbol->variable 'memqinline))))
                               (body
                                (let lp ((y* y*))
                                  (cond
                                    ((null? y*)
                                     (C #f))
                                    (else
                                     (make-test* (F 'eq? (make-ref var) (C (car y*)))
                                                 (C y*)
                                                 (lp (cdr y*))
                                                 ctxt env ec sc))))))
                          (if (ref? val)
                              body
                              (make-bind (list var) (list val) body)))))))
                ((and (eq? 'memq (primref-name e))
                      (list? (const-value lst)))
                 (F 'fast-memq val lst))
                (else #f)))
         (_ #f)))
      ((eqv? equal?)
       (match operand*
         ((lhs (? const? rhs))
          (and (eq-able? (list (const-value rhs)))
               (apply F 'eq? operand*)))
         (((? const? lhs) (? not-const? rhs))
          (f (list rhs lhs)))
         (_ #f)))
      ((fxbit-field)
       (match operand*
         ((n (? const? start) (? const? end))
          (let ((start^ (const-value start)) (end^ (const-value end)))
            (and (target-fixnum? start^) (target-fixnum? end^)
                 (< -1 end^ (target-fixnum-width))
                 (<= 0 start^ end^)
                 (let ((mask (- (expt 2 (- end^ start^)) 1)))
                   (and (target-fixnum? mask)
                        (F 'fxand
                           (F 'fxarithmetic-shift-right n start)
                           (C mask)))))))
         ((n start (? const? end))
          ;; FIXME: this is not sound
          (let ((end^ (const-value end)))
            (and (target-fixnum? end^)
                 (< -1 end^ (target-fixnum-width))
                 (let ((mask (bitwise-not
                              (bitwise-arithmetic-shift-left -1 end^))))
                   (and (target-fixnum? mask)
                        (F 'fxarithmetic-shift-right
                           (F 'fxand n (C mask))
                           start))))))
         (_ #f)))
      ((fxbit-set?)
       (match operand*
         ((n (? const? bit))
          (let ((bit^ (const-value bit)))
            (cond ((>= bit^ (- (target-fixnum-width) 1))
                   (F 'fxnegative? n))
                  ((>= bit^ 0)
                   (let ((mask (expt 2 bit^)))
                     (and (target-fixnum? mask)
                          (F 'not
                             (F 'fxzero?
                                (F 'fxand n (C mask)))))))
                  (else #f))))
         (_ #f)))
      ((fx*)
       (match operand*
         ((a (? const? b))
          (case (const-value b)
            ((2) (F 'fxarithmetic-shift-left a (C 1)))
            (else #f)))
         (((? const? a) (? not-const? b))
          (f (reverse operand*)))
         (_ #f)))
      ((fxarithmetic-shift)
       (match operand*
         ((a (? const? b))
          (let ((b^ (const-value b)))
            (and (target-fixnum? b^)
                 (cp0 (if (positive? b^)
                          (F 'fxarithmetic-shift-left a b)
                          (F 'fxarithmetic-shift-right a (C (- b^))))
                      ctxt env ec sc))))
         (_ #f)))
      ((fxdiv-and-mod)
       ;; XXX: This is just to do constant folding with multiple values
       (match operand*
         (((? const? n) (? const? d))
          (let ((d^ (const-value d)))
            (and (target-fixnum? d^)
                 (let* ((a (fresh-residual-variable (symbol->variable 'a)))
                        (a% (make-ref a)))
                   (cp0 (make-bind (list a) (list n)
                                   (F 'values
                                      (F 'fxdiv a% d)
                                      (F 'fxmod a% d)))
                        ctxt env ec sc)))))
         (_ #f)))
      ((bitwise-and)
       (match operand*
         [((? ref? a) (? funcall? b))
          (let ([b-op (funcall-operator b)])
            (and (primref? b-op)
                 (case (primref-name b-op)
                   [(-)
                    (match (funcall-operand* b)
                      [((? ref? a^) (? const? c))
                       (and (eq? (ref-name a) (ref-name a^))
                            (eqv? (const-value c) 1)
                            (F 'bitwise-lsr a))]
                      [else #f])]
                   [else #f])))]
         [_ #f]))
      ((list)
       (match operand*
         [()
          (make-const (list) #f)]
         [_ #f]))
      ((not)
       (match operand*
         [(op)
          (cp0-not op source ctxt env ec sc)]
         [_ #f]))
      ((string-ref)
       (match operand*
         [((? funcall? v) k)
          (let ([op (funcall-operator v)])
            (and (primref? op)
                 (case (primref-name op)
                   [(symbol->string)
                    ;; (string-ref (symbol->string sym) k) => (symbol-ref sym k)
                    (match (funcall-operand* v)
                      [(sym)
                       (F 'symbol-ref sym k)]
                      [else #f])]
                   [else #f])))]
         [_ #f]))
      ((string-length)
       (match operand*
         [((? funcall? v))
          (let ([op (funcall-operator v)])
            (and (primref? op)
                 (case (primref-name op)
                   [(symbol->string)
                    ;; (string-length (symbol->string sym)) => (symbol-length sym)
                    (match (funcall-operand* v)
                      [(sym)
                       (F 'symbol-length sym)]
                      [else #f])]
                   [else #f])))]
         [_ #f]))
      ((bytevector-s16-ref bytevector-u16-ref
        bytevector-s32-ref bytevector-u32-ref
        bytevector-s64-ref bytevector-u64-ref
        bytevector-ieee-single-ref bytevector-ieee-double-ref)
       (match (assq (primref-name e)
                    '((bytevector-s16-ref bytevector-s16-native-ref 2)
                      (bytevector-u16-ref bytevector-u16-native-ref 2)
                      (bytevector-s32-ref bytevector-s32-native-ref 4)
                      (bytevector-u32-ref bytevector-u32-native-ref 4)
                      (bytevector-s64-ref bytevector-s64-native-ref 8)
                      (bytevector-u64-ref bytevector-u64-native-ref 8)
                      (bytevector-ieee-single-ref bytevector-ieee-single-native-ref 4)
                      (bytevector-ieee-double-ref bytevector-ieee-double-native-ref 8)))
         [(_ native-name alignment)
          (match operand*
            [(bv (? const? idx^) (? const? endianness))
             (let ((idx (const-value idx^)))
               (and (eq? (const-value endianness) (target-endianness))
                    (integer? idx) (>= idx 0) (eqv? 0 (mod idx alignment))
                    (F native-name bv idx^)))]
            [_ #f])]))
      ((bytevector-s16-set! bytevector-u16-set!
        bytevector-s32-set! bytevector-u32-set!
        bytevector-s64-set! bytevector-u64-set!
        bytevector-ieee-single-set! bytevector-ieee-double-set!)
       (match (assq (primref-name e)
                    '((bytevector-s16-set! bytevector-s16-native-set! 2)
                      (bytevector-u16-set! bytevector-u16-native-set! 2)
                      (bytevector-s32-set! bytevector-s32-native-set! 4)
                      (bytevector-u32-set! bytevector-u32-native-set! 4)
                      (bytevector-s64-set! bytevector-s64-native-set! 8)
                      (bytevector-u64-set! bytevector-u64-native-set! 8)
                      (bytevector-ieee-single-set! bytevector-ieee-single-native-set! 4)
                      (bytevector-ieee-double-set! bytevector-ieee-double-native-set! 8)))
         [(_ native-name alignment)
          (match operand*
            [(bv (? const? idx^) v (? const? endianness))
             (let ((idx (const-value idx^)))
               (and (eq? (const-value endianness) (target-endianness))
                    (integer? idx) (>= idx 0) (eqv? 0 (mod idx alignment))
                    (F native-name bv idx^ v)))]
            [_ #f])]))
      ((fxior bitwise-ior)
       (let-values ([(constop* op*) (partition const? operand*)])
         (let ((name (primref-name e)))
           (let* ((const* (map const-value constop*))
                  (const (if (or (for-all target-fixnum? const*)
                                 (and (eq? name 'bitwise-ior)
                                      (for-all (lambda (x) (and (integer? x) (exact? x)))
                                               const*)))
                             (fold-left bitwise-ior (bitwise-ior) const*)
                             0)))
             (match op*
               [() (make-const const #f)]
               [(x) (if (eqv? const 0)
                        #f
                        (F (primref-name e) x (make-const const #f)))]
               [_ (let lp ((op* (if (eqv? const 0) op* (cons (make-const const #f) op*))))
                    (match op*
                      [(a b) (F name a b)]
                      [(a b c) (F name (F name a b) c)]
                      [(a b . x*) (F name (F name a b) (lp x*))]))])))))

      ((append)
       (letrec ((nil? (lambda (op) (and (const? op) (eq? '() (const-value op))))))
         (match operand*
           [() (make-const '() #f)]
           [(x) x]
           #;
           ((x* ... xn)
            (let ((x*^ (remp nil? x*)))
              (make-funcall (make-primref 'append) (append x*^ (list xn)) #f source)))
           [_ #f])))
      ((cons*)
       (letrec ((nil? (lambda (op) (and (const? op) (eq? '() (const-value op))))))
         (match operand*
           [(x) x]
           [(x* ... (? nil? _)) (apply F 'list x*)]
           [_ #f])))

      ;; Optimizations for the procedural record layer

      ((make-record-type-descriptor
        ;; Both of these will be treated as nongenerative wrt
        ;; optimizations, but they should be marked with the correct
        ;; generativity in the rtd.
        make-record-type-descriptor/nongenerative
        make-record-type-descriptor/generative)
       (match operand*
         [(name parent uid (? const? sealed?) (? const? opaque?) (? const? fields))
          (and
            (reasonable-make-rtd-operands? operand*)
            (const? parent)
            (or (not (const-value parent))
                (and (record-type-descriptor? (const-value parent))
                     (not (record-type-sealed? (const-value parent)))))
            (cond ((or (symbol? (const-value uid))
                       (memq (primref-name e)
                             '(make-record-type-descriptor/nongenerative
                               make-record-type-descriptor/generative)))
                   ;; Constant-folding of a nongenerative rtd. XXX:
                   ;; goes into the host implementation's RTD
                   ;; registry, which might not be good.
                   (C (apply make-record-type-descriptor (map const-value operand*))))
                  (else
                   ;; TODO: The record type is generative, i.e. it
                   ;; must result in a new record type every time this
                   ;; code runs. But we can still use some of the
                   ;; information to optimize things.
                   (C (apply make-artd (map const-value operand*)))
                   #f)))]
         [_ #f]))

      ((make-record-constructor-descriptor)
       (match operand*
         [((? const? rtd^) (? const? parent-rcd^) protocol)
          (let ((rtd (const-value rtd^))
                (parent-rcd (const-value parent-rcd^)))
            ;; FIXME: Some more verification is needed on the
            ;; arguments here before making the rcd.
            (cond ((record-type-descriptor? rtd)
                   (cond ((and (or (not parent-rcd)
                                   (and (rcd? parent-rcd)
                                        (eq? (rcd-rtd parent-rcd)
                                             (record-type-parent rtd)))))
                          (cond ((and (const? protocol) (not (const-value protocol)))
                                 (let ((parent-rcd (or parent-rcd
                                                       (let f ((rtd (record-type-parent rtd)))
                                                         (and rtd (make-rcd rtd (f (record-type-parent rtd)) #f))))))
                                   (C (make-rcd rtd parent-rcd #f))))
                                #;
                                ((and (ref? protocol)
                                      (not (variable-mutated? (ref-var protocol))))
                                 ;; TODO: implement this case
                                 #f)
                                ;; TODO: if it's a proc then bind it to a variable
                                (else #f)))
                         (else #f)))
                  (else #f)))]
         [_ #f]))

      ((record-constructor)
       (match operand*
         [((? const? rcd^))
          (let ((rcd (const-value rcd^))
                (src (C source)))
            ;; This transformation is based on the closures created by
            ;; SRFI-76. So credit must be given to Mike Sperber for
            ;; large parts of this monstrosity. Thank you for shedding
            ;; light on what was in the dark.
            (letrec ((->var (lambda (x) (fresh-residual-variable (symbol->variable x))))
                     (mkname (lambda (rtd)
                               (string->symbol (string-append "make-" (symbol->string (record-type-name rtd))))))
                     (base-record-constructor
                      (lambda (rtd)
                        (let ((args (let lp ((rtd rtd))
                                      (if (not rtd)
                                          '()
                                          (append (lp (record-type-parent rtd))
                                                  (map ->var (vector->list (record-type-field-names rtd)))))))
                              (name (mkname rtd)))
                          (make-proc (gensym name) (gensym name)
                                     (list
                                      (make-proccase
                                       (make-caseinfo (gensym name) args #t)
                                       (apply F '$record (C rtd) (map make-ref args))))
                                     '() (C name) src))))
                     (default-protocol
                      (lambda (rtd)
                        (let ((p (->var 'p))
                              (name (mkname rtd)))
                          (cond ((record-type-parent rtd)
                                 (let ((v* (let lp ((rtd (record-type-parent rtd)))
                                             (if (not rtd)
                                                 '()
                                                 (append (lp (record-type-parent rtd))
                                                         (map ->var (vector->list (record-type-field-names rtd)))))))
                                       (x* (map ->var (vector->list (record-type-field-names rtd)))))
                                   (make-proc
                                    (gensym name) (gensym name)
                                    (list
                                     (make-proccase
                                      (make-caseinfo (gensym name) (list p) #t)
                                      (make-proc (gensym name) (gensym name)
                                                 (list
                                                  (make-proccase
                                                   (make-caseinfo (gensym name) (append v* x*) #t)
                                                   (apply F (apply F (make-ref p) (map make-ref v*))
                                                          (map make-ref x*))))
                                                 '() (C name) src)))
                                    '() (C name) src)))
                                (else
                                 (make-proc (gensym name) (gensym name)
                                            (list (make-proccase
                                                   (make-caseinfo (gensym name) (list p) #t)
                                                   (make-ref p)))
                                            '() (C name) src))))))
                     (make-make-seeder
                      (lambda (real-rtd for-desc)
                        (let recur ((for-desc for-desc))
                          (let ((for-rtd (rcd-rtd for-desc)))
                            (cond ((rcd-parent-rcd for-desc) =>
                                   (lambda (parent-desc)
                                     (let ((parent-protocol (rcd-protocol* parent-desc))
                                           (parent-make-seeder (recur parent-desc)))
                                       (let ((extension-values (->var 'extension-values))
                                             (parent-protocol-args (->var 'parent-protocol-args))
                                             (for-rtd-values
                                              (map ->var (vector->list (record-type-field-names for-rtd))))
                                             (name (mkname real-rtd)))
                                         (make-proc
                                          (gensym name) (gensym name)
                                          (list
                                           (make-proccase
                                            (make-caseinfo (gensym name) (list extension-values) #f)
                                            (make-proc
                                             (gensym name) (gensym name)
                                             (list
                                              (make-proccase
                                               (make-caseinfo (gensym name) (list parent-protocol-args) #f)
                                               (make-proc (gensym name) (gensym name)
                                                          (list
                                                           (make-proccase
                                                            (make-caseinfo (gensym name) for-rtd-values #t)
                                                            (F 'apply (F parent-protocol
                                                                         (apply F 'apply parent-make-seeder
                                                                                `(,@(map make-ref for-rtd-values)
                                                                                  ,(make-ref extension-values))))
                                                               (make-ref parent-protocol-args))))
                                                          '() (C name) src)))
                                             '() (C name) src)))
                                          '() (C name) src)))))
                                  (else
                                   (let ((extension-values (->var 'extension-values))
                                         (for-rtd-values
                                          (map ->var (vector->list (record-type-field-names for-rtd))))
                                         (name (mkname real-rtd)))
                                     (make-proc (gensym name) (gensym name)
                                                (list
                                                 (make-proccase
                                                  (make-caseinfo (gensym name) (list extension-values) #f)
                                                  (make-proc (gensym name) (gensym name)
                                                             (list
                                                              (make-proccase
                                                               (make-caseinfo (gensym name) for-rtd-values #t)
                                                               (apply F 'apply (base-record-constructor real-rtd)
                                                                      `(,@(map make-ref for-rtd-values)
                                                                        ,(make-ref extension-values)))))
                                                             '() (C name) src)))
                                                '() (C name) src))))))))
                     (rcd-protocol* (lambda (rcd)
                                      (assert (not (rcd-protocol rcd)))
                                      (default-protocol (rcd-rtd rcd)))))

              (and (rcd? rcd)
                   (let ((rtd (rcd-rtd rcd))
                         (protocol (rcd-protocol* rcd)))
                     (let ((maker (F protocol (F (make-make-seeder rtd rcd)))))
                       ;; (pretty-print (record->sexpr maker))
                       (cp0 maker ctxt env ec sc))))))]
         [_ #f]))

      ((record-predicate)
       (match operand*
         [((? const? rtd^))
          (let ((rtd (const-value rtd^)))
            (and (record-type-descriptor? rtd)
                 (let ((name (string->symbol
                              (string-append (symbol->string (record-type-name rtd)) "?")))
                       (x (fresh-residual-variable (symbol->variable 'x))))
                   (let ((proc
                          (make-proc (gensym name) (gensym name)
                                     (list
                                      (make-proccase
                                       (make-caseinfo (vector x) (list x) #t)
                                       (F '$record? (make-ref x) (C rtd))))
                                     '()
                                     (C name)
                                     (C source))))
                     (cp0 proc ctxt env ec sc)))))]
         [_ #f]))

      ((record-accessor)
       (match operand*
         [((? const? rtd^) (? const? k^))
          (let ((rtd (const-value rtd^))
                (k (const-value k^)))
            (and (record-type-descriptor? rtd)
                 (fixnum? k)
                 (fx<? -1 k (vector-length (record-type-field-names rtd)))
                 (let ((x (fresh-residual-variable (symbol->variable 'x)))
                       (name (string->symbol
                              (string-append
                               (symbol->string (record-type-name rtd)) "-"
                               (symbol->string (vector-ref (record-type-field-names rtd) k)))))
                       (k^ (fx+ k (record-type-parent-fields-length rtd))))
                   (let ((proc
                          (make-proc (gensym name) (gensym name)
                                     (list
                                      (make-proccase
                                       (make-caseinfo (vector x) (list x) #t)
                                       (make-test* (if (record-type-sealed? rtd)
                                                       (F 'eq? (F '$box-type (make-ref x)) (C rtd))
                                                       (F '$record/fast? (make-ref x) (C rtd)))
                                                   (F '$box-ref (make-ref x) (C k^))
                                                   (F 'raise-accessor-error (C rtd) (C k) (make-ref x))
                                                   'value env ec sc)))
                                     '()
                                     (C name)
                                     (C source))))
                     (cp0 proc ctxt env ec sc)))))]
         [_ #f]))

      ((record-mutator)
       (match operand*
         [((? const? rtd^) (? const? k^))
          (let ((rtd (const-value rtd^))
                (k (const-value k^)))
            (and (record-type-descriptor? rtd)
                 (fixnum? k)
                 (fx<? -1 k (vector-length (record-type-field-names rtd)))
                 (let ((x (fresh-residual-variable (symbol->variable 'x)))
                       (v (fresh-residual-variable (symbol->variable 'v)))
                       (name (string->symbol
                              (string-append
                               (symbol->string (record-type-name rtd)) "-"
                               (symbol->string (vector-ref (record-type-field-names rtd) k))
                               "-set!")))
                       (k^ (fx+ k (record-type-parent-fields-length rtd))))
                   (let ((proc
                          (make-proc (gensym name) (gensym name)
                                     (list
                                      (make-proccase
                                       (make-caseinfo (vector x) (list x v) #t)
                                       (make-test* (if (record-type-sealed? rtd)
                                                       (F 'eq? (F '$box-type (make-ref x)) (C rtd))
                                                       (F '$record/fast? (make-ref x) (C rtd)))
                                                   (F '$box-set! (make-ref x) (C k^) (make-ref v))
                                                   (F 'raise-mutator-error (C rtd) (C k)
                                                      (make-ref x) (make-ref v))
                                                   'value env ec sc)))
                                     '()
                                     (C name)
                                     (C source))))
                     (cp0 proc ctxt env ec sc)))))]
         [_ #f]))

      ;; TODO: don't use together with eval
      (($record? $record/fast?)
       (match operand*
         [(x^ (? const? rtd^))
          (let ((rtd (const-value rtd^))
                (rtd:parents-start 7)
                (rtd:min-parents 4)
                (type-checks (eq? '$record? (primref-name e))))
            (let* ((x (fresh-residual-variable (symbol->variable 'x)))
                   (t (fresh-residual-variable (symbol->variable 't)))
                   (num-parents (let lp ((rtd (record-type-parent rtd)) (n 0))
                                  (if (not rtd)
                                      n
                                      (lp (record-type-parent rtd) (fx+ n 1)))))
                   (rtd-idx (fx+ num-parents rtd:parents-start)))
              (make-bind
               (list x)
               (list x^)
               (cond ((record-type-sealed? rtd)
                      (make-test* (if type-checks (F '$box? (make-ref x)) (C #t))
                                  (F 'eq? (F '$box-type (make-ref x)) (C rtd))
                                  (C #f)
                                  ctxt env ec sc))
                     (else
                      (decrement sc 1)
                      (IF (if type-checks (F '$box? (make-ref x)) (C #t))
                          (make-bind
                           (list t)
                           (list (F '$box-type (make-ref x)))
                           (IF (F 'eq? (make-ref t) (C rtd))
                               (C #t)
                               (IF (if type-checks (F '$box? (make-ref t)) (C #t))
                                   (IF (F '$box-header-type-eq? (F '$box-type (make-ref t)) (C 'rtd))
                                       (IF (if (fx<=? num-parents rtd:min-parents)
                                               (C #t)
                                               (F 'fx<?
                                                  (C rtd-idx)
                                                  (F '$box-header-length (F '$box-type (make-ref t)))))
                                           (F 'eq? (F '$box-ref (make-ref t) (C rtd-idx)) (C rtd))
                                           (C #f))
                                       (C #f))
                                   (C #f))))
                          (C #f)))))))]
         [_ #f]))

      (($record)
       (match operand*
         [((? const? rtd^) (? const? field*) ...)
          ;; Attempt constant folding on records.
          (and (record-type-descriptor? (const-value rtd^))
               (let ((rtd (const-value rtd^)))
                 (cond
                   ((and (not (record-type-generative? rtd))
                         (record-type-immutable? rtd))
                    (let* ((rcd (make-record-constructor-descriptor rtd #f #f))
                           (mk (record-constructor rcd)))
                      (C (apply mk (map const-value field*)))))
                   (else #f))))]
         [_ #f]))

      (else #f))))

(define (cp0-call e operand* source ctxt^ env ec sc)
  (define (make-funcall* e operand* source ctxt)
    (cond
      ((eq? (app-convention ctxt) 'apply)
       (if (last-operand-null? operand*)
           (make-funcall e (butlast operand*) #f source)
           (make-funcall (make-primref 'apply)
                         (cons e operand*)
                         #f source)))
      ((and (primref? e)
            (maybe-handle-primitive e operand* source ctxt^ env ec sc)))
      (else
       (make-funcall e operand* #f source))))
  (define (f e operand* convention)
    (let* ((ctxt (make-app operand* ctxt^ #f convention))
           (e (cp0 e ctxt env ec sc)))
      (if (app-inlined ctxt)
          (residualize-operand* operand* e sc)
          (make-funcall* e (map (lambda (opnd)
                                  (score-value-visit-operand! opnd sc))
                                operand*)
                         source ctxt))))
  (if (and (primref? e) (eq? (primref-name e) 'apply)
           (pair? operand*) (pair? (cdr operand*)))
      (f (score-value-visit-operand! (car operand*) sc)
         (cdr operand*) 'apply)
      (f e operand* 'call)))

(define (residualize-operand* operand* e sc)
  ;; Those operands marked with residualize-for-effect are
  ;; residualized here, the rest are discarded.
  (let lp ((operand* operand*))
    (cond ((null? operand*)
           e)
          ((not (operand-residualize-for-effect (car operand*)))
           (lp (cdr operand*)))
          (else
           (let* ((opnd (car operand*))
                  (e0 (or (operand-value opnd)
                          (cp0 (operand-exp opnd) 'effect (operand-env opnd)
                               (operand-ec opnd) sc))))
             (cond ((simple? e0)
                    (lp (cdr operand*)))
                   (else
                    (decrement sc (operand-size opnd))
                    (make-sequence e0 (lp (cdr operand*))))))))))

(define (value-visit-operand! opnd)
  (or (operand-value opnd)
      (let* ((sc (passive-counter))
             (e (cp0 (operand-exp opnd) 'value (operand-env opnd)
                     (operand-ec opnd) sc)))
        (set-operand-value! opnd e)
        (set-operand-size! opnd (passive-counter-value sc))
        e)))

(define (score-value-visit-operand! opnd sc)
  (let ((val (value-visit-operand! opnd)))
    (decrement sc (operand-size opnd))
    val))

;;; Constant folding

;; TODO: folding fixnum operations (and operations with endianness)
;; should be done in a manner that uses the target's fixnum-width
;; (and endianness).

;; In general it would be nice to have more info on all the
;; primitives. The properties below, but also what types their
;; arguments are, and what types they return. It would also be nice
;; to know, for each primitive, if it's open-coded or not. This
;; would affect the size counter.

;; (for-each proc (list x)) => (proc x). maybe cp0 could take this
;; already, if the binding of for-each was known to it.

(define (effect-free? p)
  ;; Stuff goes here only if it can't raise an error, print stuff,
  ;; etc. Allocating memory is OK.
  (memq p '(cons
            list vector cons* $record
            not void eq? eqv? equal?
            fixnum? char? pair? bytevector? string? null? vector? symbol?
            boolean? eof-object? $immsym? condition?
            fixnum-width greatest-fixnum least-fixnum
            native-endianness make-eq-hashtable)))

(define (foldable? p)
  (memq p '(+ - * / < <= > >= =
              bitwise-arithmetic-shift
              bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
              bitwise-ior bitwise-and expt
              bitwise-bit-set? bitwise-bit-field bitwise-not
              not char->integer integer->char fxzero? equal? eq? eqv? zero?
              memq memv member assq assv assoc
              fx+ fx* fx- fx<? fx<=? fx>? fx>=? fx=? fixnum? char>?
              fxnegative? fxpositive?
              fl+ fl* fl- fl/ flonum?
              null? fxarithmetic-shift-right fxand fxior fxnot fxxor
              fxarithmetic-shift-left fixnum-width
              fxbit-set? fxbit-count fxeven? fxodd?
              greatest-fixnum least-fixnum
              char? bytevector-length bytevector? pair?
              vector-length vector? bytevector-u8-ref native-endianness
              car cdr cadr vector-ref string-ref list-ref
              fxlength fxdiv fxmod div mod ceiling floor log
              length string-length
              boolean? symbol?
              $box?
              $fx-/false $fx+/false $fx*/false $fxasl/false $fxasr/false)))

(define (result-true? p)
  (memq p '(+ - * / cons list void char->integer integer->char
              bitwise-arithmetic-shift
              bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
              bitwise-bit-set? bitwise-bit-field bitwise-not
              expt fx+ fx* fx-
              fxarithmetic-shift-right
              fxarithmetic-shift-left fxand fxior fxnot fxxor
              fxbit-count
              greatest-fixnum least-fixnum fixnum-width
              string->utf8 vector-length bytevector-length
              make-bytevector bytevector-u8-ref native-endianness
              fxlength div mod ceiling floor log
              length string-length
              $record)))

(define (result-boolean? p)
  (memq p '(not eq? eqv? equal?
                fixnum? char? pair? bytevector? string? null? vector? symbol?
                list?
                string=?
                boolean? eof-object? $immsym? condition?
                < <= > >= =
                fx<? fx<=? fx>? fx>=? fx=? fxeven? fxodd? even? odd?
                fxzero? fxnegative? fxpositive?
                flonum? fl=? fl<? fl<=? fl>? fl>=?
                flinteger? flzero? flpositive? flnegative?
                flodd? fleven? flfinite? flinfinite? flnan?
                char<? char<=? char>? char>=? char=?
                integer? zero? positive? negative? real? rational?
                boolean=?
                $box?
                $box-header?
                $box-header-refs?
                $box-header-type-eq?)))

;; TODO: here it would be nice if things like (null? (list x ...))
;; => #f were known. (vector-length (make-vector x ...)) => x.

(define (fold-prim p ctxt sc)
  (define (print . x) (for-each display x) (newline))
  (let ((operand* (app-operand* ctxt)))
    (cond ((or (and (effect-free? p)
                    (context-case (app-ctxt ctxt)
                      ((effect) (make-voidcall))
                      ((test) (and (result-true? p) (make-const #t #f)))
                      (else #f)))
               (and (foldable? p)
                    (let* ((val* (map value-visit-operand! operand*))
                           (res* (map result-exp val*)))
                      (and (for-all const? res*)
                           (guard (exn
                                   (else
                                    ;; (print "WARNING: can't fold:")
                                    ;; (write `(,p ,@(map const-value res*)))
                                    ;; (newline)
                                    ;; (print exn)
                                    #f))
                             (let* ((proc (lookup-primitive p))
                                    (arg* (map const-value res*))
                                    (c (case (app-convention ctxt)
                                         ((call) (apply proc arg*))
                                         ((apply) (apply apply proc arg*))
                                         (else
                                          (error who "Internal error: bad app"
                                                 (app-convention ctxt))))))
                               (make-const c #f)))))))
           =>
           (lambda (result)
             (for-each
              (lambda (opnd)
                (set-operand-residualize-for-effect! opnd #t))
              operand*)
             (set-app-inlined! ctxt #t)
             result))
          (else
           (decrement sc 1)
           (make-primref p)))))

;;; Procedures for inlining and construction of residual let bindings

;; Inlining is performed like in the thesis, except it finds a
;; matching case (from case-lambda), and also handles optional
;; arguments by residualizing a call to list.

(define (inline proc ctxt env ec sc)
  (define (match-formals op args)
    ;; This takes a proc and a list of operands, i.e. and returns the
    ;; proccase that would be called, or #f if none.
    (define (match? case)
      (let* ((info (proccase-info case))
             (formals (caseinfo-formals info)))
        (if (caseinfo-proper? info)
            (= (length args) (length formals))
            (>= (length args) (- (length formals) 1)))))
    (cond ((memp match? (proc-cases op)) => car)
          (else #f)))

  (define (match-formals-apply op args)
    ;; Same as match-formals, except args is terminated by a list,
    ;; so the number of arguments is possibly unknown. TODO: it
    ;; would help if constants in the rest list are extracted before
    ;; this runs, and avoid doing that for circular lists (+warn).
    (cond
      ((last-operand-null? args)
       ;; Handle (apply f x ... '()).
       (match-formals op (butlast args)))
      (else
       (let lp ((case* (proc-cases op)))
         (cond
           ((null? case*)
            #f)
           (else
            (let* ((case (car case*))
                   (case* (cdr case*))
                   (info (proccase-info case))
                   (formals (caseinfo-formals info))
                   (na (fx- (length args) 1))
                   (nf (length formals)))
              ;; If it is possible that this case might not
              ;; accept the arguments, and there is a latter
              ;; case that might, then the case to use depends
              ;; on the length of the rest args list, so #f is
              ;; returned. This analysis could be better.
              (cond
                ((caseinfo-proper? info)
                 ;; (ap (lambda (a b) e) 1 2 r)   maybe
                 ;; (ap (lambda (a b) e) 1 r)     maybe
                 ;; (ap (lambda (a b) e) 1 2 3 r) never
                 (if (<= na nf)
                     (and (null? case*)
                          case)
                     (lp case*)))
                (else
                 ;; (ap (lambda (a b . x*) e) 1 2 r)    yes
                 ;; (ap (lambda (a b . x*) e) 1 r)      maybe
                 ;; (ap (lambda (a b . x*) e) 1 2 3 r)  yes
                 (cond ((> na (fx- nf 1))
                        ;; This would require residualising conses,
                        ;; which isn't done for now.
                        #f)
                       ((>= na (fx- nf 1))
                        case)
                       ((< na (fx- nf 1))
                        (and (null? case*)
                             case))
                       (else
                        (lp case*))))))))))))

  (define (make-let-binding x* operand* body sc)
    (define (effect-operand* result operand*)
      (for-each
       (lambda (operand)
         (set-operand-residualize-for-effect! operand #t))
       operand*)
      result)
    (cond ((or (const? body) (primref? body))
           ;; The body can't reference any of x*, so do not make a
           ;; let binding.
           (effect-operand* body operand*))
          ((ref? body)
           ;; The body is just a variable reference.
           (let ((y (ref-name body)))
             (let lp ((x* x*)
                      (operand* operand*))
               (cond ((null? x*) body)
                     ((eq? (car x*) y)
                      ;; The body is a reference to this operand
                      (effect-operand* (score-value-visit-operand! (car operand*) sc)
                                       (cdr operand*)))
                     (else
                      ;; This operand is not referenced. Residualize
                      ;; it for effect and go to the next one.
                      (set-operand-residualize-for-effect! (car operand*) #t)
                      (lp (cdr x*) (cdr operand*)))))))
          (else
           ;; The body is something else that might reference and
           ;; mutate the variables.
           (let lp ((x* x*)
                    (operand* operand*)
                    (new-x* '())
                    (new-operand* '()))
             (cond ((null? x*)
                    (if (null? new-x*)
                        body
                        (make-bind (reverse new-x*) (reverse new-operand*) body)))
                   ((variable-residual-referenced? (car x*))
                    ;; The variable is referenced in the
                    ;; residualized program, so make a binding for
                    ;; it.
                    (lp (cdr x*) (cdr operand*)
                        (cons (car x*) new-x*)
                        (cons (score-value-visit-operand! (car operand*) sc)
                              new-operand*)))
                   ((variable-residual-mutated? (car x*))
                    ;; The variable is mutated, but not referenced,
                    ;; so residualize the operand for effect but
                    ;; bind the variable to void.
                    (set-operand-residualize-for-effect! (car operand*) #t)
                    (lp (cdr x*) (cdr operand*)
                        (cons (car x*) new-x*)
                        (cons (make-voidcall) new-operand*)))
                   (else
                    ;; The variable is neither referenced nor mutated,
                    ;; so just residualize it for effect. The variable
                    ;; binding is not made.
                    (set-operand-residualize-for-effect! (car operand*) #t)
                    (lp (cdr x*) (cdr operand*)
                        new-x* new-operand*)))))))

  (define (return e)
    (set-app-inlined! ctxt #t)
    e)

  (let* ((operand* (app-operand* ctxt))
         (pc (case (app-convention ctxt)
               ((apply) (match-formals-apply proc operand*))
               ((call) (match-formals proc operand*)))))
    (cond
      ((not pc)
       ;; (when (= (length (proc-cases proc)) 1)
       ;;   (display "WARNING: could not inline:\n")
       ;;   (pretty-print (app-convention ctxt))
       ;;   (pretty-print (record->sexpr proc))
       ;;   (pretty-print (map record->sexpr (map operand-exp operand*)))
       ;;   (write (proc-source proc)) (newline)
       ;;   (newline))
       (cp0 proc 'value env ec sc))
      (else
       (let* ((info (proccase-info pc))
              (fml* (caseinfo-formals info)))
         (case (app-convention ctxt)
           ((call)
            (cond
              ((caseinfo-proper? info)
               (with-extended-env ((env formals) (env fml* operand*))
                 (let ((body (cp0 (proccase-body pc) (app-ctxt ctxt) env ec sc)))
                   (return (make-let-binding formals operand* body sc)))))
              (else
               ;; makes a call to list.
               (let* ((reqlen (fx- (length fml*) 1))
                      (to-cons* (list-tail operand* reqlen))
                      (ffixed* (take fml* reqlen))
                      (frest (list-tail fml* reqlen))
                      (tmp* (map (lambda (_)
                                   (fresh-residual-variable (car frest)))
                                 to-cons*)))
                 (with-extended-env ((env outer*)
                                     (env (append ffixed* tmp*) operand*))
                   (let ((consargs
                          (make-operand (make-funcall (make-primref 'list)
                                                      (map make-ref tmp*) #f #f)
                                        env #f #f ec 0 #f #f)))
                     (with-extended-env ((env inner*) (env frest (list consargs)))
                       (let ((body (cp0 (proccase-body pc) (app-ctxt ctxt)
                                        env ec sc)))
                         (return
                          (make-let-binding outer* operand*
                                            (make-let-binding inner*
                                                              (list consargs)
                                                              body
                                                              sc)
                                            sc))))))))))
           ((apply)
            ;; Outermost will be a let-binding that takes care of
            ;; everything except the rest list. Then the rest list is
            ;; picked apart.
            ;; (display "Inlining apply:\n")
            ;; (pretty-print (record->sexpr proc))
            ;; (pretty-print (map record->sexpr (map operand-exp operand*)))
            (let* ((reqlen (fx- (length operand*) 1))
                   (ctxt^ (make-app operand* (app-ctxt ctxt) #f 'call))
                   (tmp0 (fresh-residual-variable (symbol->variable 't)))
                   (body^
                    (let f ((fml* (list-tail fml* reqlen))
                            (tmp tmp0))
                      ;; (write (list 'fml* fml*))
                      ;; (newline)
                      (cond
                        ((and (not (caseinfo-proper? info))
                              (null? (cdr fml*)))
                         ;; Bind rest args.
                         (make-bind `(,(car fml*))
                                    `(,(make-ref tmp))
                                    (proccase-body pc)))
                        ((null? fml*)
                         ;; Check that no extra arguments are there.
                         ;; TODO: make this better. The bad car/cdr
                         ;; calls can be reported better if the
                         ;; machine code is slightly annotated.
                         (make-test (make-funcall (make-primref 'null?)
                                                  (list (make-ref tmp)) #f #f)
                                    (proccase-body pc)
                                    (make-funcall
                                     (make-primref 'assertion-violation)
                                     (list (proc-name proc) ;XXX: bad symbol
                                           (make-const "Expected fewer arguments" #f)
                                           (make-ref tmp0))
                                     #f #f)))
                        (else
                         (let ((tmp^ (fresh-residual-variable tmp)))
                           (make-bind `(,(car fml*) ,tmp^)
                                      `(,(make-funcall
                                          (make-primref 'car)
                                          (list (make-ref tmp)) #f #f)
                                        ,(make-funcall
                                          (make-primref 'cdr)
                                          (list (make-ref tmp)) #f #f))
                                      (f (cdr fml*) tmp^)))))))
                   (new-proc
                    (make-proc (proc-label proc)
                               (proc-end-label proc)
                               (list
                                (make-proccase
                                 (make-caseinfo
                                  #f
                                  `(,@(take fml* reqlen) ,tmp0)
                                  'proper)
                                 body^))
                               (proc-free proc)
                               (proc-name proc)
                               (proc-source proc))))
              ;; (display "After inlining:\n")
              ;; (pretty-print (record->sexpr new-proc))
              ;; (newline)
              (return (cp0 new-proc ctxt^ env ec sc))))
           (else
            (error who "Internal error: bad app ctxt" (app-convention ctxt)))))))))

;;; Procedures for processing variable references

(define (cp0-ref x ctxt env ec sc)
  (context-case ctxt
    ((effect) (make-voidcall))
    (else
     (let* ((x (lookup x env))
            (opnd (variable-operand x)))
       (cond ((and opnd (not (operand-inner-pending opnd)))
              (dynamic-wind
                (lambda () (set-operand-inner-pending! opnd #t))
                (lambda () (value-visit-operand! opnd))
                (lambda () (set-operand-inner-pending! opnd #f)))
              (if (variable-mutated? x)
                  (residualize-ref x sc)
                  (copy x opnd ctxt ec sc)))
             (else
              (residualize-ref x sc)))))))

(define (copy x opnd ctxt ec sc)
  (let ((rhs (result-exp (operand-value opnd))))
    (cond ((const? rhs) rhs)
          ((ref? rhs)
           (let ((y (ref-name rhs)))
             (if (variable-mutated? y)
                 (residualize-ref x sc)
                 (let ((opnd (variable-operand y)))
                   (if (and opnd (operand-value opnd))
                       (copy2 y opnd ctxt ec sc)
                       (residualize-ref y sc))))))
          (else (copy2 x opnd ctxt ec sc)))))

(define (copy2 x opnd ctxt ec sc)
  (let ((rhs (result-exp (operand-value opnd))))
    (cond ((proc? rhs)
           (context-case ctxt
             ((value) (residualize-ref x sc))
             ((test) (make-const #t #f))
             ((app)
              ;; Tries to inline the procedure. If the size limit is
              ;; exceeded abort is called and a variable reference
              ;; is residualized instead.
              (or (and (not (operand-outer-pending opnd))
                       (dynamic-wind
                         (lambda () (set-operand-outer-pending! opnd #t))
                         (lambda ()
                           (call/cc     ;TODO: call/1cc
                            (lambda (abort)
                              (let* ((limit (if (active-counter? sc)
                                                (counter-value sc)
                                                (cp0-size-limit)))
                                     (ec (if (active-counter? ec)
                                             ec
                                             (make-counter (cp0-effort-limit)
                                                           ctxt abort)))
                                     (sc (make-counter limit ctxt abort)))
                                (inline rhs ctxt (new-env) ec sc)))))
                         (lambda () (set-operand-outer-pending! opnd #f))))
                  (residualize-ref x sc)))))
          ((primref? rhs)
           (context-case ctxt
             ((value) rhs)
             ((test) (make-const #t #f))
             ((app) (fold-prim (primref-name rhs) ctxt sc))))
          (else
           (residualize-ref x sc)))))

(define (residualize-ref id sc)
  (decrement sc 1)
  (set-variable-residual-referenced?! id #t)
  (make-ref id))

;;;

(define (new-env) '())

(define (lookup var env)
  (cond ((assq var env) => cdr)
        (else var)))

(define (record-equal? consequence alternative ctxt)
  (or (and (const? consequence) (const? alternative)
           (let ((c1 (const-value consequence))
                 (c2 (const-value alternative)))
             (context-case ctxt
               ((effect) #t)
               ((test) (if c1 c2 (not c2)))
               (else (eq? c1 c2)))))
      (and (ref? consequence) (ref? alternative)
           (eq? (ref-name consequence) (ref-name alternative)))))

(define (cp0-fix exp ctxt env ec sc)
  (with-extended-env ((env lhs*) (env (fix-lhs* exp) #f))
    (for-each (lambda (lhs rhs)
                (set-variable-operand! lhs (make-operand rhs env #f #f ec 0 #f #f)))
              lhs* (fix-rhs* exp))
    (when (and (pair? lhs*) (null? (cdr lhs*))
               (funcall? (fix-body exp))
               (proc? (car (fix-rhs* exp)))
               (ref? (funcall-operator (fix-body exp)))
               (eq? (car (fix-lhs* exp))
                    (ref-name (funcall-operator (fix-body exp)))))
      ;; If the letrec binds a single procedure and the body is a
      ;; call to that procedure, then this is probably a loop.
      ;; Inlining it would only result in the first iteration of the
      ;; loop being "unrolled". Later on pass-loops will make the
      ;; loop nice and tidy, but the inlined iteration will mean a
      ;; closure is allocated and there's bloat. Better not inline
      ;; the first iteration.
      (set-operand-inner-pending! (variable-operand (car lhs*)) #t))
    (let ((body (cp0 (fix-body exp) ctxt env ec sc)))
      ;; Fix only binds lambdas. If the body doesn't reference one
      ;; of the variables, discard it. This might be a
      ;; library-letrec*, so it's not exactly like in the
      ;; dissertation.
      ;; TODO: a variable might end up not being referenced even
      ;; though variable-residual-referenced is #t. See an example
      ;; at the bottom of this file.
      (let ((lhs* (filter variable-residual-referenced? lhs*)))
        (cond ((null? lhs*)
               body)
              (else
               (decrement sc 1)
               (make-fix lhs*
                         (map (lambda (lhs)
                                (let ((operand (variable-operand lhs)))
                                  (score-value-visit-operand! operand sc)))
                              lhs*)
                         body)))))))

(define (make-test* expr then else ctxt env ec sc)
  ;; expr, then and else have already been through cp0 once.
  (cond ((and (funcall? expr)
              (primref? (funcall-operator expr))
              (eq? 'not (primref-name (funcall-operator expr)))
              (pair? (funcall-operand* expr))
              (null? (cdr (funcall-operand* expr))))
         ;; (if (not a) b c) ==> (if a c b)
         (make-test* (car (funcall-operand* expr))
                     else then
                     ctxt env ec sc))
        ((and (test? expr)
              (const? (test-then expr))
              (const? (test-else expr)))
         ;; (if (if x c0 c1) y z) ==> (if x y z)
         (if (const-value (test-then expr))
             (make-test* (test-expr expr) then else ctxt env ec sc)
             (make-test* (test-expr expr) else then ctxt env ec sc)))
        ((and (const? then) (boolean? (const-value then))
              (const? else) (boolean? (const-value else))
              (returns-boolean? expr))
         (cond ((boolean=? (const-value then) (const-value else))
                ;; (if e bool bool) => (begin e bool)
                (make-seq expr then))
               ((const-value then)
                ;; (if e #t #f) => e
                expr)
               (else
                ;; (if e #f #t) => (not e)
                (make-funcall (make-primref 'not) (list expr) #f #f))))
        ((and (test? expr)
              (const? then)
              (const? else))
         ;; (if (if e e0 e1) ct cf) ==> (if e (if e0 ct cf) (if e1 ct cf))
         (make-test* (test-expr expr)
                     (make-test* (test-then expr) then else ctxt env ec sc)
                     (make-test* (test-else expr) then else ctxt env ec sc)
                     ctxt env ec sc))
        ((seq? expr)
         ;; (if (begin ... p) a b) ==> (begin ... (if p a b))
         (make-sequence (seq-e0 expr)
                        (make-test* (seq-e1 expr) then else
                                    ctxt env ec sc)))
        ((bind? expr)
         ;; (if (let (...) p) a b) ==> (let (...) (if p a b))
         (make-bind (bind-lhs* expr) (bind-rhs* expr)
                    (make-test* (bind-body expr) then else
                                ctxt env ec sc)))
        ((and (ref? expr) (ref? then) (const? else)
              (eq? (ref-name expr) (ref-name then))
              (eqv? (const-value else) #f))
         ;; (if x x #f) => x
         expr)
        ((const? expr)
         (make-sequence expr (if (const-value expr) then else)))
        ;; (if →(if e c0 c1)← e0 e1) => (if e (->bool c0) (->bool c1))
        ((and (eq? ctxt 'test)
              (or (and (const? then) (not (boolean? (const-value then))))
                  (and (const? else) (not (boolean? (const-value else))))))
         (make-test* expr
                     (if (const? then) (make-const (and (const-value then) #t) #f) then)
                     (if (const? else) (make-const (and (const-value else) #t) #f) else)
                     ctxt env ec sc))
        (else
         (make-test expr then else))))

(define (cp0-not expr src ctxt env ec sc)
  (cond ((seq? expr)
         ;; (not (begin ... p)) ==> (begin ... (not p))
         (make-sequence (seq-e0 expr)
                        (cp0-not (seq-e1 expr) src ctxt env ec sc)))
        ((bind? expr)
         ;; (not (let (...) p)) ==> (let (...) (not p))
         (make-bind (bind-lhs* expr) (bind-rhs* expr)
                    (cp0-not (bind-body expr) src ctxt env ec sc)))
        ((test? expr)
         ;; (not (if p a b)) ==> (if p (not a) (not b))
         (let* ((conseq (cp0-not (test-then expr) src 'test env ec sc))
                (altern (cp0-not (test-else expr) src 'test env ec sc)))
           (make-test* (test-expr expr) conseq altern ctxt env ec sc)))
        ((const? expr)
         (make-const (not (const-value expr)) #f))
        ;; (not (not (predicate x))) => x
        ((and (funcall? expr)
              (primref? (funcall-operator expr))
              (eq? 'not (primref-name (funcall-operator expr)))
              (match (funcall-operand* expr)
                [(x)
                 (and (returns-boolean? x) x)]
                [_ #f])))
        (else
         (make-funcall (make-primref 'not) (list expr) #f src))))

(define (cp0-bind lhs* rhs* body ctxt env ec sc)
  ;; Turns let into lambda + funcall again.
  (cp0 (make-funcall (make-proc (vector #f)
                                (vector #f)
                                (list
                                 (make-proccase (make-caseinfo
                                                 (vector lhs*)
                                                 lhs* #t)
                                                body))
                                '() #f #f)
                     rhs* #f #f)
       ctxt env ec sc))

(define (returns-boolean? expr)
  (cond ((funcall? expr)
         (let ((op (funcall-operator expr)))
           (and (primref? op)
                (result-boolean? (primref-name op)))))
        ((test? expr)
         (and (returns-boolean? (test-then expr))
              (returns-boolean? (test-else expr))))
        ((const? expr)
         (boolean? (const-value expr)))
        ((seq? expr)
         (let ((r (result-exp expr)))
           (and (const? r)
                (boolean? (const-value r)))))
        ((bind? expr)
         (returns-boolean? (bind-body expr)))
        (else #f)))

;; expression, context, environment, effort counter, size counter.
(define (cp0 exp ctxt env ec sc)
  (define (relabel-caseinfo info formals)
    (make-caseinfo (relabel (caseinfo-label info))
                   formals
                   (caseinfo-proper? info)))
  (define (relabel l)
    ;; XXX: This relabeling business makes fresh assembler labels
    ;; for when a case-lambda is copied. Maybe it should be done
    ;; elsewhere.
    (if (symbol? l)
        (gensym l)
        (vector 'cp0-inlined l)))
  ;; (display "#;CP0 ")
  ;; (write (record->sexpr exp))
  ;; (display " ctxt: ")
  ;; (display (if (symbol? ctxt) ctxt 'app))
  ;; (newline)
  (decrement ec 1)
  (cond ((const? exp) exp)
        ((ref? exp)
         (cp0-ref (ref-name exp) ctxt env ec sc))
        ((seq? exp)
         (let* ((e0 (cp0 (seq-e0 exp) 'effect env ec sc))
                (e1 (cp0 (seq-e1 exp) ctxt env ec sc)))
           (make-sequence e0 e1)))
        ((test? exp)
         (let ((e0 (cp0 (test-expr exp) 'test env ec sc)))
           (if (const? (result-exp e0))
               (make-sequence e0 (cp0 (if (const-value (result-exp e0))
                                          (test-then exp)
                                          (test-else exp))
                                      ctxt env ec sc))
               (let ((ctxt (if (app? ctxt) 'value ctxt)))
                 (let* ((consequence (cp0 (test-then exp) ctxt env ec sc))
                        (alternative (cp0 (test-else exp) ctxt env ec sc)))
                   (cond ((record-equal? consequence alternative ctxt)
                          ;; (if p a a) ==> (begin p a)
                          (make-sequence e0 consequence))
                         (else
                          (decrement sc 1)
                          (make-test* e0 consequence alternative
                                      ctxt env ec sc))))))))
        ((mutate? exp)
         (let ((x (lookup (mutate-name exp) env))
               (e (mutate-expr exp)))
           (make-sequence (if (not (variable-referenced? x))
                              (cp0 e 'effect env ec sc)
                              (let ((e (cp0 e 'value env ec sc)))
                                (decrement sc 1)
                                (set-variable-residual-mutated?! x #t)
                                (make-mutate x e)))
                          (make-voidcall))))
        ((funcall? exp)
         (let ((op (funcall-operator exp))
               (operand* (funcall-operand* exp)))
           (cond ((and (fix? op) (pair? (fix-lhs* op))
                       (ref? (fix-body op))
                       (memq (ref-name (fix-body op))
                             (fix-lhs* op)))
                  ;; Sometimes code like this is generated:
                  #;((letrec ((lp (lambda (x) ...)))
                       lp)
                     0)
                  ;; This would be better handled by pass-loops:
                  #;(letrec ((lp (lambda (x) ...)))
                      (lp 0))
                  (let ((lhs* (fix-lhs* op)))
                    (cp0 (make-fix lhs* (fix-rhs* op)
                                   (make-funcall (fix-body op)
                                                 operand* #f
                                                 (funcall-source exp)))
                         ctxt env ec sc)))
                 ((and (seq? op) (or (primref? (seq-e1 op)) (ref? (seq-e1 op))))
                  ;; The letrec-prepass can generate code like this:
                  #;((begin (if valid? (void) (undefined-variable 'proc))
                            proc)
                     args ...)
                  (cp0 (make-seq (seq-e0 op)
                                 (make-funcall (seq-e1 op)
                                               operand* #f
                                               (funcall-source exp)))
                       ctxt env ec sc))
                 (else
                  (cp0-call op
                            (map (lambda (opnd)
                                   (make-operand opnd env #f #f ec 0 #f #f))
                                 operand*)
                            (funcall-source exp)
                            ctxt env ec sc)))))
        ((primref? exp)
         (context-case ctxt
           ((app) (fold-prim (primref-name exp) ctxt sc))
           ((value)
            (decrement sc 1)
            (case (primref-name exp)
              #;
              ((&condition-rtd)
               (make-const (record-type-descriptor &condition) #f))
              (else
               exp)))
           ((test effect) (make-const #t #f))))
        ((proc? exp)
         (context-case ctxt
           ((app) (inline exp ctxt env ec sc))
           ((value)
            (decrement sc 1)
            (let ((label (relabel (proc-label exp))))
              (make-proc label (vector 'end label)
                         (map (lambda (pc)
                                (let ((info (proccase-info pc)))
                                  (with-extended-env ((env formals)
                                                      (env (caseinfo-formals info) #f))
                                    (make-proccase (relabel-caseinfo info formals)
                                                   (cp0 (proccase-body pc)
                                                        'value env ec sc)))))
                              (proc-cases exp))
                         (proc-free exp)
                         (proc-name exp)
                         (proc-source exp))))
           ((test effect) (make-const #t #f))))
        ((fix? exp)
         (cp0-fix exp ctxt env ec sc))
        ((bind? exp)
         (let ((lhs* (bind-lhs* exp))
               (rhs* (bind-rhs* exp))
               (body (bind-body exp)))
           (cond
             ((and (pair? lhs*) (null? (cdr lhs*))
                   (test? body)
                   (ref? (test-expr body))
                   (eq? (car lhs*) (ref-name (test-expr body))))
              ;; This is common for (or a b c ...) expressions.
              ;; (let ((t (returns-boolean ...))) (if t a b)) ==>
              ;; (if (returns-boolean ...)
              ;;     (let ((t '#t)) a)
              ;;     (let ((t '#f)) b))
              ;; Can also be done more generally if t is only ever
              ;; used in test context.
              (let ((e0 (cp0 (car (bind-rhs* exp)) 'value env ec sc)))
                (cond ((and (returns-boolean? e0)
                            (not (const? e0)))
                       (let* ((t-op* (list (make-operand (make-const '#t #f)
                                                         env #f #f ec 0 #f #f)))
                              (f-op* (list (make-operand (make-const '#f #f)
                                                         env #f #f ec 0 #f #f)))
                              (consequence (with-extended-env ((env nlhs*) (env lhs* t-op*))
                                             (cp0 (test-then body) ctxt env ec sc)))
                              (alternative (with-extended-env ((env nlhs*) (env lhs* f-op*))
                                             (cp0 (test-else body) ctxt env ec sc))))
                         (decrement sc 1)
                         (make-test* e0 consequence alternative
                                     ctxt env ec sc)))
                      (else
                       ;; XXX: cp0 has already been run once on rhs*...
                       (cp0-bind lhs* (list e0) body ctxt env ec sc)))))
             (else
              (cp0-bind lhs* rhs* body ctxt env ec sc)))))
        (else
         (error who "Unknown type" exp))))


(define (clear-operands x)
  ;; Clear all variable-operand fields. This frees memory and in
  ;; particular frees the continuations in the counters.
  (define who 'clear-operands)
  (define (pass x)
    (cond ((bind? x)
           (for-each pass (bind-lhs* x))
           (for-each pass (bind-rhs* x))
           (pass (bind-body x)))
          ((fix? x)
           (for-each pass (fix-lhs* x))
           (for-each pass (fix-rhs* x))
           (pass (fix-body x)))
          ((rec*? x)
           (for-each pass (rec*-lhs* x))
           (for-each pass (rec*-rhs* x))
           (pass (rec*-body x)))
          ((rec? x)
           (for-each pass (rec-lhs* x))
           (for-each pass (rec-rhs* x))
           (pass (rec-body x)))
          ((proc? x)
           (for-each (lambda (x)
                       (pass (proccase-body x)))
                     (proc-cases x)))
          ((seq? x)
           (pass (seq-e0 x))
           (pass (seq-e1 x)))
          ((mutate? x)
           (pass (mutate-name x))
           (pass (mutate-expr x)))
          ((test? x)
           (pass (test-expr x))
           (pass (test-then x))
           (pass (test-else x)))
          ((funcall? x)
           (pass (funcall-operator x))
           (for-each pass (funcall-operand* x)))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          ((variable? x)
           (set-variable-operand! x #f))
          (else
           (error who "Unknown type" x))))
  (pass x)
  x)

;; Calls to make-record-type-descriptor at the top-level only run
;; once, so they can be made nongenerative.
(define (nongenerative-top-level-records x)
  (define (pass x)
    (cond ((fix? x)
           (make-fix (fix-lhs* x)
                     (map pass (fix-rhs* x))
                     (pass (fix-body x))))
          ((bind? x)
           (make-bind (bind-lhs* x)
                      (map pass (bind-rhs* x))
                      (pass (bind-body x))))
          ((seq? x)
           (make-seq (pass (seq-e0 x))
                     (pass (seq-e1 x))))
          ((test? x)
           (make-test (pass (test-expr x))
                      (pass (test-then x))
                      (pass (test-else x))))
          ((funcall? x)
           (cond ((and (primref? (funcall-operator x))
                       (eq? (primref-name (funcall-operator x))
                            'make-record-type-descriptor)
                       (reasonable-make-rtd-operands? (funcall-operand* x)))
                  (match (funcall-operand* x)
                    [(name parent uid sealed? opaque? fields)
                     ;; Use a different primitive so that the
                     ;; primitive handler knows that this rtd can be
                     ;; treated as nongenerative.
                     (make-funcall (if (const-value uid)
                                       (make-primref 'make-record-type-descriptor/nongenerative)
                                       (make-primref 'make-record-type-descriptor/generative))
                                   (list name parent uid sealed? opaque? fields)
                                   (funcall-label x)
                                   (funcall-source x))]))
                 (else
                  (make-funcall (pass (funcall-operator x))
                                (map pass (funcall-operand* x))
                                (funcall-label x)
                                (funcall-source x)))))
          ((mutate? x) x)
          ((proc? x) x)
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          (else
           (error 'nongenerative-top-level-records
                  "Unknown type" x))))
  (pass x))

(define (pass-cp0 x)
  (let ((x (nongenerative-top-level-records x)))
    (clear-operands
     (cp0 x 'value (new-env) (passive-counter) (passive-counter))))))
