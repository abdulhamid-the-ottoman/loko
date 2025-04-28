;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Eval

;; The true R6RS eval procedure is in (psyntax expander). This is
;; compatibility stuff required by it.

;; This also hosts the interpreter, which will hopefully be going
;; away.

(library (loko runtime eval)
  (export
    void eval-core
    expand expand/optimize)
  (import
    (except (rnrs) map)
    (rnrs mutable-pairs)

    (only (psyntax internal) current-primitive-locations)
    (only (psyntax expander) core-expand interaction-environment
          new-interaction-environment)

    (loko compiler recordize)
    (loko compiler let)
    (loko compiler letrec)
    (loko compiler cp0)
    (loko compiler mutation)
    (loko compiler quasiquote)
    (loko compiler values)
    (loko compiler freevar)
    (loko compiler loops)
    (loko compiler closure)
    (loko compiler infer)
    (loko compiler optimize)
    (loko config)
    (only (loko) pretty-print)
    (rename (loko runtime utils) (map-in-order map))
    (only (loko arch asm) code-generator instruction-analyzer
          target-convention assemble)
    (only (loko runtime symbols) $gensym-generate-names! symbol-value
          set-symbol-value! gensym? *unbound-hack*)
    (loko system $primitives))

(define (void)
  (if #f #f))

(define (compile code)
  (define verbose #f)
  (define (show pass x)
    (when verbose
      (display "#;")
      (display pass)
      (display " ")
      (write (record->sexpr x))
      (newline))
    #f)
  (let* ((x (pass-recordize #f code))  (_ (show 'recordize x))
         (x (pass-let x))              (_ (show 'let x))
         (x (pass-letrec-prepass x))
         (x (pass-letrec x))           (_ (show 'letrec x))
         (x (pass-cp0 x))              (_ (show 'cp0 x))
         (x (pass-let x))              (_ (show 'let x))
         (x (pass-mutation x))         (_ (show 'mutation x))
         (x (pass-quasiquote x))       (_ (show 'quasiquote x)))
    #;
    (let* ((x (pass-infer x))
           (_ (show 'infer x))
           (x (pass-closure '(eval) x))
           (_ (show 'closure x))
           (target-cpu (config-target-cpu)))
      (let*-values ([(text data)
                     (let ([primlocs (current-primitive-locations)]
                           [make-init-code? #f])
                       (code-generator target-cpu (list x) primlocs make-init-code?))]
                    [(text data) (pass-optimize text data
                                                (instruction-analyzer target-cpu)
                                                (target-convention target-cpu))])
        ;; TODO:
        ;;  1. Code wants to init the global environment, but can't
        ;;  2. Symbols that are part of the output need a stable location
        ;;  3. Allocate memory for text and data
        ;;  4. Stack unwinding table needs to be extended or the mechanism needs to be redone.
        ;;  5. Garbage collect the new text/data
        (display "text:\n")
        (for-each (lambda (x) (write x) (newline)) text)
        (display "\ndata:\n")
        (for-each (lambda (x) (write x) (newline)) data)
        (newline)
        (let-values ([(code labels)
                      (assemble target-cpu
                                `((%origin #x600000)
                                  (%section text)
                                  (%label text)
                                  (%mode 64)
                                  (%equiv formals-mismatch ,($linker-address 'formals-mismatch))
                                  (%equiv stop-and-copy ,($linker-address 'stop-and-copy))
                                  ,@text
                                  (%label data)
                                  (%section data)
                                  ,@data
                                  (%label bss)
                                  (%section bss)
                                  (%label bss-end)))])
          (display code)
          (newline)
          (let-values ([(keys vals) (hashtable-entries labels)])
            (write (vector-map cons keys vals))
            (newline)))
        #f))
    x))

(define (expand x)
  (let-values (((code invoke-libraries) (core-expand x (interaction-environment))))
    (record->sexpr (pass-recordize #f code))))

(define (expand/optimize x)
  (let-values (((code invoke-libraries) (core-expand x (interaction-environment))))
    (let* ((x (pass-recordize #f code))
           (x (pass-let x))
           (x (pass-letrec-prepass x))
           (x (pass-letrec x))
           (x (pass-cp0 x))
           (x (pass-let x))
           (x (pass-mutation x))
           (x (pass-quasiquote x))
           (x (pass-values x)))
      (record->sexpr x))))

(define-record-type info
  (sealed #t)
  (nongenerative loko-procinfo-555bfd14-d155-420f-a058-8ccd8ab0301e)
  (fields name (mutable free-length) source label end-label))

;; Takes a proc record and creates a procedure-info object.
(define (make-procedure-info proc)
  (make-info (const-value (proc-name proc)) #f (const-value (proc-source proc)) #f #f))

;; Replace the existing procedure-info on proc with the given one.
;; This is a sensitive operation because the GC uses the info to
;; determine the size of the closure.
(define ($procedure-info-set*! proc info)
  (when (not (info-free-length info))
    ;; record->eval-tree doesn't know how the size of the closure
    ;; created by eval-core, so setting it is delayed until now. This
    ;; means the same closure out of record->eval-tree must never we
    ;; handled by two different lambdas in eval-core.
    (info-free-length-set! info (info-free-length ($procedure-info proc))))
  ($procedure-info-set! proc info))

;; Replaces primitive references with procedures. Replaces variables
;; and references with symbols. Replaces some records with lists
;; (which are significantly faster than records for now).
(define (record->eval-tree x)
  (define who 'record->eval-tree)
  (define (pass x)
    (cond ((ref? x) (variable-name (ref-name x)))
          ((const? x) `(quote ,(const-value x)))
          ((bind? x)
           `(let ,(map (lambda (lhs rhs)
                         (list (variable-name lhs) (pass rhs)))
                       (bind-lhs* x)
                       (bind-rhs* x))
              ,(pass (bind-body x))))
          ((fix? x)
           (make-fix (map variable-name (fix-lhs* x))
                     (map pass (fix-rhs* x))
                     (pass (fix-body x))))
          ((proc? x)
           (let ((cases (proc-cases x))
                 (procedure-info (make-procedure-info x)))
             (if (and (pair? cases) (null? (cdr cases))
                      (caseinfo-proper? (proccase-info (car cases))))
                 (let* ((case0 (car cases))
                        (info0 (proccase-info case0)))
                   ;; Special-case easy lambdas
                   `(lambda ,(map variable-name (caseinfo-formals info0))
                      ,procedure-info
                      ,(pass (proccase-body case0))))
                 `(clam ,procedure-info
                        ,@(map (lambda (x)
                                 (let ((info (proccase-info x)))
                                   (make-proccase
                                    (make-caseinfo #f
                                                   (map variable-name
                                                        (caseinfo-formals info))
                                                   (caseinfo-proper? info))
                                    (pass (proccase-body x)))))
                               (proc-cases x))))))
          ((seq? x)
           `(begin ,(pass (seq-e0 x))
                   ,(pass (seq-e1 x))))
          ((test? x)
           `(if ,(pass (test-expr x))
                ,(pass (test-then x))
                ,(pass (test-else x))))
          ((funcall? x)
           `(funcall ,(pass (funcall-operator x))
                     ,@(map pass (funcall-operand* x))))
          ((primref? x)
           (let ((name (primref-name x)))
             (cond (((current-primitive-locations) name) =>
                    (lambda (sym)
                      (let ((v (symbol-value sym)))
                        (if (procedure? v) v (list 'quote v)))))
                   ((eq? name '$set-interaction-env!) set-symbol-value!)
                   ((eq? name '$global-ref) symbol-value)
                   ((eq? name '$global-set!) set-symbol-value!)
                   (else
                    (lambda _
                      (error 'eval "This primitive is not yet implemented" name))))))
          (else
           (error who "Unknown type" x))))
  (pass x))

(define (arity-assertion info num-expect args)
  (let ((name (info-name info))
        (source (info-source info)))
    (raise
      (condition
       (make-assertion-violation)
       (make-who-condition (string->symbol (symbol->string name)))
       (if (vector? source)
           (make-who-condition
            (string-append (vector-ref source 0) ":"
                           (number->string (vector-ref source 1)) ":"
                           (number->string (vector-ref source 2))))
           (condition))
       (make-message-condition (cond ((not num-expect)
                                      "Expected a different number of arguments")
                                     ((eqv? num-expect 1)
                                      "Expected one argument")
                                     (else
                                      (string-append "Expected "
                                                     (number->string num-expect)
                                                     " arguments"))))
       (make-irritants-condition args)))))

(define (eval-core code)
  (let eval ((x (let ((compiled (compile code)))
                  (when #f
                    (display "#;EVAL: ")
                    (write (record->sexpr compiled))
                    (newline))
                  (let ((eval-tree (record->eval-tree compiled)))
                    (when #f
                      (display "#;EVAL-TREE: ")
                      (write eval-tree)
                      (newline))
                    eval-tree)))
             (env '()))
    (cond
      ((symbol? x)
       (let lp ((env env))
         (if (eq? (caar env) x)
             (cdar env)
             (lp (cdr env)))))
      ((pair? x)
       (let ((x* (cdr x)))
         (case (car x)
           ((quote) (car x*))
           ((if)
            (if (eval (car x*) env)
                (eval (cadr x*) env)
                (eval (caddr x*) env)))
           ((begin)
            (eval (car x*) env)
            (eval (cadr x*) env))
           ((let)
            (let lp ((env env) (bind* (cadr x)))
              (if (null? bind*)
                  (eval (caddr x) env)
                  (let ((bind (car bind*)))
                    (lp (cons (cons (car bind) (eval (cadr bind) env))
                              env)
                        (cdr bind*))))))
           ((funcall)
            (apply (eval (car x*) env)
                   (map (lambda (x) (eval x env))
                        (cdr x*))))
           ((lambda)
            ;; Special-casing for easy lambdas, with only a single body
            ;; and no rest arguments.
            (let* ((formals (car x*))
                   (info (cadr x*))
                   (body (caddr x*))
                   (proc
                    (case (length formals)
                      ((2)
                       (lambda (x y)
                         (eval body `(,(cons (car formals) x)
                                      ,(cons (cadr formals) y)
                                      ,@env))))
                      ((3)
                       (lambda (x y z)
                         (eval body `(,(cons (car formals) x)
                                      ,(cons (cadr formals) y)
                                      ,(cons (caddr formals) z)
                                      ,@env))))
                      ((0)
                       (lambda ()
                         (eval body env)))
                      ((1)
                       (lambda (x)
                         (eval body `(,(cons (car formals) x) ,@env))))
                      ((4)
                       (lambda (x y z w)
                         (eval body `(,(cons (car formals) x)
                                      ,(cons (cadr formals) y)
                                      ,(cons (caddr formals) z)
                                      ,(cons (cadddr formals) w)
                                      ,@env))))
                      (else
                       (lambda x*
                         (unless (fx=? (length x*) (length formals))
                           (arity-assertion info (length formals) x*))
                         (eval body (append (map cons formals x*) env)))))))
              ($procedure-info-set*! proc info)
              proc))
           ((clam)
            (let* ((info (car x*))
                   (cases (cdr x*))
                   (proc
                    (lambda args
                      (let ((arglen (length args)))
                        (let lp ((cases cases))
                          (if (null? cases)
                              (arity-assertion info #f args)
                              (let* ((info (proccase-info (car cases)))
                                     (fml* (caseinfo-formals info)))
                                (cond ((caseinfo-proper? info)
                                       (if (fx=? (length fml*) arglen)
                                           (eval (proccase-body (car cases))
                                                 (append (map (lambda (fml arg)
                                                                (cons fml arg))
                                                              fml* args)
                                                         env))
                                           (lp (cdr cases))))
                                      (else
                                       (if (fx<=? (fx- (length fml*) 1) arglen)
                                           (let lp ((args args)
                                                    (fml* fml*)
                                                    (env env))
                                             (cond ((null? (cdr fml*))
                                                    (eval
                                                     (proccase-body (car cases))
                                                     (cons (cons (car fml*) args)
                                                           env)))
                                                   (else
                                                    (lp (cdr args) (cdr fml*)
                                                        (cons (cons (car fml*)
                                                                    (car args))
                                                              env)))))
                                           (lp (cdr cases))))))))))))
              ($procedure-info-set*! proc info)
              proc))
           (else
            (error 'eval-core "Internal error: unimplemented form" x)))))
      ((procedure? x) x)
      ((fix? x)
       (let* ((new-vars (map (lambda (lhs) (cons lhs *unbound-hack*))
                             (fix-lhs* x)))
              (new-env (append new-vars env)))
         (for-each (lambda (lhs rhs)
                     (set-cdr! lhs (eval rhs new-env)))
                   new-vars
                   (fix-rhs* x))
         (eval (fix-body x) new-env)))
      (else
       (error 'eval-core "Internal error: unimplemented form" x))))))
