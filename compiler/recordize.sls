;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Converts psyntax's output to records

;; TODO: nanopass

(library (loko compiler recordize)
  (export
    pass-recordize record->sexpr
    fix? make-fix fix-lhs* fix-rhs* fix-body
    bind? make-bind bind-lhs* bind-rhs* bind-body
    rec? make-rec rec-lhs* rec-rhs* rec-body
    rec*? make-rec* rec*-lhs* rec*-rhs* rec*-body
    primref? make-primref primref-name
    funcall? make-funcall funcall-operator funcall-operand* funcall-label funcall-source
    const? make-const const-value const-ref set-const-ref!
    seq? make-seq seq-e0 seq-e1
    proc? make-proc proc-label proc-end-label proc-cases proc-free proc-name proc-source
    set-proc-label!
    proccase? make-proccase proccase-info proccase-body
    caseinfo? make-caseinfo caseinfo-label caseinfo-formals caseinfo-proper?
    test? make-test test-expr test-then test-else
    variable? symbol->variable make-variable variable-name
    set-variable-operand! variable-operand
    variable-referenced? variable-mutated?
    variable-residual-referenced? variable-residual-mutated?
    set-variable-mutated?! set-variable-referenced?!
    set-variable-residual-mutated?! set-variable-residual-referenced?!
    variable-singly-referenced? variable-residual-singly-referenced?
    set-variable-singly-referenced?! set-variable-residual-singly-referenced?!
    variable-export-name set-variable-export-name!
    ref? make-ref ref-name
    mutate? make-mutate mutate-name mutate-expr
    labels? make-labels labels-top-level-name labels-proc* labels-body
    closure? make-closure closure-code closure-free*
    mv-call? make-mv-call mv-call-producer mv-call-consumer mv-call-source
    mv-let? make-mv-let mv-let-expr mv-let-lhs* mv-let-body mv-let-source
    mv-values? make-mv-values mv-values-expr* mv-values-source
    tagbody? make-tagbody tagbody-label tagbody-body tagbody-source
    goto? make-goto goto-label goto-source
    infer? make-infer infer-expr infer-facts set-infer-facts!

    artd? make-artd
    artd-name artd-parent artd-uid artd-sealed?
    artd-opaque? artd-fields
    arcd? make-arcd
    arcd-rtd arcd-parent-rcd arcd-protocol)
  (import
    (psyntax compat)          ;for define-record, gensym and annotations
    (loko match)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map))

(define-record rec (lhs* rhs* body))  ;letrec

(define-record rec* (lhs* rhs* body)) ;letrec*

(define-record primref (name))        ;built-in primitives

(define-record funcall (operator operand* label source)) ;application

(define-record const (value ref))     ;quote

(define-record seq (e0 e1))           ;begin

(define-record proc (label end-label cases free name source)) ;case-lambda
(define-record proccase (info body))
(define-record caseinfo (label formals proper?))

(define-record test (expr then else)) ;if

(define-record variable (name operand              ;operand for cp0
                              referenced? mutated? ;for pass-letrec
                              ;; for cp0:
                              residual-referenced? residual-mutated?
                              singly-referenced? residual-singly-referenced?
                              ;; for library-letrec*:
                              export-name))

;; TODO: it's possible that ref is useless, it should all be
;; variables directly.
(define-record ref (name))            ;variables

(define-record mutate (name expr))    ;set!

;; Introduced by pass-let:
(define-record bind (lhs* rhs* body)) ;let

;; Introduced by pass-letrec:
(define-record fix (lhs* rhs* body))  ;"fixing letrec"

;; Introduced by pass-values (see mrvs.pdf):
(define-record mv-call (producer consumer source))
(define-record mv-let (expr lhs* body source))
(define-record mv-values (expr* source))

;; Introduced by pass-loops:
(define-record tagbody (label body source))
(define-record goto (label source))

;; Introduced by pass-closure:
(define-record closure (code free*))
(define-record labels (top-level-name proc* body))

;; Introduced by pass-infer:
(define-record infer (expr facts))

;; Introduced by pass-cp0 (abstract rtd & rcd)
(define-record artd (name parent uid sealed? opaque? fields))
(define-record arcd (rtd parent-rcd protocol))

(define (formals-to-list x)
  (cond ((null? x) x)
        ((pair? x)
         (cons (car x) (formals-to-list (cdr x))))
        (else (list x))))

(define (symbol->variable x)
  (make-variable x #f #f #f #f #f #t #t #f))

(define (inner-name name)
  (and (variable? name)
       (symbol->variable
        (gensym
         (string->symbol
          (string-append
           (symbol->string (variable-name name))
           "°"))))))

(define (inner-label name)
  (assert (symbol? name))
  (gensym
   (string->symbol
    (string-append (symbol->string name) "´"))))

(define (pass-recordize top-level-name x)
  (define (print . x) (for-each display x) (newline))
  ;; The name is the name to give procedures (comes from letrecs)
  (define (pass x label name vars)
    (define (make-var x)
      ;; record a new lexical variable
      (let ((ret (symbol->variable x)))
        (hashtable-set! vars x ret)
        ret))
    (define (mk-caseinfo label formals)
      (let ((f (map make-var (formals-to-list formals)))
            (proper? (list? formals)))
        (make-caseinfo (gensym
                        (if proper?
                            (string-append (symbol->string label)
                                           "=" (number->string (length f)))
                            (string-append (symbol->string label)
                                           "≥" (number->string (fx- (length f) 1)))))
                       f proper?)))
    ;; (print "#;" `(pass ,x ,label ,name))
    (if (symbol? x)
        (cond ((hashtable-ref vars x #f) =>
               (lambda (var)
                 (set-variable-referenced?! var #t)
                 (make-ref var)))
              (else
               (make-funcall (make-primref '$global-ref) (list (make-const x #f))
                             #f #f)))
        (case (car x)
          ((set!)
           (cond ((hashtable-ref vars (cadr x) #f) =>
                  (lambda (var)
                    (set-variable-mutated?! var #t)
                    (make-mutate var (pass (caddr x) label var vars))))
                 (else
                  ;; These are generated by define in the REPL. Are
                  ;; they anywhere else?
                  (make-funcall (make-primref '$set-interaction-env!)
                                (list (make-const (cadr x) #f)
                                      (pass (caddr x) label
                                            (symbol->variable (cadr x))
                                            vars))
                                #f #f))))
          ((library-letrec*)
           ;; (library-letrec* ([local-name exported-name expression] ...) body)
           (let ((binds (cadr x))
                 (body (caddr x)))
             (let ((lhs* (map make-var (map car binds)))
                   (export* (map cadr binds))
                   (rhs* (map caddr binds)))
               ;; Everything here can be exported, either explicitly
               ;; or implicitly (by a macro).
               (for-each set-variable-export-name! lhs* export*)
               (make-rec* lhs* (map (lambda (lhs rhs)
                                      (pass rhs (variable-name lhs) lhs vars))
                                    lhs* rhs*)
                          (pass body label name vars)))))
          ((letrec* letrec)
           (let ((binds (cadr x))
                 (body (caddr x)))
             (let ((lhs* (map make-var (map car binds)))
                   (rhs* (map cadr binds)))
               ((if (eq? (car x) 'letrec*) make-rec* make-rec)
                lhs* (map (lambda (lhs rhs) (pass rhs (variable-name lhs) lhs vars))
                          lhs* rhs*)
                (pass body label name vars)))))
          ((primitive)
           (make-primref (cadr x)))
          ((quote)
           (make-const (cadr x) #f))
          ((begin)
           (let lp ((args (cdr x)))
             (if (null? (cdr args))
                 (pass (car args) label name vars)
                 (make-seq (pass (car args) label #f vars)
                           (lp (cdr args))))))
          ((if)
           (make-test (pass (cadr x) label name vars)
                      (pass (caddr x) label name vars)
                      (pass (cadddr x) label name vars)))

          ((annotated-case-lambda)
           (let ((source (cadr x))
                 (cases (cddr x)))
             (assert label)
             (let* ((formals* (map car cases))
                    (body* (map cadr cases))
                    (label (gensym label)))
               (make-proc label (vector 'end label)
                          (map (lambda (formals body)
                                 (let ((ci (mk-caseinfo label formals)))
                                   (assert label)
                                   (make-proccase ci (pass body (inner-label label)
                                                           (inner-name name) vars))))
                               formals* body*)
                          '()
                          (make-const (and name (variable-name name)) #f)
                          (make-const (and (annotation? source)
                                           (annotation-source source))
                                      #f)))))
          ((case-lambda)
           (let ((cases (cdr x)))
             (let* ((formals* (map car cases))
                    (body* (map cadr cases))
                    (label (gensym label)))
               (make-proc label (vector 'end label)
                          (map (lambda (formals body)
                                 (let ((ci (mk-caseinfo label formals)))
                                   (assert label)
                                   (make-proccase ci (pass body (inner-label label)
                                                           (inner-name name) vars))))
                               formals* body*)
                          '()
                          (make-const (and name (variable-name name)) #f)
                          (make-const #f #f)))))

          ((annotated-call)
           (make-funcall (pass (caddr x) label name vars)
                         (map (lambda (arg)
                                (pass arg label name vars))
                              (cdddr x))
                         #f
                         (and (annotation? (cadr x))
                              (annotation-source (cadr x)))))
          ((qq)
           (pass (cadr x) label name vars))
          (else
           (make-funcall (pass (car x) label name vars)
                         (map (lambda (arg)
                                (pass arg label name vars))
                              (cdr x))
                         #f #f)))))
  ;; Generate a label for top-level anonymous procedures.
  (let ((label (if top-level-name
                   (gensym
                    (call-with-string-output-port
                      (lambda (p)
                        (display "__top·" p)
                        (let lp ((x top-level-name))
                          (cond ((null? x))
                                ((null? (cdr x))
                                 (display (car x) p))
                                (else
                                 (display (car x) p)
                                 (display #\· p)
                                 (lp (cdr x))))))))
                   (gensym))))          ;likely called by eval
    (pass x label #f (make-eq-hashtable))))

(define (record->sexpr x)
  (define who 'record->sexpr)
  (define strip-gensyms
    (if #t
        values
        (let ((names (make-eq-hashtable)))
          (lambda (x)
            (define (strip1 x)
              (string->symbol (symbol->string x)))
            (define (make-sym base nums)
              (string->symbol
               (string-append (symbol->string (strip1 base))
                              "_"
                              (number->string (length nums)))))
            (define (strip x)
              (cond ((pair? x)
                     (cons (strip (car x)) (strip (cdr x))))
                    ((vector? x)
                     (vector-map strip x))
                    ((symbol? x)
                     (let* ((base (strip1 x))
                            (nums (hashtable-ref names base '())))
                       (cond ((eq? base x) x)
                             ((assq x nums) => cdr)
                             (else
                              (let ((name (make-sym x nums)))
                                (hashtable-set! names base (cons (cons x name) nums))
                                name)))))
                    (else x)))
            (strip x)))))
  (define (unbegin x)
    (match x
      [('begin a b)
       `(,@(unbegin a) ,@(unbegin b))]
       [x (list x)]))
  (define (pass x)
    (cond ((fix? x)
           `(fix ,(map list (map strip-gensyms (map variable-name (fix-lhs* x)))
                       (map pass (fix-rhs* x)))
                 ,@(unbegin (pass (fix-body x)))))
          ((bind? x)
           `(let ,(map list (map strip-gensyms (map variable-name (bind-lhs* x)))
                       (map pass (bind-rhs* x)))
              ,@(unbegin (pass (bind-body x)))))
          ((rec*? x)
           `(letrec* ,(map list (map strip-gensyms (map variable-name (rec*-lhs* x)))
                           (map pass (rec*-rhs* x)))
              ,@(unbegin (pass (rec*-body x)))))
          ((rec? x)
           `(letrec ,(map list (map strip-gensyms (map variable-name (rec-lhs* x)))
                          (map pass (rec-rhs* x)))
              ,@(unbegin (pass (rec-body x)))))
          ((caseinfo? x)
           (let ((f* (map variable-name (caseinfo-formals x))))
             (strip-gensyms
              (if (caseinfo-proper? x)
                  f*
                  (let* ((fr (reverse f*))
                         (rest (car fr)))
                    `(,@(reverse (cdr fr)) ,@rest))))))
          ((proc? x)
           (if (= (length (proc-cases x)) 1)
               `(lambda ,(pass (proccase-info (car (proc-cases x))))
                  ',(map pass (proc-free x))
                  ,@(unbegin (pass (proccase-body (car (proc-cases x))))))
               `(case-lambda
                  ,@(map (lambda (y)
                           (append (list (pass (proccase-info y))
                                         `',(map pass (proc-free x)))
                                   (unbegin (pass (proccase-body y)))))
                         (proc-cases x)))))
          ((seq? x)
           `(begin ,(pass (seq-e0 x))
                   ,(pass (seq-e1 x))))
          ((mutate? x)
           `(set! ,(strip-gensyms (variable-name (mutate-name x)))
                  ,(pass (mutate-expr x))))
          ((test? x)
           `(if ,(pass (test-expr x))
                ,(pass (test-then x))
                ,(pass (test-else x))))
          ((const? x)
           `(quote ,(strip-gensyms (const-value x))))
          ((infer? x)
           `(the ,@(reverse (infer-facts x))
                 ,(pass (infer-expr x))))
          ((ref? x)
           (pass (ref-name x)))
          ((variable? x)
           (strip-gensyms (variable-name x)))
          ((funcall? x)
           `(,(pass (funcall-operator x))
             ,@(map pass (funcall-operand* x))))
          ((primref? x)
           (primref-name x))
          ((closure? x)
           `(closure ,(strip-gensyms (proc-label (closure-code x)))
                     ,@(strip-gensyms (map pass (closure-free* x)))))
          ((labels? x)
           `(let-values ,(map (lambda (x)
                                (list (strip-gensyms
                                       (list (or (const-value (proc-name x))
                                                 '*unknown-name*)
                                             (proc-label x)))
                                      (pass x)))
                              (labels-proc* x))
              ,@(unbegin (pass (labels-body x)))))
          ((tagbody? x)
           `(tagbody ,(strip-gensyms (tagbody-label x))
                     ,(pass (tagbody-body x))))
          ((goto? x)
           `(goto ,(strip-gensyms (goto-label x))))
          ((mv-call? x)
           `(mv-call ,(pass (mv-call-producer x)) ,(pass (mv-call-consumer x))))
          ((mv-values? x)
           `(mv-values ,@(map pass (mv-values-expr* x))))
          ((mv-let? x)
           `(let*-values ([,(map strip-gensyms (map variable-name (mv-let-lhs* x)))
                           ,(pass (mv-let-expr x))])
              ,@(unbegin (pass (mv-let-body x)))))
          (else `(bad-type ,x))))
  (pass x)))
