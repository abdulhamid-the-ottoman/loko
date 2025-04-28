;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2020, 2021 G. Weinholt
#!r6rs

;;; R7RS-small standard library

(library (scheme base)
  (export
    _ ... => else
    * + - / < <= = > >= abs and append apply assoc assq
    assv begin binary-port? boolean=? boolean? bytevector
    bytevector-append bytevector-copy bytevector-copy!
    bytevector-length bytevector-u8-ref bytevector-u8-set!
    bytevector? caar cadr call-with-current-continuation
    call-with-port call-with-values call/cc car case cdar cddr
    cdr ceiling char->integer char-ready? char<=? char<? char=?
    char>=? char>? char? close-input-port close-output-port
    close-port complex? cond cond-expand cons current-error-port
    current-input-port current-output-port define
    define-record-type define-syntax define-values denominator
    do dynamic-wind eof-object eof-object? eq? equal? eqv?
    error error-object-irritants error-object-message
    error-object? even? exact exact-integer-sqrt exact-integer?
    exact? expt features file-error? floor floor-quotient
    floor-remainder floor/ flush-output-port for-each gcd
    get-output-bytevector get-output-string guard if include
    include-ci inexact inexact? input-port-open? input-port?
    integer->char integer? lambda lcm length let let*
    let*-values let-syntax let-values letrec letrec*
    letrec-syntax list list->string list->vector list-copy
    list-ref list-set! list-tail list? make-bytevector make-list
    make-parameter make-string make-vector map max member memq
    memv min modulo negative? newline not null? number->string
    number? numerator odd? open-input-bytevector
    open-input-string open-output-bytevector open-output-string
    or output-port-open? output-port? pair? parameterize
    peek-char peek-u8 port? positive? procedure? quasiquote
    quote quotient raise raise-continuable rational? rationalize
    read-bytevector read-bytevector! read-char read-error?
    read-line read-string read-u8 real? remainder reverse round
    set! set-car! set-cdr! square string string->list
    string->number string->symbol string->utf8 string->vector
    string-append string-copy string-copy! string-fill!
    string-for-each string-length string-map string-ref
    string-set! string<=? string<? string=? string>=? string>?
    string? substring symbol->string symbol=? symbol?
    syntax-error syntax-rules textual-port? truncate
    truncate-quotient truncate-remainder truncate/ u8-ready?
    unless unquote unquote-splicing utf8->string values vector
    vector->list vector->string vector-append vector-copy
    vector-copy! vector-fill! vector-for-each vector-length
    vector-map vector-ref vector-set! vector? when
    with-exception-handler write-bytevector write-char
    write-string write-u8 zero?)
  (import
    (rename (except (rnrs) case syntax-rules error define-record-type
                    string->list string-copy string->utf8 vector->list
                    vector-fill! bytevector-copy! bytevector-copy
                    utf8->string
                    map for-each member assoc
                    vector-map read
                    let-syntax
                    expt flush-output-port
                    string-for-each
                    vector-for-each
                    current-output-port
                    current-error-port
                    current-input-port
                    textual-port? binary-port?)
            (open-string-input-port open-input-string))
    (prefix (rnrs) r6:)
    (only (rnrs bytevectors) u8-list->bytevector)
    (only (rnrs control) case-lambda)
    (rnrs conditions)
    (except (rnrs io ports) flush-output-port
            current-output-port current-error-port current-input-port
            textual-port? binary-port?)
    (rnrs mutable-pairs)
    (prefix (rnrs mutable-strings) r6:)
    (only (rnrs mutable-strings) string-set!)
    (rnrs syntax-case)
    (rnrs r5rs)
    (rename (only (loko system r7rs) features
                  input-port-open? output-port-open?
                  current-output-port*
                  current-error-port*
                  current-input-port*
                  include/r7rs
                  u8-ready? char-ready?)
            (current-output-port* current-output-port)
            (current-error-port* current-error-port)
            (current-input-port* current-input-port))
    (only (loko)
          open-output-string get-output-string
          library-available?
          ;; FIXME: These are not SRFI 39 parameters!
          parameterize make-parameter))

;; private helper
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

(define (error message . irritants)
  (if (and (symbol? message) (pair? irritants) (string? (car irritants)))
      (apply r6:error message irritants)
      (apply r6:error #f message irritants)))

;; Based on the definition in R7RS.
(define-syntax cond-expand
  (lambda (x)
    (syntax-case x (and or not else library)
      ((_)
       (syntax-violation 'cond-expand "Unfulfilled cond-expand" x))
      ((_ (else body ...))
       #'(begin body ...))
      ((_ ((and) body ...) more-clauses ...)
       #'(begin body ...))
      ((_ ((and req1 req2 ...) body ...)
          more-clauses ...)
       #'(cond-expand
          (req1
           (cond-expand
            ((and req2 ...) body ...)
            more-clauses ...))
          more-clauses ...))
      ((_ ((or) body ...) more-clauses ...)
       #'(cond-expand more-clauses ...))
      ((_ ((or req1 req2 ...) body ...)
          more-clauses ...)
       #'(cond-expand
          (req1
           (begin body ...))
          (else
           (cond-expand
            ((or req2 ...) body ...)
            more-clauses ...))))
      ((_ ((not req) body ...)
          more-clauses ...)
       #'(cond-expand
          (req
           (cond-expand more-clauses ...))
          (else body ...)))
      ((cond-expand (id body ...)
                    more-clauses ...)
       (memq (syntax->datum #'id) (features))
       #'(begin body ...))
      ((_ ((library lib-name)
           body ...)
          more-clauses ...)
       (library-available? (syntax->datum #'lib-name))
       #'(begin body ...))
      ;; Fallthrough
      ((_ (feature-id body ...)
          more-clauses ...)
       #'(cond-expand more-clauses ...))
      ((_ ((library (name ...))
           body ...)
          more-clauses ...)
       #'(cond-expand more-clauses ...)))))

(define-syntax include
  (lambda (x)
    (syntax-case x ()
      ((k fn* ...)
       #'(begin
           (include/r7rs k #f fn*)
           ...)))))

(define-syntax include-ci
  (lambda (x)
    (syntax-case x ()
      ((k fn* ...)
       #'(begin
           (include/r7rs k #t fn*)
           ...)))))

(define-syntax syntax-error
  (lambda (x)
    (syntax-case x ()
      ((_ message args ...)
       (syntax-violation 'syntax-error #'message '#'(args ...))))))

;; let-syntax from Kato2014.
(define-syntax let-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ ((vars trans) ...) . expr)
       #'(r6:let-syntax ((vars trans) ...)
           (let () . expr))))))

;; From R7RS
(define-syntax define-values
  (lambda (x)
    (syntax-case x ()
      ((define-values () expr)
       #'(define dummy
           (call-with-values (lambda () expr)
                             (lambda args #f))))
      ((define-values (var) expr)
       #'(define var expr))
      ((define-values (var0 var1 ... varn) expr)
       #'(begin
           (define var0
             (call-with-values (lambda () expr)
                               list))
           (define var1
             (let ((v (cadr var0)))
               (set-cdr! var0 (cddr var0))
               v))
           ...
           (define varn
             (let ((v (cadr var0)))
               (set! var0 (car var0))
               v))))
      ((define-values (var0 var1 ... . varn) expr)
       #'(begin
           (define var0
             (call-with-values (lambda () expr)
                               list))
           (define var1
             (let ((v (cadr var0)))
               (set-cdr! var0 (cddr var0))
               v))
           ...
           (define varn
             (let ((v (cdr var0)))
               (set! var0 (car var0))
               v))))
      ((define-values var expr)
       #'(define var
           (call-with-values (lambda () expr)
                             list))))))

(define-syntax define-record-type
  (lambda (x)
    (syntax-case x ()
      ((_ name (constructor cons-arg* ...) predicate
          (field-name* field-rest** ...) ...)
       (begin
         (r6:for-each (lambda (arg)
                        (unless (exists (lambda (field-name)
                                          (bound-identifier=? arg field-name))
                                        #'(field-name* ...))
                          (syntax-violation 'define-record-type
                                            "Bad constructor argument"
                                            x arg)))
                      #'(cons-arg* ...))
         #`(r6:define-record-type (name constructor predicate)
             (r6:sealed #t)
             (r6:fields #,@(let lp ([f* #'((field-name* field-rest** ...) ...)])
                             (syntax-case f* ()
                               [() #'()]
                               [((field-name accessor) rest* ...)
                                #`((r6:immutable field-name accessor)
                                   #,@(lp #'(rest* ...)))]
                               [((field-name accessor modifier) rest* ...)
                                #`((r6:mutable field-name accessor modifier)
                                   #,@(lp #'(rest* ...)))])))
             (r6:protocol
              (lambda (p)
                (let ((field-name* (if #f #f)) ...)
                  (lambda (cons-arg* ...)
                    (p field-name* ...)))))))))))

;;; SRFI-46 style syntax-rules

;; FIXME: We should use with-syntax like:
;;   http://srfi.schemers.org/srfi-93/mail-archive/msg00024.html
(define-syntax syntax-rules
  (lambda (x)
    ;; filt and emap handle ellipsis in the patterns
    (define (filt elip x)
      (if (identifier? x)
          (cond ((free-identifier=? elip x) #'(... ...))
                ((free-identifier=? #'(... ...) x) #'bogus)
                (else x))
          x))
    (define (emap elip in)
      (syntax-case in ()
        ((x . y) (cons (emap elip #'x)
                       (emap elip #'y)))
        (#(x ...) (list->vector (emap elip #'(x ...))))
        (x (filt elip #'x))))
    ;; This translates _ into temporaries and guards -weinholt
    (define (get-underscores stx)
      (syntax-case stx ()
        [(x . y)
         (let-values (((t0 p0) (get-underscores #'x))
                      ((t1 p1) (get-underscores #'y)))
           (values (append t0 t1) (cons p0 p1)))]
        [#(x* ...)
         (let lp ((x* #'(x* ...))
                  (t* '())
                  (p* '()))
           (if (null? x*)
               (values (apply append (reverse t*))
                       (list->vector (reverse p*)))
               (let-values (((t p) (get-underscores (car x*))))
                 (lp (cdr x*) (cons t t*) (cons p p*)))))]
        [x
         (and (identifier? #'x) (free-identifier=? #'x #'_))
         (let ((t* (generate-temporaries #'(_))))
           (values t* (car t*)))]
        [x
         (values '() #'x)]))
    (syntax-case x ()
      ((_ (lit ...) (pat tmpl) ...)     ;compatible with r6rs
       (not (memq '_ (syntax->datum #'(lit ...))))
       #'(r6:syntax-rules (lit ...) (pat tmpl) ...))

      ((_ (lit ...) (pat tmpl) ...)     ;_ in the literals list
       #'(syntax-rules (... ...) (lit ...) (pat tmpl) ...))

      ((_ elip (lit ...) (pat tmpl) ...)  ;custom ellipsis
       (and (identifier? #'elip)
            (not (memq '_ (syntax->datum #'(lit ...)))))
       (with-syntax (((clause ...) (emap #'elip #'((pat tmpl) ...))))
         #'(r6:syntax-rules (lit ...) clause ...)))

      ((_ elip (lit ...) (pat tmpl) ...)
       ;; Both custom ellipsis and _ in the literals list.
       (identifier? #'elip)
       (with-syntax (((clause ...) (emap #'elip #'((pat tmpl) ...)))
                     ((lit^ ...) (filter (lambda (x)
                                           (not (free-identifier=? #'_ x)))
                                         #'(lit ...))))
         (with-syntax (((clause^ ...)
                        (r6:map (lambda (cls)
                                  (syntax-case cls ()
                                    [((_unused . pattern) template)
                                     (let-values (((t p) (get-underscores #'pattern)))
                                       (if (null? t)
                                           #'((_unused . pattern)
                                              #'template)
                                           (with-syntax ((pattern^ p) ((t ...) t))
                                             #'((_unused . pattern^)
                                                (and (underscore? #'t) ...)
                                                #'template))))]))
                                #'(clause ...))))
           #'(lambda (y)
               (define (underscore? x)
                 (and (identifier? x) (free-identifier=? x #'_)))
               (syntax-case y (lit^ ...)
                 clause^ ...))))))))

;;; Case

(define-syntax %r7case-clause
  (syntax-rules (else =>)
    ((_ obj (translated ...) ())
     (r6:case obj translated ...))
    ((_ obj (translated ...) (((e0 e1 ...) => f) rest ...))
     (%r7case-clause obj (translated ... ((e0 e1 ...) (f obj))) (rest ...)))
    ((_ obj (translated ...) ((else => f) rest ...))
     (%r7case-clause obj (translated ... (else (f obj))) (rest ...)))
    ((_ obj (translated ...) (otherwise rest ...))
     (%r7case-clause obj (translated ... otherwise) (rest ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((_ key clause ...)
     (let ((obj key))
       (%r7case-clause obj () (clause ...))))))

;;;

;; R7RS error object will be mapped to R6RS condition object
(define error-object? condition?)
(define file-error? i/o-error?)
(define read-error? lexical-violation?)

(define (error-object-irritants obj)
  (and (irritants-condition? obj)
       (condition-irritants obj)))

(define (error-object-message obj)
  (and (message-condition? obj)
       (condition-message obj)))

;;; Ports

(define open-input-bytevector open-bytevector-input-port)

;; TODO: provide a built-in version of this in (loko)
(define (open-output-bytevector)
  (let-values (((p extract) (open-bytevector-output-port)))
    (define pos 0)
    (define buf #vu8())
    (define (read! target target-start count)
      (when (zero? (- (bytevector-length buf) pos))
        (set! buf (bytevector-append buf (extract))))  ;resets p
      (let ((count (min count (- (bytevector-length buf) pos))))
        (r6:bytevector-copy! buf pos
                             target target-start count)
        (set! pos (+ pos count))
        count))
    (define (write! bv start count)
      (put-bytevector p bv start count)
      (set! pos (+ pos count))
      count)
    (define (get-position)
      pos)
    (define (set-position! new-pos)
      (set! pos new-pos))
    (define (close)
      (close-port p))
    ;; It's actually an input/output port, but only
    ;; get-output-bytevector should ever read from it. If it was just
    ;; an output port then there would be no good way for
    ;; get-output-bytevector to read the data. -weinholt
    (make-custom-binary-input/output-port
     "bytevector" read! write! get-position set-position! close)))

(define (get-output-bytevector port)
  ;; R7RS says "It is an error if port was not created with
  ;; open-output-bytevector.", so we can safely assume that the port
  ;; was created by open-output-bytevector. -weinholt
  (set-port-position! port 0)
  (let ((bv (get-bytevector-all port)))
    (if (eof-object? bv)
        #vu8()
        bv)))

(define (exact-integer? i) (and (integer? i) (exact? i)))

(define peek-u8
  (case-lambda
    (() (lookahead-u8 (current-input-port)))
    ((port) (lookahead-u8 port))))

(define read-bytevector
  (case-lambda
    ((len) (read-bytevector len (current-input-port)))
    ((len port) (get-bytevector-n port len))))

(define read-string
  (case-lambda
    ((len) (read-string len (current-input-port)))
    ((len port) (get-string-n port len))))

(define read-bytevector!
  (case-lambda
    ((bv)
     (read-bytevector! bv (current-input-port)))
    ((bv port)
     (read-bytevector! bv port 0))
    ((bv port start)
     (read-bytevector! bv port start (bytevector-length bv)))
    ((bv port start end)
     (get-bytevector-n! port bv start (fx- end start)))))

(define read-line
  (case-lambda
    (() (read-line (current-input-port)))
    ((port) (get-line port))))

(define write-u8
  (case-lambda
    ((obj) (write-u8 obj (current-output-port)))
    ((obj port) (put-u8 port obj))))

(define read-u8
  (case-lambda
    (() (get-u8 (current-input-port)))
    ((port) (get-u8 port))))

(define write-bytevector
  (case-lambda
    ((bv) (put-bytevector (current-output-port) bv))
    ((bv port) (put-bytevector port bv))
    ((bv port start) (put-bytevector port bv start))
    ((bv port start end) (put-bytevector port bv start (fx- end start)))))

(define write-string
  (case-lambda
    ((str) (put-string (current-output-port) str))
    ((str port) (put-string port str))
    ((str port start) (put-string port str start))
    ((str port start end) (put-string port str start (fx- end start)))))

(define-optional (flush-output-port [(port (current-output-port))])
  (r6:flush-output-port port))

(define (textual-port? obj)
  (and (port? obj) (r6:textual-port? obj)))

(define (binary-port? obj)
  (and (port? obj) (r6:binary-port? obj)))

;;; List additions

(define (list-set! l k obj)
  (let lp ((l l) (k k))
    (if (eqv? k 0)
        (set-car! l obj)
        (lp (cdr l) (fx- k 1)))))

(define-optional (make-list k [(fill #f)])
  (assert (fx>=? k 0))
  (do ((k k (fx- k 1))
       (ret '() (cons fill ret)))
      ((eqv? k 0)
       ret)))

(define (list-copy obj)
  (if (pair? obj)
      (cons (car obj) (list-copy (cdr obj)))
      obj))

(define map
  (case-lambda
    ((proc list)
     (r6:map proc list))
    ((proc list . list*)
     (let lp ((ret '()) (list list) (list* list*))
       (if (or (null? list) (exists null? list*))
           (reverse ret)
           (lp (cons (apply proc (car list) (map car list*)) ret)
               (cdr list) (map cdr list*)))))))

(define for-each
  (case-lambda
    ((proc list)
     (r6:for-each proc list))
    ((proc list . list*)
     (let lp ((list list) (list* list*))
       (cond ((or (null? list) (exists null? list*))
              (if #f #f))
             (else
              (apply proc (car list) (map car list*))
              (lp (cdr list) (map cdr list*))))))))

(define-optional (member obj list [(compare equal?)])
  (let lp ((list list))
    (cond ((null? list)
           #f)
          ((compare obj (car list))
           list)
          (else
           (lp (cdr list))))))

(define-optional (assoc obj alist [(compare equal?)])
  (let lp ((alist alist))
    (cond ((null? alist)
           #f)
          ((compare obj (caar alist))
           (car alist))
          (else
           (lp (cdr alist))))))

;;; Vector and string additions

;; FIXME: Optimize them
(define (string-map proc . strs)
  (list->string (apply map proc (map r6:string->list strs))))

(define (vector-map proc . args)
  (list->vector (apply map proc (map r6:vector->list args))))

(define (bytevector . lis)
  (u8-list->bytevector lis))

(define (bytevector-append . bv*)
  (let ((len (do ((bv* bv* (cdr bv*))
                  (len 0 (fx+ len (bytevector-length (car bv*)))))
                 ((null? bv*) len))))
    (do ((ret (make-bytevector len))
         (bv* bv* (cdr bv*))
         (n 0 (fx+ n (bytevector-length (car bv*)))))
        ((null? bv*) ret)
      (bytevector-copy! ret n (car bv*) 0 (bytevector-length (car bv*))))))

(define (vector-append . vec*)
  (let ((len (do ((vec* vec* (cdr vec*))
                  (len 0 (fx+ len (vector-length (car vec*)))))
                 ((null? vec*) len))))
    (do ((ret (make-vector len))
         (vec* vec* (cdr vec*))
         (n 0 (fx+ n (vector-length (car vec*)))))
        ((null? vec*) ret)
      (vector-copy! ret n (car vec*) 0 (vector-length (car vec*))))))

;;; Substring functionalities added

;; string
(define (%substring1 str start) (substring str start (string-length str)))

(define string->list
  (case-lambda
    ((str) (r6:string->list str))
    ((str start) (r6:string->list (%substring1 str start)))
    ((str start end) (r6:string->list (substring str start end)))))

(define string->vector
  (case-lambda
    ((str) (list->vector (string->list str)))
    ((str start) (string->vector (%substring1 str start)))
    ((str start end) (string->vector (substring str start end)))))

(define string-copy
  (case-lambda
    ((str) (r6:string-copy str))
    ((str start) (%substring1 str start))
    ((str start end) (substring str start end))))

(define string->utf8
  (case-lambda
    ((str) (r6:string->utf8 str))
    ((str start) (r6:string->utf8 (%substring1 str start)))
    ((str start end) (r6:string->utf8 (substring str start end)))))

(define string-fill!
  (case-lambda
    ((str fill) (r6:string-fill! str fill))
    ((str fill start) (string-fill! str fill start (string-length str)))
    ((str fill start end)
     (define (itr r)
       (unless (= r end)
         (string-set! str r fill)
         (itr (+ r 1))))
     (itr start))))

(define string-for-each
  (case-lambda
    ((proc str)
     (r6:string-for-each proc str))
    ((proc str . str*)
     (do ((len (fold-left min (string-length str) (map string-length str*)))
          (i 0 (fx+ i 1)))
         ((fx=? i len))
       (apply proc (string-ref str i)
              (map (lambda (s) (string-ref s i)) str*))))))

(define-optional (string-copy! to at from [(start 0) (end (string-length from))])
  (let ((t to) (ts at) (s from) (ss start) (k (fx- end start)))
    (if (and (eq? t s) (fx<? ss ts))
        (do ((i (fx- k 1) (fx- i 1)))
            ((fx=? i -1))
          (string-set! t (fx+ ts i) (string-ref s (fx+ ss i))))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i k))
          (string-set! t (fx+ ts i) (string-ref s (fx+ ss i)))))))

;;; vector

(define (%subvector v start end)
  (define mlen (- end start))
  (define out (make-vector (- end start)))
  (define (itr r)
    (if (= r mlen)
      out
      (begin
        (vector-set! out r (vector-ref v (+ start r)))
        (itr (+ r 1)))))
  (itr 0))

(define (%subvector1 v start) (%subvector v start (vector-length v)))

(define vector-copy
  (case-lambda
    ((v) (%subvector1 v 0))
    ((v start) (%subvector1 v start))
    ((v start end) (%subvector v start end))))

(define vector->list
  (case-lambda
    ((v) (r6:vector->list v))
    ((v start) (r6:vector->list (%subvector1 v start)))
    ((v start end) (r6:vector->list (%subvector v start end)))))

(define vector->string
  (case-lambda
    ((v) (list->string (vector->list v)))
    ((v start) (vector->string (%subvector1 v start)))
    ((v start end) (vector->string (%subvector v start end)))))

(define vector-fill!
  (case-lambda
    ((vec fill) (r6:vector-fill! vec fill))
    ((vec fill start) (vector-fill! vec fill start (vector-length vec)))
    ((vec fill start end)
     (define (itr r)
       (unless (= r end)
         (vector-set! vec r fill)
         (itr (+ r 1))))
     (itr start))))

(define-optional (vector-copy! to at from [(start 0) (end (vector-length from))])
  (let ((t to) (ts at) (s from) (ss start) (k (fx- end start)))
    (if (and (eq? t s) (fx<? ss ts))
        (do ((i (fx- k 1) (fx- i 1)))
            ((fx=? i -1))
          (vector-set! t (fx+ ts i) (vector-ref s (fx+ ss i))))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i k))
          (vector-set! t (fx+ ts i) (vector-ref s (fx+ ss i)))))))

(define vector-for-each
  (case-lambda
    ((proc vec)
     (r6:vector-for-each proc vec))
    ((proc vec . vec*)
     (do ((len (fold-left fxmin (vector-length vec) (map vector-length vec*)))
          (i 0 (fx+ i 1)))
         ((fx=? i len))
       (apply proc (vector-ref vec i) (map (lambda (s) (vector-ref s i)) vec*))))))

(define (%subbytevector bv start end)
  (define mlen (- end start))
  (define out (make-bytevector mlen))
  (r6:bytevector-copy! bv start out 0 mlen)
  out)

(define (%subbytevector1 bv start)
  (%subbytevector bv start (bytevector-length bv)))

(define bytevector-copy!
  (case-lambda
    ((to at from) (bytevector-copy! to at from 0))
    ((to at from start)
     (let ((flen (bytevector-length from))
           (tlen (bytevector-length to)))
       (let ((fmaxcopysize (fx- flen start))
             (tmaxcopysize (fx- tlen at)))
         (bytevector-copy! to at from start (fx+ start
                                               (fxmin fmaxcopysize
                                                      tmaxcopysize))))))
    ((to at from start end)
     (r6:bytevector-copy! from start to at (fx- end start)))))

(define bytevector-copy
  (case-lambda
    ((bv) (r6:bytevector-copy bv))
    ((bv start) (%subbytevector1 bv start))
    ((bv start end) (%subbytevector bv start end))))

(define utf8->string
  (case-lambda
    ((bv) (r6:utf8->string bv))
    ((bv start) (r6:utf8->string (%subbytevector1 bv start)))
    ((bv start end) (r6:utf8->string (%subbytevector bv start end)))))

;;; From division library

(define-syntax %define-division
  (syntax-rules ()
    ((_ fix quo rem q+r)
     (begin
       (define (quo x y)
         (exact (fix (/ x y))))
       (define (rem x y)
         (- x (* (quo x y) y)))
       (define (q+r x y)
         (let ((q (quo x y)))
           (values q
                   (- x (* q y)))))))))

(%define-division
  floor
  floor-quotient
  floor-remainder0 ;; Most implementation has native modulo
  floor/)
(define floor-remainder modulo)

(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define (truncate/ x y)
  (values (truncate-quotient x y)
          (truncate-remainder x y)))

(define (square x) (* x x))

(define (expt x y)
  (if (eqv? x 0.0)
      (inexact (r6:expt x y))
      (r6:expt x y))))
