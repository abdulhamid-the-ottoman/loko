;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Pairs and lists

(library (loko runtime pairs)
  (export
    pair? cons
    car caar caaar caaaar cdaaar cdaar cadaar cddaar cdar cadar caadar cdadar
    cddar caddar cdddar cdr cadr caadr caaadr cdaadr cdadr cadadr cddadr cddr
    caddr caaddr cdaddr cdddr cadddr cddddr
    null? list? list length append reverse list-tail list-ref
    map for-each
    find for-all exists filter partition fold-left fold-right
    remp remove remv remq
    memp member memv memq
    assp assoc assv assq
    cons*
    set-car! set-cdr!
    fast-memq)
  (import
    (except (rnrs)
            pair? cons
            car caar caaar caaaar cdaaar cdaar cadaar cddaar cdar cadar caadar cdadar
            cddar caddar cdddar cdr cadr caadr caaadr cdaadr cdadr cadadr cddadr cddr
            caddr caaddr cdaddr cdddr cadddr cddddr
            null? list? list length append reverse list-tail list-ref
            map for-each
            find for-all exists filter partition fold-left fold-right
            remp remove remv remq
            memp member memv memq
            assp assoc assv assq
            cons*)
    (prefix (rnrs) sys:)
    (prefix (only (rnrs mutable-pairs) set-car! set-cdr!) sys:))

(define-syntax define-integrable
  (lambda (x)
    (syntax-case x ()
      ((_ (name args ...) v)
       #'(define-syntax name (identifier-syntax (lambda (args ...) v)))))))

(define-integrable (%fast-map f x* nil)
  ;; This is map, except it only works with one list, and it need
  ;; not care about dynamic extents or improper lists, and appends a
  ;; tail.
  (let lp ((x* x*) (ret nil) (prev #f))
    (cond ((null? x*)
           ret)
          (else
           (let ((new-tail (cons (f (car x*)) nil)))
             (cond ((not prev)
                    (lp (cdr x*) new-tail new-tail))
                   (else
                    (set-cdr! prev new-tail)
                    (lp (cdr x*) ret new-tail))))))))

#;
(define-integrable (%map! f list)
  (let lp ((x* list))
    (cond ((null? x*)
           list)
          (else
           (set-car! x* (f (car x*)))
           (lp (cdr x*))))))

(define-integrable (%fast-for-all f x*)
  ;; Same as above.
  (let lp ((x* x*))
    (cond ((null? x*) #t)
          ((not (f (car x*))) #f)
          (else (lp (cdr x*))))))

(define (pair? x) (sys:pair? x))

(define (cons x y) (sys:cons x y))

(define (car x) (sys:car x))
(define (caar x) (sys:car (sys:car x)))
(define (caaar x) (sys:car (sys:car (sys:car x))))
(define (caaaar x) (sys:car (sys:car (sys:car (sys:car x)))))
(define (cdaaar x) (sys:cdr (sys:car (sys:car (sys:car x)))))
(define (cdaar x) (sys:cdr (sys:car (sys:car x))))
(define (cadaar x) (sys:car (sys:cdr (sys:car (sys:car x)))))
(define (cddaar x) (sys:cdr (sys:cdr (sys:car (sys:car x)))))
(define (cdar x) (sys:cdr (sys:car x)))
(define (cadar x) (sys:car (sys:cdr (sys:car x))))
(define (caadar x) (sys:car (sys:car (sys:cdr (sys:car x)))))
(define (cdadar x) (sys:cdr (sys:car (sys:cdr (sys:car x)))))
(define (cddar x) (sys:cdr (sys:cdr (sys:car x))))
(define (caddar x) (sys:car (sys:cdr (sys:cdr (sys:car x)))))
(define (cdddar x) (sys:cdr (sys:cdr (sys:cdr (sys:car x)))))
(define (cdr x) (sys:cdr x))
(define (cadr x) (sys:car (sys:cdr x)))
(define (caadr x) (sys:car (sys:car (sys:cdr x))))
(define (caaadr x) (sys:car (sys:car (sys:car (sys:cdr x)))))
(define (cdaadr x) (sys:cdr (sys:car (sys:car (sys:cdr x)))))
(define (cdadr x) (sys:cdr (sys:car (sys:cdr x))))
(define (cadadr x) (sys:car (sys:cdr (sys:car (sys:cdr x)))))
(define (cddadr x) (sys:cdr (sys:cdr (sys:car (sys:cdr x)))))
(define (cddr x) (sys:cdr (sys:cdr x)))
(define (caddr x) (sys:car (sys:cdr (sys:cdr x))))
(define (caaddr x) (sys:car (sys:car (sys:cdr (sys:cdr x)))))
(define (cdaddr x) (sys:cdr (sys:car (sys:cdr (sys:cdr x)))))
(define (cdddr x) (sys:cdr (sys:cdr (sys:cdr x))))
(define (cadddr x) (sys:car (sys:cdr (sys:cdr (sys:cdr x)))))
(define (cddddr x) (sys:cdr (sys:cdr (sys:cdr (sys:cdr x)))))

(define (null? x) (sys:null? x))

(define-syntax cdr/f
  (lambda (x)
    (syntax-case x ()
      ((_ v)
       #'(let ((t v))
           (and (sys:pair? t) (cdr t)))))))
(define-syntax cddr/f                   ;TODO: don't use; wasteful
  (lambda (x)
    (syntax-case x ()
      ((_ v)
       #'(let ((t v))
           (cdr/f (cdr/f t)))))))

(define (list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
        (let ((x (cdr x)))
          (if (pair? x)
              (let ((x (cdr x))
                    (lag (cdr lag)))
                (and (not (eq? x lag))
                     (lp x lag)))
              (null? x)))
        (null? x))))

(define (list . x) x)

(define (length x0)
  (define (err x)
    (assertion-violation 'length "Expected a proper list" x))
  ;; TODO: this is expected to be receive a list, so it could start
  ;; out under that assumption and switch to the tortoise-and-hare if
  ;; the list turns out to be very long
  (let lp ((x x0) (lag x0) (len 0))
    (if (pair? x)
        (let ((x (cdr x))
              (len (fx+ len 1)))
          (if (pair? x)
              (let ((x   (cdr x))
                    (lag (cdr lag))
                    (len (fx+ len 1)))
                (cond ((eq? x lag)
                       (err x0))
                      (else
                       (lp x lag len))))
              (if (null? x) len (err x0))))
        (if (null? x) len (err x0)))))

(define (check-list x0 who)
  (let lp ((x x0) (i 0))
    (cond ((fx=? i 1000)
           (unless (sys:list? x)
             (assertion-violation who "Expected a proper list" x0)))
          ((pair? x)
           (lp (cdr x) (fx+ i 1)))
          ((null? x))
          (else
           (assertion-violation who "Expected a proper list" x0)))))

(define append
  (case-lambda
    ((x obj)
     (check-list x 'append)
     (let lp ((x x))
       (if (null? x)
           obj
           (cons (car x) (lp (cdr x))))))
    ((x . x*)
     (let lp* ((x x) (x* x*))
       (cond
         ((null? x*)
          x)
         (else
          (check-list x 'append)
          (let lp ((x x) (x* x*))
            (if (null? x)
                (lp* (car x*) (cdr x*))
                (cons (car x) (lp (cdr x) x*))))))))
    (() '())))

(define (reverse x)
  (check-list x 'reverse)
  (let lp ((x x) (r '()))
    (if (null? x)
        r
        (lp (cdr x) (cons (car x) r)))))

(define (%fast-reverse x)
  (let lp ((x x) (r '()))
    (if (null? x)
        r
        (lp (cdr x) (cons (car x) r)))))

(define (list-tail list k)
  (define who 'list-tail)
  (when (or (not (fixnum? k)) (fxnegative? k))
    (assertion-violation who "Expected a non-negative fixnum" k))
  (let lp ((l list) (i k))
    (cond ((eqv? i 0) l)
          ((pair? l) (lp (cdr l) (fx+ i -1)))
          (else
           (assertion-violation who "Expected a list of length at least k" list k)))))

(define (list-ref list k)
  (define who 'list-ref)
  (when (or (not (fixnum? k)) (fxnegative? k))
    (assertion-violation who "Expected a non-negative fixnum" k))
  (let lp ((l list) (i k))
    (if (pair? l)
        (if (eqv? i 0) (car l) (lp (cdr l) (fx+ i -1)))
        (assertion-violation who "Expected a list of length at least k+1" list k))))

(define map
  (case-lambda
    ((f x*)
     (check-list x* 'map)
     (let lp ((x* x*) (r '()))
       (if (null? x*)
           (%fast-reverse r)
           (lp (sys:cdr x*)
               (cons (f (sys:car x*)) r)))))
    ((f x* y*)
     (check-list x* 'map)
     (let lp ((x* x*) (y* y*) (r '()))
       (cond ((null? x*)
              (unless (null? y*)
                (assertion-violation 'map "Expected lists of the same length" f x* y*))
              (%fast-reverse r))
             (else
              (unless (pair? y*)
                (assertion-violation 'map "Expected lists of the same length" f x* y*))
              (lp (sys:cdr x*) (sys:cdr y*)
                  (cons (f (sys:car x*) (sys:car y*)) r))))))
    ((f x . x*)
     (cond ((null? x)
            (unless (%fast-for-all null? x*)
              (apply assertion-violation 'map "Expected lists of the same length" f x x*))
            '())
           (else
            (let lp ((t (cdr/f x)) (h (cddr/f x)) (l x) (l* x*) (r '()))
              (cond
                ((eq? t h)
                 (assertion-violation 'map "Expected a proper list" f x))
                (else
                 (unless (%fast-for-all pair? l*)
                   (apply assertion-violation "Expected lists of the same length" f x x*))
                 (let ((r (sys:cons (apply f (sys:car l) (%fast-map sys:car l* '())) r)))
                   (cond ((null? t)
                          (unless (%fast-for-all (lambda (x) (and (pair? x) (null? (cdr x)))) l*)
                            (apply assertion-violation "Expected lists of the same length" f x x*))
                          (%fast-reverse r))
                         (else
                          (lp (cdr/f t) (cddr/f h) (sys:cdr l)
                              (%fast-map sys:cdr l* '()) r))))))))))))

(define for-each
  (case-lambda
    ((f x*)
     (check-list x* 'for-each)
     (unless (null? x*)
       (let lp ((x* x*))
         (cond
           ((null? (cdr x*))
            (f (car x*)))
           (else
            (f (car x*))
            (lp (sys:cdr x*)))))))
    ((f x* y*)
     (check-list x* 'for-each)
     (cond
       ((null? x*)
        (unless (null? y*)
          (assertion-violation 'for-each "Expected lists of the same length" f x* y*)))
       (else
        (let lp ((x* x*) (y* y*))
          (cond ((null? (cdr x*))
                 (unless (null? (cdr y*))
                   (assertion-violation 'for-each "Expected lists of the same length" f x* y*))
                 (f (car x*) (car y*)))
                (else
                 (unless (pair? y*)
                   (assertion-violation 'for-each "Expected lists of the same length" f x* y*))
                 (f (car x*) (car y*))
                 (lp (sys:cdr x*) (sys:cdr y*))))))))
    ;; TODO: add checks
    ((f l1 . ls)
     (cond ((null? l1)
            (unless (%fast-for-all null? ls)
              (assertion-violation 'for-each
                                   "Expected lists of the same length" f l1 ls)))
           (else
            (let lp ((l1 l1) (ls ls))
              (cond ((null? (cdr l1))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) ls)
                       (assertion-violation 'for-each
                                            "Expected lists of the same length" f l1 ls))
                     (apply f (car l1) (%fast-map sys:car ls '())))
                    (else
                     (apply f (car l1) (%fast-map sys:car ls '()))
                     (let ((l1 (cdr l1)) (ls (%fast-map sys:cdr ls '())))
                       (let ((has-null (sys:exists sys:null? ls)))
                         (when (or (and (null? l1) (not has-null))
                                   (and (pair? l1) has-null))
                           (assertion-violation 'for-each
                                                "Expected lists of the same length" f l1 ls)))
                       (lp l1 ls))))))))))

;;; (rnrs lists)

;; find is defined later.

(define for-all
  (case-lambda
    ((p list)
     (let lp ((t list) (h list))
       (cond
         ((pair? h)
          (let ((x (car h))
                (h (cdr h)))
            (cond ((null? h) (p x))
                  ((not (p x)) #f)
                  ((pair? h)
                   (cond ((null? (cdr h)) (p (car h)))
                         ((not (p (car h))) #f)
                         (else
                          (if (eq? t h)
                              (assertion-violation 'for-all "Expected a list (found a circular list)" p list)
                              (lp (cdr t) (cdr h))))))
                  (else
                   (assertion-violation 'for-all "Expected a list (found an improper list)" p list)))))
         ((null? h) #t)
         (else
          (assertion-violation 'for-all "Expected a list (found an improper list)" p list)))))
    ((p l1 . ls)
     (define (go-wrong)
       (assertion-violation 'for-all
                            "The lists do not have the same length" p l1 ls))
     (cond ((null? l1)
            (or (%fast-for-all null? ls) (go-wrong)))
           (else
            ;; FIXME: check that the lists are proper
            (let lp ((l1 l1) (ls ls))
              (cond ((null? (cdr l1))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) ls)
                       (go-wrong))
                     (apply p (car l1) (%fast-map sys:car ls '())))
                    (else
                     (and (apply p (car l1) (%fast-map sys:car ls '()))
                          (let ((l1 (cdr l1)) (ls (%fast-map sys:cdr ls '())))
                            (let ((has-null (sys:exists sys:null? ls)))
                              (when (or (and (null? l1) (not has-null))
                                        (and (pair? l1) has-null))
                                (go-wrong)))
                            (lp l1 ls)))))))))))

(define exists
  (case-lambda
    ((p list)
     (let lp ((t list) (h list))
       (cond
         ((pair? h)
          (let ((x (car h))
                (h (cdr h)))
            (cond ((null? h) (p x))
                  ((p x))
                  ((pair? h)
                   (cond ((null? (cdr h)) (p (car h)))
                         ((p (car h)))
                         (else
                          (if (eq? t h)
                              (assertion-violation 'exists "Expected a list (found a circular list)" p list)
                              (lp (cdr t) (cdr h))))))
                  (else
                   (assertion-violation 'exists "Expected a list (found an improper list)" p list)))))
         ((null? h) #f)
         (else
          (assertion-violation 'exists "Expected a list (found an improper list)" p list)))))
    ((p lst . lst*)
     (cond ((null? lst)
            (unless (%fast-for-all null? lst*)
              (apply assertion-violation 'exists
                     "The lists are not the same length"
                     p lst lst*))
            #f)
           (else
            (let lp ((t (cdr/f lst)) (h (cddr/f lst)) (x lst) (x* lst*))
              (cond ((eq? t h)
                     (apply assertion-violation 'exists
                            "The first list is not a proper list"
                            p lst lst*))
                    ((null? (cdr x))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) x*)
                       (if (%fast-for-all sys:list? x*)
                           (apply assertion-violation 'exists
                                  "One of the lists is longer than the first list"
                                  p lst lst*)
                           (apply assertion-violation 'exists
                                  "One of the lists is not a proper list"
                                  p lst lst*)))
                     (apply p (car x) (%fast-map sys:car x* '())))
                    (else
                     (let ((cdrs (%fast-map cdr x* '())))
                       (unless (%fast-for-all pair? cdrs)
                         (if (%fast-for-all sys:list? cdrs)
                             (apply assertion-violation 'exists
                                    "One of the lists is shorter than the first list"
                                    p lst lst*)
                             (apply assertion-violation 'exists
                                    "One of the lists is not a proper list"
                                    p lst lst*)))
                       (or (apply p (car x) (%fast-map car x* '()))
                           (lp (cdr/f t) (cddr/f h) (cdr x) cdrs)))))))))))

(define (partition proc list)
  (if (null? list)
      (values '() '())
      (let lp ((rett '()) (retf '()) (x list) (t (cdr/f list)) (h (cddr/f list)))
        (cond ((null? x)
               (values (%fast-reverse rett) (%fast-reverse retf)))
              ((eq? t h)
               (assertion-violation 'partition
                                    "This is not a proper list" list))
              ((proc (car x))
               (lp (cons (car x) rett) retf t (cdr/f t) (cddr/f h)))
              (else
               (lp rett (cons (car x) retf) t (cdr/f t) (cddr/f h)))))))

(define fold-left
  (case-lambda
    ((f nil lst)
     (if (null? lst)
         nil
         (let fold-left:lp ((acc nil) (t (cdr/f lst)) (h (cddr/f lst)) (x lst))
           (cond ((eq? t h)
                  (assertion-violation 'fold-left
                                       "The list argument is not a proper list"
                                       f nil lst))
                 ((null? (cdr x))
                  (f acc (car x)))
                 (else
                  (let ((acc (f acc (car x))))
                    (fold-left:lp acc (cdr/f t) (cddr/f h) (cdr x))))))))
    ((f nil lst . lst*)
     (cond ((null? lst)
            (unless (%fast-for-all null? lst*)
              (apply assertion-violation 'fold-left
                     "The lists are not the same length"
                     f nil lst lst*))
            nil)
           (else
            (let lp ((acc nil) (t (cdr/f lst)) (h (cddr/f lst)) (x lst) (x* lst*))
              (cond ((eq? t h)
                     (apply assertion-violation 'fold-left
                            "The first list is not a proper list"
                            f nil lst lst*))
                    ((null? (cdr x))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) x*)
                       (if (%fast-for-all sys:list? x*)
                           (apply assertion-violation 'fold-left
                                  "One of the lists is longer than the first list"
                                  f nil lst lst*)
                           (apply assertion-violation 'fold-left
                                  "One of the lists is not a proper list"
                                  f nil lst lst*)))
                     (apply f acc (car x) (%fast-map sys:car x* '())))
                    (else
                     ;; TODO: this could update x* in place and avoid
                     ;; making new lists. There could also be an
                     ;; apply-cars primitive that applies the cars of
                     ;; the lists, without building a new list to hold
                     ;; the cars.
                     (let ((cdrs (%fast-map sys:cdr x* '())))
                       (unless (%fast-for-all pair? cdrs)
                         (if (%fast-for-all sys:list? cdrs)
                             (apply assertion-violation 'fold-left
                                    "One of the lists is shorter than the first list"
                                    f nil lst lst*)
                             (apply assertion-violation 'fold-left
                                    "One of the lists is not a proper list"
                                    f nil lst lst*)))
                       (let ((acc (apply f acc (car x) (%fast-map sys:car x* '()))))
                         (lp acc (cdr/f t) (cddr/f h) (cdr x) cdrs)))))))))))

(define fold-right
  (case-lambda
    ((f nil lst)
     (if (null? lst)
         nil
         (let lp ((t (cdr/f lst)) (h (cddr/f lst)) (x lst))
           (cond ((eq? t h)
                  (assertion-violation 'fold-right
                                       "The list argument is not a proper list"
                                       f nil lst))
                 ((null? (cdr x))
                  (f (car x) nil))
                 (else
                  (f (car x) (lp (cdr/f t) (cddr/f h) (cdr x))))))))
    ((f nil lst . lst*)
     (cond ((null? lst)
            (unless (%fast-for-all sys:null? lst*)
              (apply assertion-violation 'fold-right
                     "The lists are not the same length"
                     f nil lst lst*))
            nil)
           (else
            (let lp ((t (cdr/f lst)) (h (cddr/f lst)) (x lst) (x* lst*))
              (cond ((eq? t h)
                     (apply assertion-violation 'fold-right
                            "The first list is not a proper list"
                            f nil lst lst*))
                    ((null? (cdr x))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) x*)
                       ;; This is the error case, so it doesn't
                       ;; matter that it's a little slower.
                       (if (%fast-for-all sys:list? x*)
                           (apply assertion-violation 'fold-right
                                  "One of the lists is longer than the first list"
                                  f nil lst lst*)
                           (apply assertion-violation 'fold-right
                                  "One of the lists is not a proper list"
                                  f nil lst lst*)))
                     (apply f (car x) (%fast-map sys:car x* (list nil))))
                    (else
                     ;; TODO: this needs an apply-cars that also
                     ;; takes an extra last argument.
                     (let ((cdrs (%fast-map sys:cdr x* '())))
                       (unless (%fast-for-all sys:pair? cdrs)
                         (if (%fast-for-all sys:list? cdrs)
                             (apply assertion-violation 'fold-right
                                    "One of the lists is shorter than the first list"
                                    f nil lst lst*)
                             (apply assertion-violation 'fold-right
                                    "One of the lists is not a proper list"
                                    f nil lst lst*)))
                       (apply f (car x)
                              (let ((nil^ (lp (cdr/f t) (cddr/f h) (cdr x) cdrs)))
                                (%fast-map sys:car x* (list nil^)))))))))))))

;; The tail-recursive variant. This is used when a user-specified
;; procedure can be called, because it might capture the stack, and
;; the stack might be enormous if the non-tail-recursive variant is
;; used.
(define-syntax define-remove
  (lambda (x)
    (syntax-case x ()
      ((_ name obj match?)
       #'(define (name obj list)
           (let lp ((ret '()) (t list) (h list))
             (cond
               ((pair? h)
                (let ((x (car h))
                      (h (cdr h)))
                  (if (not (match? x))
                      (let ((ret (sys:cons x ret)))
                        (cond
                          ((pair? h)
                           (let ((x^ (car h)))
                             (when (eq? t h)
                               (assertion-violation 'name "Expected a list (found a circular list)" obj list))
                             (if (not (match? x^))
                                 (lp (sys:cons x^ ret) (cdr t) (cdr h))
                                 (lp ret (cdr t) (cdr h)))))
                          ((null? h)
                           (%fast-reverse ret))
                          (else
                           (assertion-violation 'name "Expected a proper list" obj list))))
                      (cond
                        ((pair? h)
                         (let ((x^ (car h)))
                           (when (eq? t h)
                             (assertion-violation 'name "Expected a list (found a circular list)" obj list))
                           (if (not (match? x^))
                               (lp (sys:cons x^ ret) (cdr t) (cdr h))
                               (lp ret (cdr t) (cdr h)))))
                        ((null? h)
                         (%fast-reverse ret))
                        (else
                         (assertion-violation 'name "Expected a proper list" obj list))))))
               ((null? h)
                (%fast-reverse ret))
               (else
                (assertion-violation 'name "Expected a proper list" obj list)))))))))

(define-syntax define-remove*
  (lambda (x)
    (syntax-case x ()
      ((_ name obj match?)
       #'(define (name obj list)
           (let lp ((t list) (h list))
             (cond
               ((pair? h)
                (let ((x (car h))
                      (h (cdr h)))
                  (if (not (match? x))
                      (cond
                        ((pair? h)
                         (let ((x^ (car h)))
                           (when (eq? t h)
                             (assertion-violation 'name "Expected a list (found a circular list)" obj list))
                           (if (not (match? x^))
                               (cons x (cons x^ (lp (cdr t) (cdr h))))
                               (cons x (lp (cdr t) (cdr h))))))
                        ((null? h)
                         (cons x '()))
                        (else
                         (assertion-violation 'name "Expected a proper list" obj list)))
                      (cond
                        ((pair? h)
                         (let ((x^ (car h)))
                           (when (eq? t h)
                             (assertion-violation 'name "Expected a list (found a circular list)" obj list))
                           (if (not (match? x^))
                               (cons x^ (lp (cdr t) (cdr h)))
                               (lp (cdr t) (cdr h)))))
                        ((null? h)
                         '())
                        (else
                         (assertion-violation 'name "Expected a proper list" obj list))))))
               ((null? h)
                '())
               (else
                (assertion-violation 'name "Expected a proper list" obj list)))))))))

(define-remove filter proc (lambda (x) (not (proc x))))
(define-remove remp proc proc)
(define-remove* remove obj (lambda (x) (equal? obj x)))
(define-remove* remv obj (lambda (x) (eqv? obj x)))
(define-remove* remq obj (lambda (x) (eq? obj x)))

(define-syntax define-member
  (lambda (x)
    (syntax-case x ()
      ((_ name obj return found?)
       #'(define (name obj list)
           (let lp ((t list) (h list))
             (cond
               ((pair? h)
                (let ((x (car h)))
                  (if (found? x)
                      (return h)
                      (let ((h (cdr h)))
                        ;; Матрёшка
                        (cond
                          ((pair? h)
                           (let ((x (car h)))
                             (if (found? x)
                                 (return h)
                                 (if (eq? t h)
                                     (assertion-violation 'name "Expected a list (found a circular list)" obj list)
                                     (lp (cdr t) (cdr h))))))
                          ((null? h) #f)
                          (else
                           (assertion-violation 'name "Expected a list (found an improper list)" obj list)))))))
               ((null? h) #f)
               (else
                (assertion-violation 'name "Expected a list (found an improper list)" obj list)))))))))

(define-member find proc (lambda (x) (car x)) proc)
(define-member memp proc (lambda (x) x) proc)
(define-member member obj (lambda (x) x) (lambda (x) (equal? obj x)))
(define-member memv obj (lambda (x) x) (lambda (x) (eqv? obj x)))
(define-member memq obj (lambda (x) x) (lambda (x) (eq? obj x)))

;; Unrolled memq that does not check for non-lists. Unrolling should
;; help the branch predictor.
(define (fast-memq x lst)
  (if (null? lst)
      #f
      (if (eq? (car lst) x)
          lst
          (let ((lst (cdr lst)))
            (if (null? lst)
                #f
                (if (eq? (car lst) x)
                    lst
                    (let ((lst (cdr lst)))
                      (if (null? lst)
                          #f
                          (if (eq? (car lst) x)
                              lst
                              (let lp ((lst (cdr lst)))
                                (if (null? lst)
                                    #f
                                    (if (eq? (car lst) x)
                                        lst
                                        (let ((lst (cdr lst)))
                                          (lp lst))))))))))))))

(define-syntax define-assoc
  (lambda (x)
    (syntax-case x ()
      ((_ name obj found?)
       #'(define (name obj alist)
           (let lp ((t alist) (h alist))
             (cond
               ((pair? h)
                (let ((x (car h)))
                  (if (pair? x)
                      (if (found? (car x))
                          x
                          (let ((h (cdr h)))
                            ;; Матрёшка
                            (cond
                              ((pair? h)
                               (let ((x (car h)))
                                 (if (pair? x)
                                     (if (found? (car x))
                                         x
                                         (if (eq? t h)
                                             (assertion-violation 'name "Expected an association list (found a circular list)" obj alist)
                                             (lp (cdr t) (cdr h))))
                                     (assertion-violation 'name "Expected an association list" obj alist))))
                              ((null? h) #f)
                              (else
                               (assertion-violation 'name "Expected an association list (found an improper list)" obj alist)))))
                      (assertion-violation 'name "Expected an association list" obj alist))))
               ((null? h) #f)
               (else
                (assertion-violation 'name "Expected an association list (found an improper list)" obj alist)))))))))

(define-assoc assp proc proc)
(define-assoc assoc obj (lambda (x) (equal? obj x)))
(define-assoc assv obj (lambda (x) (eqv? obj x)))
(define-assoc assq obj (lambda (x) (eq? obj x)))

(define cons*
  (case-lambda
    ((a) a)
    ((a b) (sys:cons a b))
    (ret
     (let lp ((xs (cdr ret)) (prev ret))
       (cond ((null? (cdr xs))
              (set-cdr! prev (car xs))
              ret)
             (else
              (lp (cdr xs) xs)))))))

;; (rnrs mutable-pairs)
(define (set-car! x v) (sys:set-car! x v))
(define (set-cdr! x v) (sys:set-cdr! x v)))
