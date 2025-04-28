;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; This file is a part of Loko Scheme, an R6RS Scheme system
;; Copyright © 2019, 2020, 2021 G. Weinholt
#!r6rs

;;; Hashtables

;; Eq hashtables that use buckets have a garbage collection count. If
;; the current garbage collection counter has changed since last time
;; the hashtable was updated it is necessary to rehash the table.

(library (loko runtime hashtables)
  (export
    make-eq-hashtable make-eqv-hashtable
    (rename (make-general-hashtable make-hashtable))
    hashtable? hashtable-size hashtable-ref hashtable-set!
    hashtable-delete! hashtable-contains? hashtable-update!
    hashtable-copy hashtable-clear! hashtable-keys
    hashtable-entries
    ;; Inspection
    (rename (hashtable-cmp hashtable-equivalence-function))
    hashtable-hash-function hashtable-mutable?
    ;; Hash functions
    string-hash string-ci-hash)
  (import
    (except (rnrs)
            make-eq-hashtable make-eqv-hashtable make-hashtable
            hashtable? hashtable-size hashtable-ref hashtable-set!
            hashtable-delete! hashtable-contains? hashtable-update!
            hashtable-copy hashtable-clear! hashtable-keys
            hashtable-entries
            hashtable-equivalence-function
            hashtable-hash-function hashtable-mutable?
            string-hash string-ci-hash)
    (prefix (only (rnrs) make-eq-hashtable make-eqv-hashtable make-hashtable
                  hashtable-clear!)
            sys:)
    (rnrs mutable-pairs)
    (only (loko runtime equal) eq-hash eqv-hash)
    (only (loko runtime utils) string-hash*)
    (only (loko system $primitives) $immsym? $object->fixnum $void?)
    (only (loko) record-writer collections))

(define-syntax print
  (syntax-rules ()
    #;
    ((_ . args) (begin (when (procedure? display) (for-each display (list . args)) (newline))))
    ((_ . args) (begin 'dummy))))

(define INITIAL-SIZE 8)

(define (ht-initial-size k)
  (min (expt 2 20)
       (max 8 (+ k (div k 4)))))

(define-record-type hashtable
  (sealed #t)
  (nongenerative loko-ht-20959f3b-b6c9-47a7-8ad7-e970231ca229)
  (fields (mutable vals)
          (mutable size)
          (mutable gc-count)            ;#f or fixnum
          (immutable cmp*)
          (immutable hash*)
          (mutable mutable?)            ;#t or #f only
          (immutable gc-sensitive?) ;TODO: would be better if this was not all-or-nothing
          ;; Watermarks for deciding when to resize the vals vector
          (mutable low-watermark)
          (mutable high-watermark)))

(define (update-watermarks! ht)
  (let ((num-buckets (vector-length (hashtable-vals ht))))
    (let ((low (round (* #e0.50 num-buckets)))
          (high (round (* #e0.75 num-buckets))))
      (assert (fx<? low high))
      ;; When the hashtable size is relatively small, don't care about
      ;; shrinking it to save memory (rehashing also has a cost).
      (hashtable-low-watermark-set! ht (if (fx<? low 32) 0 low))
      (hashtable-high-watermark-set! ht high)
      ht)))

(define make-eq-hashtable
  (case-lambda
    (()
     (sys:make-eq-hashtable INITIAL-SIZE))
    ((k)
     (assert (and (integer? k) (exact? k) (not (negative? k))))
     (update-watermarks!
      (make-hashtable (make-vector (ht-initial-size k) 0) 0 #f 'eq? 'eq-hash #t #t 0 0)))))

(define make-eqv-hashtable
  (case-lambda
    (()
     (sys:make-eqv-hashtable INITIAL-SIZE))
    ((k)
     (assert (and (integer? k) (exact? k) (not (negative? k))))
     (update-watermarks!
      (make-hashtable (make-vector (ht-initial-size k) 0) 0 #f 'eqv? 'eqv-hash #t #f 0 0)))))

(define make-general-hashtable
  (case-lambda
    ((hash-function equiv)
     (sys:make-hashtable hash-function equiv INITIAL-SIZE))
    ((hash-function equiv k)
     (assert (and (integer? k) (exact? k) (not (negative? k))))
     (let ((hash* (if (eq? hash-function equal-hash) 'equal-hash hash-function))
           (cmp* (if (eq? equiv equal?) 'equal? equiv)))
       (update-watermarks!
        (make-hashtable (make-vector (ht-initial-size k) 0) 0 #f cmp* hash* #t #f 0 0))))))

(define (hashtable-cmp ht)
  (let ((cmp (hashtable-cmp* ht)))
    (case cmp
      ((eq?) eq?)
      ((eqv?) eqv?)
      ((equal?) equal?)
      (else cmp))))

(define (hashtable-hash ht)
  (let ((cmp (hashtable-hash* ht)))
    (case cmp
      ((eq-hash) eq-hash)
      ((eqv-hash) eqv-hash)
      ((equal-hash) equal-hash)
      (else cmp))))

(define (hashtable-hash-function ht)
  (let ((cmp (hashtable-hash* ht)))
    (case cmp
      ((eq-hash) #f)
      ((eqv-hash) #f)
      ((equal-hash) equal-hash)
      (else cmp))))

;; Returns the cons cell for the value that matches the key. Uses 0
;; instead of '(), because in Loko vectors are initialized to
;; contain 0.
(define (alist-lookup key vals cmp mutable?)
  (let lp ((prev #f) (it vals))
    (cond ((eqv? it 0) #f)
          ((cmp (caar it) key)
           (let ((cell (car it)))
             (if (or (not prev) (not mutable?))
                 cell
                 (let ((p (car prev))
                       (c cell))
                   ;; Self-organizing list using the transpose method.
                   (let ((pa (car p)) (pd (cdr p))
                         (ca (car c)) (cd (cdr c)))
                     (set-car! p ca)
                     (set-cdr! p cd)
                     (set-car! c pa)
                     (set-cdr! c pd)
                     p)))))
          (else
           (lp it (cdr it))))))

;; This should return #t if a GC does not change the result of eq-hash
(define (immobile? x)
  ;; TODO: detect objects that do not live in the current heap
  (or (fixnum? x)
      (char? x)
      (boolean? x)
      (eof-object? x)
      (null? x)
      (symbol? x)
      ($void? x)))

(define (hashmod x len)
  (if (fixnum? x)
      (fxmod x len)
      (fxmod (bitwise-bit-field 0 32) len)))

;; This is invoked with the old vector and a new vector. It computes
;; new hashes for all the keys and inserts them in the new vector.
(define (rehash! ht iteration ov nv)
  (print ";;; rehashing")
  (let ((start-gc (collections))
        (hash (hashtable-hash ht)))
    (hashtable-vals-set! ht nv)
    (let loop-vector ((i 0) (all-immobile #t))
      (if (fx=? i (vector-length ov))
          (when (hashtable-gc-sensitive? ht)
            (cond ((not all-immobile)
                   (let ((current-gc (collections)))
                     (cond ((fx=? start-gc current-gc)
                            (hashtable-gc-count-set! ht current-gc))
                           ((fx>? iteration 42)
                            (error 'rehash!
                                   "Internal error: rehash triggers GC every time" ht))
                           (else
                            (print ";;; rehashing did not survive GC!!")
                            (vector-fill! nv 0)
                            (rehash! ht (fx+ iteration 1) ov nv)))))
                  (else
                   (hashtable-gc-count-set! ht #f))))
          (let loop-bucket ((obucket (vector-ref ov i))
                            (all-immobile all-immobile))
            (if (eqv? obucket 0)
                (loop-vector (fx+ i 1) all-immobile)
                (let* ((key (caar obucket))
                       (value (cdar obucket))
                       (idx (hashmod (hash key) (vector-length nv))))
                  (vector-set! nv idx (cons (cons key value)
                                            (vector-ref nv idx)))
                  (loop-bucket (cdr obucket)
                               (and all-immobile (immobile? key))))))))))

(define (hashtable-ref ht key default)
  (unless (hashtable? ht)
    (assertion-violation 'hashtable-ref "Expected a hashtable" ht key default))
  (let lp ()
    (print "hashtable-ref " ht " " key " " default)
    (let* ((vals (hashtable-vals ht))
           (gc (hashtable-gc-count ht))
           (hash (hashtable-hash ht))
           (len (vector-length vals))
           (idx (hashmod (hash key) len))
           (bucket (vector-ref vals idx)))
      ;;(print "#;IDX " key " => " idx)
      (cond ((and gc (not (fx=? gc (collections))))
             ;; The hashes have been changed by the GC.
             (print ";;; must rehash on ref due to GC")
             (rehash! ht 0 vals (make-vector len 0))
             (lp))
            (else
             (let ((maybe-cell (alist-lookup key bucket (hashtable-cmp ht)
                                             (hashtable-mutable? ht))))
               (cond ((pair? maybe-cell)
                      (cdr maybe-cell))
                     (else default))))))))

(define (hashtable-set! ht key value)
  (hashtable-update!* ht key #f value))

(define (hashtable-delete! ht key)
  (assert (hashtable-mutable? ht))
  (print "hashtable-delete! " ht " " key)
  (let lp ()
    (let* ((vals (hashtable-vals ht))
           (start-gc (collections))
           (gc (hashtable-gc-count ht))
           (hash (hashtable-hash ht))
           (len (vector-length vals))
           (idx (hashmod (hash key) len)))
      (cond ((and gc (not (eq? gc (collections))))
             ;; The hashes have been changed by the GC.
             (print ";;; Must rehash on delete! due to GC")
             (rehash! ht 0 vals (make-vector len 0))
             (lp))
            (else
             (let ((cmp (hashtable-cmp ht)))
               (let lp ((prev #f) (bucket (vector-ref vals idx)))
                 (cond ((eqv? bucket 0)
                        ;; The key was not found
                        (values))
                       ((cmp (caar bucket) key)
                        ;; Delete the key by emptying the bucket or
                        ;; unlinking the cell.
                        (if (not prev)
                            (vector-set! vals idx (cdr bucket))
                            (set-cdr! prev (cdr bucket)))
                        (hashtable-size-set! ht (fx- (hashtable-size ht) 1))
                        (when (and (not gc) (hashtable-gc-sensitive? ht)
                                   (not (immobile? key)))
                          (hashtable-gc-count-set! ht start-gc))
                        (when (fx<? len (hashtable-low-watermark ht))
                          (rehash! ht 0 vals (make-vector (fxmax (ht-initial-size INITIAL-SIZE)
                                                                 (fxdiv len 2))
                                                          0))
                          (update-watermarks! ht))
                        (values))
                       (else
                        (lp bucket (cdr bucket)))))))))))

(define not-found (cons #f #f))

(define (hashtable-contains? ht key)
  (if (eq? (hashtable-ref ht key not-found) not-found) #f #t))

(define (alist-update! ht key proc default alist)
  (let ((maybe-cell (alist-lookup key alist (hashtable-cmp ht)
                                  (hashtable-mutable? ht))))
    (cond ((pair? maybe-cell)
           (print ";; already in bucket")
           (set-cdr! maybe-cell (if proc (proc (cdr maybe-cell)) default))
           alist)
          (else
           (print ";; new element in bucket")
           (hashtable-size-set! ht (fx+ (hashtable-size ht) 1))
           (cons (cons key (if proc (proc default) default))
                 alist)))))

(define (hashtable-update!* ht key proc default)
  (assert (hashtable-mutable? ht))
  (print "hashtable-update! " key " " proc " " default)
  (let lp ()
    (let* ((vals (hashtable-vals ht))
           (start-gc (collections))
           (gc (hashtable-gc-count ht))
           (hash (hashtable-hash ht))
           (len (vector-length vals))
           (idx (hashmod (hash key) len))
           (bucket (vector-ref vals idx)))
      ;;(print "#;IDX " key " => " idx)
      (cond ((and gc (not (eq? gc (collections))))
             ;; The hashes have been changed by the GC.
             (print ";;; Must rehash on set! due to GC")
             (rehash! ht 0 vals (make-vector len 0))
             (lp))
            (else
             (let ((bucket (alist-update! ht key proc default bucket)))
               (vector-set! vals idx bucket)
               (when (and (not gc) (hashtable-gc-sensitive? ht)
                          (not (immobile? key)))
                 (hashtable-gc-count-set! ht start-gc))
               (when (fx>? (hashtable-size ht) (hashtable-high-watermark ht))
                 (rehash! ht 0 vals (make-vector (fx* len 2) 0))
                 (update-watermarks! ht))
               (print "hashtable-update done.")))))))

(define (hashtable-update! ht key proc default)
  (assert (procedure? proc))
  (hashtable-update!* ht key proc default))

(define (hashtable-load-factor ht)
  (let ((vals (hashtable-vals ht)))
    (fl/ (fixnum->flonum (hashtable-size ht))
         (fixnum->flonum (vector-length vals)))))

(define (print-ht x)
  (print ";; GC count at last rehash: " (hashtable-gc-count x))
  (print ";; Current GC count: " (collections))
  (print ";; Hashtable load factor: " (hashtable-load-factor x))
  (do ((vals (hashtable-vals x))
       (i 0 (fx+ i 1)))
      ((fx=? i (vector-length vals)))
    (print "#;BUCKET " i " : " (vector-ref vals i))))

(define hashtable-copy
  (case-lambda
    ((ht)
     (unless (hashtable? ht)
       (assertion-violation 'hashtable-copy
                            "Expected a hashtable"
                            ht))
     (hashtable-copy ht #f))
    ((ht mutable?)
     (unless (hashtable? ht)
       (assertion-violation 'hashtable-copy
                            "Expected a hashtable"
                            ht mutable?))
     (let* ((old-vals (hashtable-vals ht))
            (new-vals (make-vector (vector-length old-vals) 0))
            (new-ht (make-hashtable new-vals
                                    (hashtable-size ht)
                                    (hashtable-gc-count ht)
                                    (hashtable-cmp* ht)
                                    (hashtable-hash* ht)
                                    (not (not mutable?))
                                    (hashtable-gc-sensitive? ht)
                                    (hashtable-low-watermark ht)
                                    (hashtable-high-watermark ht))))
       ;; Copy the buckets as they are.
       (do ((i 0 (fx+ i 1)))
           ((fx=? i (vector-length old-vals))
            new-ht)
         (do ((b (vector-ref old-vals i) (cdr b)))
             ((not (pair? b)))
           (let* ((k (caar b))
                  (v (cdar b)))
             (vector-set! new-vals i (cons (cons k v)
                                           (vector-ref new-vals i))))))))))

(define hashtable-clear!
  (case-lambda
    ((ht)
     (sys:hashtable-clear! ht INITIAL-SIZE))
    ((ht k)
     (assert (hashtable-mutable? ht))
     (assert (and (integer? k) (exact? k) (not (negative? k))))
     (hashtable-vals-set! ht (make-vector (ht-initial-size k) 0))
     (hashtable-size-set! ht 0)
     (hashtable-gc-count-set! ht #f))))

(define (hashtable-keys ht)
  (let ((size (hashtable-size ht))
        (vals (hashtable-vals ht)))
    (let ((k* (make-vector size)))
      (let ((b* vals))
        (let loop-vector ((j 0) (i 0))
          (unless (fx=? i (vector-length b*))
            (let loop-bucket ((j j) (b (vector-ref b* i)))
              (if (pair? b)
                  (let ((k (caar b)))
                    (vector-set! k* j k)
                    (loop-bucket (fx+ j 1) (cdr b)))
                  (loop-vector j (fx+ i 1)))))))
      k*)))

(define (hashtable-entries ht)
  (let ((size (hashtable-size ht))
        (vals (hashtable-vals ht)))
    (let ((k* (make-vector size))
          (v* (make-vector size)))
      (let ((b* vals))
        (let loop-vector ((j 0) (i 0))
          (unless (fx=? i (vector-length b*))
            (let loop-bucket ((j j) (b (vector-ref b* i)))
              (if (pair? b)
                  (let* ((k (caar b))
                         (v (cdar b)))
                    (vector-set! k* j k)
                    (vector-set! v* j v)
                    (loop-bucket (fx+ j 1) (cdr b)))
                  (loop-vector j (fx+ i 1)))))))
      (values k* v*))))

;;; Hash functions

(define (string-hash s)
  (string-hash* s))

(define (string-ci-hash s)
  (string-hash (string-foldcase s)))

;;; Hashtable writer

;; Notation borrowed from Racket
(record-writer (record-type-descriptor hashtable)
               (lambda (v p wr)
                 (let* ((cmp (hashtable-cmp* v))
                        (hash (hashtable-hash* v))
                        (prefix
                         (cond
                           ((eq? cmp 'equal?) (and (eq? hash 'equal-hash) "#hash("))
                           ((eq? cmp 'eqv?) (and (eq? hash 'eqv-hash) "#hasheqv("))
                           ((eq? cmp 'eq?) (and (eq? hash 'eq-hash) "#hasheq("))
                           (else #f))))
                   (display (or prefix "#[hashtable ") p)
                   (let-values ([(keys values) (hashtable-entries v)])
                     (do ((i 0 (fx+ i 1)))
                         ((fx=? i (vector-length keys)))
                       (display "(" p)
                       (wr (vector-ref keys i) p)
                       (display " . " p)
                       (wr (vector-ref values i) p)
                       (display #\) p)
                       (unless (fx=? (fx+ i 1) (vector-length keys))
                         (display " " p))))
                   (display (if prefix #\) #\]) p)))))
