;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2022 G. Weinholt
#!r6rs

;;; Storage device tools, caching, stuff like this

;; This provides abstractions on top of raw storage. Any interaction
;; with drivers from here passes through (loko drivers storage).

(library (loko kernel storage)
  (export
    open-storage-device            ;FIXME: use logical-storage-device?

    make-block-cache

    make-logical-storage
    logical-storage-read
    logical-storage-write

    )
  (import
    (rnrs (6))
    (loko dlists)
    (loko match)
    (loko system fibers)
    (loko drivers storage)
    )

;; This is a cache of blocks on a storage device. The current
;; implementation is meant to be used in a write-through manner, which
;; is probably less than ideal. The main lookup method is through the
;; hashtable, and the doubly-linked list is for cache expiry.
(define-record-type block-cache
  (sealed #t)
  (fields ht                            ;sector index -> blockentry
          dlist                         ;dlist of blockentry records
          memory-limit
          (mutable memory-usage)
          logical-block-size)
  (protocol
   (lambda (p)
     (lambda (memory-limit logical-block-size)
       (assert (fxpositive? memory-limit))
       (let ((memory-usage 0))
         (p (make-eqv-hashtable)
            (make-dlist)
            memory-limit
            memory-usage
            logical-block-size))))))

(define-record-type block-entry
  (sealed #t)
  (fields (mutable dnode)               ;dnode in block-cache-dlist
          lba
          ;; XXX: this uses bytevectors under the assumption that they
          ;; will be faster wrt to GC in future versions
          bv))

;; Copy the data from source, which si for block number lba, into the
;; cache. May evict some other block from the cache.
(define (cache-enter-data! cache lba source source-offset)
  (let ((ht (block-cache-ht cache))
        (dlist (block-cache-dlist cache)))
    (let ((entry
           (cond ((hashtable-ref ht lba #f) =>
                  (lambda (entry)
                    (dlist-remove! dlist (block-entry-dnode entry))
                    entry))
                 (else
                  (let* ((bv (make-bytevector (block-cache-logical-block-size cache)))
                         (entry (make-block-entry #f lba bv))
                         (usage (+ (block-cache-memory-usage cache)
                                   (bytevector-length bv))))
                    (block-cache-memory-usage-set! cache usage)
                    (when (fx>? usage (block-cache-memory-limit cache))
                      (let* ((rem (dlist-head dlist))
                             (entry^ (dnode-data rem))
                             (usage^ (fx- usage (bytevector-length (block-entry-bv entry^)))))
                        (dlist-remove! dlist rem)
                        (hashtable-delete! ht (block-entry-lba entry^))
                        (block-cache-memory-usage-set! cache usage^)))
                    entry)))))
      (let ((dnode (dlist-append! dlist entry)))
        (block-entry-dnode-set! entry dnode)
        entry)
      (hashtable-set! ht lba entry)
      (bytevector-copy! source source-offset
                        (block-entry-bv entry) 0
                        (block-cache-logical-block-size cache)))))

;; If lba is in the cache then copy it into the target bytevector and
;; return #t. Otherwise return #f.
(define (cache-copy-data! cache lba target target-offset)
  (cond ((hashtable-ref (block-cache-ht cache) lba #f) =>
         (lambda (entry)
           ;; Move to the end of the dlist
           (let ((dlist (block-cache-dlist cache)))
             (dlist-remove! dlist (block-entry-dnode entry))
             (let ((dnode (dlist-append! dlist entry)))
               (block-entry-dnode-set! entry dnode)
               entry))
           ;; Get a copy of the block
           (bytevector-copy! (block-entry-bv entry) 0
                             target target-offset
                             (block-cache-logical-block-size cache))
           #t))
        (else #f)))

(define (cache-contains-blocks? cache lba count)
  (assert (fx>=? count 0))
  (let lp ((lba lba) (count count))
    (cond ((eqv? count 0)
           #t)
          ((hashtable-contains? (block-cache-ht cache) lba)
           (lp (fx+ lba 1) (fx- count 1)))
          (else
           #f))))

(define (cache-copy-blocks cache lba count)
  (define block-size (block-cache-logical-block-size cache))
  (and (cache-contains-blocks? cache lba count)
       (do ((bv (make-bytevector (fx* count block-size)))
            (lba lba (fx+ lba 1))
            (offset 0 (fx+ offset block-size)))
           ((fx=? offset (bytevector-length bv))
            bv)
         (cache-copy-data! cache lba bv offset))))

;; This is for accessing part of a storage-device. Storage devices are
;; split into e.g. partitions, each partition making up a
;; logical-storage. As implemented it is just a linear part of a
;; storage device. Non-linear mappings (like in LVM) and other stuff
;; can be added later.
(define-record-type logical-storage
  (sealed #t)
  (fields name
          dev
          lba-first
          lba-last
          cache)
  (protocol
   (lambda (p)
     (lambda (name dev lba-first lba-last cache)
       (assert (storage-device? dev))
       (assert (>= lba-last lba-first))
       (p name dev lba-first lba-last cache)))))

;; Reads from a logical storage device
(define (logical-storage-read dev lba sectors)
  (define who 'logical-storage-read)
  (define storage-device (logical-storage-dev dev))
  (define sector-size (storage-device-logical-sector-size storage-device))
  (assert (and (fx>=? lba 0) (fx<=? 1 sectors 1024)))
  (let* ((read-lba (+ lba (logical-storage-lba-first dev)))
         (read-end-lba (+ read-lba (fx- sectors 1))))
    (cond
      ((cache-copy-blocks (logical-storage-cache dev) read-lba sectors))
      (else
       (unless (<= read-end-lba (logical-storage-lba-last dev))
         (assertion-violation who "Out of range" dev lba sectors))
       (let ((resp-ch (make-channel)))
         (put-message (storage-device-request-channel storage-device)
                      (list resp-ch 'read read-lba sectors))
         (match (get-message resp-ch)
           [('ok data)
            (do ((lba read-lba (fx+ lba 1))
                 (offset 0 (fx+ offset sector-size)))
                ((fx>? lba read-end-lba))
              (cache-enter-data! (logical-storage-cache dev)
                                 lba data offset))
            data]
           [('error . x) (error who "Read error" dev x)]))))))

(define (logical-storage-write dev lba sector-data)
  (define who 'logical-storage-write)
  (define storage-device (logical-storage-dev dev))
  (define sector-size (storage-device-logical-sector-size storage-device))
  (let-values ([(sectors slack)
                (fxdiv-and-mod (bytevector-length sector-data) sector-size)])
    (unless (eqv? slack 0)
      (assertion-violation who "Data must be a multiple of the sector size"
                           dev lba (bytevector-length sector-data)
                           sector-size))
    (let* ((write-lba (+ lba (logical-storage-lba-first dev)))
           (write-end-lba (+ write-lba (fx- sectors 1))))

      (unless (<= write-end-lba (logical-storage-lba-last dev))
        (assertion-violation who "Out of range storage device write"
                             dev lba (bytevector-length sector-data)))
      (let ((resp-ch (make-channel)))
        (put-message (storage-device-request-channel storage-device)
                     (list resp-ch 'write write-lba sector-data))
        (match (get-message resp-ch)
          [('ok)
           ;; XXX: This assumes that there are no conflicting writes.
           (do ((lba write-lba (fx+ lba 1))
                (offset 0 (fx+ offset sector-size)))
               ((fx>? lba write-end-lba))
             (cache-enter-data! (logical-storage-cache dev)
                                lba sector-data offset))
           (if #f #f)]
          [('error . x)
           (error 'logical-storage-write "Write error" dev x)])))))

;; Open a port that reads from a linear part of a storage device.
;; Ending LBA can be #f. All reads must be in multiples of the logical
;; sector size (typically 512).
(define (open-storage-device storage-device starting-lba ending-lba)
  (define lba 0)
  (define logical-sector-size
    (storage-device-logical-sector-size storage-device))
  (define id (storage-device-name storage-device))
  (define (read! bytevector start count)
    (let-values ([(blocks error) (fxdiv-and-mod count logical-sector-size)])
      (unless (eqv? error 0)
        (assertion-violation 'read!
                             "Expected a multiple of the block length" count))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i blocks))
        (let ((resp-ch (make-channel)))
          (let ((read-lba (+ starting-lba lba i)))
            (unless (and (<= starting-lba read-lba)
                         (or (not ending-lba) (<= read-lba ending-lba)))
              (assertion-violation 'read! "Out of range storage device read"
                                   storage-device read-lba))
            (put-message (storage-device-request-channel storage-device)
                         (list resp-ch 'read read-lba 1)))
          (match (get-message resp-ch)
            [('ok data)
             (unless (eqv? (bytevector-length data) logical-sector-size)
               (error 'read! "Bad data length from storage device"
                      (bytevector-length data)))
             (bytevector-copy! data 0 bytevector
                               (fx+ start (fx* i logical-sector-size))
                               (bytevector-length data))]
            [('error . _)
             (error 'read! "Error from storage device")])))
      (set! lba (fx+ lba blocks))
      (fx* blocks logical-sector-size)))
  (define (get-position)
    (fx* lba logical-sector-size))
  (define (set-position! pos)
    (let-values ([(lba^ error) (fxdiv-and-mod pos logical-sector-size)])
      (unless (eqv? error 0)
        (assertion-violation 'set-position!
                             "Expected a multiple of the logical sector length"
                             pos))
      (set! lba lba^)))
  (define (close)
    #f)
  (make-custom-binary-input-port id read! get-position set-position! close)))
