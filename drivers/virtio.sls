;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2020, 2021 G. Weinholt
#!r6rs

;;; Virtio transport help library

(library (loko drivers virtio)
  (export
    virtio-allocate-virtq
    virtq? virtq-free!

    virtq-num-avail
    virtq-num-free
    virtq-add-buffers!
    virtq-get-buffer
    virtq-enable-interrupts!
    virtq-disable-interrupts!
    virtq-kick

    ;; FIXME: The API is not sufficiently abstracted here
    virtio-device-status-set!
    virtio-isr-status!
    virtio-negotiate
    virtio-get-config-u8
    virtio-get-config-u16

    ;; virtio-device-status bitwise-ior'd flags
    VIRTIO_CONFIG_S_ACKNOWLEDGE
    VIRTIO_CONFIG_S_DRIVER
    VIRTIO_CONFIG_S_DRIVER_OK
    VIRTIO_CONFIG_S_FEATURES_OK
    VIRTIO_CONFIG_S_DEVICE_NEEDS_RESET
    VIRTIO_CONFIG_S_FAILED)
  (import
    (rnrs (6))
    (loko system fibers)
    (loko system logging)
    (loko system unsafe)
    (loko system unsafe cache)

    (only (loko system $host) dma-allocate dma-free))

(define put-mem-s61le put-mem-s61)
(define put-mem-u32le put-mem-u32)
(define put-mem-u16le put-mem-u16)
(define get-mem-s61le get-mem-s61)
(define get-mem-u32le get-mem-u32)
(define get-mem-u16le get-mem-u16)

;; Device status
(define VIRTIO_CONFIG_S_ACKNOWLEDGE        1)
(define VIRTIO_CONFIG_S_DRIVER             2)
(define VIRTIO_CONFIG_S_DRIVER_OK          4)
(define VIRTIO_CONFIG_S_FEATURES_OK        8)
(define VIRTIO_CONFIG_S_DEVICE_NEEDS_RESET 64)
(define VIRTIO_CONFIG_S_FAILED             128)

(define VIRTIO_MSI_NO_VECTOR #xffff)

;;; Virtqueues

;; Virtqueues have a descriptor table, a ring of available descriptors
;; and a ring of used descriptors. The available descriptors are going
;; to the device and the used ones have been used by the device. The
;; driver writes to the available ring and the device writes to the
;; used ring.

(define sizeof-virtq_desc (+ 8 4 2 2))
(define offsetof-virtq_desc-addr   0)   ;u64 buffer address
(define offsetof-virtq_desc-len    8)   ;u32 buffer length
(define offsetof-virtq_desc-flags  12)
(define offsetof-virtq_desc-next   14)  ;next if set in flags

(define offsetof-virtq_avail-flags 0)
(define offsetof-virtq_avail-idx   2)
(define offsetof-virtq_avail-ring  4)   ;variable sized le16 array

(define offsetof-virtq_used-flags    0)
(define offsetof-virtq_used-idx      2)
(define offsetof-virtq_used-ring     4) ;variable sized sizeof-virtq_used_elem array

(define sizeof-virtq_used_elem (+ 4 4))
(define offsetof-virtq_used_elem-id  0)
(define offsetof-virtq_used_elem-len 4)

(define-record-type virtq
  (sealed #t)
  (fields idx                           ;index of the virtq in the device
          size
          (mutable num-avail)     ;descriptors available to the device
          (mutable next-free-desc-idx)
          (mutable last-seen-used)
          kick-proc
          desc-cookies
          &desc &avail &used)
  (protocol
   (lambda (p)
     (lambda (idx queue-size kick-proc)
       (define (fxalign i alignment)
         (fxand (fx+ i (fx- alignment 1)) (fx- alignment)))
       (define (virtq-used-offset queue-size)
         (fxalign (fx+ (fx* sizeof-virtq_desc queue-size)
                       (fx* 2 (fx+ 3 queue-size)))
                  4096))
       (define (virtq-byte-size queue-size)
         (fx+ (virtq-used-offset queue-size)
              (fxalign (fx+ 6 (fx* sizeof-virtq_used_elem queue-size))
                       4096)))
       (let* ((byte-size (virtq-byte-size queue-size))
              (desc-cookies (make-vector queue-size #f))
              (&desc (dma-allocate byte-size #xFFFFFFFF000))
              (&avail (fx+ &desc (fx* sizeof-virtq_desc queue-size)))
              (&used (fx+ &desc (virtq-used-offset queue-size)))
              (num-avail 0)
              (next-free-desc-idx #f)
              (last-seen-used 0))
         (let ((virtq (p idx queue-size num-avail
                         next-free-desc-idx last-seen-used
                         kick-proc desc-cookies
                         &desc &avail &used)))
           (do ((i (fx- queue-size 1) (fx- i 1)))
               ((fx=? i -1))
             (virtq-release-descriptor! virtq i))
           virtq))))))

(define (virtq-enable-interrupts! virtq)
  (virtq-&avail-flags-set! (virtq-&avail virtq) 0))

(define (virtq-disable-interrupts! virtq)
  (virtq-&avail-flags-set! (virtq-&avail virtq) VIRTQ_AVAIL_F_NO_INTERRUPT))

(define (virtq-kick virtq)
  ((virtq-kick-proc virtq)))

(define (virtq-num-free virtq)
  (fx- (virtq-size virtq) (virtq-num-avail virtq)))

(define (virtq-free! virtq)
  ;; FIXME: Free the buffers!
  (dma-free (virtq-&desc virtq)))

(define (virtq-allocate-descriptor! virtq)
  ;; Each unused descriptor's next field links to the next unused
  ;; descriptor. The last element links to itself.
  (let ((idx (virtq-next-free-desc-idx virtq)))
    (let-values ([(_flags next) (virtq-desc-flags+next virtq idx)])
      (if (eqv? next idx)
          (virtq-next-free-desc-idx-set! virtq #f) ;last desc allocated
          (virtq-next-free-desc-idx-set! virtq next))
      idx)))

(define (virtq-release-descriptor! virtq desc-idx)
  (cond ((virtq-next-free-desc-idx virtq) =>
         (lambda (next-free-idx)
           (virtq-desc-set! virtq desc-idx 0 0 0 next-free-idx)))
        (else
         ;; There were no descriptors
         (virtq-desc-set! virtq desc-idx 0 0 0 desc-idx)))
  (virtq-next-free-desc-idx-set! virtq desc-idx))

(define VIRTQ_DESC_F_NEXT     1)        ;the next field is valid
(define VIRTQ_DESC_F_WRITE    2)        ;write-only
(define VIRTQ_DESC_F_INDIRECT 4)        ;buffer contains descriptors

(define (virtq-desc-set! virtq desc-idx addr len flags next)
  (assert (fx<? desc-idx (virtq-size virtq)))
  (let ((&desc-base (fx+ (virtq-&desc virtq) (fx* desc-idx sizeof-virtq_desc))))
    (put-mem-s61le (fx+ &desc-base offsetof-virtq_desc-addr) addr)
    (put-mem-u32le (fx+ &desc-base offsetof-virtq_desc-len) len)
    (put-mem-u16le (fx+ &desc-base offsetof-virtq_desc-flags) flags)
    (put-mem-u16le (fx+ &desc-base offsetof-virtq_desc-next) next)))

(define (virtq-desc-flags+next virtq desc-idx)
  (let ((&desc-base (fx+ (virtq-&desc virtq) (fx* desc-idx sizeof-virtq_desc))))
    (values
      (get-mem-u16le (fx+ &desc-base offsetof-virtq_desc-flags))
      (get-mem-u16le (fx+ &desc-base offsetof-virtq_desc-next)))))

(define VIRTQ_AVAIL_F_NO_INTERRUPT 1)

(define (virtq-&avail-flags-set! &avail flags)
  (put-mem-u16le &avail flags))

(define (virtq-avail-idx-set! virtq idx)
  (put-mem-u16le (fx+ (virtq-&avail virtq) offsetof-virtq_avail-idx)
                 (fxand idx #xffff)))

(define (virtq-avail-idx virtq)
  (get-mem-u16le (fx+ (virtq-&avail virtq) offsetof-virtq_avail-idx)))

(define (virtq-avail-ring-set! virtq ring-idx desc-idx)
  (put-mem-u16le (fx+ (virtq-&avail virtq) (fx+ offsetof-virtq_avail-ring (fx* ring-idx 2)))
                 desc-idx))

(define VIRTQ_USED_F_NO_NOTIFY 1)

(define (virtq-&used-flags &used)
  (get-mem-u16le &used))

(define (virtq-&used-idx &used)
  (get-mem-u16le (fx+ &used offsetof-virtq_used-idx)))

(define (virtq-&used_elem-id &used idx)
  (get-mem-u16le (fx+ &used
                      (fx+ (fx* idx sizeof-virtq_used_elem)
                           (fx+ offsetof-virtq_used-ring
                                offsetof-virtq_used_elem-id)))))

(define (virtq-&used_elem-len &used idx)
  (get-mem-u16le (fx+ &used
                      (fx+ (fx* idx sizeof-virtq_used_elem)
                           (fx+ offsetof-virtq_used_elem-len
                                offsetof-virtq_used-ring)))))

;; Adds a number of readable buffers followed by a number of writable
;; buffers. All of them are associated with a cookie that is returned
;; when the buffer has been used. They are a single chain of buffers.
;; The buf* argument is a list of (physical-address . length) pairs.
(define (virtq-add-buffers! virtq buf* num-in cookie)
  (assert cookie)
  (assert (not (null? buf*)))
  (let ((len (length buf*)))
    (cond
      ((fx<? (virtq-num-free virtq) len)
       (send-log WARNING "Not enough free descriptors")
       #f)
      (else
       (let ((desc* (map (lambda _ (virtq-allocate-descriptor! virtq)) buf*)))
         ;; Fill in and link the descriptors
         (let lp ((buf* buf*) (desc* desc*) (num-in num-in))
           (unless (null? buf*)
             (let ((buf (car buf*))
                   (desc (car desc*))
                   (read? (not (eqv? 0 num-in))))
               (let ((addr (car buf))
                     (len (cdr buf))
                     (flags (fxior (if (null? (cdr desc*)) 0 VIRTQ_DESC_F_NEXT)
                                   (if read? 0 VIRTQ_DESC_F_WRITE)))
                     (next (if (null? (cdr desc*)) 0 (cadr desc*))))
                 (virtq-desc-set! virtq desc addr len flags next)
                 (if read?
                     (lp (cdr buf*) (cdr desc*) (fx- num-in 1))
                     (lp (cdr buf*) (cdr desc*) num-in))))))
         (virtq-num-avail-set! virtq (fx+ (virtq-num-avail virtq) len))

         ;; Tell the device about the new buffers. The driver may kick
         ;; the device if it wants to.
         (let* ((idx (virtq-avail-idx virtq))
                (idx-mod (fxmod idx (virtq-size virtq))))
           #;
           (send-log DEBUG (call-with-string-output-port
                             (lambda (p)
                               (display "adding " p)
                               (write desc* p)
                               (display " @ idx " p)
                               (write (cons idx idx-mod) p)
                               (display " with " p)
                               (display (virtq-num-free virtq) p)
                               (display " free" p))))
           (vector-set! (virtq-desc-cookies virtq) idx-mod cookie)
           (virtq-avail-ring-set! virtq idx-mod (car desc*))
           (store-fence)
           (virtq-avail-idx-set! virtq (fx+ idx 1))))))))

;; Get a used buffer chain from the virtqueue.
(define (virtq-get-buffer virtq)
  (define &used (virtq-&used virtq))
  ;; TODO: Be smart about disabling interrupts and handling multiple buffers
  (cond
    ((fx=? (virtq-last-seen-used virtq) (virtq-&used-idx &used))
     (values #f #f))
    (else
     (let* ((idx (virtq-last-seen-used virtq))
            (idx-mod (fxmod idx (virtq-size virtq))))
       (let ((head-desc-idx (virtq-&used_elem-id &used idx-mod))
             (written-bytes (virtq-&used_elem-len &used idx-mod)))
         (virtq-last-seen-used-set! virtq (fxand (fx+ idx 1) #xffff))
         (let ((cookie (vector-ref (virtq-desc-cookies virtq) idx-mod)))
           (vector-set! (virtq-desc-cookies virtq) idx-mod #f)
           #;
           (send-log DEBUG (call-with-string-output-port
                             (lambda (p)
                               (display "used head " p)
                               (write head-desc-idx p)
                               (display " cookie " p)
                               (write cookie p))))
           (let lp ((desc-idx head-desc-idx)
                    (chain-len 1))
             (let-values ([(flags next-desc-idx) (virtq-desc-flags+next virtq desc-idx)])
               (virtq-release-descriptor! virtq desc-idx)
               (if (not (eqv? 0 (fxand flags VIRTQ_DESC_F_NEXT)))
                   (lp next-desc-idx (fx+ chain-len 1))
                   (virtq-num-avail-set! virtq (fx- (virtq-num-avail virtq) chain-len)))))

           (values written-bytes cookie)))))))

;;; Virtio device

;; Generic feature flags
(define VIRTIO_F_RING_INDIRECT_DESC 28)
(define VIRTIO_F_RING_EVENT_IDX     29)
(define VIRTIO_F_VERSION_1          32)
(define VIRTIO_F_ACCESS_PLATFORM    33)
(define VIRTIO_F_RING_PACKED        34)
(define VIRTIO_F_IN_ORDER           35)
(define VIRTIO_F_ORDER_PLATFORM     36)
(define VIRTIO_F_SR_IOV             37)
(define VIRTIO_F_NOTIFICATION_DATA  38)

;; Offsets into the configuration registers
(define VIRTIO_PCI_HOST_FEATURES    0)  ;u32
(define VIRTIO_PCI_GUEST_FEATURES   4)  ;u32
(define VIRTIO_PCI_QUEUE_PFN        8)  ;u32
(define VIRTIO_PCI_QUEUE_NUM        12) ;u16, queue size
(define VIRTIO_PCI_QUEUE_SEL        14) ;u16
(define VIRTIO_PCI_QUEUE_NOTIFY     16) ;u16
(define VIRTIO_PCI_STATUS           18) ;u8
(define VIRTIO_PCI_ISR              19) ;u8
(define offsetof-device-config #x14)    ;XXX: changed by MSI-X

(define (virtio-device-status-set! reg-ctl status)
  (put-i/o-u8 (fx+ reg-ctl VIRTIO_PCI_STATUS) status))

(define (virtio-device-status reg-ctl)
  (get-i/o-u8 (fx+ reg-ctl VIRTIO_PCI_STATUS)))

(define (virtio-isr-status! reg-ctl)
  (get-i/o-u8 (fx+ reg-ctl VIRTIO_PCI_ISR)))

(define (virtio-get-config-u8 reg-ctl idx)
  (get-i/o-u8 (fx+ (fx+ offsetof-device-config reg-ctl) idx)))

(define (virtio-get-config-u16 reg-ctl idx)
  (get-i/o-u16 (fx+ (fx+ offsetof-device-config reg-ctl) idx)))

;; Allocates memory for and initiates the virtq. If the queue does not
;; exist on the device then it returns #f.
(define (virtio-allocate-virtq reg-ctl queue-idx)
  (put-i/o-u16 (fx+ reg-ctl VIRTIO_PCI_QUEUE_SEL) queue-idx)
  (let ((queue-size (get-i/o-u16 (fx+ reg-ctl VIRTIO_PCI_QUEUE_NUM))))
    (if (not (eqv? (fxbit-count queue-size) 1))
        #f
        (letrec ((virtq (make-virtq queue-idx queue-size
                                    (lambda ()
                                      (when (eqv? 0 (fxand (virtq-&used-flags (virtq-&used virtq))
                                                           VIRTQ_USED_F_NO_NOTIFY))
                                        (put-i/o-u16 (fx+ reg-ctl VIRTIO_PCI_QUEUE_NOTIFY)
                                                     queue-idx))))))
          (put-i/o-u32 (fx+ reg-ctl VIRTIO_PCI_QUEUE_PFN)
                       (fxdiv (virtq-&desc virtq) 4096))
          virtq))))

(define (virtio-negotiate reg-ctl guest-features)
  (let* ((device-features (get-i/o-u32 (fx+ reg-ctl VIRTIO_PCI_HOST_FEATURES)))
         (common-features (fxand device-features guest-features)))
    (put-i/o-u32 (fx+ reg-ctl VIRTIO_PCI_GUEST_FEATURES) common-features)
    common-features)))
