;; Copyright (c) 2020 G. Weinholt
;; SPDX-License-Identifier: MIT

;;; Test Unicode normalization

(import
  (rnrs)
  (unicode-data)
  (loko match)
  (loko runtime utils))

(define (codepoints->string x)
  (list->string
   (map (lambda (str)
          (integer->char (string->number str 16)))
        (string-split x #\space))))

(define part1 (make-bytevector #x110000 0))

(define toNFC string-normalize-nfc)
(define toNFD string-normalize-nfd)
(define toNFKC string-normalize-nfkc)
(define toNFKD string-normalize-nfkd)

(define part #f)

(define exit-status 0)

(for-each
 (lambda (test)
   (match test
     [(source NFC NFD NFKC NFKD . _)
      (match (map codepoints->string (list source NFC NFD NFKC NFKD))
        [(c1 c2 c3 c4 c5)
         ;; Test the invariants noted in CONFORMANCE point 1
         (let ((t1 (string=? c2 (toNFC c1) (toNFC c2) (toNFC c3)))
               (t2 (string=? c4 (toNFC c4) (toNFC c5)))
               (t3 (string=? c3 (toNFD c1) (toNFD c2) (toNFD c3)))
               (t4 (string=? c5 (toNFD c4) (toNFD c5)))
               (t5 (string=? c4 (toNFKC c1) (toNFKC c2) (toNFKC c3) (toNFKC c4) (toNFKC c5)))
               (t6 (string=? c5 (toNFKD c1) (toNFKD c2) (toNFKD c3) (toNFKD c4) (toNFKD c5))))
           (unless (and t1 t2 t3 t4 t5 t6)
             (set! exit-status 1)
             (display "Fail(1): ")
             (write (list t1 t2 t3 t4 t5 t6 source NFC NFD NFKC NFKD))
             (newline)))
         ;; Note which characters appear in part 1
         (when (equal? part "@Part1")
           (bytevector-u8-set! part1 (char->integer (string-ref c1 0)) 1))])]
     [(part^ . x*)
      (write (cons part^ x*))
      (newline)
      (set! part part^)]))
 (get-unicode-data "unicode/UNIDATA/NormalizationTest.txt"))

(do ((i 0 (fx+ i 1)))
    ((fx=? i (bytevector-length part1)))
  ;; Tested the invariants in CONFORMANCE point 2
  (unless (fx<=? #xD800 i #xDFFF)
    (when (= (bytevector-u8-ref part1 i) 0)
      (let ((x (string (integer->char i))))
        (unless (string=? x (toNFC x) (toNFD x) (toNFKC x) (toNFKD x))
          (set! exit-status 1)
          (display "Fail(2): ")
          (write (list x (toNFC x) (toNFD x) (toNFKC x) (toNFKD x)))
          (newline))))))

(exit exit-status)
