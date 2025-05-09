;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; SPDX-License-Identifier: MIT
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(library (psyntax compat)
  (export make-parameter parameterize define-record pretty-print
          gensym void eval-core symbol-value set-symbol-value!
          file-options-spec read-library-source-file case-sensitive
          annotation? annotation-expression annotation-stripped
          read-annotated annotation-source annotation-source->condition
          library-version-mismatch-warning
          library-stale-warning
          file-locator-resolution-error
          label-binding set-label-binding! remove-location
          features
          top-level-language)
  (import
    (only (loko runtime reader)
          read-annotated annotation? annotation-expression
          annotation-stripped annotation-source
          annotation-source->condition)
    (loko runtime utils)
    (only (loko) make-parameter parameterize record-writer case-sensitive)
    (only (loko system r7rs) features)
    (rnrs)
    (only (psyntax system $bootstrap)
          gensym void eval-core symbol-value set-symbol-value!
          pretty-print))

  (define top-level-language
    (make-parameter 'r6rs))

  (define *LABELS* (make-eq-hashtable))

  (define (label-binding x)
    (hashtable-ref *LABELS* x #f))

  (define (set-label-binding! x v)
    (hashtable-set! *LABELS* x v))

  (define (remove-location x)
    (hashtable-delete! *LABELS* x))

  (define (read-library-source-file file-name)
    (with-input-from-file file-name
      (lambda ()
        ;; Strips the prefix for (reproducible builds)
        (read-annotated (current-input-port) (strip-akku-prefix file-name)))))

  (define (library-version-mismatch-warning name depname filename)
    ;;; please override this in your production implementation
    (display "Warning: inconsistent dependencies: ")
    (display name)
    (display depname)
    (display filename))

  (define (library-stale-warning name filename)
    (define p (current-error-port))
    (display "WARNING: library " p)
    (display name p)
    (display " is stale; file " p)
    (display filename p)
    (display "will be recompiled from source.\n" p))


  (define (file-locator-resolution-error libname failed-list y)
    ;;; please override this in your production implementation
    (error 'file-locator "cannot find library" libname failed-list y))

  ;; borrowed from mosh
  (define-syntax define-record
    (lambda (x)
      (define (syn->str s)
        (symbol->string
         (syntax->datum s)))
      (define (gen-getter id)
        (lambda (fld)
          (datum->syntax id
                         (string->symbol
                          (string-append (syn->str id) "-" (syn->str fld))))))
      (define (gen-setter id)
        (lambda (fld)
          (datum->syntax id
                         (string->symbol
                          (string-append "set-" (syn->str id) "-" (syn->str fld) "!")))))
      (syntax-case x ()
        [(_ name (field* ...) printer)
         #`(begin
             (define-record name (field* ...))
             (define rp (record-writer (record-type-descriptor name) printer)))]
        [(_ name (field* ...))
         (with-syntax ([(getter* ...)
                        (map (gen-getter #'name) #'(field* ...))]
                       [(setter* ...)
                        (map (gen-setter #'name) #'(field* ...))])
           #`(define-record-type name
               (sealed #t) ; for better performance
               (opaque #f)
               (nongenerative #,(datum->syntax #'name (string->symbol (string-append "X" (symbol->string (syntax->datum #'name)) ))))
               #;(nongenerative) ; for sanity
               (fields (mutable field* getter* setter*) ...)))])))

  (define (file-options-spec x)
    (error 'file-options-spec "not implemented"))

)
