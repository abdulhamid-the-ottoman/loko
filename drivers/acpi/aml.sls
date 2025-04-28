;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: EUPL-1.2+
;; Copyright © 2007, 2022 G. Weinholt
#!r6rs

;;; ACPI Control Method Machine Language (AML) interpreter

;; TODO: if acpi-table-header-revision is >=2 then integers are 64-bit.

;; AML is a rather bad language, but there is no alternative.

(library (loko drivers acpi aml)
  (export
    aml-eval
    aml-load-ports
    aml-get-term
    print-aml-tree

    aml-mutex-acquire!
    aml-mutex-release!

    ->buffer
    ->integer

    var? var-name var-parent
    var-find-child
    var-filter-children

    Processor?
    Processor-ProcessorID
    Processor-PBlockAddress
    Processor-PBlockLength

    Device?
    Name? Name-Object
    )
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (loko) loko-version record-writer)
    (loko match)
    (loko drivers acpi eisa-id)
    (loko drivers acpi resources)
    (loko drivers acpi tables)
    (loko system fibers)
    (loko system logging)
    ;; For direct hardware access
    (loko drivers pci)
    (loko system unsafe))

(define *debug* #f)

(define ACPI-revision 2)
(define AML-revision #x00000000)

(define (log/x severity x*)
  (send-log severity
            (call-with-string-output-port
              (lambda (p)
                (for-each (lambda (x) (display x p)) x*)))
            'SUBSYSTEM 'acpi))

(define (log/debug . x*) (log/x DEBUG x*))
(define (log/info . x*) (log/x INFO x*))
(define (log/error . x*) (log/x ERROR x*))
(define (log/warn . x*) (log/x WARNING x*))
(define (log/critical . x*) (log/x CRITICAL x*))

;;; AML namespace & object types

(define (var-writer v p wr)
  (display "#<" p)
  (display (record-type-name (record-rtd v)) p)
  (when (var? v)
    (display " " p)
    (wr (var->path v) p))
  (display ">" p))

(define-syntax define-record-type*
  (lambda (x)
    (syntax-case x ()
      ((_ name clause ...)
       #'(begin
           (define-record-type name clause ...)
           (define dummy
             (record-writer (record-type-descriptor name) var-writer)))))))

(define-record-type* var
  (fields name               ;string, up to four upper-case letters
          (mutable parent)   ;the parent acpi-node, or #f at the root
          (mutable children)))          ;hashtable of child vars

(define-record-type* Alias               ;namespace alias
  (sealed #t)
  (parent var)
  (fields SourceObject))                ;another var, but not an Alias

(define-record-type* Name               ;a named object
  (sealed #t)
  (parent var)
  (fields (mutable Object)))            ;not a var!

(define-record-type Field
  (sealed #t)
  (fields OperationRegion
          AccessType
          LockRule
          UpdateRule))

(define-record-type Index/Data
  (sealed #t)
  (fields IndexField
          DataField
          AccessType
          LockRule
          UpdateRule))

(define-record-type Bank/Data
  (sealed #t)
  (fields OpRegion
          BankRegister
          Value
          AccessType
          LockRule
          UpdateRule))

(define-record-type* FieldUnit
  (sealed #t)
  (parent var)
  (fields Field            ;Field or Index/Data
          Connection       ;for GenericSerialBus and GeneralPurposeIO
          BitIndex
          NumBits
          Integer?))

(define-record-type* Device
  (sealed #t)
  (parent var))

(define-record-type* Event
  (sealed #t)
  (parent var))

(define-record-type* Method
  (sealed #t)
  (parent var)
  (fields NumArgs
          SerializeRule
          SyncLevel
          body))

(define-record-type* Mutex
  (sealed #t)
  (parent var)
  (fields SyncLevel (mutable owner) acquire-ch release-ch))

(define (make-aml-mutex name sync-level)
  (let ((acquire-ch (make-channel))
        (release-ch (make-channel))
        (owner #f))
    (let ((mtx (make-Mutex name #f #f sync-level owner acquire-ch release-ch)))
      (spawn-fiber
       (lambda ()
         (let lp ()
           (match (get-message acquire-ch)
             [new-owner
              (Mutex-owner-set! mtx new-owner)
              (match (get-message release-ch)
                [owner
                 (assert (eq? owner new-owner))
                 (Mutex-owner-set! mtx #f)
                 (lp)])]))))
      mtx)))

(define (aml-mutex-acquire! mtx who timeout-ms)
  ;; XXX: This works under Loko; there's no preemption at this level
  (let ((owner (Mutex-owner mtx)))
    (cond
     ((eq? owner who)
      1)
     (else
      (perform-operation
       (choice-operation (wrap-operation (put-operation (Mutex-acquire-ch mtx) who)
                                         (lambda _ 1))
                         (if (>= timeout-ms #xffff)
                             (choice-operation)
                             (wrap-operation (wait-operation (/ timeout-ms 1000))
                                             (lambda _ 0)))))))))

(define (aml-mutex-release! mtx who)
  (let ((owner (Mutex-owner mtx)))
    (cond ((eq? owner who)
           (put-message (Mutex-release-ch mtx) who)
           #f)
          (else
           (assertion-violation 'aml-mutex-release
                                "Bad attempt to release a mutex"
                                mtx)))))

(define-record-type* OperationRegion
  (parent var)
  (fields RegionSpace                   ;one of the constants below
          Offset                        ;byte offset
          Length))                      ;bytes

(define SystemMemory     #x00)
(define SystemIO         #x01)
(define PCI_Config       #x02)
(define EmbeddedControl  #x03)
(define SMBus            #x04)
(define CMOS             #x05)
(define PciBarTarget     #x06)
(define IPMI             #x07)
(define GeneralPurposeIO #x08)
(define GenericSerialBus #x09)
(define PCC              #x0A)

(define-record-type* DataTableRegion
  (sealed #t)
  (parent OperationRegion)
  (fields SignatureString OemIDString OemTableIDString))

(define-record-type* PowerResource
  (sealed #t)
  (parent var)
  (fields SystemLevel ResourceOrder))

(define-record-type* Processor
  (sealed #t)
  (parent var)
  (fields ProcessorID PBlockAddress PBlockLength))

(define-record-type* ThermalZone
  (sealed #t)
  (parent var))

(define-record-type* BufferField
  (sealed #t)
  (parent var)
  (fields SourceBuffer BitIndex NumBits))

(define-record-type DDBHandle
  (sealed #t)
  (fields ))                            ;TODO

(define-record-type DebugObject
  (sealed #t))

(define-record-type ObjectReference
  (sealed #t)
  (fields Source Index))

#;
(define (DataRefObj? x)
  (or (integer? x)
      (string? x)
      (bytevector? x)
      (vector? x)
      (DDBHandle? x)
      (ObjectReference? x)))

(define (get-ObjectType x)
  ;; Object type codes. Except as noted in the comments, all of these
  ;; are represented as records.
  (define type-Uninitialized     0)       ;#f
  (define type-Integer           1)       ;integers (32 or 64 bits)
  (define type-String            2)       ;strings
  (define type-Buffer            3)       ;bytevectors
  (define type-Package           4)       ;vector
  (define type-FieldUnit         5)
  (define type-Device            6)
  (define type-Event             7)
  (define type-Method            8)
  (define type-Mutex             9)
  (define type-OperationRegion   10)
  (define type-PowerResource     11)
  (define type-Processor         12)
  (define type-ThermalZone       13)
  (define type-BufferField       14)
  (define type-DDBHandle         15)
  (define type-DebugObject       16)
  (cond
    ((not x) type-Uninitialized)
    ((integer? x) type-Integer)
    ((string? x) type-String)
    ((bytevector? x) type-Buffer)
    ((vector? x) type-Package)
    ((FieldUnit? x) type-FieldUnit)
    ((Device? x) type-Device)
    ((Event? x) type-Event)
    ((Method? x) type-Method)
    ((Mutex? x) type-Mutex)
    ((OperationRegion? x) type-OperationRegion)
    ((PowerResource? x) type-PowerResource)
    ((Processor? x) type-Processor)
    ((ThermalZone? x) type-ThermalZone)
    ((BufferField? x) type-BufferField)
    ((DDBHandle? x) type-DDBHandle)
    ((DebugObject? x) type-DebugObject)
    (else type-Uninitialized)))

(define (print-aml-tree port var)
  (define p (or port (current-output-port)))
  (let lp ((var var) (depth 0))
    (display (make-string (* 3 depth) #\space) p)
    (display "|- " p)
    (display (var-name var) p)
    (cond
      ((Name? var)
       (let ((obj (Name-Object var)))
         (cond
           ((not obj))
           ((integer? obj)
            (display "\t// " p)
            (display "#x" p)
            (display (number->string obj 16) p)
            (when (and (<= 0 obj #xFFFFFFFF)
                       (member (var-name var) '("_CID" "_HID")))
              (display " / " p)
              (write (eisa-id->text-id obj) p)))
           ((string? obj)
            (display "\t// " p)
            (write obj p))
           ((vector? obj)
            (display "\t// " p)
            (display "Package of length " p)
            (display (vector-length obj) p)
            (do ((len (fxmin 6 (vector-length obj)))
                 (i 0 (fx+ i 1)))
                ((fx=? i len)
                 (unless (fx=? i (vector-length obj))
                   (newline p)
                   (display (make-string (* 3 depth) #\space) p)
                   (display "  ..." p)))
              (newline p)
              (display (make-string (* 3 depth) #\space) p)
              (display "  " p)
              (write (vector-ref obj i) p)))
           ((bytevector? obj)
            (display "\t// " p)
            (cond ((<= (bytevector-length obj) 32)
                   (display obj p))
                  (else
                   (display "Buffer of length " p)
                   (display (bytevector-length obj) p)))
            (when (member (var-name var) '("_CRS" "_PRS"))
              (let ((port (open-bytevector-input-port obj)))
                (let lp ()
                  (let ((x (acpi-get-resource port)))
                    (unless (or (eof-object? x) (resource-end-tag? x))
                      (newline p)
                      (display (make-string (* 3 depth) #\space) p)
                      (display "  " p)
                      (write x p)
                      (lp)))))))
           (else
            (display "\t// " p)
            (display "Named object: " p)
            (write (Name-Object var) p)))))
      ((Method? var)
       (display "\t// Method with arity " p)
       (display (Method-NumArgs var) p))
      (else
       (display "\t// " p)
       (display (record-type-name (record-rtd var)) p)))
    (newline p)
    (cond ((var-children var) =>
           (lambda (children)
             (let-values ([(_ children) (hashtable-entries children)])
               (vector-sort! (lambda (x y)
                               (string-ci<? (var-name x) (var-name y)))
                             children)
               (vector-for-each (lambda (child) (lp child (+ depth 1)))
                                children)))))))

(define (var-find-child var name)
  (cond ((var-children var) =>
         (lambda (children)
           (hashtable-ref children name #f)))
        (else #f)))

(define (var-filter-children proc var)
  (cond ((var-children var) =>
         (lambda (children)
           (let-values ([(_k* v*) (hashtable-entries children)])
             (filter proc (vector->list v*)))))
        (else '())))

(define (var-add-child! var child-var)
  (unless (and (var? var) (var? child-var))
    (assertion-violation 'var-add-child! "Expected variables" var child-var))
  (when *debug*
    (display "A: ")
    (write (var->path var))
    (write (var->path child-var)))
  #;
  (when (var-find-child var (var-name child-var))
    (assertion-violation 'acpi-add-child! "Named child already exists" var child-var))
  (when (var-parent child-var)
    (assertion-violation 'acpi-add-child! "The child already has a parent" var child-var))
  (when (not (var-children var))
    (var-children-set! var (make-hashtable string-hash string=?)))
  (hashtable-set! (var-children var) (var-name child-var) child-var)
  (var-parent-set! child-var var)
  (when *debug*
    (display " -> ")
    (write (var->path child-var))
    (newline)))

(define (var-unlink-child! child-var)
  (let ((parent (var-parent child-var)))
    (hashtable-delete! parent (var-name child-var))
    (var-parent-set! parent #f)))

(define (var->path var)
  (reverse
   (let f ((var var))
     (cond ((var-parent var) =>
            (lambda (parent)
              (cons (var-name var) (f parent))))
           (else
            (list (var-name var)))))))

(define (make-predefined-acpi-root)
  (let ((root (make-Name 'root #f #f #f)))
    (var-add-child! root (make-Name "_GPE" #f #f #f))
    (var-add-child! root (make-Name "_PR" #f #f #f))
    (var-add-child! root (make-Name "_SB" #f #f #f))
    (var-add-child! root (make-Name "_SI" #f #f #f))
    (var-add-child! root (make-Name "_TZ" #f #f #f))
    root))

(define nullname (make-var 'null #f #f))

;; Resolve the name, which can be absolute or relative to the current
;; one (var). Returns a var object or #f.
(define (aml-resolve var name*)
  (unless (var? var)
    (assertion-violation 'aml-resolve "Expected a variable" var name*))
  (match name*
    [('root . rest)
     (let lp ((var var))
       (cond ((var-parent var) => lp)
             (else (aml-resolve var rest))))]
    [('up . rest)
     (cond ((var-parent var) =>
            (lambda (parent)
              (aml-resolve parent rest)))
           (else (aml-resolve var rest)))]
    [((? string? name) . rest)
     (cond ((var-find-child var name) =>
            (lambda (child)
              (aml-resolve child rest)))
           ((var-parent var) =>
            (lambda (parent)
              (aml-resolve parent name*)))
           (else
            (log/debug "Name " name " not found from " (var->path var))
            #f))]
    [()
     (if (Alias? var)
         (Alias-SourceObject var)
         var)]
    [('null)
     ;; This is a special case used as /dev/null for assignments
     nullname]
    [x (assertion-violation 'aml-resolve "Invalid name" name*)]))

;; This is given the current path / scope (as a var object) and path
;; used for declaring a named object. It resolves the path up until
;; the last component, which is the name of the new object. You can
;; think of it as returning two values: the directory where the object
;; will be created and the name of the object.
(define (aml-resolve+split current-var path)
  (define (split-path path)
    (let ((rev (reverse path)))
      (values (reverse (cdr rev))
              (car rev))))
  (let-values ([(path^ name) (split-path path)])
    (cond ((aml-resolve current-var path^) =>
           (lambda (path)
             (values path name)))
          (else
           (assertion-violation 'aml-resolve+split "Path not found" path)))))

;;; AML parser

(define aml-byteops
  '#(;; 00
     (0 Constant) (1 Constant) #f #f #f #f
     (Alias TermObject (NameString NameString))
     #f
     (Name TermObject (NameString DataRefObject))
     #f
     (BytePrefix DataObject ByteData)
     (WordPrefix DataObject WordData)
     (DWordPrefix DataObject DWordData)
     (StringPrefix DataObject AsciizCharList)
     (QWordPrefix DataObject QWordData)
     #f

     ;; 10
     (Scope TermObject (NameString) TermList)
     (Buffer TermObject (TermArg) ByteList)
     (Package TermObject (ByteData) Package)
     (VarPackage TermObject (TermArg) Package)
     (Method TermObject (NameString ByteData) TermList)
     (ExternalOp NameObject (NameString ByteData ByteData))
     #f #f
     #f #f #f #f #f #f #f #f

     ;; 20
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f Name Name

     ;; 30
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; 40
     #f Name Name Name Name Name Name Name Name
     Name Name Name Name Name Name Name

     ;; 50
     Name Name Name Name Name Name Name Name Name
     Name Name ExtOpPrefix Name #f Name Name

     ;; 60
     ((Local 0) LocalObject) ((Local 1) LocalObject) ((Local 2) LocalObject) ((Local 3) LocalObject)
     ((Local 4) LocalObject) ((Local 5) LocalObject) ((Local 6) LocalObject) ((Local 7) LocalObject)
     ((Arg 0) ArgObject) ((Arg 1) ArgObject) ((Arg 2) ArgObject) ((Arg 3) ArgObject)
     ((Arg 4) ArgObject) ((Arg 5) ArgObject) ((Arg 6) ArgObject) #f

     ;; 70
     (Store TermObject (TermArg SuperName))
     (RefOf TermObject (SuperName))
     (Add TermObject (TermArg TermArg Target))
     (Concat TermObject (TermArg TermArg Target))
     (Subtract TermObject (TermArg TermArg Target))
     (Increment TermObject (SuperName))
     (Decrement TermObject (SuperName))
     (Multiply TermObject (TermArg TermArg Target))
     (Divide TermObject (TermArg TermArg Target Target))
     (ShiftLeft TermObject (TermArg TermArg Target))
     (ShiftRight TermObject (TermArg TermArg Target))
     (And TermObject (TermArg TermArg Target))
     (Nand TermObject (TermArg TermArg Target))
     (Or TermObject (TermArg TermArg Target))
     (Nor TermObject (TermArg TermArg Target))
     (Xor TermObject (TermArg TermArg Target))

     ;; 80
     (Not TermObject (TermArg Target))
     (FindSetLeftBit TermObject (TermArg Target))
     (FindSetRightBit TermObject (TermArg Target))
     (DerefOf TermObject (TermArg))
     (ConcatRes TermObject (TermArg TermArg Target))
     (Mod TermObject (TermArg TermArg Target))
     (Notify TermObject (SuperName TermArg))
     (SizeOf TermObject (SuperName))
     (Index TermObject (TermArg TermArg Target))
     (Match TermObject (TermArg ByteData TermArg ByteData TermArg))
     (CreateDWordField TermObject (TermArg TermArg NameString))
     (CreateWordField TermObject (TermArg TermArg NameString))
     (CreateByteField TermObject (TermArg TermArg NameString))
     (CreateBitField TermObject (TermArg TermArg NameString))
     (ObjectType TermObject (SuperName))
     (CreateQWordField TermObject (TermArg TermArg NameString))

     ;; 90
     (LAnd TermObject (TermArg TermArg))
     (LOr TermObject (TermArg TermArg))
     (LNot TermObject (TermArg))
     (LEqual TermObject (TermArg TermArg))
     (LGreater TermObject (TermArg TermArg))
     (LLess TermObject (TermArg TermArg))
     (ToBuffer TermObject (TermArg Target))
     (ToDecimalString TermObject (TermArg Target))
     (ToHexString TermObject (TermArg Target))
     (ToInteger TermObject (TermArg Target))
     #f #f
     (ToString TermObject (TermArg TermArg Target))
     (CopyObject TermObject (TermArg SimpleName))
     (Mid TermObject (TermArg TermArg TermArg Target))
     (Continue TermObject ())

     ;; A0
     (If TermObject (TermArg) TermList)
     (Else TermObject () TermList)
     (While TermObject (TermArg) TermList)
     (Noop TermObject ())
     (Return TermObject (TermArg))
     (Break TermObject ())
     #f #f #f #f #f #f #f #f #f #f

     ;; B0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; C0
     #f #f #f #f #f #f #f #f #f #f #f #f
     (Breakpoint TermObject ())
     #f #f #f

     ;; D0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; E0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; F0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f (-1 Constant)))

(define aml-ext-byteops
  '#(;; 0
     #f
     (Mutex TermObject (NameString ByteData))
     (Event TermObject (NameString))
     #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; 10
     #f #f
     (CondRefOf TermObject (SuperName SuperName))
     (CreateField TermObject (TermArg TermArg TermArg NameString))
     #f #f #f #f #f #f #f #f #f #f #f
     (LoadTable TermObject (TermArg TermArg TermArg TermArg TermArg TermArg))

     ;; 20
     (Load TermObject (NameString SuperName))
     (Stall TermObject (TermArg))
     (Sleep TermObject (TermArg))
     (Acquire TermObject (SuperName WordData))
     (Signal TermObject (SuperName))
     (Wait TermObject (SuperName TermArg))
     (Reset TermObject (SuperName))
     (Release TermObject (SuperName))
     (FromBCD TermObject (TermArg Target))
     (ToBCD TermObject (TermArg Target))
     (Unload TermObject (SuperName))    ;XXX: deprecated
     #f #f #f #f #f

     ;; 30
     (Revision Constant)
     (Debug DebugObject)
     (Fatal TermObject (ByteData DWordData TermArg))
     (Timer TermObject ())
     #f #f #f #f #f #f #f #f #f #f #f #f

     ;; 40
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; 50
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; 60
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; 70
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; 80
     (OpRegion TermObject (NameString ByteData TermArg TermArg))
     (Field TermObject (NameString ByteData) FieldList)
     (Device TermObject (NameString) TermList)
     (Processor TermObject (NameString ByteData DWordData ByteData) TermList)
     (PowerRes TermObject (NameString ByteData WordData) TermList)
     (ThermalZone TermObject (NameString) TermList)
     (IndexField TermObject (NameString NameString ByteData) FieldList)
     (BankField TermObject (NameString NameString TermArg ByteData) FieldList)
     (DataRegion TermObject (NameString TermArg TermArg TermArg))
     #f #f #f #f #f #f #f

     ;; 90
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; A0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; B0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; C0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; D0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; E0
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f

     ;; F0
    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (get-package-length port)
  (let ((lead-byte (get-u8 port)))
    (case (fxbit-field lead-byte 6 8)
      ((0) (fxand lead-byte #b111111))
      ((1) (fxior (fxarithmetic-shift-left (get-u8 port) 4)
                  (fxand #b1111 lead-byte)))
      ((2) (let* ((x (get-u8 port))
                  (y (get-u8 port)))
             (fxior (fxarithmetic-shift-left y 12)
                    (fxarithmetic-shift-left x 4)
                    (fxand #b1111 lead-byte))))
      (else (let* ((x (get-u8 port))
                   (y (get-u8 port))
                   (z (get-u8 port)))
              (fxior (fxarithmetic-shift-left z 20)
                     (fxarithmetic-shift-left y 12)
                     (fxarithmetic-shift-left x 4)
                     (fxand #b1111 lead-byte)))))))

(define (name-segment-bytes->string b1 b2 b3 b4)
  (if (not (eqv? b4 (char->integer #\_)))
      (string (integer->char b1)
              (integer->char b2)
              (integer->char b3)
              (integer->char b4))
      (if (not (eqv? b3 (char->integer #\_)))
          (string (integer->char b1)
                  (integer->char b2)
                  (integer->char b3))
          (if (not (eqv? b2 (char->integer #\_)))
              (string (integer->char b1)
                      (integer->char b2))
              (string (integer->char b1))))))

(define (get-name-string port)
  (define (read-one-nameseg start)
    (let* ((byte1 (or start (get-u8 port)))
           (byte2 (get-u8 port))
           (byte3 (get-u8 port))
           (byte4 (get-u8 port)))
      (when (eof-object? byte4)
        (assertion-violation 'get-name-string
                             "Unexpected eof in nameseg"))
      (name-segment-bytes->string byte1 byte2 byte3 byte4)))
  (cons 'name
        (let lp ((ret '()) (byte (get-u8 port)))
          (cond
            ((eof-object? byte)
             (assertion-violation 'get-name-string
                                  "Unexpected eof in namestring"))
            ((or (fx<=? (char->integer #\A) byte (char->integer #\Z))
                 (eqv? byte (char->integer #\_)))
             (reverse (cons (read-one-nameseg byte) ret)))
            ((eqv? byte (char->integer #\\))  ;absolute path
             (lp (cons 'root ret) (get-u8 port)))
            ((eqv? byte (char->integer #\^))
             (lp (cons 'up ret) (get-u8 port)))
            ((eqv? byte (char->integer #\.))  ;dual name
             (let* ((name1 (read-one-nameseg #f))
                    (name2 (read-one-nameseg #f)))
               (reverse `(,name2 ,name1 ,@ret))))
            ((eqv? byte (char->integer #\/))  ;multi name
             (do ((len (get-u8 port))
                  (i 0 (fx+ i 1))
                  (ret ret (cons (read-one-nameseg #f) ret)))
                 ((fx=? i len)
                  (reverse ret))))
            ((eqv? byte #x00) (reverse ret))  ;null name
            (else
             (assertion-violation 'get-name-string
                                  "Unexpected byte in namestring"
                                  byte))))))

(define (get-fixed-argument port var type)
  ;; XXX: could do some validation on the types
  (case type
    ((NameString NameString^) (get-name-string port))
    ((TermArg)    (aml-get-term port var #t))
    ((ByteData)   (get-u8 port))
    ((WordData)   (bytevector-u16-ref (get-bytevector-n port 2) 0 (endianness little)))
    ((DWordData)  (bytevector-u32-ref (get-bytevector-n port 4) 0 (endianness little)))
    ((QWordData)  (bytevector-u64-ref (get-bytevector-n port 8) 0 (endianness little)))
    ((SuperName)
     (if (eqv? (lookahead-u8 port) 0)
         (begin (get-u8 port) '(name null))
         (aml-get-term port var #f)))
    ((Target)
     (if (eqv? (lookahead-u8 port) 0)
         (begin (get-u8 port) '(name null))
         (aml-get-term port var #f)))
    ((AsciizCharList)
     (let lp ((bytes '()))
       (let ((byte (get-u8 port)))
         (if (zero? byte)
             (list->string (map integer->char (reverse bytes)))
             (lp (cons (fxand #x7f byte) bytes))))))
    ((SimpleName)
     (if (eqv? (lookahead-u8 port) 0)
         (begin (get-u8 port) '(name null))
         (aml-get-term port var #f)))
    ((DataRefObject) (aml-get-term port var #t))
    (else
     (error 'get-fixed-argument
            "unknown fixed argument type" type))))

(define (get-fixed-arguments port var callsite? type*)
  (if (null? type*)
      '()
      (let ((arg (get-fixed-argument port var (car type*))))
        (match arg
          [('name . name)
           (cond
             ((and (not (memq (car type*) '(NameString SuperName)))
                   callsite? var
                   (aml-resolve var name)) =>
              (lambda (rhs)
                ;; If the name resolves to a method then it's a method
                ;; invocation and the arguments will follow. This is
                ;; such an unfortunate bytecode design.
                (if (Method? rhs)
                    (let ((nargs (Method-NumArgs rhs)))
                      (let* ((calltype* (vector->list (make-vector nargs 'TermArg)))
                             (call `(Call ,arg ,@(get-fixed-arguments port var callsite? calltype*))))
                        (cons call (get-fixed-arguments port var callsite? (cdr type*)))))
                    (cons arg (get-fixed-arguments port var callsite? (cdr type*))))))
             (else
              (cons arg (get-fixed-arguments port var callsite? (cdr type*)))))]
          [_
           (cons arg (get-fixed-arguments port var callsite? (cdr type*)))]))))

(define (get-field-element port var)
  (let ((byte (get-u8 port)))
    (case byte
      ((#x00)                           ;reserved field
       (list 'reserved-field (get-package-length port)))
      ((#x01)                           ;access field
       (let* ((access-type (get-u8 port))
              (access-attrib (get-u8 port)))
         (list 'access-field access-type access-attrib)))
      ((#x02)                           ;connect field
       (list 'connect-field (aml-get-term port var #f)))
      ((#x03)                           ;extended access field
       (let* ((access-type (get-u8 port))
              (extended-access-attribute (get-u8 port))
              (access-length (get-u8 port)))
         (list 'extended-access-field access-type
               extended-access-attribute access-length)))
      (else                             ;named field
       (let* ((byte2 (get-u8 port))
              (byte3 (get-u8 port))
              (byte4 (get-u8 port))
              (length (get-package-length port)))
         (list 'named-field
               (name-segment-bytes->string byte byte2 byte3 byte4)
               length))))))

(define (get-variable-arguments port var type end-position)
  (case type
    ((TermList)
     (let lp ((terms '()))
       (if (< (port-position port) end-position)
           (lp (cons (aml-get-term port var #t) terms))
           (reverse terms))))
    ((FieldList)
     (let lp ((fields '()))
       (if (< (port-position port) end-position)
           (lp (cons (get-field-element port var) fields))
           (reverse fields))))
    ((Package)
     (let lp ((ret '()))
       (if (< (port-position port) end-position)
           (lp (cons (aml-get-term port var #f) ret))
           `((Package ,@(reverse ret))))))
    ((ByteList)
     (list (get-bytevector-n port (- end-position (port-position port)))))
    (else
     (error 'get-variable-arguments
            "Unknown variable argument type" type))))

;; Parse an AML term from the binary input port. The port must have
;; been positioned past the header with acpi-get-table-header. Should
;; be called multiple times until the eof object is returned.
(define (aml-get-term port var callsite?)
  (let lp ((byteops aml-byteops))
    (let ((opcode (lookahead-u8 port)))
      (cond
        ((eof-object? opcode)
         opcode)

        ((vector-ref byteops opcode) =>
         (lambda (def)
           (match def
             [(name 'TermObject fixed-type* variable-type)
              (get-u8 port)
              (let* ((pos (port-position port))
                     (pkg-length (get-package-length port))
                     (fixargs (get-fixed-arguments port var callsite? fixed-type*)))
                (let ((var
                       (and var
                            (match (cons name fixargs)
                              [((or 'Device 'Method 'PowerRes 'Processor 'Scope 'ThermalZone)
                                ('name . path) . _)
                               ;; Enter the new namespace
                               (aml-resolve var path)]
                              [_ var]))))
                  (let ((varargs (get-variable-arguments port var variable-type (fx+ pos pkg-length))))
                    `(,name ,@fixargs (begin ,@varargs)))))]

             [(name 'TermObject fixed-type*)
              (get-u8 port)
              (cons name (get-fixed-arguments port var callsite? fixed-type*))]

             [(_name 'DataObject (? symbol? fixed-type))
              (get-u8 port)
              (car (get-fixed-arguments port var callsite? (list fixed-type)))]

             [(v (or 'ArgObject 'LocalObject 'DebugObject))
              (get-u8 port)
              v]

             [(v 'Constant)
              (get-u8 port)
              v]

             ['Name
              (car (get-fixed-arguments port var callsite? '(NameString^)))]

             ['ExtOpPrefix
              (get-u8 port)
              (lp aml-ext-byteops)]

             [_
              (error 'aml-get-term "Internal error: wrong definition" opcode def)])))

        (else
         (assertion-violation 'aml-get-term
                              (if (eq? byteops aml-byteops)
                                  "Undefined AML opcode"
                                  "Undefined extended AML opcode")
                              opcode))))))

;; Read all AML terms from a list of ports.
(define (aml-get-terms-from-ports ports root)
  (if (null? ports)
      '()
      (let ((port (car ports)))
        (set-port-position! port 0)
        (acpi-get-table-header port)
        (let f ()
          (let ((term (aml-get-term port root #t)))
            (if (eof-object? term)
                (aml-get-terms-from-ports (cdr ports) root)
                (cons term (f))))))))

;;; A pass to find method arities

;; Method calls are not indicated as such in the bytecode. When a name
;; appears in the code and it resolves to a method, then it is
;; automatically a method call.

;; This pass does a dry run of the code in a throwaway namespace. The
;; goal is to declare all methods and aliases. A second pass over the
;; code fixes up all method call sites.

(define (aml-dryrun term current-var)
  (match term
    [(? string?) #f]
    [(? integer?) #f]
    [('name . name) #f]

    [('Alias Source ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((source (aml-dryrun Source current-var)))
         (let ((alias (make-Alias name #f #f source)))
           (var-add-child! path alias))))]

    [('Buffer len bv) #f]

    [('Device ('name . path) ObjectList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((device (make-var name #f #f)))
         (var-add-child! path device)
         (aml-dryrun ObjectList device)))]

    [('Method ('name . path) MethodFlags code)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((NumArgs (fxbit-field MethodFlags 0 3)))
         (let ((method (make-Method name #f #f NumArgs #f 0 code)))
           (var-add-child! path method)
           ;; XXX: Do a dry run on the code to also get all
           ;; declarations from inside methods. Dynamically created
           ;; methods with arity conflicts are disallowed by this.
           (assert path)
           (assert (eq? method (aml-resolve path (list name))))
           (aml-dryrun code method))))]

    [('Name ('name . path) expr)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (aml-dryrun expr current-var)
       (var-add-child! path (make-var name #f #f)))]

    [('Package len _) #f]
    [('VarPackage len _) (aml-dryrun len current-var)]

    [('PowerRes ('name . path) SystemLevel ResourceOrder ObjectList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((power-res (make-var name #f #f)))
         (var-add-child! path power-res)
         (aml-dryrun ObjectList power-res)))]

    [('Processor ('name . path) _ProcID _PblkAddr _PblkLen ObjectList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((processor (make-var name #f #f)))
         (var-add-child! path processor)
         (aml-dryrun ObjectList processor)))]

    [('Scope ('name . path) TermList)
     ;; TODO: should this create the scope if it does not exist?
     (let ((scope (aml-resolve current-var path)))
       (aml-dryrun TermList scope))]

    [('ThermalZone ('name . path) ObjectList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((thermal-zone (make-var name #f #f)))
         (var-add-child! path thermal-zone)
         (aml-dryrun ObjectList thermal-zone)))]

    [('BankField ('name . RegionName) ('name . BankName) BankValue FieldFlags ('begin field* ...))
     (aml-define-fields! current-var #f field*)]

    [('Field ('name . opregion-name) FieldFlags ('begin field* ...))
     (aml-define-fields! current-var #f field*)]

    [('OpRegion ('name . path) RegionSpace RegionOffset RegionLen)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (aml-dryrun RegionOffset current-var)
       (aml-dryrun RegionLen current-var)
       (var-add-child! path (make-var name #f #f)))]

    [('IndexField ('name . index-fieldunit-name)
                  ('name . data-fieldunit-name)
                  FieldFlags ('begin field* ...))
     (aml-define-fields! current-var #f field*)]

    [('DataRegion ('name . path) SignatureString OemIDString OemTableIDString)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (var-add-child! path (make-var name #f #f)))]

    [('CreateBitField SourceBuffer BitIndex ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (aml-dryrun SourceBuffer current-var)
       (aml-dryrun BitIndex current-var)
       (var-add-child! path (make-var name #f #f)))]

    [('CreateField SourceBuffer BitIndex NumBits ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (aml-dryrun SourceBuffer current-var)
       (aml-dryrun BitIndex current-var)
       (aml-dryrun NumBits current-var)
       (var-add-child! path (make-var name #f #f)))]

    [((and cmd (or 'CreateQWordField 'CreateDWordField 'CreateWordField 'CreateByteField))
      SourceBuffer ByteIndex ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (aml-dryrun SourceBuffer current-var)
       (aml-dryrun ByteIndex current-var)
       (var-add-child! path (make-var name #f #f)))]

    [('Mutex ('name . path) SyncLevel)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (var-add-child! path (make-var name #f #f)))]

    ['Revision #f]
    ['Debug #f]
    [('Arg X) #f]
    [('Local X) #f]

    [(_op . args)
     (for-each (lambda (arg) (aml-dryrun arg current-var)) args)]

    [x (assertion-violation 'aml-dryrun "Invalid code" x)]))

;;; Type conversion

;; There is much that can be said about this...

(define (->integer x)
  ;; TODO: Fill in everything.
  (cond
    ((integer? x) x)
    ((string? x) (or (string->number x 16) 0))
    ((Name? x) (->integer (Name-Object x)))
    ((FieldUnit? x)
     (let ((field (FieldUnit-Field x))
           (bitidx (FieldUnit-BitIndex x))
           (numbits (FieldUnit-NumBits x)))
       (cond
         ((FieldUnit-Integer? x)
          (log/warn "TODO: FieldUnit of integer size "
                    (list field bitidx numbits))
          (let ((opregion (Field-OperationRegion field))
                (access-type (Field-AccessType field))
                (lock-rule (Field-LockRule field))
                (update-rule (Field-UpdateRule field)))
            (let ((region-space (OperationRegion-RegionSpace opregion))
                  (offset (OperationRegion-Offset opregion))
                  (length (OperationRegion-Length opregion)))
              (log/warn (list
                         (list access-type lock-rule update-rule)
                         (list region-space offset length)))
              (cond
                ((eqv? region-space PCI_Config)
                 (let* ((dev^ (var-parent opregion))
                        #;(bus^ (var-parent dev^))
                        (bus 0 #; (->integer (aml-eval bus^ `(Call (name "_ADR")))))
                        (dev/func (->integer (aml-eval dev^ `(Call (name "_ADR")))))
                        (dev (bitwise-bit-field dev/func 16 32))
                        (func (bitwise-bit-field dev/func 0 16))
                        (sel (make-pci-selector bus dev func)))
                   (cond
                     ((not (eqv? 0 (fxmod bitidx 8)))
                      (error '->integer "Unaligned PCI configuration space FieldUnit" bitidx))
                     (else
                      (let ((reg (fx+ offset (fxdiv bitidx 8))))
                        (unless (fx<=? 0 (fxdiv (fx+ bitidx numbits) 8) length)
                          (error '->integer "Out of range PCI read"))
                        (case numbits
                          ((8) (pci-get-conf-u8 sel reg))
                          ((16) (pci-get-conf-u16 sel reg))
                          ((32) (pci-get-conf-u32 sel reg))
                          (else
                           (error '->integer "Bad PCI read size" numbits))))))))
                (else
                 (error '->integer "Unimplemented region space" region-space))))))
         (else
          ;; FIXME: Return the data as a bytevector
          (log/warn "TODO: FieldUnit of non-integer size "
                    (list (FieldUnit-Field x)
                          (FieldUnit-BitIndex x)
                          (FieldUnit-NumBits x)))
          0))))
    ((var? x)
     (log/warn "TODO: ->integer of " x)
     0)
    (else
     (log/error "->integer cannot handle this: " x)
     0)))

(define (->char x)
  (cond
    ((char? x) x)
    ((integer? x) (integer->char x))
    ((Name? x) (->char (Name-Object x)))
    (else #\nul)))

(define (->buffer x)
  ;; TODO
  (cond
    ((Name? x) (->buffer (Name-Object x)))
    (else x)))

(define (->string x)
  ;; TODO
  (cond
    ((integer? x)
     ;; TODO: is is 16?
     (string->number x 16))
    ((Name? x) (->string (Name-Object x)))
    (else
     x)))

;;; AML interpreter

(define-record-type env
  (sealed #t)
  (fields args locals stack-depth return delays limit)
  (protocol
   (lambda (p)
     (lambda (arg* stack-depth return delays limit)
       ;; TODO: Maybe keep track of which variables were dynamically
       ;; created here, so they can be uncreated at the end the of
       ;; scope?
       (let ((args (make-vector 7 #f))
             (locals (make-vector 8 #f)))
         (do ((i 0 (fx+ i 1))
              (arg* arg* (cdr arg*)))
             ((null? arg*))
           (vector-set! args i (car arg*)))
         (p args locals stack-depth return delays limit))))))

(define (aml-define-fields! current-var field field*)
  (let lp ((idx 0) (field* field*) (connection #f))
    (match field*
      [(('named-field name size) . rest*)
       ;; FIXME: 32 here should be 64 if the AML revision is >= 2
       (let ((field-unit (make-FieldUnit name #f #f field connection idx size
                                         (<= size 32))))
         (var-add-child! current-var field-unit)
         (lp (fx+ idx size) rest* connection))]
      [(('reserved-field size) . rest*)
       (lp (fx+ idx size) rest* connection)]
      ;; TODO: access-field
      ;; TODO: connect-field
      ;; TODO: extended-access-field
      [() #f]
      [x
       (error 'aml-eval-statement "Unimplemented field" x)])))

(define (aml-set! name value current-var env)
  (match name
    [('name 'null) #f]
    [('Local n)
     (vector-set! (env-locals env) n value)]
    [('Arg n)
     (vector-set! (env-args env) n value)]
    [('name . path)
     (let ((var (aml-resolve current-var path)))
       (cond ((Name? var)
              (log/debug "Store " var ": " value)
              (Name-Object-set! var value))
             ;; FIXME: Need to know if this is dynamic or not,
             ;; somehow. Create a new dynamic name.
             ((not var)
              (log/warn "TODO: Store to dynamic name " current-var "/ " name))
             (else
              (aml-set! var value current-var env))))]
    [(or ('Index . _) (? ObjectReference?))
     (let ((ref (if (ObjectReference? name)
                    name
                    (aml-eval* name current-var env))))
       (let ((val (ObjectReference-Source ref))
             (idx (ObjectReference-Index ref)))
         (cond ((string? val) (string-set! val idx (->char value)))
               ((vector? val) (vector-set! val idx value))
               ((bytevector? val) (bytevector-u8-set! val idx (->integer value))))))]
    ((? BufferField?)
     (let ((source (BufferField-SourceBuffer name))
           (bitidx (BufferField-BitIndex name))
           (numbits (BufferField-NumBits name)))
       (cond ((and (eqv? 0 (mod bitidx 8))
                   (eqv? numbits 32))
              (bytevector-u32-set! source (div bitidx 8) (bitwise-bit-field (->integer value) 0 32)
                                   (native-endianness)))
             (else
              (log/warn "TODO: store to " (list bitidx numbits ) " in " source)))
       value))
    [(or 'Debug (? DebugObject?))
     (display "AML Debug: ")
     (cond
       ((number? value)
        (display "#x")
        (display (number->string value 16)))
       (else
        (write value)))
     (newline)]
    [_
     (log/warn "TODO: Store to " name)])
  value)

(define (aml-eval* term current-var env)
  ((env-limit env))                ;no infinite looooooooooops
  (when *debug*
    (log/debug (call-with-string-output-port
                 (lambda (p)
                   (display "AML: " p)
                   (write (var->path current-var) p)
                   (write (let f ((term term))
                            (match term
                              [(a b c . _) (list (f a) (f b) (f c) '...)]
                              [(a b c) (list (f a) (f b) (f c))]
                              [(a b) (list (f a) (f b))]
                              [(a) (list (f a))]
                              [x x]))
                          p)))))
  (match term
    [(? string?) term]
    [(? integer?) term]
    [('name . name)
     (let ((var (aml-resolve current-var name)))
       (cond
         #;
         ((Name? var)
          ;; TODO: This is kind of weird.
          (Name-Object var))
         ((var? var) var)
         (else
          (log/warn "Name not found: " name " -- " (var->path var))
          term)))]

    ;;; Conditionals and definition bodies

    [('begin ('If test consequent) ('Else alternate) . term*)
     (let ((ret (if (not (eqv? (->integer (aml-eval* test current-var env)) 0))
                    (aml-eval* consequent current-var env)
                    (aml-eval* alternate current-var env))))
       (if (null? term*)
           ret
           (aml-eval* `(begin ,@term*) current-var env)))]
    [('begin term . term*)
     (let ((ret (aml-eval* term current-var env)))
       (if (null? term*)
           ret
           (aml-eval* `(begin ,@term*) current-var env)))]
    [('begin) #f]

    ;;; Miscellaneous (named) object creation

    [('Alias Source ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((source (aml-eval* Source current-var env)))
         (let ((alias (make-Alias name #f #f source)))
           (var-add-child! path alias))))
     #f]

    [('Buffer (? fixnum? len) ('begin bv))
     (let ((ret (make-bytevector len 0)))
       (bytevector-copy! bv 0 ret 0 (fxmin len (bytevector-length bv)))
       ret)]

    [('Device ('name . path) TermList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((device (make-Device name #f #f)))
         (var-add-child! path device)
         (aml-eval* TermList device env)))
     #f]

    [('Method ('name . path) MethodFlags code)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((NumArgs (fxbit-field MethodFlags 0 3))
             (SerializeRule (if (fxbit-set? MethodFlags 3) 'Serialized 'NotSerialized))
             (SyncLevel (fxbit-field MethodFlags 4 8)))
         (let ((method (make-Method name #f #f NumArgs SerializeRule SyncLevel code)))
           (var-add-child! path method))))
     #f]

    [('Name ('name . path) expr)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((value (aml-eval* expr current-var env)))
         (var-add-child! path (make-Name name #f #f value))))
     #f]

    [((or 'Package 'VarPackage) len ('begin ('Package . elem*)))
     (let ((len (aml-eval* len current-var env)))
       (do ((ret (make-vector len #f))
            (elem* elem* (cdr elem*))
            (i 0 (fx+ i 1)))
           ((or (fx=? i len) (null? elem*))
            ret)
         ;; XXX: The elements can be integers, strings, bytevectors,
         ;; vectors and references, i.e. (name . <path>).
         ;; Maybe do something like a linker-step.
         (let ((elem (car elem*)))
           (match elem
             [('name . path)
              (cond ((aml-resolve current-var path) =>
                     (lambda (v) (vector-set! ret i v)))
                    (else
                     ;; The name may be defined later. Delay
                     ;; evaluation for now.
                     ((env-delays env)
                      (lambda ()
                        (let ((v (aml-resolve current-var path)))
                          (when (not v)
                            (log/debug "Resolving " current-var "/" path " failed"))
                          (vector-set! ret i v))))

                     (vector-set! ret i elem)))]
             [((or 'Buffer 'Package 'VarPackage) . _)
              (vector-set! ret i (aml-eval* elem current-var env))]
             [_                         ;integer or string
              (vector-set! ret i elem)]))))]

    [('PowerRes ('name . path) SystemLevel ResourceOrder ObjectList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((power-res (make-PowerResource name #f #f SystemLevel ResourceOrder)))
         (var-add-child! path power-res)
         (aml-eval* ObjectList power-res env)))
     #f]

    [('Processor ('name . path) ProcID PblkAddr PblkLen ObjectList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((processor (make-Processor name #f #f ProcID PblkAddr PblkLen)))
         (var-add-child! path processor)
         (aml-eval* ObjectList processor env)))
     #f]

    [('Scope ('name . path) TermList)
     (let ((scope (aml-resolve current-var path)))
       (aml-eval* TermList scope env))
     #f]

    [('ThermalZone ('name . path) ObjectList)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((thermal-zone (make-ThermalZone name #f #f)))
         (var-add-child! path thermal-zone)
         (aml-eval* ObjectList thermal-zone env)))
     #f]

    ;;; Operation Regions and Fields

    [('BankField ('name . RegionName) ('name . BankName) BankValue FieldFlags ('begin field* ...))
     (let* ((region (aml-resolve current-var RegionName))
            (bank (aml-resolve current-var BankName))
            (BankValue (->integer (aml-eval* BankValue current-var env)))
            (AccessType (fxbit-field FieldFlags 0 4))
            (LockRule (if (fxbit-set? FieldFlags 4)  'Lock 'NoLock))
            (UpdateRule (fxbit-field FieldFlags 5 7)))
       (let ((field (make-Bank/Data region bank BankValue AccessType LockRule UpdateRule)))
         (aml-define-fields! current-var field field*)))
     #f]

    [('Field ('name . opregion-name) FieldFlags ('begin field* ...))
     (let ((opregion (aml-resolve current-var opregion-name))
           (AccessType (fxbit-field FieldFlags 0 4))
           (LockRule (if (fxbit-set? FieldFlags 4)  'Lock 'NoLock))
           (UpdateRule (fxbit-field FieldFlags 5 7)))
       (let ((field (make-Field opregion AccessType LockRule UpdateRule)))
         (aml-define-fields! current-var field field*)))
     #f]

    [('OpRegion ('name . path) RegionSpace RegionOffset RegionLen)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let* ((RegionOffset (->integer (aml-eval* RegionOffset current-var env)))
              (RegionLen (->integer (aml-eval* RegionLen current-var env))))
         (let ((opregion (make-OperationRegion name #f #f RegionSpace RegionOffset RegionLen)))
           (var-add-child! path opregion))))
     #f]

    [('IndexField ('name . index-fieldunit-name)
                  ('name . data-fieldunit-name)
                  FieldFlags ('begin field* ...))
     (let ((index-fu (aml-resolve current-var index-fieldunit-name))
           (data-fu (aml-resolve current-var data-fieldunit-name))
           (AccessType (fxbit-field FieldFlags 0 4))
           (LockRule (if (fxbit-set? FieldFlags 4)  'Lock 'NoLock))
           (UpdateRule (fxbit-field FieldFlags 5 7)))
       (let ((field (make-Index/Data index-fu data-fu AccessType LockRule UpdateRule)))
         (aml-define-fields! current-var field field*)))
     #f]

    [('DataRegion ('name . path) SignatureString OemIDString OemTableIDString)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((SignatureString (->string (aml-eval* SignatureString current-var env)))
             (OemIDString (->string (aml-eval* OemIDString current-var env)))
             (OemTableIDString (->string (aml-eval* OemTableIDString current-var env))))
         ;; TODO: Look up the location of the table
         (let ((RegionSpace 'TODO)
               (RegionOffset 'TODO)
               (RegionLen 'TODO))
           (let ((region (make-DataTableRegion name #f #f
                                               RegionSpace RegionOffset RegionLen
                                               SignatureString OemIDString OemTableIDString)))
             (var-add-child! path region)
             region))))]

    ;;; Buffer Fields

    [('CreateBitField SourceBuffer BitIndex ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((SourceBuffer (->buffer (aml-eval* SourceBuffer current-var env)))
             (BitIndex (->integer (aml-eval* BitIndex current-var env))))
         (let ((field (make-BufferField name #f #f SourceBuffer BitIndex 1)))
           (var-add-child! path field))))
     #f]

    [('CreateField SourceBuffer BitIndex NumBits ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((SourceBuffer (->buffer (aml-eval* SourceBuffer current-var env)))
             (BitIndex (->integer (aml-eval* BitIndex current-var env)))
             (NumBits (->integer (aml-eval* NumBits current-var env))))
         (let ((field (make-BufferField name #f #f SourceBuffer BitIndex NumBits)))
           (var-add-child! path field))))
     #f]

    [((and cmd (or 'CreateQWordField 'CreateDWordField 'CreateWordField 'CreateByteField))
      SourceBuffer ByteIndex ('name . path))
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((SourceBuffer (->buffer (aml-eval* SourceBuffer current-var env)))
             (ByteIndex (->integer (aml-eval* ByteIndex current-var env))))
         (let ((field (make-BufferField name #f #f SourceBuffer
                                        (fx* ByteIndex 8)
                                        (case cmd
                                          ((CreateByteField) 8)
                                          ((CreateWordField) 16)
                                          ((CreateDWordField) 32)
                                          ((CreateQWordField) 64)))))
           (var-add-child! path field))))
     #f]

    ;;; Synchronization

    [('Acquire ('name . path) timeout)
     (let ((timeout (->integer (aml-eval* timeout current-var env))))
       (cond ((aml-resolve current-var path) =>
              (lambda (mtx)
                (cond ((Mutex? mtx)
                       (aml-mutex-acquire! mtx 'aml timeout))
                      (else
                       (log/warn "Attempt to acquire a non-mutex object")
                       0))))
             (else
              (log/warn "Attempt to acquire an unbound mutex")
              0)))]
    ;; TODO: Event

    [('Mutex ('name . path) SyncLevel)
     (let-values ([(path name) (aml-resolve+split current-var path)])
       (let ((mutex (make-aml-mutex name (fxbit-field SyncLevel 0 4))))
         (var-add-child! path mutex)))
     #f]

    ;; TODO: Notify
    [('Release ('name . path))
     (cond ((aml-resolve current-var path) =>
            (lambda (mtx)
              (cond ((Mutex? mtx)
                     (aml-mutex-release! mtx 'aml))
                    (else
                     (log/warn "Attempt to release a non-mutex object")
                     0))))
           (else
            (log/warn "Attempt to release an unbound mutex")
            0))]
    ;; TODO: Reset
    ;; TODO: Signal
    ;; TODO: Wait

    ;;; Object references

    [('CondRefOf ('name . path) target)
     (let ((v (cond ((aml-resolve current-var path) =>
                     (lambda (var)
                       ;; FIXME: it's weird to do -Object here.
                       ;; Maybe ->integer etc should do it?
                       (if (Name? var)
                           (Name-Object var)
                           var)))
                    (else 0))))
       (aml-set! target v current-var env)
       v)]
    ;; TODO: CondRefOf
    ;; TODO: DerefOf
    [('DerefOf Source)
     (let ((v (aml-eval* Source current-var env)))
       (cond
         ((ObjectReference? v)
          (let ((obj (ObjectReference-Source v))
                (idx (ObjectReference-Index v)))
            (cond ((vector? obj) (vector-ref obj idx))
                  ((bytevector? obj) (bytevector-u8-ref obj idx))
                  ((string? obj) (string-ref obj idx))
                  (else
                   (display "Bad DerefOf source: ")
                   (write v)
                   (newline)
                   #f))))
         (else
          (display "Bad DerefOf source: ")
          (write v)
          (newline)
          #f
          )))]
    ;; TODO: RefOf

    ;;; Integer arithmetic

    [('Add e0 e1 name)
     (let* ((v0 (->integer (aml-eval* e0 current-var env)))
            (v1 (->integer (aml-eval* e1 current-var env)))
            (v (->integer (+ v0 v1))))
       (aml-set! name v current-var env)
       v)]
    [('And e0 e1 name)
     (let* ((v0 (->integer (aml-eval* e0 current-var env)))
            (v1 (->integer (aml-eval* e1 current-var env)))
            (v (->integer (bitwise-and v0 v1))))
       (aml-set! name v current-var env)
       v)]
    (('Decrement name)
     (let* ((v0 (->integer (aml-eval* name current-var env)))
            (v (->integer (- v0 1))))
       (aml-set! name v current-var env)
       v))
    ;; TODO: Divide
    ;; TODO: FindSetLeftBit
    ;; TODO: FindSetRightBit
    (('Increment name)
     (let* ((v0 (->integer (aml-eval* name current-var env)))
            (v (->integer (+ v0 1))))
       (aml-set! name v current-var env)
       v))
    ;; TODO: Mod
    ;; TODO: Multiply
    ;; TODO: NAnd
    ;; TODO: NOr
    ;; TODO: Not
    [('Or e0 e1 name)
     (let* ((v0 (->integer (aml-eval* e0 current-var env)))
            (v1 (->integer (aml-eval* e1 current-var env)))
            (v (->integer (bitwise-ior v0 v1))))
       (aml-set! name v current-var env)
       v)]
    [('ShiftLeft e0 e1 name)
     (let* ((v0 (->integer (aml-eval* e0 current-var env)))
            (v1 (->integer (aml-eval* e1 current-var env)))
            (v (->integer (if (>= v1 64)
                              0
                              (bitwise-arithmetic-shift-left v0 v1)))))
       (aml-set! name v current-var env)
       v)]
    [('ShiftRight e0 e1 name)
     (let* ((v0 (->integer (aml-eval* e0 current-var env)))
            (v1 (->integer (aml-eval* e1 current-var env)))
            (v (->integer (if (>= v1 64)
                              0
                              (bitwise-bit-field
                               (bitwise-arithmetic-shift-right v0 v1)
                               0 64)))))
       (aml-set! name v current-var env)
       v)]
    ;; TODO: Subtract
    ;; TODO: Xor

    ;;; Logical operators

    ;; TODO: LAnd
    [('LEqual e0 e1)
     ;; FIXME: Type conversions
     (let* ((v0 (aml-eval* e0 current-var env))
            (v1 (aml-eval* e1 current-var env)))
       (if (equal? v0 v1) 1 0))]
    [((or 'LGreater 'LGreaterEqual 'LLess 'LLessEqual) e0 e1)
     (let* ((v0 (->integer (aml-eval* e0 current-var env)))
            (v1 (->integer (aml-eval* e1 current-var env))))
       (let ((<> (case (car term)
                   ((LGreater) >)
                   ((LGreaterEqual) >=)
                   ((LLess) <)
                   (else <=))))
         (if (<> v0 v1) 1 0)))]
    ;; TODO: LNot
    [('LNot e)
     (let ((v (->integer (aml-eval* e current-var env))))
       (if (eqv? 0 v) 1 0))]
    ;; TODO: LNotEqual
    ;; TODO: LOr

    ;;; Method execution control

    [('While e body)
     ;; FIXME: run in a new env where Break and Continue will exit
     ;; from this loop
     (let lp ()
       (yield-current-task)
       (let ((v (->integer (aml-eval* e current-var env))))
         (cond ((eqv? v 0)
                #f)
               (else
                (aml-eval* body current-var env)
                (lp)))))]

    ;; TODO: Break
    ;; TODO: Continue
    [('Breakpoint) #f]

    [('Fatal Type Code Arg)
     (let ((arg (aml-eval* Arg current-var env)))
       ;; TODO: Shut down the system.
       (log/critical "Fatal ACPI error. Type=" Type " Code=" Code " Arg=" arg)
       (error 'aml-eval "Fatal ACPI error" Type Code arg))]
    [('If test consequent)
     ;; XXX: Else is handled inside begin.
     (if (not (eqv? 0 (->integer (aml-eval* test current-var env))))
         (aml-eval* consequent current-var env)
         #f)]
    [('Noop) #f]
    [('Return e)
     (let ((return (env-return env))
           (v (aml-eval* e current-var env)))
       ;; XXX: return outside methods?
       (if return
           (return v)
           v))]
    [('Sleep MsecTime)
     (let ((msec (->integer (aml-eval* MsecTime current-var env))))
       (sleep (/ msec 1e3)))]
    ;; TODO: Stall

    ;;; Data type conversion and manipulation

    ;; TODO: Concatenate
    ;; TODO: CopyObject
    ['Debug (make-DebugObject)]
    ;; TODO: FromBCD
    [('Index Source Index Destination)
     (let* ((source (aml-eval* Source current-var env))
            (index (->integer (aml-eval* Index current-var env)))
            (v (make-ObjectReference source index)))
       ;; FIXME: check that the index is valid
       (unless (or (bytevector? source)
                   (string? source)
                   (vector? source))
         (display "Index with bad source: ")
         (write source)
         (newline))
       (aml-set! Destination v current-var env)
       v)]
    ;; TODO: Match
    ;; TODO: Mid
    ;; TODO: ObjectType
    ;; TODO: SizeOf
    [('Store expr destination)
     (let ((v (aml-eval* expr current-var env)))
       (aml-set! destination v current-var env)
       v)]
    ;; TODO: Timer
    ;; TODO: ToBCD
    ;; TODO: ToBuffer
    ;; TODO: ToDecimalString
    ;; TODO: ToHexString
    ;; TODO: ToInteger
    ;; TODO: ToString
    ;; TODO: ToUUID
    ;; TODO: Unicode

    [('Call ('name . path) . expr*)
     (when (> (env-stack-depth env) 100)
       (error 'aml-eval "Stack overflow"))
     (yield-current-task)
     (call/cc
       (lambda (return)
         (let* ((proc (aml-resolve current-var path))
                (arg* (map (lambda (expr) ;XXX: map-in-order?
                             (aml-eval* expr current-var env))
                           expr*))
                (new-env (make-env arg* (fx+ (env-stack-depth env) 1) return
                                   (env-delays env)
                                   (env-limit env))))
           (when *debug*
             (log/debug "AML call " (if (Method? proc) (var->path proc) proc) " with args " arg*))
           (cond ((Method? proc)
                  ;; FIXME: Do something for dynamic creation of
                  ;; variables. It does not need to be fast at all.
                  (aml-eval* (Method-body proc) proc new-env))
                 (else
                  ;; (log/error "AML call on bad object " proc " found from "
                  ;;            current-var "/" path)
                  ;; #f
                  proc)))))]

    ;;; Constants

    ['Revision AML-revision]

    ;;; Control method objects

    [('Arg n)
     (vector-ref (env-args env) n)]
    [('Local n)
     (vector-ref (env-locals env) n)]

    [_
     ;; FIXME: maybe don't error out yet?
     (log/error "Unimplemented AML term: " term)
     (error 'aml-eval "Unimplemented AML term" term)
     #f]))

(define (aml-eval current-var term)
  (define delays '())
  (define limit 100000)
  (let ((env (make-env '() 0 #f (lambda (delay)
                                  (set! delays (cons delay delays)))
                       (lambda ()
                         (set! limit (- limit 1))
                         (when (< limit 0)
                           (error 'aml-eval "Evaluation time limit exceeded"
                                  current-var term)))))
        (dynamic-var (make-Name 'Root #f #f #f)))
    (let ((v (aml-eval* term current-var env)))
      (for-each (lambda (proc) (proc))
                delays)
      v)))

(define (aml-load-ports dsdt-ports)
  (define tmproot (make-predefined-acpi-root))
  ;; Do an initial dry run over the code to find all method
  ;; declarations. Their arities are needed by the parser.
  (aml-dryrun `(Mutex (name "_GL") 0) tmproot)
  (aml-dryrun `(Name (name "_OS") "") tmproot)
  (aml-dryrun `(Name (name "_REV") 6) tmproot)
  (aml-dryrun `(Method (name "_OSI") 1 (begin (Return #x0))) tmproot)
  (for-each (lambda (port)
              (set-port-position! port 0)
              (acpi-get-table-header port)
              (do ()
                  ((port-eof? port))
                (let ((term (aml-get-term port #f #t)))
                  ;; (pretty-print term)
                  (aml-dryrun term tmproot))))
            dsdt-ports)
  ;; Now that tmproot knows all methods, the parser should work
  ;; properly.
  #;(print-aml-tree (current-output-port) tmproot)
  (let ((root (make-predefined-acpi-root)))
    ;; TODO: Implement the global lock semantics.
    (aml-eval root `(Mutex (name "_GL") 0))
    (aml-eval root `(Method (name "_OSI") 1
                            (begin
                              (Return #x0))))
    (aml-eval root `(Name (name "_OS")
                          ,(string-append "Loko Scheme " (loko-version))))
    (aml-eval root `(Name (name "_REV") 6)) ;ACPI revision
    (let ((aml-code (aml-get-terms-from-ports dsdt-ports tmproot)))
      ;; (pretty-print aml-code)
      (aml-eval root `(begin ,@aml-code))
      root)))

)
