(defpackage cl-bson.encode
  (:use cl)
  (:import-from babel
                string-to-octets)
  (:import-from cl-bson.readtable
                bson-syntax)
  (:import-from cl-bson.types
                <binary-data>
                <document>
                <javascript>
                <mongo-timestamp>
                <object-id>
                <regex>
                add-element
                code
                elements
                mongo-time
                octets
                options
                pattern
                scope
                subtype)
  (:import-from cl-intbytes
                int32->octets
                int64->octets)
  (:import-from fast-io
                fast-write-byte
                fast-write-sequence
                with-fast-output)
  (:import-from ieee-floats
                encode-float64)
  (:import-from local-time
                timestamp
                timestamp-millisecond
                timestamp-to-unix)
  (:import-from named-readtables
                in-readtable)
  (:export encode)
  (:documentation "This package defines the main function (@c(#'encode)) for actually converting a @c(<document>) object to @c(octets-array) and many helper functions for internal use. @c(*bson-out*) gets bound to a @c(fast-io:output-buffer) in the first to of @c(#'encode)."))
(in-package cl-bson.encode)
(in-readtable bson-syntax)

(defparameter *bson-out* nil "Special var that gets bound to @c(fast-io:output-buffer) on every @c(#'encode) call.")

(defgeneric encode (document)
  (:documentation "Encodes a given @cl:param(document) into an @c(octets-array) following the @link[uri=\"http://bsonspec.org/spec.html\"](BSON specification).")
  (:method ((document <document>))
    (with-fast-output (*bson-out*)
      (encode-document (elements document)))))

(defgeneric encode-key-value (key value)
  (:documentation "Main helper generic function for doing the actual work of encoding @cl:param(key) @cl:param(value) pairs. Most of the method implementations have the following skeleton:

@code[lang=lisp]((defmethod encode-key-value (key (value SOME-TYPE))
  ;; Writes the BSON-BYTE-SPECIFIER
  (fast-write-byte BSON-BYTE-SPECIFIER *bson-out*)
  ;; Encodes the key string in C-style
  (encode-cstring key)
  ;; Encodes the value with a custom function
  (encode-the-value-somehow value))
)

In general, the functions relative to @c(encode-the-value-somehow) converts the @cl:param(value) into an @c(octets-array) and then call @c(#'fast-io:fast-write-sequence)."))

(defun encode-document (elements)
  "Goes through all the elements of the @cl:param(elements) hash-table using @cl:spec(maphash) and encode the @c(key) @c(value) pairs with @c(#'encode-key-value), writing to the bound @c(*bson-out*)."
  (let ((output-doc (with-fast-output (*bson-out*)
                      (fast-write-sequence (int32->octets 0) *bson-out*)
                      (maphash (lambda (key value)
                                 (encode-key-value key value))
                               elements)
                      (fast-write-byte #x00 *bson-out*))))
    (fast-write-sequence (replace output-doc (int32->octets (length output-doc))) *bson-out*)))

(defun encode-cstring (value)
  "Coerces @cl:param(value) to string using the @cl:spec(string) function and transforms it to an @c(octets-array) using @c((babel:string-to-octets (string value) :encoding :utf-8))."
  (fast-write-sequence (string-to-octets (string value) :encoding :utf-8) *bson-out*)
  (fast-write-byte #x00 *bson-out*))

(defmethod encode-key-value (key (value float))
  "Encode a given @c(single-float) or @c(double-float) @cl:param(value) and encodes it as an 8 bytes @c(double-float), even if it originally is a @c(single-float)."
  (fast-write-byte #x01 *bson-out*)
  (encode-cstring key)
  (fast-write-sequence (int64->octets (encode-float64 value)) *bson-out*))

(defun encode-string (value)
  "Encodes a given @cl:param(value) into an array with 4 bytes for length, the string @cl:param(value) encoded with @c((string-to-octets value :encoding :utf-8)) and a null byte (0) at the end."
  (let ((string-octets (string-to-octets value :encoding :utf-8)))
    (fast-write-sequence (int32->octets (1+ (length string-octets))) *bson-out*)
    (fast-write-sequence string-octets *bson-out*)
    (fast-write-byte #x00 *bson-out*)))

(defmethod encode-key-value (key (value string))
  "Encodes an UTF-8 string @cl:param(value)."
  (fast-write-byte #x02 *bson-out*)
  (encode-cstring key)
  (encode-string value))

(defmethod encode-key-value (key (value symbol))
  "Coerces @cl:param(value) to string using the @cl:param(string) and call @c(#'encode-key-value) for strings."
  (encode-key-value key (string value)))

(defmethod encode-key-value (key (value <document>))
  "Calls @c(#'encode-document) for encoding the embedded document."
  (fast-write-byte #x03 *bson-out*)
  (encode-cstring key)
  (encode-document (elements value)))

(defun encode-document-array (key value)
  "Gets @cl:param(value) sequence and encodes it as a document. Each element gets a index as a key to create the document."
  (fast-write-byte #x04 *bson-out*)
  (encode-cstring key)
  (let ((kv-array-document #d())
        (index -1))
    (map 'nil (lambda (element)
                (add-element kv-array-document (princ-to-string (incf index)) element))
         value)
    (encode-document (elements kv-array-document))))

(defmethod encode-key-value (key (value list))
  "Dispatches to @c(#'encode-document-array)."
  (encode-document-array key value))

(defmethod encode-key-value (key (value vector))
  "Dispatches to @c(#'encode-document-array)."
  (encode-document-array key value))

(defmethod encode-key-value (key (value <binary-data>))
  "Encodes the binary data @c(subtype) and @c(octets) of the given @cl:param(value) (@c(<binary-data>) object)."
  (fast-write-byte #x05 *bson-out*)
  (encode-cstring key)
  (fast-write-sequence (int32->octets (length (octets value))) *bson-out*)
  (fast-write-byte (case (subtype value)
                     (:generic      #x00)
                     (:function     #x01)
                     (:binary-old   #x02)
                     (:uuid-old     #x03)
                     (:uuid         #x04)
                     (:md5          #x05)
                     (:user-defined #x80))
                   *bson-out*)
  (fast-write-sequence (octets value) *bson-out*))

(defmethod encode-key-value (key (value <object-id>))
  "Encodes the given @cl:param(value) (@c(<object-id>) object)."
  (fast-write-byte #x07 *bson-out*)
  (encode-cstring key)
  (fast-write-sequence (octets value) *bson-out*))

(defun encode-boolean (key value)
  "Encodes the given boolean @cl:param(value). Since Common Lisp doesn't differenciate 'false' values from 'null' values, any null value gets 'encoded' as a boolean @cl:spec(nil)."
  (fast-write-byte #x08 *bson-out*)
  (encode-cstring key)
  (fast-write-byte (if value #x01 #x00) *bson-out*))

(defmethod encode-key-value (key (value (eql nil)))
  "Dispatches to @c(#'encode-boolean)."
  (encode-boolean key value))

(defmethod encode-key-value (key (value (eql t)))
  "Dispatches to @c(#'encode-boolean)."
  (encode-boolean key value))

(defmethod encode-key-value (key (value timestamp))
  "Encodes the given @link[name=\"https://common-lisp.net/project/local-time/manual.html#Types\"](@c(local-time:timestamp)) as a 8 bytes integer representing the milisseconds since the Unix epoch. Since the @(local-time:timestamp) stores up to nanoseconds, the given @cl:param(value) loses precision, getting coerced to milliseconds. When decoded and reencoded, the precision stay the same (milliseconds)."
  (fast-write-byte #x09 *bson-out*)
  (encode-cstring key)
  (let ((timestamp-octets (int64->octets (+ (* (timestamp-to-unix value) 1000)
                                            (timestamp-millisecond value)))))
    (fast-write-sequence timestamp-octets *bson-out*)))

(defmethod encode-key-value (key (value <regex>))
  "Encodes the given @cl:param(value) (@c(<regex>) object)."
  (fast-write-byte #x0B *bson-out*)
  (encode-cstring key)
  (encode-cstring (pattern value))
  (encode-cstring (options value)))

(defmethod encode-key-value (key (value <javascript>))
  "Encodes the given @c(<javascript>) @cl:param(value). If the @c(scope) attribute of @cl:param(value) is not nil, encodes @cl:param(value) as a \"Code with scope\" object; otherwise, enocodes it as a \"JavaScript code\" object."
  (if (slot-boundp value 'scope)
      (progn
        (fast-write-byte #x0F *bson-out*)
        (encode-cstring key)
        (let ((output-doc (with-fast-output (*bson-out*)
                            (fast-write-byte (int32->octets 0) *bson-out*)
                            (encode-string (code value)) ;; initform?? unbound?
                            (encode-document (elements (scope value))))))
          (fast-write-sequence (replace output-doc (int32->octets (length output-doc)))
                               *bson-out*)))
      (progn
        (fast-write-byte #x0D *bson-out*)
        (encode-cstring key)
        (encode-string (code value)))))

(defun encode-int32 (key value)
  "Encodes the given @cl:param(value) as a 4 bytes @c(octets-array) sequence."
  (fast-write-byte #x10 *bson-out*)
  (encode-cstring key)
  (fast-write-sequence (int32->octets value) *bson-out*))

(defun encode-int64 (key value)
  "Encodes the given value as a 8 bytes @c(octets-array) sequence."
  (fast-write-byte #x12 *bson-out*)
  (encode-cstring key)
  (fast-write-sequence (int64->octets value) *bson-out*))

(defmethod encode-key-value (key (value integer))
  "Checks the @cl:spec(integer-length) of the given @cl:param(value) and dispatches to the correct function (or it throws an @cl:spec(error))."
  (let ((int-length (integer-length value)))
    (cond ((<= int-length 32)
           (encode-int32 key value))
          ((<= int-length 64)
           (encode-int64 key value))
          (t (error "Integer value (~s) too big: ~s bytes" value int-length)))))

(defmethod encode-key-value (key (value <mongo-timestamp>))
  "Encodes the given @cl:param(value) (@c(<mongo-timestamp>) object) as an @c(octets-array)."
  (fast-write-byte #x11 *bson-out*)
  (encode-cstring key)
  (fast-write-sequence (mongo-time value) *bson-out*))
