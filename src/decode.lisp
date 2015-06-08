(defpackage cl-bson.decode
  (:use cl)
  (:import-from arrow-macros
                ->)
  (:import-from babel
                octets-to-string)
  (:import-from cl-bson.types
                <binary-data>
                <document>
                <javascript>
                <mongo-timestamp>
                <object-id>
                <regex>
                add-element
                elements)
  (:import-from cl-intbytes
                octets->int32
                octets->int64)
  (:import-from ieee-floats
                decode-float64)
  (:import-from fast-io
                fast-read-byte
                fast-read-sequence
                fast-write-byte
                make-octet-vector
                with-fast-input
                with-fast-output)
  (:import-from local-time
                nsec-of
                unix-to-timestamp)
  (:import-from rutil
                ht-vals)
  (:export decode
           *bson-sequence-type*)
  (:documentation "This package defines the main function (@c(#'decode)) for converting an @c(octets-array) to BSON @c(<document>)s many helper functions for internal use."))
(in-package cl-bson.decode)

(defvar *bson-in* nil "Special var that gets bound to @c(fast-io:input-buffer) on every @c(#'decode) call. Many of the @c(#'decode-*) functions read from @c(*bson-in*) (destructively) and returns the value to be added to @c(*doc-out*).")
(defvar *doc-out* nil "Special variable that holds the current @c(<document>) on every @c(#'decode-document) call.")

(defun read-until-null ()
  "Reads from @c(*bson-in*) until it finds a null (0) byte."
  (with-fast-output (out)
    (loop for byte = (fast-read-byte *bson-in*) then (fast-read-byte *bson-in*)
       until (= byte 0)
       do (fast-write-byte byte out))))

(defun read-n-bytes (n)
  "Reads @cl:param(n) bytes from @c(*bson-in*)."
  (let ((out (make-octet-vector n)))
    (fast-read-sequence out *bson-in*)
    out))

(defun decode-cstring ()
  "Decodes a null terminated UTF-8 encoded string from @c(*bson-in*)."
  (octets-to-string (read-until-null) :encoding :utf-8))

(defun decode-string ()
  "Decodes a null terminated UTF-8 encoded string from @c(*bson-in*)."
  (fast-read-sequence (make-octet-vector 4) *bson-in*)
  (octets-to-string (read-until-null) :encoding :utf-8))

(defun decode-double ()
  "Decodes an 8 bytes double from @c(*bson-in*)."
  (-> (read-n-bytes 8)
    octets->int64
    decode-float64))

(defun decode-int32 ()
  "Decodes a 4 bytes integer from @c(*bson-in*)."
  (octets->int32 (read-n-bytes 4)))

(defun decode-object-id ()
  "Decodes a 12 bytes @c(<object-id>) from @c(*bson-in*)."
  (make-instance '<object-id> :octets (read-n-bytes 12)))

(defun decode-int64 ()
  "Decodes an 8 bytes integer from @c(*bson-in*)."
  (octets->int64 (read-n-bytes 8)))

(defun decode-mongo-timestamp ()
  "Decodes an 8 bytes @c(<mongo-timestamp>)."
  (make-instance '<mongo-timestamp>
                 :mongo-time (read-n-bytes 8)))

(defun decode-boolean ()
  "Decodes a byte from @c(*bson-in*) as boolean. Since Common Lisp doesn't differenciate 'null' values from 'false' values, it can correctly decode a 'null value', but it gets encoded back as a 'nil' value."
  (if (= 0 (fast-read-byte *bson-in*))
      nil
      t))

(defun decode-document ()
  "Binds @c(*doc-out*) to a new document and call @c(#'decode-key-value) to decode values withing the new bound @c(*doc-out*). Returns the decoded @c(*doc-out*) @c(<document>)."
  (let ((*doc-out* (make-instance '<document>)))
    (fast-read-sequence (make-octet-vector 4) *bson-in*)
    (decode-key-value)
    *doc-out*))

(defun decode-regex ()
  "Decodes 2 strings with @c(#'decode-cstring) and creates an instance of @c(<regex>)."
  (make-instance '<regex>
                 :pattern (decode-cstring)
                 :options (decode-cstring)))

(defun decode-javascript ()
  "Decodes a string from @c(*bson-in*) and creates an instance of @c(<javascript>)."
  (make-instance '<javascript> :code (decode-string)))

(defun decode-javascript-w/scope ()
  "Decodes a string and a @c(<document>) from @c(*bson-in*) and creates an instance of @c(<javascript>) with the @c(scope) field bound to the decoded @c(<document>)."
  (fast-read-byte *bson-in*)
  (let ((code (decode-string))
        (scope (decode-document)))
    (make-instance '<javascript>
                   :code code
                   :scope scope)))

(defun decode-key-value ()
  "Main function that dispatches to the correct @c(#'decode-*) function. It first decodes the element type, than dispatches to the function that decodes the value type and finally adds the decoded @c(key) @c(value) pair to @c(*doc-out*). If it finds an invalid element type, it throws an @cl:spec(error)."
  (let* ((element-type (fast-read-byte *bson-in*)))
    (if (= element-type #x00)
        *doc-out*
        (let ((key (decode-cstring))
              (value (case element-type
                       (#x01 (decode-double))
                       (#x02 (decode-string))
                       (#x03 (decode-document))
                       (#x04 (decode-sequence))
                       (#x05 (decode-binary))
                       (#x07 (decode-object-id))
                       (#x08 (decode-boolean))
                       (#x09 (decode-timestamp))
                       (#x0A (decode-boolean))
                       (#x0B (decode-regex))
                       (#x0D (decode-javascript))
                       (#x0F (decode-javascript-w/scope))
                       (#x10 (decode-int32))
                       (#x11 (decode-mongo-timestamp))
                       (#x12 (decode-int64))
                       (otherwise (error "Bad encoded BSON.")))))
          (add-element *doc-out* key value)
          (decode-key-value)))))

(defun decode-binary ()
  "Reads an @c(octets-array) and instanciates a @c(<binary-data>) object with the read data."
  (let* ((n-bytes (fast-read-byte *bson-in*))
         (subtype (case (fast-read-byte *bson-in*)
                    (#x00 :generic)
                    (#x01 :function)
                    (#x04 :uuid)
                    (#x05 :md5)
                    (#x80 :user-defined)))
         (octets (read-n-bytes n-bytes)))
    (make-instance '<binary-data> :subtype subtype :octets octets)))

(defparameter *bson-sequence-type* 'vector "Special variable that holds the kind of output to return when decoding a BSON array.")

(defun decode-sequence ()
  "Decodes a @c(<document>) and extract only the values. Depending on the value of @c(*bson-sequence-type), returns the values as a list or as a vector."
  (let* ((sequence-doc (decode-document))
         (values (ht-vals (elements sequence-doc))))
    (case *bson-sequence-type*
      (list values)
      ((array vector) (coerce values 'vector)))))

(defun decode (octets-array)
  "Main entry point to decode a given array. It performs the first binding of @c(*doc-out*) and call @c(#'decode-document)."
  (with-fast-input (*bson-in* octets-array)
    (decode-document)))

(defun decode-timestamp ()
  "Decodes a @link[uri=\"https://common-lisp.net/project/local-time/manual.html#Types\"](@c(local-time:timestamp)) with milliseconds precision."
  (let* ((time (decode-int64))
         (milisseconds (rem time 1000))
         (seconds (round (/ (- time milisseconds) 1000)))
         (timestamp (unix-to-timestamp seconds)))
    (setf (nsec-of timestamp) (* milisseconds 1000000))
    timestamp))
