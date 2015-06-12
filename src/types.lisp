(defpackage cl-bson.types
  (:use cl)
  (:import-from arrow-macros
                ->)
  (:import-from babel
                string-to-octets)
  (:import-from cl-intbytes
                int32->octets
                octets->int32)
  (:import-from fast-io
                fast-write-byte
                fast-write-sequence
                with-fast-output)
  (:import-from local-time
                now
                timestamp
                timestamp-to-unix
                unix-to-timestamp)
  (:import-from rutil
                get#
                group
                ht-keys
                rem#)
  (:import-from trivial-shell
                os-process-id)
  (:export <binary-data>
           <document>
           <javascript>
           <mongo-timestamp>
           <object-id>
           <regex>
           *allowed-regex-options*
           add-element
           code
           elements
           get-element
           get-timestamp
           keys
           make-document
           mongo-time
           octet
           octets
           octets-array
           options
           pattern
           remove-element
           scope
           subtype
           str
           string->object-id
           remove-element)
  (:documentation ""))
(in-package cl-bson.types)

(deftype octet ()
  "Equivalent to @c'(UNSIGNED-BYTE 8)). A 8-bit byte."
  '(unsigned-byte 8))

(deftype octets-array (&optional (size '*))
  "A @c(SIMPLE-ARRAY) of @c('(UNSIGNED-BYTE 8))."
  `(simple-array octet (,size)))

(defparameter *object-id-counter* (random #.(expt 2 24))
  "3-byte size integer counter, starting with a random value: @c((random (expt 2 24))) .")

(defun increment-id-counter ()
  "Increments @c(*OBJECT-ID-COUNTER*) up to @c((1- (expt 2 24))). When pass that, it \"overflows\" back to 0."
  (setf *object-id-counter*
        (rem (1+ *object-id-counter*) #.(expt 2 24))))

#+nil
(defun group (n sequence)
  "Waiting for the @link[name=\"https://github.com/vseloved/rutils/pull/22\"](PR) to be accepted."
  (declare (integer n))
  (when (zerop n)
    (error "Group length N shouldn't be zero."))
  (labels ((rec (src acc)
              (let ((rest (nthcdr n src)))
                (if (consp rest)
                    (rec rest (cons (subseq src 0 n) acc))
                    (nreverse (cons src acc))))))
    (when sequence
     (etypecase sequence
       (list (rec sequence nil))
       (sequence
        (do ((i 0 (+ i n))
             (len (length sequence))
             (acc nil))
            ((>= (+ i n) len)
             (nreverse (push (subseq sequence i) acc)))

          (push (subseq sequence i (+ i n)) acc)))))))

#+nil
(defun os-process-id ()
  "Waiting for the @link[uri=\"https://github.com/gwkkwg/trivial-shell/pull/9\"](@c(PR)) to be accepted so that I can remove this function."
  #+clisp (system::process-id)
  #+(and lispworks (or unix windows)) (system::getpid)
  #+(and sbcl unix) (sb-unix:unix-getpid)
  #+(and cmu unix) (unix:unix-getpid)
  #+openmcl (ccl::getpid)
  #-(or clisp (and lispworks (or unix windows)) (and sbcl unix) (and cmu unix) (and openmcl unix) openmcl)
  (error "Impossible to determine the PID"))

(defun generate-object-id ()
  "Generates a fresh 12 bytes @c(OCTETS-ARRAY) for an @c(<object-id>).

A typical array looks like:
@code[lang=lisp](* (generate-object-id)
; => #(34 250 116 85 97 110 100 119 99 7 0 211)
)
...where:
@begin(list)
@item(@c(#(34 250 116 85)) is the Unix timestamp of when it was generated. See it with:
@code[lang=lisp](* (get-timestamp (make-instance '<object-id> :octets *))
; => @2015-06-07T23:12:50.000000-03:00
))
@item(@c(#(97 110 100)) is the machine identifier. See it with:
@code[lang=lisp](* (babel:octets-to-string (subseq (generate-object-id) 4 7))
; => \"and\" ;; three first letters of \"andreh\"
))
@item(@c(#c(119 99)) is the PID. See it with:
@code[lang=lisp](* (intbytes:octets->int (subseq (generate-object-id) 7 9) 2)
; => 25463
))
@item(@c(#(10 0 211)) is the counter. See it with:
@code[lang=lisp](* *object-id-counter*
; => 13828114
* (intbytes:octets->uint (subseq (generate-object-id) 9 12) 3)
; => 13828115
* *object-id-counter*
; => 13828115
))
@end(list)"
  (let ((unix-epoch (-> (now)
                      timestamp-to-unix
                      int32->octets))
        (machine-identifier (-> (machine-instance)
                              (string-to-octets :encoding :utf-8)
                              (subseq 0 3)))
        (pid (-> (os-process-id)
               int32->octets
               (subseq 0 2)))
        (counter (-> (increment-id-counter)
                   int32->octets
                   (subseq 0 3))))
    (with-fast-output (out)
      (fast-write-sequence unix-epoch out)
      (fast-write-sequence machine-identifier out)
      (fast-write-sequence pid out)
      (fast-write-sequence counter out))))

(defclass <object-id> ()
  ((octets :accessor octets
           :initarg :octets
           :initform (generate-object-id)
           :type octets-array
           :documentation "Array of actual OCTETS-ARRAY that represent the @link[uri=\"http://docs.mongodb.org/manual/reference/object-id/\"](MongoDB ObjectId). Value generated by @c(#'generate-object-id)."))
  (:documentation "This class is a container for the actual OCTETS-ARRAY that represent the MongoDB ObjectId.

The structure of the array is:
@begin(enum)
@item(a 4-byte value representing the seconds since the Unix epoch.)
@item(a 3-byte machine identifier.)
@item(a 2-byte process id)
@item(a 3-byte counter, starting with a random value.)
@end(enum)

Check the @link[uri=\"http://docs.mongodb.org/manual/reference/bson-types/#objectid\"](reference) for more info."))

(defgeneric str (object-id)
  (:documentation "The hexadecimal string representation of the given @cl:param(object-id). Method from the @link[uri=\"http://docs.mongodb.org/manual/reference/object-id/#core-object-id-class\"](reference).

@code[lang=lisp](* (str (make-instance '<object-id>))
; => \"35F97455616E6477630600D3\"
)")
  (:method ((object-id <object-id>))
    (format nil "~{~2,'0X~}" (coerce (octets object-id) 'list))))

(defun string->object-id (string)
  "Utility for instanciating an @c(<object-id>) from a given @cl:param(string). Useful for fetching documents with parameters received from HTTP requests.

@code[lang=lisp](;; without the custom pprinter:
* (string->object-id \"35F97455616E6477630600D3\")
; => #<<OBJECT-ID> {1008C48CE3}>

;; with the custom pprinter:
* (enable-printers)
; => NIL
* (string->object-id \"35F97455616E6477630600D3\")
; => #i(35F97455616E6477630600D3)
)"
  (make-instance '<object-id>
                 :octets (with-fast-output (out)
                           (dolist (n (group 2 string) out)
                             (fast-write-byte (parse-integer n :radix 16) out)))))

(defgeneric get-timestamp (object-id)
  (:documentation "Returns the timestamp portion of @cl:param(object-id) as a @c(local-time:timestamp). The @link[uri=\"https://common-lisp.net/project/local-time/manual.html#Types\"](@c(local-time:timestamp)) is used to represent the MongoDB @link[uri=\"http://docs.mongodb.org/manual/reference/bson-types/#date\"](Date).")
 (:method ((object-id <object-id>))
   (-> (octets object-id)
     (subseq 0 4)
     octets->int32
     unix-to-timestamp)))

(defclass <regex> ()
  ((pattern :accessor pattern
            :initarg :pattern
            :initform ""
            :type string
            :documentation "This slot holds the actual regex pattern as a @i(string).")
   (options :accessor options
            :initarg :options
            :initform ""
            :type string
            :documentation "This slot holds the options of the @c(<regex>) object as an alphabetically sorted @i(string). Options are identified by by characters. Valid options are: 'i' for case insensitive matching, 'm' for multiline matching, 'x' for verbose mode, 'l' to make \\w, \\W, etc. locale dependent, 's' for dotall mode ('.' matches everything), and 'u' to make \\w, \\W, etc. match unicode"))
  (:documentation "This class is used to represent regexps in the BSON document."))

(defvar *allowed-regex-options* '(#\i #\l #\m #\s #\u #\w #\x)
  "List of charaters allowed in the @c(options) slot of a @c(<regex>) object.")

(defgeneric (setf options) (regex options)
  (:documentation "Checks if the @cl:param(options) string contains any invalid characters and, if not, sorts them alphabetically before @c(setf)ing. Otherwise, throws an @cl:spec(error).")
  (:method ((options string) (regex <regex>))
    (unless (subsetp (coerce options 'list)
                     *allowed-regex-options*)
      (error "Invalid regex option characters in: ~s. Allowed options are: ~s"
             (options regex) *allowed-regex-options*))
    (setf (slot-value regex 'options)
          (sort (remove-duplicates options :test #'char=) #'char<))))

(defmethod initialize-instance :after ((regex <regex>) &key)
  "@spec(sort) the @cl:param(option) of the @cl:param(regex) through @c((setf (options regex) ...))."
  (setf (options regex) (options regex))
  regex)

(defclass <binary-data> ()
  ((subtype :accessor subtype
            :initarg :subtype
            :initform :generic
            :type keyword
            :documentation "This slot holds a keyword that represents one of the @c(<binary-data>) subtypes. A valid @c(subtype) is any of the following: @c(:generic), @c(:function), @c(:binary-old) (@i(deprecated)), @c(:uuid-old) (@i(deprecated)), @c(:uuid), @c(:md5) or @c(:user-defined).")
   (octets :accessor octets
           :initarg :octets
           :initform (make-array 0 :element-type 'octet)
           :type octets-array
           :documentation "This slot holds the actual binary data."))
  (:documentation "This class is used to represent custom array of bytes in BSON. @c(<binary-data>) values have a @cl:param(subtype). This is used to indicate what kind of data is in the byte array. Subtypes from zero to 127 are predefined or reserved. Subtypes from 128 to 255 are @c(:user-defined)."))

(defmethod (setf subtype) ((subtype symbol) (binary-data <binary-data>))
  (if (member subtype
              '(:generic :function :binary-old :uuid-old :uuid :md5 :user-defined))
      (setf (slot-value binary-data 'subtype) subtype)
      (error "Invalid SUBTYPE for <BINARY-DATA> object: ~s" subtype)))

(defmethod initialize-instance :after ((binary-data <binary-data>) &key)
  "Checks if @cl:param(binary-data) has a valid @cl:param(subtype)."
  (setf (subtype binary-data) (subtype binary-data))
  binary-data)

(defclass <javascript> ()
  ((code :accessor code
         :initarg :code
         :type string
         :documentation "This slot holds JavaScript code as a @i(string).")
   (scope :accessor scope
          :initarg :scope
          :type <document>
          :documentation "This slot holds a @c(<document>) that represents the scope in which the string should be evaluated. The @c(<document>) is a mapping from identifiers to values."))
  (:documentation "This class puts together two BSON types: \"JavaScript code\" and \"Code with scope\". When the @cl:param(scope) slot is @c(nil) (default), a @c(<javascript>) object gets encoded as \"JavaScript code\". When the @cl:param(scope) slot is not @c(nil), @c(<javascript>) gets encoded as \"Code with scope\"."))

(defparameter *mongo-timestamp-counter* (random #.(expt 2 32))
  "4-byte size integer counter, starting with a random value:  @c((random (expt 2 32))).")

(defun increment-mongo-timestamp-counter ()
  "Increments *MONGO-TIMESTAMP-COUNTER* up to @c((1- (expt 2 32))). When pass that, it \"overflows\" back to 0."
  (setf *mongo-timestamp-counter*
        (rem (1+ *mongo-timestamp-counter*) #.(expt 2 32))))

(defun generate-mongo-timestamp ()
  "Generates a fresh 8 bytes @c(octets-array) for a @c(<mongo-timestamp>)."
  (let ((unix-epoch (-> (now)
                      timestamp-to-unix
                      int32->octets))
        (counter (int32->octets (increment-mongo-timestamp-counter))))
    (with-fast-output (out)
      (fast-write-sequence counter out)
      (fast-write-sequence unix-epoch out))))

(defclass <mongo-timestamp> ()
  ((mongo-time :accessor mongo-time
               :initarg :mongo-time
               :initform (generate-mongo-timestamp)
               :type octets-array
               :documentation "Array of actual @c(octets-array) that represent the @link[uri=\"http://docs.mongodb.org/manual/reference/bson-types/#timestamps\"](Mongo Timestamp)."))
  (:documentation "Special @i(internal) type used by MongoDB for replication and sharding. Within a single @c(mongod) instance, @c(<mongo-timestamp>) are always unique.

The structure of the array is:
@begin(enum)
@item(4 bytes are an increment, starting with a random value.)
@item(4 bytes are seconds since the Unix epoch.)
@end(enum)"))

(defclass <document> ()
  ((elements :accessor elements
             :initarg :elements
             :type hash-table
             :initform (make-hash-table :test #'equal)
             :documentation "@c(hash-table) that holds all the the document data."))
  (:documentation "Main class for interacting with MongoDB.

You can instanciate it with @c((make-instance '<document>)), which yields a @c(<document>) with no @c(\"_id\") field; or with @c(#'make-document), which instanciates a @c(<document>) for you with an @c(<object-id>) already."))

(defun make-document (&key (_id (make-instance '<object-id>)))
  "Utility function to easily create @c(<document>)s already with an @c(<object-id). To create an @c(<document>) with an @cl:param(_id) from a string, use:
@code[lang=lisp]((make-document :_id (string->object-id \"my id string\")))."
  (let ((doc (make-instance '<document>)))
    (if _id
        (add-element doc "_id" _id)
        doc)))

(defgeneric add-element (document key value)
  (:documentation "Properly adds a given @cl:param(key)-@cl:param(value) pair to the @cl:param(document). The @cl:param(key) is coerced to string using the @cl:spec(string) function. The type of the @cl:param(value) must be a valid BSON supported type.")
  (:method ((document <document>) key value)
    (check-type value (or float string symbol <document> list vector <binary-data> <object-id>
                          boolean <mongo-timestamp> <regex> <javascript> integer timestamp))
    (setf (get# (string key) (elements document)) value)
    document))

(defgeneric get-element (document key)
  (:documentation "Gets the elements identified by @cl:param(key). @cl:param(key) is coerced to string using the @cl:spec(string).")
  (:method ((document <document>) key)
    (get# (string key) (elements document))))

(defgeneric remove-element (document key)
  (:documentation "Removes the elements identified by @cl:param(key). @cl:param(key) is coerced to string using @cl:spec(string).")
  (:method ((document <document>) key)
    (rem# (string key) (elements document))
    document))

(defgeneric keys (document)
  (:documentation "Returns all keys of the @cl:param(document).")
  (:method ((document <document>))
    (ht-keys (elements document))))
