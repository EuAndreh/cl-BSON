(defpackage cl-bson-test
  (:import-from arrow-macros
                ->)
  (:import-from babel
                string-to-octets)
  (:import-from intbytes
                int32->octets
                octets->int32)
  (:import-from local-time
                now
                nsec-of
                timestamp
                timestamp-millisecond
                timestamp-to-unix
                timestamp<=
                unix-to-timestamp)
  (:import-from lol
                defmacro!)
  (:import-from named-readtables
                in-readtable)
  (:import-from trivial-shell
                os-process-id)
  (:use cl prove cl-bson))
(in-package cl-bson-test)
(in-readtable bson:bson-syntax)
(bson:enable-printers)
(setf *print-pretty* t)
;; NOTE: To run this test file, execute `(asdf:test-system :cl-bson)' in your Lisp.

(plan 7)

(defmacro! enc-dec-kv (key value)
  "Creates a document, adds a KEY VALUE, encodes it with #'ENCODE, decodes it with #'DECODE and uses #'GET-ELEMENT to return the value stored under KEY of the encoded->decoded document."
  `(let ((,g!doc (make-document :_id nil)))
     (add-element ,g!doc ,key ,value)
     (get-element (decode (encode ,g!doc)) ,key)))

(defmacro type-byte-of (document)
  "Acess the type of the first VALUE of the given OBJECT."
  `(aref (encode ,document) 4))

(deftest encode-decode-equivalence-test
  (is (enc-dec-kv "float" 1.5)
      1.5d0
      "SINGLE-FLOAT gets coerced to DOUBLE-FLOAT")
  (is (enc-dec-kv "double" 1.5d0)
      1.5d0
      "DOUBLE-FLOAT stays as DOUBLE-FLOAT.")
  (is (enc-dec-kv "utf-8 string" "utf8 string: áŭλ")
      "utf8 string: áŭλ"
      "Encodes and decodes correctly an UTF-8 string.")
  (is (enc-dec-kv "ascii string" "just ASCII!.?")
      "just ASCII!.?"
      "Encodes and decodes correctly an ASCII string.")
  (is-type (enc-dec-kv "embedded document" #d("1" 2))
           '<document>
           "Returns the decoded value with the correct type (<DOCUMENT>).")
  (is (get-element (enc-dec-kv "embedded document" #d("1" 2)) "1")
      2
      "Sucessfully accesses  the correct value of the encoded->decoded document.")
  (let ((*bson-sequence-type* 'vector))
    (is-type (enc-dec-kv "vector" #(1 2 3))
             'vector
             "Receives the correct return type (VECTOR) with *BSON-SEQUENCE-TYPE* bound to 'VECTOR.")
    (is (enc-dec-kv "vector" #(1 2 3))
        #(1 2 3)
        :test #'equalp
        "Correctly encodes->decodes the values of the array in the document.")
    (let ((*bson-sequence-type* 'list))
      (is-type (enc-dec-kv "list" '(1 2 3))
               'list
               "Receives the correct return type (LIST) with the *BSON-SEQUENCE-TYPE* bound to 'LIST.")
      (is (enc-dec-kv "list" '(1 2 3))
          '(1 2 3)
          :test #'equalp
          "Correctly encodes->decodes the values of the array in the document.")))
  (is-type (enc-dec-kv "_id" (make-instance '<object-id>))
           '<object-id>
           "Correctly encodes and decodes the <OBJECT-ID> type.")
  (is-type (get-element (decode (encode (make-document))) "_id")
           '<object-id>
           "Correctly encodes and decodes the <OBJECT-ID> type on the document create \"manually\".")
  (let ((_id (make-instance '<object-id>)))
    (is (octets (enc-dec-kv "_id" _id))
        (octets _id)
        :test #'equalp
        "The value of the encoded->decoded OBJECT-ID stays the same.")
    (is (octets (get-element (decode (encode (make-document :_id _id))) "_id"))
        (octets _id)
        :test #'equalp
        "The value of the encoded->decoded OBJECT-ID of the document created \"manually\" stays the same. "))
  (is (enc-dec-kv "true" t)
      t
      "Correctly encodes and decodes a T value.")
  (is (enc-dec-kv "false" nil)
      nil
      "Correctly encodes and decodes a NIL (boolean) value.")
  (let ((now (now)))
    (is (timestamp-to-unix (enc-dec-kv "timestamp" now))
        (timestamp-to-unix now)
        "Correctly encodes->decodes the value of the timestamp.")
    (is (timestamp-millisecond (enc-dec-kv "timestamp" now))
        (timestamp-millisecond now)
        "Correctly encodes->decodes the milliseconds of the timestamp.")
    (ok (<= (nsec-of (enc-dec-kv "timestamp" now))
            (nsec-of now))
        "Nanoseconds value should be different (unless I'm really unlucky and gets 0 nanoseconds of difference."))
  (let ((regex (make-instance '<regex>
                              :pattern "^my.*?pattern"
                              :options "wi")))
    (is (options (enc-dec-kv "regex" regex))
        "iw"
        "Options slot gets sorted correctly")
    (is (pattern (enc-dec-kv "regex" regex))
        "^my.*?pattern"
        "PATTERN is encoded->decoded->acessed corretly.")
    (is-error (setf (options regex) "abcd")
              'simple-error
              "Invalid regex OPTIONS throws an error."))
  (is (code (enc-dec-kv "javascript" (make-instance '<javascript> :code "var code = 1;")))
      "var code = 1;"
      "CODE is correctly encoded->decoded->accessed.")
  (is (code (enc-dec-kv "javascript with scope" (make-instance '<javascript>
                                                               :code "var second = third;"
                                                               :scope #d("third" 5))))
      "var second = third;"
      "JAVASCRIPT CODE gets correctly encoded->decoded->accessed.")
  (is (enc-dec-kv "32-bit integer" -1)
      -1
      "Negative 32-bit integers get correctly encoded->decoded.")
  (is (enc-dec-kv "64-bit integer" 123456780987654321)
      123456780987654321
      "Big 64-bit integers get corretly encoded->decoded.")
  (let ((mongo-timestamp (make-instance '<mongo-timestamp>)))
    (is (mongo-time (enc-dec-kv "mongo timestamp" mongo-timestamp))
        (mongo-time mongo-timestamp)
        :test #'equalp)))

(deftest encoded-byte-bson-type-test
  "desc"
  (is (type-byte-of #d("float" 1.1))
      #x01
      "Float type is expected to be #x01.")
  (is (type-byte-of #d("string" "string"))
      #x02
      "String type is expected to be #x02.")
  (is (type-byte-of #d("embedded document" #d("1" "2")))
      #x03
      "Embedded document type is expected to be #x03.")
  (is (type-byte-of #d("sequence" #(1 2 3)))
      #x04
      "Array type is expected to be #x04.")
  (is (type-byte-of #d("sequence" '(1 2 3)))
      #x04
      "List type is expected to be #x04, too.")
  (is (type-byte-of #d("binary data" (make-instance '<binary-data>)))
      #x05
      "Binary data type is expected to be #x05.")
  (is (type-byte-of #d("object-id" (make-instance '<object-id>)))
      #x07
      "ObjectId is expected to be #x07.")
  (is (type-byte-of #d("boolean true" t))
      #x08
      "Boolean T type is expected to be #x08.")
  (is (type-byte-of #d("boolean false" nil))
      #x08
      "Boolean NIL type is expected to be #x08, too.")
  (is (type-byte-of #d("timestamp" (now)))
      #x09
      "LOCAL-TIME:TIMESTAMP (BSON Date) is expected to be #x09.")
  (is (type-byte-of #d("regex" (make-instance '<regex>)))
      #x0B
      "<REGEX> type is expected to be #X0B.")
  (is (type-byte-of #d("javascript code" (make-instance '<javascript> :code "vax a = 10;")))
      #x0D
      "\"Javascript code\" type is expected to be #x0D.")
  (is (type-byte-of #d("javascript code with scope"
                       (make-instance '<javascript> :code "var x = 1;" :scope #d("a" "3"))))
      #x0F
      "\"Javascript code with scope\" type is expected to be #x0F.")
  (is (type-byte-of #d("32 bit integer" 123))
      #x10
      "32-bit integer type is expected to be #x10.")
  (is (type-byte-of #d("mongo timestamp" (make-instance '<mongo-timestamp>)))
      #x11
      "MongoDB internal Timestamp type is expected to be #x11.")
  (is (type-byte-of #d("64 bit integer" 1234567890987654321))
      #x12
      "64-bit integer type is expected to be #x12."))

(deftest types-test
  (subtest "subtest for #'GENERATE-OBJECT-ID functionality:"
    (ok (< cl-bson.types::*object-id-counter* (expt 2 24))
        "The randomly generated *OBJECT-ID-COUNTER* is smaller than (expt 2 24).")
    (let  ((cl-bson.types::*object-id-counter* (1- (expt 2 24))))
      (cl-bson.types::generate-object-id)
      (is cl-bson.types::*object-id-counter*
          0
          "*OBJECT-ID-COUNTER* is 0 after being the largest value possible."))
    (let* ((obj-id-bytes (cl-bson.types::generate-object-id))
           (obj-id-timestamp (-> (subseq obj-id-bytes 0 4)
                               octets->int32
                               unix-to-timestamp))
           (now (now)))
      (is-type obj-id-bytes
               'octets-array
               "OBJ-ID-BYTES is an OCTETS-ARRAY.")
      (is (length obj-id-bytes)
          12
          "Object id octets is 12 bytes long.")
      (ok (timestamp<= obj-id-timestamp
                       now)
          "The generated OBJ-ID-TIMESTAMP has an actual timestamp for now between byte 0 and 4.")
      (is (subseq obj-id-bytes 4 7)
          (-> (machine-instance)
            (string-to-octets :encoding :utf-8)
            (subseq 0 3))
          :test #'equalp
          "MACHINE-INSTANCE identifier is #'EQUALP to bytes 4 to 7 in the generated OBJ-ID-BYTES.")
      (is (subseq obj-id-bytes 7 9)
          (-> (cl-bson.types::os-process-id)
            int32->octets
            (subseq 0 2))
          :test #'equalp
          "OS-PROCESS-ID identifier is #'EQUALP to bytes 7 to 9 in the generated OBJ-ID-BYTES.")))
  (subtest "subtest for <OBJECT-ID> behaviour:"
    (let ((now (now))
          (obj-id (make-instance '<object-id>)))
      (is-type (octets obj-id)
               'octets-array
               "OCTETS of the generated OBJ-ID is an OCTETS-ARRAY.")
      (is (length (octets obj-id))
          12
          "OCTETS of the generated OBJ-ID has 12 bytes.")
      (is-type (str obj-id)
               'string
               "Bytes in the octet of the generated OBJ-ID can be parsed with the (parse-integer byte :radix 16).")
      (is (octets (string->object-id (str obj-id)))
          (octets obj-id)
          :test #'equalp
          "Bytes of the OBJ-ID stays the same when coerced to string with #'STR and coerced back to <OBJECT-ID> back with #'STRING->OBJECT-ID.")
      (ok (timestamp<= (get-timestamp obj-id)
                       now)
          "Timestamp of the generated OBJ-ID is before LOCAL-TIME:NOW (because it loses nanoseconds precision).")))
  (subtest "subtest for <REGEX> behaviour:"
    (let ((regex (make-instance '<regex>)))
      (ok (slot-boundp regex 'pattern)
          "PATTERN slot of the REGEX is bound.")
      (ok (slot-boundp regex 'options)
          "OPTIONS slot of the REGEX is bound.")
      (is (pattern regex)
          ""
          "Default pattern is an empty string.")
      (is (options regex)
          ""
          "Default options is an empty string.")


      (setf (options regex) "wui")
      (is (options regex)
          "iuw"
          "OPTIONS gets alphabetically sorted at the (SETF OPTIONS).")
      (is-error (setf (options regex) "abc")
                'simple-error
                "Invalid character for OPTIONS raises an ERROR.")
      (is (options (make-instance '<regex>
                                  :pattern "pat"
                                  :options "xsm"))
          "msx"
          "Constructor for <REGEX> correctly sorts the options.")
      (is-error (make-instance '<regex>
                               :pattern "pat"
                               :options "asdf")
                'simple-error
                "Constructor for <REGEX> raises an error when :OPTIONS has an invalid character.")))
  (subtest "subtest for <BINARY-DATA> behaviour:"
    (is-error (make-instance '<binary-data>
                             :subtype :invalid)
              'simple-error
              ":INVALID :SUBTYPE raises an ERROR.")
    (let ((bin-data (make-instance '<binary-data> :octets (fast-io:make-octet-vector 10))))
      (is (subtype bin-data)
          :generic
          "Default SUBTYPE is :GENERIC.")
      (setf (subtype bin-data) :uuid)
      (is (subtype bin-data)
          :uuid
          "SUBTYPE can be SETFed to :UUID.")
      (is-error (setf (subtype bin-data) :also-invalid)
                'simple-error
                "(setf (subtype bin-data) :also-invalid) raises an error, too.")
      (is-type (octets bin-data)
               'octets-array
               "OCTETS of BIN-DATA is an OCTETS-ARRAY.")
      (is-type (subtype bin-data)
               'keyword
               "SUBTYPE of BIN-DATA is a KEYWORD.")
      (is (octets bin-data)
          (fast-io:make-octet-vector 10)
          :test #'equalp
          "(octets bin-data) has default data assigned in the instanciation.")))
  (subtest "subtest for <JAVASCRIPT> behaviour:"
    (let ((javascript (make-instance '<javascript>)))
      (is-error (code javascript)
                'error
                "CODE slot of JAVASCRIPT is unbound by default.")
      (is (slot-boundp javascript 'scope)
          nil
          "SCOPE slot of JAVASCRIPT is NIL by default.")))
  (subtest "subtest for <MONGO-TIMESTAMP> behaviour:"
    (ok (< cl-bson.types::*mongo-timestamp-counter* (expt 2 32))
        "The randomly generated *MONGO-TIMESTAMP-COUNTER* is smaller than (expt 2 32).")
    (let ((cl-bson.types::*mongo-timestamp-counter* (1- (expt 2 32))))
      (cl-bson.types::generate-mongo-timestamp)
      (is cl-bson.types::*mongo-timestamp-counter*
          0
          "*MONGO-TIMESTAMP-COUNTER* is 0 after being the largest value possible (expt 2 32)."))
    (let* ((mongo-now (make-instance '<mongo-timestamp>))
           (mongo-now-timestamp (-> (mongo-time mongo-now)
                                  (subseq 0 4)
                                  octets->int32
                                  unix-to-timestamp))
           (now (now)))
      (is-type (mongo-time mongo-now)
               'octets-array
               "(mongo-time mongo-now) is an OCTETS-ARRAY.")
      (is (length (mongo-time mongo-now))
          8
          "mongo-time octets is 8 bytes long.")
      (ok (timestamp<= mongo-now-timestamp
                       now)
          "The generated <MONGO-TIMESTAMP> has an actual timestamp for now located between byte 4 and 8.")))
  (subtest "subtest for <document> manipulation and behaviour:"
    (let ((empty-doc (make-instance '<document>))
          (doc (make-document))
          (also-empty-doc (make-document :_id nil)))
      (is-type (get-element doc "_id")
               '<object-id>
               "Document generated with MAKE-DOCUMENT has an <OBJECT-ID> identified by \"_id\".")
      (is-values (get-element empty-doc "_id")
                 '(nil nil)
                 "Document generated with (make-instance '<document>) has no <OBJECT-ID>.")
      (is-type (elements empty-doc)
               'hash-table
               "(elements empty-doc) is a HASH-TABLE.")
      (is (length (keys empty-doc))
          0
          "EMPTY-DOC has no elements.")
      (is (length (keys doc))
          1
          "DOC (generated with MAKE-DOCUMENT) has 1 element: the \"_id_\".")
      (is (length (keys also-empty-doc))
          0
          "Document generated with (MAKE-DOCUMENT :_id nil) has no keys.")
      (is-values (get-element doc :non-existing-key)
                 '(nil nil)
                 "Getting a :NON-EXISTING-KEY from a doc returns two values (the same of GETHASH).")
      (add-element doc "my-key" "my-value")
      (is (get-element doc "my-key")
          "my-value"
          "#'GET-ELEMENT works with an already bound document.")
      (is (get-element (add-element #d() "my-key" "my-value") "my-key")
          "my-value"
          "#'GET-ELEMENT works with document just created and modified with #'ADD-ELEMENT.")
      (is (get-element #d("my-key" "my-value") "my-key")
          "my-value"
          "#'GET-ELEMENT works with literal documents.")
      (remove-element doc "my-key")
      (is-values (get-element doc "my-key")
                 '(nil nil)
                 "#'REMOVE-ELEMENT works with an already bound document.")
      (is-values (get-element (remove-element #d("my-key" "my-value") "my-key") "my-key")
                 '(nil nil)
                 "#'REMOVE-ELEMENT works with an just created literal document.")
      (is (get-element (remove-element #d("my-key" "my-value") "crazy-key") "my-key")
          "my-value"
          "#'REMOVE-ELEMENT called with the wrong key leaves the document intact."))))

(deftest readtable-test
  (subtest "<DOCUMENT> literal read-macro test:"
    (is-type #d()
             '<document>
             "#d() is of type <DOCUMENT>.")
    (is-print (princ #d())
              "#d()"
              "#d() prints like #d().")
    (is-print (princ #d("1" "2" "3" "4"))
              "#d(1 2 3 4)"
              "#d(\"1\" \"2\" \"3\" \"4\") prints correctly.")
    (is-expand #d()
               (LET ()
                 (LET (($DOCUMENT (MAKE-INSTANCE '<DOCUMENT>)))
                   $DOCUMENT))
               "Empty #d() expands correctly.")
    (is '#d()
        '(cl-bson.readtable::bson-document-literal)
        "Expands to the correct empty form.")
    (is-expand #d("1" "2" "3" "4")
               (LET ()
                 (LET (($DOCUMENT (MAKE-INSTANCE '<DOCUMENT>)))
                   (ADD-ELEMENT $DOCUMENT "1" "2")
                   (ADD-ELEMENT $DOCUMENT "3" "4")
                   $DOCUMENT))
               "#d() empty form expands corretly.")
    (is '#d("1" "2")
        '(cl-bson.readtable::bson-document-literal "1" "2")
        "Expands corretly to the non-empty form.")
    (is-error #d(1 2)
              'type-error
              "Throws an error because 1 is not of type string nor coercible with #'STRING.")
    (is-error (macroexpand '#d("1" 2 "3"))
              'simple-error
              "Throws an error because document literal has an odd number of values.")
    (is-error (macroexpand '#d("1" 2 "1" 2))
              'simple-error
              "Throws an error because document literal has repeated keys.")
    (disable-printers)
    (isnt (princ-to-string #d())
          (not "#d()")
          "#d() doesn't print like it with #'DISABLE-PRINTERS.")
    (isnt (princ-to-string #d("1" "2" "3" "4"))
          (not "#d(1 2 3 4)")
          "#d(\"1\" \"2\" \"3\" \"4\") doesn't print like it with #'DISABLE-PRINTERS."))
  (subtest "<OBJECT-ID> literal read-macro test:"
    (is-error #i()
              'error
              "Throws error becaus #i() has no <OBJECT-ID> string representation.")
    (is-type #i(50ED6E55616E64231C5D2EDF)
             '<object-id>
             "#i() corretly returns an object of <OBJECT-ID> type.")
    (enable-printers)
    (is-print (princ #i(50ED6E55616E64231C5D2EDF))
              "#i(50ED6E55616E64231C5D2EDF)"
              "Corretly prints a valid <OBJECT-ID>.")
    (is '#i(50ED6E55616E64231C5D2EDF)
        '(STRING->OBJECT-ID "50ED6E55616E64231C5D2EDF")
        "Correctly expands a #i(0000....) form.")
    (disable-printers)
    (isnt (princ-to-string #i(50ED6E55616E64231C5D2EDF))
          (not "#i(50ED6E55616E64231C5D2EDF)")
          "Doesn't print like #i(...) with #'DISABLE-PRINTERS.") ))

(deftest big-documen-types-test
  (let* ((raw-doc #d(:keyword-key :keyword-value
                     'symbol-key 'symbol-value
                     "string-key" "string-value"
                     "will be coerced to double" 1.5
                     "will stay as double" 1.5d0

                     "embedded document"
                     #d("with many"
                        #d("levels of"
                           #d("nesting"
                              #d("with another \"embedded document\": a vector" #(1 2 3)))))

                     "sequence" #(1 2 3)
                     "vector" #(1 2 3)
                     "list" '(1 2 3)
                     "regex" (make-instance '<regex> :pattern "\\d+" :options "i")
                     "binary data" (make-instance
                                    '<binary-data>
                                    :octets (fast-io:make-octet-vector 10))
                     "javascript code" (make-instance '<javascript>
                                                      :code "var example = 1;")
                     "javascript code with scope" (make-instance
                                                   '<javascript>
                                                   :code "var example = inScope;"
                                                   :scope #d("inScope" 10))
                     "object-id" (make-instance '<object-id>)
                     "boolean true" t
                     "boolean false" nil
                     "null value" nil
                     "32 bit integer" 123
                     "64 bit integer" 1234567890987654321
                     "local-time:timestamp" (local-time:now)))
         (doc (decode (encode raw-doc ))))
    (is-type (get-element doc "KEYWORD-KEY")
             'string
             ":keyword-key gets coerced to string.")
    (is-type (get-element doc "SYMBOL-KEY")
             'string
             "'symbol-key gets coerced to string.")
    (is-type (get-element doc "string-key")
             'string
             "String value stays as string.")
    (is-type (get-element doc "will be coerced to double")
             'double-float
             "SINGLE-FLOAT gets coerced to double.")
    (is-type (get-element doc "will stay as double")
             'double-float
             "DOUBLE-FLOAT stays as double.")
    (is-type (get-element doc "embedded document")
             '<document>
             "embedded document in document is a <DOCUMENT>.")
    (is-type (get-element doc "sequence")
             'sequence
             "Array in BSON document is a sequence.")
    (is-type (get-element doc "vector")
             'sequence
             "Vector in document is a sequence.")
    (is-type (get-element doc "list")
             'sequence
             "List in document is a sequence")
    (is-type (get-element doc "regex")
             '<regex>
             "regex in document is a <REGEX>.")
    (is-type (get-element doc "binary data")
             '<binary-data>
             "binary data in document is a <BINARY-DATA>.")
    (is-type (get-element doc "javascript code")
             '<javascript>
             "javascript code in document is a <JAVASCRIPT>.")
    (is-type (get-element doc "javascript code with scope")
             '<javascript>
             "javascript code with scope in document is a <JAVASCRIPT>.")
    (is-type (get-element doc "object-id")
             '<object-id>
             "object-id in document is a <OBJECT-ID>.")
    (is (get-element doc "boolean true")
        t
        "boolean true in document is T.")
    (is (get-element doc "boolean false")
        nil
        "boolean false in document is NIL.")
    (is (get-element doc "null value")
        nil
        "null value in document is NIL.")
    (is-type (get-element doc "32 bit integer")
             'integer
             "32 bit integer in document is integer.")
    (ok (<= (integer-length (get-element doc "32 bit integer")) 32)
        "32 bit integer in document occupies less than 33 bits.")
    (is-type (get-element doc "64 bit integer")
             'integer
             "64 bit integer in document is integer.")
    (ok (<= (integer-length (get-element doc "64 bit integer")) 64)
        "64 bit integer in document occupies less than 65 bits.")
    (is-type (get-element doc "local-time:timestamp")
             'timestamp
             "local-time:timestamp in document is a TIMESTAMP.")))

(deftest binary-data-subtypes-test
  (is (subtype (enc-dec-kv "bin" (make-instance '<binary-data>)))
      :generic
      "Default <BINARY-DATA> subtype is :GENERIC.")
  (is (subtype (enc-dec-kv "bin" (make-instance '<binary-data> :subtype :function)))
      :function
      "<BINARY-DATA> subtype is :FUNCTION")
  (is (subtype (enc-dec-kv "bin" (make-instance '<binary-data> :subtype :binary-old)))
      :binary-old
      "<BINARY-DATA> subtype is :BINARY-OLD")
  (is (subtype (enc-dec-kv "bin" (make-instance '<binary-data> :subtype :uuid-old)))
      :uuid-old
      "<BINARY-DATA> subtype is :UUID-OLD")
  (is (subtype (enc-dec-kv "bin" (make-instance '<binary-data> :subtype :uuid)))
      :uuid
      "<BINARY-DATA> subtype is :UUID")
  (is (subtype (enc-dec-kv "bin" (make-instance '<binary-data> :subtype :md5)))
      :md5
      "<BINARY-DATA> subtype is :MD5")
  (is (subtype (enc-dec-kv "bin" (make-instance '<binary-data> :subtype :user-defined)))
      :user-defined
      "<BINARY-DATA> subtype is :USER-DEFINED")
  (is-error (make-instance '<binary-data> :subtype :invalid)
            'simple-error
            "<BINARY-DATA> invalid subtype throws an error."))

(deftest bad-encoded-document-test
  (is-error (decode (replace (encode #d("int" 2)) #(99) :start1 4))
            'simple-error
            "Unvalid BSON type raises an error."))

(run-test-all)
