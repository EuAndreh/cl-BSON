(defpackage cl-bson
  (:use cl cl-bson.readtable cl-bson.types cl-bson.encode cl-bson.decode)
  (:nicknames bson)
  (:export <binary-data>
           <document>
           <javascript>
           <mongo-timestamp>
           <object-id>
           <regex>
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
           remove-element

           encode

           decode
           *bson-sequence-type*

           bson-syntax
           disable-bson-document-printer
           disable-object-id-printer
           disable-printers
           enable-bson-document-printer
           enable-object-id-printer
           enable-printers)
  (:documentation "The main API consists of two functions: @c(encode) and @c(decode), which are the most important functionallity you'll need to implement a MongoDB driver, like @link[uri=\"http://euandre.org/cl-MongoDB\"](cl-MongoDB).

All complies with the @link[uri=\"http://bsonspec.org/\"](BSON spec).

@begin(section)
@title(Examples)
Setting up with @link[uri=\"https://www.quicklisp.org/beta/\"](Quicklisp) and @link[uri=\"https://common-lisp.net/project/named-readtables/\"](Named-Readtables):
@code[lang=lisp](* (ql:quickload :cl-bson)
; => (:CL-BSON)
* (in-package :cl-bson)
; => #<PACKAGE \"CL-BSON\">
* (named-readtables:in-readtable bson-syntax)
; => big alist of readtables...
* (enable-printers)
; => NIL
* (decode (encode #d(\"string\" \"string\"
                     \"double\" 1.5d0
                     \"embedded\" #d(\"embedded\" \"document\"))))
; => #d(\"string\" \"string\" \"double\" 1.5d0 \"embedded\" #d(\"embedded\" \"document\"))
* (encode #d(\"my\" \"document\"))
; => #(22 0 0 0 2 109 121 0 9 0 0 0 100 111 99 117 109 101 110 116 0 0)
* (decode *)
; => #d(\"my\" \"document\")
)
@end(section)"))
