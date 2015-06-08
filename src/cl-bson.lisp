(defpackage cl-bson
  (:use cl cl-bson.readtable cl-bson.types cl-bson.encode cl-bson.decode)
  (:nicknames bson)
  (:import-from rutil
                eval-always)
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
           octets
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
  (:documentation "This package exports all the symbols of `cl-bson` API. See documentation at http://euandreh.org/cl-bson for usage, tutorial and examples."))
