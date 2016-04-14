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
           enable-printers))
