(defsystem cl-bson
  :name "cl-bson"
  :version "0.1.0"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/cl-bson"
  :bug-tracker "https://github.com/EuAndreh/cl-bson/issues"
  :source-control (:git "git@github.com:EuAndreh/cl-bson.git")
  :license "LLGPL"
  :description "BSON encoder/decoder for Common Lisp."
  :depends-on (arrow-macros
               babel
               cl-intbytes
               cl-reexport
               fast-io
               ieee-floats
               let-over-lambda
               local-time
               named-readtables
               rutils
               #+nil trivial-shell)
  :components ((:module "src"
                        :components ((:file "types")
                                     (:file "readtable")
                                     (:file "encode")
                                     (:file "decode")
                                     (:file "cl-bson")))
               (:static-file "README.md"))
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-bson-test))))
