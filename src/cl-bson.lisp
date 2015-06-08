(defpackage cl-bson
  (:use cl cl-bson.readtable cl-bson.types cl-bson.encode cl-bson.decode)
  (:nicknames bson)
  (:import-from cl-reexport
                reexport-from)
  (:import-from rutil
                eval-always)
  (:documentation "This package exports all the symbols of `cl-bson` API. See documentation at http://euandreh.org/cl-bson for usage, tutorial and examples."))
(in-package bson)

#+nil
(eval-always
 (mapcar #'reexport-from
         '(:cl-bson.types
           :cl-bson.readtable
           :cl-bson.encode
           :cl-bson.decode)))
