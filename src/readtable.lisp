(defpackage cl-bson.readtable
  (:use cl)
  (:import-from cl-bson.types
                <document>
                <object-id>
                add-element
                get-element
                keys
                str
                string->object-id)
  (:import-from lol
                defmacro!)
  (:import-from rutil
                acond
                it
                group)
  (:import-from named-readtables
                defreadtable
                in-readtable)
  (:export bson-syntax
           disable-bson-document-printer
           disable-object-id-printer
           disable-printers
           enable-bson-document-printer
           enable-object-id-printer
           enable-printers)
  (:documentation "Package for optional BSON read-print functionality. Uses @link[uri=\"https://common-lisp.net/project/named-readtables/\"](named-readtables) for @cl:spec(*readtable*) manipulation.

Defines read-macros for @c(#d()) (literal @c(<document>)) and for @c(#i()) (literal @c(<document>)).

Also defines @cl:spec(pprint)int behaviour (for consistent read-print equivalence) for @c(#d()) and @c(#i()) read-macros in the @c(bson-syntax) readtable."))
(in-package cl-bson.readtable)
(in-readtable lol:lol-syntax)

(defun repeated-keys-p (pairs)
  "Checks if any @c(key) as in @c((key value)) is repeated (tested with @cl:spec(equal) for @c(hash-table)-like string comparison) in the @cl:param(pairs) list. If @c(T), returns the repeated @c(key)."
  (dolist (p pairs)
    (if (< 1 (count (car p) pairs :key #'car :test #'equal))
        (return (car p)))))

(defmacro! bson-document-literal (&rest contents)
  "Converts the @cl:param(contents) list into pairs of @c((key value)) and makes a @c(<document>) from it. Expands in the form that create such @c(<document>).

If any key is repeated (tested with @cl:spec(equal) in @c(#'repeated-keys-p)), or if @cl:param(contents) has an odd number of elements, or if @cl:param(contents) has non-string keys it throws an @cl:spec(error) at compile-time."
  (let ((pairs (group 2 contents)))
    (acond ((oddp (length contents))
            (error "Odd number of values in bson document literal."))
           ((repeated-keys-p pairs)
            (error "Repeated key(~s) in bson document literal." it))
          (t `(let ((,g!document (make-instance '<document>)))
                ,@(mapcar #` (add-element ,g!document ,(first a1) ,(second a1))
                          pairs)
                 ,g!document)))))

(defun bson-document-reader (stream char numarg)
  "@c(<document>) literal reader function. Reads in a form as a @c(#'bson-document-literal) form."
  (declare (ignore char numarg))
  `(bson-document-literal ,@(read stream)))

(defun object-id-reader (stream char numarg)
  "@c(<object-id>) literal reader function. Converts the symbol inside the form into an @c(<object-id>) using @c(#'string->object-id)."
  (declare (ignore char numarg))
  `(string->object-id ,(princ-to-string (car (read stream)))))

(defun pprint-bson-document (*standard-output* document)
  "Pprints a given @cl:param(document) in the @c(#d(key value)) format. Stablishes read-print-equivalence for @c(<document>) objects."
  (pprint-logical-block (*standard-output* nil :prefix "#d(" :suffix ")")
    (let* ((doc-keys (keys document))
           (end (length doc-keys))
           (i 0))
      (when (plusp end)
        (block printing
          (mapcar (lambda (key)
                    (pprint-pop)
                    (write key)
                    (princ " ")
                    (write (get-element document key))
                    (if (= (incf i) end) (return-from printing nil))
                    (write-char #\Space)
                    (pprint-newline :fill))
                  doc-keys))))))

(defun pprint-object-id (*standard-output* object-id)
  "Pprints a given @cl:param(object-id) in the @c(#(hex-id)) format. Stablishes read-print-equivalence for @c(<object-id>) objects."
  (pprint-logical-block (*standard-output* nil :prefix "#i(" :suffix ")")
    (princ (str object-id))))

(defun enable-bson-document-printer ()
  "Enable pprinter for @c(<document>) objects."
  (set-pprint-dispatch '<document> 'pprint-bson-document))

(defun enable-object-id-printer ()
  "Enable pprinter for @c(<object-id>) objects."
  (set-pprint-dispatch '<object-id> 'pprint-object-id))

(defun enable-printers ()
  "Enables pprinter for both @c(<document>) and @c(<object-id>) objects."
  (enable-bson-document-printer)
  (enable-object-id-printer))

(defun disable-bson-document-printer ()
  "Disables the pprinter for @c(<document>) objects."
  (set-pprint-dispatch '<document> nil))

(defun disable-object-id-printer ()
  "Disables the pprinter for @c(<object-id>) objects."
  (set-pprint-dispatch '<object-id> nil))

(defun disable-printers ()
  "Disables pprinter for @c(<document>) and @c(<object-id>) objects."
  (disable-bson-document-printer)
  (disable-object-id-printer))

(defreadtable bson-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\d #'bson-document-reader)
  (:dispatch-macro-char #\# #\i #'object-id-reader))
