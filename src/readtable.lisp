(defpackage cl-bson.readtable
  (:use cl)
  (:import-from arrow-macros
                ->)
  (:import-from cl-bson.types
                <document>
                <object-id>
                <regex>
                *allowed-regex-options*
                add-element
                get-element
                keys
                options
                pattern
                str
                string->object-id)
  (:import-from lol
                defmacro!)
  (:import-from rutil
                acond
                aif
                it
                group
                last1
                strcat
                substr)
  (:import-from named-readtables
                defreadtable
                in-readtable)
  (:export bson-syntax
           disable-bson-document-printer
           disable-object-id-printer
           disable-printers
           enable-bson-document-printer
           enable-object-id-printer
           enable-printers
           repeated-keys-p)

  (:documentation "Package for optional BSON read-print functionality. Uses @link[uri=\"https://common-lisp.net/project/named-readtables/\"](named-readtables) for @cl:spec(*readtable*) manipulation.

Defines read-macros for @c(#d()) (literal @c(<document>)) and for @c(#i()) (literal @c(<document>)).

Also defines @cl:spec(pprint)int behaviour (for consistent read-print equivalence) for @c(#d()) and @c(#i()) read-macros in the @c(bson-syntax) readtable.

All of them are just optional. If you don't like, feel free to not use it =]

@begin(section)
@title(Usage)
@code[lang=lisp](* (make-instance '<regex> :pattern \"^abc$\" :options \"i\")
; => #/^abc$/i
* (make-instance '<object-id>)
; => #i(90E08055616E64726568310C67E3D1)
* (make-document)
; => #d(\"_id\" #i(9CE08055616E64726568310C68E3D1))

* (enable-printers)
; => NIL
* (make-instance '<regex> :pattern \"^abc$\" :options \"i\")
; => #/^abc$/i
* (make-instance '<object-id>)
; => #i(B4E08055616E64726568310C69E3D1)
* (make-document)
; => #d(\"_id\" #i(B8E08055616E64726568310C6AE3D1))

* (named-readtables:in-readtable bson-syntax)
; big alist of readtables
* #/asdf/li
; => #/asdf/il
* #i(B4E08055616E64726568310C69E3D1)
; => #i(B4E08055616E64726568310C69E3D1)
* #d(\"my\" \"doc\")
; => #d(\"my\" \"doc\")
)

Explicar sobre #d($lt 1)
@end(section)"))
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
                ,@(mapcar #`(add-element ,g!document ,(first a1) ,(second a1))
                          pairs)
                 ,g!document)))))

(defun bson-document-reader (stream char numarg)
  "@c(<document>) literal reader function. Reads in a form as a @c(#'bson-document-literal) form. Escapes any symbol that starts with @c($)."
  (declare (ignore char numarg))
  (let ((*readtable* (copy-readtable *readtable* nil)))
    (set-macro-character #\$ (lambda (stream char)
                               (declare (ignore char))
                               (strcat "$" (string-downcase (read stream)))))
    `(bson-document-literal ,@(read stream))))

(defun object-id-reader (stream char numarg)
  "@c(<object-id>) literal reader function. Converts the symbol inside the form into an @c(<object-id>) using @c(#'string->object-id)."
  (declare (ignore char numarg))
  `(string->object-id ,(princ-to-string (car (read stream)))))

(defvar *regex-literal-delimiter* #\/
  "Limiter character used for @c(<regex>) objects literals.")

(defun regexp-reader (stream char numarg)
  "@c(<regex>) literal reader function."
  (declare (ignore char numarg))
  (let ((pattern (-> (loop for x = (read-char stream) then (read-char stream)
                        collect x into chars
                        until (and (eql x #\/)
                                   (not (eql (last1 chars 2) #\\)))
                        finally (return chars))
                   (coerce 'string)
                   (substr 0 -1)))
        (options (-> (loop for x = (read-char stream) then (read-char stream)
                        collect x until (not (alpha-char-p x))
                        finally (unread-char x stream))
                   (coerce 'string)
                   (substr 0 -1))))
    `(make-instance '<regex> :pattern ,pattern :options ,options)))

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

(defun pprint-regex (*standard-output* regex)
  "Pprints a given @cl:param(regex>) int.."
  (pprint-logical-block (*standard-output* nil :prefix "#/")
    (princ (pattern regex))
    (princ "/")
    (princ (options regex))))

(defun enable-bson-document-printer ()
  "Enable pprinter for @c(<document>) objects."
  (set-pprint-dispatch '<document> 'pprint-bson-document))

(defun enable-object-id-printer ()
  "Enable pprinter for @c(<object-id>) objects."
  (set-pprint-dispatch '<object-id> 'pprint-object-id))

(defun enable-regex-printer ()
  "Enable pprint for @c(<regex>) objects."
  (set-pprint-dispatch '<regex> 'pprint-regex))

(defun enable-printers ()
  "Enables pprinter for both @c(<document>) and @c(<object-id>) objects."
  (enable-bson-document-printer)
  (enable-object-id-printer)
  (enable-regex-printer))

(defun disable-bson-document-printer ()
  "Disables the pprinter for @c(<document>) objects."
  (set-pprint-dispatch '<document> nil))

(defun disable-object-id-printer ()
  "Disables the pprinter for @c(<object-id>) objects."
  (set-pprint-dispatch '<object-id> nil))

(defun disable-regex-printer ()
  "Disable the pprinter for @c(<regex>) objects."
  (set-pprint-dispatch '<regex> nil))

(defun disable-printers ()
  "Disables pprinter for @c(<document>) and @c(<object-id>) objects."
  (disable-bson-document-printer)
  (disable-object-id-printer)
  (disable-regex-printer))

(defreadtable bson-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\/ #'regexp-reader)
  (:dispatch-macro-char #\# #\d #'bson-document-reader)
  (:dispatch-macro-char #\# #\i #'object-id-reader))
