# cl-bson
[![Build Status](https://travis-ci.org/EuAndreh/cl-bson.svg?branch=master)](https://travis-ci.org/EuAndreh/cl-bson)
[![Coverage Status](https://coveralls.io/repos/EuAndreh/cl-bson/badge.svg?branch=master)](https://coveralls.io/r/EuAndreh/cl-bson?branch=master)

Inspired by [cl-mongo](https://github.com/fons/cl-mongo) and [mongo-cl-driver](https://github.com/archimag/mongo-cl-driver/tree/master/bson).

[API and documentation](http://euandre.org/cl-bson) generated by the (awesome) [Codex](https://github.com/CommonDoc/codex).

## Usage
### Setup
```lisp
* (ql:quickload :cl-bson)
; => (:CL-BSON)
* (in-package :cl-bson)
; => #<PACKAGE "CL-BSON">
* (named-readtables:in-readtable bson-syntax)
; => big alist of readtables...
* (enable-printers)
; => NIL
```

### Main API
The main functions are `encode` and `decode`:
```lisp
* (encode #d(:a-key 'a-value))
; => #(24 0 0 0 2 65 45 75 69 89 0 7 0 0 0 65 45 86 65 76 85 69 0 0)
* (decode *)
; => #d("A-KEY" "A-VALUE")

* (encode #d(:keyword-key :keyword-value
             'symbol-key 'symbol-value
             "string-key" "string-value"
             "will be coerced to double" 1.5
             "will stay as double" 1.5d0
             "embedded document" #d("one more level" #d("i believe" "that's enough"))
             "sequence" #(1 2 3)
             "vector" #(1 2 3)
             "list" '(1 2 3)
             "regex" (make-instance '<regex> :pattern "\\d+" :options "i")
             "binary data" (make-instance '<binary-data>
                                          :octets (fast-io:make-octet-vector 10))
             "javascript code" (make-instance '<javascript> :code "var example = 1;")
             "javascript code with scope" (make-instance '<javascript>
                                                         :code "var example = inScope;"
                                                         :scope #d("inScope" 10))
             "object-id" (make-instance '<object-id>)
             "boolean true" t
             "boolean false" nil
             "null value" nil
             "32 bit integer" 123
             "64 bit integer" 1234567890987654321
             "local-time:timestamp" (local-time:now)))
; => #(116 2 0 0 2 75 69 89 87 79 82 68 45 75 69 89 0 14 0 0 0 75 69 89 87 79 82 68 45 86 65 76 85 69 0 2 83 89 77 66 79 76 45 75 69 89 0 13 0 0 0 83 89 77 66 79 76
      45 86 65 76 85 69 0 2 115 116 114 105 110 103 45 107 101 121 0 13 0 0 0 115 116 114 105 110 103 45 118 97 108 117 101 0 1 119 105 108 108 32 98 101 32 99 111
      101 114 99 101 100 32 116 111 32 100 111 117 98 108 101 0 0 0 0 0 0 0 248 63 1 119 105 108 108 32 115 116 97 121 32 97 115 32 100 111 117 98 108 101 0 0 0 0
      0 0 0 248 63 3 101 109 98 101 100 100 101 100 32 100 111 99 117 109 101 110 116 0 55 0 0 0 3 111 110 101 32 109 111 114 101 32 108 101 118 101 108 0 34 0 0 0
      2 105 32 98 101 108 105 101 118 101 0 14 0 0 0 116 104 97 116 39 115 32 101 110 111 117 103 104 0 0 0 4 115 101 113 117 101 110 99 101 0 26 0 0 0 16 48 0 1 0
      0 0 16 49 0 2 0 0 0 16 50 0 3 0 0 0 0 4 118 101 99 116 111 114 0 26 0 0 0 16 48 0 1 0 0 0 16 49 0 2 0 0 0 16 50 0 3 0 0 0 0 4 108 105 115 116 0 26 0 0 0 16
      48 0 1 0 0 0 16 49 0 2 0 0 0 16 50 0 3 0 0 0 0 11 114 101 103 101 120 0 92 100 43 0 105 0 5 98 105 110 97 114 121 32 100 97 116 97 0 10 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 13 106 97 118 97 115 99 114 105 112 116 32 99 111 100 101 0 17 0 0 0 118 97 114 32 101 120 97 109 112 108 101 32 61 32 49 59 0 15 106 97 118 97 115 99
      114 105 112 116 32 99 111 100 101 32 119 105 116 104 32 115 99 111 112 101 0 46 0 0 0 0 118 97 114 32 101 120 97 109 112 108 101 32 61 32 105 110 83 99 111
      112 101 59 0 18 0 0 0 16 105 110 83 99 111 112 101 0 10 0 0 0 0 7 111 98 106 101 99 116 45 105 100 0 226 141 117 85 97 110 100 119 99 18 167 87 8 98 111 111
      108 101 97 110 32 116 114 117 101 0 1 8 98 111 111 108 101 97 110 32 102 97 108 115 101 0 0 8 110 117 108 108 32 118 97 108 117 101 0 0 16 51 50 32 98 105
      116 32 105 110 116 101 103 101 114 0 123 0 0 0 18 54 52 32 98 105 116 32 105 110 116 101 103 101 114 0 177 28 108 177 244 16 34 17 9 108 111 99 97 108 45 116
      105 109 101 58 116 105 109 101 115 116 97 109 112 0 184 59 50 211 77 1 0 0 0)
* (decode *)
; => #d("KEYWORD-KEY" "KEYWORD-VALUE"
        "SYMBOL-KEY" "SYMBOL-VALUE"
        "string-key" "string-value"
        "will be coerced to double" 1.5d0
        "will stay as double" 1.5d0
        "embedded document" #d("one more level" #d("i believe" "that's enough"))
        "sequence" #(1 2 3)
        "vector" #(1 2 3)
        "list" #(1 2 3)
        "regex" #<<REGEX> {100D000183}>
        "binary data" #<<BINARY-DATA> {100D002AF3}>
        "javascript code" #<<JAVASCRIPT> {100D074A43}>
        "javascript code with scope" #<<JAVASCRIPT> {100D0E5613}>
        "object-id" #i(C92F7555616E64776371F874)
        "boolean true" T
        "boolean false" NIL
        "null value" NIL
        "32 bit integer" 123
        "64 bit integer" 1234567890987654321
        "local-time:timestamp" @2015-06-08T03:01:45.734000-03:00)
```

Examples from the [FAQ](http://bsonspec.org/faq.html):
```lisp
;; Original: {"hello": "world"}
* (encode #d("hello" "world"))
; => #(22 0 0 0 2 104 101 108 108 111 0 5 0 0 0 119 111 114 108 100 0 0)

;; Since the example is in hexadecimal base:
* (map 'vector (lambda (_)
                 (let ((*print-base* 16))
                   (princ-to-string _)))
       *)

;; Output:
#("16" "0" "0" "0"
  "2"
  "68" "65" "6C" "6C" "6F" "0"
  "6" "0" "0" "0" "77" "6F" "72" "6C" "64" "0"
  "0")

\x16\x00\x00\x00                   // total document size
\x02                               // 0x02 = type String
hello\x00                          // field name
\x06\x00\x00\x00world\x00          // field value
\x00                               // 0x00 = type EOO ('end of object')


;; Original: {"BSON": ["awesome", 5.05, 1986]}
* (encode #d("BSON" #("awesome" 5.05d0 1986)))
#(49 0 0 0 4 66 83 79 78 0 38 0 0 0 2 48 0 8 0 0 0 97 119 101 115 111 109 101 0 1 49 0 51 51 51 51 51 51 20 64 16 50 0 194 7 0 0 0 0)

;; To hexadecimal:
* (map 'vector (lambda (_)
                 (let ((*print-base* 16))
                   (princ-to-string _)))
       *)

;; Output:
#("31" "0" "0" "0"
  "4" "42" "53" "4F" "4E" "0"
  "26" "0" "0" "0"
  "2" "30" "0" "8" "0" "0" "0" "61" "77" "65" "73" "6F" "6D" "65" "0"
  "1" "31" "0" "33" "33" "33" "33" "33" "33" "14" "40"
  "10" "32" "0" "C2" "7" "0" "0"
  "0"
  "0")

\x31\x00\x00\x00
\x04BSON\x00
\x26\x00\x00\x00
\x02\x30\x00\x08\x00\x00\x00awesome\x00
\x01\x31\x00\x33\x33\x33\x33\x33\x33\x14\x40
\x10\x32\x00\xc2\x07\x00\x00
\x00
\x00
```

Check the [detailed explanation](https://groups.google.com/d/msg/bson/8g76R0cb-CQ/DWdjQaS0tMMJ) of the last example.

## TODO
### on documentation:
+ Add `"fork me on GitHub"` button to documentation
+ Add links to home in the documentation
+ Add package docstrings to documentation
+ Add (setf options) docstrings to documentation
+ Add method docstrings to documentation

### Tests
+ Make read-print work on ccl
+ Make other implementations work on Travis

## Dependencies
This library depends on:
+ [arrow-macros](https://github.com/hipeta/arrow-macros/)
+ [babel](https://common-lisp.net/project/babel/)
+ [cl-intbytes](https://github.com/EuAndreh/cl-intbytes)
+ [fast-io](https://github.com/rpav/fast-io)
+ [ieee-floats](https://github.com/marijnh/ieee-floats)
+ [let-over-lambda](https://github.com/thephoeron/let-over-lambda)
+ [local-time](https://common-lisp.net/project/local-time/)
+ [named-readtables](https://common-lisp.net/project/named-readtables/)
+ [rutils](https://github.com/vseloved/rutils)
+ [trivial-shell](https://github.com/gwkkwg/trivial-shell)

The test package uses the [prove](https://github.com/fukamachi/prove) test library, and the documentation uses the [Codex](https://github.com/CommonDoc/codex) documentation library.

## Installation
```lisp
(ql:quickload :cl-bson)
```

## Bugs
If you find any bug or inconsistency in the code, or if you find it too hard to use, please, feel free to open an issue.

## Tests
This library is tested under [ABCL](https://common-lisp.net/project/armedbear/), [SBCL](http://www.sbcl.org/), [CCL](http://ccl.clozure.com/), [CLISP](http://www.clisp.org/) and [ECL](https://common-lisp.net/project/ecl/) Common Lisp implementations.

To run all the defined tests, use:
```lisp
* (asdf:test-system :cl-bson)
; prints lots of (colorful) stuff...
; => T
```

Tests are ran with [Travis CI](https://travis-ci.org/EuAndreh/cl-bson) using [cl-travis](https://github.com/luismbo/cl-travis) and [CIM](https://github.com/KeenS/CIM). Check it out!

## Author
+ [André Miranda](https://github.com/EuAndreh)

## License
[LLGPL](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext). 
