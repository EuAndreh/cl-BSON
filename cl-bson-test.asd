(defsystem "cl-bson-test"
  :name "cl-BSON-test"
  :version "0.1.0"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/cl-bson"
  :bug-tracker "https://github.com/EuAndreh/cl-bson/issues"
  :source-control (:git "git@github.com:EuAndreh/cl-bson.git")
  :license "LLGPL"
  :description "Test system for cl-BSON."
  :depends-on ("cl-bson"
               "prove")
  :components ((:module "t"
                        :components
                        ((:test-file "cl-bson"))))
  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (o c) (symbol-call :prove-asdf :run-test-system c)))
