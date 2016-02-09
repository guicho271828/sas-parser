#|
  This file is a part of sas-parser project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage sas-parser.test-asd
  (:use :cl :asdf))
(in-package :sas-parser.test-asd)


(defsystem sas-parser.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:sas-parser
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c)
                    (eval (read-from-string
                           "(every #'fiveam::TEST-PASSED-P (5am:run! :sas-parser))"))))
