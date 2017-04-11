#|
  This file is a part of sas-parser project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Fast Downward SAS parser for common lisp

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem sas-parser
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia.ppcre :cl-ppcre :trivia :alexandria :iterate)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "sasp"))))
  :description "Fast Downward SAS parser for common lisp"
  :in-order-to ((test-op (test-op :sas-parser.test))))
