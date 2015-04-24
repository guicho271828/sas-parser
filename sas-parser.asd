#|
  This file is a part of sas-parser project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Fast Downward SAS parser for common lisp

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage sas-parser-asd
  (:use :cl :asdf))
(in-package :sas-parser-asd)


(defsystem sas-parser
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia.ppcre :cl-ppcre :trivia :alexandria :immutable-struct)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "Fast Downward SAS parser for common lisp"
  :in-order-to ((test-op (load-op :sas-parser.test))))
