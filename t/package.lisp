#|
  This file is a part of sas-parser project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :sas-parser.test
  (:use :cl
        :sas-parser
        :fiveam
        :trivia.ppcre :cl-ppcre :eazy-process :trivia :alexandria))
(in-package :sas-parser.test)



(def-suite :sas-parser)
(in-suite :sas-parser)

;; run test with (run! test-name) 

(test sas-parser

  )



