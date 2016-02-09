#|
  This file is a part of sas-parser project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :sas-parser.test
  (:use :cl
        :sas-parser
        :fiveam :alexandria))
(in-package :sas-parser.test)



(def-suite :sas-parser)
(in-suite :sas-parser)

;; run test with (run! test-name) 

(test translater-output
  (for-all ((path (lambda ()
                    (random-elt
                     (directory
                      (merge-pathnames
                       "t/translated/*.sas"
                       (asdf:system-source-directory :sas-parser)))))))
    (handler-bind ((warning #'muffle-warning)
                   (error (lambda (c) (declare (ignorable c)) (print path))))
      (finishes (parse path)))))



(test preprocessor-output
  (for-all ((path (lambda ()
                    (random-elt
                     (directory
                      (merge-pathnames
                       "t/preprocessed/*.sasp"
                       (asdf:system-source-directory :sas-parser)))))))
    (handler-bind ((warning #'muffle-warning)
                   (error (lambda (c) (declare (ignorable c)) (print path))))
      (finishes (parse path)))))

