
(in-package :cl-user)

(uiop:quit (if (handler-case
                   (progn
                     (asdf:load-system :sas-parser.test)
                     (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :sas-parser))"))
)
                 (serious-condition (c)
                   (describe c)
                   (uiop:quit 2)))
               0 1))


