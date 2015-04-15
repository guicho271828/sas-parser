#|
  This file is a part of sas-parser project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage sas-parser
  (:use :cl :trivia.ppcre :cl-ppcre :eazy-process :trivia :alexandria)
  (:shadow :variable)
  (:export :parse))
(in-package :sas-parser)

;; blah blah blah.

(defun parse (pathname)
  (with-open-file (*standard-input* pathname)
    (begin)))

(defpattern begin (entity)
  `(ppcre "begin_(.*)" ,entity))
(defpattern end (entity)
  `(ppcre "end_(.*)" ,entity))

(defun int (str)
  (parse-integer str :junk-allowed t))

(define-condition sas-parse-error (simple-error)
  ((entity :initarg :entity :reader entity))
  (:report (lambda (s c)
             (format s "begin_~a does not end with end_~a !" (entity c) (entity c)))))
(defun sas-parse-error (entity)
  (error 'sas-parse-error :entity entity))

(defmacro section-parser (name var parser-fn next &body if-not-exists)
  (let ((fn-name (intern (substitute #\- #\_ (string-upcase name)))))
    `(progn
       (defvar ,var)
       (defun ,fn-name ()
         (match (read-line)
           ((begin ,name)
            (let ((,var (,parser-fn)))
              (match (read-line)
                ((end ,name)
                 ,next)
                (_ (sas-parse-error ,name)))))
           (_ ,@if-not-exists))))))

(section-parser "version" *version* read (metric) (error "version 1 or 2 !"))
(section-parser "metric" *metric* read (variable-num) (error "version 1 !"))

(defvar *variable-num*)
(defun variable-num ()
  (let ((*variable-num* (read)))
    (assert (numberp *variable-num*))
    (variable)))

(section-parser "variable" *variables* read-mutex-group
                (let ((*variable-num* (1- *variable-num*)))
                  (if (plusp *variable-num*)
                      (variable)
                      (mutex-group)))
                (error "insufficient number of variable sections !"))

;; (defstruct variable name axiom range atoms)
;; (make-variable
;;  :name name
;;  :axiom (int axiom)
;;  :range (int range)
;;  :atoms (mapcar #'parse-atom atoms))

(defvar *mutex-group-num*)
(defun mutex-group-num ()
  (let ((*mutex-group-num* (read)))
    (assert (numberp *mutex-group-num*))
    (mutex-group)))

(section-parser "mutex_group" *mutex-groups* read-mutex-group
    (let ((*mutex-group-num* (1- *mutex-group-num*)))
      (if (plusp *mutex-group-num*)
          (mutex-group)
          (state)))
  (error "insufficient number of mutex_group sections !"))

(section-parser "state" *states* read-states (goal) (error "missing states!"))
(section-parser "goal" *goals* read-goals (operator) (error "missing goals!"))

(defvar *operator-num*)
(defun operator-num ()
  (let ((*operator-num* (read)))
    (assert (numberp *operator-num*))
    (operator)))

(section-parser "operator" *operators* read-operator
    (let ((*operator-num* (1- *operator-num*)))
      (if (plusp *operator-num*)
          (operator)
          (rule)))
  (error "insufficient number of operator sections!"))

(defun operator (s name prevails-num prevails effects-num effects cost)
  )


(defvar *rule-num*)
(defun rule-num ()
  (let ((*rule-num* (read)))
    (assert (numberp *rule-num*))
    (rule)))

(section-parser "rule" *rules* read-rule
    (let ((*rule-num* (1- *rule-num*)))
      (if (plusp *rule-num*)
          (rule)
          (sg)))
  (error "insufficient number of rule sections!"))




(section-parser "SG" *SGs* read-SG
    (let ((*SG-num* (1- *SG-num*)))
      (if (plusp *SG-num*)
          (SG)
          (sg)))
  (error "insufficient number of SG sections!"))


(section-parser "DTG" *DTGs* read-DTG
    (let ((*DTG-num* (1- *DTG-num*)))
      (if (plusp *DTG-num*)
          (DTG)
          (sg)))
  (error "insufficient number of DTG sections!"))


(section-parser "CG" *CGs* read-CG
    (let ((*CG-num* (1- *CG-num*)))
      (if (plusp *CG-num*)
          (CG)
          (sg)))
  (error "insufficient number of CG sections!"))

