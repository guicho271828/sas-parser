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

;;; api

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
            (let ((,var (funcall ,parser-fn)))
              (match (read-line)
                ((end ,name)
                 (funcall ,next))
                (_ (sas-parse-error ,name)))))
           (_ ,@if-not-exists))))))

;;; parse sections

(defvar *count*)
(defun call/counter (fn &optional (reader #'read))
  (lambda ()
    (let ((*count* (funcall reader)))
      (assert (numberp *count*))
      (funcall fn))))
(defun next-section (continue next)
  (lambda ()
    (let ((*count* (1- *count*)))
      (if (plusp *count*)
          (funcall continue)
          (funcall next)))))

(section-parser "version" *version* #'read #'metric
  (error "version 1 or 2 !"))
(section-parser "metric" *metric* #'read (call/counter #'variable)
  (error "version 1 !"))
(section-parser "variable" *variables* #'read-variable (next-section #'variable (call/counter #'mutex-group))
  (error "insufficient number of variable sections !"))
(section-parser "mutex_group" *mutex-groups* #'read-mutex-group (next-section #'mutex-group #'state)
  (error "insufficient number of mutex_group sections !"))
(section-parser "state" *states* #'read-states #'goal
  (error "missing states!"))
(section-parser "goal" *goals* #'read-goals (call/counter #'operator)
  (error "missing goals!"))
(section-parser "operator" *operators* #'read-operator (next-section #'operator (call/counter #'rule))
  (error "insufficient number of operator sections!"))
(section-parser "rule" *rules* #'read-rule (next-section #'rule #'sg)
  (error "insufficient number of rule sections!"))
(section-parser "SG" *sgs* #'read-sg (next-section #'sg (call/counter #'dtg (lambda () (length *variables*))))
  (error "insufficient number of sg sections!"))
(section-parser "DTG" *dtgs* #'read-dtg (next-section #'dtg #'cg)
  (error "insufficient number of dtg sections!"))
(section-parser "CG" *cgs* #'read-cg #'finalize
  (error "insufficient number of cg sections!"))

(defun finalize ()
  )

;; (defstruct variable name axiom range atoms)
;; (make-variable
;;  :name name
;;  :axiom (int axiom)
;;  :range (int range)
;;  :atoms (mapcar #'parse-atom atoms))

;; (defun operator (s name prevails-num prevails effects-num effects cost)
;;   )

;;; contents
