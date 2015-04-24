#|
  This file is a part of sas-parser project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#
;;; package
(in-package :cl-user)
(defpackage sas-parser
  (:use :cl :trivia.ppcre :cl-ppcre :eazy-process :trivia :alexandria :iterate
        :immutable-struct)
  (:shadow :variable :next)
  (:shadowing-import-from :immutable-struct :ftype :defstruct)
  (:export :parse))
(in-package :sas-parser)

;;; api
(defvar *pathname*)
(defun parse (*pathname*)
  (with-open-file (*standard-input* *pathname*)
    (version-section)))

(defpattern begin (entity)
  `(ppcre "begin_(.*)" ,entity))
(defpattern end (entity)
  `(ppcre "end_(.*)" ,entity))

(defun int (str)
  (parse-integer str :junk-allowed t))

(define-condition sas-parse-error (simple-error)
  ((entity :initarg :entity :reader entity))
  (:report (lambda (c s)
             (format s "begin_~a does not end with end_~a !" (entity c) (entity c)))))
(defun sas-parse-error (entity)
  (error 'sas-parse-error :entity entity))

(defmacro section-parser (name var parser-fn next &body if-not-exists)
  (let ((fn-name (intern (substitute #\- #\_ (string-upcase (concatenate 'string name "-section"))))))
    `(progn
       (defvar ,var nil)
       (defun ,fn-name ()
         (match (read-line *standard-input* nil)
           ((begin ,name)
            (let ((,var (funcall ,parser-fn)))
              (match (read-line)
                ((end ,name)
                 (funcall ,next))
                (_ (sas-parse-error ,name)))))
           (_ ,@if-not-exists))))))

;;; parse sections

(defvar *count*)
(defvar *total*)
(defun call/counter (continue next &optional (reader #'read))
  (lambda ()
    (let* ((*total* (funcall reader)) (*count* -1))
      (assert (numberp *total*))
      (funcall (next-section continue next)))))
(defun next-section (continue next)
  (lambda ()
    (let ((*count* (1+ *count*)))
      (if (< *count* *total*)
          (funcall continue)
          (funcall next)))))

(section-parser "version" *version* #'read #'metric-section
  (error "version 1 or 2 !"))
(section-parser "metric" *metric* #'read
    (call/counter #'variable-section #'mutex-group-section)
  (error "version 1 !"))
(section-parser "variable" *variables* #'read-variable
    (next-section #'variable-section
                  (call/counter #'mutex-group-section #'state-section))
  (error "insufficient number of variable sections !"))
(section-parser "mutex_group" *mutex-groups* #'read-mutex-group
    (next-section #'mutex-group-section #'state-section)
  (error "insufficient number of mutex_group sections !"))
(section-parser "state" *states* #'read-state #'goal-section
  (error "missing states!"))
(section-parser "goal" *goals* #'read-goal
    (call/counter #'operator-section #'rule-section)
  (error "missing goals!"))
(section-parser "operator" *operators* #'read-operator
    (next-section #'operator-section (call/counter #'rule-section #'sg-section))
  (error "insufficient number of operator sections!"))
(section-parser "rule" *rules* #'read-rule (next-section #'rule-section #'sg-section)
  (error "insufficient number of rule sections!"))
(section-parser "SG" *sg* #'read-sg
    (call/counter #'dtg-section #'cg-section (lambda () (length *variables*)))
  (warn "this is a translator file")
  (finalize))
(section-parser "DTG" *dtgs* #'read-dtg (next-section #'dtg-section #'cg-section)
  (error "insufficient number of dtg sections!"))
(section-parser "CG" *cgs* #'read-cg #'finalize
  (error "missing CG section!"))

(defun finalize ()
  )

;;;; section reader

(defstruct variable name axiom-layer values)

(defun read-variable ()
  (let* ((*variables* (or *variables* (make-array *total* :fill-pointer 0)))
         (name (read))
         (axiom-layer (read))
         (range (read))
         (values (read-values range)))
    (vector-push (variable name axiom-layer values) *variables*)
    *variables*))

(defun read-values (range)
  (iter (for i below range)
        (collect (read-value) result-type vector)))

(defun read-value ()
  (ematch (read-line)
    ((ppcre "Atom ([^\\(]*)\\((.*)\\)" name args)
     (sas-atom (read-from-string name) (mapcar #'read-from-string (split ",\\s*" args))))
    ((ppcre "NegatedAtom ([^\\(]*)\\((.*)\\)" name args)
     (negated-atom (read-from-string name) (mapcar #'read-from-string (split ",\\s*" args))))
    ("<none of those>"
     (sas-atom :none-of-those nil))))

(defstruct sas-atom name args)
(defstruct (negated-atom (:include sas-atom)))

(defun read-fixed-number-of-atoms ()
  (iter (for i below (read))
        ;; one line for each fact
        (ematch (elt *variables* (read))
          ((variable values)
           (collecting (elt values (read)) result-type 'vector)))))

(defun read-mutex-group ()
  (let ((*mutex-groups* (or *mutex-groups* (make-array *total* :fill-pointer 0))))
    (vector-push (read-fixed-number-of-atoms) *mutex-groups*)
    *mutex-groups*))

(defun read-state ()
  (iter (for var in-vector *variables* with-index i)
        (ematch var
          ((variable values)
           (collecting (elt values (read)) result-type 'vector)))))

(defun read-goal ()
  (read-fixed-number-of-atoms))

(defstruct operator name args prevail effects cost)
(defun read-operator ()
  (with-input-from-string (s (read-line))
    (let* ((name (read s))
           (args (iter (for arg = (read s nil))
                       (while arg)
                       (collect arg)))
           (prevail (read-fixed-number-of-atoms))
           (effects (read-effects))
           (cost (read)))
      (let ((*operators* (or *operators* (make-array *total* :fill-pointer 0))))
        (vector-push (operator name args prevail effects cost) *operators*)
        *operators*))))

(defstruct effect conditions affected require newval)

(defun read-effects ()
  (iter (for i below (read))
        (let ((effect-conditions (read-fixed-number-of-atoms)))
          (multiple-value-bind (aff req new) (read-effect-transition)
            (collect
                (effect effect-conditions aff req new)
              result-type 'vector)))))

(defun read-effect-transition ()
  (let* ((affected (elt *variables* (read)))
         (require
          (ematch (read)
            (-1 :*)
            (valnum (elt (variable-values affected) valnum))))
         (newval (read)))
    (values affected require newval)))

(defun read-rule ()
  (let ((body (read-fixed-number-of-atoms)))
    (multiple-value-bind (aff req new) (read-effect-transition)
      (let ((*rules* (or *rules* (make-array *total* :fill-pointer 0))))
        (vector-push (operator :axiom body (effect (vector) aff req new) 0) *rules*)
        *rules*))))


(defstruct generator-switch switch immediate-ops generator-for-value default-generator)
(defstruct generator-generator op)

(defun read-sg ()
  "Reimplementation of successor_generator.cc in FD"
  (labels ((main ()
             (let ((keyword (read)))
               (cond
                 ((string= keyword "SWITCH") (read-switch (read)))
                 ((string= keyword "CHECK") (read-check (read))))))
           (read-switch (switch-var)
             (generator-switch switch-var
                               (main)
                               (iter (for i below (length (variable-values (elt *variables* switch-var))))
                                     (collect (main) result-type 'vector))
                               (main)))
           (read-check (count)
             (generator-generator
              (iter (for i below count)
                    (collect (elt *operators* (read)) result-type 'vector)))))
    (main)))

(defun read-dtg ()
  (let ((*dtgs* (or *dtgs* (make-array (length *variables*) :fill-pointer 0))))
    (let ((var (elt *variables* *count*)))
      (vector-push
       (iter outer
             (for from
                  in-vector (variable-values var)
                  with-index i)
             (iter (repeat (read))
                   (for to = (elt (variable-values var) (read)))
                   (in outer
                       (collect (read-transition from to) result-type 'vector))))
       *dtgs*)
      *dtgs*)))

(defstruct transition from to op conditions)
(defun read-transition (from to)
  (transition from to
              (elt *operators* (read))
              (read-fixed-number-of-atoms)))

(defstruct cause from to weight)
(defun read-cg ()
  (iter outer
        (for var in-vector *variables*)
        (iter (repeat (read))
              (in outer
                  (collect
                      (cause var (elt *variables* (read)) (read))
                    result-type 'vector)))))


;; (defstruct variable name axiom range atoms)
;; (make-variable
;;  :name name
;;  :axiom (int axiom)
;;  :range (int range)
;;  :atoms (mapcar #'parse-atom atoms))

;; (defun operator (s name prevails-num prevails effects-num effects cost)
;;   )

;;; contents
