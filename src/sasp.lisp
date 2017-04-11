(in-package :sas-parser)

(defvar *sg*)
(section-parser "SG" *sg* #'read-sg (dtg-sections)
  (warn "this is a translator file")
  (finalize-translator))
(defvar *dtgs*)
(defun dtg-sections ()
  (let ((*dtgs*
         (iter
           (with *total* = (length *variables*))
           (for *count* below *total*)
           (collect (dtg-section) result-type 'vector))))
    (cg-section)))
(section-parser "DTG" g #'read-dtg g
  (error "insufficient number of dtg sections!"))
(defvar *cgs*)
(section-parser "CG" *cgs* #'read-cg (finalize)
  (error "missing CG section!"))

(defun finalize ()
  (values *version*
          (sas *metric* *variables* *mutex-groups* *operators* *states* *goals*)
          (list :successor-generator *sg*
                :domain-transition-graph *dtgs*
                :causal-graph *cgs*)))

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
  (let ((var (elt *variables* *count*)))
    (iter outer
          (for from
               in-vector (variable-values var)
               with-index i)
          (iter (repeat (read))
                (for to = (elt (variable-values var) (read)))
                (in outer
                    (collect (read-transition from to) result-type 'vector))))))

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
