
(in-package :weblocks)

(export '(predicate predicate-presentation
	  predicate-presentation-false-string
	  predicate-presentation-true-string
	  checkbox checkbox-presentation
	  predicate-parser))

;;; Data presentation
(defclass predicate-presentation (text-presentation)
  ((false-string :initform "No"
		 :accessor predicate-presentation-false-string
		 :initarg :false-string
		 :documentation "A string to be printed when the
		 predicate is false.")
   (true-string :initform "Yes"
		:accessor predicate-presentation-true-string
		:initarg :true-string
		:documentation "A string to be printed when the
		 predicate is true."))
  (:documentation "A default presentation that renders values as
  predicates, where nil is treated as false, and any other value is
  treated as true."))

(defmethod render-view-field-value ((value null) (presentation predicate-presentation)
				    field view widget obj &rest args)
  (apply #'call-next-method value presentation field view widget obj
	 :ignore-nulls-p t
	 args))

(defmethod print-view-field-value (value (presentation predicate-presentation)
				   field view widget obj &rest args)
  (declare (ignore args))
  (if value
      (predicate-presentation-true-string presentation)
      (predicate-presentation-false-string presentation)))

;;; Form presentation
(defclass checkbox-presentation (form-presentation)
  ()
  (:documentation "Treats values as predicates and presents them as a
  checkbox."))

(defmethod render-view-field-value (value (presentation checkbox-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (ignore args)
	   (special *presentation-dom-id*))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-checkbox (if field-info
                       (attributize-view-field-name field-info)
                       (attributize-name (view-field-slot-name field)))
		     (if intermediate-value-p
			 intermediate-value
			 value)
		     :id *presentation-dom-id*)))

;;; Parser
(defclass predicate-parser (parser)
  ((error-message :initform "checked or unchecked"))
  (:documentation "A parser designed to parse strings into
  predicates."))

(defmethod parse-view-field-value ((parser predicate-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (cond
    ((member value '("t" "f") :test #'string-equal) (values t t t))
    ((null value) (values t t nil))
    (t nil)))

;;; Scaffolding magic
(defmethod typespec->view-field-presentation ((scaffold scaffold)
					      (typespec (eql 'boolean)) args)
  (values t (make-instance 'predicate-presentation)))

(defmethod typespec->view-field-presentation ((scaffold-type form-scaffold)
					      (typespec (eql 'boolean)) args)
  (values t (make-instance 'checkbox-presentation)))

(defmethod typespec->form-view-field-parser ((scaffold-type form-scaffold)
					     (typespec (eql 'boolean)) args)
  (values t (make-instance 'predicate-parser)))

