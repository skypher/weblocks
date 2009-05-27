
(in-package :weblocks)

(export '(*textarea-rows* *textarea-cols* *max-textarea-input-length*
	  textarea textarea-presentation textarea-presentation-rows
	  textarea-presentation-cols))

;;; some defaults
(defparameter *textarea-rows* 10
  "Default number of rows rendered in textarea")

(defparameter *textarea-cols* 50
  "Default number of columns rendered in textarea")

(defparameter *max-textarea-input-length* 200
  "Maximum number of characters that can be entered before the server complains.")

;;; textarea
(defclass textarea-presentation (input-presentation)
  ((max-length :initform *max-textarea-input-length*)
   (rows :initform *textarea-rows*
	 :accessor textarea-presentation-rows
	 :initarg :rows
	 :documentation "Number of rows in the text area.")
   (cols :initform *textarea-cols*
	 :accessor textarea-presentation-cols
	 :initarg :cols
	 :documentation "Number of columns in the text area."))
  (:documentation "Present values in a text area HTML control."))

(defmethod render-view-field-value (value (presentation textarea-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-textarea (if field-info
                       (attributize-view-field-name field-info)
                       (attributize-name (view-field-slot-name field)))
		     (if intermediate-value-p
			 intermediate-value
			 (apply #'print-view-field-value value presentation
				field view widget obj args))
		     (textarea-presentation-rows presentation)  
		     (textarea-presentation-cols presentation)
		     :id *presentation-dom-id*)))
