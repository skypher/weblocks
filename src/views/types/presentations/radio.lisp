
(in-package :weblocks)

(export '(radio radio-presentation))

;;; Radio buttons
(defclass radio-presentation (form-presentation choices-presentation-mixin)
  ())

(defmethod render-view-field ((field form-view-field) (view form-view)
			      widget (presentation radio-presentation) value obj
			      &rest args &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
	 (validation-error (assoc attribute-slot-name validation-errors
				  :test #'string-equal
				  :key #'view-field-slot-name))
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
	   (:span :class "label"
		  (:span :class "slot-name"
			 (:span :class "extra"
				(str (view-field-label field)) ":&nbsp;")))
	   (apply #'render-view-field-value
		  value presentation
		  field view widget obj
		  args)
	   (when validation-error
	     (htm (:p :class "validation-error"
		      (:em
		       (:span :class "validation-error-heading" "Error:&nbsp;")
		       (str (format nil "~A" (cdr validation-error)))))))))))

(defmethod render-view-field-value (value (presentation radio-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values &allow-other-keys)
  (declare (ignore args)
	   (special *presentation-dom-id*))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-radio-buttons (view-field-slot-name field)
			  (obtain-presentation-choices presentation obj)
			  :selected-value (if intermediate-value-p
					      intermediate-value
					      (when value
						(attributize-name value)))
			  :id *presentation-dom-id*)))

