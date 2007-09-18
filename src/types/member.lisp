
(in-package :weblocks)

(defslotmethod render-form-slot (obj slot-name (slot-type (eql 'member)) slot-value &rest keys
				     &key (human-name slot-name) validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name slot-name))
	 (validation-error (assoc attribute-slot-name validation-errors :test #'string-equal))
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
	   (:span :class "label"
		  (:span :class "slot-name"
			 (:span :class "extra"
				(str (humanize-name human-name)) ":&nbsp;")))
	   (apply #'render-form-aux obj slot-name slot-type slot-value keys)
	   (when validation-error
	     (htm (:p :class "validation-error"
		      (:em
		       (:span :class "validation-error-heading" "Error:&nbsp;")
		       (str (format nil "~A" (cdr validation-error)))))))))))

(defslotmethod render-form-aux (obj slot-name (slot-type (eql 'member)) slot-value &rest
			    keys &key intermediate-fields &allow-other-keys)
  (let ((intermediate-value (slot-intermedia-value slot-name intermediate-fields)))
    (render-radio-buttons slot-name (mapcar (lambda (i)
					      (cons (humanize-name (format nil "~A" i))
						    (attributize-name (format nil "~A" i))))
					    (cdr slot-type))
			  :selected-value (if intermediate-value
					      (cdr intermediate-value)
					      (when slot-value
						(attributize-name slot-value))))))

