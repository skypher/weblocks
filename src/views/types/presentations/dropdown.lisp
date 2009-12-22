
(in-package :weblocks)

(export '(dropdown dropdown-presentation
	  dropdown-presentation-welcome-name))

;;; Dropdown
(defclass dropdown-presentation (form-presentation choices-presentation-mixin)
  ((welcome-name :initarg :welcome-name
		 :accessor dropdown-presentation-welcome-name
		 :documentation "If bound, uses this value to
		 present a welcome message in the form of [Select
		 Welcome-Name] as the first choice. By default uses
		 the view field label.")))

(defmethod render-view-field-value (value (presentation dropdown-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (ignore args)
	   (special *presentation-dom-id*))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-dropdown (if field-info
                       (attributize-view-field-name field-info)
                       (attributize-name (view-field-slot-name field)))
		     (obtain-presentation-choices presentation obj)
		     :welcome-name (if value
				       (if (form-view-field-required-p field)
					   nil "None")
                                      (if (slot-boundp presentation 'welcome-name) 
                                        (dropdown-presentation-welcome-name presentation)
                                        (view-field-label field)))
		     :selected-value (if intermediate-value-p
					 intermediate-value
					 (when value
					   (presentation-choices-default-value-key value)))
		     :id *presentation-dom-id*)))

