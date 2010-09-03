
(in-package weblocks)

(export '(*max-password-length* password password-presentation))

(defparameter *max-password-length* 12
  "Default maximum for the length of the password field")

(defclass password-presentation (text-presentation input-presentation)
  ((max-length :initform *max-password-length*))
  (:documentation "A presentation for passwords."))

(defmethod render-view-field-value (value (presentation password-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &key field-info &allow-other-keys)
  (declare (ignore args)
	   (special *presentation-dom-id*))
  (render-password (if field-info
                     (attributize-view-field-name field-info)
                     (attributize-name (view-field-slot-name field)))
                   nil
		   :maxlength (input-presentation-max-length presentation)
                   :size (input-presentation-size presentation)
		   :id *presentation-dom-id*))

(defmethod render-view-field-value ((value null) (presentation password-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &key field-info &allow-other-keys)
  (declare (ignore args)
	   (special *presentation-dom-id*))
  (render-password (if field-info
                     (attributize-view-field-name field-info)
                     (attributize-name (view-field-slot-name field)))
                   nil
		   :maxlength (input-presentation-max-length presentation)
                   :size (input-presentation-size presentation)
		   :id *presentation-dom-id*))

(defmethod print-view-field-value (value (presentation password-presentation)
				   field view widget obj &rest args)
  (declare (ignore presentation obj view field args))
  (format nil "*******"))

