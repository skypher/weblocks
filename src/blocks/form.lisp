
(in-package :weblocks)

(export '(*submit-control-name* *cancel-control-name* with-html-form))

(defparameter *submit-control-name* "submit"
  "The name of the control responsible for form submission.")

(defparameter *cancel-control-name* "cancel"
  "The name of the control responsible for cancellation of form
  submission.")

(defmacro with-html-form ((method-type action-code &key id class) &body body)
  "Transforms to cl-who (:form) with standard form code (AJAX support, actions, etc.)"
  `(with-html
     (:form :id ,id :class ,class :action "" :method (attributize-name ,method-type)
	    :onsubmit (format nil "initiateFormAction(\"~A\", $(this), \"~A\"); return false;"
			      (url-encode ,action-code)
			      (session-name-string-pair))
	    (with-extra-tags
	      (htm (:fieldset
		    ,@body
		    (:input :name "action" :type "hidden" :value (url-encode ,action-code))))))))

