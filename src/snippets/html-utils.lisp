
(in-package :weblocks)

(export '(*submit-control-name* *cancel-control-name* with-html-form
	  render-link))

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
		    (:input :name *action-string* :type "hidden" :value ,action-code)))))))

(defun render-link (action-code name)
  "Renders an action into an href link. The link will be rendered in
such a way that the action will be invoked via AJAX, or will fall back
to regular request if JavaScript is not available. When the user
clicks on the link, the action will be called on the server.

'action-code' - The action created with 'make-action'.
'name' - A string that will be presented to the user in the
link."
  (let ((url (make-action-url action-code)))
    (with-html
      (:a :href url :onclick (format nil "initiateAction(\"~A\", \"~A\"); return false;"
				     action-code (session-name-string-pair))
	  (str name)))))

