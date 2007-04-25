;;;; Generic form renderer
(in-package :weblocks)

(export '(*submit-control-name* *cancel-control-name* with-form-header
	  render-validation-summary render-form-controls
	  render-form-slot render-form))

(defparameter *submit-control-name* "submit"
  "The name of the control responsible for form submission.")

(defparameter *cancel-control-name* "cancel"
  "The name of the control responsible for cancellation of form
  submission.")

(defgeneric with-form-header (obj body-fn &rest keys &key name preslots-fn 
				  postslots-fn method &allow-other-keys)
  (:documentation
   "Responsible for rendering headers of a form
presentation. Similar to 'with-data-header'.

'method' - a submission method for the form. Normally :get
or :post.

Other keys are also accepted and supplied to functions called
internally (e.g. 'action'). See 'render-form-controls' for more
details."))

(defmethod with-form-header (obj body-fn &rest keys &key name
			     validation-errors preslots-fn
			     (postslots-fn #'render-form-controls)
			     (method :get)
			     &allow-other-keys)
  (let ((header-class (format nil "renderer form ~A"
			      (attributize-name (object-class-name obj))))
	(object-name (if (null name)
			 (humanize-name (object-class-name obj))
			 name)))
    (with-html
      (:form :class header-class :action "" :method (attributize-name method)
	     (with-extra-tags
	       (htm (:fieldset
		     (:h1 (:span :class "action" "Modifying:&nbsp;")
			  (:span :class "object" (str object-name)))
		     (render-validation-summary validation-errors)
		     (safe-apply preslots-fn obj keys)
		     (:h2 :class "form-fields-title" "Form fields:")
		     (:ul (funcall body-fn))
		     (safe-apply postslots-fn obj keys))))))))

(defun render-validation-summary (errors)
  "Renders a summary of validation errors on top of the form."
  (when errors
    (with-html
      (:div :class "validation-errors-summary"
	    (:h2 :class "error-count"
		 (let ((error-count (length errors)))
		   (if (eql error-count 1)
		       (str (format nil "There is 1 validation error:"))
		       (str (format nil "There are ~S validation errors:" error-count)))))
	    (:ul
	     (mapc (lambda (err)
		     (with-html
		       (:li
			(str (format nil "~A" (cdr err))))))
		   errors))))))

(defgeneric render-form-controls (obj &rest keys &key action &allow-other-keys)
  (:documentation
   "By default, this method renders 'Submit' and 'Cancel' buttons
for the form renderer. It is normally passed as 'postslots-fn' to
'with-form-header'. Override this method to render form controls
differently.

'obj' - the object being rendered.
'action' - an action created by 'make-action' to be called in
case the user clicks submit."))

(defmethod render-form-controls (obj &rest keys &key action &allow-other-keys)
  (with-html
    (:div :class "submit"
	  (:input :name "action" :type "hidden" :value action)
	  (:input :name *submit-control-name* :type "submit" :value "Submit")
	  (:input :name *cancel-control-name* :type "submit" :value "Cancel"))))

(defgeneric render-form-slot (obj slot-name slot-value &rest keys &key validation-errors &allow-other-keys)
  (:documentation
   "Renders a given slot of a particular object. Similar to
'render-data-slot'."))

(defmethod render-form-slot (obj slot-name (slot-value standard-object) &rest keys
			     &key validation-errors &allow-other-keys)
  (render-object-slot #'render-form #'render-form-slot obj slot-name slot-value keys))

(defmethod render-form-slot (obj slot-name slot-value &rest keys
			     &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name slot-name))
	 (validation-error (assoc attribute-slot-name validation-errors :test #'string-equal))
	 (field-class (when validation-error "item-not-validated")))
    (with-html
      (:li :class field-class
       (:label
	(:span (str (humanize-name slot-name)) ":&nbsp;")
	(apply #'render-form slot-value :name attribute-slot-name keys)
	(when validation-error
	  (htm (:p :class "validation-error"
		   (:em
		    (:span :class "validation-error-heading" "Error:&nbsp;")
		    (str (format nil "~A" (cdr validation-error))))))))))))

(defgeneric render-form (obj &rest keys &key inlinep name intermediate-fields &allow-other-keys)
  (:documentation
   "A generic form presentation renderer. Similar to
'render-data'.

In addition to other keys accepts a key 'validation-errors' which
optionally contains an association list of slot names and associated
errors that may have happened during a previous for submission."))

(defmethod render-form ((obj standard-object) &rest keys
			&key inlinep name intermediate-fields &allow-other-keys)
  (apply #'render-standard-object #'with-form-header #'render-form-slot obj keys))

(defmethod render-form (obj &rest keys &key inlinep name intermediate-fields &allow-other-keys)
  (let ((intermediate-value (assoc name intermediate-fields :test #'string-equal)))
    (with-html
      (:input :type "text" :name name :value (if intermediate-value
						 (cdr intermediate-value)
						 obj)))))

