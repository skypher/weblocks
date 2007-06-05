;;;; Generic form renderer
(in-package :weblocks)

(export '(*submit-control-name* *cancel-control-name* with-form-header
	  render-validation-summary render-form-controls
	  render-form-slot render-form required-validation-error))

(defparameter *submit-control-name* "submit"
  "The name of the control responsible for form submission.")

(defparameter *cancel-control-name* "cancel"
  "The name of the control responsible for cancellation of form
  submission.")

(defgeneric with-form-header (obj body-fn &rest keys &key name preslots-fn 
				  postslots-fn method action &allow-other-keys)
  (:documentation
   "Responsible for rendering headers of a form
presentation. Similar to 'with-data-header'.

'method' - a submission method for the form. Normally :get
or :post (defaults to :get).

'validation-errors' - if there were any errors during form validation,
they should be passed via this keyword parameter. The header will
present a summary via 'render-validation-summary'.

Other keys are also accepted and supplied to functions called
internally (e.g. 'action'). See 'render-form-controls' for more
details."))

(defmethod with-form-header (obj body-fn &rest keys &key
			     validation-errors preslots-fn
			     (postslots-fn #'render-form-controls)
			     (method :get) action
			     &allow-other-keys)
  (let ((header-class (format nil "renderer form ~A"
			      (attributize-name (object-class-name obj)))))
    (with-html
      (:form :class header-class :action "" :method (attributize-name method)
	     :onsubmit (format nil "initiateFormAction(\"~A\", $(this), \"~A\"); return false;"
			       action
			       (session-name-string-pair))
	     (with-extra-tags
	       (htm (:fieldset
		     (:h1 (:span :class "action" "Modifying:&nbsp;")
			  (:span :class "object" (str (humanize-name (object-class-name obj)))))
		     (render-validation-summary validation-errors)
		     (safe-apply preslots-fn obj keys)
		     (:h2 :class "form-fields-title" "Form fields:")
		     (:ul (funcall body-fn))
		     (safe-apply postslots-fn obj keys))))))))

(defun render-validation-summary (errors)
  "Renders a summary of validation errors on top of the form. Redefine
this function to render validation summary differently."
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
	  (:input :name *submit-control-name* :type "submit" :class "submit" :value "Submit"
		  :onclick "disableIrrelevantButtons(this);")
	  (:input :name *cancel-control-name* :type "submit" :class "submit cancel" :value "Cancel"
		  :onclick "disableIrrelevantButtons(this);")
	  (:input :name "action" :type "hidden" :value action))))

(defgeneric render-form-slot (obj slot-name slot-value &rest keys
				  &key human-name validation-errors &allow-other-keys)
  (:documentation
   "Renders a given slot of a particular object. Similar to
'render-data-slot'."))

(defmethod render-form-slot (obj slot-name (slot-value standard-object) &rest keys)
  (render-object-slot #'render-form #'render-form-slot obj slot-name slot-value keys))

(defmethod render-form-slot (obj slot-name slot-value &rest keys
			     &key (human-name slot-name) validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name slot-name))
	 (validation-error (assoc attribute-slot-name validation-errors :test #'string-equal))
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
       (:label
	(:span (str (humanize-name human-name)) ":&nbsp;")
	(apply #'render-form slot-value keys)
	(when validation-error
	  (htm (:p :class "validation-error"
		   (:em
		    (:span :class "validation-error-heading" "Error:&nbsp;")
		    (str (format nil "~A" (cdr validation-error))))))))))))

(defgeneric render-form (obj &rest keys &key inlinep name validation-errors
			     intermediate-fields &allow-other-keys)
  (:documentation
   "A generic form presentation renderer. Similar to
'render-data'.

'validation-errors' - an association list of slot names and associated
errors that may have happened during a previous for submission. These
are rendered in a summary within a header, as well as in particular
fields.

'intermediate-fields' - If there are any validation errors, it's bad
form to lose values the user has already
entered. 'intermediate-fields' should be a copy of the request, in
which case form renderer chooses values entered as part of the request
over values obtained from the object."))

(defmethod render-form ((obj standard-object) &rest keys)
  (apply #'render-standard-object #'with-form-header #'render-form-slot obj keys))

(defmethod render-form (obj &rest keys &key inlinep slot-path intermediate-fields &allow-other-keys)
  (let* ((slot-name (attributize-name (last-item slot-path)))
	 (intermediate-value (assoc slot-name intermediate-fields :test #'string-equal)))
    (with-html
      (:input :type "text" :name slot-name :value (if intermediate-value
								  (cdr intermediate-value)
								  obj)))))

