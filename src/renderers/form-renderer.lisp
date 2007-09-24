;;;; Generic form renderer
(in-package :weblocks)

(export '(*form-error-summary-threshold* with-form-header
	  render-validation-summary render-form-controls
	  render-form-slot render-form form-print-object
	  render-form-value required-validation-error
	  slot-intermedia-value))

(defparameter *form-error-summary-threshold* 15
  "When the number of fields in a form is longer than this threshold,
an error summary is rendered at top whenever applicable.")

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
			     (title-action "Modifying")
			     &allow-other-keys)
  (let ((header-class (format nil "renderer form ~A"
			      (attributize-name (object-class-name obj)))))
    (when (>= (apply #'object-full-visible-slot-count obj keys)
	      *form-error-summary-threshold*)
      (setf header-class (concatenate 'string header-class " long-form")))
    (with-html-form (method action :class header-class)
      (:h1 (:span :class "action" (str (concatenate 'string title-action ":&nbsp;")))
	   (:span :class "object" (str (humanize-name (object-class-name obj)))))
      (render-validation-summary validation-errors)
      (safe-apply preslots-fn obj keys)
      (:h2 :class "form-fields-title" "Form fields:")
      (:ul (funcall body-fn))
      (safe-apply postslots-fn obj keys))))

(defun render-validation-summary (errors)
  "Renders a summary of validation errors on top of the form. This
function can be redefined to render validation summary differently."
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
	  (render-button *submit-control-name*)
	  (render-button *cancel-control-name* :class "submit cancel"))))

(defgeneric render-form-slot (obj slot-name slot-type slot-value &rest keys
				  &key human-name validation-errors &allow-other-keys)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Renders a given slot of a particular object. Similar to
'render-data-slot'."))

(defslotmethod render-form-slot (obj slot-name slot-type slot-value &rest keys
				     &key (human-name slot-name) validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name slot-name))
	 (validation-error (assoc attribute-slot-name validation-errors :test #'string-equal))
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
	   (:label
	     (:span :class "slot-name"
		    (:span :class "extra"
		     (str (humanize-name human-name)) ":&nbsp;"
		     (when (slot-value-required-p (class-name (class-of obj)) slot-name)
		       (htm (:em :class "required-slot" "(required)&nbsp;")))))
	    (apply #'render-form-value obj slot-name slot-type slot-value keys)
	    (when validation-error
	      (htm (:p :class "validation-error"
		       (:em
			(:span :class "validation-error-heading" "Error:&nbsp;")
			(str (format nil "~A" (cdr validation-error))))))))))))

(defun render-form (obj &rest keys &key parent-object slot-name
		    (slot-type t) &allow-other-keys)
  "A convinient wrapper for 'render-form-value'.

See 'render-data' for examples."
  (apply #'render-standard-object #'with-form-header #'render-form-slot obj keys))


(defun slot-intermedia-value (slot-name intermediate-fields)
  "Returns an intermediate field for 'slot-name' from
'intermediate-fields'. If the field is not present, returns
nil. Otherwise returns a cons cell the car of which is slot-name and
the cdr is the intermediate value."
  (assoc (attributize-name slot-name) intermediate-fields :test #'string-equal))

(defgeneric form-print-object (obj slot-name slot-type slot-value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Prints 'slot-value' to a string that will be used to render the
value in a form renderer. This function is called by
'render-form-value'. Default implementation returns nil if
'slot-value' is nil, otherwise calls 'data-print-object'. Specialize
this function to customize simple data printing without having to
specialize more heavy 'render-data-aux'."))

(defslotmethod form-print-object (obj slot-name slot-type slot-value &rest args)
  (when slot-value
    (apply #'data-print-object obj slot-name slot-type slot-value args)))

(defgeneric render-form-value (obj slot-name slot-type slot-value &rest
				 keys &key name validation-errors
				 intermediate-fields
				 &allow-other-keys)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "A generic form presentation renderer. Similar to
'render-data-aux'.

'validation-errors' - an association list of slot names and associated
errors that may have happened during a previous for submission. These
are rendered in a summary within a header, as well as in particular
fields.

'intermediate-fields' - If there are any validation errors, it's bad
form to lose values the user has already
entered. 'intermediate-fields' should be a copy of the request, in
which case form renderer chooses values entered as part of the request
over values obtained from the object."))

(defslotmethod render-form-value (obj slot-name slot-type (slot-value standard-object) &rest keys)
  (let* ((name (object-name slot-value))
	 (type (normalized-type-of name)))
    (render-form-value obj slot-name type name)))

(defslotmethod render-form-value (obj slot-name slot-type slot-value &rest
				    keys &key slot-path intermediate-fields &allow-other-keys)
  (let ((attributized-slot-name (attributize-name (if slot-name slot-name (last-item slot-path))))
	(intermediate-value (slot-intermedia-value slot-name intermediate-fields)))
    (with-html
      (:input :type "text" :name attributized-slot-name
	      :value (if intermediate-value
			 (cdr intermediate-value)
			 (apply #'form-print-object obj slot-name
				slot-type slot-value keys))
	      :maxlength (max-raw-slot-input-length obj slot-name slot-type)))))

