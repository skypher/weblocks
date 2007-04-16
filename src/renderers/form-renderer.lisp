;;;; Generic form renderer
(in-package :weblocks)

(export '(*submit-control-name* with-form-header
	  render-form-controls render-form-slot render-form))

(defparameter *submit-control-name* "submit"
  "The name of the control responsible for form submission.")

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

(defmethod with-form-header (obj body-fn
			     &rest keys &key name preslots-fn
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
		     (safe-apply preslots-fn obj keys)
		     (:ul (funcall body-fn))
		     (safe-apply postslots-fn obj keys))))))))

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
	  (:input :name "cancel" :type "submit" :value "Cancel"))))

(defgeneric render-form-slot (obj slot-name slot-value &rest args)
  (:documentation
   "Renders a given slot of a particular object. Similar to
'render-data-slot'."))

(defmethod render-form-slot (obj slot-name (slot-value standard-object) &rest args)
  (render-object-slot #'render-form #'render-form-slot obj slot-name slot-value args))

(defmethod render-form-slot (obj slot-name slot-value &rest args)
  (let ((attribute-slot-name (attributize-name slot-name)))
    (with-html
      (:li (:label
		   (:span (str (humanize-name slot-name)) ":&nbsp;")
		   (apply #'render-form slot-value :name attribute-slot-name args))))))

(defgeneric render-form (obj &rest keys &key inlinep name &allow-other-keys)
  (:documentation
   "A generic form presentation renderer. Similar to
'render-data'."))

(defmethod render-form ((obj standard-object) &rest keys &key inlinep name &allow-other-keys)
  (apply #'render-standard-object #'with-form-header #'render-form-slot obj keys))

(defmethod render-form (obj &rest keys &key inlinep name &allow-other-keys)
  (with-html
    (:input :type "text" :name name :value obj)))

