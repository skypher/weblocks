
(in-package :weblocks)

(export '(*form-default-error-summary-threshold*
	  *required-field-message* *invalid-input-message* form form-view
	  form-view-error-summary-threshold form-view-use-ajax-p
	  form-view-default-method form-view-default-enctype
	  form-view-default-action form-view-default-title form-view-persist-p
	  form-view-buttons form-view-field-writer-mixin form-view-field
	  form-view-field-parser form-view-field-satisfies
	  form-view-field-writer form-view-field-required-p mixin-form
	  mixin-form-view-field mixin-form-view-field-persist-p
	  *max-raw-input-length* input-presentation-max-length form-presentation
	  input input-presentation render-validation-summary
	  render-form-view-buttons form-field-intermediate-value))

;;; Default initialization parameters
(defparameter *form-default-error-summary-threshold* 15
  "When the number of fields in a form is longer than this threshold,
an error summary is rendered at top whenever applicable. This is a
default value used to initialize 'error-summary-threshold' slot of
'form-view' objects.")

(defparameter *required-field-message* "~A is a required field."
  "This message will be passed to 'format' along with the humanized
name of the field to inform users that the field is required.")

(defparameter *invalid-input-message* "~A must be ~A."
  "This message will be passed to 'format' along with the humanized
name of the field and humanized typespec to inform users that their
input is not valid.")

;;; Form view
(defclass form-view (view)
  ((error-summary-threshold :initform *form-default-error-summary-threshold*
			    :initarg :error-summary-threshold
			    :accessor form-view-error-summary-threshold
			    :documentation "When the number of fields
                            in a form is longer than this threshold,
                            an error summary is rendered at top
                            whenever applicable.")
   (use-ajax-p :initform t
	       :initarg :use-ajax-p
	       :accessor form-view-use-ajax-p
	       :documentation "If set to true (default) uses AJAX on
	       form submission. Otherwise, a full postback is done to
	       submit the form.")
   (default-method :initform :get
                   :initarg :default-method
		   :accessor form-view-default-method
		   :documentation "Default HTML method used for this
	           form if :method isn't specified in keyword
	           parameters when rendering the view. Possible values
	           are :get (default) and :post.")
   (default-action :initform nil
                   :initarg :default-action
		   :accessor form-view-default-action
		   :documentation "A default action that will be
	           called upon submission of the form if :action isn't
	           specified in keyword parameters when rendering the
	           view.")
   (enctype :initform nil
	    :initarg :enctype
	    :accessor form-view-default-enctype
	    :documentation "An enctype that will be
	    used upon submission of the form.")
   (default-title :initform "Modifying"
                  :initarg :default-title
		  :accessor form-view-default-title
		  :documentation "A default title that will be
	          presented to the user if :title isn't specified in
	          keyword parameters when rendering the view.")
   (persistp :initform t
	     :initarg :persistp
	     :accessor form-view-persist-p
	     :documentation "Specifies whether the object should be
	     persisted via 'persist-object' on successful
	     deserealization of the form.")
   (buttons :initform (list :submit :cancel)
	    :initarg :buttons
	    :accessor form-view-buttons
	    :documentation "Contains a list of keywords that identify
	    buttons to be rendered (by default contains :submit
	    and :cancel).  Default form view only recognizes :submit
	    and :cancel keywords."))
  (:documentation "A view designed to interact with the user via input
  forms."))

;;; Form view fields
(defclass form-view-field-writer-mixin ()
  ((writer :initarg :writer
	   :accessor form-view-field-writer
	   :documentation "If this slot is bound to a function object,
	   the function will be called with a new slot value and the
	   object being rendered as arguments. If the slot is not
	   bound, '(setf slot-value)' will be used."))
  (:documentation "A writer slot mixin"))

(defclass form-view-field (inline-view-field form-view-field-writer-mixin)
  ((presentation :initform (make-instance 'input-presentation))
   (parser :initform (make-instance 'text-parser)
	   :initarg :parse-as
	   :accessor form-view-field-parser
	   :documentation "A parser object to be used to parse this
	   field from a form. If not specified, the string parser will
	   be used. In addition, scaffold views will attempt to
	   determine the default parser from the value of the slot
	   type, if one exists.")
   (satisfies :initform nil
	      :initarg :satisfies
	      :accessor form-view-field-satisfies
	      :documentation "A predicate, or a list of predicates,
	      that set constraints for parsed values during
	      validation.")
   (requiredp :initform nil
	      :initarg :requiredp
	      :accessor form-view-field-required-p
	      :documentation "A predicate which determines whether the
	      field is required."))
  (:documentation "A field class of the form view."))

(defclass mixin-form-view-field (mixin-view-field form-view-field-writer-mixin)
  ((persistp :initarg :persistp
	     :accessor mixin-form-view-field-persist-p
	     :documentation "If this slot is set to true, the mixed in
	     object will be persisted prior to being written to its
	     parent via 'persist-object'. If null, 'persist-object'
	     will not be called. If this slot is unbound (the
	     default), the value will be taken from
	     'form-view-persist-p' of the mixin view."))
  (:documentation "A field class of the form view."))

(defmethod mixin-form-view-field-persist-p ((obj mixin-form-view-field))
  (if (slot-boundp obj 'persistp)
      (slot-value obj 'persistp)
      (form-view-persist-p (find-view (mixin-view-field-view obj)))))

(defmethod view-default-field-type ((view-type (eql 'form)) (field-type (eql 'mixin)))
  'mixin-form)

;;; Presentation
(defclass form-presentation ()
  ()
  (:documentation "A base class for all presentations that output form
  constructs. All form presentations should derive from this class in
  order to allow the framework to treat them correctly when
  deserializing the request."))

(defparameter *max-raw-input-length* 40
  "Default maximum allowed input length for input fields.")

(defclass input-presentation (form-presentation)
  ((max-length :initform *max-raw-input-length*
	       :initarg :max-length
	       :accessor input-presentation-max-length
	       :documentation "Maximum length of an input."))
  (:documentation "A default presentation for forms renders an input
  field."))

;;; Form rendering protocol
(defgeneric render-validation-summary (view obj widget errors)
  (:documentation "Renders a summary of validation errors on top of
the form. This function can be redefined to render validation summary
differently.")
  (:method ((view form-view) obj widget errors)
    (declare (ignore view obj))
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
		       errors)))))))

(defgeneric render-form-view-buttons (view obj widget &rest args)
  (:documentation
   "Renders buttons specified view 'buttons' slot of the 'form-view'
object. By default, this method renders 'Submit' and 'Cancel' buttons
for the form view. Override this method to render form controls
differently.

'view' - the view being rendered.
'obj' - the object being rendered.")
  (:method ((view form-view) obj widget &rest args)
    (declare (ignore obj args))
    (with-html
      (:div :class "submit"
	    (when (member :submit (form-view-buttons view))
	      (render-button *submit-control-name*))
	    (when (member :cancel (form-view-buttons view))
	      (render-button *cancel-control-name* :class "submit cancel"))))))

;;; Implement rendering protocol
(defmethod with-view-header ((view form-view) obj widget body-fn &rest args &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (title (form-view-default-title view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     validation-errors
			     &allow-other-keys)
  (let ((header-class (format nil "view form ~A"
			      (attributize-name (object-class-name obj)))))
    (when (>= (count-view-fields view)
	      (form-view-error-summary-threshold view))
      (setf header-class (concatenate 'string header-class " long-form")))
    (with-html-form (method action :class header-class
			    :enctype (form-view-default-enctype view)
			    :use-ajax-p (form-view-use-ajax-p view))
      (:h1 (:span :class "action" (str (concatenate 'string title ":&nbsp;")))
	   (:span :class "object" (str (humanize-name (object-class-name obj)))))
      (render-validation-summary view obj widget validation-errors)
      (:h2 :class "form-fields-title" "Form fields:")
      (safe-apply fields-prefix-fn view obj args)
      (:ul (apply body-fn view obj args))
      (safe-apply fields-suffix-fn view obj args)
      (apply #'render-form-view-buttons view obj widget args))))

(defmethod render-view-field ((field form-view-field) (view form-view)
			      widget presentation value obj 
			      &rest args &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
	 (validation-error (assoc field validation-errors))
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
	   (:label :class (attributize-presentation
			   (view-field-presentation field))
		   (:span :class "slot-name"
			  (:span :class "extra"
				 (str (view-field-label field)) ":&nbsp;"
				 (when (form-view-field-required-p field)
				   (htm (:em :class "required-slot" "(required)&nbsp;")))))
		   (apply #'render-view-field-value
			  value presentation
			  field view widget obj
			  args)
		   (when validation-error
		     (htm (:p :class "validation-error"
			      (:em
			       (:span :class "validation-error-heading" "Error:&nbsp;")
			       (str (format nil "~A" (cdr validation-error))))))))))))

(defmethod render-view-field-value (value (presentation input-presentation)
				    field view widget obj
				    &rest args &key intermediate-values &allow-other-keys)
  (let ((attributized-slot-name (attributize-name (view-field-slot-name field))))
    (multiple-value-bind (intermediate-value intermediate-value-p)
	(form-field-intermediate-value field intermediate-values)
      (with-html
	  (:input :type "text" :name attributized-slot-name
		  :value (if intermediate-value-p
			     intermediate-value
			     (apply #'print-view-field-value value presentation field view widget obj args))
		  :maxlength (input-presentation-max-length presentation))))))

(defmethod print-view-field-value ((value null) (presentation input-presentation)
				   field view widget obj &rest args)
  (declare (ignore presentation obj view field args)))

;;; Intermediate values helper
(defun form-field-intermediate-value (field intermediate-values)
  "Returns an intermediate value for 'slot-name' from
'intermediate-values'. The second value is true if the field was
present in intermediate-values, nil otherwise."
  (let ((value (assoc field intermediate-values)))
    (values (cdr value) (not (null value)))))

