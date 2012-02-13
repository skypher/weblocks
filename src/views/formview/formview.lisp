
(in-package :weblocks)

(export '(*form-default-error-summary-threshold*
          *required-field-message* form form-view
          form-view-error-summary-threshold form-view-use-ajax-p
          form-view-default-method form-view-default-enctype
          form-view-default-action form-view-persist-p form-view-focus-p
          form-view-satisfies form-view-buttons form-view-field-writer-mixin
          form-view-field form-view-field-parser form-view-field-satisfies
          form-view-field-writer form-view-field-required-p
          form-view-field-required-error-msg mixin-form mixin-form-view-field
          mixin-form-view-field-persist-p *max-raw-input-length*
          input-presentation-max-length form-presentation input
          input-presentation render-validation-summary render-form-view-buttons
          form-field-intermediate-value *default-required-indicator*))

;;; Default initialization parameters
(defparameter *form-default-error-summary-threshold* 15
  "When the number of fields in a form is longer than this threshold,
an error summary is rendered at top whenever applicable. This is a
default value used to initialize 'error-summary-threshold' slot of
'form-view' objects.")

(defparameter *required-field-message* "~A is a required field."
  "This message will be passed to 'format' along with the humanized
name of the field to inform users that the field is required.")


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
   (default-action :initform (lambda (&rest args)
			       (declare (ignore args)))
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
   (persistp :initform t
	     :initarg :persistp
	     :accessor form-view-persist-p
	     :documentation "Specifies whether the object should be
	     persisted via 'persist-object' on successful
	     deserealization of the form.")
   (focusp :initform nil
	   :initarg :focusp
	   :accessor form-view-focus-p
	   :documentation "If set to true, renders appropriate
	   JavaScript to focus on the first element of the form once
	   the form is loaded. This slot is set to false by default.")
   (buttons :initform (list :submit :cancel)
	    :initarg :buttons
	    :accessor form-view-buttons
	    :documentation "Contains a list of keywords that identify
	    buttons to be rendered (by default contains :submit
	    and :cancel).  Default form view only recognizes :submit
	    and :cancel keywords. Each item of the list may be a cons
	    pair, in which case CAR of the pair should be a keyword,
	    and CDR of the pair should be a string that will be
	    presented to the user via value of the button.")
   (satisfies :initform nil
	      :initarg :satisfies
	      :accessor form-view-satisfies
	      :documentation "A function or a list of functions that
perform validation on the entire view (possibly combining multiple fields).
The function should expect keyword arguments corresponding to view slot
names, each keyword argument corresponds to one of the parsed values.
The function should either return t if the form validates properly, or
values nil error-message if it does not.")
  (instructions :type (or string null)
                :initform nil
                :initarg :instructions
                :accessor form-view-instructions
                :documentation "Instructions given to the user."))
  (:documentation "A view designed to interact with the user via input
  forms."))

;;; Form view fields
(defclass form-view-field-writer-mixin ()
  ((writer :initarg :writer
	   :accessor form-view-field-writer
	   :documentation "If this slot is bound to a function object,
	   the function will be called with a new slot value and the
	   object being rendered as arguments. If the slot is not
	   bound, '(setf slot-value)' will be used.")
   (delayed-write-p :initarg :delayed-write-p
		    :initform nil
		    :accessor form-view-field-writer-delayed-p
		    :documentation "If this slot is set to t, then the
writer will get called after the object has been persisted. This is useful
for updating relations, where objects need to be assigned ids and stored
before relations can be updated."))
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
	      :documentation "A predicate, or a list of predicates, that
	      set constraints for parsed values during validation. A
	      predicate may return multiple values, in which case the
	      second value is used as the error message instead of the
	      default one supplied by the parser.")
   (requiredp :initform nil
	      :initarg :requiredp
	      :accessor form-view-field-required-p
	      :documentation "A predicate which determines whether the
	      field is required.")
   (required-indicator :initform t
		       :initarg :required-indicator
		       :accessor form-view-field-required-indicator
		       :documentation "A string, t or nil. When this 
                       field is required, this value is rendered after
                       the field's label. A value of t will render 
                       *default-required-indicator*.")
   (required-error-msg :initform nil
                       :initarg :required-error-msg
                       :accessor form-view-field-required-error-msg
                       :documentation "If this value isn't nil, it is
                       presented to the user when the field is
                       required and missing from the input
                       data. Otherwise, the standard required error
                       message is presented."))
  (:documentation "A field class of the form view."))

(defun get-required-error-msg (form-view-field)
  "Returns an error message for a missing required field."
  (if (form-view-field-required-error-msg form-view-field)
      (form-view-field-required-error-msg form-view-field)
      (format nil *required-field-message*
              (humanize-name
               (view-field-label form-view-field)))))

(defparameter *default-required-indicator* "(required)"
  "Default string to render after a field's label if the field is required.")

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
(defclass form-presentation (presentation)
  ()
  (:documentation "A base class for all presentations that output form
  constructs. All form presentations should derive from this class in
  order to allow the framework to treat them correctly when
  deserializing the request."))

(defparameter *max-raw-input-length* 40
  "Default maximum allowed input length for input fields.")

(defclass input-presentation (form-presentation text-presentation-mixin)
  ((max-length :initform *max-raw-input-length*
	       :initarg :max-length
	       :accessor input-presentation-max-length
	       :documentation "Maximum length of an input.")
   (size :accessor input-presentation-size
         :initarg :size
         :initform nil))
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
      (let ((non-field-errors (find-all errors #'null :key #'car))
	    (field-errors (find-all errors (compose #'not #'null) :key #'car)))
	(with-html
	  (:div :class "validation-errors-summary"
		(:h2 :class "error-count"
		     (let ((error-count (length errors)))
		       (if (eql error-count 1)
			   (str (format nil "There is 1 validation error:"))
			   (str (format nil "There are ~S validation errors:" error-count)))))
		(when non-field-errors
		  (htm
		   (:ul :class "non-field-validation-errors"
			(mapc (lambda (err)
				(with-html
				  (:li
				   (str (format nil "~A" (cdr err))))))
			      non-field-errors))))
		(when field-errors
		  (htm
		   (:ul :class "field-validation-errors"
			(mapc (lambda (err)
				(with-html
				  (:li
				   (str (format nil "~A" (cdr err))))))
			      field-errors))))))))))

(defgeneric render-form-view-buttons (view obj widget &rest args &key buttons &allow-other-keys)
  (:documentation
   "Renders buttons specified view 'buttons' slot of the 'form-view'
object. By default, this method renders 'Submit' and 'Cancel' buttons
for the form view. Override this method to render form controls
differently.

'view' - the view being rendered.
'obj' - the object being rendered.
'buttons' - same as form-view-buttons, can be used to override
form-view-buttons for a given view.")
  (:method ((view form-view) obj widget &rest args &key form-view-buttons &allow-other-keys)
    (declare (ignore obj args))
    (flet ((find-button (name)
	     (ensure-list
	       (if form-view-buttons
		   (find name form-view-buttons
			 :key (lambda (item)
				(car (ensure-list item))))
		 (find name (form-view-buttons view)
		       :key (lambda (item)
			      (car (ensure-list item))))))))
      (with-html
	(:div :class "submit"
	      (let ((submit (find-button :submit)))
		(when submit
		  (render-button *submit-control-name*
				 :value (or (cdr submit)
					    (humanize-name (car submit))))))
	      (let ((cancel (find-button :cancel)))
		(when cancel
		  (render-button *cancel-control-name*
				 :class "submit cancel"
				 :value (or (cdr cancel)
					    (humanize-name (car cancel)))))))))))

(defmethod view-caption ((view form-view))
  (if (slot-value view 'caption)
      (slot-value view 'caption)
      (with-html-output-to-string (out)
	(:span :class "action" "Modifying:&nbsp;")
	(:span :class "object" "~A"))))

;;; Implement rendering protocol
(defmethod with-view-header ((view form-view) obj widget body-fn &rest args &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     validation-errors
			     &allow-other-keys)
  (declare (special *on-ajax-complete-scripts* *form-submit-dependencies*))
  (let ((form-id (gen-id))
	(header-class (format nil "view form ~A"
			      (attributize-name (object-class-name obj)))))
    (when (>= (count-view-fields view)
	      (form-view-error-summary-threshold view))
      (setf header-class (concatenate 'string header-class " long-form")))
    (let ((form-body
	   (let ((*weblocks-output-stream* (make-string-output-stream)))
	     (with-html
               (awhen (view-caption view)
                 (htm (:h1 (fmt (view-caption view) (humanize-name (object-class-name obj))))))
	       (render-validation-summary view obj widget validation-errors)
	       (safe-apply fields-prefix-fn view obj args)
	       (:ul (apply body-fn view obj args))
	       (safe-apply fields-suffix-fn view obj args)
	       (apply #'render-form-view-buttons view obj widget args)
	       (get-output-stream-string *weblocks-output-stream*)))))
      (with-html-form (method action
			      :id (when (form-view-focus-p view) form-id)
			      :class header-class
			      :enctype (form-view-default-enctype view)
			      :extra-submit-code (render-form-submit-dependencies *form-submit-dependencies*)
			      :use-ajax-p (form-view-use-ajax-p view))
	(write-string form-body *weblocks-output-stream*)))
    (when (form-view-focus-p view)
        (send-script (ps* `((@ ($ ,form-id) focus-first-element)))))))

(defmethod render-view-field ((field form-view-field) (view form-view)
			      widget presentation value obj 
			      &rest args &key validation-errors field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))
  (let* ((attributized-slot-name (if field-info
                                   (attributize-view-field-name field-info)
                                   (attributize-name (view-field-slot-name field))))
	 (validation-error (assoc field validation-errors))
	 (field-class (concatenate 'string (aif attributized-slot-name it "")
				   (when validation-error " item-not-validated")))
         (*presentation-dom-id* (gen-id)))
    (with-html
      (:li :class field-class
	   (:label :class (attributize-presentation
			   (view-field-presentation field))
                   :for *presentation-dom-id*
		   (:span :class "slot-name"
			  (:span :class "extra"
				 (unless (empty-p (view-field-label field))
				   (str (view-field-label field))
				   (str ":&nbsp;"))
				 (let ((required-indicator (form-view-field-required-indicator field)))
				   (when (and (form-view-field-required-p field)
					      required-indicator)
				     (htm (:em :class "required-slot"
					       (if (eq t required-indicator)
						   (str *default-required-indicator*)
						   (str required-indicator))
					       (str "&nbsp;"))))))))
           (apply #'render-view-field-value
                  value presentation
                  field view widget obj
                  :field-info field-info
                  args)
           (when validation-error
             (htm (:p :class "validation-error"
                      (:em
                        (:span :class "validation-error-heading" "Error:&nbsp;")
                        (str (format nil "~A" (cdr validation-error)))))))))))

(defmethod render-view-field-value (value (presentation input-presentation)
				    field view widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))
  (let ((attributized-slot-name (if field-info
                                  (attributize-view-field-name field-info)
                                  (attributize-name (view-field-slot-name field)))))
    (multiple-value-bind (intermediate-value intermediate-value-p)
	(form-field-intermediate-value field intermediate-values)
      (with-html
	  (:input :type "text" :name attributized-slot-name
		  :value (if intermediate-value-p
			     intermediate-value
			     (apply #'print-view-field-value value presentation field view widget obj args))
		  :maxlength (input-presentation-max-length presentation)
                  :size (input-presentation-size presentation)
		  :id *presentation-dom-id*)))))

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

