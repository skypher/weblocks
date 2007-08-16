
(in-package :weblocks)

(export '(dataform dataform-data dataform-on-cancel
	  dataform-on-success render-dataform render-dataform-data
	  render-dataform-form dataform-submit-action))

(defwidget dataform (widget)
  ((data :accessor dataform-data
	 :initform nil
	 :initarg :data
	 :documentation "Data object rendered and modified by
	 this widget.")
   (ui-state :initform :data
	     :initarg :ui-state
	     :documentation "Current internal state of the
	     widget. Normally :data when rendering via 'render-data'
	     and :form when rendering via 'render-form'.")
   (validation-errors :initform nil
		      :documentation "An association list of slot
		      names and validation errors that occurred during
		      the previous form submission.")
   (intermediate-form-values :initform nil
			     :documentation "If user enters form
			     values and validation fails, these values
			     are stored in this variable so the form
			     isn't lost while the user fixes
			     errors.")
   (on-cancel :accessor dataform-on-cancel
	      :initform nil
	      :initarg :on-cancel
	      :documentation "An optional callback function with one
	      argument (the dataform widget). Called when the user
	      presses a cancel button.")
   (on-success :accessor dataform-on-success
	       :initform nil
	       :initarg :on-success
	       :documentation "An optional callback function with one
	       argument (the dataform widget). Called when the user
	       successfully submitted data that passed through the
	       validation stage."))
  (:documentation
   "A class that represents a dataform widget. By default this
widget renders the data object via 'render-data' generic renderer
along with a 'modify' link. If the user clicks on the link, the
widget's state is updated and it renders into a form via
'render-form' generic renderer. If a user then submits the form,
the data object is updated from the form (see
dataform-submit-action)."))

(defmethod render-widget-body ((obj dataform) &rest args)
  (apply #'render-dataform obj (dataform-data obj) args))

(defgeneric render-dataform (obj data &rest args)
  (:documentation
   "Renders the dataform widget in its current state. Normally called
by 'render-widget-body'. Override to customize rendering for
particular types of data.

'obj' - widget object to render.
'data' - data to render.
'args' - keyword arguments passed to functions internally. See
'render-data', 'render-form', etc."))

(defmethod render-dataform ((obj dataform) data &rest args)
  (ecase (slot-value obj 'ui-state)
    (:data (apply #'render-dataform-data obj data args))
    (:form (apply #'render-dataform-form obj data args))))

(defgeneric render-dataform-data (obj data &rest args)
  (:documentation
   "Renders the dataform widget when it's in ':data'
state. Normally called by 'render-dataform'. Override to
customize data behavior."))

(defmethod render-dataform-data ((obj dataform) data &rest args)
  (apply #'render-data
	 data
	 :postslots-fn
	 (lambda (data-value &rest keys)
	   (with-html
	     (:div :class "submit"
		   (render-link (make-action (lambda (&rest args)
					       (setf (slot-value obj 'ui-state) :form)))
				"Modify"))))
	 args))

(defgeneric render-dataform-form (obj data &rest args)
  (:documentation
   "Renders the dataform widget when it's in ':form'
state. Normally called by 'render-dataform'. Override to
customize form behavior."))

(defmethod render-dataform-form ((obj dataform) data &rest args)
  (apply #'render-form
	 data
	 :method :post
	 :action (make-action (lambda (&key submit cancel &allow-other-keys)
				(let (break-out)
				  (when submit
				    (multiple-value-bind (success errors)
					(apply #'dataform-submit-action obj data args)
				      (if success
					  (progn
					    (mark-dirty obj :putp t)
					    (safe-funcall (dataform-on-success obj) obj)
					    (setf break-out t))
					  (progn
					    (setf (slot-value obj 'validation-errors) errors)
					    (setf (slot-value obj 'intermediate-form-values)
						  (apply #'request-parameters-for-object
							 data args))))))
				  (when cancel
				    (safe-funcall (dataform-on-cancel obj) obj)
				    (setf break-out t))
				  (when break-out
				    (setf (slot-value obj 'validation-errors) nil)
				    (setf (slot-value obj 'intermediate-form-values) nil)
				    (setf (slot-value obj 'ui-state) :data)))))
	 :validation-errors (slot-value obj 'validation-errors)
	 :intermediate-fields (slot-value obj 'intermediate-form-values)
	 args))

(defgeneric dataform-submit-action (obj data &rest args)
  (:documentation
   "Called when the user invokes a submit action on a dataform
widget. The default implementation updates the data object in the
widget via 'update-object-from-request'. Override to customize
submission behavior."))

(defmethod dataform-submit-action ((obj dataform) data &rest args)
  (apply #'update-object-from-request data args))
