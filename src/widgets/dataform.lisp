
(in-package :weblocks)

(export '(dataform dataform-data-view dataform-form-view
	  render-dataform render-dataform-data render-dataform-form
	  annihilate-dataform dataform-submit-action dataform-ui-state))

(defwidget dataform (data-editor)
  ((data-view :accessor dataform-data-view
	      :initform nil
	      :initarg :data-view
	      :documentation "View object used to render 'data' object
	      into information view. If 'data-view' isn't provided,
	      the scaffold view will be used by default.")
   (form-view :accessor dataform-form-view
	      :initform nil
	      :initarg :form-view
	      :documentation "View object used to render 'data' object
	      into form view. If 'form-view' isn't provided, the
	      scaffold view will be used by default.")
   (ui-state :initform :data
	     :accessor dataform-ui-state
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
			     errors."))
  (:documentation
   "A class that represents a dataform widget. By default this widget
renders the data object with a data view along with a 'modify'
link. If the user clicks on the link, the widget's state is updated
and it renders into a form via a form view. If a user then submits the
form, the data object is updated from the form (see
dataform-submit-action)."))

(defmethod dataform-data-view ((obj dataform))
  (when (null (slot-value obj 'data-view))
    (setf (slot-value obj 'data-view)
	  (find-view (list 'data (class-name (class-of (dataform-data obj)))))))
  (find-view (slot-value obj 'data-view)))

(defmethod dataform-form-view ((obj dataform))
  (when (null (slot-value obj 'form-view))
    (setf (slot-value obj 'form-view)
	  (find-view (list 'form (class-name (class-of (dataform-data obj)))))))
  (find-view (slot-value obj 'form-view)))

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
'render-data', 'render-form', etc.")
  (:method ((obj dataform) data &rest args)
    (ecase (dataform-ui-state obj)
      (:data (apply #'render-dataform-data obj data (dataform-data-view obj) args))
      (:form (apply #'render-dataform-form obj data (dataform-form-view obj) args)))))

;;; Rendering data
(defgeneric render-dataform-data (obj data view &rest args)
  (:documentation
   "Renders the dataform widget when it's in ':data'
state. Normally called by 'render-dataform'. Override to
customize data behavior.")
  (:method ((obj dataform) data view &rest args)
    (apply #'render-object-view data view
	   :fields-suffix-fn (lambda (&rest args)
			       (declare (ignore args))
			       (render-dataform-data-buttons obj data))
	   :widget obj
	   args)))

;;; Rendering form
(defgeneric render-dataform-form (obj data view &rest args)
  (:documentation
   "Renders the dataform widget when it's in ':form'
state. Normally called by 'render-dataform'. Override to
customize form behavior.")
  (:method ((obj dataform) data view &rest args)
    (apply #'render-object-view
	   data view
	   :method :post
	   :action (make-action (lambda (&key submit cancel &allow-other-keys)
				  (catch 'annihilate-dataform
				    (let (break-out)
				      (when submit
					(multiple-value-bind (success errors)
					    (apply #'dataform-submit-action obj data args)
					  (if success
					      (progn
						(mark-dirty obj :propagate t)
						(safe-funcall (dataform-on-success obj) obj)
						(setf break-out t))
					      (progn
						(setf (slot-value obj 'validation-errors) errors)
						(setf (slot-value obj 'intermediate-form-values)
						      (apply #'request-parameters-for-object-view
							     view args))))))
				      (when cancel
					(safe-funcall (dataform-on-cancel obj) obj)
					(setf break-out t))
				      (when break-out
					(setf (slot-value obj 'validation-errors) nil)
					(setf (slot-value obj 'intermediate-form-values) nil)
					(setf (dataform-ui-state obj) :data))))))
	   :validation-errors (slot-value obj 'validation-errors)
	   :intermediate-values (slot-value obj 'intermediate-form-values)
	   :widget obj
           :form-view-buttons (data-editor-form-buttons obj)
	   args)))

(defgeneric dataform-submit-action (obj data &rest args)
  (:documentation
   "Called when the user invokes a submit action on a dataform
widget. The default implementation updates the data object in the
widget via 'update-object-view-from-request'. Override to customize
submission behavior.")
  (:method ((obj dataform) data &rest args)
    (apply #'update-object-view-from-request data
	   (find-view (dataform-form-view obj))
	   :class-store (dataform-class-store obj)
	   args)))

;; Dependencies
(defmethod dependencies append ((obj dataform))
  (dependencies
   (ecase (dataform-ui-state obj)
     (:form (dataform-form-view obj))
     (:data (dataform-data-view obj)))))
