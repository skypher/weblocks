
(in-package :weblocks)

(export '(dataform dataform-data render-dataform
	  render-dataform-data render-dataform-form
	  dataform-submit-action))

(defclass dataform ()
  ((data :accessor dataform-data
	 :initform nil
	 :initarg :data
	 :documentation "Data object rendered and modified by
	 this widget.")
   (ui-state :initform :data
	     :documentation "Current internal state of the
	     widget. Normally :data when rendering via
	     'render-data' and :form when rendering via
	     'render-form'."))
  (:documentation
   "A class that represents a dataform widget. By default this
widget renders the data object via 'render-data' generic renderer
along with a 'modify' link. If the user clicks on the link, the
widget's state is updated and it renders into a form via
'render-form' generic renderer. If a user then submits the form,
the data object is updated from the form (see
dataform-submit-action)."))

(defmethod render ((obj dataform) &rest args)
  (apply #'render-dataform obj (dataform-data obj) args))

(defgeneric render-dataform (obj data &rest args)
  (:documentation
   "Renders the dataform widget in its current state. Normally
called by 'render'. Override to customize rendering for
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
		   (render-link (make-action (lambda ()
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
	 :action (make-action (lambda ()
				(when (request-parameter *submit-control-name*)
				  (apply #'dataform-submit-action obj data args))
				(setf (slot-value obj 'ui-state) :data)))
	 args))

(defgeneric dataform-submit-action (obj data &rest args)
  (:documentation
   "Called when the user invokes a submit action on a dataform
widget. The default implementation updates the data object in the
widget via 'update-object-from-request'. Override to customize
submission behavior."))

(defmethod dataform-submit-action ((obj dataform) data &rest args)
  (update-object-from-request data))
