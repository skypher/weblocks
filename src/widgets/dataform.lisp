
(in-package :weblocks)

(defclass dataform ()
  ((data :accessor dataform-data
	 :initform nil
	 :initarg :data)
   (ui-state :initform :data)))

(defmethod render ((obj dataform) &rest args)
  (apply #'render-dataform obj (dataform-data obj) args))

(defmethod render-dataform ((obj dataform) data &rest args)
  (ecase (slot-value obj 'ui-state)
    (:data (apply #'render-dataform-data obj data args))
    (:form (apply #'render-dataform-form obj data args))))

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

(defmethod render-dataform-form ((obj dataform) data &rest args)
  (apply #'render-form
	 data
	 :method :post
	 :action (make-action (lambda ()
				(when (request-parameter *submit-control-name*)
				  (apply #'dataform-submit-action obj data args))
				(setf (slot-value obj 'ui-state) :data)))
	 args))

(defmethod dataform-submit-action ((obj dataform) data &rest args)
  (update-object-from-request data))
