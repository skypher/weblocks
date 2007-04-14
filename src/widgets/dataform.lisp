
(in-package :weblocks)

(defclass dataform ()
  ((data :accessor dataform-data
	 :initform nil
	 :initarg :data)
   (ui-state :initform :data)))

(defmethod render ((obj dataform) &rest args)
  (apply #'render-dataform obj (dataform-data obj) args))

(defmethod render-dataform ((obj dataform) data &rest args)
  (with-slots (ui-state) obj
    (case ui-state
      (:data (apply #'render-data
		    data
		    :postslots-fn
		    (lambda (obj &rest keys)
		      (with-html
			(:div :class "submit"
			      (render-link (make-action (lambda ()
							  (setf ui-state :form)))
					   "Modify"))))
		    args))
      (:form (apply #'render-form
		    data
		    :method :post
		    :action (make-action (lambda ()
					   (update-object-from-request data)
					   (setf ui-state :data)))
		    args)))))
