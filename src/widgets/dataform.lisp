
(in-package :weblocks)

(defclass dataform ()
  ((data :accessor dataform-data
	 :initform nil
	 :initarg :data)
   (ui-state :initform :data)))

(defmethod render ((obj dataform) &rest args)
  (with-slots (data ui-state) obj
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
					   (setf ui-state :data)))
		    args)))))
