(in-package :weblocks)

(export '(widget-presentation get-widget-form-value-from-request))

(defclass widget-presentation (form-presentation)
  ((widget-init :initarg :widget-init :accessor widget-presentation-widget)
   (widget-set-value :initform nil :initarg :set-value)
   (current-widget :initform nil)))

(defmethod render-view-field-value (value (presentation widget-presentation) field view widget obj &rest args &key intermediate-values field-info)
  (let ((attributized-slot-name (if field-info
                                  (attributize-view-field-name field-info)
                                  (attributize-name (view-field-slot-name field))))
        (field-widget (setf 
                        (slot-value presentation 'current-widget)
                        (or (get (view-field-slot-name field) widget)
                            (let ((field-widget 
                                    (funcall (slot-value presentation 'widget-init)
                                             :value value 
                                             :presentation presentation 
                                             :field field 
                                             :view view 
                                             :form widget 
                                             :object obj)))
                              (funcall (slot-value presentation 'widget-set-value)
                                       :value value 
                                       :widget field-widget
                                       :type :init-value)
                              (setf (get (view-field-slot-name field) widget) field-widget))))))

    (render-widget field-widget)))

(defmethod request-parameter-for-presentation (name (presentation widget-presentation))
  (let ((value (get-widget-form-value-from-request (slot-value presentation 'current-widget))))
    (funcall (slot-value presentation 'widget-set-value)
             :value value 
             :widget (slot-value presentation 'current-widget)
             :type :set-value)
    value))

(defgeneric get-widget-form-value-from-request (widget)
  (:documentation "Should parse and return request value for widget. Used for widgets rendered inside of widget-presentation")
  (:method ((widget t))
   nil))

