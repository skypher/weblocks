(in-package :weblocks)

(export '(integer-range-presentation integer-range-parser))

(defclass integer-range-presentation (text-presentation input-presentation)
  () (:documentation "A presentation for integer ranges."))

(defmethod render-view-field-value (value (presentation integer-range-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args
                                    &key intermediate-values &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (let* ((value (if intermediate-value-p intermediate-value value))
           (value (if (consp value) value (cons value value))))
      (render-input-field "text" (view-field-slot-name field)
                          (format nil "~D-~D" (car value) (cdr value))))))

(defclass integer-range-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser integer-range-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let* ((parsed-value (cl-ppcre:split "-" value))
         (min (parse-integer (car parsed-value)))
         (max (parse-integer (car (metatilities:ensure-list (cdr parsed-value)))))
         (present-p (and min max))
         (valid-p (and present-p (cl-ppcre:scan "[0-9]+-[0-9]+|[0-9]+" value)
                       (<= min max))))
      (values valid-p present-p (cons min max))))

