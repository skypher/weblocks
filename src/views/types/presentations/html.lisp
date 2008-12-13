(in-package #:weblocks)

(export '(html-presentation))

(defclass html-presentation (text-presentation)
  ()
  (:documentation "A presentation that simply renders its value as-is,
  without any escaping, allowing for HTML inclusion."))

(defmethod render-view-field-value (value (presentation html-presentation)
                                    field view widget obj &rest args
                                    &key &allow-other-keys)
  (let ((printed-value (apply #'print-view-field-value value presentation field view widget obj args)))
    (with-html
      (:span :class "value"
             (str printed-value)))))
