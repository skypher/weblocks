(in-package #:weblocks)

(export '(html-presentation))

(defclass html-presentation (text-presentation)
  ()
  (:documentation "A presentation that simply renders its value as-is,
  without any escaping, allowing for HTML inclusion."))

(defun html-presentation-field-value-wt (&key value)
  (with-html-to-string
    (:span :class "value"
     (str value))))

(deftemplate :html-presentation-field-value-wt #'html-presentation-field-value-wt)

(defmethod render-view-field-value (value (presentation html-presentation)
                                          field view widget obj &rest args
                                          &key &allow-other-keys)
  (let ((printed-value (apply #'print-view-field-value value presentation field view widget obj args)))
      (render-wt 
        :html-presentation-field-value-wt
        (list :field field :view view :widget widget :object obj :presentation presentation)
        :value printed-value)))
