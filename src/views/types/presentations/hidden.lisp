

(in-package :weblocks)

(export '(hidden hidden-presentation))

;;; hidden presentation
(defclass hidden-presentation (input-presentation)
  ()
  (:documentation "A presentation that outputs no HTML"))

(defmethod render-view-field ((field form-view-field)
                              (view form-view)
                              widget
                              (presentation hidden-presentation)
                              value obj &rest args)
  (declare (ignore field view widget presentation value obj args)))

(defmethod render-view-field ((field data-view-field)
                              (view data-view)
                              widget
                              (presentation hidden-presentation)
                              value obj &rest args)
  (declare (ignore field view widget presentation value obj args)))
