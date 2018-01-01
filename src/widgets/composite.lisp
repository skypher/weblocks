;;;;
;;;; COMPATIBILITY INTERFACE
;;;;
;;;; do not use in new code!
;;;;

(in-package :weblocks)

(export '(composite composite-widgets))

(defwidget composite (widget)
  ())

(defmethod initialize-instance :around ((obj composite) &rest initargs &key widgets &allow-other-keys)
  (remf initargs :widgets)
  (apply #'call-next-method obj :children widgets initargs))

(defmethod composite-widgets (comp) (widget-children comp))

(defmethod (setf composite-widgets) (value comp)
  (setf (widget-children comp) value))

;; Seems it is used only in tests,
;; TODO: may be to remove
(defun root-composite ()
  "Expands to code that can be used as a place to access to the root
composite."
  (weblocks.session:get-value 'root-widget))

(defun (setf root-composite) (value)
  (setf (weblocks.session:get-value 'root-widget)
        value))
