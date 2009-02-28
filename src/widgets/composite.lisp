;;;;
;;;; COMPATIBILITY INTERFACE
;;;;
;;;; do not use in new code!
;;;;

(in-package :weblocks)

(export '(composite composite-widgets root-composite))

(defwidget composite (widget)
  ())

(defmethod composite-widgets ((comp composite))
  (widget-children comp))

(defmethod (setf composite-widgets) (value (comp composite))
  (set-children-of-type comp value :composite))

(defmacro root-composite ()
  "Expands to code that can be used as a place to access to the root
composite."
  `(webapp-session-value 'root-widget))

