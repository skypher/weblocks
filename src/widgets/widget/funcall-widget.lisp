
(in-package :weblocks)

(defwidget funcall-widget ()
  ((fun-designator :type (or symbol function)
                   :accessor funcall-widget-fun-designator
                   :initarg :fun-designator)))

(defmethod render-widget-body ((widget funcall-widget) &rest args)
  (funcall (funcall-widget-fun-designator widget) widget))
                   
(defmethod make-widget ((obj symbol))
  "Create a widget from a symbol denoting a function."
  (make-instance 'funcall-widget :fun-designator obj))

(defmethod make-widget ((obj function))
  "Create a widget from a function object."
  (make-instance 'funcall-widget :fun-designator obj))

