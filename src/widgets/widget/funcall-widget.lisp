
(in-package :weblocks)

(export 'funcall-widget)

(defwidget funcall-widget ()
  ((fun-designator :type (or symbol function)
                   :accessor funcall-widget-fun-designator
                   :initarg :fun-designator)))

(defmethod render-widget-body ((widget funcall-widget) &rest args)
  (when (widget-continuation widget)
    (setf args (cons (widget-continuation widget) args)))
  (let ((fun-designator (funcall-widget-fun-designator widget)))
    (etypecase fun-designator
      (symbol
        (if (fboundp fun-designator)
          (apply fun-designator args)
          (error "Cannot render ~A as widget. Symbol not bound to a function."
                 fun-designator)))
      (function
        (apply fun-designator args)))))
                   
(defmethod make-widget ((obj symbol))
  "Create a widget from a symbol denoting a function."
  (make-instance 'funcall-widget :fun-designator obj))

(defmethod make-widget ((obj function))
  "Create a widget from a function object."
  (make-instance 'funcall-widget :fun-designator obj))

