
(in-package :weblocks)

(export '(render))

(defgeneric render (obj &rest args)
  (:documentation
   "A generic function that renders a widget in its current
state.

'obj' - widget object to render.

One of the implementations allows \"rendering\" functions. When
'render' is called on a function, the function is simply called. This
allows to easily add functions to composite widgets that can do custom
rendering without much boilerplate."))

(defmethod render ((obj function) &rest args)
  (apply obj args))
