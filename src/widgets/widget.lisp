
(in-package :weblocks)

(export '(render))

(defgeneric render (obj &rest args)
  (:documentation
   "A generic function that renders a widget in its current
state.

'obj' - widget object to render."))

