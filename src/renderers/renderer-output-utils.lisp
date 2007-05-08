;;;; Utility functions for generic renderers
(in-package :weblocks)

(export '(object-class-name object-name render-object-slot
	  render-standard-object))

(defgeneric object-class-name (obj)
  (:documentation
   "Returns an object's class name (i.e. \"Employee\"). This
method is used by renderers to present the name of an entity to
the user. Override this method to change the name for particular
objects."))

(defmethod object-class-name (obj)
  (class-name (class-of obj)))

(defgeneric object-name (obj)
  (:documentation
   "Takes an object and returns its name (as opposed to class
name, the name of an object is specific to its instance,
i.e. \"Joe Average\", instead of \"Employee\"). The renderers use
this method to present objects that aren't rendered inline by
name.

By default this method looks for a symbol of a form
[class-name]-NAME (e.g. EMPLOYEE-NAME) in the same package where
the class of 'obj' was defined. If this symbol is bound to a
function the function is then callsed with the object as the only
argument. If it returns a string, the value is returned as the
name of the object. Otherwise, the value of 'object-class-name'
is returned.

Override this method to provide object names."))

(defmethod object-name (obj)
  (let* ((cls-symbol (class-name (class-of obj)))
	 (cls-package (symbol-package cls-symbol))
	 (expected-name (concatenate 'string (symbol-name cls-symbol) "-NAME"))
	 (expected-symbol (find-symbol expected-name cls-package)))
    (if (and (not (null expected-symbol))
	     (fboundp expected-symbol))
	(let ((obj-name (funcall expected-symbol obj)))
	  (if (stringp obj-name)
	      (return-from object-name obj-name)))))
  (object-class-name obj))

(defun render-object-slot (render-object-fn render-slot-fn obj slot-name slot-value args)
  "Renders a given slot of a CLOS object (usually if the slot
itself is a standard CLOS object). This function encapsulates
rendering behavior common to multiple generic renderers.

If the members of the slot object are meant to be rendered
inline (tested with 'render-slot-inline-p', checks for '-ref' in
the name of the slot by default), calls 'render-object-fn'
with :inlinep set to true. Otherwise, grabs the name of the
object using 'object-name' and simply renders the name using
'render-slot-fn'."
  (if (render-slot-inline-p obj slot-name)
      (apply render-object-fn slot-value :inlinep t :name slot-name args)
      (apply render-slot-fn obj slot-name (object-name slot-value) args)))

(defun visit-object-slots (obj render-slot-fn keys)
  "Used by 'render-standard-object' to visit visible slots of an
object and apply a render function to them."
  (mapc (lambda (slot)
	  (apply render-slot-fn obj (slot-definition-name (car slot))
		 (get-slot-value obj (car slot)) :human-name (cdr slot) keys))
	(apply #'object-visible-slots obj keys)))

(defun render-standard-object (header-fn render-slot-fn obj &rest keys &key inlinep &allow-other-keys)
  "Renders the slots of a CLOS object into HTML. This function
encapsulates rendering behavior common to multiple generic
renderers.

If the object needs to be rendered inline within another
object ('inlinep' is true), 'render-standard-object' walks
through the visible slots of the object (obtained via
'object-visible-slots') and applies 'render-slot-fn' to each of
those.

If the object is not rendered inline ('inlinep' is nil, which is
the default), 'render-standard-object' applies the same logic as
above, except it wraps it with a call to 'header-fn' in order to
render a header."
  (if inlinep
      (visit-object-slots obj render-slot-fn keys)
      (apply header-fn obj (curry #'visit-object-slots obj render-slot-fn keys) keys)))
