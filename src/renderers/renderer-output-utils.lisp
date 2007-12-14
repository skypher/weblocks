;;;; Utility functions for generic renderers
(in-package :weblocks)

(export '(object-class-name object-name render-standard-object
	  slot-type->css-class))

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
function the function is then called with the object as the only
argument. If it returns a string, the value is returned as the
name of the object. Otherwise, the value of 'object-class-name'
is returned.

Override this method to provide object names."))

(defmethod object-name (obj)
  (let* ((cls-symbol (class-name (class-of obj)))
	 (cls-package (symbol-package cls-symbol))
	 (expected-name (concatenate 'string (symbol-name cls-symbol) (string '#:-name)))
	 (expected-symbol (find-symbol expected-name cls-package)))
    (if (and (not (null expected-symbol))
	     (fboundp expected-symbol))
	(let ((obj-name (funcall expected-symbol obj)))
	  (if (stringp obj-name)
	      (return-from object-name obj-name)))))
  (humanize-name (object-class-name obj)))

(defun render-standard-object (header-fn render-slot-fn obj &rest keys)
  "Renders the slots of a CLOS object into HTML. This function
encapsulates rendering behavior common to multiple generic renderers -
it walks through the visible slots of the object (obtained via
'object-visible-slots') and applies 'render-slot-fn' to each of those,
wrapping the call with 'header-fn' in order to render a header."
  (apply header-fn obj (curry #'visit-object-slots obj render-slot-fn) keys))

(defun slot-type->css-class (slot-type)
  "Converts slot type to CSS class if reasonably possible."
  (when (atom slot-type)
    (let ((attr-slot-type (attributize-name slot-type)))
      (when (> (length attr-slot-type) 1)
	attr-slot-type))))

