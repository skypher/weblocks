
(in-package :weblocks)

(export '(quickform make-quickform quickform-satisfies))

(defwidget quickform (dataform)
  ((satisfies :accessor quickform-satisfies
	      :initform nil
	      :initarg :satisfies
	      :documentation "A function that corresponds to the value
	      of 'satisfies' key to 'make-quickform'."))
  (:documentation "A widget based on dataform designed to quickly
  present forms. Use 'make-quickform' for easy configuration."))

(defun make-quickform (view &key on-success on-cancel satisfies
		       data (answerp t) (class 'quickform)
		       (data-class-name (gensym)) class-store)
  "Returns an instance of a dataform widget configured to quickly and
easily present forms. The advantage of using 'make-quickform' over
simply calling 'render-view' is that the widget produced by
'make-quickform' validates the form and deserializes it into an
object.

'view' - a form view to be rendered.

'on-success' - an optional function of two arguments (widget itself
and deserialized object), called when the form has successfully been
validated.

'on-cancel' - an optional function of one argument (widget), called
when user cancels out of the form.

'satisfies' - an optional function called with two arguments (widget
and data object) after the fields have been successfully parsed into
the object. It is expected to return true if the object is valid, or
nil as the first value and an association list of fields and error
messages as the second value.

'data' - an optional data object to be used. If the data object isn't
provided, it will be generated from the view automatically.

'answerp' - if set to true (default), the widget automatically calls
answer on itself on success or cancellation. If on-success is present,
answers with its return value. Otherwise, returns the data object.

'class' - a class of the quickform widget. By default uses
'quickform'.

'data-class-name' - if 'data' isn't provided, the name of the class to
be generated from the view."
  (assert (subtypep class 'quickform))
  (make-instance class
		 :data (or data (make-instance (class-from-view view data-class-name)))
		 :ui-state :form
		 :on-success (lambda (obj)
			       (let ((response (if on-success
						   (funcall on-success obj (dataform-data obj))
						   (dataform-data obj))))
				 (when answerp
				   (answer obj response)))
			       (setf (slot-value obj 'validation-errors) nil)
			       (setf (slot-value obj 'intermediate-form-values) nil)
			       (throw 'annihilate-dataform nil))
		 :on-cancel (lambda (obj)
			      (safe-funcall on-cancel obj)
			      (when answerp
				(answer obj))
			      (throw 'annihilate-dataform nil))
		 :form-view view
		 :class-store class-store
		 :satisfies satisfies))

(defmethod dataform-submit-action ((obj quickform) data &rest args)
  (if (quickform-satisfies obj)
      (apply #'call-next-method obj data
	     :satisfies (curry (quickform-satisfies obj) obj)
	     args)
    (call-next-method)))


