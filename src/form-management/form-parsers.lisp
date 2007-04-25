
(in-package :weblocks)

(export '(parse-validation-error parse-slot-from-request))

(define-condition parse-validation-error (form-validation-error)
  ((expected-type :accessor validation-expected-type :initarg :expected-type))
  (:report (lambda (condition stream)
	     (format stream "~S must be of type ~A."
		     (humanize-name (validation-error-slot condition))
		     (humanize-name (validation-expected-type condition)))))
  (:documentation "A condition signalled by form parsers when the
  string received as part of a form submission cannot be parsed into a
  type specified on an object slot. See 'parse-slot-from-request' for
  more details."))

(defgeneric parse-slot-from-request (slot-type slot-name request-slot-value)
  (:documentation
   "Parses 'request-slot-value' into a type specified by
'slot-type'. By default implementations are provided for basic
types. If no type is specified for the slot or there is no matching
generic function, a string is used as is.

Returns a parsed value, or signals a 'parse-validation-error'.

This method is used to parse strings entered into forms into
appropriate types in order to populate object slots. Extend this
method to provide further specializations for 'slot-type'."))

(defmethod parse-slot-from-request (slot-type slot-name request-slot-value)
  request-slot-value)

(defmethod parse-slot-from-request ((slot-type (eql 'integer)) slot-name request-slot-value)
  (handler-case (parse-integer request-slot-value)
    (parse-error (condition) (error (make-condition 'parse-validation-error
						    :slot-name slot-name
						    :expected-type 'integer)))))

