
(in-package :weblocks)

(export '(*required-field-message* *invalid-input-message*
	  invalid-input-error-message))

(defparameter *required-field-message* "~A is a required field."
  "This message will be passed to 'format' along with the humanized
name of the field to inform users that the field is required.")

(defparameter *invalid-input-message* "~A must be ~A."
  "This message will be passed to 'format' along with the humanized
name of the field and humanized typespec to inform users that their
input is not valid.")

(defun slot-value-required-p (class-name slot)
  "Returns true if 'slot' is declared to have an existance validator,
nil otherwise. See 'decl-validate' for more detauls.

'class-name' - the name of the class that contains the slot.
'slot' - either a slot definition or a slot name."
  (let ((type (slot-definition-type (if (symbolp slot)
					(get-slot-definition class-name slot)
					slot))))
    (and type
	 (not (typep nil type)))))

(defun slot-from-request-valid-p (obj slot parsed-request-slot-value)
  "Checks if the type of 'parsed-request-slot-value' is a valid
subtype of the slot-definition-type for 'slot'. Returns true if the
parsed value is valid, false otherwise.

Note, if type isn't declared this function always returns true.

'obj' - the object we're trying to deserialize into.
'slot' - slot definition object of the slot we're trying to
deserialize into.
'parsed-value-from-request' - value entered by the user after it was
parsed."
  (let ((type (slot-definition-type slot)))
    (if type
	(typep parsed-request-slot-value type)
	t)))

(defgeneric invalid-input-error-message (obj slot-name humanized-name slot-type parsed-request-slot-value)
  (:documentation
   "This function returns an error message that's displayed to the
user when he enters invalid data. By default a message defined in
*invalid-input-message* is used. Specialize this function to output
custom error messages for specific slots/types.

Note that by default if slot-type isn't a compound only specifier
and (car-safe (ensure-list slot-type)) is not an external symbol,
slot-type is expanded via 'expand-typespec' to generate a better error
message."))

(defmethod invalid-input-error-message (obj slot-name humanized-name slot-type parsed-request-slot-value)
  (format nil *invalid-input-message* humanized-name
	  (humanize-typespec (case (symbol-status (car-safe (ensure-list slot-type)))
			       (:external slot-type)
			       (otherwise (expand-typespec slot-type))))))
