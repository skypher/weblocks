
(in-package :weblocks)

(export '(*required-field-message* *invalid-input-message*
	  *max-raw-input-length* *max-raw-input-length-error-message*
	  slot-from-request-valid-p invalid-input-error-message
	  max-raw-slot-input-length ))

(defparameter *required-field-message* "~A is a required field."
  "This message will be passed to 'format' along with the humanized
name of the field to inform users that the field is required.")

(defparameter *invalid-input-message* "~A must be ~A."
  "This message will be passed to 'format' along with the humanized
name of the field and humanized typespec to inform users that their
input is not valid.")

(defparameter *max-raw-input-length* 40
  "Default maximum allowed input length for input fields.")

(defparameter *max-raw-input-length-error-message*
  "~A must not exceed ~A characters."
  "A 'format' string that accepts two parameters (humanized slot name
and number of characters) used to display a max input size error
messages to the user.")

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

(defgeneric slot-from-request-valid-p (obj slot-name slot-type parsed-request-slot-value &rest args)
  (:documentation "Checks if the type of 'parsed-request-slot-value' is a valid
subtype of the slot-definition-type for 'slot'. Returns true if the
parsed value is valid, false otherwise.

Note, if type isn't declared this function always returns true.

'obj' - the object we're trying to deserialize into.
'slot-name' - name of the slot.
'slot-type' - declared type of the slot. Note,
slot-management-generic-function is *not* the metaclass for this
generic function as it would interfere with checking the
type. Therefore flexible specialization on 'slot-type' isn't possible.
'parsed-value-from-request' - value entered by the user after it was
parsed.")
  (:method (obj slot-name slot-type parsed-request-slot-value &rest args)
    (if slot-type
	(typep parsed-request-slot-value slot-type)
	t)))

(defgeneric invalid-input-error-message (obj slot-name humanized-name slot-type parsed-request-slot-value)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "This function returns an error message that's displayed to the
user when he enters invalid data. By default a message defined in
*invalid-input-message* is used. Specialize this function to output
custom error messages for specific slots/types.

Note that by default if slot-type isn't a compound only specifier
and (car-safe (ensure-list slot-type)) is not an external symbol,
slot-type is expanded via 'expand-typespec' to generate a better error
message."))

(defslotmethod invalid-input-error-message (obj slot-name humanized-name slot-type parsed-request-slot-value)
  (format nil *invalid-input-message* humanized-name
	  (humanize-typespec (case (symbol-status (car-safe (ensure-list slot-type)))
			       (:external slot-type)
			       (otherwise (expand-typespec slot-type))))))

;;; Some pre-parse validation
(defgeneric max-raw-slot-input-length (obj slot-name slot-type)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Must return a maximum length of user input for a given
slot. Default implementation returns the value of
*max-raw-input-length*."))

(defslotmethod max-raw-slot-input-length (obj slot-name slot-type)
  *max-raw-input-length*)

