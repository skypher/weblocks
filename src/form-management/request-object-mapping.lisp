
(in-package :weblocks)

(export '(update-object-from-request slot-in-request-empty-p))

(defgeneric update-object-from-request (obj &key slots &allow-other-keys)
  (:documentation
   "Tries to deserialize an object from a request via
'object-from-request-valid-p' (used to easily process forms), and in
case of success sets object slots to appropriate values.

If succeeded returns true. Otherwise returns nil as the first value,
and an association list of slot names and errors as the second value.

'obj' - An object to be updated from the request.
'slots' - A list of slots to update that wouldn't be
rendered/updated normally (see 'object-visible-slots' for more
details)."))

(defmethod update-object-from-request ((obj standard-object) &key slots &allow-other-keys)
  (let ((visible-slots (object-visible-slots obj :slots slots)))
    (multiple-value-bind (success results) (object-from-request-valid-p obj visible-slots)
      (if success
	  (mapc (lambda (slot)
		  (let ((parsed-value (assoc (attributize-name (cdr slot)) results :test #'string-equal)))
		    (when parsed-value
		      (setf (slot-value obj (cdr slot))
			    (cdr parsed-value)))))
		visible-slots)
	  (values nil results)))))

(defun object-from-request-valid-p (obj visible-slots)
  "Verifies whether form data that came in with the request can be
successfully deserialized into an object. In the process,
'parse-slot-from-request' is called to convert request strings into
appropriate types and 'validate-slot-from-request' is called to apply
validators that were defined on the slot. Special treatment is given
to the required validator via 'slot-value-required-p' - if a required
slot is missing ('slot-in-request-empty-p'), no further processing of
the slot can be done.

The function returns two values.

If parsing and validating the request completed successfully, the
first return value is true, and the second return value is an
association list of slot names and parsed results.

If the object coudln't be successfully deserialized, the first return
value is nil, and the second return value is an association list of
slot names and error conditions that prevented the slot from
deserializing.

Note, this function does not actually set the slot values, this is
done by 'update-object-from-request'."
  (let (errors results)
    (mapc (lambda (slot)
	    (let* ((slot-key (attributize-name (cdr slot)))
		   (request-slot-value (request-parameter slot-key))
		   (slot-type (slot-definition-type (car slot))))
	      (when request-slot-value
		(if (slot-in-request-empty-p slot-type request-slot-value)
		    (if (slot-value-required-p (car slot))
			(push `(,slot-key . ,(make-condition 'required-validation-error
							     :slot-name (cdr slot))) errors)
			(push `(,slot-key . nil) results))
		    (let (parsed-value)
		      (handler-case (progn
				      (setf parsed-value
					    (parse-slot-from-request slot-type (cdr slot)
								     request-slot-value))
				      (validate-slot-from-request obj slot parsed-value)
				      (push `(,slot-key . ,parsed-value) results))
			(form-validation-error (condition) (push `(,slot-key . ,condition) errors))))))))
	  visible-slots)
    (if errors
	(values nil errors)
	(values t results))))

(defgeneric slot-in-request-empty-p (slot-type request-slot-value)
  (:documentation
   "Returns true if the slot value in request is to be considered
'empty', i.e. all whitespace, 'Select State' dropdown value, etc.

Default implementation returns true when all characters of the request
value are a whitespace."))

(defmethod slot-in-request-empty-p (slot-type request-slot-value)
  (string-whitespace-p request-slot-value))

