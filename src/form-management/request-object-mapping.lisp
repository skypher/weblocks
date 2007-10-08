
(in-package :weblocks)

(export '(update-object-from-request slot-in-request-empty-p request-parameters-for-object))

(defgeneric update-object-from-request (obj &rest args)
  (:documentation
   "Tries to deserialize an object from a request via
'object-from-request-valid-p' (used to easily process forms), and in
case of success sets object slots to appropriate values.

If succeeded returns true. Otherwise returns nil as the first value,
and an association list of slot names and errors as the second value.

'obj' - An object to be updated from the request.

Other arguments normally passed to weblocks functions (see
'object-visible-slots') apply to 'update-object-from-request' as
well."))

(defmethod update-object-from-request ((obj standard-object) &rest args)
  (multiple-value-bind (success results) (apply #'object-from-request-valid-p obj args)
    (if success
	(apply #'update-object-from-request-aux obj results args)
	(values nil (reverse results)))))

(defun update-object-from-request-aux (obj parsed-request &rest args)
  "An auxillary function used to implement update-object-from-request
method."
  (mapc (lambda (slot)
	  (let* ((slot-name (slot-definition-name (vs-slot-definition slot)))
		 (parsed-value (assoc (attributize-name slot-name) parsed-request :test #'string-equal)))
	    (when parsed-value
	      (setf (slot-value (vs-object slot) slot-name)
		    (cdr parsed-value)))))
	(apply #'object-visible-slots obj args)))

(defun object-from-request-valid-p (object &rest args)
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
	    (let* ((slot-name (slot-definition-name (vs-slot-definition slot)))
		   (slot-key (attributize-name slot-name))
		   (request-slot-value (request-parameter slot-key))
		   (slot-type (slot-definition-type (vs-slot-definition slot)))
		   (human-slot-name (humanize-name (vs-slot-presentation slot)))
		   (obj (vs-object slot)))
	      (if (slot-in-request-empty-p slot-type request-slot-value)
		  (if (slot-value-required-p (class-name-of obj) (vs-slot-definition slot))
		      (push (cons slot-key (format nil *required-field-message*
						   human-slot-name))
			    errors)
		      (push (cons slot-key nil) results))
		  (if (> (length request-slot-value)
			 (max-raw-slot-input-length obj slot-name slot-type))
		      (push (cons slot-key
				  (format nil *max-raw-input-length-error-message*
					  human-slot-name (max-raw-slot-input-length obj slot-name
										     slot-type)))
			    errors)
		      (multiple-value-bind (parsedp parsed-value)
			  (invoke-parsers-on-slot-with-count obj slot-name slot-type request-slot-value)
			(if (and parsedp (slot-from-request-valid-p obj slot-name slot-type parsed-value))
			    (push (cons slot-key parsed-value) results)
			    (push (cons slot-key
					(invalid-input-error-message obj slot-name human-slot-name
								     slot-type parsed-value))
				  errors)))))))
	  (apply #'object-visible-slots object args))
    (if errors
	(values nil errors)
	(values t results))))

(defgeneric slot-in-request-empty-p (slot-type request-slot-value)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Returns true if the slot value in request is to be considered
'empty', i.e. all whitespace, 'Select State' dropdown value, etc.

Note, 'request-slot-value' is a string value entered by the user, not
the parsed value.

Default implementation returns true when all characters of the request
value are a whitespace."))

(defslotmethod slot-in-request-empty-p (slot-type request-slot-value)
  (string-whitespace-p (or request-slot-value "")))

(defun request-parameters-for-object (object &rest args)
  "Returns a copy of the request parameters taking into account a
particular object. This function is necessary because in certain cases
web browsers don't send some parameters (e.g. unchecked checkboxes).

Note that the object is *not* modified by this function.

'parameters' - an alist of request parameters (can be obtained
via 'request-parameters'.)
'object' - the object to take account of.
'args' - a set of arguments to be passed to 'object-visible-slots'."
  (apply #'append
	 (mapcar (lambda (slot)
		   (let* ((slot-name (slot-definition-name (vs-slot-definition slot)))
			  (slot-key (attributize-name slot-name))
			  (request-slot-value (request-parameter slot-key)))
		     (list (cons slot-key request-slot-value))))
		 (apply #'object-visible-slots object args))))
