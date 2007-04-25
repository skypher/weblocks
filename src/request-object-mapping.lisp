
(in-package :weblocks)

(export '(update-object-from-request parse-slot-from-request))

(defgeneric update-object-from-request (obj &key slots &allow-other-keys)
  (:documentation
   "Finds parameters in the request that match slots with
accessors (or slots passed in the key) and populates them with
the values from the form. If any given slot has a type specifier,
the request parameter is parsed into that type. Otherwise, the
value of the string is assigned to the slot.

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
  "Collects slot values and errors from the request."
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

(define-condition parse-validation-error (form-validation-error)
  ((expected-type :accessor validation-expected-type :initarg :expected-type))
  (:report (lambda (condition stream)
	     (format stream "~S must be of type ~A."
		     (humanize-name (validation-error-slot condition))
		     (humanize-name (validation-expected-type condition))))))

(defgeneric parse-slot-from-request (slot-type slot-name request-slot-value)
  (:documentation
   "Parses 'request-slot-value' into a type specified by
'slot-type'. By default implementations are provided for basic
types. If no type is specified for the slot or there is no matching
generic function, a string is used as is.

Returns a parsed value, or signals an error.

This method is used by 'update-object-from-request' to parse strings
entered into forms into appropriate types in order to populate object
slots. Extend this method to provide further specializations for
'slot-type'."))

(defmethod parse-slot-from-request (slot-type slot-name request-slot-value)
  request-slot-value)

(defmethod parse-slot-from-request ((slot-type (eql 'integer)) slot-name request-slot-value)
  (handler-case (parse-integer request-slot-value)
    (parse-error (condition) (error (make-condition 'parse-validation-error
						    :slot-name slot-name
						    :expected-type 'integer)))))
