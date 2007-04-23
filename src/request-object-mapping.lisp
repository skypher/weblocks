
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
  (mapc (lambda (slot)
	  (let ((request-slot-value (request-parameter (attributize-name (cdr slot))))
		(slot-type (slot-definition-type (car slot))))
	    (when request-slot-value
	      (if (and (slot-in-request-empty-p slot-type request-slot-value)
		       (slot-value-required-p (car slot)))
		  (error "~A is required" (humanize-name (cdr slot)))
		  (if (not (slot-in-request-empty-p slot-type request-slot-value))
		      (let ((parsed-value (parse-slot-from-request slot-type request-slot-value)))
			(validate-slot-from-request obj slot parsed-value)
			(setf (slot-value obj (cdr slot))
			      parsed-value))
		      (setf (slot-value obj (cdr slot)) nil))))))
	(object-visible-slots obj :slots slots)))

;; Check if a value is all whitespace
;;     Check if a value is required
;;         yes -> error
;;         no -> nil
;; Parse value
;; Run validetors on that value
;; If all is well
;;     Store proper value
;;     else, store errors
;; If all is well
;;     Update object
;;     else, return erros

(defgeneric parse-slot-from-request (slot-type request-slot-value)
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

(defmethod parse-slot-from-request (slot-type request-slot-value)
  request-slot-value)

(defmethod parse-slot-from-request ((slot-type (eql 'integer)) request-slot-value)
  (parse-integer request-slot-value))
