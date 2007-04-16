
(in-package :weblocks)

(export '(update-object-from-request))

(defmethod update-object-from-request ((obj standard-object) &key slots &allow-other-keys)
  "Finds parameters in the request that match slots with
accessors (or slots passed in the key) and populates them with
the values from the form. If any given slot has a type specifier,
the request parameter is parsed into that type. Otherwise, the
value of the string is assigned to the slot.

'obj' - An object to be updated from the request.
'slots' - A list of slots to update that wouldn't be
rendered/updated normally (see 'object-visible-slots' for more
details)."
  (mapc (lambda (slot)
	  (let ((request-slot-value (request-parameter (attributize-name (cdr slot)))))
	    (when request-slot-value
	      (setf (slot-value obj (cdr slot)) request-slot-value))))
	(object-visible-slots obj :slots slots)))
