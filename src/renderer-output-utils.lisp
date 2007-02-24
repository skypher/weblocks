;;;; Utility functions for generic renderers
(in-package :weblocks)

;;; Convert "some-name" or 'SOME-NAME to "Some Name"
(defun humanize-name (name)
  "Takes a string or a symbol and converts it to a human readable format.
   Dashes are replaced by spaces and words are capitalized."
  (let ((namestr (if (symbolp name)
		     (string-downcase (symbol-name name))
		     name)))
    (string-capitalize (substitute #\Space #\- namestr))))

;;; Convert 'SOME-NAME to  "some-name"
(defun attributize-name (name)
  "Takes a string or a symbol and converts it to html convential name"
  (let ((namestr (if (symbolp name)
		     (symbol-name name)
		     name)))
    (string-downcase namestr)))

;;; Returns a list of direct slot objects for a class and its subclasses
(defmethod object-visible-slots (obj)
  "Returns a list of direct slot objects for an object and its parents
   iff they have reader accessors."
  (class-visible-slots (class-of obj)))

;;; Returns a list of direct slot objects for a class and its subclasses
(defun class-visible-slots (cls)
  "Returns a list of direct slot objects for a class and its subclasses
   iff they have reader accessors."
  (if (eql (class-name cls) 'standard-object)
      nil
      (apply #'append (append (mapcar #'class-visible-slots (class-direct-superclasses cls))
			      (list (remove-if (compose #'null #'slot-definition-readers)
					       (class-direct-slots cls)))))))

;;; Takes some-object and returns its class name
(defmethod object-class-name (obj)
  "Returns an object's class name."
  (class-name (class-of obj)))

;;; If a reader accessor for the slot exists, get its value via the accessor
;;; otherwise, use slot-value to access the slot
(defun get-slot-value (obj slot)
  (let ((slot-reader (car (slot-definition-readers slot))))
    (if (null slot-reader)
	(slot-value obj (slot-definition-name slot))
	(funcall slot-reader obj))))

