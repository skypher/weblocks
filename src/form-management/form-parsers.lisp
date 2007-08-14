
(in-package :weblocks)

(export '(parse-slot-from-request))

;;; Parsers
(defgeneric parse-slot-from-request (slot-type slot-name request-slot-value)
  (:documentation
   "Parses 'request-slot-value' into a type specified by
'slot-type'. By default implementations are provided for basic
types. If no type is specified for the slot or there is no matching
generic function, a string is used as is.

Returns a parsed value, or signals 'parse-error'.

This method is used to parse strings entered into forms into
appropriate types in order to populate object slots. Extend this
method to provide further specializations for 'slot-type'."))

(defmethod parse-slot-from-request ((slot-type (eql 'integer)) slot-name request-slot-value)
  (parse-integer request-slot-value :junk-allowed nil))

(defmethod parse-slot-from-request ((slot-type (eql t)) slot-name request-slot-value)
  request-slot-value)

(defmethod parse-slot-from-request ((slot-type (eql 'string)) slot-name request-slot-value)
  request-slot-value)

;;; Determine parser precidence list from typespec
(defun parsers-from-typespec (typespec)
  (remove-duplicates
   (append (parsers-from-typespec-aux typespec)
	   (parsers-from-typespec-aux (expand-typespec typespec)))
   :from-end t))

(defun parsers-from-typespec-aux (typespec)
  "Walks the typespec and returns a list of potential parser names."
  (when (not (listp typespec))
    (return-from parsers-from-typespec-aux
      (if (member typespec '(null))
	  nil
	  (list typespec))))
  (remove nil
	  (case (car typespec)
	    (mod (list 'integer))
	    (eql (list (type-of (cadr typespec))))
	    (member (mapcar #'type-of (cdr typespec)))
	    (not (list nil))
	    (satisfies (list nil))
	    (values (list nil))
	    (and (apply #'append (mapcar #'parsers-from-typespec-aux (cdr typespec))))
	    (or (apply #'append (mapcar #'parsers-from-typespec-aux (cdr typespec))))
	    (otherwise (list (car typespec))))))

;;; Invoking parsers
(defun invoke-parsers-on-slot (slot-type slot-name request-slot-value)
  "Obtains a list of parsers via 'parsers-from-typespec' and calls
each parser on the list until a value has been succesfully parsed.

This function returns two values. The first value is true if
'request-slot-value' has successfully been parsed and false
otherwise. If the first value is true, the second value is the actual
parsed value.

This function will signal an error if 'parsers-from-typespec' returns
no parsers or none of the parsers can be used due to lack of
specialization in 'parse-slot-from-request'."
  (let ((parser-list (parsers-from-typespec slot-type))
	one-parser-valid-p result error)
    (when (null parser-list)
      (error "No parsers could be obtained from type specifier ~A" slot-type))
    (loop for i in parser-list
	 do (handler-case (parse-slot-from-request i slot-name request-slot-value)
	      (parse-error (condition)
		(setf error condition)
		(setf one-parser-valid-p t))
	      (error (condition)
		(setf error condition))
	      (:no-error (res &rest args)
		(setf result res)
		(setf error nil)
		(loop-finish))))
    (if error
	(when (not one-parser-valid-p)
	  (error "No specialized methods for any of the parsers obtained from ~A"
		 slot-type))
	(values t result))))
