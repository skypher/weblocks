
(in-package :weblocks)

(export '(*nested-parser-count* parse-slot-from-request))

(defparameter *nested-parser-count* 50
  "Because weblocks tries to expand typespecs, parsers can get into
infinite loops. This is the maximum number of nested calls before a
parser fails.")

;;; Parsers
(defun parse-foreign-object (obj slot-name slot-type request-slot-value)
  "Converts a foreign object id into an actual foreign object."
  (let ((fobject (find request-slot-value (form-potential-values obj slot-name slot-type)
		       :key (compose (curry #'format nil "~A") #'object-id) :test #'string-equal)))
    (if fobject
	(values t fobject)
	(values nil nil))))

(defgeneric parse-slot-from-request (obj slot-name slot-type request-slot-value)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Parses 'request-slot-value' into a type specified by
'slot-type'. By default implementations are provided for basic
types. If no type is specified for the slot or there is no matching
generic function, a string is used as is.

This function returns two values. The first value is true if
'request-slot-value' has successfully been parsed and false
otherwise. If the first value is true, the second value is the actual
parsed value.

This function is used to parse strings entered into forms into
appropriate types in order to populate object slots. Specialize this
function to provide customized behavior."))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql t)) request-slot-value)
  (if (render-slot-inline-p obj slot-name)
      (values t request-slot-value)
      (parse-foreign-object obj slot-name slot-type request-slot-value)))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type t) request-slot-value)
  (if (render-slot-inline-p obj slot-name)
      (call-next-method)
      (parse-foreign-object obj slot-name slot-type request-slot-value)))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type integer) request-slot-value)
  (ignore-errors
    (values t (parse-integer request-slot-value :junk-allowed nil))))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type string) request-slot-value)
  (values t request-slot-value))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql 'mod)) request-slot-value)
  (parse-slot-from-request obj slot-name
			   `(integer 0 (- (cadr slot-type) 1))
			   request-slot-value))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql 'eql)) request-slot-value)
  (invoke-parsers-on-slot obj slot-name (normalized-type-of (cadr slot-type)) request-slot-value))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql 'satisfies)) request-slot-value)
  (let ((sym (string-downcase (symbol-name (cadr slot-type)))))
    (invoke-parsers-on-slot obj slot-name
			    (find-symbol
			     (string-upcase
			      (cond
				((string-ends-with sym "-p") (string-remove-right sym "-p"))
				((string-ends-with sym "p") (string-remove-right sym "p"))
				(t sym)))
			     (symbol-package (cadr slot-type)))
			    request-slot-value)))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql 'or)) request-slot-value)
  (loop for i in (cdr slot-type)
       do (multiple-value-bind (successp value)
	      (invoke-parsers-on-slot obj slot-name i request-slot-value)
	    (when successp
	      (return (values t value))))))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql 'and)) request-slot-value)
  (loop for i in (cdr slot-type)
       do (multiple-value-bind (successp value)
	      (invoke-parsers-on-slot obj slot-name i request-slot-value)
	    (when successp
	      (return (values t value))))))

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql 'member)) request-slot-value)
  (loop for i in (cdr slot-type)
       do (multiple-value-bind (successp value)
	      (invoke-parsers-on-slot obj slot-name (normalized-type-of i) request-slot-value)
	    (when (and successp
		       (typep value slot-type))
	      (return (values t value))))))

;;; Invoking parsers
(defun invoke-parsers-on-slot-with-count (obj slot-name slot-type request-slot-value)
  "It's necessary to to count the number of parse calls because
parsers can get into infinite loops. See *nested-parser-count* for
details."
  (let ((*parse-counter* 0))
    (declare (special *parse-counter*))
    (invoke-parsers-on-slot obj slot-name slot-type request-slot-value)))

(defun invoke-parsers-on-slot (obj slot-name slot-type request-slot-value)
  "Attempts to parse 'request-slot-value' using
'parse-slot-from-request'. If the parser fails, expands 'slot-type'
using 'expand-typespec' and tries again.

This function returns two values. The first value is true if
'request-slot-value' has successfully been parsed and false
otherwise. If the first value is true, the second value is the actual
parsed value."
  (declare (special *parse-counter*))
  (when (> *parse-counter* *nested-parser-count*)
    (return-from invoke-parsers-on-slot nil))
  (let ((*parse-counter* (1+ *parse-counter*)))
    (declare (special *parse-counter*))
    (multiple-value-bind (successp value)
	(ignore-errors
	  (parse-slot-from-request obj slot-name slot-type request-slot-value))
      (if successp
	  (values t value)
	  (multiple-value-bind (successp value)
	      (ignore-errors
		(parse-slot-from-request obj slot-name (expand-typespec slot-type) request-slot-value))
	    (if successp
		(values t value)
		nil))))))

