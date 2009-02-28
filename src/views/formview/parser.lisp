
(in-package :weblocks)

(export '(parser parser-error-message parse-view-field-value
	  parser-class-name text text-parser text-parser-matches
	  text-input-present-p))

;;; Parser
(defclass parser ()
  ((error-message :initform "This value must be valid"
		  :accessor parser-error-message
		  :initarg :error-message
		  :documentation "A default error message. If subclasses
override 'parser-error-message', it is still recommended that they allow
callers to override the error message using the ':error-message'
initarg. See the implementation of 'integer-parser' for an example of
this.")))

;;; Parsing protocol
(defgeneric parse-view-field-value (parser value obj view field &rest args)
  (:documentation "Parse a string 'value' obtained from a request
using a specified 'parser' and save it into an appopriate 'obj' slot
obtained from a view 'field'.

This function returns three values. The first value is true if
'request-slot-value' has successfully been parsed and false
otherwise. If the first value is true, the second value is true if the
input is not empty, and false otherwise. If the second value is true,
the third value is the actual parsed value.

Specialize this function to add custom parsers or to parse given
fields differently."))

(defgeneric parser-class-name (parser-type)
  (:documentation "Given a type of the parser, returns its class
name. Default implementation adds '-parser' to the type and returns
the symbol.")
  (:method (parser-type)
    (entity-class-name parser-type '#:-parser)))

;;; Compiler hook
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod view-argument-quoting-strategy ((arg-name (eql :present-as)))
    :list)
  
  (setf (gethash :parse-as *custom-view-field-argument-compilers*)
	(lambda (slot-name parser)
	  (let ((parser (ensure-list parser)))
	    `(setf (form-view-field-parser ,slot-name)
		   (funcall #'make-instance (parser-class-name ',(car parser))
			    ,@(quote-property-list-arguments
			       (cdr parser))))))))

;;; Default parser
(defclass text-parser (parser)
  ((matches :initform nil
	    :accessor text-parser-matches
	    :initarg :matches
	    :documentation "If this slot is a regular expression, the
	   input value will be validated against it."))
  (:documentation "A default parser for forms. Simply returns strings
  obtained from the request."))

(defmethod parse-view-field-value ((parser text-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (if (and (text-parser-matches parser)
           (text-input-present-p value))
      (let ((res (ppcre:all-matches (text-parser-matches parser) value)))
        (when (and res (= (length res) 2))
            (values t (text-input-present-p value) value)))
      (values t (text-input-present-p value) value)))

(defun text-input-present-p (value)
  "Returns true if the text input is to be considered non-empty."
  (not (string-whitespace-p (or value ""))))

