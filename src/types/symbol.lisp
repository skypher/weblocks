
(in-package :weblocks)

(defmethod data-print-object (obj slot-name slot-type (slot-value symbol) &rest args)
  (humanize-name (call-next-method)))

(defun parse-symbol-from-request (request-slot-value)
  "Parser a symbol from request, respecting the value returned
by (readtable-case *readtable*). This function returns a string which
is later interned by a method of 'parse-slot-from-request' specialized
on 'symbol' and 'keyword'."
  (ecase (readtable-case *readtable*)
    (:upcase (string-upcase request-slot-value))
    (:downcase (string-downcase request-slot-value))
    (:preserve request-slot-value)
    (:invert (if (or (every #'lower-case-p request-slot-value)
		     (every #'upper-case-p request-slot-value))
		 (string-invert-case request-slot-value)
		 request-slot-value))))

(defmethod parse-slot-from-request ((slot-type (eql 'symbol)) slot-name request-slot-value)
  (intern
   (parse-symbol-from-request request-slot-value)
   (symbol-package slot-name)))

