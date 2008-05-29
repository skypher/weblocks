
(in-package :weblocks)

(export '(integer-parser integer-parser-radix integer-parser-min
	  integer-parser-max symbol-parser keyword-parser object-id
	  object-id-parser object-id-parser-class-name))

;;; Integer
(defclass integer-parser (parser)
  ((radix :initform 10
	  :initarg :radix
	  :accessor integer-parser-radix
	  :documentation "Parse the integer in the specified radix.")
   (min :initform nil
	:initarg :min
	:accessor integer-parser-min
	:documentation "If not null, the parsed value must be greater
	than or equal to this slot.")
   (max :initform nil
	:initarg :max
	:accessor integer-parser-max
	:documentation "If not null, the parsed value must be less
	than or equal to this slot."))
  (:default-initargs :error-message nil)
  (:documentation "A parser designed to parse strings into
  integers."))

(defmethod parser-error-message ((parser integer-parser))
  (with-slots (error-message) parser
    (or error-message
	(concatenate 'string
		     "This value must be an integer"
		     (when (integer-parser-min parser)
		       (format nil " greater than ~A" (- (integer-parser-min parser) 1)))
		     (when (and (integer-parser-min parser)
				(integer-parser-max parser))
		       " and")
		     (when (integer-parser-max parser)
		       (format nil " less than ~A" (+ (integer-parser-max parser) 1)))))))

(defmethod parse-view-field-value ((parser integer-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (declare (optimize safety))
  (ignore-errors
    (let* ((presentp (text-input-present-p value))
	   (integer-value (when presentp
			    (parse-integer value
					   :junk-allowed nil
					   :radix (integer-parser-radix parser)))))
      (when (and integer-value (integer-parser-min parser))
	(assert (>= integer-value (integer-parser-min parser))))
      (when (and integer-value (integer-parser-max parser))
	(assert (<= integer-value (integer-parser-max parser))))
      (values t presentp integer-value))))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'integer)) args)
  (values t (make-instance 'integer-parser)))

;;; Symbol
(defclass symbol-parser (parser)
  ()
  (:documentation "A parser designed to parse strings into
  symbols."))

(defun parse-symbol-from-request (request-slot-value)
  "Parser a symbol from request, respecting the value returned
by (readtable-case *readtable*). This function returns a string which
is later interned by a method of 'parse-view-field-value' specialized
on 'symbol' and 'keyword'."
  (ecase (readtable-case *readtable*)
    (:upcase (string-upcase request-slot-value))
    (:downcase (string-downcase request-slot-value))
    (:preserve request-slot-value)
    (:invert (if (or (every #'lower-case-p request-slot-value)
		     (every #'upper-case-p request-slot-value))
		 (string-invert-case request-slot-value)
		 request-slot-value))))

(defmethod parse-view-field-value ((parser symbol-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (values t (text-input-present-p value)
	  (intern
	   (parse-symbol-from-request value)
	   (symbol-package (view-field-slot-name field)))))

;;; Keyword
(defclass keyword-parser (parser)
  ()
  (:documentation "A parser designed to parse strings into
  keywords."))

(defmethod parse-view-field-value ((parser keyword-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (values t (text-input-present-p value)
	  (intern
	   (parse-symbol-from-request value)
	   (find-package :keyword))))

;;; Object id
(defclass object-id-parser (parser)
  ((class-name :initform nil
	       :initarg :class-name
	       :accessor object-id-parser-class-name
	       :documentation "A class of the object whose id is being
	       parsed."))
  (:default-initargs :error-message nil)
  (:documentation "A parser designed to convert an object id into an
  object instance."))

(defmethod parser-error-message ((parser object-id-parser))
  (with-slots (error-message) parser
    (or error-message
	(format nil "This value must be a valid ~A"
		(string-downcase
		 (humanize-name
		  (object-id-parser-class-name parser)))))))

(defmethod parse-view-field-value ((parser object-id-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (when (not (text-input-present-p value))
    (return-from parse-view-field-value (values t nil)))
  (let ((object
	 (find-persistent-object-by-id (class-store (object-id-parser-class-name parser))
				       (object-id-parser-class-name parser)
				       (parse-integer value :junk-allowed nil))))
    (when object
      (values t t object))))

