
(in-package :weblocks)

(export '(number-parser
          number-parser-min
          number-parser-max
	  integer-parser
          integer-parser-radix
          float-parser
          float-parser-round
	  symbol-parser
          keyword-parser
          object-id
          object-id-parser
	  object-id-parser-class-name
          object-id-parser-test))

;;; Numeric base
(defclass number-parser (parser)
  ((min :initform nil
	:initarg :min
	:accessor number-parser-min
	:documentation "If not null, the parsed value must be greater
	than or equal to this slot.")
   (max :initform nil
	:initarg :max
	:accessor number-parser-max
	:documentation "If not null, the parsed value must be less
	than or equal to this slot."))
  (:default-initargs :error-message nil)
  (:documentation "An abstract class that should serve as a base for numeric parsers."))

;;; Integer
(defclass integer-parser (number-parser)
  ((radix :initform 10
	  :initarg :radix
	  :accessor integer-parser-radix
	  :documentation "Parse the integer in the specified radix."))
  (:default-initargs :error-message nil)
  (:documentation "A parser designed to parse strings into
  integers."))

(defmethod parser-error-message ((parser integer-parser))
  (with-slots (error-message) parser
    (or error-message
	(concatenate 'string
		     "This value must be an integer"
		     (when (number-parser-min parser)
		       (format nil " greater than ~A" (- (number-parser-min parser) 1)))
		     (when (and (number-parser-min parser)
				(number-parser-max parser))
		       " and")
		     (when (number-parser-max parser)
		       (format nil " less than ~A" (+ (number-parser-max parser) 1)))))))

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
      (when (and integer-value (number-parser-min parser))
	(assert (>= integer-value (number-parser-min parser))))
      (when (and integer-value (number-parser-max parser))
	(assert (<= integer-value (number-parser-max parser))))
      (values t presentp integer-value))))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'integer)) args)
  (values t (make-instance 'integer-parser)))

;;; Float
(defclass float-parser (number-parser)
  ((round :type (or boolean null integer)
          :accessor float-parser-round
          :initarg :round
          :initform nil
          :documentation "If non-NIL, round the parsed float to ROUND decimals
          (0 if T is passed)."))
  (:default-initargs :error-message nil)
  (:documentation "A parser designed to parse strings into
  floats."))

(defmethod parser-error-message ((parser float-parser))
  (with-slots (error-message) parser
    (or error-message
	(concatenate 'string
		     "This value must be a decimal"
		     (when (number-parser-min parser)
		       (format nil " greater than ~A" (- (number-parser-min parser) 1)))
		     (when (and (number-parser-min parser)
				(number-parser-max parser))
		       " and")
		     (when (number-parser-max parser)
		       (format nil " less than ~A" (+ (number-parser-max parser) 1)))))))

(defmethod parse-view-field-value ((parser float-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (declare (optimize safety))
  (ignore-errors
    (let* ((presentp (text-input-present-p value))
	   (float-value (when presentp
                          (float (read-from-string value))))
           (round-factor (awhen (float-parser-round parser)
                           (expt 10 (if (eq it t) 0 it)))))
      (unless (floatp float-value)
        (error 'parse-error))
      (when (and float-value (number-parser-min parser))
	(assert (>= float-value (number-parser-min parser))))
      (when (and float-value (number-parser-max parser))
	(assert (<= float-value (number-parser-max parser))))
      (values t presentp (if round-factor
                           (/ (round (* float-value round-factor)) round-factor)
                           float-value)))))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'float)) args)
  (values t (make-instance 'float-parser)))

;;; Symbol
(defclass symbol-parser (parser)
  ((target-package :type (or package symbol string) :accessor symbol-parser-target-package
                   :initarg :target-package))
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
	   (if (slot-boundp parser 'target-package)
             (find-package (symbol-parser-target-package parser))
             (symbol-package (view-field-slot-name field))))))

;;; Keyword
(defclass keyword-parser (symbol-parser)
  ()
  (:default-initargs :target-package :keyword)
  (:documentation "A parser designed to parse strings into keywords."))

;;; Object id
(defclass object-id-parser (parser)
  ((class-name :type (or symbol null)
               :initform nil
	       :initarg :class-name
	       :accessor object-id-parser-class-name
	       :documentation "A class of the object whose id is being
	       parsed.")
   (test :type function
         :initform (constantly t)
         :initarg :test
         :accessor object-id-parser-test
         :documentation "A function of one argument that determines
         whether the parsed object is valid."))
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
    (when (and object (funcall (object-id-parser-test parser) object))
      (values t t object))))

