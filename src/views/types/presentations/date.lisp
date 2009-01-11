(in-package #:weblocks)

(export '(date-parser date-printing-mixin
	  date-presentation date-entry-presentation))

(defconstant +seconds-per-day+ (* 24 60 60))

(defun date->utime (day month year)
  (encode-universal-time 0 0 0 day month year 0))

(defclass date-parser (parser)
  ()
  (:default-initargs :error-message nil)
  (:documentation ""))

;; Note: this is a very simple parser -- it will try to match three
;; numbers separated by one of ./- and make them into a date assuming it
;; is in the format DD.MM.YYYY.
(defmethod parse-view-field-value ((parser date-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (if (text-input-present-p value)
      (multiple-value-bind (matched elements)
	  (cl-ppcre:scan-to-strings "(\\d+)[\\-/\\.](\\d+)[\\-/\\.](\\d+)" value)
	(when matched
	  (let ((date (date->utime (parse-integer (aref elements 0) :junk-allowed t)
				   (parse-integer (aref elements 1) :junk-allowed t)
				   (parse-integer (aref elements 2) :junk-allowed t))))
	    (when date
	      (values t t date)))))
      (values t (text-input-present-p value) value)))


(defclass date-printing-mixin ()
  ((format :accessor date-printing-format :initarg :format
	   :initform "%d/%m/%Y"
	   :documentation "`format-date' format string to use."))
  (:documentation "Show a universal time in a friendly
  `format-date'-generated form."))

(defmethod print-view-field-value (value (presentation date-printing-mixin)
				   field view widget obj &rest args)
  (declare (ignore obj view field widget args))
  (format-date (date-printing-format presentation) value))

(defclass date-presentation (text-presentation date-printing-mixin)
  ()
  (:documentation "Simple date display"))

(defclass date-entry-presentation (input-presentation date-printing-mixin)
  ()
  (:documentation "Simple date entry"))


