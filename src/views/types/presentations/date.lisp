(in-package #:weblocks)

(export '(date-parser date-printing-mixin
	  date-presentation date-entry-presentation))

(defconstant +seconds-per-day+ (* 24 60 60))

(defun date->utime (day month year hour minute)
  (encode-universal-time 0 minute hour day month year))

(defclass date-parser (parser)
  ()
  (:default-initargs :error-message nil)
  (:documentation ""))

;; Note: this is a very simple parser -- it will try to match three
;; numbers separated by one of ./- and make them into a date assuming
;; it is in the format YYYY.MM.DD. If there is a HH:MM after the date,
;; it will get parsed as well.
(defmethod parse-view-field-value ((parser date-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (if (text-input-present-p value)
      (multiple-value-bind (matched elements)
	  (cl-ppcre:scan-to-strings "(\\d+)[\\-/\\.](\\d+)[\\-/\\.](\\d+)(\\s+(\\d+)[:\\.](\\d+))?" value)
	(when matched
	  (let ((date (date->utime (parse-integer (aref elements 2) :junk-allowed t)
				   (parse-integer (aref elements 1) :junk-allowed t)
				   (parse-integer (aref elements 0) :junk-allowed t)
				   (or (and (aref elements 4) (parse-integer (aref elements 4) :junk-allowed t)) 0)
				   (or (and (aref elements 5) (parse-integer (aref elements 5) :junk-allowed t)) 0))))
	    (when date (values t t date)))))
      (values t (text-input-present-p value) value)))


(defclass date-printing-mixin ()
  ((format :accessor date-printing-format :initarg :format
	   :initform "%Y-%m-%d"
	   :documentation "`format-date' format string to use. Default
	   is YYYY-MM-DD (ISO 8601 extended format)."))
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


