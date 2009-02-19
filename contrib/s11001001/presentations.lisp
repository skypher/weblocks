;;; presentations.lisp: Additional presentations.

(in-package #:weblocks-s11)

(export '(us-cents us-cents-input us-cents-presentation
	  us-cents-input-presentation us-cents-parser))

(arnesi:enable-sharp-l-syntax)

;;;; Money represented in US dollars and stored as #cents

(defclass us-cents-printer ()
  ()
  (:documentation "Mixin for data and form; see
  `us-cents-presentation' and `us-cents-input-presentation'."))

(defclass us-cents-presentation (us-cents-printer text-presentation)
  ()
  (:documentation "Present a count of US cents as a pretty US dollar
  amount."))

(defclass us-cents-input-presentation (us-cents-printer input-presentation)
  ()
  (:documentation "The counterpart to `us-cents-presentation' for
  forms."))

(defclass us-cents-parser (text-parser)
  ()
  (:documentation "Parse a US dollar amount and answer the # of US cents."))

(defmethod print-view-field-value
    (value (self us-cents-printer) field view widget obj &rest args)
  (declare (ignore field view widget obj args))
  (multiple-value-bind (dollars cents) (truncate value 100)
    (format nil "$~:D.~2,'0D" dollars cents)))

(defmethod weblocks:parse-view-field-value
    ((parser us-cents-parser) value obj view field &rest args)
  (declare (ignore obj view field args))
  (let* ((present? (text-input-present-p value))
	 (float-start
	  (and present?
	       (position-if #L(or (char<= #\0 !1 #\9) (char= #\. !1)) value)))
	 (float
	  (and float-start
	       (ignore-errors (arnesi:parse-float value :start float-start)))))
    (values (or (not present?) float) present?
	    (and float (round (* 100 float))))))

;;; presentations.lisp ends here
