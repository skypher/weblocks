
(in-package :weblocks)

(export '(*text-data-cutoff-threshold* excerpt excerpt-presentation
	  excerpt-presentation-cutoff-threshold))

(defparameter *text-data-cutoff-threshold* 15
  "In the excerpt mode, the number of characters to be rendered
before the text is cut off and an ellipsis is inserted.")

(defclass excerpt-presentation (text-presentation)
  ((cutoff-threshold :initform *text-data-cutoff-threshold*
		     :accessor excerpt-presentation-cutoff-threshold
		     :initarg :cutoff-threshold
		     :documentation "Number of characters before the
		     text is cut off."))
  (:documentation "Presents a large amount of text as an HTML
  paragraph."))

(defmethod render-view-field-value (value (presentation excerpt-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (if (null value)
      (call-next-method)
      (let* ((orig-item (apply #'print-view-field-value 
			       value (make-instance 'text-presentation) field view widget obj args))
	     (item (apply #'print-view-field-value value presentation field view widget obj args))
	     (lit-item (if highlight
			   (highlight-regex-matches item highlight presentation)
			   (escape-for-html item))))
	(with-html
	  (:span :class "value"
		 (str lit-item)
		 (unless (<= (length orig-item)
			     (excerpt-presentation-cutoff-threshold presentation))
		   (htm (:span :class "ellipsis" "..."))))))))

(defmethod print-view-field-value (value (presentation excerpt-presentation)
				   field view widget obj &rest args)
  (declare (ignore obj view field args))
  (let ((threshold (excerpt-presentation-cutoff-threshold presentation))
	(item (call-next-method)))
    (if (<= (length item) threshold)
	item
	(subseq item 0 threshold))))

