
(in-package :weblocks)

(export '(paragraph paragraph-presentation))

(defclass paragraph-presentation (text-presentation)
  ()
  (:documentation "Presents a large amount of text as an HTML
  paragraph."))

(defmethod render-view-field-value (value (presentation paragraph-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (if (null value)
      (call-next-method)
      (let* ((item (apply #'print-view-field-value value presentation field view widget obj args))
	     (lit-item (if highlight
			   (highlight-regex-matches item highlight presentation)
			   (escape-for-html item))))
	(with-html
	  (:p :class "value text"
	      (str (apply #'concatenate 'string
			  (intersperse (tokenize-string lit-item
							:delimiter #\Newline
							:include-empties? t)
				       "<br />"))))))))

