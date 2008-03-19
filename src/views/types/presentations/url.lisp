
(in-package :weblocks)

(export '(url url-presentation url-presentation-body))

(defclass url-presentation (text-presentation)
  ((body :initform "Link"
	 :initarg :body
	 :accessor url-presentation-body
	 :documentation "Body of the link. This can be a string or a
	 function that accepts same parameters as
	 'render-view-field-value'. The function is expected to render
	 the body into the usual output stream. Its return value is
	 ignored."))
  (:documentation "Presents text as a url."))

(defmethod render-view-field-value (value (presentation url-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore highlight))
  (if (null value)
      (call-next-method)
      (with-html
	(:a :href value
	    :onclick "stopPropagation(event);"
	    (etypecase (url-presentation-body presentation)
	      (string (str (url-presentation-body presentation)))
	      (function (apply (url-presentation-body presentation)
			       value presentation field view widget obj args)))))))

