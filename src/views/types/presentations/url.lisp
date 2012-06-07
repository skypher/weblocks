
(in-package :weblocks)

(export '(url url-presentation url-presentation-body))

(defclass url-presentation (text-presentation)
  ((body :initform nil
	 :initarg :body
	 :accessor url-presentation-body
	 :documentation "Body of the link. This can be a string or a
	 function that accepts same parameters as
	 'render-view-field-value'. The function is expected to render
	 the body into the usual output stream. Its return value is
	 ignored.")
   (escape :initform t
	   :initarg :escape
	   :accessor url-presentation-escape
	   :documentation "If true (the default), the URL will be HTML-
	   escaped on output to the page.")
   (nofollow :initform nil
	     :initarg :nofollow
	     :accessor url-presentation-nofollow
	     :documentation "If true, `rel=\"nofollow\" will be emitted to 
	     tell search engines that this is a user-entered link that
	     should not be considered in ranking the target."))
  (:documentation "Presents text as a URL. The link body is normally HTML-
  escaped; you can turn this off, if needed, by using `:escape nil',
  or circumvent it with a body function.  The 'href' attribute is
  not escaped; be careful to validate it in advance."))

(defmethod render-view-field-value (value (presentation url-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore highlight))
  (if (null value)
      (call-next-method)
      (with-html
	(:a :href value
	    :rel (and (url-presentation-nofollow presentation) "nofollow")
	    :onclick "stopPropagation(event);"
	    (flet ((out (x)
		     (if (url-presentation-escape presentation)
			 (esc x)
		       (str x))))
	      (etypecase (url-presentation-body presentation)
		(string (out (url-presentation-body presentation)))
		(function (apply (url-presentation-body presentation)
				 value presentation field view widget obj args))
		(null (out value))))))))

