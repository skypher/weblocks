
(in-package :weblocks)

(export '(data data-view data-view-field data-scaffold
	  text-presentation highlight-regex-matches))

;;; Data view
(defclass data-view (view)
  ()
  (:documentation "A view designed to present data to the user."))

;;; Data view field
(defclass data-view-field (inline-view-field)
  ((presentation :initform (make-instance 'text-presentation)))
  (:documentation "A field class of the data view."))

;;; Make scaffolding system happy
(defclass data-scaffold (scaffold)
  ())

;;; Presentation
(defclass text-presentation (presentation)
  ()
  (:documentation "A default presentation that renders values as
  text."))

;;; Custom view caption
(defmethod view-caption ((view data-view))
  (if (slot-value view 'caption)
      (slot-value view 'caption)
      (with-html-output-to-string (out)
	(:span :class "action" "Viewing:&nbsp;")
	(:span :class "object" "~A"))))

;;; Implement rendering protocol
(defmethod with-view-header ((view data-view) obj widget body-fn &rest args &key
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (with-html
    (:div :class (format nil "view data ~A"
			 (attributize-name (object-class-name obj)))
	  (with-extra-tags
	    (htm
	     (unless (empty-p (view-caption view))
	       (htm (:h1 (fmt (view-caption view)
			      (humanize-name (object-class-name obj))))))
	     (safe-apply fields-prefix-fn view obj args)
	     (:ul (apply body-fn view obj args))
	     (safe-apply fields-suffix-fn view obj args))))))

(defmethod render-view-field ((field data-view-field) (view data-view)
			      widget presentation value obj
			      &rest args)
  (with-html
    (:li :class (attributize-name (view-field-slot-name field))
	 (unless (empty-p (view-field-label field))
	   (htm (:span :class (concatenate 'string "label "
					   (attributize-presentation
					    (view-field-presentation field)))
		       (str (view-field-label field)) ":&nbsp;")))
	 (apply #'render-view-field-value
		value presentation
		field view widget obj
		args))))

(defvar *presentation-dom-id* nil "DOM id of the currently rendered
  presentation object. If bound during rendering (for example, in
  an :around method for render-object-view, will be used by the various
  functions that render form elements..")

(defmethod render-view-field-value (value (presentation text-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (let ((printed-value (apply #'print-view-field-value value presentation field view widget obj args)))
    (with-html
      (:span :class "value"
	     (str (if highlight
		      (highlight-regex-matches printed-value highlight)
		      (escape-for-html printed-value)))))))

(defmethod render-view-field-value ((value null) (presentation text-presentation)
				    field view widget obj &rest args
				    &key ignore-nulls-p &allow-other-keys)
  (declare (ignore args))
  (if ignore-nulls-p
      (call-next-method)
      (with-html
	(:span :class "value missing" "Not Specified"))))

(defun highlight-regex-matches (item highlight)
  "This function highlights regex matches in text by wrapping them in
HTML 'string' tag. The complexity arises from the need to escape HTML
to prevent XSS attacks. If we simply wrap all matches in 'strong' tags
and then escape the result, we'll escape our 'strong' tags which isn't
the desired outcome."
  (apply #'concatenate 'string
	 (remove ""
		 (loop for i = 0 then k
		       for (j k . rest) on (ppcre:all-matches highlight item) by #'cddr
		       for match = (subseq item j k)
		    collect (escape-for-html (subseq item i j)) into matches
		    when (not (equalp match ""))
		         collect (format nil "<strong>~A</strong>"
					 (escape-for-html match))
		           into matches
		    when (null rest) collect (escape-for-html (subseq item k (length item))) into matches
		    finally (return (if matches
					matches
					(list (escape-for-html item))))))))

(defmethod print-view-field-value (value presentation field view widget obj &rest args)
  (declare (ignore presentation obj view field args))
  (format nil "~A" value))

(defmethod print-view-field-value ((value symbol) presentation field view widget obj &rest args)
  (declare (ignore presentation obj view field args))
  (humanize-name (call-next-method)))

(defmethod print-view-field-value ((value standard-object) presentation field view widget obj &rest args)
  (declare (ignore presentation obj view field args))
  (humanize-name (object-class-name value)))

