;;;; Generic data renderer
(in-package :weblocks)

(defun render-extra-tags (tag-class count)
  (with-html-output (*weblocks-output-stream*)
    (loop for i from 1 to count
          for attr = (format nil "~A~A" tag-class i)
       do (htm (:div :class attr "&nbsp;")))))

(defmethod with-data-header (obj body-fn)
  (let ((header-class (format nil "data ~A"
			      (attributize-name (object-class-name obj)))))
    (with-html-output (*weblocks-output-stream*)
      (:div :class header-class
	    (render-extra-tags "extra-top-" 3)
	    (htm (:h1 (:span :class "action" "Viewing:&nbsp;")
		      (:span :class "object" (str (humanize-name (object-class-name obj)))))
		 (:ul (funcall body-fn)))
	    (render-extra-tags "extra-bottom-" 3)))))

(defmethod render-data-slot (obj slot-name (slot-value standard-object) &rest args)
  (if (render-slot-inline-p obj slot-name)
      (apply #'render-data slot-value :inlinep t args)
      (apply #'render-data-slot obj slot-name (object-name slot-value) args)))

(defmethod render-data-slot (obj slot-name slot-value &rest args)
  (with-html-output (*weblocks-output-stream*)
    (:li (:h2 (str (humanize-name slot-name)) ":")
	 (apply #'render-data slot-value args))))

(defmethod render-data ((obj standard-object) &rest keys &key inlinep &allow-other-keys)
  (let ((render-body
	 (lambda ()
	   (let ((keys-copy (copy-list keys)))
	     (remf keys-copy :inlinep)
	     (mapc (lambda (slot)
		     (apply #'render-data-slot obj (cdr slot)
			    (get-slot-value obj (car slot)) keys))
		   (apply #'object-visible-slots obj keys-copy))))))
    (if inlinep
	(funcall render-body)
	(with-data-header obj render-body))
    *weblocks-output-stream*))

(defmethod render-data (obj &rest keys &key inlinep &allow-other-keys)
  (with-html-output (*weblocks-output-stream*)
    (:span (str obj)))
  *weblocks-output-stream*)

