;;;; Generic form renderer
(in-package :weblocks)

(export '(with-form-header render-form-slot render-form))

(defgeneric with-form-header (obj body-fn &key name &allow-other-keys)
  (:documentation
   "Responsible for rendering headers of a form
presentation. Similar to 'with-data-header'."))

(defmethod with-form-header (obj body-fn &key name &allow-other-keys)
  (let ((header-class (format nil "renderer form ~A"
			      (attributize-name (object-class-name obj))))
	(object-name (if (null name)
			 (humanize-name (object-class-name obj))
			 name)))
    (with-html
      (:form :class header-class :action "#" :method "post"
	     (with-extra-tags
	       (htm (:fieldset
		     (:h1 (:span :class "action" "Modifying:&nbsp;")
			  (:span :class "object" (str object-name)))
		     (:ul (funcall body-fn)
			  (htm (:li :class "submit"
				    (:input :name "ok" :type "submit" :value "Submit")
				    (:input :name "cancel" :type "submit" :value "Cancel")))))))))))

(defgeneric render-form-slot (obj slot-name slot-value &rest args)
  (:documentation
   "Renders a given slot of a particular object. Similar to
'render-data-slot'."))

(defmethod render-form-slot (obj slot-name (slot-value standard-object) &rest args)
  (render-object-slot #'render-form #'render-form-slot obj slot-name slot-value args))

(defmethod render-form-slot (obj slot-name slot-value &rest args)
  (let ((attribute-slot-name (attributize-name slot-name)))
    (with-html
      (:li (:label
		   (:span (str (humanize-name slot-name)) ":&nbsp;")
		   (apply #'render-form slot-value :name attribute-slot-name args))))))

(defgeneric render-form (obj &rest keys &key inlinep name &allow-other-keys)
  (:documentation
   "A generic form presentation renderer. Similar to
'render-data'."))

(defmethod render-form ((obj standard-object) &rest keys &key inlinep name &allow-other-keys)
  (apply #'render-standard-object #'with-form-header #'render-form-slot obj keys))

(defmethod render-form (obj &rest keys &key inlinep name &allow-other-keys)
  (with-html
    (:input :type "text" :name name :value obj))
  *weblocks-output-stream*)

