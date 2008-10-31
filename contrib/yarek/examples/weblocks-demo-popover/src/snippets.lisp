
(in-package :weblocks-demo-popover)

;;; Header
(defun render-header (&rest args)
  (declare (ignore args))
  "This function renders the page header."
  (with-html
    (:div :class "header"
	  (with-extra-tags))))

;;; Footer
(defmethod render-page-body :after ((app weblocks-demo-popover) rendered-html)
  (with-html
      (:div :class "footer"
	    (:p :id "system-info"
		"Running on "
		(str (concatenate 'string (server-type) " " (server-version)))
		" (" (str (concatenate 'string (lisp-implementation-type) " "
				      (lisp-implementation-version))) ")")
	    (:p :id "contact-info"
		"Contact me with any questions or comments at "
		(:a :href "mailto:coffeemug@gmail.com" "coffeemug@gmail.com") ".")
	    (:img :src "/pub/images/footer/valid-xhtml11.png" :alt "This site has valid XHTML 1.1.")
	    (:img :src "/pub/images/footer/valid-css.png" :alt "This site has valid CSS."))))

