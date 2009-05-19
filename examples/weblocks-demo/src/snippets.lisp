
(in-package :weblocks-demo)

;;; Header
(defun render-header (&rest args)
  (declare (ignore args))
  "This function renders the page header."
  (with-html
    (:div :class "header"
	  (with-extra-tags))))

;;; Footer
(defmethod render-page-body :after ((app weblocks-demo) rendered-html)
  (with-html
      (:div :class "footer"
	    #|(:p :id "system-info"
		"Running on "
		(str (concatenate 'string (server-type) " " (server-version)))
		" (" (str (concatenate 'string (lisp-implementation-type) " "
				      (lisp-implementation-version))) ")")|#
	    (:p :id "contact-info" :style "color:white;"
		"Contact us with any questions or comments at the "
		(:a :href "http://groups.google.com/group/weblocks" "Weblocks Google Group") ".") (:br)
	    ;;(:img :src "/pub/images/footer/valid-xhtml11.png" :alt "This site has valid XHTML 1.1.")
	    ;;(:img :src "/pub/images/footer/valid-css.png" :alt "This site has valid CSS.")
	    )))

