
(in-package :weblocks)

(defwebapp weblocks-default 
    :description "A default welcome application for weblocks"
    :init-user-session 'init-user-session
    :prefix "/weblocks-default"
    :autostart nil)

(defmethod render-page-body :after ((app weblocks-default) rendered-html)
  (with-html
    (:div :class "footer"
	  (:p "Running on "
	      (str (concatenate
		    'string (server-type) " " (server-version)))
	      " (" (str (concatenate 'string (lisp-implementation-type) " "
				     (lisp-implementation-version))) ")")
	  (:img :src (make-webapp-public-file-uri "images/footer/valid-xhtml11.png") :alt "This site has valid XHTML 1.1.")
	  (:img :src (make-webapp-public-file-uri "images/footer/valid-css.png") :alt "This site has valid CSS."))))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(list
	 (lambda (&rest args)
	   (declare (ignore args))
	   (with-html
	     (:div :class "header"
		   (with-extra-tags))
	     (:h1 "Welcome to " (:em "Weblocks!"))
	     (:p "To learn more on how to get started
                 writing " (:em "Weblocks") " applications, please see the "
                 (:a :href "http://trac.common-lisp.net/cl-weblocks/wiki/UserManual" "user
                 manual.")
                 (:br) "For general information
                 about " (:em "Weblocks") ", information on how to get
                 support, find documentation, etc. please
                 start " (:a :href "http://common-lisp.net/project/cl-weblocks" "here") ".")
	     (:p (:em "Happy hacking!")))))))


