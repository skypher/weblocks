
(in-package :{APPNAME})

;; Define our application
(defwebapp '{APPNAME}
    :description "A web application based on Weblocks")

;; Set public files directory to {APPNAME}/pub
(setf *public-files-path* (compute-public-files-path :{APPNAME}))

;; Define callback function to initialize new sessions
(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(list (lambda (&rest args)
		(with-html
		  (:strong "Happy Hacking!"))))))

