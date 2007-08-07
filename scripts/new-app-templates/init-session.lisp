
(in-package :{APPNAME})

(defwebapp '{APPNAME}
    :description "A web application based on Weblocks")

(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(list (lambda (&rest args)
		(with-html
		  (:strong "Happy Hacking!"))))))

