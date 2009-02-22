
(in-package :{APPNAME})

;; Define callback function to initialize new sessions
(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(list (lambda (&rest args)
		(with-html
		  (:strong "Happy Hacking!"))))))

