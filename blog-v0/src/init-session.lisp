
(in-package :blog)

;; Define our application
(defwebapp 'blog
    :description "A web application based on Weblocks")

;; Set public files directory to blog/pub
(setf *public-files-path* (compute-public-files-path :blog))

;; Define callback function to initialize new sessions
(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(list (lambda (&rest args)
		(with-html
		  (:strong "Happy Hacking!"))))))

