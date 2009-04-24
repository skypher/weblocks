
(in-package :simple-blog)

;; Define callback function to initialize new sessions
(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(make-main-page)))
