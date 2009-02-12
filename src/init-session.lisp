
(in-package :blog)

;; Define callback function to initialize new sessions
(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(make-admin-page)))
