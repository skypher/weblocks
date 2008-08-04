
(in-package :weblocks)

(export '(disable-global-debugging enable-global-debugging))

(defun enable-global-debugging ()
  "Setup hooks for session maintenance and showing backtraces"
  ;; Set hunchentoot defaults (for everyone)
  (setf *show-lisp-errors-p* t)
  (setf *show-lisp-backtraces-p* t)
  ;; Set session maintenance (for everyone)
  (unless *maintain-last-session*
    (setf *maintain-last-session*
	  (hunchentoot-mp:make-lock "*maintain-last-session*"))))

(defun disable-global-debugging ()
  "A manual method for resetting global debugging state"
  (setf *show-lisp-errors-p* nil)
  (setf *show-lisp-backtraces-p* nil)
  (setf *maintain-last-session* nil))

(defun render-debug-toolbar ()
  "When weblocks is started in debug mode, called on every request to
present the user with a toolbar that aids development."
  (with-html
    (:div :class "debug-toolbar"
	  (:a :href (make-action-url "debug-reset-sessions")
	      :title "Reset Sessions"
	      (:img :src (make-webapp-uri "/pub/images/reset.png")
		    :alt "Reset Sessions")))))

(defun initialize-debug-actions ()
  "When weblocks is started in debug mode, called on session
initalization so that appropriate actions that aid development can be
created."
  (make-action (lambda (&rest args)
		 (declare (ignore args))
		 (reset-sessions)
		 (redirect (make-webapp-uri "/")))
	       "debug-reset-sessions"))

