
(in-package :weblocks)

(export '(disable-global-debugging enable-global-debugging))

(defun enable-global-debugging ()
  "Setup hooks for session maintenance and showing backtraces"
  ;; Set hunchentoot defaults (for everyone)
  (setf *show-lisp-errors-p* t)
  ;(setf *show-lisp-backtraces-p* t)
  ;; Set session maintenance (for everyone)
  (unless *maintain-last-session*
    (setf *maintain-last-session*
	  (bordeaux-threads:make-lock "*maintain-last-session*"))))

(defun disable-global-debugging ()
  "A manual method for resetting global debugging state"
  (setf *show-lisp-errors-p* nil)
  ;(setf *show-lisp-backtraces-p* nil)
  (setf *maintain-last-session* nil))

(defun render-debug-toolbar ()
  "When weblocks is started in debug mode, called on every request to
present the user with a toolbar that aids development."
  (with-html
    (:div :class "debug-toolbar"
	  (:a :href (make-action-url "debug-reset-sessions")
	      :title "Reset Sessions"
	      (:img :src (make-webapp-public-file-uri "images/reset.png")
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

;;; Further aid in debugging by reporting potential problems

(defun style-warn (condition &rest warn-args)
  "A variant of `warn' that doesn't guarantee evaluation of its
arguments."
  (apply #'warn condition warn-args))

(define-compiler-macro style-warn (condition &rest warn-args)
  `(when (or (not (boundp '*current-webapp*))
             (null *current-webapp*)
	     (weblocks-webapp-debug (current-webapp)))
     (warn ,condition . ,warn-args)))

(define-condition webapp-style-warning (style-warning)
  ()
  (:documentation "Signalled by Weblocks when detecting unwise
behavior on the part of webapps."))

(define-condition non-idempotent-rendering (webapp-style-warning)
  ((change-made :initarg :change-made :reader change-made-during-rendering
		:documentation "A description of the change that
		should be moved to action handling."))
  (:report report-non-idempotent-rendering)
  (:documentation "Signalled in common cases where code that alters
the webapp state appears in a rendering process."))

(defun report-non-idempotent-rendering (c stream)
  "Describe a condition where code appears in rendering that should be
in the action handler."
  (format stream "During the rendering phase, ~A, which should ~
		  typically be done only during action handling"
	  (change-made-during-rendering c)))

(define-condition misunderstood-action (webapp-style-warning)
  ((action :initarg :action :reader misunderstood-action
	   :documentation "What the user did to reveal this problem.")
   (missing :initarg :missing :reader missing-action-handler-part
	    :documentation "A description of what's missing for ACTION
	    to be handled correctly."))
  (:report report-misunderstood-action)
  (:documentation "Signalled when a user invoked an action, and some
part of the handler for which the app writer is responsible doesn't
seem to be implemented correctly."))

(defun report-misunderstood-action (c stream)
  "Describe a `misunderstood-action'."
  (format stream "A webapp user did: \"~A\"
But it was handled incorrectly; this is probably an issue with ~A"
	  (misunderstood-action c) (missing-action-handler-part c)))
