
(in-package :weblocks)

(export '(log-ui-action log-link log-form))

(defvar *rendered-actions*)
(setf (documentation '*rendered-actions* 'variable)
      "A list of actions rendered during the request.")

;; Logging UI action elements for easy querying in tests
(defun log-ui-action (type name action &key id class)
  "If called during an active unit test, logs the UI action to the
  test's temporary database so that it can be queried during the unit
  test's verification process. If no active unit test is present, does
  nothing."
  (declare (special *rendered-actions*))
  (when (boundp '*rendered-actions*)
    (push (list (cons :type type)
		(cons :name name)
		(cons :id id)
		(cons :class class)
		(cons :action action))
	  *rendered-actions*)))

(defun log-link (name action &key id class)
  "Wrapper around log-ui-action for links."
  (log-ui-action :link name action :id id :class class))

(defun log-form (action &key id class)
  "Wrapper around log-ui-action for forms."
  (log-ui-action :form nil action :id id :class class))

