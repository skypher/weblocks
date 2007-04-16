
(in-package :weblocks)

(defparameter *action-string* "action")

(defun make-action (action-fn)
  (let ((action-code (gensym)))
    (setf (session-value action-code) action-fn)
    action-code))

(defun render-link (action-code name)
  (let ((url (concatenate 'string "?" *action-string* "=" (princ-to-string action-code))))
    (with-html
      (:a :href url (str name)))))

;; if action exists, call that fn
;; then recurse over all widgets and render
(defun handle-client-request ()
  (let ((action-fn (get-request-action)))
    (when action-fn
      (funcall action-fn))
    (hala)))

(defun get-request-action ()
  (let ((action-name (request-parameter *action-string*))
	request-action)
    (when action-name
      (setf request-action (session-value action-name))
      (assert request-action (request-action) "Cannot find action.")
      request-action)))

