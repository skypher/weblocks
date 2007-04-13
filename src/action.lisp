
(in-package :weblocks)

(defun make-action (action-fn)
  (let ((action-code (gensym)))
    (setf (session-value action-code) action-fn)
    action-code))

(defun render-link (action-code name)
  (let ((url (concatenate 'string "?action=" (princ-to-string action-code))))
    (with-html
      (:a :href url (str name)))))

;; if action exists, call that fn
;; then recurse over all widgets and render
(defun handle-client-request ()
  (let ((action-fn (session-value (get-parameter "action"))))
    (if action-fn
	(funcall action-fn))
    (hala)))
