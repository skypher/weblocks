
(in-package :weblocks-test)

(export '(find-ui-actions find-ui-link find-ui-form))

;; Querying UI elements
(defun find-ui-actions (type &key name id class (test #'equalp))
  "If called during an active unit test, looks for the action in the
  unit test database and returns the action (if found), or nil. If no
  active unit test is present, signals an error."
  (declare (special weblocks::*rendered-actions*))
  (unless (boundp 'weblocks::*rendered-actions*)
    (error "FIND-UI-ACTION can only be called during an active unit test."))
  ;; Return elements that match the query
  (flet ((prop-match (action prop val &optional (test #'equalp))
	   (funcall test val (cdr (assoc prop action)))))
    (loop
       for action in weblocks::*rendered-actions*
       when (and (prop-match action :type type)
		 (or (null name) (prop-match action :name name test))
		 (or (null id) (prop-match action :id id test))
		 (or (null class) (prop-match action :class class test)))
       collect (cdr (assoc :action action)))))

(defun find-ui-link (&key name id class (test #'equalp))
  "A wrapper for find-ui-actions for links. Returns the first link in
the list, if any."
  (car (find-ui-actions :link :name name :id id :class class :test test)))

(defun find-ui-form (&key id class (test #'equalp))
  "A wrapper for find-ui-action for forms. Returns the first form in
the list, if any."
  (car (find-ui-actions :form :id id :class class :test test)))

