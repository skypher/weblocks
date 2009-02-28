
(in-package :weblocks-test)

;;; testing get-request-action-name
(deftest get-request-action-name-1
    (with-request :get '(("action" . "blah"))
      (setf (slot-value *request* 'post-parameters) '(("action" . "blah1")))
      (weblocks::get-request-action-name))
  "blah")

(deftest get-request-action-name-2
    (with-request :get nil
      (setf (slot-value *request* 'post-parameters) '(("action" . "blah")))
      (weblocks::get-request-action-name))
  "blah")

;;; testing make-action
(deftest make-action/get-request-action-1
    (with-request :get nil
      (let ((action-name (make-action (lambda (&rest keys) 123))))
	(do-request `(("action" . ,action-name)))))
  123)

(deftest make-action/get-request-action-2
    (with-request :post nil
      (let ((action-name (make-action (lambda (&rest keys) 123))))
	(do-request `(("action" . ,action-name)))))
  123)

(deftest make-action/get-request-action-3
    (with-request :post nil
      (let ((action-name (make-action (lambda (&rest keys) 123))))
	(setf (slot-value *request* 'get-parameters) `(("action" . ,action-name)))
	(weblocks::eval-action)))
  123)

;;; test function-or-action->action
(deftest function-or-action->action-1
    (with-request :get nil
      (multiple-value-bind (res err)
	  (ignore-errors
	    (weblocks::function-or-action->action "abc123"))
	(values res (not (null err)))))
  nil t)

(deftest function-or-action->action-2
    (with-request :get nil
      (make-action #'identity "abc123")
      (weblocks::function-or-action->action "abc123"))
  "abc123")

(deftest function-or-action->action-3
    (with-request :get nil
      (weblocks::function-or-action->action #'identity))
  "abc123")

;;; testing make-action-url
(deftest make-action-url-1
    (with-request :get nil
      (make-action-url "test-action"))
  "/foo/bar?action=test-action")

;;; test eval-action
(deftest eval-action-1
    (with-request :get `(("name" . "Bob")
			 ("cancel" . "Cancel")
			 (,weblocks::*action-string* . "abc123"))
      (make-action (lambda (&key name cancel &allow-other-keys)
		     (concatenate 'string name cancel)))
      (weblocks::eval-action))
  "BobCancel")

