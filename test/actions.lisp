
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

;;; testing make-action-url
(deftest make-action-url-1
    (make-action-url "test-action")
  "?action=test-action")

;;; testing render-link
(deftest-html render-link-1
    (with-request :get nil
      (render-link "abc123" "some link"))
  #.(link-action-template "abc123" "some link"))

;;; test eval-action
(deftest eval-action-1
    (with-request :get `(("name" . "Bob")
			 ("cancel" . "Cancel")
			 (,weblocks::*action-string* . "abc123"))
      (make-action (lambda (&key name cancel &allow-other-keys)
		     (concatenate 'string name cancel)))
      (weblocks::eval-action))
  "BobCancel")

