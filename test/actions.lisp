
(in-package :weblocks-test)

;;; testing make-action
(deftest make-action/get-request-action-1
    (with-request :get nil
      (let ((action-name (make-action (lambda () 123))))
	(setf (slot-value *request* 'get-parameters) `(("action" . ,action-name)))
	(funcall (weblocks::get-request-action))))
  123)

;;; testing render-link
(deftest-html render-link-1
    (render-link "abc123" "some link")
  (:a :href "?action=abc123" "some link"))

