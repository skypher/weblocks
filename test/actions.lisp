
(in-package :weblocks-test)

;;; testing make-action
(deftest make-action/get-request-action-1
    (with-request :get nil
      (let ((action-name (make-action (lambda () 123))))
	(do-request `(("action" . ,action-name)))))
  123)

(deftest make-action/get-request-action-2
    (with-request :post nil
      (let ((action-name (make-action (lambda () 123))))
	(do-request `(("action" . ,action-name)))))
  123)

;;; testing make-action-url
(deftest make-action-url-1
    (make-action-url "test-action")
  "?action=test-action")

;;; testing render-link
(deftest-html render-link-1
    (render-link "abc123" "some link")
  (:a :href "?action=abc123" "some link"))

