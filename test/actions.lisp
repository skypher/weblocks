
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

;;; testing render-link
(deftest-html render-link-1
    (render-link "abc123" "some link")
  (:a :href "?action=abc123" "some link"))

;;; testing handle-client-request
(deftest handle-client-request-1
    (with-request :get nil
      (let (weblocks::*webapp-name* result1 result2)
	;; set up our mini-application with one dataform widget
	(declare (special weblocks::*webapp-name*))
	(defwebapp 'hello)
	(defun init-user-session (comp)
	  (push `("test" . ,(make-instance 'dataform :data *joe*)) (composite-widgets comp)))
	;; handle the first request (make sure data is drawn)
	(setf result1 (handle-client-request))
	;; unbind init-user-session to make root-composite persists
	(fmakunbound 'init-user-session)
	;; fake user clicking on "modify"
	(setf (slot-value *request* 'get-parameters) `((,weblocks::*action-string* . ,*dummy-action*)))
	;; handle another request (make sure form is drawn)
	(setf result2 (handle-client-request))
	(values result1 result2)))
p  "blah")
