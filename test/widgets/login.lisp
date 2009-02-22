
(in-package :weblocks-test)

;;; test authenticatedp
(deftest authenticatedp-1
    (with-request :get nil
      (authenticatedp))
  nil)

(deftest authenticatedp-2
    (with-request :get nil
      (setf (webapp-session-value *authentication-key*) 123)
      (authenticatedp))
  123)

;;; test logout
(deftest logout-1
    (with-request :get nil
      (setf (session-value *authentication-key*) 123)
      (logout)
      (authenticatedp))
  nil)

;;; test hash-password
(deftest hash-password-1
    (let ((test1 (hash-password "test1"))
	  (test2 (hash-password "test2")))
      (or (equalp test1 "test1")
	  (equalp test2 "test2")
	  (equalp test1 test2)))
  nil)

;;; test login flow
(deftest login-1
    (with-request :get nil
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (login (make-instance 'login
				  :on-login (lambda (w o)
					      (declare (ignore w))
					      (slot-value o 'email)))))
	(declare (special *weblocks-output-stream*))
	(render-widget-body login)
	(do-request `(("submit" . "Login")
		      ("email" . "Foo")
		      ("password" . "Bar")
		      (,weblocks::*action-string* . "abc123")))
	(authenticatedp)))
  "Foo")

