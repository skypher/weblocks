
(in-package :weblocks-test)

;;; test refresh-request-p
(deftest refresh-request-p-1
    (with-request :get nil
      (setf (session-value 'weblocks::last-request-uri) '("foo" "bar"))
      (refresh-request-p))
  t)

(deftest refresh-request-p-2
    (with-request :get nil
      (setf (session-value 'weblocks::last-request-uri) '("foo"))
      (refresh-request-p))
  nil)

(deftest refresh-request-p-3
    (with-request :get `((,weblocks::*action-string* . "abc123"))
      (make-action (lambda () nil))
      (setf (session-value 'weblocks::last-request-uri) '("foo" "bar"))
      (refresh-request-p))
  nil)

;;; test initial-request-p
(deftest initial-request-p-1
    (with-request :get nil
      (initial-request-p))
  nil)

(deftest initial-request-p-2
    (with-request :get nil
      (setf (session-value 'last-request-uri) :none)
      (initial-request-p))
  nil)

;;; test ajax-request-p
(deftest ajax-request-p-1
    (with-request :get nil
      (ajax-request-p))
  nil)

(deftest ajax-request-p-2
    (with-request :get nil
      (make-request-ajax)
      (ajax-request-p))
  "test")

;;; test pure-request-p
(deftest pure-request-p-1
    (with-request :get nil
      (not (null (pure-request-p))))
  nil)

(deftest pure-request-p-2
    (with-request :get '(("pure" . blah))
      (not (null (pure-request-p))))
  nil)

(deftest pure-request-p-3
    (with-request :get '(("pure" . true))
      (not (null (pure-request-p))))
  t)

;;; test redirect
(deftest redirect-1
    (with-request :get nil
      (catch 'handler-done
	(redirect "/foo")
	1))
  nil)

(deftest redirect-2
    (with-request :get nil
      (make-request-ajax)
      (catch 'handler-done
	(redirect "/foo")
	1))
  "{\"redirect\":\"/foo\"}")
