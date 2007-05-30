
(in-package :weblocks-test)

;;; test defwebapp
(deftest defwebapp-1
    (let (weblocks::*webapp-name*)
      (declare (special weblocks::*webapp-name*))
      (defwebapp 'hello)
      weblocks::*webapp-name*)
  hello)

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

;;; test ajax-request-p
(deftest ajax-request-p-1
    (with-request :get nil
      (setf (slot-value *request* 'hunchentoot::headers-in)
	    (cons '("X-Requested-With" . "blah") (slot-value *request* 'hunchentoot::headers-in)))
      (not (null (ajax-request-p))))
  t)

(deftest ajax-request-p-2
    (with-request :get nil
      (not (null (ajax-request-p))))
  nil)

;;; test session-name-string-pair
(deftest session-name-string-pair-1
    (let ((*rewrite-for-session-urls* nil))
      (declare (special *rewrite-for-session-urls*))
      (weblocks::session-name-string-pair))
  "")

(deftest session-name-string-pair-2
    (with-request :post nil
      (let ((*rewrite-for-session-urls* t))
	(declare (special *rewrite-for-session-urls*))
	(setf (slot-value *request* 'hunchentoot::cookies-in)
	      (cons `(,*session-cookie-name* . "foo") (slot-value *request* 'hunchentoot::cookies-in)))
	(weblocks::session-name-string-pair)))
  "")

(deftest session-name-string-pair-3
    (with-request :post nil
      (let ((*rewrite-for-session-urls* t))
	(declare (special *rewrite-for-session-urls*))
	(weblocks::session-name-string-pair)))
  "weblocks-session=1%3Atest")
