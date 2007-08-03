
(in-package :weblocks-test)

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
