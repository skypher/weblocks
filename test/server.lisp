
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
      (setf (slot-value *request* 'headers-in) '(("X-Requested-With" . "test")))
      (ajax-request-p))
  "test")
