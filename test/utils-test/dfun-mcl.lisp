
(in-package :weblocks-test)

;;; Test compute-discriminating-function on MCL We need three tests
;;; because for efficiency reasons MCL provides different functions
;;; depending on the number of arguments
(defmethod mcl-cdt-test-fn-1 (a) a)
(defmethod mcl-cdt-test-fn-2 (a b) (values a b))
(defmethod mcl-cdt-test-fn-3 (a b c) (values a b c))

(deftest test-mcl-cdt-1
    (funcall (compute-discriminating-function #'mcl-cdt-test-fn-1)
	     1)
  1)

(deftest test-mcl-cdt-2
    (funcall (compute-discriminating-function #'mcl-cdt-test-fn-2)
	     1 2)
  1 2)

(deftest test-mcl-cdt-3
    (funcall (compute-discriminating-function #'mcl-cdt-test-fn-3)
	     1 2 3)
  1 2 3)
