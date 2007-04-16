
(in-package :weblocks-test)

(defparameter *edit-joe* (make-instance 'dataform :data *joe*))

;;; test dataform render
(deftest-html render-dataform-1
    (with-request :get '()
      (render *edit-joe*))
  1)

