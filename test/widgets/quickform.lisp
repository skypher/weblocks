
(in-package :weblocks-test)

(deftest test-quickform-1
    (with-request :get nil
      (typep 
       (make-quickform (defview nil ()
			 test))
       'quickform))
  t)

(deftest-html test-quickform-2
    (with-request :get nil
      (let ((quickform (make-quickform
			(defview nil (:type form
					    :persistp nil)
			  test)
			:data-class-name 'test-quickform-class-1
			:satisfies (lambda (&rest args)
				     (declare (ignore args))
				     (values nil (list (cons nil "testing")))))))
	(render-widget quickform)
	;; click submit
	(do-request `((,weblocks::*action-string* . "abc123")
		      ("submit" . "Submit")))
	(render-widget quickform)))
  (htm
   (:div :class "widget data-editor dataform quickform" :id "id-123"
	 #.(form-header-template
	    "abc123"
	    '((:li :class "test"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Test:&nbsp;")))
               (:input :type "text" :name "test" :maxlength "40" :id "id-123")))
	    :method "post"
	    :data-class-name "test-quickform-class-1"))
   (:div :class "widget data-editor dataform quickform" :id "id-123"
	 #.(form-header-template
	    "abc124"
	    '((:li :class "test"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Test:&nbsp;")))
               (:input :type "text" :name "test" :maxlength "40" :id "id-123")))
	    :method "post"
	    :data-class-name "test-quickform-class-1"
	    :preslots '((:div :class "validation-errors-summary"
			 (:h2 :class "error-count"
			  "There is 1 validation error:")
			 (:ul :class "non-field-validation-errors"
			  (:li "testing"))))))))

(deftest test-quickform-3
    (with-request :get nil
      (let* ((c1 (make-instance 'composite))
	     (c2 (make-instance 'composite :widgets c1))
	     (quickform (make-quickform
			 (defview nil (:type form
					     :persistp nil)
			   test)
			 :data-class-name 'test-quickform-class-1
			 :on-success (lambda (w o)
				       (declare (ignore w))
				       (slot-value o 'test))))
	     (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	(let (res)
	  (with-flow c1
	    (setf res (yield quickform)))
	  (render-widget c2)
	  ;; click submit
	  (do-request `((,weblocks::*action-string* . "abc123")
			("submit" . "Submit")
			("test" . "testing123")))
	  res)))
  "testing123")

(deftest test-quickform-4
    (with-request :get nil
      (let* ((c1 (make-instance 'composite))
	     (c2 (make-instance 'composite :widgets c1))
	     (quickform (make-quickform
			 (defview nil (:type form
					     :persistp nil)
			   test)
			 :data-class-name 'test-quickform-class-1))
	     (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	(let (res)
	  (with-flow c1
	    (setf res (yield quickform)))
	  (render-widget c2)
	  ;; click submit
	  (do-request `((,weblocks::*action-string* . "abc123")
			("submit" . "Submit")
			("test" . "testing123")))
	  (type-of res))))
  test-quickform-class-1)

