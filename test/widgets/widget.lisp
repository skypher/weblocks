
(in-package :weblocks-test)

;;; test render-widget-body
(deftest-html render-widget-body-1
    (render-widget-body (lambda (&rest args)
			  (with-html (:p "blah"))))
  (:p "blah"))

(deftest-html render-widget-body-2
    (render-widget-body "test")
  (:p "test"))

(deftest-html render-widget-body-3
    (render-widget-body "test" :id "foo" :class "bar")
  (:p :id "foo" :class "bar" "test"))

;;; test widget-css-classes
(deftest widget-css-classes-1
    (widget-css-classes #'identity)
  "widget function")

(deftest widget-css-classes-2
    (widget-css-classes "test")
  "widget string")

(deftest widget-css-classes-3
    (widget-css-classes (make-instance 'gridedit))
  "widget datagrid gridedit")

;;; test with-widget-header
(deftest-html with-widget-header-1
    (with-request :get nil
      (with-widget-header (make-instance 'dataform :data *joe*)
	(lambda (obj &rest args)
	  (with-html (:p "test")))
	:prewidget-body-fn (lambda (&rest args) (with-html (:p "hello")))
	:postwidget-body-fn (lambda (&rest args) (with-html (:p "world")))))
  (:div :class "widget dataform" :id "widget-123"
	(:p "hello")
	(:div :class "widget-body"
	      (:p "test"))
	(:p "world")))

;;; test widget-name specialization for functions
(deftest widget-name-1
    (widget-name #'identity)
  nil)

;;; render function as a widget
(deftest-html render-function-1
    (render-widget (lambda (&rest args)
		     (with-html (:p "blah"))))
  (:div :class "widget function"
	(:div :class "widget-body"
	      (:p "blah"))))

;;; render some other widget without a name
(deftest-html render-widget-1
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe*)))
  (:div :class "widget dataform" :id "widget-123"
	(:div :class "widget-body"
	      #.(data-header-template
		 "abc123"
		 '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
		   (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
		    (:span :class "value" "Jim")))))))

;;; render some widget with a name
(deftest-html render-widget-2
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe* :name "Test Widget")))
  (:div :class "widget dataform" :id "test-widget"
	(:div :class "widget-body"
	      #.(data-header-template
		 "abc123"
		 '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
		   (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
		    (:span :class "value" "Jim")))))))

(deftest-html render-widget-3
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe*) :inlinep t))
  #.(data-header-template "abc123"
     '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))

;;; test make-dirty
(deftest make-dirty-1
    (multiple-value-bind (res errors)
	(ignore-errors (make-dirty (lambda () nil)))
      (values res (null errors)))
  nil nil)

(deftest make-dirty-2
    (let ((weblocks::*dirty-widgets* nil))
      (declare (special weblocks::*dirty-widgets*))
      (make-dirty (make-instance 'composite :name "test"))
      (widget-name (car weblocks::*dirty-widgets*)))
  "test")

(deftest make-dirty-3
    (with-request :get nil
      (setf (session-value 'weblocks::root-composite) (create-site-layout))	
      (let ((weblocks::*dirty-widgets* nil))
	(declare (special weblocks::*dirty-widgets*))
	(make-dirty (make-instance 'composite :name "test"
				   :propagate-dirty '((root-inner test-nav-1 test2 test2-leaf)))
		    :putp t)
	(mapcar #'widget-name weblocks::*dirty-widgets*)))
  (test2-leaf "test"))

;;; test that (setf slot-value-using-class) method is modified for
;;; widgets to automatically mark them as dirty
(deftest setf-slot-value-using-class-1
    (with-request :get nil
      (let ((weblocks::*dirty-widgets* nil)
	    (w (make-instance 'dataform)))
	(declare (special weblocks::*dirty-widgets*))
	(setf (slot-value w 'weblocks::ui-state) :form)
	(widget-name (car weblocks::*dirty-widgets*))))
  "widget-123")

;;; test find-widget-by-path
(deftest find-widget-by-path-1
    (find-widget-by-path '(hello) nil)
  nil)

(deftest find-widget-by-path-2
    (find-widget-by-path nil 'hello)
  hello)

(deftest find-widget-by-path-3
    (with-request :get nil
      (setf (session-value 'weblocks::root-composite) (create-site-layout))
      (let ((res (find-widget-by-path '(root-inner test-nav-1 test2 test2-leaf))))
	(values (widget-name res)
		(type-of res))))
  test2-leaf composite)

(deftest find-widget-by-path-4
    (with-request :get nil
      (setf (session-value 'weblocks::root-composite) (create-site-layout))
      (find-widget-by-path '(doesnt exist)))
  nil)
