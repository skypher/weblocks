
(in-package :weblocks-test)

;;; test setting composite widget to single widget on init
(deftest init-composite-single-1
    (with-request :get nil
      (let ((c (make-instance 'composite :widgets "foo")))
	(composite-widgets c)))
  ("foo"))

;;; test proper setting of widget parent on adding to composite
(deftest composite-add-widget-1
    (with-request :get nil
      ;; make sure this doesn't signal error
      (make-instance 'composite :widgets "foo")
      nil)
  nil)

(deftest composite-add-widget-2
    (with-request :get nil
      (let* ((w (make-instance 'composite))
	     (c (make-instance 'composite :widgets w)))
	(eq (widget-parent w) c)))
  t)

(deftest composite-add-widget-3
    (multiple-value-bind (res err)
	(ignore-errors
	  (let ((w (make-instance 'composite)))
	    (make-instance 'composite :widgets w)
	    (make-instance 'composite :widgets w)))
      (not (null err)))
  t)

(deftest composite-add-widget-4
    (with-request :get nil
      (let* ((w1 (make-instance 'composite))
	     (w2 (make-instance 'composite))
	     (c (make-instance 'composite :widgets w1)))
	(assert (not (null (widget-parent w1))))
	(assert (null (widget-parent w2)))
	(setf (composite-widgets c) w2)
	(assert (null (widget-parent w1)))
	(assert (not (null (widget-parent w2))))))
  nil)

;;; testing render for composite widget
(deftest-html render-composite-1
    (with-request :get nil
      (let ((comp (make-instance 'composite)))
	(push-end (make-instance 'dataform :data *joe*)
		  (composite-widgets comp))
	(push-end (make-instance 'dataform :data *home-address*)
		  (composite-widgets comp))
	(render-widget comp)))
  (htm
   (:div :class "widget composite" :id "id-123"
	 (:div :class "widget dataform" :id "id-123"
	       #.(data-header-template
		  "abc123"
		  '((:li :class "name" (:span :class "label text" "Name:&nbsp;")
		     (:span :class "value" "Joe"))
		    (:li :class "manager"
		     (:span :class "label text" "Manager:&nbsp;")
		     (:span :class "value" "Jim")))))
	 (:div :class "widget dataform" :id "id-123"
	       #.(data-header-template
		  "abc124"
		  '((:li :class "street"
		     (:span :class "label text" "Street:&nbsp;") (:span :class "value" "100 Broadway"))
		    (:li :class "city"
		     (:span :class "label text" "City:&nbsp;") (:span :class "value" "New York")))
		  :data-class-name "address")))))

