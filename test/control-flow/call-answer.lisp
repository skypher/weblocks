
(in-package :weblocks-test)

(deftestsuite control-flow/call-answer-suite (weblocks-suite) nil)

;;; test widget-continuation
(deftest widget-continuation-1
    (let ((foo (lambda () nil)))
      (eq foo (widget-continuation foo)))
  t)

(deftest widget-continuation-2
    (setf (widget-continuation 10) 'foo)
  foo)

(deftest widget-continuation-3
    (let ((sym (gensym)))
      (setf (widget-continuation sym) 10)
      (widget-continuation sym))
  10)

;;; test call/answer
(deftest call/answer-1
    (let (widget (res 0))
      (with-call/cc
	(when (= 10 (weblocks::call (lambda (k)
				      (incf res)
				      (answer k 10))
				    (lambda (callee)
				      (setf widget callee))))
	  (incf res)))
      (funcall widget)
      res)
  2)

(deftest call/answer-2
    (with-request :get nil
      (let ((w1 (make-instance 'composite))
	    w2)
	(with-call/cc
	  (weblocks::call w1
			  (lambda (callee)
			    (setf w2 callee))))
	(values
	 (eq w1 w2)
	 (not (null (widget-continuation w1))))))
  t t)

;;; test do-widget
(deftest do-widget-1
    (with-request :get nil
      (let* ((w1 (make-instance 'composite))
	     (w2 (make-instance 'composite))
	     (c (make-instance 'composite :widgets w1)))
	(with-call/cc
	  (do-widget w1 w2))
	(values (eq (car (composite-widgets c)) w2)
		(progn (answer w2)
		       (eq (car (composite-widgets c)) w1)))))
  t t)

(deftest do-widget-2
    (with-request :get nil
      (let* ((w1 (make-instance 'composite))
	     (c (make-instance 'composite :widgets w1)))
	(with-call/cc
	  (do-widget w1 "test"))
	(equalp (car (composite-widgets c)) "test")))
  t)

(deftest do-widget-3
    (with-request :get nil
      (let* ((w1 (make-instance 'composite))
	     (c (make-instance 'composite :widgets w1))
	     (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	(with-call/cc
	  (do-widget w1 (lambda (k)
			  (answer k))))
	(render-widget c)
	(eq (car (composite-widgets c)) w1)))
  t)

(deftest do-widget-4
    (with-request :get nil
      (let* ((w1 (make-instance 'widget))
	     (w2 (make-instance 'widget))
	     (w3 (make-instance 'widget))
	     (c (make-instance 'widget :children (list w1 w2))))
	(setf (root-widget) c)
	(with-call/cc
	  (do-widget nil w3))
	(list (equalp (widget-children c) (list w3))
              (progn (answer w3)
                     (equalp (widget-children c) (list w1 w2))))))
  (t t))

(deftest do-widget-5
    (with-request :get nil
      (let* ((w1 (make-instance 'composite))
	     (w2 (make-instance 'composite))
	     (c (make-instance 'composite :widgets w1)))
	(with-call/cc
	  (do-widget w1 w2 (lambda (new-callee)
			     (if (eq new-callee w2)
				 "0"
				 "1"))))
	(values (equal (car (composite-widgets c)) "0")
		(progn (answer w2)
		       (eq (car (composite-widgets c)) w1)))))
  t t)

(deftest do-widget-6
    (with-request :get nil
      (let* (weblocks::*dirty-widgets*
	     (w1 (make-instance 'composite))
	     (w2 (make-instance 'composite))
	     (w3 (make-instance 'composite))
	     (c (make-instance 'composite :widgets (list w1 w2 w3)))
	     (w4 (make-instance 'composite)))
	(declare (special weblocks::*dirty-widgets*))
	(with-call/cc
	  (do-widget w2 w4))
	(values (equalp (composite-widgets c) (list w1 w4 w3))
		(progn (answer w4)
		       (equalp (composite-widgets c) (list w1 w2 w3)))
		(not (null (widget-dirty-p c))))))
  t t t)

(addtest do-widget-idempotent
  (let* ((w1 (make-instance 'composite))
	 (w2 (make-instance 'composite))
	 (parent (make-instance 'composite
				:widgets (list w1))))
    (setf (weblocks::weblocks-webapp-debug (weblocks::current-webapp)) t)
    (do-widget w1 w2)
    (ensure-condition widget-not-in-parent
      (do-widget w1 w2))))

;;; test do-page
(deftest do-page-1
    (with-request :get nil
      (let ((w (make-instance 'composite)))
	(with-request :get nil
	  (setf (root-composite) (make-instance 'composite))
	  (with-call/cc
	    (do-page w))
	  (values (eq (car (composite-widgets
			    (root-composite)))
		      w)
		  (progn (answer w)
			 (null (composite-widgets (root-composite))))))))
  t t)

;;; test do-modal
(deftest-html do-modal-1
    (with-request :get nil
      (setf (root-composite) (make-instance 'composite))
      (with-call/cc
	(do-modal "Some Title" (lambda (&rest args)
				 (with-html "foo"))))
      (render-widget (root-composite)))
  (:div :class "widget composite" :id "id-123"
	(:div :class "widget function"
	      (:div :class "modal"
		    (:h1 (:span "Some Title"))
		    (:div (:div :class "widget function" "foo"))))))

(deftest do-modal-2
    (with-request :get nil
      (let ((w (make-instance 'composite)))
	(with-request :get nil
	  (setf (root-composite) (make-instance 'composite))
	  (with-call/cc
	    (do-modal "Some Title" w))
	  (answer w)
	  (null (composite-widgets (root-composite))))))
  t)

