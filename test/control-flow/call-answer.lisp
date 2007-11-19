
(in-package :weblocks-test)

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
    (let ((w1 (make-instance 'composite))
	  w2)
      (with-call/cc
	(weblocks::call w1
			(lambda (callee)
			  (setf w2 callee))))
      (values
       (eq w1 w2)
       (not (null (widget-continuation w1)))))
  t t)

;;; test do-place
(deftest do-place-1
    (let ((c1 (make-instance 'composite))
	  (c2 (make-instance 'composite)))
      (with-call/cc
	(do-place (composite-widgets c1) c2))
      (values (eq (car (composite-widgets c1)) c2)
	      (progn (answer c2)
		     (null (composite-widgets c1)))))
  t t)

(deftest do-place-2
    (let ((c1 (make-instance 'composite))
	  (c2 (make-instance 'composite)))
      (with-call/cc
	(do-place (composite-widgets c1) c2 (lambda (new-callee)
					      (if (eq new-callee c2)
						  0
						  1))))
      (values (eq (car (composite-widgets c1)) 0)
	      (progn (answer c2)
		     (null (composite-widgets c1)))))
  t t)

;;; test do-page
(deftest do-page-1
    (let ((w (make-instance 'composite)))
      (with-request :get nil
	(setf (root-composite) (make-instance 'composite))
	(with-call/cc
	  (do-page w))
	(values (eq (car (composite-widgets
			  (root-composite)))
		    w)
		(progn (answer w)
		       (null (composite-widgets (root-composite)))))))
  t t)

;;; test do-modal
(deftest-html do-modal-1
    (with-request :get nil
      (setf (root-composite) (make-instance 'composite))
      (with-call/cc
	(do-modal "Some Title" (lambda (&rest args)
				 (with-html "foo"))))
      (render-widget (root-composite)))
  (:div :class "widget composite" :id "widget-123"
	(:div :class "widget function"
	      (:div :class "modal"
		    (:h1 (:span "Some Title"))
		    (:div (:div :class "widget function" "foo"))))))

(deftest do-modal-2
    (let ((w (make-instance 'composite)))
      (with-request :get nil
	(setf (root-composite) (make-instance 'composite))
	(with-call/cc
	  (do-modal "Some Title" w))
	(answer w)
	(null (composite-widgets (root-composite)))))
  t)

