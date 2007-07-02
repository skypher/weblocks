
(in-package :weblocks-test)

(defun evaluate-flash-hooks ()
  "A utility function that evaluates hooks set up by the flash
widget."
  (declare (special *request-hook*))
  (funcall (car (request-hook :session :pre-action)))
  (funcall (car (request-hook :session :post-action)))
  (funcall (car (request-hook :session :post-render))))

;;; test render-widget-body for flash
(deftest-html render-widget-body-flash-1
    (with-request :get nil
      (let ((w (make-instance 'flash)))
	(flash-message w "Hello World!")
	(flash-message w "Foo")
	(render-widget-body w)))
  (htm
   (:div :class "renderer"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "Hello World!")))
	      (:li
	       (:div :class "widget string"
		     (:p "Foo"))))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;"))))

(deftest render-widget-body-flash-2
    (with-request :get nil
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (w (make-instance 'flash :messages '("Hello World!" "Something Else"))))
	(declare (special *weblocks-output-stream*))
	(render-widget-body w)
	(values (weblocks::flash-messages w)
		(weblocks::flash-old-messages w)
		(+ (length (request-hook :session :pre-action))
		   (length (request-hook :session :post-action))
		   (length (request-hook :session :pre-render))
		   (length (request-hook :session :post-render))))))
  ("Hello World!" "Something Else")
  ("Hello World!" "Something Else")
  3)

(deftest render-widget-body-flash-3
    (with-request :get `((,weblocks::*action-string* . "abc123"))
      (make-action (lambda () nil))
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (w (make-instance 'flash)))
	(declare (special *weblocks-output-stream*))
	(flash-message w "Test")
	(flash-message w "Test2")
	(render-widget-body w)
	(evaluate-flash-hooks)
	(flash-messages w)))
  nil)

(deftest render-widget-body-flash-4
    (with-request :get `((,weblocks::*action-string* . "abc123"))
      (make-action (lambda () nil))
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (w (make-instance 'flash))
	    *on-ajax-complete-scripts* *on-post-request-onetime*)
	(declare (special *weblocks-output-stream*
			  *on-ajax-complete-scripts*
			  *on-post-request-onetime*))
	(make-request-ajax)
	(flash-message w "Test")
	(render-widget-body w)
	(evaluate-flash-hooks)
	*on-ajax-complete-scripts*))
  ("new Function(\"new Effect.BlindUp('widget-123');\")"))

;; After refresh, state shouldn't be reset
(deftest render-widget-body-flash-5
    (with-request :get nil
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (*uri-tokens* nil) ; refresh
	    (w (make-instance 'flash)))
	(declare (special *weblocks-output-stream* *uri-tokens*))
	(flash-message w "Test")
	(flash-message w "Test2")
	(render-widget-body w)
	(evaluate-flash-hooks)
	(flash-messages w)))
  ("Test" "Test2"))

(deftest flash-message-1
    (with-request :post nil
      (let ((w (make-instance 'flash)))
	(flash-message w "test")
	(flash-message w "bar")
	(flash-messages w)))
  ("test" "bar"))
