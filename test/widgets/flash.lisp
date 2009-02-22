
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
   (:div :class "view"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "Hello World!")))
	      (:li
	       (:div :class "widget string"
		     (:p "Foo"))))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->"))
         (str (with-javascript-to-string "$('id-123').show();"))
         ))

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

(defjstest render-widget-body-flash-4
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
    ("new Effect.BlindUp('id-123');"
     "$('id-123').show();"))

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

;; test flash-messages-to-show
(deftest flash-messages-to-show-1
    (with-request :get nil
      (let ((w (make-instance 'flash)))
	(flash-message w "Hello World!")
	(flash-message w "Foo")
	(weblocks::flash-messages-to-show w)))
  ("Hello World!" "Foo"))

(deftest flash-messages-to-show-2
    (with-request :get nil
      (let ((w (make-instance 'flash)))
	(flash-message w "Hello World!")
	(flash-message w "Foo")
	(evaluate-flash-hooks)
	(weblocks::flash-messages-to-show w)))
  nil)

(deftest flash-messages-to-show-3
    (with-request :get nil
      (let ((w (make-instance 'flash)))
	(flash-message w "Hello World!")
	(flash-message w "Foo")
	(funcall (car (request-hook :session :pre-action)))
	(funcall (car (request-hook :session :post-action)))
	(make-request-ajax)
	(weblocks::flash-messages-to-show w)))
  ("Hello World!" "Foo"))

;; test with-widget-header for flash
(deftest-html with-widget-header-empty-flash-1
    (with-request :get nil
      (let ((w (make-instance 'flash)))
	(with-widget-header w (lambda (&rest args) nil))))
  (:div :class "widget flash" :id "id-123" "<!-- empty flash -->"))

;; test flash-message
(deftest flash-message-1
    (with-request :post nil
      (let ((w (make-instance 'flash)))
	(flash-message w "test")
	(flash-message w "bar")
	(flash-messages w)))
  ("test" "bar"))
