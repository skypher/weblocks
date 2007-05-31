
(in-package :weblocks-test)

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
	       (:div :class "widget simple-character-string"
		     (:p "Hello World!")))
	      (:li
	       (:div :class "widget simple-character-string"
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
	(values (weblocks::flash-rendered-p w)
		(length (on-session-pre-request)))))
  t 1)

(deftest render-widget-body-flash-3
    (with-request :get `((,weblocks::*action-string* . "abc123"))
      (make-action (lambda () nil))
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (w (make-instance 'flash)))
	(declare (special *weblocks-output-stream*))
	(flash-message w "Test")
	(flash-message w "Test2")
	(render-widget-body w)
	(funcall (car (on-session-pre-request)))
	(values (weblocks::flash-rendered-p w)
		(flash-messages w))))
  nil nil)

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
	(funcall (car (on-session-pre-request)))
	(funcall (car *on-post-request-onetime*))
	(values *on-ajax-complete-scripts*
		(weblocks::flash-rendered-p w))))
  ("function () { new Effect.BlindUp('widget-123'); }")
  nil)

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
	(funcall (car (on-session-pre-request)))
	(weblocks::flash-rendered-p w)))
  t)

(deftest flash-message-1
    (with-request :post nil
      (let ((w (make-instance 'flash)))
	(flash-message w "test")
	(flash-message w "bar")
	(flash-messages w)))
  ("test" "bar"))
