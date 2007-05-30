
(in-package :weblocks-test)

;;; test render-widget-body for flash
(deftest-html render-widget-body-flash-1
    (with-request :get nil
      (render-widget-body (make-instance 'flash :message "Hello World!")))
  (htm
   (:div :class "renderer"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:div :class "message"
	       (:div :class "widget simple-character-string"
		     (:p "Hello World!")))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;"))))

(deftest render-widget-body-flash-2
    (with-request :get nil
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (w (make-instance 'flash :message "Hello World!")))
	(declare (special *weblocks-output-stream*))
	(render-widget-body w)
	(values (weblocks::flash-rendered-p w)
		(length (on-session-pre-request)))))
  t 1)

(deftest render-widget-body-flash-3
    (with-request :get nil
      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (w (make-instance 'flash)))
	(declare (special *weblocks-output-stream*))
	(flash-message w "Test")
	(render-widget-body w)
	(funcall (car (on-session-pre-request)))
	(values (weblocks::flash-rendered-p w)
		(flash-message* w))))
  nil nil)

(deftest render-widget-body-flash-4
    (with-request :get nil
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

(deftest flash-message-1
    (with-request :post nil
      (let ((w (make-instance 'flash)))
	(flash-message w "test")
	(flash-message* w)))
  "test")
