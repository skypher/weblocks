
(in-package :weblocks)

(export '(flash flash-messages flash-message))

(defwidget flash (widget)
  ((messages :accessor flash-messages
	     :initform nil
	     :initarg :messages
	     :documentation "A list of messages to be rendered the
 	     next time the widget is to be presented. This can be any
 	     renderable object (a widget, a function, a string,
 	     etc.)")
   (old-messages :accessor flash-old-messages
		 :initform nil
		 :initarg :messages
		 :affects-dirty-status-p nil
		 :documentation "A list of messages from the previous
		 request."))
  (:documentation "A widget that allows displaying a message that
  disappears on the following request. It is useful for one time
  messages (welcome, etc.)"))

;;; Add a callback function that resets the flash widget after
;;; rendering. On ajax calls the BlindUp effect is added.
(defmethod initialize-instance :after ((obj flash) &rest initargs)
  (push (lambda ()
	  (when (and (flash-messages obj)
		     (not (refresh-request-p)))
	    (setf (flash-old-messages obj) (flash-messages obj))
	    (setf (flash-messages obj) nil)))
	(request-hook :session :pre-action))
  (push (lambda ()
	  (declare (special *on-ajax-complete-scripts*))
	  (when (and (ajax-request-p)
		     (flash-old-messages obj))
	    (if (flash-messages obj)
		(push (format nil "function () { new Effect.Pulsate('~A', { pulses: 3, duration: 0.5 } ); }"
			      (attributize-name (widget-name obj)))
		      *on-ajax-complete-scripts*)
		(push (format nil "function () { new Effect.BlindUp('~A'); }"
			      (attributize-name (widget-name obj)))
		      *on-ajax-complete-scripts*))))
	(request-hook :session :post-action))
  (push (lambda ()
	  (declare (special *on-ajax-complete-scripts*))
	  (when (and (null (flash-messages obj))
		     (flash-old-messages obj)
		     (ajax-request-p))
	    (setf (flash-old-messages obj) nil)))
	(request-hook :session :post-render)))

(defun flash-message (flash msg)
  "Add a 'msg' to a list of messages to show on this request in the
'flash' object."
  (check-type flash flash)
  (push-end msg (flash-messages flash)))

(defmethod render-widget-body ((obj flash) &rest args)
  (declare (special *on-ajax-complete-scripts* *dirty-widgets*))
  (let ((messages (or (flash-messages obj)
		      (and (ajax-request-p)
			   (flash-old-messages obj)))))
    (when messages
      (with-html
	(:div :class "renderer"
	      (with-extra-tags
		(htm
		 (:ul :class "messages"
		      (mapc (lambda (msg)
			      (htm (:li (apply #'render-widget msg args))))
			    messages)))))))))

