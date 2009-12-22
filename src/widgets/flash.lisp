
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
  (declare (ignore initargs))
  (push (lambda ()
	  (when (and (flash-messages obj)
		     (not (refresh-request-p))
		     (not (initial-request-p)))
	    (setf (flash-old-messages obj) (flash-messages obj))
	    (setf (flash-messages obj) nil)))
	(request-hook :session :pre-action))
  (push (lambda ()
	  (declare (special *on-ajax-complete-scripts*))
	  (when (and (ajax-request-p)
		     (flash-old-messages obj))
	    (if (flash-messages obj)
		(send-script
		  (ps* `(new ((slot-value *effect '*pulsate) ,(dom-id obj)
                                                             (create :pulses 3 :duration 0.5)))))
		(send-script
                  (ps* `(new ((slot-value *effect '*blind-up) ,(dom-id obj))))))))
	(request-hook :session :post-action))
  (push (lambda ()
	  (declare (special *on-ajax-complete-scripts*))
	  (when (and (null (flash-messages obj))
		     (flash-old-messages obj))
	    (setf (flash-old-messages obj) nil)))
	(request-hook :session :post-render)))

(defun flash-messages-to-show (flash)
  "Returns a list of messages that need to be shown or nil if there is
nothing to show. This functions takes into consideration any stale
messages that need to be shown for AJAX effects."
  (or (flash-messages flash)
      (and (ajax-request-p)
	   (flash-old-messages flash))))

;;; Specialize with-widget-header to display a comment if there are no
;;; messages. We need this fix for IE6.
(defmethod with-widget-header ((obj flash) body-fn &rest args)
  (if (flash-messages-to-show obj)
      (call-next-method)
      (apply #'call-next-method
	     obj body-fn
	     :widget-prefix-fn (lambda (&rest args)
				 (declare (ignore args))
				 (format *weblocks-output-stream* "<!-- empty flash -->"))
	     args)))

(defun flash-message (flash msg)
  "Add a 'msg' to a list of messages to show on this request in the
'flash' object."
  (check-type flash flash)
  (push-end (make-widget msg) (flash-messages flash)))

(defmethod render-widget-body ((obj flash) &rest args)
  (declare (special *on-ajax-complete-scripts* *dirty-widgets*))
  (let ((messages (flash-messages-to-show obj)))
    (when messages
      (with-html
	(:div :class "view"
	      (with-extra-tags
		(htm
		 (:ul :class "messages"
		      (mapc (lambda (msg)
			      (htm (:li (apply #'render-widget msg args))))
			    messages))))))
      (send-script (ps* `((@ ($ ,(dom-id obj)) show)))))))

