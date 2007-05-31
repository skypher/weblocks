
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
   (renderedp :accessor flash-rendered-p
	      :initform nil
	      :documentation "True if the newly set message has been
	      rendered."))
  (:documentation "A widget that allows displaying a message that
  disappears on the following request. It is useful for one time
  messages (welcome, etc.)"))

;;; Add a callback function that resets the flash widget after
;;; rendering. On ajax calls the BlindUp effect is added.
(defmethod initialize-instance :after ((obj flash) &rest initargs)
  (push (lambda ()
	  (declare (special *on-ajax-complete-scripts* *on-post-request-onetime*))
	  (when (and (flash-messages obj)
		     (flash-rendered-p obj)
		     (not (refresh-request-p)))
	    (if (ajax-request-p)
		(progn
		  (push (format nil "function () { new Effect.BlindUp('~A'); }"
				(attributize-name (widget-name obj)))
			*on-ajax-complete-scripts*)
		  (push (lambda ()
			  (setf (flash-messages obj) nil)
			  (setf (flash-rendered-p obj) nil))
			*on-post-request-onetime*))
		(progn
		  (setf (flash-messages obj) nil)
		  (setf (flash-rendered-p obj) nil)))))
	(on-session-pre-request)))

(defun flash-message (flash msg)
  "Add a 'msg' to a list of messages to show on this request in the
'flash' object."
  (check-type flash flash)
  (push-end msg (flash-messages flash)))

(defmethod render-widget-body ((obj flash) &rest args)
  (when (flash-messages obj)
    (with-html
      (:div :class "renderer"
	    (with-extra-tags
	      (htm
	       (:ul :class "messages"
		    (mapc (lambda (msg)
			    (htm (:li (apply #'render-widget msg args))))
			  (flash-messages obj)))))))
    (setf (flash-rendered-p obj) t)))
