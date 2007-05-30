
(in-package :weblocks)

(export '(flash flash-message* flash-message))

(defclass flash (widget)
  ((message :accessor flash-message*
	    :initform nil
	    :initarg :message
	    :documentation "Message to be rendered the next time the
	    widget is to be presented. This can be any renderable
	    object (a widget, a function, a string, etc.)")
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
	  (when (and (flash-message* obj)
		     (flash-rendered-p obj))
	    (if (ajax-request-p)
		(progn
		  (push (format nil "function () { new Effect.BlindUp('~A'); }"
				(attributize-name (widget-name obj)))
			*on-ajax-complete-scripts*)
		  (push (lambda ()
			  (setf (flash-message* obj) nil)
			  (setf (flash-rendered-p obj) nil))
			*on-post-request-onetime*))
		(progn
		  (setf (flash-message* obj) nil)
		  (setf (flash-rendered-p obj) nil)))))
	(on-session-pre-request)))

(defun flash-message (flash msg)
  "Show a message on this request in the 'flash' object."
  (check-type flash flash)
  (setf (flash-message* flash) msg))

(defmethod render-widget-body ((obj flash) &rest args)
  (when (flash-message* obj)
    (with-html
      (:div :class "renderer"
	    (with-extra-tags
	      (htm (:div :class "message"
			 (apply #'render-widget (flash-message* obj) args))))))
    (setf (flash-rendered-p obj) t)))
