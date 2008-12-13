(in-package :weblocks)

(export '(template-block-mixin template-block-source template-block-vars recreate-template-printer))

(defclass template-block-mixin ()
  ((template-printer :accessor template-printer-of :initform nil)
   (source :accessor template-block-source :initarg :source :initform nil)
   (vars :type list :accessor template-block-vars :initarg :vars :initform nil))
  (:documentation "A block of HTML taken from 'source', which is processed by
HTML-TEMPLATE using 'vars'."))

(defmethod recreate-template-printer ((obj template-block-mixin))
  (if (template-block-source obj)
      (progn
	(setf (template-printer-of obj)
	      (html-template:create-template-printer (template-block-source obj)))
	t)
      nil))

(defmethod ensure-printer-exists ((obj template-block-mixin))
  (if (template-printer-of obj)
      t
      (recreate-template-printer obj)))

(defmethod render-widget-body ((obj template-block-mixin) &rest args)
  (declare (ignore args))
  (when (ensure-printer-exists obj)
    (html-template:fill-and-print-template (template-printer-of obj)
					   (template-block-vars obj)
					   :stream *weblocks-output-stream*)))

