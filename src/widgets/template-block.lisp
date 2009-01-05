(in-package :weblocks)

(export '(template-block template-block-source template-block-vars recreate-template-printer))

(defwidget template-block ()
  ((template-printer :accessor template-printer-of :initform nil
		     :affects-dirty-status-p nil)
   (source :accessor template-block-source :initarg :source :initform nil)
   (vars :type list :accessor template-block-vars :initarg :vars :initform nil))
  (:documentation "A block of HTML taken from 'source', which is processed by
HTML-TEMPLATE using 'vars'."))

(defmethod recreate-template-printer ((obj template-block))
  (if (template-block-source obj)
      (progn
	(setf (template-printer-of obj)
	      (html-template:create-template-printer (template-block-source obj)))
	t)
      nil))

(defmethod ensure-printer-exists ((obj template-block))
  (if (template-printer-of obj)
      t
      (recreate-template-printer obj)))

(defmethod render-template ((obj template-block))
  (declare (ignore args))
  (when (ensure-printer-exists obj)
    (html-template:fill-and-print-template (template-printer-of obj)
					   (template-block-vars obj)
					   :stream *weblocks-output-stream*)))

