(in-package :weblocks)

(export '(template-block
          template-block-source
          template-block-vars
          recreate-template-printer
	  render-template))

(defwidget template-block ()
  ((template-printer :accessor template-printer-of :initform nil
		     :affects-dirty-status-p nil)
   (source :accessor template-block-source :initarg :source :initform nil)
   (vars :type list :accessor template-block-vars :initarg :vars :initform nil))
  (:documentation "A block of HTML taken from 'source', which is processed by
HTML-TEMPLATE using 'vars'."))

(defmethod recreate-template-printer ((obj template-block))
  (when (template-block-source obj)
    (setf (template-printer-of obj)
          (html-template:create-template-printer (template-block-source obj)))))

(defmethod ensure-printer-exists ((obj template-block))
  (or (template-printer-of obj) (recreate-template-printer obj)))

(defmethod render-template ((obj template-block))
  (ensure-printer-exists obj)
  (html-template:fill-and-print-template (template-printer-of obj)
                                         (template-block-vars obj)
                                         :stream *weblocks-output-stream*))

