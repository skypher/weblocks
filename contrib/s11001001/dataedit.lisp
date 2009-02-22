;;; dataedit.lisp: Helpers for extending `dataedit'.

(in-package #:weblocks-s11)

(export '(dataedit-editor-initargs))

(defun dataedit-editor-initargs (dataedit &optional (item nil item?))
  "Answer initargs appropriate for passing to a `data-editor' to talk
back to DATAEDIT when appropriate.  If ITEM, this is a form for an
existing instance."
  (list :data (if item?
		  item
		  (make-instance (dataseq-data-form-class dataedit)))
	:class-store (dataseq-class-store dataedit)
	:on-success (lambda (obj)
		      (unless item?
			(safe-funcall (dataedit-on-add-item dataedit) dataedit obj))
		      (flash-message (dataseq-flash dataedit)
				     (format nil (if item? "Modified ~A." "Added ~A.")
					     (humanize-name (dataseq-data-class dataedit))))
		      (dataedit-reset-state dataedit)
		      (when item?
			(mark-dirty dataedit)))
	:on-close (lambda (obj)
		    (declare (ignore obj))
		    (dataedit-reset-state dataedit))
	:on-cancel (lambda (obj)
		     (declare (ignore obj))
		     (dataedit-reset-state dataedit)
		     (unless item?
		       (throw 'annihilate-dataform nil)))))

;;; dataedit.lisp ends here
