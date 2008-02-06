
(in-package :weblocks)

;;; Scaffolding magic
(defmethod typespec->view-field-presentation ((scaffold form-scaffold)
					      (typespec (eql 'member)) args)
  (values t (make-instance 'radio-presentation
			   :choices (if (every (lambda (arg)
						 (or (symbolp arg)
						     (stringp arg))) args)
					args
					(mapcar (curry #'format nil "~A")
						args)))))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'member)) args)
  (cond
    ((every #'keywordp args) (values t (make-instance 'keyword-parser)))
    ((every #'symbolp args) (values t (make-instance 'symbol-parser)))
    (t nil)))

