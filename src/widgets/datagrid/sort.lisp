
(in-package :weblocks)

;;; Support sorting by clicking on headers
(defmethod render-view-field-header ((field table-view-field) (view table-view)
				     (widget datagrid) presentation value obj 
				     &rest args &key field-info
				     &allow-other-keys)
  (declare (ignore args))
  (if (dataseq-field-sortable-p widget field)
      (let* ((slot-name (view-field-slot-name field))
	     (slot-path (get-field-info-sort-path field-info))
	     (th-class (when (equalp slot-path (dataseq-sort-path widget))
			 (concatenate 'string " sort-"
				      (attributize-name (string (dataseq-sort-direction widget))))))
	     (sort-dir (dataseq-sort-direction widget)))
	(with-html
	  (:th :class (concatenate 'string
                                   (if field-info
                                     (attributize-view-field-name field-info)
                                     (attributize-name slot-name))
                                   th-class)
	       (:span :class "label"
		      (render-link
		       (make-action
			(lambda (&rest args)
			  (declare (ignore args))
			  (let ((new-dir :asc))
			    (when (equalp (dataseq-sort-path widget) slot-path)
			      (setf new-dir (negate-sort-direction sort-dir)))
			    (setf (dataseq-sort widget) (cons slot-path new-dir)))
			  ;; we also need to clear the selection
			  (dataseq-clear-selection widget)))
		       (view-field-label field))))))
      (call-next-method)))

