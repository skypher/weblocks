
(in-package :weblocks)

(defun render-select-bar (grid &rest keys)
  "Renders commands relevant to item selection (select all, none,
etc.)"
  (declare (ignore keys))
  (with-html
    (:p :class "datagrid-select-bar"
	(:strong "Select: ")
	(render-link (make-action (lambda (&rest args)
                                    (declare (ignore args))
				    (setf (dataseq-selection grid)
					  (cons :none (mapcar #'object-id (dataseq-data grid))))
				    (mark-dirty grid)))
		     "All")
	", "
	(render-link (make-action (lambda (&rest args)
                                    (declare (ignore args))
				    (dataseq-clear-selection grid)
				    (mark-dirty grid)))
		     "None"))))

;; Custom selection field
(defclass datagrid-select-field (table-view-field)
  ((allow-sorting-p :initform nil))
  (:documentation "A field used to render select control."))

(defun make-select-field (grid-obj)
  "Makes a custom field for rendering select controls."
  (declare (ignore grid-obj))
  (make-instance 'datagrid-select-field
		 :label "Select"
		 :reader "Select"
		 :present-as nil))

(defmethod render-view-field-header ((field datagrid-select-field) (view table-view)
				     (widget datagrid) presentation value obj 
				     &rest args)
  (declare (ignore args widget))
  (with-html (:th :class "select" "")))

(defmethod render-view-field ((field datagrid-select-field) (view table-view)
			      (widget datagrid) presentation value obj &rest args)
  (declare (ignore args))
  (let ((checkbox-name (concatenate 'string
				    "item-" (attributize-name (object-id obj))))
	(drilldownp (and (dataseq-allow-drilldown-p widget)
			 (dataseq-on-drilldown widget))))
    (with-html
      (:td :class "select"
	   :onclick (when drilldownp "stopPropagation(event);")
	   :style (when drilldownp "cursor: default;")
	   (:div
	    (render-checkbox checkbox-name
			     (dataseq-item-selected-p widget (object-id obj))
			     :class nil))))))

