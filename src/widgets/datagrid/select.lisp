
(in-package :weblocks)

(export '(datagrid-item-selected-p datagrid-select-item
	  datagrid-clear-selection datagrid-selection-empty-p))

(defun render-select-bar (grid &rest keys)
  "Renders commands relevant to item selection (select all, none,
etc.)"
  (with-html
    (:p :class "datagrid-select-bar"
	(:strong "Select: ")
	(render-link (make-action (lambda (&rest args)
				    (setf (datagrid-selection grid)
					  (cons :none (mapcar #'object-id (datagrid-data grid))))
				    (mark-dirty grid)))
		     "All")
	", "
	(render-link (make-action (lambda (&rest args)
				    (datagrid-clear-selection grid)
				    (mark-dirty grid)))
		     "None"))))

(defun datagrid-item-selected-p (grid item-id)
  "Checks if an item in the datagrid is marked as selected."
  (let ((state (car (datagrid-selection grid)))
	(items (cdr (datagrid-selection grid))))
    (ecase state
      ;(:all (not (member item-id items :test #'string-equal)))
      (:none (member (princ-to-string item-id) items :test #'string-equal :key #'princ-to-string)))))

(defun datagrid-select-item (grid item-id)
  "Marks an item in the datagrid as selected."
  (let ((state (car (datagrid-selection grid))))
    (ecase state
;;       (:all (setf (cdr (datagrid-selection grid))
;; 		  (remove item-id (cdr (datagrid-selection grid)))))
      (:none (setf (cdr (datagrid-selection grid))
		   (pushnew item-id (cdr (datagrid-selection grid)))))))
  (mark-dirty grid))

(defun datagrid-clear-selection (grid)
  "Clears selected items."
  (setf (datagrid-selection grid) (cons :none nil)))

(defun datagrid-selection-empty-p (selection-or-grid)
  "Returns true if no items are selected, nil otherwise.
'selection-or-grid' should either be a datagrid widget or its
selection slot (both are accepted for convinience)."
  (etypecase selection-or-grid
    (datagrid (datagrid-selection-empty-p (datagrid-selection selection-or-grid)))
    (cons (let ((state (car selection-or-grid))
		(items (cdr selection-or-grid)))
	    (ecase state
	      ;(:all ???)
	      (:none (if (null items)
			 t
			 nil)))))))

;; Custom selection field
(defclass datagrid-select-field (grid-view-field)
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
				     widget presentation value obj 
				     &rest args)
  (declare (ignore args widget))
  (with-html (:th :class "select" "")))

(defmethod render-view-field ((field datagrid-select-field) (view table-view)
			      widget presentation value obj &rest args)
  (declare (ignore args))
  (let ((checkbox-name (concatenate 'string
				    "item-" (attributize-name (object-id obj))))
	(drilldownp (and (datagrid-allow-drilldown-p widget)
			 (datagrid-on-drilldown widget))))
    (with-html
      (:td :class "select"
	   :onclick (when drilldownp "stopPropagation(event);")
	   :style (when drilldownp "cursor: default;")
	   (:div
	    (render-checkbox checkbox-name
			     (datagrid-item-selected-p widget (object-id obj))
			     :class nil))))))

