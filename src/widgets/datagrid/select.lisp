
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

(defun datagrid-render-select-body-cell (grid obj slot-name slot-type slot-value &rest args)
  "Renders a cell with a checkbox used to select items."
  (let ((checkbox-name (concatenate 'string
				    "item-" (attributize-name (object-id obj))))
	(drilldownp (and (datagrid-allow-drilldown-p grid)
			 (datagrid-on-drilldown grid))))
    (with-html
      (:td :class "select"
	   :onclick (when drilldownp "stopPropagation(event);")
	   :style (when drilldownp "cursor: default;")
	   (:div
	    (render-checkbox checkbox-name
			     (datagrid-item-selected-p grid (object-id obj))
			     :class nil))))))

(defslotmethod render-table-header-cell (obj (slot-name (eql 'select)) slot-type slot-value &rest keys
					     &key grid-obj &allow-other-keys)
  (with-html (:th :class "select" "")))
