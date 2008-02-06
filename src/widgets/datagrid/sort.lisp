
(in-package :weblocks)

(export '(datagrid-sort-path datagrid-sort-direction
	  datagrid-sort-slot))

;;; Default sort column
(defun datagrid-update-sort-column (grid)
  "This function is called to ensure that a datagrid is sorted on
sortable column (if one is available). If the datagrid is not sorted,
this function finds the first sortable column and sorts the datagrid
on that column."
  (loop
     for field-info in (get-object-view-fields nil (datagrid-view grid))
     while (null (datagrid-sort grid))
     do (when (datagrid-field-sortable-p grid (field-info-field field-info))
	  (setf (datagrid-sort grid) (cons (get-field-info-sort-path field-info) :asc)))))

;;; Utils
(defun datagrid-field-sortable-p (grid field)
  "Determines if a column is sortable."
  (if (slot-boundp field 'allow-sorting-p)
      (grid-view-field-allow-sorting-p field)
      (grid-view-allow-sorting-p (find-view (datagrid-view grid)))))

(defun get-field-info-sort-path (field-info)
  "Given a field info, returns a sort path that can be passed to
backend stores. The return value may be a symbol or a list of multiple
symbols."
  (let ((parent-info (field-info-parent-info field-info))
	(field (field-info-field field-info))
	result)
    (setf result
	  (append (when parent-info
		    (ensure-list
		     (get-field-info-sort-path parent-info)))
		  (if (slot-boundp field 'order-by)
		      (ensure-list (grid-view-field-order-by field))
		      (ensure-list (view-field-slot-name field)))))
    (if (second result)
	result
	(car result))))

;;; Dealing with sort structure
(defun datagrid-sort-path (grid)
  "The current sort path in a given datagrid."
  (when grid
    (car (datagrid-sort grid))))

(defun datagrid-sort-direction (grid)
  "The current sort direction in a given datagrid."
  (when grid
    (cdr (datagrid-sort grid))))

(defun datagrid-sort-slot (grid)
  "The current sort slot in a given datagrid."
  (last-element
   (ensure-list
    (datagrid-sort-path grid))))

(defun negate-sort-direction (dir)
  "Returns a negated direction of a sort (returns :asc
for :desc and vica versa)."
  (ecase dir
    (:asc :desc)
    (:desc :asc)))

;;; Support sorting by clicking on headers
(defmethod render-view-field-header ((field grid-view-field) (view grid-view)
				     widget presentation value obj 
				     &rest args &key field-info
				     &allow-other-keys)
  (declare (ignore args))
  (if (datagrid-field-sortable-p widget field)
      (let* ((slot-name (view-field-slot-name field))
	     (slot-path (get-field-info-sort-path field-info))
	     (th-class (when (equalp slot-path (datagrid-sort-path widget))
			 (concatenate 'string " sort-"
				      (attributize-name (string (cdr (datagrid-sort widget)))))))
	     (sort-dir (datagrid-sort-direction widget)))
	(with-html
	  (:th :class (concatenate 'string (attributize-name slot-name) th-class)
	       (:span (render-link
		       (make-action
			(lambda (&rest args)
			  (declare (ignore args))
			  (let ((new-dir :asc))
			    (when (equalp (datagrid-sort-path widget) slot-path)
			      (setf new-dir (negate-sort-direction sort-dir)))
			    (setf (datagrid-sort widget) (cons slot-path new-dir)))
			  ;; we also need to clear the selection
			  (datagrid-clear-selection widget)))
		       (view-field-label field))))))
      (call-next-method)))

