
(in-package :weblocks)

(export '(render-datagrid-header-cell datagrid-sorted-slot-name))

(defun datagrid-update-sort-column (grid &rest args)
  "An interface for 'datagrid-update-sort-column-aux'. Gets the data
and tries to update the sort column only when necessary."
  (when (null (datagrid-sort grid))
    (apply #'datagrid-update-sort-column-aux
	   grid
	   (make-instance (datagrid-data-class grid))
	   args)))

(defun datagrid-update-sort-column-aux (grid data-obj &rest args)
  "This function is called to ensure that a datagrid is sorted on
sortable column (if one is available). If the datagrid is not sorted,
this function finds the first sortable column and sorts the datagrid
on that column. If this isn't done the datagrid UI is confusing when
it's sorted on nothing."
  (apply #'visit-object-slots
	 data-obj
	 (lambda (obj slot-name slot-type slot-value &rest keys &key slot-path &allow-other-keys)
	   (if (typep slot-value 'standard-object)
	       (if (render-slot-inline-p obj slot-name)
		   (datagrid-update-sort-column-aux grid slot-value :slot-path slot-path)
		   (when (and (null (datagrid-sort grid))
			      (datagrid-column-sortable-p grid slot-path (object-name slot-value)))
		     (setf (datagrid-sort grid) (cons slot-path :ascending))))
	       (when (and (null (datagrid-sort grid))
			  (datagrid-column-sortable-p grid slot-path slot-value))
		 (setf (datagrid-sort grid) (cons slot-path :ascending)))))
	 :call-around-fn-p nil
	 args))

(defun datagrid-column-sortable-p (grid-obj column-path column-value)
  "Determines if a column is sortable based on the 'allow-sorting'
slot and availability of 'strictly-less-p' and 'equivalentp' generic
functions."
  (and
   (datagrid-allow-sorting grid-obj)
   (if (listp (datagrid-allow-sorting grid-obj))
       (member (datagrid-sorted-slot-name column-path)
	       (datagrid-allow-sorting grid-obj)
	       :test #'equalp)
       t)
   (not (member (datagrid-sorted-slot-name column-path)
		(datagrid-forbid-sorting-on grid-obj)
		:test #'equalp))
   ; Currently column-value is always nil so computing applicable
   ; methods is unnecessary. It also seems to interfere with
   ; profiling.
   #|(compute-applicable-methods #'strictly-less-p (list column-value column-value))
   (compute-applicable-methods #'equivalentp (list column-value column-value))|#))

(defun datagrid-sorted-slot-name (sort-info)
  "Returns the name of the sorted slot.
'sort-info' - normally the value returned by 'datagrid-sort'."
  (car (last (ensure-list (if (dotted-pair-p sort-info)
			      (car sort-info)
			      sort-info)))))

(defun datagrid-sort-data (grid-obj data)
  "Sorts the data of the datagrid."
  (with-slots (sort) grid-obj
    (if sort
	(stable-sort (copy-list data)
		     (if (equalp (cdr sort) :ascending)
			 #'strictly-less-p
			 (lambda (a b)
			   (and (not (strictly-less-p a b))
				(not (equivalentp a b)))))
		     :key (curry-after #'slot-value-by-path (car sort) :observe-inline-p t))
	data)))

(defun negate-sort-direction (dir)
  "Returns a negated direction of a sort (returns :ascending
for :descending and vica versa)."
  (ecase dir
    (:ascending :descending)
    (:descending :ascending)))

;;; Hooks into table renderer's header rendering mechanism and
;;; forwards the calls to 'render-datagrid-header-cell' whenever
;;; appropriate to support sorting via clicking on
;;; headers. Additionally, specialize header rendering for 'select'
;;; slot to allow for selecting all/none slot selection
(defslotmethod render-table-header-cell :around (obj slot-name slot-type slot-value &rest keys
						     &key grid-obj &allow-other-keys)
  (if (or (null grid-obj)
	  (typep slot-value 'standard-object)
	  (not (datagrid-column-sortable-p grid-obj slot-name slot-value)))
      (apply #'call-next-method obj slot-name slot-type slot-value keys)
      (apply #'render-datagrid-header-cell obj slot-name slot-type slot-value keys)))

(defgeneric render-datagrid-header-cell (obj slot-name slot-type slot-value &rest keys
					&key human-name slot-path grid-obj &allow-other-keys)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Renders table headers for the datagrid. The default implementation
renders a link that it associates with a sorting action which modifies
'sort' slot of the datagrid object. Specialize this function to
customize the way datagrid headers are rendered."))

(defslotmethod render-datagrid-header-cell (obj slot-name slot-type
						slot-value &rest keys
						&key (human-name
						slot-name) slot-path
						grid-obj
						&allow-other-keys)
  (let ((th-class (when (equalp slot-name (datagrid-sorted-slot-name (datagrid-sort grid-obj)))
		    (concatenate 'string " sort-"
				 (attributize-name (string (cdr (datagrid-sort grid-obj)))))))
	slot dir (new-dir :ascending))
    (unless (null (datagrid-sort grid-obj))
      (setf slot (datagrid-sorted-slot-name (datagrid-sort grid-obj)))
      (setf dir (cdr (datagrid-sort grid-obj))))
    (with-html
      (:th :class (concatenate 'string (attributize-name slot-name) th-class)
	   (:span (render-link (make-action (lambda (&rest args)
					      (when (equalp slot slot-name)
						(setf new-dir (negate-sort-direction dir)))
					      (setf (datagrid-sort grid-obj) (cons slot-path new-dir))
					      ;; we also need to clear the selection
					      (datagrid-clear-selection grid-obj)))
			       (humanize-name human-name)))))))

