
(in-package :weblocks)

(export '(datagrid datagrid-data datagrid-sort datagrid-allow-sorting
	  render-datagrid-header-cell datagrid-sorted-slot-name))

(defwidget datagrid (widget)
  ((data :accessor datagrid-data
	 :initform nil
	 :initarg :data
	 :documentation "Either a sequence of data objects that will
	 be rendered and modified by this widget, or a function that
	 accepts sorting and paging parameters. If this slot is bound
	 to a sequence, datagrid will do the paging and sorting itself
	 in memory destructively. If the slot is bound to a function,
	 the function is expected to return a properly sorted and
	 paged sequence.")
   (sort :accessor datagrid-sort
	 :initform nil
	 :initarg :sort
	 :documentation "Holds a dotted pair of a path to the sorted
         column and the direction of the sort (:ascending
         or :descending).")
   (allow-sorting :accessor datagrid-allow-sorting
		  :initform t
		  :initarg :allow-sorting
		  :documentation "This slot controls whether the
		  datagrid object should support sorting. If set to
		  t (default), sorting is allowed; if set to nil
		  sorting is disallowed. If set to a list of slot
		  names, only these slots will be available for the
		  user to sort on. Note, if generic functions
		  'strictly-less-p' and 'equivalentp' aren't defined
		  on the datatype of the column, sorting for that
		  column will be turned off regardless of the value of
		  this slot."))
  (:documentation "Represents a sortable, pagable table. This
  widget is inspired by ASP.NET's datagrid control."))

;;; Hooks into table renderer's header rendering mechanism and
;;; forwards the calls to 'render-datagrid-header-cell' whenever
;;; appropriate to support sorting via clicking on headers
(defmethod render-table-header-cell :around (obj slot-name slot-value &rest keys
						 &key grid-obj &allow-other-keys)
  (if (or (null grid-obj)
	  (typep slot-value 'standard-object)
	  (not (datagrid-column-sortable-p grid-obj slot-name slot-value)))
      (apply #'call-next-method obj slot-name slot-value keys)
      (apply #'render-datagrid-header-cell obj slot-name slot-value keys)))

(defgeneric render-datagrid-header-cell (obj slot-name slot-value &rest keys
					&key human-name slot-path grid-obj &allow-other-keys)
  (:documentation
   "Renders table headers for the datagrid. The default implementation
renders a link that it associates with a sorting action which modifies
'sort' slot of the datagrid object. Specialize this function to
customize the way datagrid headers are rendered."))

(defmethod render-datagrid-header-cell (obj slot-name slot-value &rest keys
					&key (human-name slot-name) slot-path grid-obj &allow-other-keys)
  (let ((th-class (when (equalp slot-name (datagrid-sorted-slot-name (datagrid-sort grid-obj)))
		    (concatenate 'string " sort-"
				 (attributize-name (string (cdr (datagrid-sort grid-obj)))))))
	slot dir (new-dir :ascending))
    (unless (null (datagrid-sort grid-obj))
      (setf slot (datagrid-sorted-slot-name (datagrid-sort grid-obj)))
      (setf dir (cdr (datagrid-sort grid-obj))))
    (with-html
      (:th :class (concatenate 'string (attributize-name slot-name) th-class)
	   (render-link (make-action (lambda ()
				       (when (equalp slot slot-name)
					 (setf new-dir (negate-sort-direction dir)))
				       (setf (datagrid-sort grid-obj) (cons slot-path new-dir))))
			(humanize-name human-name))))))

(defun datagrid-update-sort-column (grid data-obj &rest args)
  "This function is called to ensure that a datagrid is sorted on
sortable column (if one is available). If the datagrid is not sorted,
this function finds the first sortable column and sorts the datagrid
on that column. If this isn't done the datagrid UI is confusing when
it's sorted on nothing."
  (apply #'visit-object-slots
	 data-obj
	 (lambda (obj slot-name slot-value &rest keys &key slot-path &allow-other-keys)
	   (if (typep slot-value 'standard-object)
	       (datagrid-update-sort-column grid slot-value :slot-path slot-path)
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
   (compute-applicable-methods #'strictly-less-p (list column-value column-value))
   (compute-applicable-methods #'equivalentp (list column-value column-value))))

(defun datagrid-sorted-slot-name (sort-info)
  "Returns the name of the current slot from the slot path."
  (car (last (ensure-list (if (dotted-pair-p sort-info)
			      (car sort-info)
			      sort-info)))))

(defun datagrid-sort-data (grid-obj)
  "Destructively sorts the 'data' slot of the datagrid if and only if
it is a sequence of values. If the 'data' slot is a function,
'datagrid-sort-data' does nothing."
  (with-slots (data sort) grid-obj
    (when (and (typep data 'sequence) sort)
      (setf data (stable-sort data (if (equalp (cdr sort) :ascending)
				       #'strictly-less-p
				       (lambda (a b)
					 (and (not (strictly-less-p a b))
					      (not (equivalentp a b)))))
			      :key (curry-after #'slot-value-by-path (car sort)))))))

;;; If the 'data' slot is a sequence, simply returns it. If it is a
;;; function, calls it with appropriate parameters. Signals an error
;;; otherwise.
(defmethod datagrid-data ((grid-obj datagrid))
  (with-slots (data) grid-obj
    (etypecase data
      (function (funcall data (datagrid-sort grid-obj)))
      (sequence data))))

(defun negate-sort-direction (dir)
  "Returns a negated direction of a sort (returns :ascending
for :descending and vica versa)."
  (ecase dir
    (:ascending :descending)
    (:descending :ascending)))

;;; Renders the body of the data grid.
(defmethod render-widget-body ((obj datagrid) &rest args)
  (apply #'datagrid-update-sort-column obj (car (datagrid-data obj)) args)
  (datagrid-sort-data obj)
  (apply #'render-table (datagrid-data obj)
	 :grid-obj obj
	 :summary (if (datagrid-sort obj)
		      (format nil "Ordered by ~A, ~A."
			      (string-downcase (humanize-name
						(datagrid-sorted-slot-name (datagrid-sort obj))))
			      (string-downcase (humanize-name
						(cdr (datagrid-sort obj)))))
		      nil)
	 args))

