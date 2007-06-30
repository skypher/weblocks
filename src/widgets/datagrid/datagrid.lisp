
(in-package :weblocks)

(export '(datagrid datagrid-data-class datagrid-data datagrid-sort
	  datagrid-allow-sorting datagrid-forbid-sorting-on
	  datagrid-search datagrid-allow-searching-p
	  datagrid-show-hidden-entries-count-p datagrid-selection
	  datagrid-allow-select-p datagrid-item-ops
	  datagrid-allow-item-ops-p datagrid-data
	  datagrid-data-count))

(defwidget datagrid (widget)
  ((data-class :accessor datagrid-data-class
	       :initform nil
	       :initarg :data-class
	       :documentation "The class of the objects rendered by
	       this datagrid. While the class can potentially be
	       obtained from the data directly, it is not always
	       possible. Additionally, specifying the class makes
	       datagrid more efficient. For these reasons it is
	       required to specify this slot at the instantiation
	       time.")
   (data :accessor datagrid-data
	 :initform nil
	 :initarg :data
	 :documentation "Either a sequence of data objects that will
	 be rendered and modified by this widget, or a function that
	 accepts searching, sorting, and paging parameters. If this
	 slot is bound to a sequence, datagrid will do the paging and
	 sorting itself in memory destructively. If the slot is bound
	 to a function, the function is expected to return a properly
	 sorted and paged sequence. The function should also accept
	 a :countp keyword argument. If true, the function should
	 return only the number of items with the given search
	 parameters, not the actual items themselves.")
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
		  user to sort on.")
   (forbid-sorting-on :accessor datagrid-forbid-sorting-on
		      :initform nil
		      :initarg :forbid-sorting-on
		      :documentation "A list of slot names on which
		      datagrid will not do sorting. Note, this slot
		      takes precedence over 'allow-sorting'.")
   (search :accessor datagrid-search
	   :initform nil
	   :initarg :search
	   :affects-dirty-status-p nil
	   :documentation "Contains a search string that will filter
	   results. Initially set to nil, which means all results will
	   be visible.")
   (allow-searching-p :accessor datagrid-allow-searching-p
		      :initform t
		      :initarg :allow-searching-p
		      :documentation "Set to true to enable searching,
		      or to nil to disable it.")
   (show-hidden-entries-count-p :accessor datagrid-show-hidden-entries-count-p
				:initform t
				:initarg :show-hidden-entries-count-p
				:documentation "If set to true (the
				default), and searching is allowed,
				the datagrid displays a message during
				search specifying how many items have
				been hidden.")
   (selection :accessor datagrid-selection
	      :initform (cons :none nil)
	      :initarg :selection
	      :affects-dirty-status-p nil
	      :documentation "Contains a cons cell that identifies
	      currently selected elements. That 'car' of the cell is
	      either :all or :none. In case of :all, all items except
	      the ones in the cdr list are selected. In case of :none,
	      none of the elements except the ones in the cdr list are
	      selected.")
   (allow-select-p :accessor datagrid-allow-select-p
		   :initform nil
		   :initarg :allow-select-p
		   :documentation "If true, datagrid provides the UI
		   to select entries in the collection.")
   (item-ops :accessor datagrid-item-ops
	     :initform nil
	     :initarg :item-ops
	     :documentation "A list of cons cells that define
	     operations on items. A car of each cell should contain a
	     name of the operation, while the cdr of each cell should
	     contain a function that accepts two arguments - the grid
	     object, and the selection cons cell passed to the
	     function for convenience. Any given function is called by
	     the datagrid when the appropriate operation has been
	     invoked by the user.")
   (allow-item-ops-p :accessor datagrid-allow-item-ops-p
		     :initform t
		     :initarg :allow-item-ops-p
		     :documentation "If true, datagrid provides UI to
		     access operations defined in 'item-ops'."))
  (:documentation "Represents a sortable, pagable table. This
  widget is inspired by ASP.NET's datagrid control."))

;;; Ensure that data-class is specified and that we cannot sort on the
;;; "select" slot
(defmethod initialize-instance :after ((obj datagrid) &rest initargs &key &allow-other-keys)
  (pushnew 'select (datagrid-forbid-sorting-on obj))
  (when (null (datagrid-data-class obj))
    (error "data-class must be specified to initialize a datagrid.")))

;;; If the 'data' slot is a sequence, filters it, sorts the filtered
;;; data, and returns the result. If it is a function, calls it with
;;; appropriate parameters. Signals an error otherwise.
(defmethod datagrid-data ((grid-obj datagrid))
  (with-slots (data) grid-obj
    (etypecase data
      (function (funcall data (datagrid-search grid-obj) (datagrid-sort grid-obj)))
      (sequence 
       (datagrid-sort-data grid-obj
			   (datagrid-filter-data grid-obj data))))))

(defun datagrid-data-count (grid &key totalp)
  "Returns the number of items in the grid. This function works
similarly to datagrid-data in principle. Note, if totalp is set to
true, 'datagrid-data-count' returns the the total number of items and
ignores searching parameters."
  (with-slots (data) grid
    (etypecase data
      (function (funcall data
			 (when (not totalp)
			   (datagrid-search grid))
			 nil
			 :countp t))
      (sequence 
       (if totalp
	   (length data)
	   (length (datagrid-filter-data grid data)))))))

(defun append-custom-slots (custom-slots args)
  "Appends 'custom-slots' to whatever custom slots that are already
defined in 'args'."
  (append (cadr (member :custom-slots args))
	  custom-slots))

(defun render-item-ops-bar (grid &rest args)
  "Renders the operations define in 'item-ops' slot if
'allow-item-ops-p' is true."
  (with-html
    (:div :class "item-operations"
	  (mapc (lambda (op)
		  (with-html
		    (:input :name (attributize-name (car op))
			    :type "submit"
			    :class "submit"
			    :value (humanize-name (car op))
			    :onclick "disableIrrelevantButtons(this);")))
		(datagrid-item-ops grid)))))

;;; Renders the body of the data grid.
(defmethod render-widget-body ((obj datagrid) &rest args
			       &key pre-data-mining-fn post-data-mining-fn)
  (safe-funcall pre-data-mining-fn obj)
  (when (>= (datagrid-data-count obj :totalp t) 1)
    (with-html
      (:div :class "data-mining-bar"
	    (when (datagrid-allow-searching-p obj)
	      (apply #'datagrid-render-search-bar obj args))
	    (when (datagrid-allow-select-p obj)
	      (apply #'render-select-bar obj args)))))
  (safe-funcall post-data-mining-fn obj)
  (let ((action (make-action (lambda (&rest args)
			       (datagrid-clear-selection obj)
			       (loop for i in (request-parameters)
				    when (string-starts-with (car i) "item-")
				    do (datagrid-select-item obj (substring (car i) 5)))
			       (loop for i in (datagrid-item-ops obj)
				    when (member (car i) (request-parameters)
						 :key #'car
						 :test #'string-equal)
				    do (funcall (cdr i) obj (datagrid-selection obj)))
			       (datagrid-clear-selection obj)))))
    (with-html
      (:form :action "" :method "get"
	     :onsubmit (format nil "initiateFormAction(\"~A\", $(this), \"~A\"); ~
                                    return false;"
			       action
			       (session-name-string-pair))
	     :class "datagrid-form"
	     (:fieldset
	      (apply #'render-datagrid-table-body obj args)
	      (when (and (datagrid-allow-item-ops-p obj)
			 (datagrid-item-ops obj))
		(apply #'render-item-ops-bar obj args))
	      (:input :name "action" :type "hidden" :value action))))))

(defun render-datagrid-table-body (grid &rest args)
  "Renders the actual data of the datagrid without any controls before
or after. This function is used to update the body during ajax search
requests."
  (apply #'datagrid-update-sort-column grid args)
  (with-html
    (:div :class "datagrid-body"
	  (apply #'render-table (datagrid-data grid)
		 :grid-obj grid
		 :summary (if (datagrid-sort grid)
			      (format nil "Ordered by ~A, ~A."
				      (string-downcase (humanize-name
							(datagrid-sorted-slot-name
							 (datagrid-sort grid))))
				      (string-downcase (humanize-name
							(cdr (datagrid-sort grid)))))
			      nil)
		 :highlight (when (datagrid-search grid)
			      (make-isearch-regex (datagrid-search grid)))
		 :custom-slots (append-custom-slots
				(when (datagrid-allow-select-p grid)
				  `((0 . (select
					  . ,(curry #'datagrid-render-select-body-cell grid)))))
				args)
		 args))))

