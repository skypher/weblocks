
(in-package :weblocks)

(export '(datagrid datagrid-data datagrid-data-count datagrid-sort
	  datagrid-allow-sorting datagrid-forbid-sorting-on
	  datagrid-search datagrid-allow-searching-p
	  datagrid-show-hidden-entries-count-p
	  render-datagrid-header-cell datagrid-sorted-slot-name))

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
	   :documentation "Contains a search string that will filter
	   results. Initially set to nil, which means all results will
	   be visible.")
   (allow-searching-p :accessor datagrid-allow-searching-p
		      :initform t
		      :initarg :allow-searching
		      :documentation "Set to true to enable searching,
		      or to nil to disable it.")
   (show-hidden-entries-count-p :accessor datagrid-show-hidden-entries-count-p
				:initform t
				:initarg :show-hidden-entries-count-p
				:documentation "If set to true (the
				default), and searching is allowed,
				the datagrid displays a message during
				search specifying how many items have
				been hidden."))
  (:documentation "Represents a sortable, pagable table. This
  widget is inspired by ASP.NET's datagrid control."))

;;; Ensure that data-class is specified
(defmethod initialize-instance :after ((obj datagrid) &rest initargs &key &allow-other-keys)
  (when (null (datagrid-data-class obj))
    (error "data-class must be specified to initialize a datagrid.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering and searching ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun datagrid-filter-data (grid-obj data)
  "Returns filtered datagrid data. Applies 'object-satisfies-search-p'
repeatedly to each item in the sequence."
  (with-slots (search) grid-obj
    (if search
	(remove nil
		(mapcar (lambda (item)
			  (when (object-satisfies-search-p (make-isearch-regex search) item)
			    item))
			data))
	data)))

(defun object-satisfies-search-p (search-regex object &rest args)
  "Determines if an object satisfies a search regex. Applies regex to
string representations of each slot value, and if one matches returns
true."
  (some (compose #'not #'null)
	(flatten
	 (apply #'visit-object-slots object
		(lambda (obj slot-name slot-value &rest keys &key slot-path &allow-other-keys)
		  (if (typep slot-value 'standard-object)
		      (if (render-slot-inline-p obj slot-name)
			  (object-satisfies-search-p search-regex slot-value :slot-path slot-path)
			  (ppcre:scan search-regex (format nil "~A" (object-name slot-value))))
		      (ppcre:scan search-regex (format nil "~A" slot-value))))
		:call-around-fn-p nil
		args))))

(defun make-isearch-regex (search)
  "Create a regular expression from the user's input that tries to be
faithful to Emacs' isearch."
  (if (some #'upper-case-p search)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode nil)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode t)))

(defun hidden-items-message (grid)
  "Returns a text message specifying how many items are hidden by the
search."
  (let ((hidden-items-count (- (datagrid-data-count grid :totalp t)
			       (datagrid-data-count grid :totalp nil))))
    (if (> hidden-items-count 0)
	(format nil "(<span class=\"item-count\">~A</span> ~A ~A hidden by the search)"
		hidden-items-count
		(proper-number-form hidden-items-count "item")
		(proper-number-form hidden-items-count "is"))
	"&nbsp;")))

(defun datagrid-render-search-bar (grid &rest keys)
  "Renders a search bar for the datagrid."
  (with-html
    (:div :class "datagrid-search-bar"
	  (with-extra-tags
	    (htm (:span :class "title" "Search table"))
	    (apply #'render-isearch "search"
		   (make-action
		    (lambda (&key search &allow-other-keys)
		      (declare (special *on-ajax-complete-scripts*))
		      (setf (datagrid-search grid) (when (not (empty-p search))
						     search))
		      (when (datagrid-show-hidden-entries-count-p grid)
			(push (format nil "function () { $('~A').~
                                           getElementsByClassName('hidden-items')[0].~
                                             innerHTML = '~A';~
                                         }"
				      (attributize-name (widget-name grid))
				      (hidden-items-message grid))
			      *on-ajax-complete-scripts*))))
		   :value (datagrid-search grid)
		   keys)
	    (when (datagrid-show-hidden-entries-count-p grid)
	      (let ((hidden-items-message (hidden-items-message grid)))
		(when hidden-items-message
		  (with-html
		    (:span :class "hidden-items"
			   (str hidden-items-message))))))))))

;;; Render the searchbar in the header
(defmethod with-widget-header ((obj datagrid) body-fn &rest args)
  (apply #'call-next-method
	 obj body-fn
	 :prewidget-body-fn (when (datagrid-allow-searching-p obj)
			      (curry #'datagrid-render-search-bar obj))
	 args))

;;;;;;;;;;;;;;;
;;; Sorting ;;;
;;;;;;;;;;;;;;;
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
	 (lambda (obj slot-name slot-value &rest keys &key slot-path &allow-other-keys)
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
   (compute-applicable-methods #'strictly-less-p (list column-value column-value))
   (compute-applicable-methods #'equivalentp (list column-value column-value))))

(defun datagrid-sorted-slot-name (sort-info)
  "Returns the name of the current slot from the slot path."
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
	   (:span (render-link (make-action (lambda (&rest args)
					      (when (equalp slot slot-name)
						(setf new-dir (negate-sort-direction dir)))
					      (setf (datagrid-sort grid-obj) (cons slot-path new-dir))))
			       (humanize-name human-name)))))))

;;; If the 'data' slot is a sequence, simply returns it. If it is a
;;; function, calls it with appropriate parameters. Signals an error
;;; otherwise.
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

;;; Renders the body of the data grid.
(defmethod render-widget-body ((obj datagrid) &rest args)
  (apply #'datagrid-update-sort-column obj args)
  (apply #'render-table (datagrid-data obj)
	 :grid-obj obj
	 :summary (if (datagrid-sort obj)
		      (format nil "Ordered by ~A, ~A."
			      (string-downcase (humanize-name
						(datagrid-sorted-slot-name (datagrid-sort obj))))
			      (string-downcase (humanize-name
						(cdr (datagrid-sort obj)))))
		      nil)
	 :highlight (when (datagrid-search obj)
		      (make-isearch-regex (datagrid-search obj)))
	 args))

