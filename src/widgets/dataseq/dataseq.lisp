
(in-package :weblocks)

(export '(dataseq dataseq-view dataseq-data-class dataseq-class-store
	  dataseq-on-query dataseq-allow-sorting-p dataseq-sort
	  dataseq-allow-select-p dataseq-selection
	  dataseq-allow-drilldown-p dataseq-on-drilldown
	  dataseq-drilled-down-item
	  dataseq-autoset-drilled-down-item-p
	  dataseq-allow-operations-p dataseq-item-ops
	  dataseq-common-ops dataseq-allow-pagination-p
	  dataseq-persistent-query-function
	  dataseq-pagination-widget dataseq-show-total-items-count-p
	  dataseq-flash dataseq-data dataseq-data-count
	  dataseq-render-pagination-widget
	  dataseq-render-pagination-widget-default
          dataseq-selection-empty-p
	  dataseq-clear-selection dataseq-item-selected-p
	  dataseq-select-item dataseq-update-sort-column
	  dataseq-field-sortable-p get-field-info-sort-path
	  dataseq-sort-path dataseq-sort-direction dataseq-sort-slot
	  negate-sort-direction humanize-sort-direction
	  dataseq-render-operations
          dataseq-render-operations-default
          dataseq-render-mining-bar
	  render-dataseq-body dataseq-wrap-body-in-form-p
	  render-total-items-message
          dataseq-operations-action
	  dataseq-data-form-class))

(defmethod dataseq-data-form-class (obj)
  (dataseq-data-class obj))

(defwidget dataseq ()
  (;; View information
   (view :accessor dataseq-view
	 :initform nil
	 :initarg :view
	 :documentation "A sequence view object used to render the
	 dataseq. If the value of this slot is nil, a scaffold view
	 will be used as determined by the concrete class derived from
	 dataseq.")
   ;; Data binding information
   (data-class :accessor dataseq-data-class
	       :initform nil
	       :initarg :data-class
	       :documentation "The class name of the objects rendered
	       by this dataseq. While the class can potentially be
	       obtained from the data directly, it is not always
	       possible. Additionally, specifying the class makes
	       dataseq more efficient. For these reasons it is
	       required to specify this slot at the instantiation
	       time.")
   (class-store :accessor dataseq-class-store
		:initform nil
		:initarg :class-store
		:documentation "The store responsible for maintaining
		instances of the 'data-class'. By default, the store
		is obtained by calling 'class-store' at dataseq
		initialization. Otherwise, the specified store will be
		used by the dataseq. This functionality is useful for
		'scratch' stores - non-persistant repositories of
		data.")
   (on-query :accessor dataseq-on-query
	     :initform nil
	     :initarg :on-query
	     :documentation "A function designator that accepts the
	     widget instance, as well as sorting and pagination
	     parameters. The function is expected to return a properly
	     sorted and paged sequence. The function should also
	     accept a :countp keyword argument. If true, the function
	     should return only the number of items with the given
	     parameters, not the actual items themselves. If this slot
	     is NIL (the default), the dataseq will operate on all
	     persistent classes specified in
	     'data-class'. Alternatively, 'on-query' may be a list of
	     store-dependent keywords that will be passed to the
	     datastore. This may be a SQL 'where' clause, etc.")
   ;; Sorting
   (allow-sorting-p :initform t
		    :initarg :allow-sorting-p
		    :accessor dataseq-allow-sorting-p
		    :documentation "Set to true to enable sorting, or
		    nil to disable it.")
   (sort :accessor dataseq-sort
	 :initform nil
	 :initarg :sort
	 :documentation "Holds a dotted pair of a path to the sorted
         column and the direction of the sort (:asc or :desc).")
   ;; Selecting items
   (allow-select-p :accessor dataseq-allow-select-p
		   :initform nil
		   :initarg :allow-select-p
		   :documentation "If true, dataseq provides the UI
		   to select entries in the collection.")
   (selection :accessor dataseq-selection
	      :initform (cons :none nil)
	      :initarg :selection
	      :affects-dirty-status-p nil
	      :documentation "Contains a cons cell that identifies
	      currently selected elements. That 'car' of the cell is
	      either :all or :none. In case of :all, all items except
	      the ones in the cdr list are selected. In case of :none,
	      none of the elements except the ones in the cdr list are
	      selected.")
   ;; Drilling down on items
   (allow-drilldown-p :accessor dataseq-allow-drilldown-p
		      :initform t
		      :initarg :allow-drilldown-p
		      :documentation "If set to true and
		      'on-drilldown' isn't nil, dataseq provides the
		      UI to drill down into items.")
   (on-drilldown :accessor dataseq-on-drilldown
		 :initform nil
		 :initarg :on-drilldown
		 :documentation "A cons cell that represents a
	         drilldown operation. The car of the cell should
	         contain symbol representing the name of the
	         operation (i.e. 'details', 'edit', etc.) and the cdr
	         of the cell should contain a function of two
	         arguments (widget obj and item object). If this slot
	         isn't set to nil and 'allow-drilldown-p' is true,
	         dataseq provides the UI to drill down into items.")
   (drilled-down-item :accessor dataseq-drilled-down-item
		      :initform nil
		      :initarg :drilled-down-item
		      :documentation "If 'allow-drilldown-p' is set to
		      true and the user drills down on an item this
		      slot can be set to the item in question. This
		      slot is set to the drilled down item
		      automatically only when
		      'autoset-drilled-down-item-p' is true. Note, it
		      is always the responsibility of the dataseq's
		      client to reset this slot back to nil.")
   (autoset-drilled-down-item-p :accessor dataseq-autoset-drilled-down-item-p
				:initform nil
				:initarg :autoset-drilled-down-item-p
				:documentation "If set to true,
				'drilled-down-item' will be
				automatically set to the appropriate
				item when the user drills down on the
				widget.")
   ;; Operations
   (allow-operations-p :accessor dataseq-allow-operations-p
		       :initform t
		       :initarg :allow-operations-p
		       :documentation "If true, the widget deriving
		       from dataseq provides a UI to access operations
		       defined in 'item-ops' and 'common-ops'.")
   (item-ops :accessor dataseq-item-ops
	     :initform nil
	     :initarg :item-ops
	     :documentation "A list of cons cells that define
	     operations on items. A car of each cell should contain a
	     name of the operation, while the cdr of each cell should
	     contain a function that accepts two arguments - the widget
	     object, and the selection cons cell passed to the
	     function for convenience. Any given function is called by
	     the dataseq when the appropriate operation has been
	     invoked by the user.")
   (common-ops :accessor dataseq-common-ops
	       :initform nil
	       :initarg :common-ops
	       :documentation "Similar to 'item-ops', except
	       operations aren't performed on a particular item, but
	       on the widget as a whole (e.g. adding an item, deleting
	       all items, etc). The destinction is made between
	       common-ops and item-ops because different widgets may
	       want to provide different UI for the two.")
   ;; Dataset pagination
   (allow-pagination-p :accessor dataseq-allow-pagination-p
		       :initform t
		       :initarg :allow-pagination-p
		       :documentation "Set to true to enable
		       pagination (default) or nil to disable it.")
   (pagination-widget :accessor dataseq-pagination-widget
		      :initform nil
		      :initarg :pagination-widget
		      :documentation "An instance of a pagination
		      widget used by the dataseq to manage data
		      pagination.")
   ;; Mining bar configuration
   (show-total-items-count-p :accessor dataseq-show-total-items-count-p
			     :initform t
			     :initarg :show-total-items-count-p
			     :documentation "If set to true (the
			     default), the dataseq displays a message
			     specifying how many items are visible.")
   ;; Information flash widget
   (flash :accessor dataseq-flash
	  :initform (make-instance 'flash)
	  :initarg :flash
	  :documentation "A flash widget provided by the dataseq to
	  display relevant information to the user (errors, item
	  modification information, etc.)")
   ;; Post rendering information
   (rendered-data-sequence :reader dataseq-rendered-data-sequence
                           :initform nil
                           :type sequence
                           :documentation "A sequence of items currently rendered."))
  (:documentation "A base class for widgets that are designed to
  present a sequence of data items. This class provides functionality
  for data binding; sorting, selecting, and drilling down on items;
  performing operations on items; pagination of the dataset; and
  displaying information back to the user."))

;;; Initialization
(defmethod initialize-instance :after ((obj dataseq) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; Ensure data-class is specified
  (when (null (dataseq-data-class obj))
    (error "data-class must be specified to initialize a dataseq-based widget."))
  ;; Ensure class-store is specified
  (setf (dataseq-class-store obj)
	(or (dataseq-class-store obj)
	    (class-store (dataseq-data-class obj))))
  ;; Ensure pagination widget is instantiated
  (unless (dataseq-pagination-widget obj)
    (setf (dataseq-pagination-widget obj)
	  (make-instance 'pagination
			 :on-change (lambda (&rest args)
				      (declare (ignore args))
				      (dataseq-clear-selection obj)
				      (mark-dirty obj))
			 :on-error (dataseq-flash obj)
			 :show-total-items-p nil
			 :total-items (dataseq-data-count obj)))))

;;; Default default scaffold view if it's not provided by the user
(defmethod dataseq-view ((obj dataseq))
  (or (slot-value obj 'view)
      (find-view
       (list 'sequence
	     (if (symbolp (dataseq-data-class obj))
		 (dataseq-data-class obj)
		 (class-name (dataseq-data-class obj)))))))

;;; Dataset management
(defun dataseq-data (obj)
  "Returns the items in the sequence. If 'dataseq-on-query' is a
function designator, calls the function designated by
'dataseq-on-query'. Otherwise, uses persistent store API."
  (multiple-value-bind (begin end)
      (when (dataseq-allow-pagination-p obj)
	(pagination-page-item-range (dataseq-pagination-widget obj)))
    (if (function-designator-p (dataseq-on-query obj))
	(funcall (dataseq-on-query obj)
		 obj (dataseq-sort obj)
		 (when (and begin end) (cons begin end)))
	(apply #'find-persistent-objects
	       (dataseq-class-store obj)
	       (dataseq-data-class obj)
	       :order-by (dataseq-sort obj)
	       :range (when (and begin end) (cons begin end))
	       (dataseq-on-query obj)))))

(defun dataseq-data-count (obj)
  "Returns the number of items in the sequence."
  (if (function-designator-p (dataseq-on-query obj))
      (funcall (dataseq-on-query obj)
	       obj nil nil :countp t)
      (apply #'count-persistent-objects
	     (dataseq-class-store obj)
	     (dataseq-data-class obj)
	     (dataseq-on-query obj))))

(defun dataseq-persistent-query-function (on-query-list)
  "Returns a function suitable for slot `on-query' on `dataseq' that
will call the persistent store API with the given on-query-list."
  (lambda (widget sorting pagination &key countp)
    (if countp
	(apply #'count-persistent-objects
	       (dataseq-class-store widget)
	       (dataseq-data-class widget)
	       on-query-list)
	(apply #'find-persistent-objects
	       (dataseq-class-store widget)
	       (dataseq-data-class widget)
	       :order-by sorting
	       :range pagination
	       on-query-list))))

;;; Pagination management
(defgeneric dataseq-render-pagination-widget (obj &rest args)
  (:documentation
   "This function is responsible for rendering the pagination widget
for the dataseq.")
  (:method ((obj dataseq) &rest args)
    (apply #'dataseq-render-pagination-widget-default obj args)))


(defgeneric dataseq-render-pagination-widget-default (obj &rest args)
  (:documentation
   "This function renders the default the pagination widget for the dataseq.")
  (:method ((obj dataseq) &rest args)
    (declare (ignore args))
    (render-widget (dataseq-pagination-widget obj))))

(defmethod dependencies append ((obj dataseq))
  (when (dataseq-allow-pagination-p obj)
    (list (make-local-dependency :stylesheet "pagination"))))

;;; Selection
(defun dataseq-selection-empty-p (selection-or-dataseq)
  "Returns true if no items are selected, nil otherwise.
'selection-or-dataseq' should either be a dataseq widget or its
selection slot (both are accepted for convinience)."
  (etypecase selection-or-dataseq
    (dataseq (dataseq-selection-empty-p (dataseq-selection selection-or-dataseq)))
    (cons (let ((state (car selection-or-dataseq))
		(items (cdr selection-or-dataseq)))
	    (ecase state
	      ;(:all ???)
	      (:none (if (null items)
			 t
			 nil)))))))

(defun dataseq-clear-selection (obj)
  "Clears selected items."
  (setf (dataseq-selection obj) (cons :none nil)))

(defun dataseq-item-selected-p (obj item-id)
  "Checks if an item in the dataseq is marked as selected."
  (let ((state (car (dataseq-selection obj)))
	(items (cdr (dataseq-selection obj))))
    (ecase state
      ;(:all (not (member item-id items :test #'string-equal)))
      (:none (member (princ-to-string item-id) items :test #'string-equal :key #'princ-to-string)))))

(defun dataseq-select-item (obj item-id)
  "Marks an item in the dataseq as selected."
  (let ((state (car (dataseq-selection obj))))
    (ecase state
      ;; (:all (setf (cdr (dataseq-selection obj))
      ;;       (remove item-id (cdr (dataseq-selection obj)))))
      (:none (setf (cdr (dataseq-selection obj))
		   (pushnew item-id (cdr (dataseq-selection obj)))))))
  (mark-dirty obj))

;;; Sorting
(defun dataseq-update-sort-column (obj)
  "This function is called to ensure that a dataseq is sorted on
sortable column (if one is available). If the dataseq is not sorted,
this function finds the first sortable column and sorts the dataseq
on that column."
  (loop
     for field-info in (get-object-view-fields nil (dataseq-view obj))
     while (null (dataseq-sort obj))
     do (when (dataseq-field-sortable-p obj (field-info-field field-info))
	  (setf (dataseq-sort obj) (cons (get-field-info-sort-path field-info) :asc)))))

(defun dataseq-field-sortable-p (obj field)
  "Determines if a field is sortable."
  (and (dataseq-allow-sorting-p obj)
       (view-field-sorting-mixin-allow-sorting-p field)))

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
		      (ensure-list (view-field-sorting-mixin-order-by field))
		      (ensure-list (view-field-slot-name field)))))
    (if (second result)
	result
	(car result))))

(defun dataseq-sort-path (obj)
  "The current sort path in a given dataseq."
  (when obj
    (car (dataseq-sort obj))))

(defun dataseq-sort-direction (obj)
  "The current sort direction in a given dataseq."
  (when obj
    (cdr (dataseq-sort obj))))

(defun dataseq-sort-slot (obj)
  "The current sort slot in a given dataseq."
  (last-element
   (ensure-list
    (dataseq-sort-path obj))))

(defun negate-sort-direction (dir)
  "Returns a negated direction of a sort (returns :asc
for :desc and vica versa)."
  (ecase dir
    (:asc :desc)
    (:desc :asc)))

(defun humanize-sort-direction (dir)
  "Converts a sort direction to a human readable name."
  (humanize-name
   (ecase dir
     (:asc "ascending")
     (:desc "descending"))))

;;; Item operations for CMUCL (CMUCL doesn't "compile
;;; operations-action.lisp" properly). Bytecompiler, however, works.
#+cmu (load (merge-pathnames
	     (make-pathname :directory '(:relative "src" "widgets" "dataseq")
			    :name "operations-action" :type "lisp")
	     (asdf-system-directory :weblocks)))

;;; Rendering API
(defgeneric dataseq-render-operations (obj &rest args
					   &key &allow-other-keys)
  (:documentation
   "This function is responsible for rendering the operations for the
widget deriving from dataseq. Specialize this function to provide
custom operations rendering. Default implementation is implemented by
'dataseq-render-operations-default' function.

Note, this function should take special care to respect
dataseq-wrap-body-in-form-p because its return value determines
whether the item ops should render their own form or they're already
wrapped by a form of the widget. ")
  (:method ((obj dataseq) &rest args)
    (apply #'dataseq-render-operations-default obj args)))


(defgeneric dataseq-render-operations-default (obj &rest args
                                                    &key &allow-other-keys)
  (:documentation
   "The default implementation of operation rendering.  It renders item-ops
and common-ops as buttons. Note, it renders item operations if
render-item-ops-p argument is set to true (default), and common operations
if render-common-ops-p is set to true (default).")
  (:method ((obj dataseq) &rest args)
    (declare (ignore args))
    (flet ((render-operations ()
	     (with-html
	       (:div :class "operations"
		     (mapc (lambda (op)
			     (render-button (car op)))
			   (append
			    (dataseq-common-ops obj)
			    (when (dataseq-allow-select-p obj)
			      (dataseq-item-ops obj))))))))
      (if (dataseq-wrap-body-in-form-p obj)
	  (render-operations)
	  (with-html-form (:get (make-action (curry #'dataseq-operations-action obj))
				:class "operations-form")
	    (render-operations))))))

(defgeneric dataseq-render-mining-bar (obj &rest args)
  (:documentation
   "This function renders the data mining bar for widgets deriving
from dataseq. The data mining bar might include controls for selection
information on total items available, etc. This function is meant to
be implemented by deriving widgets."))

(defgeneric render-dataseq-body (obj &rest args)
  (:documentation
   "Renders the actual data of the dataseq without any controls before
or after. Widgets deriving from dataseq should specialize this
function to render their elements."))

(defgeneric dataseq-wrap-body-in-form-p (obj)
  (:documentation "Determines whether 'render-widget-body- wraps the
body in a form. This behavior is occassionally useful (in case of the
datagrid), and occassionally harmful (in case of the datalist). It is
useful when selection is allowed and selection controls must be in the
same form as the item ops. It is harmful when each item of the
sequence might be its own form (nested forms are problematic).

Default implementation of this method returns true. Specialize this
method to return false, if necessary. Note, this is done via a generic
function instead of a slot because it's an implementation specific
design choice. It cannot be changed at runtime and must not be visible
to the user of the widget.")
  (:method ((obj dataseq))
    t))

;;; Template for rendering the body of the data sequence.
(defmethod render-widget-body ((obj dataseq) &rest args &key
			       pre-data-mining-fn post-data-mining-fn)
  ;; Do necessary bookkeeping
  (dataseq-update-sort-column obj)
  (when (dataseq-allow-pagination-p obj)
    (setf (pagination-total-items (dataseq-pagination-widget obj))
	  (dataseq-data-count obj)))
  ;; Render Data mining
  (safe-funcall pre-data-mining-fn obj)
  (when (and (>= (dataseq-data-count obj) 1)
	     (or (dataseq-allow-select-p obj)
		 (dataseq-show-total-items-count-p obj)))
    (apply #'dataseq-render-mining-bar obj args))
  (safe-funcall post-data-mining-fn obj)
  ;; Render flash
  (render-widget (dataseq-flash obj))
  ;; Render Body
  (flet ((render-body ()
	   ;; Render items
	   (apply #'render-dataseq-body obj args)
	   ;; Render item ops
	   (when (and (dataseq-allow-operations-p obj)
		      (or (dataseq-item-ops obj)
			  (dataseq-common-ops obj)))
	     (apply #'dataseq-render-operations obj args))))
    (if (dataseq-wrap-body-in-form-p obj)
	(with-html-form (:get (make-action (curry #'dataseq-operations-action obj))
			      :class "dataseq-form")
	  (render-body))
	(render-body)))
  ;; Render Pagination
  (when (dataseq-allow-pagination-p obj)
    (dataseq-render-pagination-widget obj)))

(defun render-total-items-message (obj)
  "Renders the total items message."
  (with-html
    (:span :class "total-items"
	   (str (let ((total-items-count (dataseq-data-count obj)))
		  (format nil "(Total of ~A ~A)"
			  total-items-count
			  (proper-number-form total-items-count
					      (humanize-name
					       (dataseq-data-class obj)))))))))

