
(in-package :weblocks)

(export '(datalist datalist-item-data-view datalist-ordered-p
	  datalist-render-item))

(defparameter *datalist-selection-attempt-error-message* "Datalist does not support item selection."
  "This message will be presented to the programmer if there is an
attempt to set 'allow-select-p' on a datalist to true. See
documentation for 'allow-select-p' on the datalist for more details.")

(defwidget datalist (dataseq)
  ((item-data-view :initform nil
		   :initarg :item-data-view
		   :accessor datalist-item-data-view
		   :documentation "An item view used by the datalist
	           to render each data item. Note, because datalist
	           renders independent items in a list, it does not
	           use sequence view for rendering. However, sequence
	           view is used to determine sorting preferences,
	           etc. For this reason, datalist uses dataseq-view
	           that it inherits from dataseq, as well as
	           datalist-item-data-view.")
   (orderedp :initform t
	     :initarg :orderedp
	     :accessor datalist-ordered-p
	     :documentation "Determines if the datalist presents its
	     items as an ordered list (default is true).")
   (allow-select-p :initform nil
		   :documentation "Datalist does not allow selection
		   as it does not fit in the datalist UI model. This
		   slot must always be nil. An error is signalled if
		   there is an attempt to set it to true."))
  (:documentation "Represents a sortable, pagable list. This widget is
  similar to the datagrid widget, except items are presented in a list
  instead of a grid."))

;;; Initialization
(defmethod initialize-instance :after ((obj datalist) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; Ensure the user didn't attempt to set :allow-select-p to true
  (when (slot-value obj 'allow-select-p)
    (error *datalist-selection-attempt-error-message*)))

(defmethod (setf dataseq-allow-select-p) (new-value (obj datalist))
  (error *datalist-selection-attempt-error-message*))

;;; Ensure scaffold item view is selected if no view is provided
;;; explicitly by the user
(defmethod datalist-item-data-view ((obj datalist))
  (or (slot-value obj 'item-data-view)
      (find-view
       (list 'data
	     (if (symbolp (dataseq-data-class obj))
		 (dataseq-data-class obj)
		 (class-name (dataseq-data-class obj)))))))

;;; Mining bar
(defun datalist-render-sort-dropdown (obj)
  (let* ((sort-path (dataseq-sort-path obj))
	 (sort-dir (dataseq-sort-direction obj))
	 (opposite-sort-dir (negate-sort-direction sort-dir))
	 (sort-dir-class (concatenate 'string "sort-"
				      (attributize-name
				       (string sort-dir)))))
    (flet ((action (&rest args &key datalist-sort &allow-other-keys)
	     (declare (ignore args))
	     (let ((path (get-field-info-sort-path
			  (find datalist-sort (get-object-view-fields nil (dataseq-view obj))
				:key (compose #'attributize-name #'view-field-slot-name #'field-info-field)
				:test #'equalp))))
	       (setf (dataseq-sort obj)
		     (cons path
			   (if (equalp path (dataseq-sort-path obj))
			       opposite-sort-dir
			       :asc))))))
      (with-html-form (:get #'action :class "datalist-sort-bar")
	(:label
	 (:span :class "label" "Sort&nbsp;")
	 (scriptonly
	   (render-link (lambda (&rest args)
			  (declare (ignore args))
			  (setf (dataseq-sort obj)
				(cons sort-path opposite-sort-dir)))
			(format nil "<span class='direction'>~A</span>"
				(humanize-sort-direction sort-dir))
			:class sort-dir-class))
	 (noscript
	   (:span :class sort-dir-class
		  (:span :class "direction"
			 (str (humanize-sort-direction sort-dir)))))
	 (:span :class "preposition" "&nbsp;By&nbsp;")
	 (render-dropdown
	  "datalist-sort"
	  (remove nil
		  (map-view-fields
		   (lambda (field-info)
		     (let ((field (field-info-field field-info)))
		       (when (dataseq-field-sortable-p obj field)
			 (cons 
			  (view-field-label field)
			  (attributize-name (view-field-slot-name field))))))
		   (dataseq-view obj)
		   nil))
	  :selected-value (loop
			     ;; Try every slot value from last to
			     ;; first. We need to do this in case we sort
			     ;; foreign values by a slot we don't
			     ;; have. Consider: (company name).
			     for i from (1- (length
					     (ensure-list
					      (dataseq-sort-path obj))))
			     downto 0
			     collect (nth i (ensure-list (dataseq-sort-path obj))))
	  :autosubmitp t)
	 (noscript
	  (render-button *submit-control-name* :value (humanize-name "Go"))))))))

(defmethod dataseq-render-mining-bar ((obj datalist) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "data-mining-bar"
	  (when (dataseq-allow-sorting-p obj)
	    (datalist-render-sort-dropdown obj))
	  (when (dataseq-show-total-items-count-p obj)
	    (render-total-items-message obj)))))

;;; Don't wrap body in a form
(defmethod dataseq-wrap-body-in-form-p ((obj datalist))
  nil)

;; Drilldown
(defun datalist-on-drilldown (obj item-ids)
  (let ((item (find-persistent-object-by-id (dataseq-class-store obj)
					    (dataseq-data-class obj)
					    (cadr item-ids))))
    (when (dataseq-autoset-drilled-down-item-p obj)
      (setf (dataseq-drilled-down-item obj) item))
    (funcall (cdr (dataseq-on-drilldown obj)) obj item)))

;;; Body
(defun datalist-update-operations (obj)
  "Datalist implements drilldown via an item-op. This function first
removes the drilldown operation, and then adds it again if necessary."
  (let ((drilldown-op-name (car (dataseq-on-drilldown obj))))
    (setf (dataseq-item-ops obj)
	  (remove drilldown-op-name (dataseq-item-ops obj)
		  :key #'car :test #'string-equal))
    (when (and (dataseq-allow-drilldown-p obj)
	       (dataseq-on-drilldown obj))
      (pushnew (cons drilldown-op-name #'datalist-on-drilldown)
	       (dataseq-item-ops obj)
	       :key #'car))))

(defgeneric datalist-render-item (obj item args)
  (:documentation "Renders a given item of a datalist object.")
  (:method ((obj datalist) item args)
    (flet ((render-operations ()
	     (with-html
	       (:div :class "operations"
		     (:div :class "submit"
			   (mapc (lambda (op)
				   (render-link (lambda (&rest args)
						  (declare (ignore args))
						  (funcall (cdr op) obj
							   (cons :none (list (object-id item)))))
						(humanize-name (car op))
						:class "operation")
				   (with-html "&nbsp;"))
				 (dataseq-item-ops obj)))))))
      (apply #'render-object-view
	     item (datalist-item-data-view obj)
	     :widget obj
	     :fields-suffix-fn (lambda (&rest args)
				 (declare (ignore args))
				 (when (and (dataseq-allow-operations-p obj)
					    (dataseq-item-ops obj))
				   (render-operations)))
	     args))))

(defmethod render-dataseq-body ((obj datalist) &rest args)
  (datalist-update-operations obj)
  (let ((data-sequence (dataseq-data obj)))
    (setf (slot-value obj 'rendered-data-sequence) nil)
    (with-html
      (:div :class "datalist-body"
            (render-list data-sequence
                         :orderedp (datalist-ordered-p obj)
                         :render-fn (curry-after (curry #'datalist-render-item obj) args)
                         :empty-message (sequence-view-empty-message (find-view (dataseq-view obj)))
                         :empty-caption (view-caption (find-view (dataseq-view obj)))
                         :item-prefix-fn (lambda (item)
                                           (safe-apply (sequence-view-row-prefix-fn
                                                        (find-view (dataseq-view obj)))
                                                       (find-view (dataseq-view obj))
                                                       item args))
                         :item-suffix-fn (lambda (item)
                                           (safe-apply (sequence-view-row-suffix-fn
                                                        (find-view (dataseq-view obj)))
                                                       (find-view (dataseq-view obj))
                                                       item args)))))
    (setf (slot-value obj 'rendered-data-sequence) data-sequence)))

