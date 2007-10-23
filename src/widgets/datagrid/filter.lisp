
(in-package :weblocks)

(export '(object-satisfies-search-p))

(defun datagrid-filter-data (grid-obj data)
  "Returns filtered datagrid data. Applies 'object-satisfies-search-p'
repeatedly to each item in the sequence."
  (with-slots (search) grid-obj
    (if search
	(remove nil
		(mapcar (lambda (item)
			  (when (object-satisfies-search-p (make-isearch-regex search) nil nil t item)
			    item))
			data))
	data)))

(defgeneric object-satisfies-search-p (search-regex obj slot-name slot-type slot-value &rest args)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Determines if 'slot-value' satisfies a search regex. Default
implementation applies 'search-regex' to string representations of
each slot value obtained via calling 'data-print-object', and if one
matches returns true.

'obj' - the object that contains the slot in question.
'slot-name' - name of the slot.
'slot-type' - declared type of the slot.
'slot-value' - the value to be tested."))

(defslotmethod object-satisfies-search-p (search-regex obj slot-name slot-type (slot-value standard-object)
						       &rest args)
  (some (compose #'not #'null)
	(flatten
	 (apply #'visit-object-slots slot-value	(curry #'object-satisfies-search-p search-regex)
		:call-around-fn-p nil args))))

(defslotmethod object-satisfies-search-p (search-regex obj slot-name slot-type slot-value
						       &rest args)
  (not (null
	(if (typep slot-value 'standard-object)
	    (ppcre:scan search-regex (object-name slot-value))
	    (ppcre:scan search-regex (apply #'data-print-object
					    obj slot-name slot-type slot-value args))))))

(defun make-isearch-regex (search)
  "Create a regular expression from the user's input that tries to be
faithful to Emacs' isearch."
  (if (some #'upper-case-p search)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode nil)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode t)))

(defun total-items-message (grid)
  "Returns a text message specifying the total number of items and the
number of items that matched the search."
  (let ((total-items-count (datagrid-data-count grid :totalp t))
	(matched-items-count (datagrid-data-count grid :totalp nil)))
    (if (datagrid-search grid)
	(format nil "(Found ~A of ~A ~A)"
		matched-items-count
		total-items-count
		(proper-number-form total-items-count "Item"))
	(format nil "(Total of ~A ~A)"
		total-items-count
		(proper-number-form total-items-count "Item")))))

(defun datagrid-render-search-bar (grid &rest keys)
  "Renders a search bar for the datagrid."
  (with-html
    (:div :class "datagrid-search-bar"
	  (with-extra-tags
	    (htm (:span :class "title" (:strong "Search table&nbsp;")))
	    (when (datagrid-show-total-items-count-p grid)
	      (let ((total-items-message (total-items-message grid)))
		(when total-items-message
		  (with-html
		    (:span :class "total-items"
			   (str total-items-message))))))
	    (apply #'render-isearch "search"
		   (lambda (&key search &allow-other-keys)
		     (declare (special *on-ajax-complete-scripts*))
		     (setf (datagrid-search grid) (when (not (empty-p search))
						    search))
					; we also need to clear the selection
		     (datagrid-clear-selection grid)
		     (push (format nil "new Function(~A)"
				   (encode-json-to-string
				    (format nil "updateElementBody($('~A').~
                                                    getElementsByClassName('datagrid-body')[0], ~A);"
					    (attributize-name (widget-name grid))
					    (encode-json-to-string
					     (let ((*weblocks-output-stream* (make-string-output-stream)))
					       (declare (special *weblocks-output-stream*))
					       (apply #'render-datagrid-table-body grid (widget-args grid))
					       (get-output-stream-string *weblocks-output-stream*))))))
			   *on-ajax-complete-scripts*)
		     (when (datagrid-show-total-items-count-p grid)
		       (push
			(format nil
				"new Function(~A)"
				(encode-json-to-string
				 (format nil "updateElementBody($('~A').~
                                                 getElementsByClassName('total-items')[0], '~A');"
					 (attributize-name (widget-name grid))
					 (total-items-message grid))))
			*on-ajax-complete-scripts*)))
		   :value (datagrid-search grid)
		   keys)))))
