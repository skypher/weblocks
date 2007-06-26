
(in-package :weblocks)

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
	(format nil "(~A ~A ~A hidden by the search)"
		hidden-items-count
		(proper-number-form hidden-items-count "item")
		(proper-number-form hidden-items-count "is"))
	"&nbsp;")))

(defun datagrid-render-search-bar (grid &rest keys)
  "Renders a search bar for the datagrid."
  (with-html
    (:div :class "datagrid-search-bar"
	  (with-extra-tags
	    (htm (:span :class "title" (:strong "Search table&nbsp")))
	    (when (datagrid-show-hidden-entries-count-p grid)
	      (let ((hidden-items-message (hidden-items-message grid)))
		(when hidden-items-message
		  (with-html
		    (:span :class "hidden-items"
			   (str hidden-items-message))))))
	    (apply #'render-isearch "search"
		   (make-action
		    (lambda (&key search &allow-other-keys)
		      (declare (special *on-ajax-complete-scripts*))
		      (setf (datagrid-search grid) (when (not (empty-p search))
						     search))
		      ; we also need to clear the selection
		      (datagrid-clear-selection grid)
		      (push (format nil "~
function () { ~
  updateElementBody($('~A').getElementsByClassName('datagrid-body')[0], ~A); ~
}"
				      (attributize-name (widget-name grid))
				      (encode-json-to-string
				      (let ((*weblocks-output-stream* (make-string-output-stream)))
					(declare (special *weblocks-output-stream*))
					(apply #'render-datagrid-table-body grid (widget-args grid))
					(get-output-stream-string *weblocks-output-stream*))))
			      *on-ajax-complete-scripts*)
		      (when (datagrid-show-hidden-entries-count-p grid)
			(push (format nil "~
function () { ~
  updateElementBody($('~A').getElementsByClassName('hidden-items')[0], '~A'); ~
}"
				      (attributize-name (widget-name grid))
				      (hidden-items-message grid))
			      *on-ajax-complete-scripts*))))
		   :value (datagrid-search grid)
		   keys)))))
