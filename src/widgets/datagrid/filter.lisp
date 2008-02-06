
(in-package :weblocks)

(export '())

(defun datagrid-render-search-bar (grid &rest keys)
  "Renders a search bar for the datagrid."
  (with-html
    (:div :class "datagrid-search-bar"
	  (with-extra-tags
	    (htm (:span :class "title" (:strong "Search table&nbsp;")))
	    (when (datagrid-show-total-items-count-p grid)
	      (render-total-items-message grid))
	    (apply #'render-isearch "search"
		   (lambda (&key search &allow-other-keys)
		     (declare (special *on-ajax-complete-scripts*))
		     (setf (datagrid-search grid) (when (not (empty-p search))
						    search))
		     ;; we also need to clear the selection
		     (datagrid-clear-selection grid)
		     (push (format nil "new Function(~A)"
				   (encode-json-to-string
				    (format nil "updateElementBody($('~A').~
                                                    getElementsByClassName('datagrid-body')[0], ~A);"
					    (attributize-name (widget-name grid))
					    (encode-json-to-string
					     (let ((*weblocks-output-stream* (make-string-output-stream)))
					       (declare (special *weblocks-output-stream*))
					       (funcall #'render-datagrid-table-body grid)
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
