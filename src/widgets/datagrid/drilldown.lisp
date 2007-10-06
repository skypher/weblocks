
(in-package :weblocks)

(export '(render-datagrid-drilldown-body-cell
	  render-datagrid-drilldown-header-cell
	  with-datagrid-drilldown-table-body-row))

;;; Utilities
(defun datagrid-drilldown-style (slot-name)
  "Returns a style used in datagrid drilldown cells."
  (concatenate 'string "drilldown" " " (attributize-name slot-name)))

(defun datagrid-drilldown-slot-p (grid-obj slot-name)
  "Returns true if the slot represents a 'drilldown' action slot, nil
otherwise."
  (and (datagrid-allow-drilldown-p grid-obj)
       (datagrid-on-drilldown grid-obj)
       (eql slot-name (car (datagrid-on-drilldown grid-obj)))))

;;; Drilldown rendering
(defgeneric render-datagrid-drilldown-header-cell (grid-obj obj slot-name slot-type slot-value &rest args
							    &key row-action &allow-other-keys)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Renders the 'drilldown' action slot's table header."))

(defslotmethod render-datagrid-drilldown-header-cell (grid-obj obj slot-name slot-type slot-value &rest args
							       &key row-action &allow-other-keys)
  (with-html (:th :class (datagrid-drilldown-style slot-name) "")))

(defgeneric render-datagrid-drilldown-body-cell (grid-obj obj slot-name slot-type slot-value &rest args
							  &key row-action &allow-other-keys)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Renders a cell with a link used to drill down into items."))

(defslotmethod render-datagrid-drilldown-body-cell (grid-obj obj slot-name slot-type slot-value &rest args
							     &key row-action &allow-other-keys)
  (with-html
    (:td :class (datagrid-drilldown-style slot-name)
	 (unless (ajax-request-p)
	   (htm
	    (:noscript
	     (:div
	      (render-link row-action
			   (humanize-name (car (datagrid-on-drilldown grid-obj)))
			   :ajaxp nil))))))))

(defmethod with-table-body-row :around (obj body-fn &rest keys &key alternp grid-obj &allow-other-keys)
  (if (and grid-obj
	   (datagrid-allow-drilldown-p grid-obj)
	   (datagrid-on-drilldown grid-obj))
      (apply #'with-datagrid-drilldown-table-body-row grid-obj obj body-fn keys)
      (call-next-method)))

(defgeneric with-datagrid-drilldown-table-body-row (grid-obj obj body-fn &rest keys
							     &key alternp &allow-other-keys)
  (:documentation
   "Renders datagrid table row templates for rows that provide
drilldown functionality. The default implementation inserts necessary
JS code to allow the user to click on a row."))

(defmethod with-datagrid-drilldown-table-body-row (grid-obj obj body-fn &rest keys
						   &key alternp &allow-other-keys)
  (let ((row-action (make-action
		     (lambda (&rest args)
		       (when (datagrid-autoset-drilled-down-item-p grid-obj)
			 (setf (datagrid-drilled-down-item grid-obj) obj))
		       (funcall (cdr (datagrid-on-drilldown grid-obj)) grid-obj obj))))
	(drilled-down-p (and (datagrid-drilled-down-item grid-obj)
			     (eql (datagrid-drilled-down-item grid-obj) obj))))
    (if (and (typep grid-obj 'datagrid)
	     (datagrid-allow-drilldown-p grid-obj))
	(with-html
	  (:tr :class (when (or alternp drilled-down-p)
			(concatenate 'string
				     (when alternp "altern")
				     (when (and alternp drilled-down-p) " ")
				     (when drilled-down-p "drilled-down")))
	       :onclick (format nil "initiateActionOnEmptySelection(\"~A\", \"~A\");"
				row-action (session-name-string-pair))
	       :onmouseover "this.style.cursor = \"pointer\";"
	       :style "cursor: expression(\"hand\");"
	       (apply body-fn :row-action row-action keys)))
	(apply #'call-next-method obj body-fn keys))))
