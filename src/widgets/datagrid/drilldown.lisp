
(in-package :weblocks)

(export '(datagrid-drilldown-field
	  with-datagrid-drilldown-table-view-body-row))

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

;;; Custom drilldown field
(defclass datagrid-drilldown-field (grid-view-field)
  ((allow-sorting-p :initform nil))
  (:documentation "A field used to render drilldown control."))

(defun make-drilldown-field (grid-obj)
  "Makes a custom field for rendering drilldown controls."
  (let ((label (humanize-name (car (datagrid-on-drilldown grid-obj)))))
    (make-instance 'datagrid-drilldown-field
		   :label label
		   :reader label
		   :present-as nil)))

;;; Drilldown cells
(defmethod render-view-field-header ((field datagrid-drilldown-field) (view grid-view)
				     widget presentation value obj &rest args)
  (declare (ignore args))
  (with-html (:th :class (datagrid-drilldown-style
			  (car (datagrid-on-drilldown widget)))
		  "")))

(defmethod render-view-field ((field datagrid-drilldown-field) (view grid-view)
			      widget presentation value obj &rest args
			      &key row-action &allow-other-keys)
  (declare (ignore args))
  (with-html
    (:td :class (datagrid-drilldown-style (car (datagrid-on-drilldown widget)))
	 (unless (ajax-request-p)
	   (htm
	    (:noscript
	     (:div
	      (render-link row-action
			   (humanize-name (car (datagrid-on-drilldown widget)))
			   :ajaxp nil))))))))

;;; Drilldown row
(defmethod with-table-view-body-row ((view grid-view) obj widget &rest args
				     &key alternp &allow-other-keys)
  (if (and (datagrid-allow-drilldown-p widget)
	   (datagrid-on-drilldown widget))
      (let ((row-action (make-action
			 (lambda (&rest args)
			   (declare (ignore args))
			   (when (datagrid-autoset-drilled-down-item-p widget)
			     (setf (datagrid-drilled-down-item widget) obj))
			   (funcall (cdr (datagrid-on-drilldown widget)) widget obj))))
	    (drilled-down-p (and (datagrid-drilled-down-item widget)
				 (eql (object-id (datagrid-drilled-down-item widget))
				      (object-id obj)))))
	(safe-apply (table-view-row-prefix-fn view) view obj args)
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
	       (apply #'render-table-view-body-row view obj widget :row-action row-action args)))
	(safe-apply (table-view-row-suffix-fn view) view obj args))
      (call-next-method)))

