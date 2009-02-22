
(in-package :weblocks)

(export '(datagrid))

(defwidget datagrid (dataseq)
  ()
  (:documentation "Represents a sortable, pagable table. This
  widget is inspired by ASP.NET's datagrid control."))

;;; Ensure scaffold view is selected if no view is provided explicitly
(defmethod dataseq-view ((obj datagrid))
  (or (slot-value obj 'view)
      (find-view
       (list 'table
	     (if (symbolp (dataseq-data-class obj))
		 (dataseq-data-class obj)
		 (class-name (dataseq-data-class obj)))))))

;;; Mining bar
(defmethod dataseq-render-mining-bar ((obj datagrid) &rest args)
  (with-html
    (:div :class "data-mining-bar"
	  (when (dataseq-show-total-items-count-p obj)
	    (render-total-items-message obj))
	  (when (dataseq-allow-select-p obj)
	    (apply #'render-select-bar obj args)))))

;;; Body
(defmethod render-dataseq-body ((obj datagrid) &rest args)
  (let ((data-sequence (dataseq-data obj)))
    (setf (slot-value obj 'rendered-data-sequence) nil)
    (with-html
      (:div :class "datagrid-body"
            (apply #'render-object-view data-sequence (dataseq-view obj)
                   :widget obj
                   :summary (if (dataseq-sort obj)
                                (format nil "Ordered by ~A, ~A."
                                        (string-downcase
                                         (humanize-name
                                          (dataseq-sort-slot obj)))
                                        (string-downcase
                                         (humanize-sort-direction
                                          (dataseq-sort-direction obj))))
                                nil)
                   :custom-fields (append-custom-fields
                                   (remove nil
                                           (list
                                            (when (dataseq-allow-select-p obj)
                                              (cons 0 (make-select-field obj)))
                                            (when (and (dataseq-allow-drilldown-p obj)
                                                       (dataseq-on-drilldown obj))
                                              (make-drilldown-field obj))))
                                   args)
                   args))
      (setf (slot-value obj 'rendered-data-sequence) data-sequence))))

