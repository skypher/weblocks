
(in-package :weblocks)

(defun render-select-bar (grid &rest keys)
  "Renders commands relevant to item selection (select all, none,
etc.)"
  (declare (ignore keys))
  (with-html
    (:p :class "datagrid-select-bar"
        (:strong (str (translate "Select: ")))
        (render-link (make-action (lambda (&rest args)
                                    (declare (ignore args))
                                    (setf (dataseq-selection grid)
                                          (cons :none (mapcar #'object-id (dataseq-data grid))))
                                    (mark-dirty grid)))
                     (translate "All"))
        ", "
        (render-link (make-action (lambda (&rest args)
                                    (declare (ignore args))
                                    (dataseq-clear-selection grid)
                                    (mark-dirty grid)))
                     (translate "None")))))

;; Custom selection field
(defclass datagrid-select-field (table-view-field)
  ((allow-sorting-p :initform nil))
  (:documentation "A field used to render select control."))

(defun make-select-field (grid-obj)
  "Makes a custom field for rendering select controls."
  (declare (ignore grid-obj))
  (make-instance 'datagrid-select-field
		 :label "Select"
		 :reader "Select"
		 :present-as nil))

(defun table-select-view-field-header-wt (&rest args)
  (with-html-to-string (:th :class "select" "")))

(deftemplate :table-select-view-field-header-wt 'table-select-view-field-header-wt)

(defmethod render-view-field-header ((field datagrid-select-field)
                                     (view table-view)
                                     (widget datagrid) presentation value obj 
                                     &rest args)
  "Render cell for select control"
  (declare (ignore args widget))
  (render-wt 
    :table-select-view-field-header-wt 
    (list :field field :view view :widget widget :presentation presentation :value value :object obj)))

(defun table-select-view-field-wt (&key drilldownp checkbox-name selectedp &allow-other-keys)
  (with-html-to-string
    (:td :class "select"
     :onclick (when drilldownp "stopPropagation(event);")
     :style (when drilldownp "cursor: default;")
     (:div
       (render-checkbox checkbox-name
                        selectedp
                        :class nil)))))

(deftemplate :table-select-view-field-wt 'table-select-view-field-wt)

(defmethod render-view-field ((field datagrid-select-field) 
                              (view table-view)
                              (widget datagrid) presentation value obj &rest args)
  (declare (ignore args))
  (let ((checkbox-name (concatenate 'string
                                    "item-" (attributize-name (object-id obj))))
        (drilldownp (and (dataseq-allow-drilldown-p widget)
                         (dataseq-on-drilldown widget))))
    (render-wt 
      :table-select-view-field-wt 
      (list :field field :view view :widget widget :presentation presentation :value value :object obj)
      :drilldownp drilldownp
      :checkbox-name checkbox-name 
      :selectedp (dataseq-item-selected-p widget (object-id obj)))))

