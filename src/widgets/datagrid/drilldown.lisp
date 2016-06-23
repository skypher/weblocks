
(in-package :weblocks)

(export '(datagrid-drilldown-field))

;;; Utilities
(defun datagrid-drilldown-style (slot-name)
  "Returns a style used in datagrid drilldown cells."
  (concatenate 'string "drilldown" " " (attributize-name slot-name)))

;;; Custom drilldown field
(defclass datagrid-drilldown-field (table-view-field)
  ((allow-sorting-p :initform nil))
  (:documentation "A field used to render drilldown control."))

(defun make-drilldown-field (grid-obj)
  "Makes a custom field for rendering drilldown controls."
  (let ((label (humanize-name (car (dataseq-on-drilldown grid-obj)))))
    (make-instance 'datagrid-drilldown-field
                   :label label
                   :reader label
                   :present-as nil)))

(defun table-drilldown-field-header-cell-wt (&key class content &allow-other-keys)
  (with-html-to-string (:th :class class (str content))))

(deftemplate :table-drilldown-field-header-cell-wt 'table-drilldown-field-header-cell-wt)

;;; Drilldown cells
(defmethod render-view-field-header ((field datagrid-drilldown-field) (view table-view)
                                     (widget datagrid) presentation value obj &rest args)
  (declare (ignore args))
  (render-wt 
    :table-drilldown-field-header-cell-wt
    (list :field field :view view :widget widget :presentation presentation :object obj :value value)
    :class (datagrid-drilldown-style
             (car (dataseq-on-drilldown widget)))))

(defmethod render-view-field ((field datagrid-drilldown-field) (view table-view)
                              (widget datagrid) presentation value obj &rest args
                              &key row-action &allow-other-keys)
  (declare (ignore args))
  (with-html
    (:td :class (datagrid-drilldown-style (car (dataseq-on-drilldown widget)))
         (unless (ajax-request-p)
           (htm
            (:noscript
             (:div
              (render-link row-action
                           (humanize-name (car (dataseq-on-drilldown widget)))
                           :ajaxp nil))))))))

(defun datagrid-table-view-body-row-wt (&key row-class prefix suffix row-action session-string content &allow-other-keys)
  (with-html-to-string
    (str prefix)
    (:tr :class row-class
     :onclick (format nil "initiateActionOnEmptySelection(\"~A\", \"~A\");" row-action session-string)
     :onmouseover "this.style.cursor = \"pointer\";"
     :style "cursor: expression(\"hand\");"
     (str content))
    (str suffix)))

(deftemplate :datagrid-table-view-body-row-wt 'datagrid-table-view-body-row-wt)

;;; Drilldown row
(defmethod with-table-view-body-row ((view table-view) obj (widget datagrid) &rest args
                                                       &key alternp &allow-other-keys)
  (if (and (dataseq-allow-drilldown-p widget)
           (dataseq-on-drilldown widget))
    (let ((row-action (make-action
                        (lambda (&rest args)
                          (declare (ignore args))
                          (when (dataseq-autoset-drilled-down-item-p widget)
                            (setf (dataseq-drilled-down-item widget) obj))
                          (funcall (cdr (dataseq-on-drilldown widget)) widget obj))))
          (drilled-down-p (and (dataseq-drilled-down-item widget)
                               (eql (object-id (dataseq-drilled-down-item widget))
                                    (object-id obj)))))
      
      (render-wt 
        :datagrid-table-view-body-row-wt
        (list :view view :object obj :widget widget)
        :object obj
        :prefix (capture-weblocks-output (safe-apply (sequence-view-row-prefix-fn view) view obj args))
        :suffix (capture-weblocks-output (safe-apply (sequence-view-row-suffix-fn view) view obj args))
        :row-action row-action
        :session-string (session-name-string-pair)
        :row-class (when (or alternp drilled-down-p)
                     (concatenate 'string
                                  (when alternp "altern")
                                  (when (and alternp drilled-down-p) " ")
                                  (when drilled-down-p "drilled-down")))
        :alternp alternp 
        :drilled-down-p drilled-down-p
        :content (capture-weblocks-output (apply #'render-table-view-body-row view obj widget :row-action row-action args))))
    (call-next-method)))

