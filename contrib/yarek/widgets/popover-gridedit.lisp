(in-package :weblocks-yarek)

(export '(popover-gridedit
          popover-gridedit-close-dialog
          popover-gridedit-adjust-item-widget-actions-for-dialog))

(defwidget popover-gridedit (gridedit)
  ((adjust-item-widget-actions
     :accessor popover-gridedit-adjust-item-widget-actions-for-dialog-p
     :initform t
     :initarg  :adjust-item-widget-actions
     :documentation
       "When set to true, the item widget will be passed to 'popover-gridedit-adjust-item-widget-actions-for-dialog'.")
   (dialog-css-class
     :accessor popover-gridedit-dialog-css-class
     :initform nil
     :initarg  :dialog-css-class
     :documentation
       "The CSS class of the popover dialog."))
  (:documentation "A widget based on the gridedit that uses a popover dialog for the editor"))

(defgeneric popover-gridedit-adjust-item-widget-actions-for-dialog (w)
  (:documentation "Called to adjust the widget actions to do an 'answer' on all desired actions")
  (:method ((w t))
    nil))

(defmethod popover-gridedit-adjust-item-widget-actions-for-dialog ((w dataform))
  ;; all action functions on the default edit form must call (answer obj) to close the dialog
  (let ((cancel-fn (dataform-on-cancel w)))
    (setf (dataform-on-cancel w)
          #'(lambda (obj)
              (safe-funcall cancel-fn obj)
              (answer obj))))   
  (let ((success-fn (dataform-on-success w)))
    (setf (dataform-on-success w)
          #'(lambda (obj)
              (safe-funcall success-fn obj)
              (answer obj)))) 
  (let ((close-fn (dataform-on-close w)))
    (setf (dataform-on-close w)
          #'(lambda (obj)
              (safe-funcall close-fn obj) 
              (answer obj)))))

(defmethod dataedit-create-drilldown-widget ((pg popover-gridedit) item)
  (let ((item-widget (call-next-method)))
    (popover-gridedit-adjust-item-widget-actions-for-dialog item-widget) 
    item-widget))

(defmethod dataedit-create-new-item-widget ((pg popover-gridedit))
  (let ((item-widget (call-next-method)))
    (popover-gridedit-adjust-item-widget-actions-for-dialog item-widget) 
    item-widget))

(defmethod dataedit-drilldown-action ((pg popover-gridedit) item) 
  (call-next-method)
  (popover-gridedit-show-dialog pg))

(defmethod dataedit-add-items-action ((pg popover-gridedit))
  (call-next-method) 
  (popover-gridedit-show-dialog pg))

(defgeneric popover-gridedit-show-dialog (pg)
  (:documentation
    "Creates and shows the data editor dialog.")
  (:method ((pg popover-gridedit))
    (let ((item-w (dataedit-item-widget pg))
          (title (ecase (dataedit-ui-state pg)
                   (:drilldown "Edit an item") 
                   (:add "Add a new item"))))
;      (break "do dialog ~A ~A" title item-w)
      (do-dialog title item-w
                 :close #'(lambda (&rest args)
                            (declare (ignore args))
                            (popover-gridedit-close-dialog pg))
                 :css-class (popover-gridedit-dialog-css-class pg)))))

(defmethod render-widget-body ((pg popover-gridedit) &rest args) 
  (dataedit-update-operations pg)
  ;; note: we only render the datasequence only; we explicitly don't render the
  ;; item-widget, since that's supposed to show up in the dialog 
  (apply #'render-widget-body-dataseq pg args))


(defgeneric popover-gridedit-close-dialog (pg)
  (:documentation "This function, when called, will close the drilldown/new-item popover. In case of a drilldown, it will leave the drilled down item in selected state.")
  (:method ((pg popover-gridedit))
    (answer (dataedit-item-widget pg))
    ;; was: (dataedit-reset-state (pop-grid pde))
    ;; new: this is like dataedit-reset-state, but preserves selection
    (setf (dataedit-ui-state pg) nil
          (dataedit-item-widget pg) nil)))