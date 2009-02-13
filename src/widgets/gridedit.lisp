
(in-package :weblocks)

(export '(gridedit gridedit-drilldown-type))

(defwidget gridedit (dataedit-mixin datagrid)
  ((allow-select-p :initform t)
   (drilldown-type :accessor gridedit-drilldown-type
		 :initform :edit
		 :initarg :drilldown-type
		 :documentation "Type of the drilldown UI to be provided
		 by the gridedit. The default, :edit, provides a form
		 via 'dataform' widget when the user drills down into
		 a given item. If :view is specified the UI will
		 provide a dataform in the view mode. You can also
		 customize 'gridedit-create-drilldown-widget' to provide
		 more fine grained behavior."))
  (:documentation "A widget based on the 'datagrid' that enhances it
  with user interface to add, remove, and modify data entries."))

(defmethod dataedit-create-new-item-widget ((grid gridedit))
    (make-instance 'dataform
     :data (make-instance (dataseq-data-form-class grid))
     :class-store (dataseq-class-store grid)
     :ui-state :form
     :on-cancel (lambda (obj)
		  (declare (ignore obj))
		  (dataedit-reset-state grid)
		  (throw 'annihilate-dataform nil))
     :on-success (lambda (obj)
		   (safe-funcall (dataedit-on-add-item grid) grid (dataform-data obj))
		   (safe-funcall (dataedit-on-add-item-completed grid) grid (dataform-data obj))
                   (when (or (dataedit-mixin-flash-message-on-first-add-p grid)
                             (> (dataseq-data-count grid) 1))
                     (flash-message (dataseq-flash grid)
                                    (format nil "Added ~A."
                                            (humanize-name (dataseq-data-class grid)))))
		   (dataedit-reset-state grid)
                   (throw 'annihilate-dataform nil))
     :data-view (dataedit-item-data-view grid)
     :form-view (dataedit-item-form-view grid)))

(defmethod dataedit-create-drilldown-widget ((grid gridedit) item)
  (make-instance 'dataform
		 :data item
		 :class-store (dataseq-class-store grid)
		 :ui-state (if (eql (gridedit-drilldown-type grid) :edit)
			       :form
			       :data)
		 :on-success (lambda (obj)
			       (declare (ignore obj))
			       (flash-message (dataseq-flash grid)
					      (format nil "Modified ~A."
						      (humanize-name (dataseq-data-class grid))))
			       (if (eql (gridedit-drilldown-type grid) :edit)
                                   (progn
                                     (dataedit-reset-state grid)
                                     (throw 'annihilate-dataform nil))
				   (mark-dirty grid)))
		 :on-cancel (when (eql (gridedit-drilldown-type grid) :edit)
			      (lambda (obj)
				(declare (ignore obj))
				(dataedit-reset-state grid)
                                (throw 'annihilate-dataform nil)))
		 :on-close (lambda (obj)
			     (declare (ignore obj))
			     (dataedit-reset-state grid))
		 :data-view (dataedit-item-data-view grid)
		 :form-view (dataedit-item-form-view grid)))

;;; Renders the body of the gridedit. Essentially, just calls
;;; datagrid's render method, except that proper controls (add item,
;;; delete item, etc.) are added or removed depending on the settings
(defmethod render-widget-body ((obj gridedit) &rest args)
  (declare (ignore args))
  (dataedit-update-operations obj)
  (call-next-method)
  (when (dataedit-item-widget obj)
    (render-widget (dataedit-item-widget obj))))

