
(in-package :weblocks)

(export '(listedit))

(defwidget listedit (dataedit-mixin datalist)
  ()
  (:documentation "A widget based on the 'datalist' that enhances it
  with user interface to add, remove, and modify data entries."))

(defmethod dataedit-create-new-item-widget ((obj listedit))
  (make-quickform (dataedit-item-form-view obj)
		  :data (make-instance (dataseq-data-form-class obj))
		  :class-store (dataseq-class-store obj)
		  :on-success (lambda (w item)
				(declare (ignore w))
				(safe-funcall (dataedit-on-add-item obj) obj item)
                                (when (or (dataedit-mixin-flash-message-on-first-add-p obj)
                                          (> (dataseq-data-count obj) 1))
                                  (flash-message (dataseq-flash obj)
                                                 (format nil "Added ~A."
                                                         (humanize-name (dataseq-data-class obj)))))
                                (safe-funcall (dataedit-on-add-item-completed obj) obj item)
				(dataedit-reset-state obj))
		  :on-cancel (lambda (w)
			       (declare (ignore w))
			       (dataedit-reset-state obj)
			       (throw 'annihilate-dataform nil))))

(defmethod dataedit-create-drilldown-widget ((obj listedit) item)
  (make-quickform (dataedit-item-form-view obj)
		  :data item
		  :class-store (dataseq-class-store obj)
		  :on-success (lambda (w item)
				(declare (ignore w item))
				(flash-message (dataseq-flash obj)
					       (format nil "Modified ~A."
						       (humanize-name (dataseq-data-class obj))))
				(dataedit-reset-state obj))
		  :on-cancel (lambda (item)
			       (declare (ignore item))
			       (dataedit-reset-state obj))))

(defmethod datalist-render-item ((obj listedit) item args)
  (if (and (dataseq-drilled-down-item obj)
	   (equalp (object-id (dataseq-drilled-down-item obj))
		   (object-id item)))
      (render-widget (dataedit-item-widget obj))
      (call-next-method)))

;;; Renders the body of the listedit. Essentially, just calls
;;; datalist's render method, except that proper controls (add item,
;;; delete item, etc.) are added or removed depending on the settings
(defmethod render-widget-body ((obj listedit) &rest args)
  (declare (ignore args))
  (dataedit-update-operations obj)
  (call-next-method)
  (when (eq (dataedit-ui-state obj) :add)
    (render-widget (dataedit-item-widget obj))))

