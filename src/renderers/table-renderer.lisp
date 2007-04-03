;;;; Generic table renderer
(in-package :weblocks)

(defmethod with-table-header (obj body-fn)
  (let ((header-class (format nil "renderer table ~A"
			      (attributize-name (object-class-name obj)))))
    (with-html
      (:div :class header-class
	    (render-extra-tags "extra-top-" 3)
	    (funcall body-fn)
	    (render-extra-tags "extra-bottom-" 3)))))

(defmethod render-table-header-cell (slot-name slot-value &rest keys)
  (with-html
    (:th (str (humanize-name slot-name)))))

(defmethod render-table-header-cell (slot-name (slot-value standard-object) &rest keys)
  (apply #'render-table-header slot-value (apply #'object-visible-slots slot-value keys) :inlinep t keys))

(defmethod render-table-header (obj slots &rest keys &key inlinep &allow-other-keys)
  (let ((render-headers (lambda ()
			  (mapcar (lambda (slot)
				    (if (render-slot-inline-p obj (cdr slot))
					(apply #'render-table-header-cell (cdr slot)
					       (get-slot-value obj (car slot)) keys)
					(apply #'render-table-header-cell (cdr slot)
					       (object-name (get-slot-value obj (car slot)))
					       keys)))
				  slots))))
    (if (not inlinep)
	(with-html
	  (:thead (:tr (funcall render-headers))))
	(funcall render-headers))))

(defmethod render-table-cell (obj slot-name (slot-value standard-object)
			      &rest keys &key inlinep &allow-other-keys)
  (if (render-slot-inline-p obj slot-name)
      (apply #'render-table-row slot-value (apply #'object-visible-slots slot-value keys) :inlinep t keys)
      (with-html
	(:td (render-data (object-name slot-value))))))

(defmethod render-table-cell (obj slot-name slot-value
			      &rest keys &key inlinep &allow-other-keys)
  (with-html
    (:td (render-data slot-value))))

(defmethod render-table-row (obj slots &rest keys &key inlinep &allow-other-keys)
  (let ((render-row (lambda ()
		      (mapcar (lambda (slot)
				(apply #'render-table-cell obj (cdr slot) (get-slot-value obj (car slot))
				       keys))
			      slots))))
    (if (not inlinep)
	(with-html
	  (:tr (funcall render-row)))
	(funcall render-row))))

(defmethod render-table ((objs sequence) &rest keys)
  (let ((render-body (lambda ()
		         (with-html
			   (:table
			    (apply #'render-table-header (car objs)
				   (apply #'object-visible-slots (car objs) keys) keys)
			    (htm
			     (:tbody
			      (mapcar (lambda (obj)
					(apply #'render-table-row obj
					       (apply #'object-visible-slots obj keys) keys))
				      objs))))))))
    (with-table-header (car objs) render-body)))
     
