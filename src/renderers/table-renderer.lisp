;;;; Generic table renderer
(in-package :weblocks)

(defmethod render-table-header-cell (slot-name slot-value &rest keys)
  (with-html
    (:th (str slot-name))))

(defmethod render-table-header-cell (slot-name (slot-value standard-object) &rest keys)
  (apply #'render-table-header slot-value (apply #'object-visible-slots slot-value keys) :inlinep t keys))

(defmethod render-table-header (obj slots &rest keys &key inlinep &allow-other-keys)
  (let ((render-headers (lambda ()
			  (mapcar (lambda (slot)
				    (apply #'render-table-header-cell (cdr slot)
					     (get-slot-value obj (car slot)) keys))
				  slots))))
    (if (not inlinep)
	(with-html
	  (:thead (:tr (funcall render-headers))))
	(funcall render-headers))))

(defmethod render-table-cell (obj slot-name slot-value)
  (with-html
    (:td (render-data slot-value))))

(defmethod render-table-row (obj &rest keys)
  (with-html
    (:tr
     (mapcar (lambda (slot)
	       (render-table-cell obj (cdr slot) (get-slot-value obj (car slot))))
	     (apply #'object-visible-slots obj keys)))))

(defmethod render-table ((objs sequence) &rest keys)
  (with-html
    (:table
     (apply #'render-table-header (car objs) (apply #'object-visible-slots (car objs) keys) keys)
     (htm
      (:tbody
       (mapcar (lambda (obj)
		 (apply #'render-table-row obj keys))
	       objs))))))
     
